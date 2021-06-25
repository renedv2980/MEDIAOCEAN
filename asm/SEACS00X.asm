*          DATA SET SEACS00X   AT LEVEL 162 AS OF 07/28/94                      
*PHASE TA0D00A,*                                                                
*INCLUDE SEC1RP                                                                 
*                                                                               
*                                                                               
         TITLE 'SEACS00 - SECURITY ACCESS PROGRAM - CONTROLLER'                 
ACS00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**ACS0**,RA,R9,R8,CLEAR=YES,RR=RE                    
         LR    R7,RC                                                            
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         AH    RC,=Y(SYSWORK-WORKD)                                             
         USING SYSWORK,RC          RC=A(SYSTEM WIDE W/S)                        
         L     R6,12(R1)                                                        
         USING COMFACSD,R6         R6=A(COMFACS)                                
         L     R5,20(R1)                                                        
         USING TWAD,R5             R5=A(TWA)                                    
*                                                                               
         ST    R1,ACFULL           SET BASE ADDRESSES                           
         ST    RE,ACRELO                                                        
         ST    RB,ACBASE1                                                       
         ST    RA,ACBASE2                                                       
         ST    R9,ACBASE3                                                       
         ST    R8,ACBASE4                                                       
         ST    RD,ACWORKA                                                       
*                                                                               
         GOTO1 CCALLOV,ACPARM,(1,0),0,0         LOAD TABLE PHASE (01)           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD CONTROLLER TABLES                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,1(R1)          A(LOAD POINT)                                
         LA    R0,(ACSELTAB-ACRECTAB)/4+1 N'TABLES                              
         XR    RE,RE                                                            
*                                                                               
ACS2     L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACRECTAB(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,ACS2                                                          
*                                                                               
         LA    R1,CONADDRS         SET A(CONTROLLER HOOKS)                      
         XR    RE,RE                                                            
         LA    R0,CONADDRN                                                      
ACS4     L     RF,CONADDRS(RE)                                                  
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         A     RF,ACRELO                                                        
         ST    RF,ACPHSLST(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,ACS4                                                          
*                                                                               
         LA    R1,ROUTS            SET A(COMMON ROUTINES)                       
         LA    R0,COMRTNN                                                       
         LA    RE,COMRTNS                                                       
         XR    RF,RF                                                            
ACS6     ST    R1,0(RE)                                                         
         STC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,ACS6                                                          
*                                                                               
         LA    RF,IODA1            SET I/O VARIABLES                            
         ST    RF,ACIOADD                                                       
         MVC   ACIOLEN,=AL2(IOLENQ)                                             
         MVI   ACIONUM,IONUMQ                                                   
         MVI   ACIOIND,ACIOIDA+ACIOIWK                                          
*                                                                               
         MVC   ACSYSPGM,=X'0A0D'                                                
         MVI   ACHLPSCR,X'FE'      SET TWA VARIABLES                            
         MVI   ACTWAREC,1                                                       
         MVI   ACTWAACT,2                                                       
         MVI   ACTWAKEY,3                                                       
         MVI   ACTWAOPT,4                                                       
         MVC   ACENDTWA,=AL2(ACSTABH-TWAD)                                      
*                                                                               
*&&UK*&& MVI   SCDELIM,C'/'        OVERIDE NORMAL KEY MERGE CHARACTER           
*                                  SET HOOK INDICATORS                          
         MVI   ACFSTIND,C'Y'       FIRST TIME HOOK REQUIRED                     
*                                                                               
*&&UK*&& OI    ACINDS,ACIDEST      SET USE DESTINATION NAME ON REPORTS          
         MVI   ACACTIND,ACHKAFT    ACTION HOOK AFTER                            
         OI    ACLSMIND,ACLSMISK   SET SELTAB EXTENSION FLAG                    
*                                                                               
         LA    RF,IOAREA1                                                       
         ST    RF,AIOAREA1         SAVE A(IOAREA 1)                             
         LA    RF,IOAREA2-IOAREA1(RF)                                           
         ST    RF,AIOAREA2         SAVE A(IOAREA 2)                             
         LA    RF,IOAREA3-IOAREA2(RF)                                           
         ST    RF,AIOAREA3         SAVE A(IOAREA 3)                             
         LA    RF,SYSWORK-IOAREA3(RF)                                           
         ST    RF,ASYSWORK         SAVE A(SYSTEM WIDE W/S)                      
*                                                                               
         GOTO1 CDICTATE,ACPARM,C'LU  ',DDDCLIST,DDDSLIST                        
*                                                                               
         LH    RF,=Y(SECAREA-TWAD) SAVE A(SECURITY ACCESS BLOCK)                
         LA    RF,TWAD(RF)                                                      
         ST    RF,ACASEC                                                        
         TM    SECINDS-SECD(RF),SECIINIT                                        
         BO    ACS8                                                             
         GOTO1 CSECRET,ACPARM,('SECPINIT',ACASEC),SECAREAL                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACS8     L     R1,ACFULL           DUMMY-UP REGULAR FACPAK PLIST                
         MVC   ACPARM+00(4),28(R1)                                              
         MVC   ACPARM+04(4),20(R1)                                              
         MVC   ACPARM+08(4),00(R1)                                              
         MVC   ACPARM+12(4),04(R1)                                              
         MVC   ACPARM+16(4),12(R1)                                              
         LA    R1,ACPARM                                                        
         ST    R1,ACPARMA          SET A(REG PLIST) IN ACPARMA                  
*                                                                               
         GOTO1 CGENERAL,WORKD      CALL GENERAL CONTROLLER                      
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
         DROP  R5,R6,RC                                                         
         EJECT                                                                  
***********************************************************************         
* HOOK ROUTINES CALLED WITH ACMODE SETTINGS                           *         
*                                                                     *         
* NTRY - ACMODE=MODE FOR HOOK                                         *         
*                                                                     *         
* EXIT - FVMSGNO SET TO ERROR NUMBER WITH CC=NEQ ON ERROR             *         
*        FVMSGNO SET TO FVFOK WITH CC=EQ IF OK                        *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1  BASE=ACBASE1                                                     
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ASYSWORK                                                      
         USING SYSWORK,R8                                                       
         USING TWAD,R5                                                          
         L     R5,ATWA                                                          
         L     RE,4(RD)                                                         
         MVC   0(4,RE),=C'+HOO'                                                 
*        CLI   TWAOMODE,TWAOLOAD   IGNORE SPOOF LOAD SCREEN MODE                
*        BE    HOOKOKEX                                                         
*                                                                               
*        LA    RE,ACSRECH          SET CURSOR ON ERROR                          
*        ST    RE,FVADDR                                                        
         ZIC   RF,ACMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     HKFIRST             ACMFRST - FIRST TIME FOR TRANSACTION         
         B     HOOKX               ACMRECR - PROCESS RECORD TYPE                
         B     HKVALACT            ACMACTR - PROCESS ACTION                     
         B     HOOKX               ACMKEYR - PROCESS KEY                        
         B     HOOKX               ACMOPTR - PROCESS OPTIONS                    
         B     HOOKX               ACMLFMR - PROCESS FILE MAINTENANCE           
         B     HOOKX               ACMLSMR - PROCESS LIST/SELECT SCREEN         
         B     HOOKX               ACMREPR - PROCESS REPORT                     
         B     HOOKX               ACMOTHR - PROCESS OTHER                      
         B     HOOKX               ACMRECA - PROCESS REC/ACTION (LIST)          
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX               ACMLAST - LAST TIME MODE                     
*                                                                               
HOOKOKEX MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     HOOKX                                                            
HOOKBADX MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     HOOKX                                                            
HOOKX    CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST TIME HOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
HKFIRST  EQU   *                                                                
*                                                                               
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         CLI   C#SYS,0             TEST CONNECT INFO ALREADY SET UP             
         BNE   HOOKOKEX                                                         
         DROP  RF                                                               
         GOTO1 ASETCON                                                          
         BNE   HOOKX                                                            
*                                                                               
         B     HOOKOKEX                                                         
         EJECT                                                                  
***********************************************************************         
* ACTION HOOK - AFTER GENERAL'S RECORD/ACTION CHECKING                *         
***********************************************************************         
         SPACE 1                                                                
* CHECK FOR DDS RETRICTED ACCESS RECORD-ACTIONS                                 
*                                                                               
HKVALACT EQU   *                                                                
*&&UK                                                                           
         ICM   RE,15,AMIXNTRY      GET A(MIXTABLE ENTRY)                        
         BNZ   *+6                                                              
         DC    H'00'                                                            
         USING MIXTABD,RE                                                       
         TM    MIXINDS,MIXIREP     IGNORE IN REPORT MODE                        
         BO    HKACT100                                                         
         TM    MIXUSER,RESTRCTQ    CHECK FOR RESTRICTED ACCESS                  
         BZ    HKACT100                                                         
         GOTO1 VGETFACT,APPARM,0                                                
         L     R2,0(R1)            R1 = A(SYSTEM DEFINITION BLOCK)              
         USING FACTSD,R2                                                        
         CLI   FASYSID,1                                                        
         BE    HKACT100            IGNORE IN TEST SYSTEM                        
         CLI   FASYSID,3                                                        
         BE    HKACT100            IGNORE IN TTS                                
         CLI   FASYSID,5                                                        
         BE    HKACT100            IGNORE IN FACNEW                             
         TM    FATFLAG,X'08'       CHECK PERSONAL PASSWORD                      
         BZ    HKACT010              IF NOT ACCESS NOT ALLOWED                  
         CLC   CUAALF,=C'**'       TEST SPECIAL AGENCY ALPHA                    
         BE    HKACT100              IF SO ACCESS ALLOWED                       
*                                    ELSE ERROR MESSAGE                         
HKACT010 MVC   FVMSGNO,=AL2(CE#SECLO)                                           
         B     HOOKX                                                            
         DROP  RE,R2                                                            
HKACT100 EQU   *                                                                
*&&                                                                             
*                                                                               
HKACTX   B     HOOKOKEX                                                         
         DROP  R5,R8                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES AVAILABLE TO CONTROLLER AND OVERLAYS                *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST)                                         *         
*        RF=ROUTINE NUMBER (HIGH ORDER BYTE)                          *         
*                                                                     *         
* EXIT - FVMSGNO SET TO ERROR NUMBER WITH CC=NEQ ON ERROR             *         
*        FVMSGNO SET TO FVFOK WITH CC=EQ IF OK                        *         
***********************************************************************         
         SPACE 1                                                                
ROUTS    NTR1  BASE=ACBASE1,WORK=(RC,RWRKX-RWRKD),LABEL=NO                      
         USING RWRKD,RC            RC=A(LOCAL W/S)                              
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ASYSWORK                                                      
         USING SYSWORK,R8                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   RIOSAVE,IOAREA      SAVE I/O AREA                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     ROUTSBR(RF)                                                      
*                                                                               
ROUTSX   CLC   RIOSAVE,IOAREA      TEST ANY I/O EXECUTED                        
         BE    *+14                                                             
         OI    APINDS,APILRERD     YES - SET APPLICATION FLAG                   
         MVC   IOAREA(L'RIOSAVE),RIOSAVE                                        
         CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF BRANCH ADDRESSES TO ROUTS ROUTINES                         *         
***********************************************************************         
         SPACE 1                                                                
ROUTSBR  DS    0H                                                               
         B     ADDELS              ADD AN ELEMENT TO A RECORD                   
         B     DELELS              DELETE AN ELEMENT FROM A RECORD              
         B     GETELS              GET AN ELEMENT FROM A RECORD                 
*                                                                               
         B     SETACT              ADD/CHANGE ACTIVITY ELEMENT                  
         B     DISACT              DISPLAY ACTIVITY INFORMATION                 
         B     GETACT              GET ACTIVITY ELEMENT INTO ACTEL              
*                                                                               
         B     VALUID              VALIDATE USER-ID CODE                        
         B     DISUID              DISPLAY USER-ID CODE                         
*                                                                               
         B     VALSYS              VALIDATE SYSTEM NAME (IE MEDIA)              
         B     DISSYS              DISPLAY SYSTEM NAME OR NUMBER                
         B     VALPGM              VALIDATE PROGRAM NAME                        
         B     DISPGM              DISPLAY PROGRAM NAME                         
         B     VALOVPG             VALIDATE SYSTEM/PROGRAM NAMES                
*                                                                               
         B     VALCODE             VALIDATE FORMATTED CODE                      
         B     DISCODE             DISPLAY FORMATTED CODE (HEX/NUM/CHR)         
*                                                                               
         B     VALACG              VALIDATE ACCESS GROUP CODE                   
         B     DISACG              DISPLAY ACCESS GROUP CODE                    
*                                                                               
         B     VALTXT              VALIDATE SYSTEMS TEXT NUMBER                 
         B     DISTXT              DISPLAY SYSTEMS TEXT                         
         B     VALDIC              VALIDATE DICTIONARY REFERENCE NUMBER         
         B     DISDIC              DISPLAY DICTIONARY WORD                      
*                                                                               
         B     BLDBITT             BUILD BIT TABLE FROM ELEMENT                 
         B     TAGBITT             TAG BIT TABLE ONTO ELEMENT                   
*                                                                               
         B     GETPNAM             GET NAME FROM PERSONAL-ID RECORD             
         B     DISPNAM             FORMAT PERSON NAME FOR DISPLAY               
         B     GETPNUM             GET PASSWORD NUMBER FROM PID RECORD          
         B     GETGNAM             GET ACCESS GROUP NAME FROM RECORD            
         B     GETDNAM             GET DEPARTMENT NAME FORM RECORD              
         B     GETONAM             GET OFFICE NAME FROM RECORD                  
         B     GETLNAM             GET LIMIT ACCESS GROUP NAME FROM REC         
         B     GETPID              GET PERSONAL-ID FROM PASSWORD RECORD         
*                                                                               
         B     DISLACC             BUILD LIMIT ACCESS DATA IN APWORK            
         B     VALLACC             VALIDATE LIMIT ACCESS FIELD                  
*                                                                               
         B     TXTFLT              LIST TEXT FILTER                             
*                                                                               
         B     SETCON              BUILD LIST OF CONNECTABLE TO THINGS          
         B     TSTUID              TEST USER-ID IS CONNECTABLE TO               
         B     TSTSYS              TEST SYSTEM IS CONNECTABLE TO                
*                                                                               
         B     SETSEL              SAVE USER SYSTEM ELEMENT DATA                
         B     TSTPGM              TEST PROGRAM IN USER SYSTEM ELEMENT          
*                                                                               
         B     TSTOMAN             TEST PERSON IS OFFICE MANAGER                
         B     TSTGMAN             TEST PERSON IS GROUP MANAGER                 
         B     TSTDMAN             TEST PERSON IS DEPARTMENT MANAGER            
*                                                                               
         B     VAL1RP              VALIDATE 1R PERSON CODE FIELD                
         B     DIS1RP              BUILD 1R PERSON CODE IN APWORK               
*                                                                               
         B     GETAAD              GET AGENCY ACCESS DETAILS IN APWORK          
*                                                                               
         B     SUBDIC              SUBSTITUTE DICTIONARY PARAMETERS             
         DC    2XL4'00'            N/D                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - R1=A(RECORD TO ADD ELEMENT TO)                               *         
*        APELEM CONTAINS ELEMENT                                      *         
***********************************************************************         
         SPACE 1                                                                
ADDELS   LR    R0,R1                                                            
         GOTO1 VHELLO,RPARM,(C'P',CTFILE),(R0),APELEM,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
ADDELSX  B     ROUTSX                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DELETE AN ELEMENT FROM A RECORD                          *         
*                                                                     *         
* NTRY - R1=A(RECORD TO DELETE ELEMENT FROM)                          *         
*        APELEM CONTAINS ELEMENT CODE OF ELEMENT TO BE DELETED        *         
***********************************************************************         
         SPACE 1                                                                
DELELS   LR    R0,R1                                                            
         GOTO1 VHELLO,RPARM,(C'D',CTFILE),(APELEM,(R0)),               *        
               (APELEM+1,APELEM+2)                                              
         CLI   12(R1),6            TEST ELEMENT NOT FOUND                       
         BE    DELELSX                                                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELELSX  B     ROUTSX                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET AN ELEMENT IN A RECORD                               *         
*                                                                     *         
* NTRY - R1=A(RECORD TO GET ELEMENT FROM)                             *         
*        APELEM CONTAINS ELEMENT CODE AND DATA TO SEARCH FOR          *         
* EXIT - APPARM CONTAINS ADDRESS OF ELEMENT OR ZEROES IF NOT FOUND    *         
***********************************************************************         
         SPACE 1                                                                
GETELS   LR    R0,R1                                                            
         GOTO1 VHELLO,RPARM,(C'G',CTFILE),(APELEM,(R0)),               *        
               (APELEM+1,APELEM+2)                                              
         XC    APPARM(4),APPARM                                                 
         CLI   12(R1),0                                                         
         BNE   *+10                                                             
         MVC   APPARM(4),12(R1)                                                 
GETELSX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ACTIVITY ELEMENT                                  *         
*                                                                     *         
* NTRY - R1=A(RECORD TO ADD ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
SETACT   OC    ACTEL,ACTEL         TEST ACTVITY ELEMENT FOUND                   
         BNZ   SETACT1                                                          
         GOTO1 AGETACT             NO - EXTRACT IT                              
         MVC   ACTEL,APELEM                                                     
*                                                                               
SETACT1  LA    R2,ACTEL                                                         
         USING SAACVD,R2                                                        
         MVI   SAACVEL,SAACVELQ                                                 
         MVI   SAACVLEN,SAACVLNQ                                                
         MVC   SAACVDT,ASBDAT                                                   
         ICM   RF,3,SAACVSEQ       INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,SAACVSEQ                                                    
*                                                                               
         XC    APELEM,APELEM                                                    
         MVC   APELEM(1),ACTEL     SET ACTIVITY ELEMENT CODE                    
         GOTO1 ADELELS             DELETE AND RE-ADD ELEMENT                    
         MVC   APELEM(L'ACTEL),ACTEL                                            
         GOTO1 AADDELS                                                          
         XC    ACTEL,ACTEL         CLEAR ACTIVITY ELEMENT                       
         DROP  R2                                                               
*                                                                               
SETACTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACTIVITY DATE                                    *         
*                                                                     *         
* NTRY - R1=A(RECORD)                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISACT   GOTO1 AGETACT             EXTRACT ACTIVITY ELEMENT                     
         BNE   DISACTX                                                          
*                                                                               
         MVI   FVXTRA,C'-'                                                      
         MVCDD FVXTRA+2(7),CT#UPDTE                                             
         LA    R3,FVXTRA+10                                                     
*                                                                               
         LA    R2,ACTEL            POINT TO ACTIVITY ELEMENT                    
         USING SAACVD,R2                                                        
         GOTO1 VDATCON,RPARM,(3,SAACVDT),(8,(R3))                               
*&&UK*&& OI    0(R3),X'F0'                                                      
         DROP  R2                                                               
*                                                                               
DISACTX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ACTIVITY ELEMENT INTO ACTEL                          *         
*                                                                     *         
* NTRY - R1=A(RECORD)                                                 *         
* EXIT - ACTEL CONTAINS ACTIVITY ELEMENT OR BINARY ZEROES             *         
*        APELEM CONTAINS ACTIVITY ELEMENT CODE                        *         
*        FVMSGNO=ZERO IF ACTIVITY ELEMENT NOT FOUND                   *         
*        CC=EQUAL IF ELEMENT FOUND, NOT EQUAL IF NOT FOUND            *         
***********************************************************************         
         SPACE 1                                                                
GETACT   XC    APELEM,APELEM                                                    
         MVI   APELEM,1                                                         
         XC    ACTEL,ACTEL                                                      
         GOTO1 AGETELS                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         ICM   R1,15,APPARM                                                     
         BNZ   *+14                                                             
         XC    FVMSGNO,FVMSGNO     SET IF NO ACTIVITY ELEMENT                   
         B     GETACTX                                                          
         MVC   ACTEL,0(R1)         SAVE ELEMENT IN WORKING STORAGE              
GETACTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE USER-ID CODE                                    *         
*                                                                     *         
* NTRY: R1=A(USER-ID CODE FIELD HEADER)                               *         
* EXIT: IF CODE VALID CC=EQUAL                                        *         
*               APHALF=USER-ID NUMBER, APWORK=NAME                    *         
***********************************************************************         
         SPACE 1                                                                
VALUID   MVI   FVMINL,1            VALIDATE INPUT                               
         GOTO1 AFVAL                                                            
         BNE   VALUIDX                                                          
         OI    FHOID(R1),FHOITR                                                 
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(ID RECORD)                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,FVIFLD                                                    
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     VALUIDX                                                          
         SPACE 1                                                                
         XC    APWORK,APWORK                                                    
         XC    APHALF,APHALF                                                    
         LA    R3,CTIDATA          R3=A(FIRST ELEMENT)                          
         XR    RF,RF                                                            
         SPACE 1                                                                
         USING CTAGYD,R3                                                        
VUID2    CLI   CTAGYEL,CTAGYELQ    TEST AGENCY ELEMENT                          
         BNE   VUID4                                                            
         MVC   RHALF,CTAGYID       EXTRACT AGENCY ALPHA                         
         B     VUID10                                                           
         SPACE 1                                                                
         USING CTDSCD,R3                                                        
VUID4    CLI   CTDSCEL,CTDSCELQ    TEST DESCRIPTION ELEMENT                     
         BNE   VUID6                                                            
         MVC   APHALF,CTDSC        EXTRACT UID NUMBER                           
         B     VUID10                                                           
         SPACE 1                                                                
         USING CTDSTD,R3                                                        
VUID6    CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION ELEMENT                     
         BNE   VUID8                                                            
         MVC   APWORK(L'CTDSTNAM),CTDSTNAM  EXTRACT NAME                        
         B     VUID10                                                           
         SPACE 1                                                                
VUID8    CLI   0(R3),0             TEST E-O-R                                   
         BE    VUID20                                                           
         SPACE 1                                                                
VUID10   IC    RF,1(R3)                                                         
         BXH   R3,RF,VUID2         BUMP R3 TO NEXT ELEMENT                      
         SPACE 1                                                                
VUID20   GOTO1 VALAGA,RHALF        TEST AGENCY ALPHA OK                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     VALUIDX                                                          
         GOTO1 ATSTUID,APHALF      TEST USER CAN CONNECT TO USER-ID             
VALUIDX  B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AGENCY ALPHA                                    *         
*                                                                     *         
* NTRY: R1=A(AGENCY ALPHA)                                            *         
* EXIT: IF CODE VALID CC=EQUAL                                        *         
***********************************************************************         
         SPACE 1                                                                
VALAGA   NTR1                                                                   
         MVC   RWORK(L'CT5KALPH),0(R1)                                          
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,RWORK                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BNE   VAGAX                                                            
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         XR    RF,RF                                                            
         USING CTSEAD,R3                                                        
VAGA2    CLI   CTSEAEL,0           TEST SECURITY AGENCY ELEMENT                 
         BE    VAGA6                                                            
         CLI   CTSEAEL,CTSEAELQ    TEST SECURITY AGENCY ELEMENT                 
         BNE   VAGA4                                                            
         MVC   RWORK(L'CTSEAAID),CTSEAAID                                       
         B     VAGA6                                                            
VAGA4    IC    RF,1(R3)                                                         
         BXH   R3,RF,VAGA2         BUMP R3 TO NEXT ELEMENT                      
         SPACE 1                                                                
VAGA6    MVC   RHALF,CUAALF                                                     
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+10                                                             
         MVC   RHALF,OPTAGY                                                     
         CLC   RHALF,RWORK                                                      
VAGAX    XIT1                                                                   
         DROP  R2,R3                                                            
         SPACE 3                                                                
***********************************************************************         
* ROUTINE TO DISPLAY USER-ID CODE                                     *         
*                                                                     *         
* NTRY: R1=A(USER-ID NUMBER)                                          *         
* EXIT: APWORK=USER-ID CODE/NAME                                      *         
***********************************************************************         
         SPACE 1                                                                
DISUID   LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(ID RECORD)                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R1)                                                    
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     DISUIDX                                                          
         SPACE 1                                                                
         XC    APWORK,APWORK                                                    
         LA    R3,CTIDATA                                                       
DUID02   CLI   0(R3),0             TEST E-O-R                                   
         BE    DISUIDX                                                          
         SPACE 1                                                                
         USING CTDSCD,R3                                                        
         CLI   0(R3),CTDSCELQ      TEST DESCRIPTION ELEMENT                     
         BNE   DUID04                                                           
         IC    RF,CTDSCLEN                                                      
         SH    RF,=Y(CTDSC+1-CTDSCD)                                            
         EX    RF,*+4                                                           
         MVC   APWORK(0),CTDSC                                                  
         B     DUID08                                                           
         SPACE 1                                                                
         USING CTDSTD,R3                                                        
DUID04   CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION ELEMENT                     
         BNE   DUID08                                                           
         MVC   APWORK+9(L'CTDSTNAM),CTDSTNAM  EXTRACT NAME                      
         DROP  R3                                                               
         SPACE 1                                                                
DUID08   XR    RF,RF               BUMP R3 TO NEXT ELEMENT                      
         IC    RF,1(R3)                                                         
         BXH   R3,RF,DUID02                                                     
         SPACE 1                                                                
DISUIDX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SYSTEM NAME                                     *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF SYSTEM FIELD)                           *         
* EXIT - APWORK+0(1)=NATIVE SYSTEM NUMBER                             *         
*        APWORK+1(1)=EQUATED SYSTEM NUMBER                            *         
*        APPARM(4)=A(SYSTEM LIST TABLE ENTRY)                         *         
*        CC=EQUAL IF SYSTEM NAME IS VALID, NOT EQUAL IF INVALID       *         
* NOTE - IF SHORTENED NAME WAS INPUT, FULL NAME WILL BE OUTPUT        *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   MVI   FVMAXL,L'SYSLNAME                                                
         GOTO1 AFVAL                                                            
         BNE   VALSYSX                                                          
*                                                                               
         XR    RF,RF               RF=L'INPUT-1                                 
         IC    RF,FVXLEN           RF=L'INPUT-1                                 
         L     R3,ASYSLST                                                       
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
*                                                                               
VALSYS2  CLI   0(R3),0             TEST E-O-T                                   
         BE    VALSYS4                                                          
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VALSYS3                                                          
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    *+12                                                             
VALSYS3  LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VALSYS2                                                          
*                                                                               
         MVC   APWORK(2),SYSLNUM   RETURN SYSTEM NUMBERS                        
         ST    R3,APPARM           RETURN A(SYSLST ENTRY)                       
         L     R1,FVADDR           DISPLAY FULL NAME                            
         OI    FHOID(R1),FHOITR                                                 
         MVC   FHDAD(L'SYSLNAME,R1),SYSLNAME                                    
*                                                                               
*&&UK*&& GOTO1 ATSTSYS,SYSLNUM     TEST USER CAN CONNECT TO SYSTEM              
         B     VALSYSX                                                          
*                                                                               
VALSYS4  MVC   FVMSGNO,=AL2(FVFESYS)         INVALID SYSTEM                     
*                                                                               
VALSYSX  B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEM NAME                                      *         
*                                                                     *         
* NTRY - R1=A(SYSTEM NUMBER)                                          *         
* EXIT - APWORK+0(7)=SYSTEM NAME                                      *         
*      - APPARM+0(4)=A(SYSLIST ENTRY) - 0 = UNKNOWN                   *         
***********************************************************************         
         SPACE 1                                                                
DISSYS   XR    RF,RF                                                            
         IC    RF,0(R1)            RF=SYSTEM NUMBER                             
         L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
*                                                                               
DISSYS2  CLI   0(RE),0             TEST E-O-T                                   
         BE    DISSYS4                                                          
         CLM   RF,1,SYSLNUM        MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DISSYS2                                                          
         MVC   APWORK(L'SYSLNAME),SYSLNAME                                      
         STCM  RE,15,APPARM        RETURN A(SYSLIST ENTRY)                      
         B     DISSYSX                                                          
*                                                                               
DISSYS4  MVI   APWORK,C' '         OUTPUT SYSTEM NUMBER                         
         MVC   APWORK+1(L'SYSLNAME-1),APWORK                                    
         XC    APPARM(4),APPARM    NO A(SYSLIST ENTRY)                          
         CVD   RF,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  RWORK(3),RDUB                                                    
         CLI   RWORK,C'0'                                                       
         BE    *+14                                                             
         MVC   APWORK(3),RWORK                                                  
         B     DISSYSX                                                          
         CLI   RWORK+1,C'0'                                                     
         BE    *+14                                                             
         MVC   APWORK(2),RWORK+1                                                
         B     DISSYSX                                                          
         MVC   APWORK(1),RWORK+2                                                
*                                                                               
DISSYSX  B     ROUTSX                                                           
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PROGRAM NAME                                    *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(PROGRAM FIELD HEADER)       *         
* EXIT - APWORK+0(1)=PROGRAM NUMBER                                   *         
*        APPARM(4)=A(PROGRAM LIST ENTRY)                              *         
*        CC=EQUAL IF PROGRAM NAME IS VALID, NOT EQUAL IF INVALID      *         
* NOTE - IF SHORTENED NAME WAS INPUT, FULL NAME WILL BE OUTPUT        *         
***********************************************************************         
         SPACE 1                                                                
VALPGM   MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R1,0(R1)            POINT TO INPUT FIELD HEADER                  
         MVI   FVMAXL,L'PGMNAME                                                 
         GOTO1 AFVAL               TEST FOR INPUT                               
         BNE   VALPGMX                                                          
         L     R1,ASYS                                                          
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,RWORK                                                    
         BE    VALPGM2                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)         INVALID SYSTEM                     
         B     VALPGMX                                                          
*                                                                               
VALPGM2  L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         XR    R2,R2                                                            
         IC    R2,FVXLEN           R2=INPUT LENGTH-1                            
VALPGM4  CLI   PGMCTRY,0           MATCH ON COUNTRY                             
         BE    *+14                                                             
         CLC   PGMCTRY,CUCTRY                                                   
         BNE   VALPGM8                                                          
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
         BE    VALPGM10                                                         
VALPGM8  BXLE  R1,RE,VALPGM4                                                    
         MVC   FVMSGNO,=AL2(CE#PGNVS)        INVALID PROGRAM                    
         B     VALPGMX                                                          
*                                                                               
VALPGM10 ST    R1,APPARM           SET A(PGMLST ENTRY)                          
         MVC   APWORK(1),PGMNUM                                                 
         L     RE,FVADDR           DISPLAY FULL NAME                            
         OI    FHOID(RE),FHOITR                                                 
         MVC   FHDAD(L'PGMNAME,RE),PGMNAME                                      
VALPGMX  B     ROUTSX                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PROGRAM NAME                                     *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(PROGRAM NUMBER)             *         
* EXIT - APWORK(7)=PROGRAM NAME                                       *         
*        APPARM(4)=A(PROGRAM LIST ENTRY) OR ZERO IF NOT FOUND         *         
***********************************************************************         
         SPACE 1                                                                
DISPGM   MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R1,0(R1)            POINT TO INPUT FIELD HEADER                  
         MVC   RWORK+1(1),0(R1)                                                 
         L     R1,ASYS                                                          
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,RWORK                                                    
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DISPGM12                                                         
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DISPGM2  CLC   PGMNUM,RWORK+1                                                   
         BNE   DISPGM8                                                          
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    DISPGM10                                                         
         CLC   PGMCTRY,CUCTRY                                                   
         BE    DISPGM10                                                         
DISPGM8  BXLE  R1,RE,DISPGM2                                                    
         B     DISPGM12                                                         
DISPGM10 ST    R1,APPARM                                                        
         MVC   APWORK(L'PGMNAME),PGMNAME                                        
         B     DISPGMX                                                          
*                                                                               
DISPGM12 MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'PGMNAME-1),APWORK                                     
         MVC   APWORK(4),=C'PGM='                                               
         XOUT  RWORK+1,APWORK+4,1                                               
         XC    APPARM(4),APPARM                                                 
*                                                                               
DISPGMX  B     ROUTSX                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SYSTEM/PROGRAM INPUTS                           *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON IF NO PROGRAM/SYSTEM VALID               *         
*             1-3 = A(SYSTEM INPUT FIELD)                             *         
*       P2 BYTE 0 = MIN. LENGTH OF PROGRAM INPUT FIELD                *         
*             1-3 = A(PROGRAM INPUT FIELD)                            *         
*       P3        = A(PROGRAM DESCRIPTION OUTPUT FIELD) OR 0          *         
* EXIT:    APHALF = SYSTEM/PROGRAM CODES                              *         
*          APELEM = PROGRAM ELEMENT                                   *         
***********************************************************************         
         SPACE 1                                                                
VALOVPG  LM    R2,R4,0(R1)                                                      
         MVC   APBYTE,4(R1)                                                     
         XC    APHALF,APHALF                                                    
         XC    APWORK,APWORK                                                    
         XC    APELEM,APELEM                                                    
*                                                                               
         TM    0(R1),X'80'         TEST NO SYSTEM/PROGRAM VALID                 
         BZ    VOVPG02                                                          
         CLI   FHILD(R2),0         TEST NO SYSTEM                               
         BNE   VOVPG02                                                          
         CLI   FHILD(R3),0         TEST NO PROGRAM                              
         BE    VOVPG10                                                          
         ST    R3,FVADDR                                                        
         MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         B     VALOVPGX                                                         
*                                                                               
VOVPG02  MVI   FVMINL,1            VALIDATE SYSTEM                              
         GOTO1 AVALSYS,(R2)                                                     
         BNE   VALOVPGX                                                         
         MVC   APHALF(1),APWORK                                                 
*                                                                               
         MVC   FVMINL,APBYTE       VALIDATE PROGRAM                             
         CLI   FVMINL,0            TEST PROGRAM REQUIRED                        
         BNE   VOVPG04                                                          
         CLI   FHILD(R3),0         TEST PROGRAM ENTERED                         
         BNE   VOVPG04                                                          
         XC    APWORK,APWORK                                                    
         B     VOVPG10                                                          
VOVPG04  GOTO1 AVALPGM,RPARM,(APHALF,(R3))                                      
         BNE   VALOVPGX                                                         
         MVC   APHALF+1(1),APWORK                                               
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAPGREC,R2          R2=A(PROGRAM RECORD KEY)                     
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
         MVC   SAPGOVPG,APHALF                                                  
*                                                                               
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOCONFIL+IOREAD                                              
         BE    *+14                PROGRAM RECORD MUST BE ON FILE               
         MVC   FVMSGNO,=AL2(CE#PNDSS)                                           
         B     VALOVPGX                                                         
*                                                                               
         LA    R3,SAPGDATA                                                      
         USING SAPGMD,R3           R3=A(PROGRAM ELEMENT)                        
         XR    RF,RF                                                            
         IC    RF,SAPGMLN                                                       
         CLI   SAPGMEL,SAPGMELQ                                                 
         BE    *+8                                                              
         BXH   R3,RF,*-12                                                       
         XC    APELEM,APELEM                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APELEM(0),SAPGMD                                                 
*                                                                               
         GOTO1 ADISTXT,RPARM,(SAPGOVS,SAPGMDSC)                                 
*                                                                               
         USING FHD,R4                                                           
VOVPG10  LTR   R4,R4               OUTPUT PROGRAM DESCRIPTION                   
         BZ    VOVPG12                                                          
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+1)                                                   
         EX    RE,*+4                                                           
         MVC   FHDA(0),APWORK                                                   
         OI    FHOI,FHOITR                                                      
*                                                                               
VOVPG12  XR    R1,R1                                                            
         CLI   APHALF+1,0          TEST HAVE PROGRAM RECORD                     
         BE    *+8                                                              
         LA    R1,SAPGREC                                                       
         GOTO1 ASUBDIC,(R1)        SET DICTIONARY SUBSTITUTION PARMS            
*                                                                               
VALOVPGX B     ROUTSX                                                           
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A FORMATTED CODE                                *         
*                                                                     *         
* NTRY: P1=(FORMAT OF CODE, A(FIELD HEADER))                          *         
* EXIT: APBYTE=CODE NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALCODE  MVC   RBYTE,0(R1)         EXRACT FORMAT                                
         L     R1,0(R1)            R1=A(FIELD HEADER)                           
         OI    FHOID(R1),FHOITR                                                 
         MVI   FVMINL,1                                                         
         SPACE 1                                                                
         CLI   RBYTE,SAPGMIFA      VALIDATE ALPHAMERIC FORMAT                   
         BNE   VCODE2                                                           
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALCODEX                                                         
         MVC   APBYTE,FVIFLD                                                    
         B     VCODE10                                                          
         SPACE 1                                                                
VCODE2   CLI   RBYTE,SAPGMIFN      VALIDATE NUMERIC FORMAT                      
         BNE   VCODE4                                                           
         GOTO1 AFVAL                                                            
         BNE   VALCODEX                                                         
         TM    FVIIND,FVINUM                                                    
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALCODEX                                                         
         MVC   APBYTE,SCFULL+3     TEST NUMBER<256                              
         OC    SCFULL(3),SCFULL                                                 
         BZ    VCODE10                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALCODEX                                                         
         SPACE 1                                                                
VCODE4   CLI   RBYTE,SAPGMIFX      VALIDATE HEXADECIMAL FORMAT                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FVMINL,2                                                         
         MVI   FVMAXL,2                                                         
         GOTO1 AFVAL                                                            
         BNE   VALCODEX                                                         
         TM    FVIIND,FVIHEX                                                    
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTH)                                            
         B     VALCODEX                                                         
         SPACE 1                                                                
         LA    R1,FVIFLD           FIND HEX VALUE                               
         XR    R0,R0                                                            
         LA    RE,X'0F'                                                         
VCODE6   IC    RF,0(R1)                                                         
         CLI   0(R1),C'0'                                                       
         BNL   *+8                                                              
         LA    RF,X'09'(RF)                                                     
         NR    RF,RE                                                            
         SLL   R0,4                                                             
         AR    R0,RF                                                            
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNE   VCODE6                                                           
         STC   R0,APBYTE                                                        
         SPACE 1                                                                
VCODE10  CLI   APBYTE,0            ENSURE CODE IS NON ZERO                      
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VALCODEX B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY A FORMATTED CODE                                 *         
*                                                                     *         
* NTRY: P1=(FORMAT OF CODE, A(CODE))                                  *         
* EXIT: APWORK=OUTPUT                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCODE  MVI   APWORK,C' '                                                      
         MVC   APWORK+1(2),APWORK                                               
         L     R2,0(R1)                                                         
         SPACE 1                                                                
         CLI   0(R1),SAPGMIFA      DISPLAY ALPHAMERIC                           
         BNE   DCODE2                                                           
         MVC   APWORK(1),0(R2)                                                  
         B     DISCODEX                                                         
         SPACE 1                                                                
DCODE2   CLI   0(R1),SAPGMIFN      DISPLAY NUMERIC                              
         BNE   DCODE4                                                           
         EDIT  (B1,0(R2)),(3,APWORK),ALIGN=LEFT,WRK=RWORK,DUB=RDUB              
         B     DISCODEX                                                         
         SPACE 1                                                                
DCODE4   CLI   0(R1),SAPGMIFX      DISPLAY HEXADECIMAL                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APWORK+1(1),0(R2)                                                
         UNPK  APWORK(1),APWORK+1(1)                                            
         LA    R0,2                                                             
         LA    R1,APWORK                                                        
DCODE6   OI    0(R1),C'0'                                                       
         CLI   0(R1),C'9'                                                       
         BNH   DCODE8                                                           
         IC    RE,0(R1)                                                         
         LA    RE,C'G'(RE)                                                      
         STC   RE,0(R1)                                                         
DCODE8   LA    R1,1(R1)                                                         
         BCT   R0,DCODE6                                                        
         SPACE 1                                                                
DISCODEX B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ACCESS GROUP CODE                               *         
*                                                                     *         
* NTRY: R1=A(ACCESS GROUP CODE FIELD HEADER)                          *         
* EXIT: IF CODE VALID CC=EQUAL                                        *         
*               APWORK=NAME                                           *         
***********************************************************************         
         SPACE 1                                                                
VALACG   MVI   FVMINL,1            VALIDATE FIELD                               
         GOTO1 AFVAL                                                            
         BNE   VALACGX                                                          
         OI    FHOID(R1),FHOITR                                                 
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2          R2=A(ACCESS GROUP RECORD)                    
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SAAGAGY,OPTAGY                                                   
         MVC   SAAGAGR,FVIFLD                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#AGRNF)                                           
         B     VALACGX                                                          
         SPACE 1                                                                
         LA    R3,SAAGDATA                                                      
         USING SAAGND,R3           R3=A(AGENCY ELEMENT)                         
         XR    RF,RF                                                            
VACG2    IC    RF,SAAGNLN                                                       
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    VACG4                                                            
         BXH   R3,RF,VACG2                                                      
         SPACE 1                                                                
VACG4    MVC   APHALF,SAAGNNUM     SAVE GROUP NUMBER                            
         XC    APWORK,APWORK       SAVE GROUP NAME                              
         SH    RF,=Y(SAAGNLNQ+1)                                                
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   APWORK(0),SAAGNNAM                                               
         SPACE 1                                                                
VALACGX  B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS GROUP CODE                                *         
*                                                                     *         
* NTRY: R1=A(ACCESS GROUP NUMBER)                                     *         
* EXIT: APWORK=ACCESS GROUP CODE/NAME                                 *         
***********************************************************************         
         SPACE 1                                                                
DISACG   LR    R4,R1               R4=A(ACCESS GROUP NUMBER)                    
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2          R2=A(ACCESS GROUP RECORD)                    
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SAAGAGY,OPTAGY                                                   
         MVI   SAAGKEY+L'SAAGKEY-1,X'01'                                        
         SPACE 1                                                                
         LA    R2,RIO                                                           
         LA    R1,IOHI+IOCONFIL                                                 
         B     *+8                                                              
DACG2    LA    R1,IOSQ+IOCONFIL                                                 
         ST    R2,IOADDR                                                        
         GOTO1 AIO                 READ NEXT RECORD FOR AGENCY                  
         CLC   SAAGKEY(SAAGAGR-SAAGKEY),IOKEYSAV                                
         BE    DACG4                                                            
         MVI   APWORK,C'.'                                                      
         MVI   APWORK+1,C'?'                                                    
         MVC   APWORK+2(L'APWORK-2),APWORK+1                                    
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     DISACGX                                                          
         SPACE 1                                                                
DACG4    XR    RF,RF                                                            
         LA    R3,SAAGDATA                                                      
         USING SAAGND,R3           R3=A(AGENCY ELEMENT)                         
DACG6    IC    RF,SAAGNLN                                                       
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    DACG8                                                            
         BXH   R3,RF,DACG6         BUMP R3 TO NEXT ELEMENT                      
         SPACE 1                                                                
DACG8    CLC   SAAGNNUM,0(R4)      MATCH ON AGENCY NUMBER                       
         BNE   DACG2                                                            
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'SAAGAGR),SAAGAGR                                        
         SH    RF,=Y(SAAGNLNQ+1)                                                
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   APWORK+L'SAAGAGR+1(0),SAAGNNAM                                   
         SPACE 1                                                                
DISACGX  B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A SYSTEMS TEXT NUMBER                           *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON IF NO INPUT MEANS USE DICTIONARY         *         
*                 = SYSTEM OVERLAY NUMBER                             *         
*             1-3 = A(FIELD HEADER OF NUMBER)                         *         
*       P2        = A(DICTIONARY REF #) (IF REQUIRED)                 *         
* EXIT:    APHALF = TEXT NUMBER                                       *         
*          APWORK = TEXT                                              *         
***********************************************************************         
         SPACE 1                                                                
VALTXT   LM    R2,R3,0(R1)                                                      
         MVC   RBYTE,0(R1)                                                      
         NI    RBYTE,X'FF'-X'80'                                                
         TM    0(R1),X'80'                                                      
         BO    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BH    VALTXTX                                                          
         BE    VALTXT02                                                         
         XC    APHALF,APHALF                                                    
         GOTO1 ADISDIC,ACPARM,(RBYTE,L'APWORK),(C'L',(R3))                      
         B     VALTXTX                                                          
*                                                                               
VALTXT02 TM    FVIIND,FVINUM                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALTXTX                                                          
         OC    SCFULL,SCFULL                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALTXTX                                                          
         MVC   APHALF,SCFULL+2                                                  
         LA    R1,ACPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,APHALF                                                   
         MVI   GTMAXL,L'APWORK                                                  
         LA    R0,APWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         OI    GT1INDS,GT1OWRK                                                  
         MVC   GTMSYS,RBYTE                                                     
         GOTO1 VGETTXT                                                          
         TM    GT1INDS,GT1NOMSG                                                 
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALTXTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEMS TEXT                                     *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON IF TEXT #=0 MEANS USE DICTIONARY         *         
*                 = SYSTEM OVERLAY NUMBER                             *         
*             1-3 = A(TEXT NUMBER)                                    *         
*       P2        = A(DICTIONARY REF #) (IF REQUIRED)                 *         
* EXIT:    APWORK = TEXT                                              *         
***********************************************************************         
         SPACE 1                                                                
DISTXT   LM    R2,R3,0(R1)                                                      
         MVC   RBYTE,0(R1)                                                      
         NI    RBYTE,X'FF'-X'80'                                                
*                                                                               
         TM    0(R1),X'80'                                                      
         BZ    DISTXT02                                                         
         OC    0(2,R2),0(R2)                                                    
         BNZ   DISTXT02                                                         
         GOTO1 ADISDIC,ACPARM,(RBYTE,L'APWORK),(C'L',(R3))                      
         B     DISTXTX                                                          
*                                                                               
DISTXT02 LA    R1,ACPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,0(R2)                                                    
         MVI   GTMAXL,L'APWORK                                                  
         LA    R0,APWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         OI    GT1INDS,GT1OWRK                                                  
         MVC   GTMSYS,RBYTE                                                     
         GOTO1 VGETTXT                                                          
         TM    GT1INDS,GT1NOMSG                                                 
         BZ    DISTXTX                                                          
         MVI   APWORK,C'?'                                                      
         MVC   APWORK+1(L'APWORK-1),APWORK                                      
*                                                                               
DISTXTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A DICTIONARY EQUATE                             *         
*                                                                     *         
* NTRY: P1=(SYSTEM OVERLAY NUMBER, L(WORD))                           *         
*       P2=('U'PPER/'L'OWER CASE, A(FIELD HEADER))                    *         
* EXIT: APHALF=TEXT NUMBER         APWORK=WORD                        *         
***********************************************************************         
         SPACE 1                                                                
VALDIC   MVC   RIPARM(8),0(R1)     SAVE INPUT PARAMETERS                        
         XR    R3,R3                                                            
         ICM   R3,7,5(R1)                                                       
         USING FHD,R3              R3=A(FIELD HEADER)                           
         OI    FHOI,FHOITR                                                      
*                                                                               
         XR    RF,RF                                                            
         IC    RF,RIPARM+0                                                      
         MH    RF,=Y(3)                                                         
         LA    RF,PFXTABLE(RF)     RF=PFXTABLE+(SYSTEM * 3)                     
         MVC   RFULL(2),0(RF)                                                   
         MVI   RFULL+2,C'#'        RFULL(3)=DICTIONARY EQUATE PREFIX            
*                                                                               
         MVI   FVMINL,1            VALIDATE FIELD                               
         GOTO1 AFVAL,FHD                                                        
         BNE   VALDICX                                                          
         CLC   FVIFLD(3),RFULL     TEST USER ENTERED PREFIX                     
         BE    VDIC04                                                           
         CLI   FVILEN,5            NO - SEE IF CAN INSERT IT                    
         BNH   VDIC02                                                           
         MVC   FVMSGNO,=AL2(CE#EQPRE)                                           
         MVC   FVXTRA(3),RFULL                                                  
         B     VALDICX                                                          
VDIC02   MVC   FHDA(3),RFULL       PREFIX INPUT WITH THE PREFIX                 
         MVC   FHDA+3(5),FVIFLD                                                 
         MVC   FVIFLD(8),FHDA                                                   
*                                                                               
VDIC04   LA    R2,IOKEY            READ EQUATE NAME PASSIVE                     
         USING GMSGD,R2                                                         
         XC    GQKEY,GQKEY                                                      
         MVI   GQKREC,GQKRECQ                                                   
         MVC   GQKQNAME,FVIFLD                                                  
         GOTO1 AIO,IOHI+IOGENDIR                                                
         BNE   *+14                                                             
         CLC   GQKEY(GQKMNUM-GQKEY),IOKEYSAV                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#EQNOF)                                           
         B     VALDICX                                                          
         MVC   APHALF,GQKMNUM      RETURN REFERENCE # IN APHALF                 
         DROP  R2                                                               
*                                                                               
         MVI   RPARM,C'S'          SET UP DICTATE PARAMETER 1                   
         MVC   RPARM+1(1),RIPARM+4                                              
         MVC   RPARM+2(1),RIPARM+0                                              
         MVI   RPARM+3,C' '                                                     
         XC    APWORK,APWORK       SET UP DICTATE ESCAPE SEQUENCE               
         MVI   APWORK+0,CT#ESCL                                                 
         MVC   APWORK+1(2),APHALF  DISPLAY WORD                                 
         MVC   APWORK+3(1),RIPARM+3                                             
         GOTO1 VDICTAT,RPARM,,APWORK                                            
*                                                                               
VALDICX  B     ROUTSX                                                           
         SPACE 1                                                                
       ++INCLUDE DDPFXTBLE                                                      
         DS    0H                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO DISPLAY A DICTIONARY WORD                                *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON TO RETURN DICTIONARY EQUATE TEXT         *         
*                 = SYSTEM OVERLAY NUMBER                             *         
*               3 = L(WORD)                                           *         
*       P2 BYTE 0 = C'U' TO RETURN WORD IN UPPER CASE                 *         
*                   C'L' TO RETURN WORD IN LOWER CASE                 *         
*             1-3 = A(DICTIONARY REFERENCE #)                         *         
* EXIT: APWORK+00 = DICTIONARY WORD                                   *         
*       APWORK+64 = DICTIONARY EQUATE (IF REQUIRED)                   *         
***********************************************************************         
         SPACE 1                                                                
DISDIC   MVC   RIPARM(8),0(R1)     SAVE INPUT PARAMETERS                        
*                                                                               
         MVI   RPARM,C'S'          SET UP DICTATE PARAMETER 1                   
         MVC   RPARM+1(1),RIPARM+4                                              
         MVC   RPARM+2(1),RIPARM                                                
         NI    RPARM+2,X'FF'-X'80'                                              
         MVI   RPARM+3,C' '                                                     
*                                                                               
         XC    APWORK,APWORK       SET UP DICTATE ESCAPE SEQUENCE               
         MVI   APWORK,CT#ESCL                                                   
         MVC   APWORK+3(1),RIPARM+3                                             
         L     R1,RIPARM+4                                                      
         MVC   APWORK+1(2),0(R1)                                                
*                                                                               
         GOTO1 VDICTAT,RPARM,,APWORK                                            
*                                                                               
         TM    RIPARM,X'80'        TEST DICTIONARY EQUATE REQUIRED              
         BZ    ROUTSX                                                           
*                                                                               
         LA    R2,IOKEY                                                         
         USING GMSGD,R2                                                         
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVC   GMKSYS,RIPARM                                                    
         NI    GMKSYS,X'FF'-X'80'                                               
         MVI   GMKTYP,GMKTGDIC                                                  
         L     R1,RIPARM+4                                                      
         MVC   GMKMSG,0(R1)                                                     
         GOTO1 AIO,IOHI+IOGENDIR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,GMDDA                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOGET+IOGENFIL                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,GMFIRST(R2)                                                   
         USING GMQSYD,R3                                                        
         XR    RF,RF                                                            
         CLI   GMQSYEL,GMQSYELC                                                 
         BE    *+12                                                             
         IC    RF,GMQSYELL                                                      
         BXH   R3,RF,*-12                                                       
         MVC   APWORK+64(L'GMQSYSYM),GMQSYSYM                                   
         DROP  R3,R2                                                            
*                                                                               
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT BIT TABLE FROM ELEMENT                           *         
*                                                                     *         
* NTRY: P1=A(ELEMENT)                                                 *         
*       P2=(L(BIT TABLE), A(BIT TABLE))                               *         
***********************************************************************         
         SPACE 1                                                                
BLDBITT  LR    RF,R1               RF=A(PARAMETER LIST)                         
         LM    R1,R2,0(RF)         R1=A(ELEMENT), R2=A(BIT TABLE)               
         SPACE 1                                                                
         ICM   RE,1,4(RF)          RE=LENGTH OF BIT TABLE                       
         BNZ   *+8                                                              
         LA    RE,32               DEFAULT=32                                   
         BCTR  RE,0                                                             
         EX    RE,*+4              ZEROIZE BIT TABLE                            
         XC    0(0,R2),0(R2)                                                    
         SPACE 1                                                                
         LA    RF,SAMIXLNQ                                                      
         CLI   0(R1),SAMIXELQ      TEST RECORD/ACTIONS ELEMENT                  
         BE    BBITT2                                                           
         LA    RF,SAFCWLNQ                                                      
         CLI   0(R1),SAFCWELQ      TEST FCONTROL WRITE/READ ELEMENT             
         BE    BBITT2                                                           
         LA    RF,SAFCRLNQ                                                      
         CLI   0(R1),SAFCRELQ      TEST FCONTROL READ ELEMENT                   
         BE    BBITT2                                                           
         LA    RF,SAOCTLNQ                                                      
         CLI   0(R1),SAOCTELQ      TEST OCONTROL ELEMENT                        
         BE    BBITT2                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
BBITT2   XR    RE,RE               EXTRACT BIT TABLE                            
         IC    RE,1(R1)                                                         
         SR    RE,RF                                                            
         BZ    BLDBITTX                                                         
         BCTR  RE,0                                                             
         AR    RF,R1                                                            
         EX    RE,*+4                                                           
         MVC   0(0,R2),0(RF)                                                    
         SPACE 1                                                                
BLDBITTX B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TAG BIT TABLE ONTO END OF AN ELEMENT                     *         
*                                                                     *         
* NTRY: P1=A(ELEMENT)                                                 *         
*       P2=(L(BIT TABLE), A(BIT TABLE))                               *         
***********************************************************************         
         SPACE 1                                                                
TAGBITT  LR    RF,R1                                                            
         LM    R1,R2,0(RF)                                                      
         SPACE 1                                                                
         XC    RBITT,RBITT         COPY BIT TABLE TO RBITT                      
         ICM   RE,1,4(RF)                                                       
         BNZ   *+8                                                              
         LA    RE,32                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   RBITT(0),0(R2)                                                   
         SPACE 1                                                                
         LA    RF,SAMIXLNQ(R1)                                                  
         CLI   0(R1),SAMIXELQ      TEST RECORD/ACTIONS ELEMENT                  
         BE    TBITT2                                                           
         LA    RF,SAFCWLNQ(R1)                                                  
         CLI   0(R1),SAFCWELQ      TEST FCONTROL WRITE/READ ELEMENT             
         BE    TBITT2                                                           
         LA    RF,SAFCRLNQ(R1)                                                  
         CLI   0(R1),SAFCRELQ      TEST FCONTROL READ ELEMENT                   
         BE    TBITT2                                                           
         LA    RF,SAOCTLNQ(R1)                                                  
         CLI   0(R1),SAOCTELQ      TEST OCONTROL ELEMENT                        
         BE    TBITT2                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
TBITT2   OC    RBITT,RBITT         TEST NO BITS SET                             
         BZ    TBITT4                                                           
         MVC   0(L'RBITT,RF),RBITT                                              
         LA    RF,L'RBITT-1(RF)    FIND END OF ELEMENT                          
         CLI   0(RF),0                                                          
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
TBITT4   SR    RF,R1               SET LENGTH OF ELEMENT                        
         STC   RF,1(R1)                                                         
         SPACE 1                                                                
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NAME FROM PERSONAL-ID RECORD                         *         
*                                                                     *         
* NTRY: R1=A(PERSONAL-ID)                                             *         
* EXIT: APWORK=NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETPNAM  EQU   *                                                                
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(PERSON RECORD)                          
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   SAPEAGY,OPTAGY                                                   
         B     *+10                                                             
         MVC   SAPEAGY,CUAALF                                                   
         MVC   SAPEPID,0(R1)       GET PERSONAL ID                              
*                                  GET TODAYS DATE COMPLEMENT                   
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',APHALF)                          
         MVC   SAPEDEF,APHALF                                                   
         XC    SAPEDEF,=XL2'FFFF'                                               
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOHI+IOCONFIL                                                
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     ROUTSX                                                           
         BNE   *+14                                                             
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEREC                                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     ROUTSX                                                           
*                                                                               
         XC    APWORK,APWORK                                                    
         LA    R3,SAPEDATA                                                      
*                                                                               
GPNA010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SANAMELQ                                                   
         BE    GPNA020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GPNA010                                                          
GPNA020  GOTO1 ADISPNAM,(R3)                                                    
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT PERSON NAME FOR DISPLAY                           *         
*                                                                     *         
* NTRY: R1=A(NAME ELEMENT)                                            *         
* EXIT: APWORK=NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISPNAM  LR    R3,R1                                                            
         USING SANAMD,R3                                                        
         XC    APWORK,APWORK                                                    
         LA    RE,SANAMELN                                                      
         LA    RF,APWORK                                                        
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN                                                
         BZ    DPNA010                                                          
         MVC   0(1,RF),1(RE)                                                    
         MVI   1(RF),C' '                                                       
         LA    RF,2(RF)                                                         
         IC    R1,0(RE)                                                         
         LA    RE,1(R1,RE)                                                      
*                                                                               
DPNA010  TM    SANAMIND,SANAMIMN                                                
         BNZ   DPNA012                                                          
         MVI   0(RF),C' '                                                       
         B     DPNA014                                                          
DPNA012  MVC   0(1,RF),1(RE)                                                    
         IC    R1,0(RE)                                                         
         LA    RE,1(R1,RE)                                                      
DPNA014  MVI   1(RF),C' '                                                       
         LA    RF,2(RF)                                                         
*                                                                               
DPNA020  TM    SANAMIND,SANAMILN                                                
         BZ    DPNA030                                                          
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RE)                                                    
*                                                                               
DPNA030  B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PASSWORD NUMBER FROM PERSONAL-ID RECORD              *         
*                                                                     *         
* NTRY: P1=A(PERSONAL ID)                                             *         
* EXIT: APHALF=PASSWORD NUMBER, APWORK=PASSWORD CODE                  *         
***********************************************************************         
         SPACE 1                                                                
GETPNUM  EQU   *                                                                
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(PERSON RECORD)                          
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   SAPEAGY,OPTAGY                                                   
         B     *+10                                                             
         MVC   SAPEAGY,CUAALF                                                   
         MVC   SAPEPID,0(R1)       GET PERSONAL ID                              
*                                  GET TODAYS DATE COMPLEMENT                   
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',APHALF)                          
         MVC   SAPEDEF,APHALF                                                   
         XC    SAPEDEF,=XL2'FFFF'                                               
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOHI+IOCONFIL                                                
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     ROUTSX                                                           
         BNE   *+14                                                             
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEREC                                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     ROUTSX                                                           
*                                                                               
         XC    APWORK,APWORK                                                    
         LA    R3,SAPEDATA                                                      
*                                                                               
GPNU010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SAPWDELQ                                                   
         BE    GPNU020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GPNU010                                                          
         USING SAPWDD,R3                                                        
GPNU020  MVC   APWORK(L'SAPWDCOD),SAPWDCOD                                      
         MVC   APHALF(L'SAPWDNUM),SAPWDNUM                                      
         B     ROUTSX                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET DEPARTMENT NAME FROM DEPARTMENT RECORD               *         
*                                                                     *         
* NTRY: P1=A(DEPARTMENT RECORD)                                       *         
* EXIT: APWORK=NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETDNAM  XC    APWORK,APWORK                                                    
         LA    R3,SADPDATA-SADPREC(R1)                                          
*                                                                               
GDNA010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SADPTELQ                                                   
         BE    GDNA020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GDNA010                                                          
         USING SADPTD,R3                                                        
GDNA020  ZIC   R1,SADPTLN                                                       
         SH    R1,=Y(SADPTLNQ+1)                                                
         BM    ROUTSX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SADPTNAM                                               
         B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET OFFICE NAME FROM OFFICE RECORD                       *         
*                                                                     *         
* NTRY: P1=A(OFFICE RECORD)                                           *         
* EXIT: APWORK=NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETONAM  XC    APWORK,APWORK                                                    
         LA    R3,SAOFDATA-SAOFREC(R1)                                          
*                                                                               
GONA010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SAOFFELQ                                                   
         BE    GONA020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GONA010                                                          
         USING SAOFFD,R3                                                        
GONA020  ZIC   R1,SAOFFLN                                                       
         SH    R1,=Y(SAOFFLNQ+1)                                                
         BM    ROUTSX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SAOFFNAM                                               
         B     ROUTSX                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET ACCESS GROUP NAME FROM ACCESS GROUP RECORD           *         
*                                                                     *         
* NTRY: P1=A(ACCESS GROUP RECORD)                                     *         
* EXIT: APWORK=NAME APHALF=NUMBER                                     *         
***********************************************************************         
         SPACE 1                                                                
GETGNAM  XC    APWORK,APWORK                                                    
         LA    R3,SAAGDATA-SAAGREC(R1)                                          
*                                                                               
GGNA010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SAAGNELQ                                                   
         BE    GGNA020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GGNA010                                                          
         USING SAAGND,R3                                                        
GGNA020  ZIC   R1,SAAGNLN                                                       
         SH    R1,=Y(SAAGNLNQ+1)                                                
         BM    GGNA030                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SAAGNNAM                                               
GGNA030  MVC   APHALF,SAAGNNUM                                                  
         B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET LIMIT ACCESS GROUP NAME FROM LIMIT ACCESS GROUP REC  *         
*                                                                     *         
* NTRY: P1=A(LIMIT ACCESS GROUP RECORD)                               *         
* EXIT: APWORK=NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETLNAM  XC    APWORK,APWORK                                                    
         LA    R3,SALADATA-SALAREC(R1)                                          
*                                                                               
GLNA010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SALANELQ                                                   
         BE    GLNA020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GLNA010                                                          
         USING SALAND,R3                                                        
GLNA020  ZIC   R1,SALANLN                                                       
         SH    R1,=Y(SALANLNQ+1)                                                
         BM    ROUTSX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SALANNAM                                               
         B     ROUTSX                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET PERSONAL-ID GIVEN PASSWORD NUMBER                    *         
*                                                                     *         
* NTRY: R1=A(PASSWORD NUMBER)                                         *         
* EXIT: APWORK=PERSONAL-ID                                            *         
***********************************************************************         
         SPACE 1                                                                
GETPID   EQU   *                                                                
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           R2=A(PASSWORD RECORD)                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   SA0KAGY,OPTAGY                                                   
         B     *+10                                                             
         MVC   SA0KAGY,CUAALF                                                   
         MVC   SA0KNUM,0(R1)                                                    
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     ROUTSX                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     ROUTSX                                                           
*                                                                               
         XC    APWORK,APWORK                                                    
         LA    R3,SA0DATA                                                       
*                                                                               
GPID010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SAPALELQ                                                   
         BE    GPID020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GPID010                                                          
         USING SAPALD,R3                                                        
GPID020  MVC   APWORK(L'SAPALPID),SAPALPID                                      
         B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*   BUILD LIMIT ACCESS IN APWORK                                      *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(LIMIT ACCESS INTERNAL CODE) *         
* EXIT - APWORK+0(6)=LIMIT ACCESS CODE                                *         
***********************************************************************         
         SPACE 1                                                                
DISLACC  MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R3,0(R1)            POINT TO LIMIT ACCESS CODE                   
         XC    APWORK,APWORK                                                    
         OC    0(L'SASYSLMT,R3),0(R3)                                           
         BZ    DLACCX                                                           
         MVC   APWORK(L'SASYSLMT),0(R3)                                         
*                                                                               
*&&UK                                                                           
         CLI   RWORK,4             TEST UK/MEDIA                                
         BNE   DLACC30                                                          
         LA    RE,APWORK                                                        
         LR    R4,R3                                                            
         LA    R2,4                                                             
DLACC10  SR    R0,R0                255,255,99,99 MAX VALUES                    
         IC    R0,0(R4)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,ZERO=NOBLANK,                 *        
               WRK=RWORK,DUB=RDUB                                               
         AR    RE,R0     '                                                      
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,DLACC10                                                       
         BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         B     DLACCX                                                           
*&&                                                                             
*&&US                                                                           
         CLI   RWORK,3             TEST NETWORK                                 
         BE    *+12                                                             
         CLI   RWORK,2             TEST SPOT                                    
         BNE   DLACC20                                                          
*                                                                               
         MVC   APPARM+4(4),=X'D9000A15' CLUNPK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R3),APWORK                                            
*                                                                               
DLACC20  EQU   *                                                                
*&&                                                                             
DLACC30  CLI   RWORK,14            TEST PERSONNEL                               
         BNE   DLACCX                                                           
         XC    APWORK,APWORK                                                    
         OC    2(2,R3),2(R3)                                                    
         BZ    DLACCX                                                           
         LA    RE,APWORK           FORMAT N(NN)-N(NN)                           
         ZIC   R0,2(R3)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,WRK=RWORK,DUB=RDUB                     
         AR    RE,R0                                                            
         MVI   0(RE),C'-'                                                       
         ZIC   R0,3(R3)                                                         
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT,WRK=RWORK,DUB=RDUB                     
*                                                                               
DLACCX   B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
*   VALIDATE LIMIT ACCESS FIELD IN FVAREA                             *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(LIMIT ACCESS INTERNAL CODE) *         
* EXIT - CC=EQUAL IF LIMIT ACCESS VALID, NOT EQUAL IF INVALID         *         
***********************************************************************         
         SPACE 1                                                                
VALLACC  MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R3,0(R1)            POINT TO LIMIT ACCESS CODE                   
         MVC   0(L'SASYSLMT,R3),FVIFLD                                          
         CLI   RWORK,14            TEST PERSONNEL SYSTEM                        
         BNE   VLAC10                                                           
         XC    0(L'SASYSLMT,R3),0(R3)                                           
         GOTO1 VSCANNER,APPARM,FVIHDR,(2,RBLOCK),C',=*-'                        
         XC    8(4,R1),8(R1)       CLEAR SPECIAL SCAN CHARACTERS                
         CLI   4(R1),0                                                          
         BE    VAEIIF                                                           
         CLI   4(R1),1             FORMAT=N(NN)-N(NN)                           
         BNE   VAEIIF                                                           
         CLI   RBLOCK,0            VALIDATE LOWER LIMIT                         
         BE    VAEIIF                                                           
         TM    RBLOCK+2,X'80'                                                   
         BZ    VAEFNN                                                           
         OC    RBLOCK+4(4),RBLOCK+4                                             
         BZ    VAEIIF                                                           
         OC    RBLOCK+4(3),RBLOCK+4                                             
         BNZ   VAEIIF                                                           
         MVC   2(1,R3),RBLOCK+7                                                 
         MVC   3(1,R3),RBLOCK+7                                                 
         CLI   RBLOCK+1,0          TEST UPPER LIMIT GIVEN                       
         BE    VLACX                                                            
         MVI   FVINDX,2                                                         
         TM    RBLOCK+3,X'80'      VALIDATE UPPER LIMIT                         
         BZ    VAEFNN                                                           
         OC    RBLOCK+8(4),RBLOCK+8                                             
         BZ    VAEIIF                                                           
         OC    RBLOCK+8(3),RBLOCK+8                                             
         BNZ   VAEIIF                                                           
         MVC   3(1,R3),RBLOCK+11                                                
         CLC   3(1,R3),2(R3)                                                    
         BL    VAEIIF                                                           
         B     VLACX                                                            
VLAC10   DS    0H                                                               
*&&UK                                                                           
         CLI   RWORK,4             TEST UK/MEDIA                                
         BNE   VLACX                                                            
         GOTO1 VSCANNER,APPARM,FVIHDR,(5,RBLOCK),X'6B5E6BFF'                    
         CLI   4(R1),4             FORMAT=N(NN),N(NN),N(N),N(N)                 
         BNE   VAEIIF                                                           
         LA    RF,RBLOCK                                                        
         LR    R1,R3                                                            
         LA    R0,4                                                             
         LA    RE,1                                                             
VLAC20   STC   RE,FVINDX                                                        
         CLI   0(RF),0                                                          
         BE    VAEMIF                                                           
         CLI   0(RF),3                                                          
         BH    VAEFTL                                                           
         TM    2(RF),X'80'                                                      
         BZ    VAEFNN                                                           
         CLI   FVINDX,2            MAXVALUES=255,255,99,99                      
         BH    VLAC22                                                           
         CLC   6(2,RF),=H'255'                                                  
         BH    VAEFTB                                                           
         B     VLAC24                                                           
VLAC22   CLC   6(2,RF),=H'99'                                                   
         BH    VAEFTB                                                           
VLAC24   MVC   0(1,R1),7(RF)                                                    
         LA    RF,L'RBLOCK(RF)                                                  
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,VLAC20                                                        
         MVI   FVINDX,0                                                         
         B     VLACX                                                            
*&&                                                                             
*&&US                                                                           
         CLI   RWORK,3             TEST NETWORK                                 
         BE    *+12                                                             
         CLI   RWORK,2             TEST SPOT                                    
         BNE   VLAC30                                                           
*                                                                               
         MVC   APPARM+4(4),=X'D9000A14' CLPACK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),FVIFLD,(R3)                                            
         CLI   0(R1),0                                                          
         BNE   VAEIIF                                                           
*                                                                               
VLAC30   EQU   *                                                                
*&&                                                                             
*                                                                               
VLACX    B     ROUTSX                                                           
*                                  GETTXT MESSAGE # ERROR EXITS                 
VAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ROUTSX              INPUT FIELD ERROR                            
VAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     ROUTSX              INPUT FIELD TOO LONG                         
VAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     ROUTSX              INPUT FIELD NOT NUMERIC                      
VAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     ROUTSX              INPUT FIELD TOO SHORT                        
VAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ROUTSX              INPUT FIELD ERROR                            
VAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     ROUTSX              MISSING FIELD                                
VAEFTB   MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX              FIELD VALUE EXCEEDS MAXIMUM                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER ON A TEXT STRING                                  *         
* NTRY -                                                              *         
* PARAM1 BYTE0    - L'FILTER TEXT                                     *         
*        BYTE1-3  - A(FILTER TEXT)                                    *         
* PARAM2 BYTE0    - L'TEXT TO FILTER ON                               *         
*        BYTE1-3  - A(TEXT TO FILTER ON)                              *         
* TEXT FORMAT  ABC A*C (ABC  '*' = WILDCARD '(' = SCAN FOR TEXT       *         
*                                                                     *         
* EXIT - CC  =  IF TEXT FOUND                                         *         
*        CC  <> IF TEXT NOT FOUND                                     *         
***********************************************************************         
         SPACE 1                                                                
TXTFLT   L     R4,0(R1)            R4=A(TEXT TO FILTER WITH)                    
         L     R3,4(R1)            R3=A(TEXT TO FILTER ON)                      
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)          RF=L'FILTER TEXT                             
         BZ    TXTFLTOK            NO FILTER                                    
         LR    R1,R3               GET L'FIELD TO FILTER ON                     
         SRL   R1,24                                                            
         CLM   R1,1,=AL1(L'ACWORK) PREVENT OVERFLOW                             
         BNH   *+8                                                              
         LA    R1,L'ACWORK                                                      
*                                                                               
         MVI   ACWORK,C' '         CONVERT FIELD TO FILTER ON TO U/C            
         MVC   ACWORK+1(L'ACWORK-1),ACWORK                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    ACWORK(0),0(R3)                                                  
         LA    R3,ACWORK                                                        
         LA    R1,1(R1)            SET BACK TO FULL LENGTH                      
         CLI   0(R4),C'('          CONVENTION TO SCAN FOR FILTER                
         BE    TXTF020                                                          
         CR    R1,RF               TEST FIELD LONG ENOUGH TO BOTHER             
         BL    TXTFLTNE                                                         
         LR    RE,R4               RE=A(FILTER TEXT)                            
         SPACE 1                                                                
TXTF010  CLI   0(RE),C'*'          IGNORE WILDCARD CHARACTERS                   
         BE    *+14                                                             
         CLC   0(1,RE),0(R3)                                                    
         BNE   TXTFLTNE                                                         
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT CHAR IN FILTER AND TEXT         
         LA    RE,1(RE)                                                         
         BCT   RF,TXTF010                                                       
         B     TXTFLTOK            FILTER MATCHES DATA                          
         SPACE 1                                                                
TXTF020  SH    RF,=Y(2)            DROP THE '(' AND GET EX L'FILTER             
         BM    TXTFLTNE                                                         
         LR    RE,R1                                                            
         SR    RE,RF               RE=NUMBER OF COMPARES REQUIRED               
         BNP   TXTFLTNE            TEXT NOT LONG ENOUGH TO BOTHER               
         SPACE 1                                                                
TXTF030  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),1(R4)                                                    
         BE    TXTFLTOK                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,TXTF030                                                       
         SPACE 1                                                                
TXTFLTNE MVC   FVMSGNO,=AL2(FVFNOTV) FORCE CC NOT EQUAL (NOT FOUND)             
*                                                                               
TXTFLTOK B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD CONNECTABLE SYSTEM & USER-ID LISTS                            *         
***********************************************************************         
         SPACE 1                                                                
SETCON   LH    R4,=Y(CONNECT-TWAD)                                              
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
*                                  TEMPORARY BYPAS TILL SPOOF FIXED             
*        CLI   ASONOFF,ASOFF       CHECK IF OFFLINE                             
*        BE    ROUTSX                                                           
*                                                                               
         LA    R2,IOKEY            CHECK NOT ATTACHED TO SECURITY AGY           
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,CUAALF                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA1                                                      
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         SR    RF,RF                                                            
*                                                                               
SCON010  CLI   0(R3),0                                                          
         BE    SCON020             IF NOT FOUND CONTINUE OK                     
         CLI   0(R3),CTSEAELQ                                                   
         BE    SCONERR             IF FOUND CANT ACCESS SECURITY PROG           
*                                                                               
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     SCON010                                                          
*                                                                               
SCON020  EQU   *                                                                
         GOTO1 SETOVF,(R4)         SET SECURITY MANAGER OVERRIDE FLAG           
         BNE   SETCONX                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           R2=A(PASSWORD RECORD)                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUAALF                                                   
         MVC   SA0KNUM,CUPASS                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   SCONERR                                                          
         L     R2,AIOAREA1                                                      
*                                                                               
         XR    RE,RE                                                            
         LA    R3,SA0DATA                                                       
         USING SASYSD,R3           R3=A(SYSTEM AUTH. ELEMENT)                   
SCON02   CLI   SASYSEL,0                                                        
         BE    SCON10                                                           
         CLI   SASYSEL,SASYSELQ                                                 
         BNE   SCON08                                                           
*                                                                               
         OC    SASYSALL,SASYSALL   TEST NOTHING ALLOWED                         
         BNZ   *+12                                                             
         CLI   SASYSLN,SASYSLNQ                                                 
         BE    SCON08                                                           
         LA    RF,CSYSLST(RE)                                                   
         MVC   0(1,RF),SASYSNUM                                                 
         LA    RE,1(RE)                                                         
*                                                                               
SCON08   XR    RF,RF                                                            
         IC    RF,SASYSLN                                                       
         BXH   R3,RF,SCON02                                                     
*                                                                               
SCON10   STC   RE,C#SYS            SET NUMBER OF VALID SYSTEMS                  
*                                                                               
         GOTO1 GETIDS,C#UID                                                     
*                                                                               
         LA    R2,IOKEY                                                         
         USING CTTREC,R2           R2=A(TERMINAL RECORD)                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,CUADDR                                                   
         GOTO1 GETIDS,RIO                                                       
         BE    SETCONX             ALL IDS VALID FOR TERMINAL                   
*                                                                               
         CLI   C#UID,C#UIDALL      IF ALL VALID FOR PERSON                      
         BNE   SCON12                                                           
         MVC   C#UID,RIO             COPY TERMINAL LIST                         
         MVC   CUIDLST(CUIDLSTN),RIO+1                                          
         MVC   CUIDLST+CUIDLSTN(CUIDLSTN),RIO+1+CUIDLSTN                        
         B     SETCONX                                                          
*                                                                               
SCON12   LA    R1,CUIDLST          R1=A(USER-IDS FOR PERSON)                    
         XR    R0,R0                                                            
         IC    R0,C#UID            R0=NUMBER OF USER-IDS FOR PERSON             
         XR    RE,RE                                                            
SCON14   LA    RF,RIO+1            RF=A(USER-IDS FOR TERMINAL)                  
         IC    RE,RIO              RE=NUMBER OF USER-IDS FOR TERMINAL           
SCON16   CLC   0(L'CTIKNUM,RF),0(R1)                                            
         BE    SCON18                                                           
         LA    RF,L'CTIKNUM(RF)                                                 
         BCT   RE,SCON16                                                        
         XC    0(L'CTIKNUM,R1),0(R1) DELETE IF USER-ID NOT IN BOTH              
SCON18   LA    R1,L'CTIKNUM(R1)                                                 
         BCT   R0,SCON14                                                        
         B     SETCONX                                                          
*                                  INVALID CONNECT AGENCY AUTH                  
SCONERR  MVC   FVMSGNO,=AL2(CE#PACIN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
*                                                                               
SETCONX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ROUTSX                                                           
         SPACE 1                                                                
         DROP  R2,R3                                                            
         SPACE 3                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET ID LIST                                                         *         
*                                                                     *         
* NTRY: R1=A(OUTPUT)                                                  *         
*       IOKEY=KEY OF RECORD                                           *         
* EXIT: OUTPUT+0 =NO OF USER-IDS IN LIST                              *         
*       OUPTUT+1=USER-ID LIST                                         *         
*       CC=ZERO IF ALL USER-IDS ARE VALID                             *         
***********************************************************************         
         SPACE 1                                                                
GETIDS   NTR1  ,                                                                
         LR    R6,R1               R6=A(OUTPUT)                                 
         USING C#UID,R6                                                         
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1         R2=A(RECORD)                                 
         GOTO1 VGETIDS,RPARM,(C'C',(R2)),ATIA,VDMGR                             
         CLI   RPARM,X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   C#UID,RPARM                                                      
*                                                                               
         CLI   C#UID,0             TEST LIST COUNT IS ZERO                      
         BNE   GETID10                                                          
         CLI   0(R2),CT0KTEQU      FOR AUTH. RECORD NONE=ALL                    
         BE    GETIDALL                                                         
         CLI   0(R2),CTIKTYPQ      FOR ID RECORD NONE=NONE                      
         BE    GETIDX                                                           
         CLI   0(R2),CTTKTYPQ      FOR TERMINAL RECORD                          
         BE    *+6                 NONE=COMPATABLE ID'S OF PRINCIPLE ID         
         DC    H'0'                                                             
         LA    RF,CTTDATA-CTTREC(R2)                                            
         USING CTPIDD,RF           RF=A(PRINCIPLE ID ELEMENT)                   
         XR    RE,RE                                                            
GETID02  CLI   CTPIDEL,0                                                        
         BE    GETIDX                                                           
         CLI   CTPIDEL,CTPIDELQ                                                 
         BE    *+12                                                             
         IC    RE,CTPIDLEN                                                      
         BXH   RF,RE,GETID02                                                    
         MVC   RWORK(L'CTPID),CTPID                                             
         LA    RF,IOKEY                                                         
         USING CTIREC,RF           RF=A(ID RECORD KEY)                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,RWORK                                                     
         DROP  RF                                                               
         GOTO1 GETIDS,C#UID                                                     
         XR    RE,RE               ADD PRINCIPLE ID TO LIST                     
         IC    RE,C#UID                                                         
         LA    RE,1(RE)                                                         
         STC   RE,C#UID                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'CTIKID+2)                                                
         L     RF,ATIA                                                          
         AR    RE,RF                                                            
         MVC   0(L'CTIKID,RE),RWORK                                             
*                                                                               
GETID10  MVI   RFLAG,0                                                          
         CLI   0(R2),CTTKTYPQ      TEST TERMINAL RECORD                         
         BNE   GETID14                                                          
         XR    RF,RF                                                            
         LA    R1,CTTDATA-CTTREC(R2)                                            
         USING CTTRMD,R1           R1=A(TERMINAL DEFN. ELEMENT)                 
GETID12  CLI   CTTRMEL,0                                                        
         BE    GETID14                                                          
         CLI   CTTRMEL,CTTRMELQ                                                 
         BE    *+12                                                             
         IC    RF,CTTRMLEN                                                      
         BXH   R1,RF,GETID12                                                    
         MVC   RFLAG,CTTRMOFC      SET OFFICE CODE                              
         DROP  R1                                                               
*                                                                               
GETID14  XR    R0,R0               CONVERT UID NAME LIST TO NUMBER LIST         
         IC    R0,C#UID                                                         
         LA    R3,CUIDLST          R3=A(NUMBER LIST)                            
         L     R4,ATIA             R4=A(NAME LIST)                              
*                                                                               
GETID16  CLI   RFLAG,C'*'          TEST TERMINAL RECORD & DDS TERMINAL          
         BNE   *+14                                                             
         CLC   0(L'CTIKID,R4),ALLIDS  TEST 'ALL' IDS                            
         BE    GETIDALL                                                         
*                                                                               
         GOTO1 GETUID,(R4)                                                      
         BNE   GETID18                                                          
         MVC   0(L'CTIKNUM,R3),0(R1)                                            
         LA    R3,L'CTIKNUM(R3)                                                 
GETID18  LA    R4,L'CTIKID+2(R4)                                                
         BCT   R0,GETID16                                                       
*                                                                               
GETIDX   LTR   RB,RB                                                            
         B     EXIT                                                             
GETIDALL MVI   C#UID,C#UIDALL      ALL IDS VALID                                
         CR    RB,RB                                                            
         B     EXIT                                                             
         SPACE 1                                                                
ALLIDS   DC    CL10'ALL'                                                        
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GET USER-ID NUMBER FROM NAME                                        *         
*                                                                     *         
* NTRY: R1=A(NAME)                                                    *         
* EXIT: R1=A(NUMBER)                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETUID   NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING SAIREC,R2                                                        
         XC    SAIKEY,SAIKEY                                                    
         MVI   SAIKTYP,SAIKTYPQ                                                 
         MVC   SAIKID,0(R1)                                                     
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNE   GETUIDN                                                          
         L     R2,AIOAREA3                                                      
         LA    R3,SAIDATA                                                       
         USING SADSCD,R3                                                        
         XR    RF,RF                                                            
GUID02   CLI   SADSCEL,0                                                        
         BE    GETUIDN                                                          
         CLI   SADSCEL,SADSCELQ                                                 
         BE    GUID04                                                           
         IC    RF,SADSCLEN                                                      
         BXH   R3,RF,GUID02                                                     
*                                                                               
GUID04   LA    R1,SADSC                                                         
         CR    RB,RB                                                            
         B     *+6                                                              
GETUIDN  LTR   RB,RB                                                            
         XIT1  REGS=(R1)                                                        
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET SECURITY MANAGER OVERRIDE FLAG                                  *         
*                                                                     *         
* NTRY: R1=A(CONNECT DATA GLOBAL WORK AREA)                           *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
*       COVFLAG SET TO VALUE FROM PERSON RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
SETOVF   NTR1  ,                                                                
         LR    R4,R1                                                            
         USING CONNECT,R4                                                       
         MVI   COVFLAG,0                                                        
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           R2=A(PASSWORD RECORD)                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUAALF                                                   
         MVC   SA0KNUM,CUPASS                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   SOVFNO                                                           
         L     R2,AIOAREA1                                                      
         LA    R3,SA0DATA                                                       
         USING SAPALD,R3           R3=A(SYSTEM AUTH. ELEMENT)                   
*                                                                               
SOVF010  CLI   SAPALEL,0                                                        
         BE    SOVFOK                                                           
         CLI   SAPALEL,SAPALELQ                                                 
         BE    SOVF020                                                          
         SR    RF,RF                                                            
         ZIC   RF,SAPALLN                                                       
         AR    R3,RF                                                            
         B     SOVF010                                                          
         DROP  R2                                                               
*                                                                               
SOVF020  EQU   *                                                                
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(PERSON RECORD)                          
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   SAPEAGY,OPTAGY                                                   
         B     *+10                                                             
         MVC   SAPEAGY,CUAALF                                                   
         MVC   SAPEPID,SAPALPID    GET PERSONAL ID                              
         DROP  R3                                                               
*                                  GET TODAYS DATE COMPLEMENT                   
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',APHALF)                          
         MVC   SAPEDEF,APHALF                                                   
         XC    SAPEDEF,=XL2'FFFF'                                               
         GOTO1 AIO,IOHI+IOCONFIL+IO1                                            
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     SOVFNO                                                           
         BNE   *+14                                                             
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEREC                                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     SOVFNO                                                           
*                                                                               
         XC    APWORK,APWORK                                                    
         L     R2,AIOAREA1                                                      
         LA    R3,SAPEDATA                                                      
         USING SAPERD,R3                                                        
*                                                                               
SOVF030  CLI   SAPEREL,0                                                        
         BE    SOVFOK                                                           
         CLI   SAPEREL,SAPERELQ                                                 
         BE    SOVF040                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     SOVF030                                                          
SOVF040  MVC   COVFLAG,SAPEROVF                                                 
         B     SOVFOK                                                           
*                                                                               
SOVFNO   LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
SOVFOK   CR    RB,RB                                                            
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* TEST USER-ID IN LIST                                                *         
***********************************************************************         
         SPACE 1                                                                
TSTUID   OC    OPTAGY,OPTAGY                                                    
         BNZ   ROUTSX                                                           
*                                  TEMPORARY BYPAS TILL SPOOF FIXED             
*        CLI   ASONOFF,ASOFF       CHECK IF OFFLINE                             
*        BE    ROUTSX                                                           
*                                                                               
         LH    R4,=Y(CONNECT-TWAD)                                              
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
*                                                                               
         CLI   C#UID,C#UIDALL      TEST ALL USER-IDS ARE ALLOWED                
         BE    ROUTSX                                                           
         OC    0(L'CUIDLST,R1),0(R1)                                            
         BZ    ROUTSX                                                           
         XR    R0,R0                                                            
         IC    R0,C#UID                                                         
         LA    R2,CUIDLST                                                       
*                                                                               
TUID02   CLC   0(L'CUIDLST,R2),0(R1)                                            
         BE    TSTUIDX                                                          
         LA    R2,L'CUIDLST(R2)                                                 
         BCT   R0,TUID02                                                        
         MVC   FVMSGNO,=AL2(CE#NCUID)                                           
TSTUIDX  B     ROUTSX                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TEST SYSTEM IN LIST                                                 *         
*                                                                     *         
* NTRY: R1=A(SYSTEM NUMBER)                                           *         
***********************************************************************         
         SPACE 1                                                                
TSTSYS   LH    R4,=Y(CONNECT-TWAD)                                              
         A     R4,ATWA                                                          
*                                  TEMPORARY BYPAS TILL SPOOF FIXED             
*        CLI   ASONOFF,ASOFF       CHECK IF OFFLINE                             
*        BE    ROUTSX                                                           
*                                                                               
         USING CONNECT,R4                                                       
         LA    R2,CSYSLST                                                       
         XR    R0,R0                                                            
         ICM   R0,1,C#SYS                                                       
         BZ    TSTSYSN                                                          
TSYS02   CLC   0(L'CSYSLST,R2),0(R1)                                            
         BE    TSTSYSX                                                          
         LA    R2,L'CSYSLST(R2)                                                 
         BCT   R0,TSYS02                                                        
TSTSYSN  MVC   FVMSGNO,=AL2(CE#NCSYS)                                           
TSTSYSX  B     ROUTSX                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE CONNECTED USER SYSTEM ELEMENT FOR GIVEN SYSTEM                 *         
*                                                                     *         
* NTRY: R1=A(SYSTEM NUMBER)                                           *         
***********************************************************************         
         SPACE 1                                                                
SETSEL   MVC   RBYTE,0(R1)         GET SYSTEM NUMBER                            
         LH    R4,=Y(CONNECT-TWAD)                                              
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
         XC    CSYSEL,CSYSEL                                                    
         MVC   APHALF,CUUSER       FOR UK USE CONNECTED USER ID                 
*&&US                                                                           
         LA    R2,IOKEY            FOR US ONLY USE PRINCIPLE USER ID            
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,CUAALF                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         SR    RF,RF                                                            
*                                                                               
SSEL002  CLI   0(R3),0                                                          
         BE    SSEL006             IF NOT FOUND STICK WITH CONNECT ID           
         CLI   0(R3),X'02'                                                      
         BE    SSEL004             PRINCIPLE ID ELEMENT FOUND                   
*                                                                               
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     SSEL002                                                          
*                                                                               
SSEL004  MVC   APHALF,2(R3)        SAVE PRINCIPLE USER ID #                     
SSEL006  EQU   *                                                                
*&&                                                                             
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAIREC,R2           R2=A(ID RECORD)                              
         XC    SAIKEY,SAIKEY                                                    
         MVI   SAIKTYP,SAIKTYPQ                                                 
         MVC   SAIKNUM,APHALF                                                   
         OC    OPTUSE,OPTUSE                                                    
         BZ    SSEL010                                                          
         MVC   SAIKNUM,OPTUSE                                                   
*                                                                               
SSEL010  GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R3,SAIDATA          R3=A(FIRST ELEMENT)                          
         USING SASYSD,R3           R3=A(SYSTEM AUTH. ELEMENT)                   
SSEL020  CLI   SASYSEL,0                                                        
         BE    SSELX                                                            
         CLI   SASYSEL,SASYSELQ                                                 
         BNE   SSEL030                                                          
         CLC   SASYSNUM,RBYTE                                                   
         BE    SSEL040                                                          
*                                                                               
SSEL030  XR    RF,RF                                                            
         IC    RF,SASYSLN                                                       
         BXH   R3,RF,SSEL020                                                    
*                                                                               
SSEL040  XR    RF,RF               SAVE SYSTEM ELEMENT                          
         IC    RF,SASYSLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CSYSEL(0),SASYSEL                                                
         B     SSELX                                                            
*                                                                               
SSELX    B     ROUTSX                                                           
         SPACE 1                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* TEST PROGRAM AUTHORISATION IN SAVED SYSTEM ELEMENT                  *         
*                                                                     *         
* NTRY: R1=A(PROGRAM NUMBER)                                          *         
* EXIT - APWORK+0(2)=AUTHORISATION CODE                               *         
***********************************************************************         
         SPACE 1                                                                
TSTPGM   MVC   RBYTE,0(R1)         GET PROGRAM CODE                             
         LH    R4,=Y(CONNECT-TWAD)                                              
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
*                                                                               
         XC    APWORK,APWORK                                                    
         LA    R3,CSYSEL           GET A(SAVED SYSTEM ELEMENT)                  
         USING SASYSD,R3                                                        
         OC    CSYSEL,CSYSEL                                                    
         BZ    TPGM200                                                          
         MVC   APWORK(2),SASYSALL                                               
         LA    R1,SASYSPGM                                                      
         ZIC   RE,SASYSLN                                                       
*                                                                               
TPGM010  CH    RE,=H'16'                                                        
         BNH   TPGM100                                                          
         CLC   RBYTE,0(R1)                                                      
         BE    TPGM020                                                          
         LA    R1,3(R1)            GET NEXT PROGAM CODE                         
         SH    RE,=H'3'                                                         
         B     TPGM010                                                          
*                                                                               
TPGM020  MVC   APWORK(2),1(R1)     SAVE ACCESS CODE                             
*                                                                               
TPGM100  OC    APWORK(2),APWORK    CHECK IF ACCESS=NO                           
         BNZ   TPGMX                                                            
         MVC   FVMSGNO,=AL2(CE#PGNVS)                                           
         B     TPGMX                                                            
*                                  NO SYSTEM ELEMENT SET UP                     
TPGM200  MVC   FVMSGNO,=AL2(CE#NCSYS)                                           
         B     TPGMX                                                            
*                                                                               
TPGMX    B     ROUTSX                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF CONNECTED PERSON IS IN OFFICE RECORD SECURITY    *         
* MANGER ID LIST                                                      *         
* NTRY: R1=A(OFFICE CODE)                                             *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
TSTOMAN  EQU   *                                                                
         MVI   RBYTE,0                                                          
         MVC   RWORK(L'SAOFOID),0(R1)  SAVE OFFICE CODE                         
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         TM    COVFLAG,COVFON      TEST IF PERSON OVERRIDE FLAG ON              
         BO    TOMAX                                                            
         DROP  RF                                                               
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2          R2=A(OFFICE RECORD)                          
         XC    SAOFKEY,SAOFKEY     BUILD KEY                                    
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,CUAALF                                                   
         MVC   SAOFOID,RWORK                                                    
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    TOMADATA                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    IOERR,IOEDEL                                                     
         BO    TOMADATA                                                         
*&&US*&& MVC   FVMSGNO,=AL2(CE#SECMN)                                           
*&&US*&& MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
*                                                                               
TOMADATA LA    R3,SAOFDATA                                                      
*                                                                               
TOMA010  CLI   0(R3),0                                                          
         BE    TOMAX                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    TOMA020                                                          
TOMA012  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     TOMA010                                                          
         USING SAMAND,R3                                                        
TOMA020  CLC   SAMANID,CUPASS                                                   
         BE    ROUTSX                                                           
         MVI   RBYTE,X'FF'                                                      
         B     TOMA012                                                          
TOMAX    EQU   *                                                                
         CLI   RBYTE,0                                                          
         BE    ROUTSX                                                           
         MVC   FVMSGNO,=AL2(CE#SECMN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF CONNECTED PERSON IS IN GROUP RECORD SECURITY     *         
* MANGER ID LIST                                                      *         
* NTRY: R1=A(GROUP CODE)                                              *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
TSTGMAN  EQU   *                                                                
         MVI   RBYTE,0                                                          
         MVC   RWORK(L'SAAGAGR),0(R1)  SAVE GROUP CODE                          
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         TM    COVFLAG,COVFON      TEST IF PERSON OVERRIDE FLAG ON              
         BO    TGMAX                                                            
         DROP  RF                                                               
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2          R2=A(GROUP RECORD)                           
         XC    SAAGKEY,SAAGKEY     BUILD RECORD KEY                             
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,CUAALF                                                   
         MVC   SAAGAGR,RWORK                                                    
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    TGMADATA                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    IOERR,IOEDEL                                                     
         BO    TGMADATA                                                         
*&&US*&& MVC   FVMSGNO,=AL2(CE#SECMN)                                           
*&&US*&& MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
*                                                                               
TGMADATA LA    R3,SAAGDATA                                                      
*                                                                               
TGMA010  CLI   0(R3),0                                                          
         BE    TGMAX                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    TGMA020                                                          
TGMA012  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     TGMA010                                                          
         USING SAMAND,R3                                                        
TGMA020  CLC   SAMANID,CUPASS                                                   
         BE    ROUTSX                                                           
         MVI   RBYTE,X'FF'                                                      
         B     TGMA012                                                          
TGMAX    EQU   *                                                                
         CLI   RBYTE,0                                                          
         BE    ROUTSX                                                           
         MVC   FVMSGNO,=AL2(CE#SECMN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF CONNECTED PERSON IS IN DEPARTMENT RECORD SECURITY*         
* MANGER ID LIST                                                      *         
* NTRY: R1=A(OFFICE CODE)                                             *         
* EXIT: CC .EQ. IF OK ELSE .NE.                                       *         
***********************************************************************         
         SPACE 1                                                                
TSTDMAN  EQU   *                                                                
         LM    R2,R3,0(R1)                                                      
         MVI   RBYTE,0                                                          
*                                                                               
         GOTO1 ATSTOMAN,(R2)                                                    
         BE    ROUTSX              TEST OFFICE MANAGER ACCESS OK                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   RBYTE,X'FF'         FLAG OFFICE MANAGER RESTRICTED               
*                                                                               
         MVC   RWORK(L'SADPOID),0(R2)  SAVE OFFICE CODE                         
         MVC   RWORK+L'SADPOID(L'SADPDID),0(R3)  SAVE DEPARTMENT CODE           
         LH    RF,=Y(CONNECT-TWAD)                                              
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         TM    COVFLAG,COVFON      TEST IF PERSON OVERRIDE FLAG ON              
         BO    TDMAX                                                            
         DROP  RF                                                               
         LA    R2,IOKEY                                                         
         USING SADPREC,R2          R2=A(OFFICE RECORD)                          
         XC    SADPKEY,SADPKEY     BUILD KEY                                    
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,CUAALF                                                   
         MVC   SADPOID,RWORK                                                    
         MVC   SADPDID,RWORK+L'SADPOID                                          
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BNL   *+6                                                              
         DC    H'00'                                                            
         BE    TDMADATA                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    IOERR,IOEDEL                                                     
         BO    TDMADATA                                                         
*&&US*&& MVC   FVMSGNO,=AL2(CE#SECMN)                                           
*&&US*&& MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
*                                                                               
TDMADATA LA    R3,SADPDATA                                                      
*                                                                               
TDMA010  CLI   0(R3),0                                                          
         BE    TDMAX                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    TDMA020                                                          
TDMA012  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     TDMA010                                                          
         USING SAMAND,R3                                                        
TDMA020  CLC   SAMANID,CUPASS                                                   
         BE    ROUTSX                                                           
         MVI   RBYTE,X'FF'                                                      
         B     TDMA012                                                          
TDMAX    EQU   *                                                                
         CLI   RBYTE,0                                                          
         BE    ROUTSX                                                           
         MVC   FVMSGNO,=AL2(CE#SECMN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE 1R PERSON CODE IN ACCOUNT SYSTEM                *         
*                                                                     *         
* NTRY: R1=A(1R PERSON CODE FIELD HEADER)                             *         
* EXIT: IF CODE VALID CC=EQUAL                                        *         
*               APHALF=USER-ID NUMBER, APWORK=NAME                    *         
***********************************************************************         
         SPACE 1                                                                
VAL1RP   MVI   FVMINL,1            VALIDATE INPUT                               
         GOTO1 AFVAL                                                            
         BNE   VAL1RPX                                                          
         OI    FHOID(R1),FHOITR                                                 
*                                                                               
         MVC   RHALF,CUAALF        GET AGENCY APLHA ID                          
         OC    OPTAGY,OPTAGY         OR OPTION OVERRIDE                         
         BZ    *+10                                                             
         MVC   RHALF,OPTAGY                                                     
         GOTO1 GETCMP,RHALF        GET COMPANY CODE FROM ACCESS RECD.           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     VAL1RPX             EXIT NO ACCOUNT SYSTEM ENTRY                 
         XC    RWORK(15),RWORK                                                  
         MVC   RWORK(1),RBYTE      COMPANY CODE FROM GETCMP IN RBYTE            
         MVI   RWORK+1,C'1'        1R UNIT/LEDGER                               
         MVI   RWORK+2,C'R'                                                     
         MVC   RWORK+3(12),FVIFLD  ACCOUNT CODE FROM INPUT FIELD                
         GOTO1 =V(SEC1RP),RPARM,(R7),(0,RWORK),RR=ACRELO                        
         CLI   4(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     VAL1RPX                                                          
*                                                                               
VAL1RPX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY 1R PERSON CODE                                   *         
*                                                                     *         
* NTRY: R1=A(1R PERSON NUMBER)                                        *         
* EXIT: APWORK=USER-ID CODE/NAME                                      *         
***********************************************************************         
         SPACE 1                                                                
DIS1RP   LA    R2,IOKEY                                                         
         MVC   RHALF,CUAALF        GET AGENCY APLHA ID                          
         OC    OPTAGY,OPTAGY         OR OPTION OVERRIDE                         
         BZ    *+10                                                             
         MVC   RHALF,OPTAGY                                                     
         GOTO1 GETCMP,RHALF        GET COMPANY CODE FROM ACCESS RECD.           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     DIS1RPX             EXIT NO ACCOUNT SYSTEM ENTRY                 
         XC    RWORK(15),RWORK                                                  
         MVC   RWORK(1),RBYTE      COMPANY CODE FROM GETCMP IN RBYTE            
         MVI   RWORK+1,C'1'        1R UNIT/LEDGER                               
         MVI   RWORK+2,C'R'                                                     
         MVC   RWORK+3(12),FVIFLD  ACCOUNT CODE FROM INPUT FIELD                
         GOTO1 =V(SEC1RP),RPARM,(R7),(1,RWORK),RR=ACRELO                        
         CLI   4(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     DIS1RPX                                                          
*                                                                               
DIS1RPX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AGENCY ACCOUNT SYSTEM COMPANY CODE                   *         
*                                                                     *         
* NTRY: R1=A(AGENCY ALPHA)                                            *         
* EXIT: IF CODE VALID CC=EQUAL AND RWORK=COMPANY CODE (AGENCY BINARY) *         
***********************************************************************         
         SPACE 1                                                                
GETCMP   NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R1)                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BNE   GCMPNO                                                           
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         XR    RF,RF                                                            
         USING CTSYSD,R3                                                        
GCMP2    CLI   CTSYSEL,0           TEST EOR                                     
         BE    GCMPNO                                                           
         CLI   CTSYSEL,CTSYSELQ    TEST ACCOUNT SYSTEM ELEMENT                  
         BNE   GCMP4                                                            
         CLI   CTSYSNUM,X'06'                                                   
         BNE   GCMP4                                                            
         MVC   RBYTE,CTSYSAGB                                                   
         B     GCMPOK                                                           
GCMP4    IC    RF,1(R3)                                                         
         BXH   R3,RF,GCMP2         BUMP R3 TO NEXT ELEMENT                      
         SPACE 1                                                                
GCMPOK   SR    RC,RC                                                            
GCMPNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AGENCY ACCESS DETAILS                                *         
*                                                                     *         
* NTRY: R1=A(AGENCY ALPHA)                                            *         
* EXIT: ACCESS DETAILS IN APWORK                                      *         
*       APWORK+0 = ACCESS FLAGS                                       *         
*       APWORK+1 = PASSWORD TIMEOUT DAYS                              *         
*       APWORK+2 = PASSWORD MINIMUM LENGTH                            *         
***********************************************************************         
         SPACE 1                                                                
GETAAD   LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    APWORK,APWORK                                                    
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R1)                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IORD+IOCONFIL                                                
         BNE   GAADX                                                            
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         XR    RF,RF                                                            
         USING CTAADD,R3                                                        
GAAD010  CLI   CTAADEL,0           TEST EOR                                     
         BE    GAADX                                                            
         CLI   CTAADEL,CTAADELQ    TEST ACCOUNT SYSTEM ELEMENT                  
         BE    GAAD030                                                          
GAAD020  IC    RF,1(R3)                                                         
         BXH   R3,RF,GAAD010       BUMP R3 TO NEXT ELEMENT                      
*                                                                               
GAAD030  EQU   *                                                                
         MVC   APWORK(1),CTAADFLG                                               
         MVC   APWORK+1(1),CTAADPTO                                             
         MVC   APWORK+2(1),CTAADPML                                             
         B     GAADX                                                            
         DROP  R3                                                               
*                                                                               
GAADX    B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SUBSITIUTE DICTIONARY PARAMETERS                         *         
*                                                                     *         
* NTRY: R1 = A(PROGRAM RECORD)                                        *         
*    OR R1 = 0 TO SET DEFAULT VALUES                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SUBDIC   L     R5,ATWA                                                          
         USING TWAD,R5                                                          
         XC    RWORK,RWORK                                                      
         USING SADICD,RWORK                                                     
         LTR   R2,R1                                                            
         BZ    SDIC10                                                           
         USING SAPGREC,R2          R2=A(PROGRAM RECORD)                         
         LA    R1,SAPGDATA                                                      
         XR    RF,RF                                                            
SDIC02   CLI   0(R1),0             LOCATE DICTIONARY ELEMENT                    
         BE    SDIC10                                                           
         IC    RF,1(R1)                                                         
         CLI   0(R1),SADICELQ                                                   
         BE    *+8                                                              
         BXH   R1,RF,SDIC02                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SADICD(0),0(R1)                                                  
*                                                                               
SDIC10   GOTO1 VDICTAT,RPARM,C'LL  ',SDDD,DICSUB                                
         MVI   DICINDS,0                                                        
*                                                                               
         OC    SADICARC,SADICARC                                                
         BZ    SDIC12                                                           
         GOTO1 ADISDIC,RPARM,(SAPGOVS,L'AC@RECTY),(C'L',SADICARC)               
         MVC   AC@RECTY,APWORK                                                  
         GOTO1 (RF),(R1),(SAPGOVS,L'AC8RECTY),(C'L',SADICARC)                   
         MVC   AC8RECTY,APWORK                                                  
         OI    DICINDS,DICIRECO                                                 
*                                                                               
SDIC12   OC    SADICAAC,SADICAAC                                                
         BZ    SDIC14                                                           
         GOTO1 ADISDIC,RPARM,(SAPGOVS,L'AC@ACT),(C'L',SADICAAC)                 
         MVC   AC@ACT,APWORK                                                    
         OI    DICINDS,DICIACTO                                                 
*                                                                               
SDIC14   OC    SADICFWR,SADICFWR                                                
         BZ    SDIC16                                                           
         GOTO1 ADISDIC,RPARM,(SAPGOVS,L'FC@WRITE),(C'L',SADICFWR)               
         MVC   FC@WRITE,APWORK                                                  
         OI    DICINDS,DICIFCTO                                                 
*                                                                               
SDIC16   OC    SADICFRD,SADICFRD                                                
         BZ    SDIC18                                                           
         GOTO1 ADISDIC,RPARM,(SAPGOVS,L'FC@READ),(C'L',SADICFRD)                
         MVC   FC@READ,APWORK                                                   
         OI    DICINDS,DICIFCTO                                                 
*                                                                               
SDIC18   OC    SADICFNO,SADICFNO                                                
         BZ    SDIC20                                                           
         GOTO1 ADISDIC,RPARM,(SAPGOVS,L'FC@NO),(C'L',SADICFNO)                  
         MVC   FC@NO,APWORK                                                     
         OI    DICINDS,DICIFCTO                                                 
*                                                                               
SDIC20   OC    SADICARL,SADICARL                                                
         BNZ   SDIC22                                                           
         MVI   APWORK+0,L'AC@RECTY+1                                            
         MVC   APWORK+1(L'AC@RECTY),AC@RECTY                                    
         MVI   APWORK+1+L'AC@RECTY,0                                            
         LA    RF,CI#LSDIS                                                      
         GOTO1 VGETTXT,RPARM,(RF),(L'AC@RECLD,AC@RECLD),(C'I',0),0,    *        
               (X'08',APWORK),0                                                 
         B     SDIC24                                                           
SDIC22   GOTO1 ADISTXT,RPARM,(SAPGOVS,SADICARL)                                 
         MVC   AC@RECLD,APWORK                                                  
*                                                                               
SDIC24   OC    SADICAAL,SADICAAL                                                
         BNZ   SDIC26                                                           
         MVI   APWORK+0,L'AC@ACT+1                                              
         MVC   APWORK+1(L'AC@ACT),AC@ACT                                        
         MVI   APWORK+1+L'AC@ACT,0                                              
         LA    RF,CI#LSDIS                                                      
         GOTO1 VGETTXT,RPARM,(RF),(L'AC@ACTLD,AC@ACTLD),(C'I',0),0,    *        
               (X'08',APWORK),0                                                 
         B     SDIC30                                                           
SDIC26   GOTO1 ADISTXT,RPARM,(SAPGOVS,SADICAAL)                                 
         MVC   AC@ACTLD,APWORK                                                  
*                                                                               
SDIC30   MVI   INPUTS,C' '                                                      
         MVC   INPUTS+1(INPUTSL-1),INPUTS                                       
         MVC   INPYES(1),CT@YES                                                 
         MVC   INPYESX(1),CT@YES                                                
         MVI   INPYESX+1,C'+'                                                   
         CLI   SCRECN,RECFCT       TEST FOR FCONTROL RECORD                     
         BE    SDIC32                                                           
         MVC   INPWRT(1),CT@WRITE                                               
         MVC   INPWRTX(1),CT@WRITE                                              
         MVI   INPWRTX+1,C'+'                                                   
         MVC   INPREAD(1),CT@READ                                               
         MVC   INPREADX(1),CT@READ                                              
         MVI   INPREADX+1,C'+'                                                  
         MVC   INPNO(1),CT@NO                                                   
         MVC   INPNOX(1),CT@NO                                                  
         MVI   INPNOX+1,C'+'                                                    
         B     SDIC34                                                           
SDIC32   MVC   INPWRT(1),FC@WRITE                                               
         MVC   INPWRTX(1),FC@WRITE                                              
         MVI   INPWRTX+1,C'+'                                                   
         MVC   INPREAD(1),FC@READ                                               
         MVC   INPREADX(1),FC@READ                                              
         MVI   INPREADX+1,C'+'                                                  
         MVC   INPNO(1),FC@NO                                                   
         MVC   INPNOX(1),FC@NO                                                  
         MVI   INPNOX+1,C'+'                                                    
SDIC34   MVI   INPHELP,C'?'                                                     
         MVI   INPHELPX,C'?'                                                    
         MVI   INPHELPX+1,C'+'                                                  
         MVC   INPSEL(1),CT@SEL                                                 
         MVC   INPSELX(1),CT@SEL                                                
         MVI   INPSELX+1,C'+'                                                   
         MVI   INPSELD,C'*'                                                     
*                                                                               
SUBDICX  B     ROUTSX                                                           
         POP   USING                                                            
*                                                                               
SDDD     DS    0X                  * DEFAULT DICTIONARY VALUES *                
         DCDD  CT#RECTY,L'AC@RECTY,PAD=NONE                                     
         DCDD  CT#RECTY,L'AC8RECTY,PAD=NONE                                     
         DCDD  CT#ACT,L'AC@ACT,PAD=NONE                                         
         DCDD  CT#WRITE,L'FC@WRITE,PAD=NONE                                     
         DCDD  CT#READ,L'FC@READ,PAD=NONE                                       
         DCDD  CT#NO,L'FC@NO,PAD=NONE                                           
         DCDD  CT#HELP,L'SE@HELP,PAD=NONE                                       
         DCDD  CT#BACK,L'SE@BACK,PAD=NONE                                       
         DCDD  CT#FWD,L'SE@FWD,PAD=NONE                                         
         DCDD  CT#NXTLT,L'SE@NXTLT,PAD=NONE                                     
         DCDD  CT#QUILT,L'SE@QUILT,PAD=NONE                                     
         DCDD  CT#PFK,L'SE@PF,PAD=NONE                                          
SDDDX    DC    AL1(0)                                                           
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ENTRY POINT FOR OPTION ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
OBASE    NTR1  BASE=ACBASE1,LABEL=NO                                            
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     RE,4(RD)                                                         
         MVC   0(4,RE),=C'+OBA'                                                 
         SRL   RF,24               DETERMINE OPTION NUMBER (1 BASED)            
         SLL   RF,2                                                             
         CH    RF,=AL2(OPTMAX)                                                  
         BNH   *+2(RF)                                                          
         DC    H'0'                OPTION NUMBER OUSIDE RANGE                   
         SPACE 2                                                                
OBRANCH1 B     OVALAGY                                                          
         B     OVALUID                                                          
OPTMAX   EQU   *-OBRANCH1                                                       
         EJECT                                                                  
*                                  VALIDATE AGENCY ALPHA                        
OVALAGY  LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(ID RECORD)                              
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BE    OVAGY10                                                          
         MVC   FVMSGNO,=AL2(CE#INVAA)                                           
         B     OVALAGYX                                                         
OVAGY10  MVC   SCWORK(L'OPTAGY),FVIFLD   SAVE AGENCY ALPHA ID                   
*                                                                               
OVALAGYX B     EXIT                                                             
*                                  VALIDATE USER ID                             
*                                    RETURN ASSOCIATED AGENCY ALPHA ID          
OVALUID  LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(ID RECORD)                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,FVIFLD                                                    
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   OVUIDBAD                                                         
*                                                                               
OVUID10  L     R2,AIOAREA1                                                      
         LA    R3,CTIDATA          R3=A(FIRST ELEMENT)                          
OVUID20  CLI   0(R3),0             GET ELEMENT DATA                             
         BE    OVUID100                                                         
         CLI   0(R3),CTAGYELQ      AGENCY ELEMENT                               
         BE    OVUID30                                                          
         CLI   0(R3),X'02'         ID# ELEMENT                                  
         BE    OVUID50                                                          
OVUID22  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     OVUID20                                                          
*                                                                               
         USING CTAGYD,R3                                                        
OVUID30  OC    OPTAGY,OPTAGY                                                    
         BZ    OVUID40                                                          
         CLC   CTAGYID,OPTAGY       CHECK CORRECT AGENCY                        
         BNE   OVUIDBAD                                                         
OVUID40  MVC   SCWORK(L'OPTAGY),CTAGYID  SAVE AGENCY ALPHA ID                   
         B     OVUID22                                                          
*                                   SAVE AGENCY USER ID #                       
OVUID50  MVC   SCWORK+L'OPTAGY(L'OPTUSE),2(R3)                                  
         B     OVUID22                                                          
*                                   SAVE AGENCY USER ID #                       
OVUID100 OC    SCWORK(L'OPTAGY),SCWORK   CHECK AGENCY ALPHA FOUND               
         BZ    OVUIDBAD                                                         
         B     OVALUIDX                                                         
*                                                                               
OVUIDBAD MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     OVALUIDX                                                         
*                                                                               
OVALUIDX B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
FVFNOTH  EQU   005+X'FF00'         FIELD SHOULD BE HEXADECIMAL                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
         SPACE 1                                                                
CONADDRS DS    0F                  ** ADDRESS OF ROUTINES ETC. **               
         DC    A(PHASES)                                                        
         DC    A(HOOK)                                                          
         DC    A(OBASE)                                                         
         DC    A(ROUTS)                                                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         SPACE 1                                                                
* SEACSDICT                                                                     
       ++INCLUDE SEACSDICT                                                      
         SPACE 1                                                                
PHASES   DS    0X                  ** LOADED CORERES PHASES **                  
         DC    X'FF'                                                            
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
* DDMASTD                                                                       
       ++INCLUDE DDMASTD                                                        
* FAUTL                                                                         
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTS S/R LOCAL W/S                                                 *         
***********************************************************************         
         SPACE 1                                                                
RWRKD    DSECT                                                                  
RIOSAVE  DS    XL(IOAREAX-IOAREA)                                               
RDUB     DS    D                                                                
RWORK    DS    XL64                                                             
RBITT    DS    XL32                                                             
RPARM    DS    8F                                                               
RIPARM   DS    8F                                                               
RFULL    DS    F                                                                
RHALF    DS    H                                                                
RBYTE    DS    XL1                                                              
RFLAG    DS    XL1                                                              
RBLOCK   DS    20CL32                                                           
RIO      DS    2000C                                                            
RWRKX    EQU   *                                                                
         SPACE 2                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENMSG                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENMSG                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'162SEACS00X  07/28/94'                                      
         END                                                                    
