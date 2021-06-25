*          DATA SET SEACS00    AT LEVEL 010 AS OF 09/17/19                      
*PHASE TA0D00A                                                                  
*INCLUDE SEC1RP                                                                 
                                                                                
         TITLE 'SEACS00 - SECURITY ACCESS PROGRAM - CONTROLLER'                 
ACS00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**ACS0**,RA,R9,R8,CLEAR=YES,RR=RE                    
         LR    R7,RC                                                            
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         AHI   RC,SYSWORK-WORKD                                                 
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
         JE    *+2                 CAN'T LOAD CONTROLLER TABLES                 
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
         LA    R0,ACNUMTAB         NUMBER OF MY TABLES                          
         LA    R2,0                                                             
*                                                                               
ACS3     L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACPFKTAB(R2)                                                  
         LA    R2,4(,R2)                                                        
         LA    RE,4(,RE)                                                        
         BCT   R0,ACS3                                                          
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
         MVI   ACKEYIND,ACHKAFT                                                 
         MVI   ACLSTIND,ACHKBEF                                                 
         MVI   ACLFMIND,ACHKBEF+ACLFMIRV                                        
*                                                                               
*&&UK*&& OI    ACINDS,ACIDEST      SET USE DESTINATION NAME ON REPORTS          
         OI    ACINDS,ACI24PFK     ACTIVATE FOR 24 PFKEYS                       
         MVI   ACRECIND,ACHKAFT    RECORD HOOK AFTER                            
         MVI   ACACTIND,ACHKAFT    ACTION HOOK AFTER                            
         MVI   ACOPTIND,ACHKAFT    OPTION HOOK AFTER                            
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
         LHI   RF,SECAREA-TWAD     SAVE A(SECURITY ACCESS BLOCK)                
         LA    RF,TWAD(RF)                                                      
         ST    RF,ACASEC                                                        
         TM    SECINDS-SECD(RF),SECIINIT                                        
         BO    ACS8                                                             
         GOTO1 CSECRET,ACPARM,('SECPINIT',ACASEC),SECAREAL                      
         JNE   *+2                                                              
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
HOOK     NTR1  BASE=ACBASE1                                                     
*                                                                               
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
         B     HKVALREC            ACMRECR - PROCESS RECORD TYPE                
         B     HKVALACT            ACMACTR - PROCESS ACTION                     
         B     HOOKX               ACMKEYR - PROCESS KEY                        
         B     HKVALOPT            ACMOPTR - PROCESS OPTIONS                    
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
         B     HKBLDPF             ACMLAST - LAST TIME MODE                     
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
HKFIRST  EQU   *                                                                
*                                                                               
         LHI   R4,CONNECT-TWAD                                                  
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
*                                                                               
         TM    CUSTAT,CUSPCP       TEST RUNNING UNDER PC PAK                    
         BZ    HKF020              . NO                                         
         CLC   ACSOPT(4),=C'GRID'                                               
         BE    HKF010                                                           
         CLC   ACSOPT(4),=C'GOFF'                                               
         BNE   HKF020                                                           
         NI    PCGRIDS,X'FF'-PCGRPCQ-PCGRIDQ                                    
         B     *+8                                                              
HKF010   OI    PCGRIDS,PCGRPCQ                                                  
         XC    ACSOPT,ACSOPT                                                    
         OI    ACSOPTH+FHOID,FHOITR                                             
         XC    ACSMSG,ACSMSG                                                    
         OI    ACSMSGH+FHOID,FHOITR                                             
*                                                                               
HKF020   CLI   C#SYS,0             TEST CONNECT INFO ALREADY SET UP             
         BNE   HOOKOKEX                                                         
         DROP  R4                                                               
*                                                                               
         GOTO1 ASETCON                                                          
         BNE   HOOKX                                                            
*                                                                               
         B     HOOKOKEX                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* RECORD HOOK - AFTER GENERAL'S RECORD CHECKING                       *         
***********************************************************************         
* CHECK FOR LIMIT ACCESS NON-SECURITY AGENCY ACCESS - UK ONLY                   
* 4/26, PASSWORD RECORD NOT LONGER VALID FOR PPS AGENCY                         
HKVALREC EQU   *                                                                
*                                                                               
         LHI   R4,CONNECT-TWAD                                                  
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
*                                                                               
         CLC   INREC,TWALREC             RECORD CHANGE?                         
         BE    HKREC020                                                         
         OI    TWASWPST,TWAFIRST         TURN ON FIRST FOR RECORD               
         NI    PCGRIDS,X'FF'-PCGINIQ     TURN OFF GRIDS INIT BITS               
         DROP  R4                                                               
*                                                                               
HKREC020 ICM   RE,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
         USING RECTABD,RE                                                       
         CLI   RECNUMB,RECPWD      PASSWORD RECORD?                             
         BNE   HKREC050                                                         
         DROP  RE                                                               
*                                                                               
         OC    ACASEC,ACASEC                                                    
         BZ    *+16                                                             
         L     RF,ACASEC           'DDS' PASSWORD                               
         TM    SECINDS-SECD(RF),SECIDDS                                         
         BO    HKREC040             YES                                         
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    HKRECX                                                           
*                                                                               
         CLC   XIAGYSEC,XIAGYPER   SEC AGY = PID AGY?                           
         BE    HKREC050                                                         
         DROP  RE,RF                                                            
*                                                                               
HKREC040 MVC   FVMSGNO,=AL2(CE#INVIF)                                           
         B     HOOKX                                                            
*                                                                               
HKREC050 EQU   *                                                                
*&&UK                                                                           
         ICM   RE,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         USING RECTABD,RE                                                       
         CLI   RECNUMB,RECLIM      EXIT OK IF LIMIT ACCESS RECORD               
         BE    HKRECX                                                           
*                                                                               
         LA    R2,IOKEY            CHECK NOT ATTACHED TO SECURITY AGY           
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,CUAALF                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         JNE   *+2                                                              
*                                                                               
         L     R2,AIOAREA1                                                      
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         SR    RF,RF                                                            
*                                                                               
HKREC100 CLI   0(R3),0                                                          
         BE    HKRECX              IF NOT FOUND CONTINUE OK                     
         CLI   0(R3),CTSEAELQ                                                   
         BE    HKRECERR            IF FOUND CANT ACCESS SECURITY PROG           
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     HKREC100                                                         
*                                  INVALID CONNECT AGENCY AUTH                  
HKRECERR MVC   FVMSGNO,=AL2(CE#RACIN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     HOOKX                                                            
         DROP  RE,R2                                                            
*&&                                                                             
*                                                                               
HKRECX   B     HOOKOKEX                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* ACTION HOOK - AFTER GENERAL'S RECORD/ACTION CHECKING                *         
***********************************************************************         
* CHECK FOR DDS RETRICTED ACCESS RECORD-ACTIONS                                 
HKVALACT EQU   *                                                                
*&&UK                                                                           
         ICM   RE,15,AMIXNTRY      GET A(MIXTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
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
         ICM   RE,15,AMIXNTRY      GET A(MIXTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
         USING MIXTABD,RE                                                       
*                                                                               
         LHI   RF,PCDRIVEN-TWAD                                                 
         A     RF,ATWA                                                          
         USING PCDRIVEN,RF                                                      
*                                                                               
         CLI   MIXACTB,ACTGRD                                                   
         BNE   HKACT110                                                         
         OI    PCGRIDS,PCGRIDQ                                                  
         TM    PCGRIDS,PCGRPCQ                                                  
         BO    HKACTX                                                           
         MVC   FVMSGNO,=AL2(CE#INVIF)                                           
         B     HOOKX                                                            
*                                                                               
HKACT110 NI    PCGRIDS,X'FF'-PCGINIQ     TURN OFF GRIDS INIT BITS               
*                                                                               
         CLI   MIXRECB,RECPWD      PASSWORD RECORD?                             
         BNE   HKACTX              NO - EXIT                                    
*                                                                               
         CLI   MIXACTB,ACTLST      ACTION LIST?                                 
         BNE   HKACT120                                                         
         LA    R1,CUAALF           CONNECTED AGENCY                             
         GOTO1 AGETAAD                                                          
         TM    APWORK,CTAADPRQ     PPS?                                         
         BO    HKACT130            YES - ERROR                                  
         B     HKACTX              NO - EXIT                                    
*                                                                               
HKACT120 CLI   MIXACTB,ACTRST      ACTION RESET?                                
         BNE   HKACTX                                                           
         LA    R1,CUAALF           CONNECTED AGENCY                             
         GOTO1 AGETAAD                                                          
         TM    APWORK,CTAADPRQ     PPS?                                         
         BO    HKACTX              YES - GOOD                                   
         B     HKACT130            NO - ERROR                                   
*                                                                               
HKACT130 MVC   FVMSGNO,=AL2(CE#INVAC)   INVALID ACTION                          
         B     HOOKX                                                            
*                                                                               
HKACTX   B     HKCKPFK                                                          
         DROP  R8,RE,RF                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* KEY HOOK - AFTER GENERAL'S KEY CHECKING                                       
***********************************************************************         
HKCKPFK  EQU  *                                                                 
*                                                                               
         ICM   RE,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         BZ    HOOKX                                                            
*                                                                               
         USING RECTABD,RE                                                       
         CLI   RECNUMB,RECDAC      DATA ACCESS RECORD?                          
         BE    HCPFK010                                                         
         CLI   RECNUMB,RECDA2      DATA2 RECORD?                                
         BE    HCPFK010                                                         
         B     HOOKX                                                            
         DROP  RE                                                               
*                                                                               
HCPFK010 BRAS  RE,CHKPFK                                                        
         B     HOOKX                                                            
                                                                                
***********************************************************************         
* OPTION HOOK - AFTER GENERAL'S OPTION CHECKING                       *         
***********************************************************************         
HKVALOPT BRAS  RE,CHKOPTS                                                       
         B     HOOKX                                                            
                                                                                
***********************************************************************         
* BUILD AND DISPLAY PFKEYS IN EXTEND FIELD HEADERS                              
*        #254 (X'FE') PFKEY FIELD LINE                                          
***********************************************************************         
HKBLDPF  EQU   *                                                                
         BRAS  RE,BLDPFK                                                        
         B     HOOKX                                                            
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
         B     GETAPG              GET APPROVER GROUP RECORD DATA               
*                                                                               
         B     GRIDS               GRIDS OUTPUT ROUTINE                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - R1=A(RECORD TO ADD ELEMENT TO)                               *         
*        APELEM CONTAINS ELEMENT                                      *         
*        APBYTE CONTAINS FILE EQUATE IF NECESSARY                     *         
***********************************************************************         
ADDELS   LR    R0,R1                                                            
*                                                                               
         USING RECTABD,R2                                                       
         ICM   R2,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
         LA    R3,GENFIL                                                        
         CLI   RECUSER,GENFILQ           GENFIL?                                
         BE    ADDELS10                                                         
         CLI   RECUSER,CTFILEQ+GENFILQ   BOTH?                                  
         BNE   *+12                                                             
         CLI   APBYTE,GENFILQ            GENFIL?                                
         BE    ADDELS10                                                         
         DROP  R2                                                               
*                                                                               
         LA    R3,CTFILE                                                        
ADDELS10 GOTO1 VHELLO,RPARM,(C'P',(R3)),(R0),APELEM,0                           
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
ADDELSX  B     ROUTSX                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DELETE AN ELEMENT FROM A RECORD                          *         
*                                                                     *         
* NTRY - R1=A(RECORD TO DELETE ELEMENT FROM)                          *         
*        APELEM CONTAINS ELEMENT CODE OF ELEMENT TO BE DELETED        *         
*        APBYTE CONTAINS FILE EQUATE IF NECESSARY                     *         
***********************************************************************         
DELELS   LR    R0,R1                                                            
*                                                                               
         USING RECTABD,R2                                                       
         ICM   R2,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
         LA    R3,GENFIL                                                        
         CLI   RECUSER,GENFILQ           GENFIL?                                
         BE    DELELS10                                                         
         CLI   RECUSER,CTFILEQ+GENFILQ   BOTH?                                  
         BNE   *+12                                                             
         CLI   APBYTE,GENFILQ            GENFIL?                                
         BE    DELELS10                                                         
         DROP  R2                                                               
*                                                                               
         LA    R3,CTFILE                                                        
DELELS10 GOTO1 VHELLO,RPARM,(C'D',(R3)),(APELEM,(R0)),                 *        
               (APELEM+1,APELEM+2)                                              
         CLI   12(R1),6            TEST ELEMENT NOT FOUND                       
         BE    DELELSX                                                          
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
*                                                                               
DELELSX  B     ROUTSX                                                           
                                                                                
***********************************************************************         
* ROUTINE TO GET AN ELEMENT IN A RECORD                               *         
*                                                                     *         
* NTRY - R1=A(RECORD TO GET ELEMENT FROM)                             *         
*        APELEM CONTAINS ELEMENT CODE AND DATA TO SEARCH FOR          *         
*        APBYTE CONTAINS FILE EQUATE IF NECESSARY                     *         
* EXIT - APPARM CONTAINS ADDRESS OF ELEMENT OR ZEROES IF NOT FOUND    *         
***********************************************************************         
GETELS   LR    R0,R1                                                            
*                                                                               
         USING RECTABD,R2                                                       
         ICM   R2,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
         LA    R3,GENFIL                                                        
         CLI   RECUSER,GENFILQ           GENFIL?                                
         BE    GETELS10                                                         
         CLI   RECUSER,CTFILEQ+GENFILQ   BOTH?                                  
         BNE   *+12                                                             
         CLI   APBYTE,GENFILQ            GENFIL?                                
         BE    GETELS10                                                         
         DROP  R2                                                               
*                                                                               
         LA    R3,CTFILE                                                        
GETELS10 GOTO1 VHELLO,RPARM,(C'G',(R3)),(APELEM,(R0)),                 *        
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
         USING RECTABD,R2                                                       
SETACT   ICM   R2,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         BZ    SETACT2                                                          
*                                                                               
         TM    RECUXIND,RECUXACT   DO WE WANT THE EXPANDED ELEMENT              
         BO    *+12                                                             
         CLI   RECUSER,CTFILEQ     CTFILE?                                      
         BE    SETACT2             YES                                          
*                                                                               
         BRAS  RE,SETGACT          SET EXPANDED ACTIVITY ELEMENT                
*                                                                               
         CLI   RECUSER,CTFILEQ     CTFILE?                                      
         BNE   SETACTX             NO - EXPANDED ONLY ONE NEEDED                
         DROP  R2                                                               
*                                                                               
SETACT2  OC    ACTEL,ACTEL         TEST ACTVITY ELEMENT FOUND                   
         BNZ   SETACT4                                                          
         GOTO1 AGETACT             NO - EXTRACT IT                              
         MVC   ACTEL,APELEM                                                     
*                                                                               
SETACT4  LA    R2,ACTEL                                                         
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
DISACT   GOTO1 AGETACT             EXTRACT ACTIVITY ELEMENT                     
         BNE   DISACTX                                                          
*                                                                               
         MVI   FVXTRA,C'-'                                                      
         MVCDD FVXTRA+2(7),CT#UPDTE                                             
         LA    R3,FVXTRA+10                                                     
*                                                                               
         LA    R2,ACTEL            POINT TO ACTIVITY ELEMENT                    
         USING SAACVD,R2                                                        
         LA    R4,SAACVDT                                                       
         DROP  R2                                                               
*                                                                               
         USING RECTABD,RF                                                       
         ICM   RF,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         BZ    DISACT10                                                         
         CLI   RECUSER,CTFILEQ     CTFILE?                                      
         BE    DISACT10            . NO                                         
         DROP  RF                                                               
*                                                                               
         USING GACTELD,R2                                                       
         LA    R4,GACTCDT                                                       
         DROP  R2                                                               
*                                                                               
DISACT10 GOTO1 VDATCON,RPARM,(3,(R4)),(8,(R3))                                  
*&&UK*&& OI    0(R3),X'F0'                                                      
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
GETACT   XC    APELEM,APELEM                                                    
         XC    ACTEL,ACTEL                                                      
*                                                                               
         MVI   APELEM,CTACTELQ     CONTROL FILE ACTIVITY ELEMENT                
         USING RECTABD,R2                                                       
         ICM   R2,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         BZ    GETACT10                                                         
         CLI   RECUSER,CTFILEQ     CTFILE?                                      
         BE    GETACT10            . NO                                         
         MVI   APELEM,GACTELQ      GENFIL TYPE ACTIVITY ELEMENT                 
         DROP  R2                                                               
*                                                                               
GETACT10 GOTO1 AGETELS                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         ICM   R1,15,APPARM                                                     
         BNZ   *+14                                                             
         XC    FVMSGNO,FVMSGNO     SET IF NO ACTIVITY ELEMENT                   
         B     GETACTX                                                          
*                                                                               
         MVC   ACTEL,0(R1)         SAVE ELEMENT IN WORKING STORAGE              
         CLI   ACTEL,CTACTELQ                                                   
         BE    GETACTX                                                          
         MVC   ACTELG,0(R1)                                                     
*                                                                               
GETACTX  B     ROUTSX                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE USER-ID CODE                                              
*                                                                               
* NTRY: R1=A(USER-ID CODE FIELD HEADER)                                         
* EXIT: IF CODE VALID CC=EQUAL                                                  
*               APHALF=USER-ID NUMBER, APWORK=NAME                              
***********************************************************************         
VALUID   MVI   FVMINL,1            VALIDATE INPUT                               
         GOTO1 AFVAL                                                            
         BNE   VALUIDX                                                          
         OI    FHOID(R1),FHOITR                                                 
*                                                                               
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
*                                                                               
         XC    APWORK,APWORK                                                    
         XC    APHALF,APHALF                                                    
         LA    R3,CTIDATA          R3=A(FIRST ELEMENT)                          
         XR    RF,RF                                                            
*                                                                               
         USING CTAGYD,R3                                                        
VUID2    CLI   CTAGYEL,CTAGYELQ    TEST AGENCY ELEMENT                          
         BNE   VUID4                                                            
         MVC   RHALF,CTAGYID       EXTRACT AGENCY ALPHA                         
         B     VUID10                                                           
*                                                                               
         USING CTDSCD,R3                                                        
VUID4    CLI   CTDSCEL,CTDSCELQ    TEST DESCRIPTION ELEMENT                     
         BNE   VUID6                                                            
         MVC   APHALF,CTDSC        EXTRACT UID NUMBER                           
         B     VUID10                                                           
*                                                                               
         USING CTDSTD,R3                                                        
VUID6    CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION ELEMENT                     
         BNE   VUID8                                                            
         MVC   APWORK(L'CTDSTNAM),CTDSTNAM  EXTRACT NAME                        
         B     VUID10                                                           
*                                                                               
VUID8    CLI   0(R3),0             TEST E-O-R                                   
         BE    VUID20                                                           
*                                                                               
VUID10   IC    RF,1(R3)                                                         
         BXH   R3,RF,VUID2         BUMP R3 TO NEXT ELEMENT                      
*                                                                               
VUID20   GOTO1 VALAGA,RHALF        TEST AGENCY ALPHA OK                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(CE#INVID)                                           
         B     VALUIDX                                                          
         GOTO1 ATSTUID,APHALF      TEST USER CAN CONNECT TO USER-ID             
VALUIDX  B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AGENCY ALPHA                                              
*                                                                               
* NTRY: R1=A(AGENCY ALPHA)                                                      
* EXIT: IF CODE VALID CC=EQUAL                                                  
***********************************************************************         
VALAGA   NTR1                                                                   
*                                                                               
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
*                                                                               
VAGA6    MVC   RHALF,CUAALF                                                     
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+10                                                             
         MVC   RHALF,OPTAGY                                                     
         CLC   RHALF,RWORK                                                      
VAGAX    J     EXIT                                                             
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY USER-ID CODE                                     *         
*                                                                     *         
* NTRY: R1=A(USER-ID NUMBER)                                          *         
* EXIT: APWORK=USER-ID CODE/NAME                                      *         
***********************************************************************         
                                                                                
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
                                                                                
         XC    APWORK,APWORK                                                    
         LA    R3,CTIDATA                                                       
DUID02   CLI   0(R3),0             TEST E-O-R                                   
         BE    DISUIDX                                                          
*                                                                               
         USING CTDSCD,R3                                                        
         CLI   0(R3),CTDSCELQ      TEST DESCRIPTION ELEMENT                     
         BNE   DUID04                                                           
         IC    RF,CTDSCLEN                                                      
         SHI   RF,CTDSC+1-CTDSCD                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),CTDSC                                                  
         B     DUID08                                                           
                                                                                
         USING CTDSTD,R3                                                        
DUID04   CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION ELEMENT                     
         BNE   DUID08                                                           
         MVC   APWORK+9(L'CTDSTNAM),CTDSTNAM  EXTRACT NAME                      
         DROP  R3                                                               
                                                                                
DUID08   XR    RF,RF               BUMP R3 TO NEXT ELEMENT                      
         IC    RF,1(R3)                                                         
         BXH   R3,RF,DUID02                                                     
                                                                                
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
         GOTO1 ATSTSYS,SYSLNUM     TEST USER CAN CONNECT TO SYSTEM              
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
VALPGM   MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R1,0(R1)            POINT TO INPUT FIELD HEADER                  
         MVI   FVMAXL,L'PGMNAME                                                 
         GOTO1 AFVAL               TEST FOR INPUT                               
         BNE   VALPGMX                                                          
         L     R3,SCAUTL                                                        
         USING UTLD,R3                                                          
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
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PROGRAM NAME                                     *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(PROGRAM NUMBER)             *         
* EXIT - APWORK(7)=PROGRAM NAME                                       *         
*        APPARM(4)=A(PROGRAM LIST ENTRY) OR ZERO IF NOT FOUND         *         
***********************************************************************         
DISPGM   MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R1,0(R1)            POINT TO INPUT FIELD HEADER                  
         MVC   RWORK+1(1),0(R1)                                                 
         L     R2,SCAUTL                                                        
         USING UTLD,R2                                                          
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
         DROP  R1,R2                                                            
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
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),SAPGMD                                                 
*                                                                               
         GOTO1 ADISTXT,RPARM,(SAPGOVS,SAPGMDSC)                                 
*                                                                               
         USING FHD,R4                                                           
VOVPG10  LTR   R4,R4               OUTPUT PROGRAM DESCRIPTION                   
         BZ    VOVPG12                                                          
         IC    RE,FHLN                                                          
         SHI   RE,FHDAD+1                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
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
                                                                                
VALCODE  MVC   RBYTE,0(R1)         EXRACT FORMAT                                
         L     R1,0(R1)            R1=A(FIELD HEADER)                           
         OI    FHOID(R1),FHOITR                                                 
         MVI   FVMINL,1                                                         
                                                                                
         CLI   RBYTE,SAPGMIFA      VALIDATE ALPHAMERIC FORMAT                   
         BNE   VCODE2                                                           
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALCODEX                                                         
         MVC   APBYTE,FVIFLD                                                    
         B     VCODE10                                                          
                                                                                
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
                                                                                
DISCODE  MVI   APWORK,C' '                                                      
         MVC   APWORK+1(2),APWORK                                               
         L     R2,0(R1)                                                         
                                                                                
         CLI   0(R1),SAPGMIFA      DISPLAY ALPHAMERIC                           
         BNE   DCODE2                                                           
         MVC   APWORK(1),0(R2)                                                  
         B     DISCODEX                                                         
                                                                                
DCODE2   CLI   0(R1),SAPGMIFN      DISPLAY NUMERIC                              
         BNE   DCODE4                                                           
         EDIT  (B1,0(R2)),(3,APWORK),ALIGN=LEFT,WRK=RWORK,DUB=RDUB              
         B     DISCODEX                                                         
                                                                                
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
                                                                                
DISCODEX B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ACCESS GROUP CODE                               *         
*                                                                     *         
* NTRY: R1=A(ACCESS GROUP CODE FIELD HEADER)                          *         
* EXIT: IF CODE VALID CC=EQUAL                                        *         
*               APWORK=NAME                                           *         
***********************************************************************         
                                                                                
VALACG   MVI   FVMINL,1            VALIDATE FIELD                               
         GOTO1 AFVAL                                                            
         BNE   VALACGX                                                          
         OI    FHOID(R1),FHOITR                                                 
                                                                                
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
                                                                                
         LA    R3,SAAGDATA                                                      
         USING SAAGND,R3           R3=A(AGENCY ELEMENT)                         
         XR    RF,RF                                                            
VACG2    IC    RF,SAAGNLN                                                       
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    VACG4                                                            
         BXH   R3,RF,VACG2                                                      
*                                                                               
VACG4    MVC   APHALF,SAAGNNUM     SAVE GROUP NUMBER                            
         XC    APWORK,APWORK       SAVE GROUP NAME                              
         SHI   RF,SAAGNLNQ+1                                                    
         BM    VALACGX                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SAAGNNAM                                               
*                                                                               
VALACGX  B     ROUTSX                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS GROUP CODE                                *         
*                                                                     *         
* NTRY: R1=A(ACCESS GROUP NUMBER)                                     *         
* EXIT: APWORK=ACCESS GROUP CODE/NAME                                 *         
***********************************************************************         
                                                                                
DISACG   LR    R4,R1               R4=A(ACCESS GROUP NUMBER)                    
                                                                                
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
                                                                                
DACG4    XR    RF,RF                                                            
         LA    R3,SAAGDATA                                                      
         USING SAAGND,R3           R3=A(AGENCY ELEMENT)                         
DACG6    IC    RF,SAAGNLN                                                       
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    DACG8                                                            
         BXH   R3,RF,DACG6         BUMP R3 TO NEXT ELEMENT                      
*                                                                               
DACG8    CLC   SAAGNNUM,0(R4)      MATCH ON AGENCY NUMBER                       
         BNE   DACG2                                                            
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'SAAGAGR),SAAGAGR                                        
         SHI   RF,SAAGNLNQ+1                                                    
         BM    DISACGX                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK+L'SAAGAGR+1(0),SAAGNNAM                                   
*                                                                               
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
                                                                                
VALDIC   MVC   RIPARM(8),0(R1)     SAVE INPUT PARAMETERS                        
         XR    R3,R3                                                            
         ICM   R3,7,5(R1)                                                       
         USING FHD,R3              R3=A(FIELD HEADER)                           
         OI    FHOI,FHOITR                                                      
*                                                                               
         XR    RF,RF                                                            
         IC    RF,RIPARM+0                                                      
         MHI   RF,3                                                             
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
                                                                                
       ++INCLUDE DDPFXTBLE                                                      
         DS    0H                                                               
                                                                                
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
                                                                                
BLDBITT  LR    RF,R1               RF=A(PARAMETER LIST)                         
         LM    R1,R2,0(RF)         R1=A(ELEMENT), R2=A(BIT TABLE)               
                                                                                
         ICM   RE,1,4(RF)          RE=LENGTH OF BIT TABLE                       
         BNZ   *+8                                                              
         LA    RE,32               DEFAULT=32                                   
         BCTR  RE,0                                                             
         EX    RE,*+8              ZEROIZE BIT TABLE                            
         B     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
                                                                                
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
*                                                                               
BBITT2   XR    RE,RE               EXTRACT BIT TABLE                            
         IC    RE,1(R1)                                                         
         SR    RE,RF                                                            
         BZ    BLDBITTX                                                         
         BCTR  RE,0                                                             
         AR    RF,R1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
*                                                                               
BLDBITTX B     ROUTSX                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO TAG BIT TABLE ONTO END OF AN ELEMENT                     *         
*                                                                     *         
* NTRY: P1=A(ELEMENT)                                                 *         
*       P2=(L(BIT TABLE), A(BIT TABLE))                               *         
***********************************************************************         
TAGBITT  LR    RF,R1                                                            
         LM    R1,R2,0(RF)                                                      
                                                                                
         XC    RBITT,RBITT         COPY BIT TABLE TO RBITT                      
         ICM   RE,1,4(RF)                                                       
         BNZ   *+8                                                              
         LA    RE,32                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RBITT(0),0(R2)                                                   
                                                                                
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
                                                                                
         B     ROUTSX                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO GET NAME FROM PERSONAL-ID RECORD                         *         
*                                                                     *         
* NTRY: R1=A(PERSONAL-ID)                                             *         
* EXIT: APWORK=NAME                                                   *         
***********************************************************************         
GETPNAM  LA    R2,IOKEY                                                         
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
GETPNUM  LA    R2,IOKEY                                                         
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
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO GET DEPARTMENT NAME FROM DEPARTMENT RECORD               *         
*                                                                     *         
* NTRY: P1=A(DEPARTMENT RECORD)                                       *         
* EXIT: APWORK=NAME                                                   *         
***********************************************************************         
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
         SHI   R1,SADPTLNQ+1                                                    
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
         SHI   R1,SAOFFLNQ+1                                                    
         BM    ROUTSX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SAOFFNAM                                               
         B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO GET ACCESS GROUP NAME FROM ACCESS GROUP RECORD           *         
*                                                                     *         
* NTRY: P1=A(ACCESS GROUP RECORD)                                     *         
* EXIT: APWORK=NAME APHALF=NUMBER                                     *         
***********************************************************************         
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
         SHI   R1,SAAGNLNQ+1                                                    
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
         SHI   R1,SALANLNQ+1                                                    
         BM    ROUTSX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK+L'SALANCOD(0),SALANNAM                                    
         MVC   APWORK(L'SALANCOD),SALANCOD                                      
         MVC   APHALF,SALANNUM                                                  
         B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO GET PERSONAL-ID GIVEN PASSWORD NUMBER                    *         
*                                                                     *         
* NTRY: R1=A(PASSWORD NUMBER)                                         *         
* EXIT: APWORK=PERSONAL-ID                                            *         
***********************************************************************         
GETPID   LA    R2,IOKEY                                                         
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
         DROP  R2,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
*   BUILD LIMIT ACCESS IN APWORK                                      *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(LIMIT ACCESS INTERNAL CODE) *         
* EXIT - APWORK+0(6)=LIMIT ACCESS CODE                                *         
***********************************************************************         
DISLACC  MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R3,0(R1)            POINT TO LIMIT ACCESS CODE                   
         XC    APWORK,APWORK                                                    
         OC    0(L'SASYSLMT,R3),0(R3)                                           
         BZ    DLACX                                                            
         MVC   APWORK(L'SASYSLMT),0(R3)                                         
         CLC   APWORK(2),=XL2'FFFF'                                             
         BNE   DLAC010                                                          
         MVC   APWORK(2),=CL2'L='                                               
         B     DLACX                                                            
*                                                                               
DLAC010  EQU   *                                                                
*&&UK                                                                           
         CLI   RWORK,4             TEST UK/MEDIA                                
         BNE   DLAC100                                                          
         LA    RE,APWORK                                                        
         LR    R4,R3                                                            
         LA    R2,4                                                             
DLAC030  SR    R0,R0                255,255,255,255 MAX VALUES                  
         IC    R0,0(R4)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,ZERO=NOBLANK,                 *        
               WRK=RWORK,DUB=RDUB                                               
         AR    RE,R0     '                                                      
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,DLAC030                                                       
         BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         B     DLACX                                                            
*&&                                                                             
*&&US                                                                           
         CLI   RWORK,9             TEST MEDIABASE                               
         BNE   DLAC020                                                          
         GOTO1 VHEXOUT,APPARM,(R3),APWORK,4,=C'N'                               
         B     DLACX                                                            
*----------------------------------------------------------------------         
* US MEDIA LIMITED ACCESS (SPOT,NET,PRINT,TRAFFIC)                              
*----------------------------------------------------------------------         
DLAC020  MVI   RHALF,C'S'                                                       
         CLI   RWORK,2             SPOT                                         
         BE    DLAC040                                                          
         CLI   RWORK,13            SPOT TRAFFIC                                 
         BE    DLAC040                                                          
         MVI   RHALF,C'N'                                                       
         CLI   RWORK,3             NETWORK                                      
         BE    DLAC040                                                          
         MVI   RHALF,C'P'                                                       
         CLI   RWORK,4             PRINT                                        
         BNE   DLAC100                                                          
*                                                                               
DLAC040  CLI   0(R3),C'$'          OFFICE LIST                                  
         BE    DLACX                                                            
         CLI   0(R3),C'+'          MARKET                                       
         BE    DLACX                                                            
         CLI   0(R3),C'*'          OFFICE OR CLIENT GROUP?                      
         BNE   DLAC080             NO: THIS IS A CLIENT                         
         CLI   1(R3),C'*'          TWO CHARACTER OFFICE?                        
         BE    DLAC060             YES: PROCESS TWO CHARACTER OFFICE            
         OC    2(2,R3),2(R3)       ZEROES MEANS ONE BYTE OFFICE                 
         BZ    DLAC060                                                          
         CLC   2(2,R3),=CL2' '     SPACES ALSO MEAN ONE BYTE OFFICE             
         BE    DLAC060                                                          
*----------------------------------------------------------------------         
* CLIENT GROUP LIMITED ACCESS                                                   
*----------------------------------------------------------------------         
         CLI   2(R3),C'A'          MAKE SURE THIS IS A VALID CLT GROUP          
         BL    DLAC041             YES: CONTINUE                                
         MVC   APWORK(L'CTSYSLMT),0(R3)                                         
         B     DLACX               INVALID CLIENT GROUP                         
*                                                                               
DLAC041  LA    R1,SPCGRTAB                                                      
DLAC042  CLC   2(1,R1),1(R3)       FIND CLIENT GROUP ID IN TABLE                
         BE    DLAC044             FOUND IT                                     
         CLI   0(R1),C'Z'          C'Z' MARKS THE END OF THE TABLE              
         BNE   DLAC043             NOT THERE YET: CONTINUE                      
         MVC   APWORK(L'CTSYSLMT),0(R3)                                         
         MVI   APWORK+L'CTSYSLMT+1,C'?'                                         
         B     DLACX                                                            
DLAC043  LA    R1,L'SPCGRTAB(,R1)                                               
         B     DLAC042                                                          
*                                                                               
DLAC044  MVC   APWORK(9),=CL9' '                                                
         MVC   APWORK(3),=C'CG='                                                
         MVC   APWORK+3(2),0(R1)   CLIENT GROUP ID ALPHA CODE                   
*                                                                               
         LA    R1,APWORK+4                                                      
         CLI   0(R1),C' '          IS THIS A SINGLE CHARACTER ID                
         BNH   *+8                 YES                                          
         LA    R1,1(,R1)           TWO CHARACTER GROUP ID                       
*                                                                               
         IC    RF,2(R3)            CONVERT PWOS W/ X'F' DELIMITER               
         SRL   RF,4                TO CHARACTER NUMBERS                         
         STC   RF,0(R1)            .                                            
         OI    0(R1),X'F0'         .                                            
         LA    R1,1(,R1)           .                                            
*                                  .                                            
         TM    2(R3),X'0F'         .                                            
         BO    DLACX               .                                            
         MVC   0(1,R1),2(R3)       .                                            
         OI    0(R1),X'F0'         .                                            
         LA    R1,1(,R1)           .                                            
*                                  .                                            
         TM    3(R3),X'F0'         .                                            
         BO    DLACX               .                                            
         IC    RF,3(R3)            .                                            
         SRL   RF,4                .                                            
         STC   RF,0(R1)            .                                            
         OI    0(R1),X'F0'         .                                            
         LA    R1,1(,R1)           .                                            
*                                  .                                            
         TM    3(R3),X'0F'         .                                            
         BO    DLACX               .                                            
         MVC   0(1,R1),3(R3)       .                                            
         OI    0(R1),X'F0'         .                                            
         LA    R1,1(,R1)           .                                            
         B     DLACX                                                            
*----------------------------------------------------------------------         
* OFFICE LIMITED ACCESS                                                         
*----------------------------------------------------------------------         
DLAC060  LA    RF,ACWORK                                                        
         USING OFFICED,RF                                                       
         XC    ACWORK,ACWORK                                                    
         MVC   OFCSYS,RHALF        SYSTEM ID                                    
         MVC   OFCAGY,CUAALF       AGENCY ALPHA                                 
         MVC   OFCOFC,1(R3)        1 BYTE OFFICE                                
         DROP  RF                                                               
*                                                                               
         USING COMFACSD,RE                                                      
         L     RE,ACOM                                                          
         XC    ACPARM(8),ACPARM                                                 
         MVC   ACPARM+4(4),=X'D9000A38' GET OFFICER ADDRESS                     
         GOTO1 CCALLOV,ACPARM                                                   
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         DROP  RE                                                               
*                                                                               
         L     RF,ACPARM                CALL OFFICER                            
         GOTO1 (RF),ACPARM,(C'2',ACWORK),ACOM                                   
*                                                                               
         LA    R1,ACWORK                                                        
         USING OFFICED,R1                                                       
         TM    OFCINDS,OFCINOLA+OFCIOINV   NOT USING 2 OFFS OR INVALID          
         BNZ   DLACX                                                            
         XC    0(L'CTSYSLMT,R3),0(R3)                                           
         MVI   APWORK,C'*'                                                      
         MVI   APWORK+1,C'*'               (**OF)                               
         MVC   APWORK+2(L'OFCOFC2),OFCOFC2 DISPLAY 2 BYTE OFFICE VALUE          
         B     DLACX                                                            
         DROP  R1                                                               
*----------------------------------------------------------------------         
* CLIENT LIMITED ACCESS                                                         
*----------------------------------------------------------------------         
DLAC080  CLI   RWORK,4             PRINT                                        
         BE    DLACX               NO: NEED FOR UNPACKING                       
         MVC   APPARM+4(4),=X'D9000A15' CLUNPK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0       SPOT/NET/TRAFFIC NEED TO UNPACK              
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R3),APWORK                                            
         B     DLACX                                                            
*&&                                                                             
*----------------------------------------------------------------------         
* PERSONNEL                                                                     
*----------------------------------------------------------------------         
DLAC100  CLI   RWORK,14            TEST PERSONNEL                               
         BNE   DLACX                                                            
         XC    APWORK,APWORK                                                    
         OC    2(2,R3),2(R3)                                                    
         BZ    DLACX                                                            
         LA    RE,APWORK           FORMAT N(NN)-N(NN)                           
         ZIC   R0,2(R3)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,WRK=RWORK,DUB=RDUB                     
         AR    RE,R0                                                            
         MVI   0(RE),C'-'                                                       
         ZIC   R0,3(R3)                                                         
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT,WRK=RWORK,DUB=RDUB                     
*                                                                               
DLACX    B     ROUTSX                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*   VALIDATE LIMIT ACCESS FIELD IN FVAREA                             *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(LIMIT ACCESS INTERNAL CODE) *         
* EXIT - CC=EQUAL IF LIMIT ACCESS VALID, NOT EQUAL IF INVALID         *         
***********************************************************************         
VALLACC  MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
*&&US                                                                           
         CLI   RWORK,X'0A'         CONTROL                                      
         BE    VAEIIF                                                           
         CLI   RWORK,X'0C'         CPP                                          
         BE    VAEIIF                                                           
         CLI   RWORK,X'07'         TALENT                                       
         BE    VAEIIF                                                           
         CLI   RWORK,X'08'         REP                                          
         BE    VAEIIF                                                           
*&&                                                                             
         L     R3,0(R1)            POINT TO LIMIT ACCESS CODE                   
         MVC   0(L'SASYSLMT,R3),FVIFLD                                          
         CLC   0(2,R3),=CL2'L='    TEST LIMIT ACCESS LIST                       
         BNE   VLAC010             FOR ALL SYSTEMS                              
         LA    R2,IOKEY                                                         
         USING SALMREC,R2                                                       
         XC    SALMKEY,SALMKEY                                                  
         MVI   SALMTYP,SALMTYPQ                                                 
         MVI   SALMSUB,SALMSUBQ                                                 
         MVC   SALMSYS,RWORK                                                    
         MVC   SALMAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SALMAGY,OPTAGY                                                   
         MVC   SALMLID,2(R3)                                                    
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   VAEIIF                                                           
         MVC   0(2,R3),=XL2'FFFF'                                               
         B     VLACX                                                            
*                                                                               
VLAC010  EQU   *                                                                
*                                                                               
*&&US                                                                           
VLACSUS  EQU   *                   SPECIAL VALIDATION FOR SYSTEMS IN US         
         CLI   RWORK,6             TEST ACC                                     
         BE    VLACA00                                                          
         CLI   RWORK,3             TEST NET                                     
         BE    VLACN00                                                          
         CLI   RWORK,4             TEST PRINT                                   
         BE    VLACN00                                                          
         CLI   RWORK,2             TEST SPOT                                    
         BE    VLACS00                                                          
         CLI   RWORK,13            TEST SPOT TRAFFIC                            
         BE    VLACT00                                                          
         B     VLAC015                                                          
*                                                                               
VLACA00  EQU   *                                                                
         MVI   RBYTE,0                                                          
         GOTOR VALOC                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOL                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOF2                                                           
         BE    VLACSUSX                                                         
         B     VAEIIF                                                           
*                                                                               
VLACN00  EQU   *                                                                
         GOTOR VALCC                                                            
         BE    VLACSUSX                                                         
         MVI   RBYTE,0                                                          
         GOTOR VALOC                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOL                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOLL                                                           
         BE    VLACSUSX                                                         
         GOTOR VALOC2M                                                          
         BE    VLACSUSX                                                         
         GOTOR VALCGRP                                                          
         BE    VLACSUSX                                                         
         B     VAEIIF                                                           
*                                                                               
VLACS00  EQU   *                                                                
         GOTOR VALCC                                                            
         BE    VLACSUSX                                                         
         MVI   RBYTE,0                                                          
         GOTOR VALOC                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOL                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOLL                                                           
         BE    VLACSUSX                                                         
         GOTOR VALMC                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOC2M                                                          
         BE    VLACSUSX                                                         
         GOTOR VALCGRP                                                          
         BE    VLACSUSX                                                         
         B     VAEIIF                                                           
*                                                                               
VLACT00  EQU   *                                                                
         GOTOR VALCC                                                            
         BE    VLACSUSX                                                         
         MVI   RBYTE,0                                                          
         GOTOR VALOC                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOL                                                            
         BE    VLACSUSX                                                         
         GOTOR VALOLL                                                           
         BE    VLACSUSX                                                         
         GOTOR VALOC2M                                                          
         BE    VLACSUSX                                                         
         B     VAEIIF                                                           
*                                                                               
VLACSUSX BRAS  RE,VALUSMLA         VALIDATE US MEDIA LIMIT ACCESS               
         BNE   VAEIIF                                                           
         B     VLACX                                                            
*                                                                               
VLAC015  CLI   RWORK,9             MEDIABASE SYSTEM                             
         BNE   VLAC020                                                          
         XC    0(L'SASYSLMT,R3),0(R3)                                           
         CLI   FVILEN,8            MUST BE LESS THAN 8                          
         BH    VAEFTL                                                           
         TM    FVILEN,X'01'        AND MUST BE EVEN                             
         BNZ   VAEIIF                                                           
         ZIC   R0,FVILEN                                                        
         XC    APDUB,APDUB                                                      
         GOTO1 VHEXIN,APPARM,FVIFLD,APDUB,(R0),0                                
         OC    APPARM+12(4),APPARM+12                                           
         BZ    VAEIIF                                                           
         MVC   0(4,R3),APDUB                                                    
*&&                                                                             
*                                  TEST PERSONNEL SYSTEM                        
VLAC020  CLI   RWORK,14                                                         
         BNE   VLAC030                                                          
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
VLAC030  DS    0H                                                               
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
VLAC040  STC   RE,FVINDX                                                        
         CLI   0(RF),0                                                          
         BE    VAEMIF                                                           
         CLI   0(RF),3                                                          
         BH    VAEFTL                                                           
         TM    2(RF),X'80'                                                      
         BZ    VAEFNN                                                           
         CLC   6(2,RF),=H'255'     MAXVALUE FOR CR/BA/L1/L2 = 255               
         BH    VAEFTB                                                           
*                                                                               
VLAC044  MVC   0(1,R1),7(RF)                                                    
         LA    RF,L'RBLOCK(RF)                                                  
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,VLAC040                                                       
         MVI   FVINDX,0                                                         
         B     VLACX                                                            
*&&                                                                             
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
                                                                                
TXTF010  CLI   0(RE),C'*'          IGNORE WILDCARD CHARACTERS                   
         BE    *+14                                                             
         CLC   0(1,RE),0(R3)                                                    
         BNE   TXTFLTNE                                                         
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT CHAR IN FILTER AND TEXT         
         LA    RE,1(RE)                                                         
         BCT   RF,TXTF010                                                       
         B     TXTFLTOK            FILTER MATCHES DATA                          
                                                                                
TXTF020  SHI   RF,2                DROP THE '(' AND GET EX L'FILTER             
         BM    TXTFLTNE                                                         
         LR    RE,R1                                                            
         SR    RE,RF               RE=NUMBER OF COMPARES REQUIRED               
         BNP   TXTFLTNE            TEXT NOT LONG ENOUGH TO BOTHER               
                                                                                
TXTF030  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),1(R4)                                                    
         BE    TXTFLTOK                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,TXTF030                                                       
                                                                                
TXTFLTNE MVC   FVMSGNO,=AL2(FVFNOTV) FORCE CC NOT EQUAL (NOT FOUND)             
*                                                                               
TXTFLTOK B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD CONNECTABLE SYSTEM & USER-ID LISTS                            *         
***********************************************************************         
                                                                                
SETCON   LHI   R4,CONNECT-TWAD                                                  
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
*                                                                               
         LA    R2,IOKEY            CHECK NOT ATTACHED TO SECURITY AGY           
         USING CT5REC,R2           R2=A(AGENCY ACCESS RECORD)                   
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    *+10                OFFLINE - DON'T USE DDS INT SEC AGY          
         MVC   CT5KALPH,XIAGYPER   USE AGENCY THAT PID BELONGS                  
         DROP  RE,RF                                                            
         OC    CT5KALPH,CT5KALPH                                                
         BNZ   *+10                                                             
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
*&&US*&& BE    SCONERR             IF FOUND CANT ACCESS SECURITY PROG           
*&&UK*&& BE    SETCONX             IF FOUND EXIT IN UK                          
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
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    *+10                OFFLINE - DON'T USE DDS INT SEC AGY          
         MVC   SA0KAGY,XIAGYPER    USE AGENCY THAT PID BELONGS                  
         DROP  RE,RF                                                            
         OC    SA0KAGY,SA0KAGY                                                  
         BNZ   *+10                                                             
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
         BNZ   SCON06                                                           
         CLI   SASYSLN,SASYSLNQ                                                 
         BNE   SCON06                                                           
*&&UK                                                                           
         CLI   SASYSNUM,X'04'      UK MEDIA/FEE/MPL =CT OVERRIDE                
         BE    SCON06                                                           
         CLI   SASYSNUM,X'05'      UK MEDIA/FEE/MPL =CT OVERRIDE                
         BE    SCON06                                                           
         CLI   SASYSNUM,X'07'      UK MEDIA/FEE/MPL =CT OVERRIDE                
         BE    SCON06                                                           
*&&                                                                             
         B     SCON08                                                           
SCON06   LA    RF,CSYSLST(RE)                                                   
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
         B     SETCONX             IGNORE TERMINAL RECORD IN US & UK !          
*                                                                               
*        LA    R2,IOKEY                                                         
*        USING CTTREC,R2           R2=A(TERMINAL RECORD)                        
*        XC    CTTKEY,CTTKEY                                                    
*        MVI   CTTKTYP,CTTKTYPQ                                                 
*        MVC   CTTKTID,CUADDR                                                   
*        GOTO1 GETIDS,RIO                                                       
*        BE    SETCONX             ALL IDS VALID FOR TERMINAL                   
*                                                                               
*        CLI   C#UID,C#UIDALL      IF ALL VALID FOR PERSON                      
*        BNE   SCON12                                                           
*        MVC   C#UID,RIO             COPY TERMINAL LIST                         
*        LA    R0,CUIDLST                                                       
*        LA    RE,RIO+L'C#UID                                                   
*        LHI   R1,L'CUIDLST*CUIDLSTN                                            
*        LR    RF,R1                                                            
*        MVCL  R0,RE                                                            
*        B     SETCONX                                                          
*                                                                               
*CON12   LA    R1,CUIDLST          R1=A(USER-IDS FOR PERSON)                    
*        XR    R0,R0                                                            
*        ICM   R0,3,C#UID          R0=NUMBER OF USER-IDS FOR PERSON             
*        XR    RE,RE                                                            
*CON14   LA    RF,RIO+L'C#UID      RF=A(USER-IDS FOR TERMINAL)                  
*        ICM   RE,3,RIO            RE=NUMBER OF USER-IDS FOR TERMINAL           
*CON16   CLC   0(L'CTIKNUM,RF),0(R1)                                            
*        BE    SCON18                                                           
*        LA    RF,L'CTIKNUM(RF)                                                 
*        BCT   RE,SCON16                                                        
*        XC    0(L'CTIKNUM,R1),0(R1) DELETE IF USER-ID NOT IN BOTH              
*CON18   LA    R1,L'CTIKNUM(R1)                                                 
*        BCT   R0,SCON14                                                        
*        B     SETCONX                                                          
*                                  INVALID CONNECT AGENCY AUTH                  
SCONERR  MVC   FVMSGNO,=AL2(CE#PACIN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
*                                                                               
SETCONX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ROUTSX                                                           
                                                                                
         DROP  R2,R3                                                            
                                                                                
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET ID LIST                                                                   
*                                                                               
* NTRY: R1=A(OUTPUT)                                                            
*       IOKEY=KEY OF RECORD                                                     
* EXIT: OUTPUT+0 =NO OF USER-IDS IN LIST                                        
*       OUPTUT+1=USER-ID LIST                                                   
*       CC=ZERO IF ALL USER-IDS ARE VALID                                       
***********************************************************************         
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
         MVC   C#UID,RPARM+8                                                    
         MVC   C#UID+1(1),RPARM                                                 
*                                                                               
GETID01  OC    C#UID,C#UID         TEST LIST COUNT IS ZERO                      
         BNZ   GETID10                                                          
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
         ICM   RE,3,C#UID                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,C#UID                                                       
         BCTR  RE,0                                                             
         MHI   RE,L'CTIKID+2                                                    
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
         ICM   R0,3,C#UID                                                       
         LA    R3,CUIDLST          R3=A(NUMBER LIST)                            
         L     R4,ATIA             R4=A(NAME LIST)                              
*                                                                               
GETID16  EQU   *                                                                
         CLI   0(R2),CT0KTEQU      FOR AUTH. RECORD FOR 'ALL' IDS               
         BE    *+12                                                             
         CLI   RFLAG,C'*'          TEST TERMINAL RECORD & DDS TERMINAL          
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
*                                                                               
ALLIDS   DC    CL10'ALL'                                                        
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET USER-ID NUMBER FROM NAME                                                  
*                                                                               
* NTRY: R1=A(NAME)                                                              
* EXIT: R1=A(NUMBER)                                                            
***********************************************************************         
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
                                                                                
***********************************************************************         
* SET SECURITY MANAGER OVERRIDE FLAG                                  *         
*                                                                     *         
* NTRY: R1=A(CONNECT DATA GLOBAL WORK AREA)                           *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
*       COVFLAG SET TO VALUE FROM PERSON RECORD                       *         
***********************************************************************         
SETOVF   NTR1  ,                                                                
                                                                                
         LR    R4,R1                                                            
         USING CONNECT,R4                                                       
         MVI   COVFLAG,0                                                        
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           R2=A(PASSWORD RECORD)                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    *+10                OFFLINE - DON'T USE DDS INT SEC AGY          
         MVC   SA0KAGY,XIAGYPER    USE AGENCY THAT PID BELONGS                  
         DROP  RE,RF                                                            
         OC    SA0KAGY,SA0KAGY                                                  
         BNZ   *+10                                                             
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
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    *+10                OFFLINE - DON'T USE DDS INT SEC AGY          
         MVC   SAPEAGY,XIAGYPER    USE AGENCY THAT PID BELONGS                  
         DROP  RE,RF                                                            
         OC    SAPEAGY,SAPEAGY                                                  
         BNZ   *+10                                                             
         MVC   SAPEAGY,CUAALF                                                   
**IT DOESN'T MAKE SENSE TO USE THE AGY= OPTION AS THE AGENCY ******             
**TO READ THE PID RECORD AND GET THE OVERRIDE FLAG, ***************             
**BUT JUST LEAVE IT HERE FOR THE TIME BEING. *9/28/04, YYUN********             
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+10                                                             
         MVC   SAPEAGY,OPTAGY                                                   
*******************************************************************             
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
* TEST USER-ID IN LIST                                                          
***********************************************************************         
TSTUID   OC    OPTAGY,OPTAGY                                                    
         BNZ   ROUTSX                                                           
*                                                                               
         LHI   R4,CONNECT-TWAD                                                  
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
*                                                                               
         CLI   C#UID,C#UIDALL      TEST ALL USER-IDS ARE ALLOWED                
         BE    ROUTSX                                                           
         OC    0(L'CUIDLST,R1),0(R1)                                            
         BZ    ROUTSX                                                           
         XR    R0,R0                                                            
         ICM   R0,3,C#UID                                                       
         BZ    TUID05                                                           
         LA    R2,CUIDLST                                                       
*                                                                               
TUID02   CLC   0(L'CUIDLST,R2),0(R1)                                            
         BE    TSTUIDX                                                          
         LA    R2,L'CUIDLST(R2)                                                 
         BCT   R0,TUID02                                                        
*                                                                               
TUID05   MVC   FVMSGNO,=AL2(CE#NCUID)                                           
TSTUIDX  B     ROUTSX                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TEST SYSTEM IN LIST                                                 *         
*                                                                     *         
* NTRY: R1=A(SYSTEM NUMBER)                                           *         
***********************************************************************         
                                                                                
TSTSYS   L     RF,ACASEC           'DDS' PASSWORD CAN CONNECT TO ANY            
         TM    SECINDS-SECD(RF),SECIDDS                                         
         BO    TSTSYSX                                                          
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    TSYS01              OFFLINE - DON'T USE DDS INT SEC AGY          
         OC    XIPID,XIPID                                                      
         BZ    TSYS01                                                           
         CLC   XIPID,=X'1000'                                                   
         BL    TSTSYSX             DDS INTERNAL SEC PID - SKIP CHECKING         
         DROP  RE,RF                                                            
TSYS01   EQU   *                                                                
*&&UK                                                                           
         CLC   CUAALF,=C'**'       TEST SPECIAL AGENCY ALPHA                    
         BE    TSTSYSX               IF SO ACCESS ALLOWED                       
*&&                                                                             
         LHI   R4,CONNECT-TWAD                                                  
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
                                                                                
SETSEL   MVC   RBYTE,0(R1)         GET SYSTEM NUMBER                            
         LHI   R4,CONNECT-TWAD                                                  
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
         JNE   *+2                                                              
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         SR    RF,RF                                                            
*                                  FIRST SEARCH FOR SECURITY PID                
SSEL002  CLI   0(R3),0                                                          
         BE    SSEL006                                                          
         CLI   0(R3),CTSPUELQ                                                   
         BE    SSEL004             SECURITY PID ELEMENT FOUND                   
*                                                                               
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     SSEL002                                                          
*                                                                               
         USING CTSPUD,R3                                                        
SSEL004  MVC   APHALF,CTSPUNUM     SAVE SECURITY USER ID #                      
         B     SSEL009                                                          
         DROP  R3                                                               
*                                                                               
SSEL006  EQU   *                                                                
         L     R2,AIOAREA2                                                      
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
         SR    RF,RF                                                            
*                                  ELSE SEARCH FOR CONTROL PID                  
SSEL007  CLI   0(R3),0                                                          
         BE    SSEL009             IF NOT FOUND STICK WITH CONNECT ID           
         CLI   0(R3),X'02'                                                      
         BE    SSEL008             CONTROL PID ELEMENT FOUND                    
*                                                                               
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     SSEL007                                                          
*                                                                               
SSEL008  MVC   APHALF,2(R3)        SAVE PRINCIPLE USER ID #                     
*                                                                               
SSEL009  EQU   *                                                                
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
         JNE   *+2                                                              
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R3,SAIDATA          R3=A(FIRST ELEMENT)                          
         USING SASYSD,R3           R3=A(SYSTEM AUTH. ELEMENT)                   
SSEL020  CLI   SASYSEL,0                                                        
         BE    SSELER                                                           
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
SSELER   MVC   FVMSGNO,=AL2(CE#NCSYS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     SSELX                                                            
*                                                                               
SSELX    B     ROUTSX                                                           
                                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* TEST PROGRAM AUTHORISATION IN SAVED SYSTEM ELEMENT                  *         
*                                                                     *         
* NTRY: R1=A(PROGRAM NUMBER)                                          *         
* EXIT - APWORK+0(2)=AUTHORISATION CODE                               *         
***********************************************************************         
                                                                                
TSTPGM   MVC   RBYTE,0(R1)         GET PROGRAM CODE                             
         LHI   R4,CONNECT-TWAD                                                  
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
TPGM010  CHI   RE,16                                                            
         BNH   TPGM100                                                          
         CLC   RBYTE,0(R1)                                                      
         BE    TPGM020                                                          
         LA    R1,3(R1)            GET NEXT PROGAM CODE                         
         SHI   RE,3                                                             
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
                                                                                
TSTOMAN  EQU   *                                                                
         MVI   APBYTE,0                                                         
         MVC   RWORK(L'SAOFOID),0(R1)  SAVE OFFICE CODE                         
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    TOMA005             OFFLINE - DON'T USE DDS INT SEC AGY          
         OC    XIPID,XIPID                                                      
         BZ    TOMA005                                                          
         CLC   XIPID,=X'1000'                                                   
         BL    TOMAX               DDS INTERNAL SEC PID - SKIP CHECKING         
         DROP  RE,RF                                                            
TOMA005  EQU   *                                                                
*                                                                               
         LHI   RF,CONNECT-TWAD                                                  
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
TOMA020  EQU   *                                                                
         MVI   APBYTE,X'FF'                                                     
         CLC   SAMANID,CUPASS                                                   
         BE    ROUTSX                                                           
         B     TOMA012                                                          
TOMAX    EQU   *                                                                
         CLI   APBYTE,0                                                         
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
                                                                                
TSTGMAN  EQU   *                                                                
         MVI   RBYTE,0                                                          
         MVC   RWORK(L'SAAGAGR),0(R1)  SAVE GROUP CODE                          
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    TGMA005             OFFLINE - DON'T USE DDS INT SEC AGY          
         OC    XIPID,XIPID                                                      
         BZ    TGMA005                                                          
         CLC   XIPID,=X'1000'                                                   
         BL    TGMAX               DDS INTERNAL SEC PID - SKIP CHECKING         
         DROP  RE,RF                                                            
TGMA005  EQU   *                                                                
*                                                                               
         LHI   RF,CONNECT-TWAD                                                  
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
TGMA020  EQU   *                                                                
         MVI   RBYTE,X'FF'                                                      
         CLC   SAMANID,CUPASS                                                   
         BE    ROUTSX                                                           
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
                                                                                
TSTDMAN  EQU   *                                                                
         LM    R3,R4,0(R1)                                                      
         MVI   RBYTE,0                                                          
*                                                                               
         MVC   RWORK(L'SADPOID),0(R3)  SAVE OFFICE CODE                         
         MVC   RWORK+L'SADPOID(L'SADPDID),0(R4)  SAVE DEPARTMENT CODE           
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    TDMA005             OFFLINE - DON'T USE DDS INT SEC AGY          
         OC    XIPID,XIPID                                                      
         BZ    TDMA005                                                          
         CLC   XIPID,=X'1000'                                                   
         BL    ROUTSX              DDS INTERNAL SEC PID - SKIP CHECKING         
         DROP  RE,RF                                                            
TDMA005  EQU   *                                                                
*                                                                               
         LHI   RF,CONNECT-TWAD                                                  
         A     RF,ATWA                                                          
         USING CONNECT,RF                                                       
         TM    COVFLAG,COVFON      TEST IF PERSON OVERRIDE FLAG ON              
         BO    ROUTSX                                                           
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
TDMADATA LA    R4,SADPDATA                                                      
*                                                                               
TDMA010  CLI   0(R4),0                                                          
         BE    TDMAX                                                            
         CLI   0(R4),SAMANELQ                                                   
         BE    TDMA020                                                          
TDMA012  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     TDMA010                                                          
         USING SAMAND,R4                                                        
TDMA020  EQU   *                                                                
         MVI   RBYTE,X'FF'                                                      
         CLC   SAMANID,CUPASS                                                   
         BE    TDMAXX                                                           
         B     TDMA012                                                          
TDMAX    EQU   *                                                                
         CLI   RBYTE,0                                                          
         BE    TDMAX1                                                           
         GOTO1 ATSTOMAN,(R3)                                                    
         BNE   TDMAXX                                                           
         CLI   APBYTE,0                                                         
         BNE   TDMAXX                                                           
         MVC   FVMSGNO,=AL2(CE#SECMN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX                                                           
*                                  CHECK OVERIDING OFFICE MANAGER               
TDMAX1   GOTO1 ATSTOMAN,(R3)                                                    
TDMAXX   B     ROUTSX                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE 1R PERSON CODE IN ACCOUNT SYSTEM                *         
*                                                                     *         
* NTRY: R1=A(1R PERSON CODE FIELD HEADER)                             *         
* EXIT: IF CODE VALID CC=EQUAL                                        *         
*               APHALF=USER-ID NUMBER, APWORK=NAME                    *         
***********************************************************************         
                                                                                
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
                                                                                
GCMPOK   SR    RC,RC                                                            
GCMPNO   LTR   RC,RC                                                            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AGENCY ACCESS DETAILS                                *         
*                                                                     *         
* NTRY: R1=A(AGENCY ALPHA)                                            *         
* EXIT: ACCESS DETAILS IN APWORK                                      *         
*       APWORK+0 = ACCESS FLAGS                                       *         
*       APWORK+1 = PASSWORD TIMEOUT DAYS                              *         
*       APWORK+2 = PASSWORD MINIMUM LENGTH                            *         
*       APWORK+3 = PASSWORD REUSE COUNT                               *         
*       APWORK+4 = PASSWORD RULE                                      *         
***********************************************************************         
                                                                                
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
*                                                                               
         LA    R3,CT5DATA          R3=A(FIRST ELEMENT)                          
GAAD010  CLI   0(R3),0             TEST EOR                                     
         BE    GAADX                                                            
         CLI   0(R3),CTAGDELQ      X'B4' AGENCY GROUP DETAILS                   
         BE    GAAD030                                                          
         CLI   0(R3),CTAADELQ      X'B9' AGENCY ACCESS DETAILS                  
         BE    GAAD040                                                          
GAAD020  LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GAAD010                                                          
*                                                                               
         USING CTAGDD,R3                                                        
GAAD030  MVC   APWORK+5(1),CTAGOPTS  AGENCY OPTIONS                             
         B     GAAD020                                                          
         DROP  R3                                                               
*                                                                               
         USING CTAADD,R3                                                        
GAAD040  MVC   APWORK+0(1),CTAADFLG                                             
         MVC   APWORK+1(1),CTAADPTO                                             
         MVC   APWORK+2(1),CTAADPML                                             
         MVC   APWORK+3(1),CTAADPRU                                             
         MVC   APWORK+4(1),CTAADPVR                                             
         B     GAAD020                                                          
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
         EX    RF,*+8                                                           
         B     *+10                                                             
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
                                                                                
***********************************************************************         
* ROUTINE TO GET TIME SHEET APPROVER GROUP RECORD DATA                *         
*                                                                     *         
* NTRY: P1=A(APPROVER GROUP CODE)                                     *         
* EXIT: APWORK=NAME APHALF=NUMBER                                     *         
***********************************************************************         
                                                                                
GETAPG   XC    APWORK,APWORK                                                    
*                                                                               
         LA    R2,IOKEY                                                         
         USING SAAPREC,R2          R2=A(APPROVER GROUP RECORD KEY)              
         XC    SAAPKEY,SAAPKEY                                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,CUAALF                                                   
         OC    OPTAGY,OPTAGY       TEST OPTION AGENCY=                          
         BZ    *+10                                                             
         MVC   SAAPAGY,OPTAGY                                                   
         MVC   SAAPAGR,0(R1)                                                    
*                                                                               
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOCONFIL+IOREAD                                              
         BE    *+14                PROGRAM RECORD MUST BE ON FILE               
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     GAPGX                                                            
*                                                                               
         LA    R3,SAAPDATA                                                      
*                                                                               
GAPG010  CLI   0(R3),0                                                          
         BE    ROUTSX                                                           
         CLI   0(R3),SAAPGELQ                                                   
         BE    GAPG020                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GAPG010                                                          
         USING SAAPGD,R3                                                        
GAPG020  ZIC   R1,SAAPGLN                                                       
         SHI   R1,SAAPGLNQ+1                                                    
         BM    GAPG030                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SAAPGNAM                                               
GAPG030  MVC   APHALF,SAAPGNUM                                                  
*                                                                               
GAPGX    B     ROUTSX                                                           
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GRIDS OUTPUT ROUTINE                                                          
***********************************************************************         
GRIDS    MVC   RGPARM(8),0(R1)     SAVE PARMS                                   
         BRAS  RE,PGRIDS                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
         LTORG                                                                  
*                                                                               
FVFNOTH  EQU   005+X'FF00'         FIELD SHOULD BE HEXADECIMAL                  
*                                                                               
CTFILE   DC    C'CTFILE '                                                       
GENFIL   DC    C'GENFIL '                                                       
*                                                                               
CONADDRS DS    0F                  ** ADDRESS OF ROUTINES ETC. **               
         DC    A(PHASES)                                                        
         DC    A(HOOK)                                                          
         DC    A(OBASE)                                                         
         DC    A(ROUTS)                                                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
*                                                                               
       ++INCLUDE SPCGRTAB          SPOT/NET/PRINT/TRAFFIC CLIENT GROUP          
*                                                                               
* SEACSDICT                                                                     
       ++INCLUDE SEACSDICT                                                      
*                                                                               
PHASES   DS    0X                  ** LOADED CORERES PHASES **                  
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* ANALYZE OPTIONS                                                               
***********************************************************************         
CHKOPTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   R4,CONNECT-TWAD                                                  
         A     R4,ATWA                                                          
         USING CONNECT,R4                                                       
         CLC   SAVOPTS,INOPTS            OPTIONS CHANGED?                       
         BE    *+8                       . NO                                   
         NI    PCGRIDS,X'FF'-PCGDISQ     . YES,RESET GRIDS DISPLAY BITS         
         DROP  R4                                                               
*                                                                               
         ICM   RE,15,ARECNTRY      GET A(RECTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
         USING RECTABD,RE                                                       
         CLI   RECNUMB,RECPWD      PASSWORD RECORD?                             
         BNE   HKOPT20                                                          
         DROP  RE                                                               
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    HKOPTX                                                           
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    HKOPTX                                                           
*                                                                               
         CLC   OPTAGY,XIAGYPER     AGY = PID AGY?                               
         BE    HKOPTX                                                           
         B     HKOPTBAD                                                         
         DROP  RE,RF                                                            
*                                                                               
HKOPT20  ICM   RE,15,AMIXNTRY      GET A(MIXTABLE ENTRY)                        
         JZ    *+2                                                              
*                                                                               
         USING MIXTABD,RE                                                       
         CLI   MIXRECB,RECSYS      SYSTEM RECORD?                               
         BNE   HKOPT50                                                          
         CLI   MIXACTB,ACTREP      REPORT RECORD?                               
         BNE   HKOPTX                                                           
         DROP  RE                                                               
*                                                                               
         OC    OPTPWS,OPTPWS                                                    
         BZ    HKOPTX                                                           
*                                                                               
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    HKOPTX                                                           
         CLC   XIAGYSEC,XIAGYPER   SEC AGY = PID AGY?                           
         BNE   HKOPTBAD                                                         
         OC    OPTAGY,OPTAGY       ANY OPTION AGY                               
         BZ    *+14                                                             
         CLC   OPTAGY,XIAGYPER     AGY = PID AGY?                               
         BNE   HKOPTBAD                                                         
         DROP  RE,RF                                                            
*                                                                               
         OC    ACASEC,ACASEC                                                    
         BZ    *+16                                                             
         L     RF,ACASEC           'DDS' PASSWORD                               
         TM    SECINDS-SECD(RF),SECIDDS                                         
         BO    HKOPTBAD             YES                                         
         B     HKOPTX                                                           
*                                                                               
HKOPT50  CLI   OPTCTL,C' '         ANY SPECIAL CONTROL OPTION?                  
         B     HKOPTX              ALL GOOD, EXIT                               
*                                                                               
         BNH   HKOPTX              SAVE THIS CODE FOR THE FUTURE                
         MVI   APBYTE,OPTCTLN                                                   
         GOTO1 VSECRET,APPARM,('SECPOPTP',ACASEC),APBYTE                        
         BE    HKOPTX              ALL GOOD, EXIT                               
*                                                                               
HKOPTNOA MVC   FVMSGNO,=AL2(FVFNAOPT) NOT AUTHORIZED FOR OPTION                 
         J     EXIT                                                             
HKOPTBAD MVC   FVMSGNO,=AL2(FVFNOTV)  INVALID OPTION                            
         J     EXIT                                                             
HKOPTX   MVC   FVMSGNO,=AL2(FVFOK)                                              
         J     EXIT                                                             
         DROP  R5                                                               
                                                                                
***********************************************************************         
*  GET RECORD, ACTION AND PFKEY MATCH FROM PFKTABLE                   *         
***********************************************************************         
CHKPFK   NTR1  BASE=*,LABEL=*                                                   
         L     R5,ATWA                                                          
         USING TWAD,R5                                                          
*                                                                               
*        XC    APFKENTR,APFKENTR                                                
         NI    TWASWPST,X'FF'-TWASWAP                                           
         MVC   TWASWPRE,SCRECN     INITIALIZE SWAP RECORD                       
         MVC   TWASWPAC,SCACTN     INITIALIZE SWAP ACTION                       
         CLI   APPFKEY,0                                                        
         BE    CKPFYEX                                                          
         CLI   INACT,ACTSEL        MAKE SURE IT IS SELECT MODE                  
         BE    *+12                                                             
         NI    TWAMODE,X'FF'-TWAMSEL                                            
         MVI   SVSELACT,0                                                       
         L     R1,AACTHDR                                                       
         ST    R1,FVADDR                                                        
*                                                                               
         USING PFKTABD,R3                                                       
         L     R3,ACPFKTAB         FIND REC/ACTION/PFK IN PFK TABLE             
CKPF30   CLI   PFKSEL,EOT                                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(239)   INVALID PFKEY                                
         B     CKPFNOX                                                          
         CLC   PFKSEL,APPFKEY      MATCH ON PFKEY                               
         BNE   CKPF32                                                           
         GOTOR VALPFK,RPARM,(R3)                                                
         BE    CKPF40                                                           
*                                                                               
CKPF32   LA    R3,PFKTABL(,R3)                                                  
         B     CKPF30                                                           
*                                                                               
         USING RECTABD,R2                                                       
CKPF40   TM    PFKIND,PFKIRST      RESTORE SAVED REC/ACT?                       
         BZ    CKPF45                                                           
         MVI   TWASWPAC,ACTDIS     DEFAULT TO DISPLAY                           
         CLI   SVRECORD,0                                                       
         BE    CKPF43                                                           
         MVC   TWASWPRE,SVRECORD                                                
         MVC   TWASWPAC,SVACTION                                                
*                                                                               
CKPF43   OI    TWASWPST,TWASWAP                                                 
         CLI   PFKNEWRE,0          RESTORE RECORD ONLY?                         
         BE    *+10                YES, SO FAR                                  
         MVC   TWASWPRE,PFKNEWRE   NO, ALWAYS USE PFK RECORD                    
         CLI   PFKNEWAC,0          RESTORE ACTION ONLY?                         
         BE    *+10                YES                                          
         MVC   TWASWPAC,PFKNEWAC   NO, ALWAYS USE PFK ACTION                    
         B     CKPF90                                                           
*                                                                               
CKPF45   CLC   PFKNEWRE,INREC                                                   
         BE    *+16                                                             
         XC    SVRECORD,SVRECORD                                                
         XC    SVACTION,SVACTION                                                
         TM    PFKIND,PFKISAV      SHOULD WE SAVE CURRENT REC/ACT               
         BZ    CKPF50              NO                                           
         MVC   SVRECORD,INREC      CURRENT RECORD                               
         MVC   SVACTION,INACT      CURRENT ACTION                               
         TM    TWAMODE,TWAMLSM     LIST SELECT MODE?                            
         BZ    CKPF50              NO                                           
         MVC   SVRECORD,SCRECN     CURRENT RECORD                               
         MVC   SVACTION,SCACTN     CURRENT ACTION                               
*                                                                               
CKPF50   CLI   SVACTION,ACTADD     WAS ACTION SAVED ADD ?                       
         BNE   *+8                                                              
         MVI   SVACTION,ACTCHA     SWITCH ADD TO CHANGE                         
         CLC   PFKREC,PFKNEWRE     IS THERE A NEW RECORD TYPE?                  
         BE    CKPF60                                                           
         MVC   TWASWPRE,PFKNEWRE   SWAP RECORD                                  
         OI    TWASWPST,TWASWAP    TURN ON SWAP                                 
*                                                                               
CKPF60   CLI   PFKNEWAC,0          NO NEW ACTION?                               
         BE    CKPF90                                                           
         OI    TWASWPST,TWASWAP    TURN ON SWAP                                 
         MVC   TWASWPAC,PFKNEWAC   SWAP ACTION                                  
*                                                                               
CKPF90   EQU   *                                                                
*KPF90   ST    R3,APFKENTR         SAVE ENTRY OF PFKEY TABLE                    
*                                                                               
CKPFYEX  XR    RC,RC                                                            
CKPFNOX  LTR   RC,RC                                                            
CKPFX    J     EXIT                                                             
                                                                                
***********************************************************************         
* BUILD AND DISPLAY PFKEYS IN EXTEND FIELD HEADERS                              
*        #254 (X'FE') PFKEY FIELD LINE                                          
***********************************************************************         
         USING PFKTABD,R3                                                       
BLDPFK   NTR1  BASE=*,LABEL=*                                                   
         L     R5,ATWA                                                          
         USING TWAD,R5                                                          
*                                                                               
         L     R2,AACTHDR                                                       
         SR    R1,R1                                                            
         XC    APFKHDR1,APFKHDR1                                                
         XC    APFKHDR2,APFKHDR2                                                
*                                                                               
BPF05    CLI   0(R2),0             AT END OF SCREEN?                            
         BE    BPF10               YES GET OUT FINISHED FOR NOW                 
         IC    R1,0(,R2)           LENGTH OF FIELD                              
         TM    1(R2),FVAXTND       EXTENDED FIELD HEADER?                       
         BZ    BPF09               NO, SO BUMP TO NEXT                          
*                                                                               
BPF06    SHI   R1,8                SUBTRACT LENGTH OF EXTEND FIELD              
         LR    RF,R2               USE RF FOR CALCULATIONS                      
         AR    RF,R1               BUMP TO EXTENDED FIELD HEADER                
         LA    R3,APFKHDR1         PFKEY LINE ADDRESS                           
         OC    APFKHDR1,APFKHDR1   DID WE ALREADY SAVE AN ADDRESS?              
         BZ    *+8                 NO, SO SAVE IN HDR1                          
         LA    R3,APFKHDR2         YES, SO SAVE 2ND LINE IN HDR2                
         CLI   0(RF),X'FE'         254 (PFK HELP)                               
         BNE   BPF08               FOUND PFKEY LINE                             
*                                                                               
BPF07    SHI   R1,8                R1 = SIZE OF FIELD                           
         STH   R1,4(,R3)           SIZE OF FIELD                                
         ST    R2,0(,R3)           SAVE ADDRESS OF SCREEN                       
*                                                                               
BPF08    IC    R1,0(,R2)           RESTORE BUMPING LENGTH                       
*                                                                               
BPF09    AR    R2,R1               BUMP (IN THE NIGHT)                          
         B     BPF05               TRY AGAIN                                    
*                                                                               
BPF10    OC    APFKHDR1,APFKHDR1   ANY PFKEY LINE?                              
         BZ    BPFX                NO                                           
         MVI   RBYTE,0             FIRST PASS,I.E. FULL LNGTH FOR NAMES         
*                                                                               
BPF12    MVI   RPFKAREA,C' '       CLEAR AREA                                   
         MVC   RPFKAREA+1(L'RPFKAREA-1),RPFKAREA                                
         ICM   R2,15,APFKHDR2      CLEAR 2ND PFKEY HELP LINE IF ANY             
         BZ    BPF15               NONE                                         
         TWAXC (R2),(R2),PROT=Y                                                 
*                                                                               
BPF15    L     R2,APFKHDR1         CLEAR 1ST PFKEY LINE                         
         TWAXC (R2),(R2),PROT=Y                                                 
*                                                                               
         USING PFKTABD,R3                                                       
         L     R3,ACPFKTAB                                                      
         LA    R4,RPFKAREA+2                                                    
BPF20    CLI   PFKREC,EOT          END OF TABLE?                                
         BE    BPF60               YES, SO CLEAN UP                             
         GOTOR VALPFK,RPARM,(R3)                                                
         BNE   BPF50                                                            
         TM    PFKIND,PFKIRST      RESTORE TYPE PFKEY?                          
         BZ    BPF30               NO, SO OK                                    
         TM    TWAMODE,TWAMSEL+TWAMLSM                                          
         BNZ   BPF30                                                            
         CLI   PFKNEWRE,0                                                       
         BNE   BPF30               NEXT                                         
         CLI   SVRECORD,0          ANYTHING TO RESTORE?                         
         BE    BPF50               NO, SO NOT VALID PFKEY                       
*                                                                               
BPF30    MVC   RPFKAREA(2),=C'PF'  MOVE IN PF                                   
         SR    R1,R1                                                            
         CLI   PFKNAME,C' '        IGNORE PFKNAME IF SPACES                     
         BE    BPF50               LOOP FOR NEXT PFK                            
         IC    R1,PFKSEL           GET NUMBER                                   
         CVD   R1,RDUB             CONVERT TO CHARACTER                         
         OI    RDUB+7,X'0F'        ADJUST SIGN FOR CHARACTER FORMAT             
         UNPK  RWORK(2),RDUB+6(2)                                               
         MVC   RWORK+2(1),SCEQUAL  PUT THE NUMBER EQUALS "="                    
         LA    R0,2                SAVE LENGTH OF TWO                           
         MVC   0(2,R4),RWORK+1     R4=PFKEY AREA                                
         CLI   RWORK,C'0'          IF 1-9 ONLY 2 CHARACTERS NEEDED              
         BE    *+14                                                             
         LA    R0,3                SAVE LENGTH OF THREE                         
         MVC   0(3,R4),RWORK       IF >9 THEN 3 CHARACTERS                      
         AR    R4,R0               BUMP UP IN PFKEY AREA                        
*                                                                               
         MVC   RPFKNAM,PFKNAME                                                  
         CLI   RPFKNAM,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNL   BPF32                                                            
         CLI   RBYTE,0             USING FULL LEN FOR PFK NAMES?                
         BE    BPF31               YES, CONTINUE                                
         TM    PFKIND2,PFKSHRTL    ANY SHORT LEN NAME AVAILABLE?                
         BZ    BPF31               NO, SKIP                                     
         MVI   RPFKNAM+3,PFKNAMSQ  USE SHORT LENGTH NAME                        
*                                                                               
BPF31    GOTO1 VDICTAT,RPARM,C'SL  ',RPFKNAM,0                                  
*                                                                               
BPF32    CLI   RBYTE,0             USING FULL LEN FOR PFK NAMES?                
         BE    BPF33               YES, USE FULL LENGTH                         
         TM    PFKIND2,PFKSHRTL    ANY SHORT LEN NAME AVAILABLE?                
         BO    BPF34               YES, USE SHORT LENGTH                        
*                                                                               
BPF33    MVC   0(PFKNAMEQ,R4),RPFKNAM  PFKEY NAME WITH LONG NAME                
         LA    R4,PFKNAMEQ-1(R4)       LAST BYTE OF NAME                        
         B     BPF35                   CONTINUE                                 
*                                                                               
BPF34    MVC   0(PFKNAMSQ,R4),RPFKNAM  PFKEY NAME WITH SHORT NAME               
         LA    R4,PFKNAMSQ-1(R4)       LAST BYTE OF NAME                        
*                                                                               
BPF35    CLI   0(R4),C' '          FIND NONE SPACE CHARACTER                    
         BNE   *+8                                                              
         BCT   R4,BPF35                                                         
         LA    R4,2(,R4)           BUMP UP BY ONE                               
*                                  FIND NUM OF BYTES USED                       
         LA    R2,RPFKAREA         START OF AREA                                
         LR    R6,R4               END OF AREA SO FAR                           
         SR    R6,R2               LENGTH USED                                  
         CH    R6,=Y(MAXPFLEN)     DOES THIS PFKEY FIT?                         
         BL    BPF50               YES, LOOP FOR NEXT PFKEY                     
         CLI   RBYTE,0             USING FULL  LEN  FOR  PFK  NAMES ?           
         BNE   BPF60               NO,   NO    MORE WILL FIT,                   
         MVI   RBYTE,1             USE   SHORT PFK  NAMES                       
         B     BPF12               REDO  WITH  SHORT     PFK  NAMES             
*                                                                               
BPF50    LA    R3,PFKTABL(,R3)                                                  
         B     BPF20                                                            
*                                                                               
BPF60    LA    R2,RPFKAREA         START OF PFKEY LINE                          
         LR    R3,R2                                                            
         LH    R1,PFKLEN1          LENGTH OF PFKEY FIELD                        
         AR    R3,R1               POINT INTO PFKAREA                           
         CLI   0(R3),C' '          FIND FIRST BLANK                             
         BE    BPF62                                                            
         BCTR  R3,0                                                             
         BCT   R1,*-10                                                          
         LH    R1,PFKLEN1                                                       
*                                                                               
BPF62    BCTR  R1,0                                                             
         L     R2,APFKHDR1                                                      
         OI    6(R2),FVOXMT        TRANSMIT                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RPFKAREA                                                 
         LH    R1,PFKLEN2                                                       
         BCTR  R1,0                                                             
         ICM   R2,15,APFKHDR2      GET 2ND PFK SCREEN ADDRESS                   
         BZ    BPFX                NONE, SO GET OUT                             
         OI    6(R2),FVOXMT        TRANSMIT                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)      MOVE REMAINDER TO SCREEN, 2ND LINE            
*                                                                               
BPFX     J     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
*  VALIDATE PFKEY ENTRY PASSED IN R3                                            
***********************************************************************         
         USING PFKTABD,R3                                                       
         USING TWAD,R5                                                          
VALPFK   NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(,R1)           LOAD TABLE PFK ENTRY                         
         CLI   PFKREC,EOT          END OF TABLE                                 
         BE    VALPFKNO            INVALID PFKEY PRESSED                        
         CLI   PFKSCR,0            DO WE NEED TO MATCH SCREEN?                  
         BE    VALPFK05            NO                                           
         CLC   PFKSCR,INSCRN       MATCH TO CURRENT SCREEN?                     
         BNE   VALPFKNO            NO MATCH                                     
*                                                                               
VALPFK05 CLC   PFKREC,SCRECN       CURRENT RECORD                               
         BNE   VALPFKNO            NO MATCH                                     
         CLI   PFKACT,ANYACT       VALID FOR ANY ACTION?                        
         BE    VALPFK20            NO ACTION NEED TO MATCH                      
         TM    PFKIND,PFKISAT      SELECT ACTION MATCHING?                      
         BZ    VALPFK10                                                         
         CLC   PFKACT,SVSELACT     MUST BE SET IN DISKEY                        
         BE    VALPFK20                                                         
         B     VALPFKNO                                                         
*                                                                               
VALPFK10 CLC   PFKACT,INACT        DOES ACTION MATCH? (FOR SELECT)              
         BE    VALPFK20            OK SO FAR                                    
         CLC   PFKACT,SCACTN       DOES ACTION MATCH?                           
         BE    VALPFK20            NO MATCH ON ACTION                           
         TM    TWAMODE,TWAMSEL                                                  
         BZ    VALPFKNO                                                         
         CLI   PFKACT,ACTSEL       DOES ACTION MATCH?                           
         BNE   VALPFKNO            NO MATCH ON ACTION                           
*                                                                               
VALPFK20 CLI   PFKTYPE,ANYTYPE     VALID FOR ANY TYPE?                          
         BE    VALPFK25                                                         
         CLC   PFKTYPE,APRASTY#    DOES TYPE MATCH?                             
         BNE   VALPFKNO            NO, NOT A VALID PFKEY                        
*                                                                               
VALPFK25 TM    PFKIND,PFKDDS       DDS ONLY PFKEY?                              
         BZ    VALPFK30            NO, SO CONTINUE                              
         TM    CUSTAT,CUSDDS       IS IT DDS TERMINAL?                          
         BZ    VALPFKNO            NO, SO GET NEXT PFKEY                        
*                                                                               
VALPFK30 TM    PFKIND,PFKILSM      SPECIAL MODE VALIDATION?                     
         BZ    VALPFK35                                                         
         TM    TWAMODE,TWAMLSM     LSM MODE?                                    
         BZ    VALPFKNO            NOT RIGHT MODE                               
*                                                                               
VALPFK35 TM    PFKIND,PFKISEL      SEL MODE?                                    
         BZ    VALPFK40                                                         
         TM    TWAMODE,TWAMSEL                                                  
         BZ    VALPFKNO            NOT VALID PFK                                
*                                                                               
VALPFK40 TM    PFKIND,PFKILFM                                                   
         BZ    VALPFK45                                                         
         TM    TWAMODE,TWAMSEL+TWAMLSM                                          
         BNZ   VALPFKNO                                                         
*                                                                               
VALPFK45 OC    TWASAGN,TWASAGN     NEW SECURITY?                                
         BZ    VALPFKOK            NO                                           
         MVC   RHALF(1),INREC      CURRENT RECORD                               
         CLI   PFKNEWRE,0          IS THERE A NEW RECORD TYPE?                  
         BE    *+10                NO                                           
         MVC   RHALF(1),PFKNEWRE   YES SO USE THAT ONE                          
         MVC   RHALF+1(1),INACT    CURRENT ACTION                               
         TM    TWAMODE,TWAMLSM     LIST/SELECT MODE                             
         BZ    *+10                                                             
         MVC   RHALF+1(1),SCACTN   CURRENT ACTION                               
         CLI   PFKNEWAC,0          IS THERE A NEW ACTION TYPE?                  
         BE    *+10                NO                                           
         MVC   RHALF+1(1),PFKNEWAC YES SO USE THAT ONE                          
         TM    PFKIND,PFKIRST                                                   
         BZ    VALPFK80                                                         
         MVI   RHALF+1,ACTDIS      DEFAULT TO DISPLAY                           
         CLI   SVRECORD,0          ANYTHING SAVED?                              
         BE    VALPFK80            NO                                           
         MVC   RHALF(1),SVRECORD                                                
         MVC   RHALF+1(1),SVACTION                                              
*                                                                               
VALPFK80 LA    RF,RHALF                                                         
         GOTO1 VSECRET,RPARM,('SECPRACT',ACASEC),(0(RF),1(RF))                  
         BNE   VALPFKNO            NO GOOD, GET NEXT                            
*                                                                               
VALPFKOK SR    RE,RE                                                            
VALPFKNO LTR   RE,RE                                                            
         J     EXIT                                                             
         DROP  R3,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS GRIDS                                                                 
***********************************************************************         
PGRIDS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   R2,DLCB-SYSWORK           DOWN-LOAD CONTROL BLOCK                
         A     R2,ASYSWORK                                                      
         USING DLCBD,R2                                                         
*                                                                               
         LHI   R3,PCDRIVEN-TWAD                                                 
         A     R3,ATWA                                                          
         USING PCDRIVEN,R3                                                      
*                                                                               
         CLI   RGPARM,C'C'               CLEAR SCREEN                           
         BNE   GR006                                                            
         GOTOR DWNL,DWNCLEAR                                                    
         J     EXIT                                                             
*                                                                               
GR006    OC    DLCBAPR,DLCBAPR           INIT?                                  
         BNZ   GR009                     . INIT ALREADY DONE                    
         GOTOR DWNL,DWNINIT                                                     
         JNE   *+2                                                              
*                                                                               
         CLI   RGPARM,C'E'               END OF REPORT                          
         BE    GR290                                                            
*                                                                               
         CLI   RGPARM,C'F'               INIT THEN SCREEN FLUSH?                
         BNE   GR010                     . NO, CONTINUE                         
         MVC   FVOMSG,GRMSGPR            . YES, THEN JUST PUSH RETURN           
         J     EXIT                                                             
*                                                                               
GR009    CLI   RGPARM,C'F'               SCREEN FLUSH                           
         BNE   GR010                                                            
         BRAS  RE,OUTGRITS               OUTPUT GRIDS SCREEN                    
         J     EXIT                                                             
*                                                                               
GR010    CLI   RGPARM,C'E'               END OF REPORT                          
         BE    GR300                                                            
*                                                                               
         XC    RGRCID#,RGRCID#           RECURRING COLUMN COUNT                 
         MVI   RFLAG,RFG1STQ             SPECIAL PROCESS FOR 1ST COL            
         L     R1,RGPARM                 A(GRIDS COLUMN TABLE)                  
         LA    R4,GRFLNQ(R1)             FIRST COLUMN ENTRY                     
         USING GRIDCD,R4                                                        
         CLI   GRCID,GRLDQ               GRID DESCRIPTION LINE DATA?            
         BNE   GR020                                                            
         BRAS  RE,PGRDLDA                                                       
                                                                                
*----------------------------------------                                       
* PROCESS GRID COLUMNS                                                          
*----------------------------------------                                       
GR020    CLI   GRCID,X'FF'               END OF COLUMN TABLE?                   
         MVC   RGRCID,GRCID                                                     
         BE    GR200                                                            
         CLI   GRCID,GRLDQ               GRID DESCRIPTION LINE DATA?            
         BNE   GR022                                                            
         AHI   R4,GRLLNQ                 SKIP DESCRIPTION LINE DATA             
         B     GR020                                                            
                                                                                
*----------------------------------------                                       
* COLUMN SELECTION                                                              
*----------------------------------------                                       
GR022    TM    GRCCIN,GRCCDDS            DDS ONLY COLUMN?                       
         BZ    GR030                                                            
         TM    CUSTAT,CUSDDS             DDS TERMINAL?                          
         BZ    GR182                                                            
*                                                                               
GR030    ICM   R1,15,RGPARM+4            ADDRESS OF COLUMN SELECTOR             
         BZ    GR032                                                            
         CLI   0(R1),0                                                          
         BE    GR032                                                            
*                                      * NON-PRIMARY SELECTOR                   
         TM    GRCCIN,GRCCNPO            NON-PRIMARY ONLY COLUMN?               
         BO    GR050                     . YES, PROCESS                         
         TM    GRCCIN,GRCCPON            PRIMARY ONLY COLUMN?                   
         BO    GR182                     . YES, SKIP                            
         CLI   GRCCS,0                   PRIMARY COLUMN?                        
         BE    GR050                     . YES, PROCESS                         
         CLC   GRCCS,0(R1)               SPECIFIC COLUMN WANTED?                
         BE    GR050                     . YES, PROCESS                         
         B     GR182                                                            
*                                      * PRIMARY SELECTOR                       
GR032    TM    GRCCIN,GRCCPON            PRIMARY ONLY COLUMN?                   
         BO    GR050                     . YES, PROCESS                         
         TM    GRCCIN,GRCCNPO            NON-PRIMARY ONLY COLUMN?               
         BO    GR182                     . YES, SKIP                            
         CLI   GRCCS,0                   PRIMARY COLUMN?                        
         BNE   GR182                     . NO, SKIP                             
                                                                                
*----------------------------------------                                       
* CHECK FOR ROW CONTINUATION                                                    
*----------------------------------------                                       
GR050    TM    GRCCIN,GRCCRCC+GRCCVNM    RECURRING COLUMN?                      
         BNO   GR052                                                            
         TM    GRCDIN,GRCDROT            ROUTINE TO HANDLE CUSTOM DATA          
         BZ    GR052                                                            
         TM    GRCNIN,GRCNROT            MAKE SURE ALL BITS ARE ON              
         BZ    GR052                                                            
         OI    RFLAG,RFGRCCQ             . YES                                  
         GOTO1 VHEXOUT,APPARM,RGRCID#,APWORK,L'RGRCID#,=C'N'                    
         MVC   RGRCID,APWORK+1                                                  
*                                                                               
GR052    CLI   PCGRCN,0                  FIRST COLUMN?                          
         BE    GR060                                                            
         CLC   PCGRCN,RGRCID             CONTINUATION COLUMN?                   
         BNE   GR181                                                            
         MVI   PCGRCN,0                                                         
                                                                                
*-----------------------------------                                            
* COLUMN NAMES IF NEEDED                                                        
*-----------------------------------                                            
GR060    TM    PCGRIDS,PCGCOFQ           ALREADY HAVE COLUMNS?                  
         BO    GR070                                                            
         BRAS  RE,PGRCON                 PROCESS GRID COLUMN NAME               
         BNE   GR182                                                            
         NI    RFLAG,X'FF'-RFG1STQ       NO SPECIAL PROCESS FOR NAMES           
         B     GRTEXT                                                           
                                                                                
*-----------------------------------                                            
* COLUMN DATA                                                                   
*-----------------------------------                                            
GR070    L     R5,AIOAREA3               A(DATA AREA)                           
         TM    GRCDIN,GRCDIO3                                                   
         BO    GR072                                                            
         L     R5,AIOAREA2                                                      
         TM    GRCDIN,GRCDIO2                                                   
         BO    GR072                                                            
         L     R5,ATWA                   A(DATA AREA)                           
         TM    GRCDIN,GRCDTWA                                                   
         BO    GR072                                                            
         LA    R5,WORKD                                                         
         TM    GRCDIN,GRCDWS                                                    
         BO    GR072                                                            
         L     R5,AIOAREA1                                                      
*                                                                               
GR072    CLI   GRCELEC,0                 DATA IN AN ELEMENT ON RECORD?          
         BE    GR080                     . NO                                   
*                                                                               
         USING RECTABD,R1                                                       
         ICM   R1,15,ARECNTRY            GET A(RECTABLE ENTRY)                  
         LHI   RF,SAPGDATA-SAPGREC       CTFILE FIRST ELEMENT                   
         CLI   RECUSER,GENFILQ           GENFIL?                                
         BNE   *+8                                                              
         LHI   RF,GFIRST                 GENFILE FIRST ELEMENT                  
         AR    R5,RF                                                            
         DROP  R1                                                               
*                                                                               
GR074    CLI   0(R5),0                   END OF RECORD?                         
         BE    GRBLANK                   . YES                                  
         CLC   GRCELEC,0(R5)             FOUND ELEMENT?                         
         BNE   GR078                     . NO                                   
         OC    GRCELES,GRCELES           USING SUB-ELEMENT CODE?                
         BZ    GR080                     . NO                                   
         CLC   GRCELES,2(R5)             FOUND SUB-ELEMENT?                     
         BE    GR080                     . YES                                  
GR078    SR    RF,RF                     NEXT ELEMENT                           
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     GR074                                                            
*                                                                               
GR080    SR    R1,R1                                                            
         IC    R1,GRCDLEN                PICK UP LENGTH OF DATA                 
         SR    RF,RF                                                            
         ICM   RF,3,GRCDATA              DISPLACEMENT TO DATA                   
         TM    GRCDIN,GRCDEVL            DATA IN ELEM IS VARIABLE LEN?          
         BZ    GR082                                                            
         IC    R1,1(R5)                  LENGTH OF ELEMENT                      
         SR    R1,RF                     R1 = LENGTH OF DATA                    
*                                                                               
GR082    AR    R5,RF                     R5 = A(DATA FIELD)                     
*                                                                               
         LTR   R1,R1                     TEST DATA LENGTH FIELD                 
         BNP   GRBLANK                                                          
*                                                                               
         TM    GRCDIN,GRCDROT            ROUTINE TO HANDLE CUSTOM DATA          
         BZ    GR088                                                            
         L     RF,0(R5)                                                         
         GOTO1 (RF),APPARM,(C'D',DLCBFLX),GRIDCD,RGRCID#                        
         BE    GRTEXT                                                           
         NI    RFLAG,X'FF'-RFGRCCQ       <> MEANS NO MORE RECURRING             
         B     GR182                                                            
                                                                                
*------------------------------                                                 
* FORMAT OUTPUT                                                                 
*------------------------------                                                 
GR088    SR    RF,RF                                                            
         IC    RF,GRCTYPE                GRID COLUMN DATA TYPE                  
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     FGRTXT                    1 - GRCTTXT - TEXT                     
         B     FGRDAT                    2 - GRCTDAT - DATE                     
         B     FGRTXT                    3 - GRCTNUM - NUMBER                   
         B     FGRBOO                    4 - GRCTBOO - BOOLEAN VALUE            
*                                                                               
* FORMAT TEXT                                                                   
*                                                                               
FGRTXT   CLI   0(R5),0                   IF DATA IS ZERO, PROCESS BLANK         
         BE    GRBLANK                                                          
*                                                                               
         LA    RF,0(R1,R5)               POINT TO END OF DATA                   
FGRT10   BCTR  RF,0                                                             
         CLI   0(RF),C' '                SKIP OVER SPACES AND ZEROS             
         BH    *+12                                                             
         BCT   R1,FGRT10                 CALCULATE TRUE LENGTH                  
         B     GRTEXT                                                           
*                                                                               
         CHI   R1,L'DLCBFLX              FITS IN EXTENDED FIELD?                
         BNL   FGRT12                    . NO                                   
*                                                                               
         BCTR  R1,0                      RF=LENGTH OF DATA                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLX(0),0(R5)          MOVE INTO OUTPUT FIELD                 
         B     GRTEXT                                                           
*                                                                               
FGRT12   CHI   R1,255                    DATA TOO LONG FOR DLFLD                
         BNH   *+8                       MUST HANDLE MYSELF                     
         LHI   R1,255                    MAX IS ONE BYTE                        
*                                                                               
         LH    RE,DLCBNUMC               CHECK IF LONG TEXT WILL FIT            
         LA    RE,3(RE,R1)               ON SCREEN                              
         LH    RF,DLCXMAXL                                                      
         CR    RE,RF                                                            
         BL    FGRT20                    . YES, IT WILL                         
*                                                                               
         BRAS  RE,OUTGRITS               . NO, IT WON'T, OUTPUT DATA            
         B     GRERX                     CURRENT FIELD DIDN'T FIT               
*                                                                               
FGRT20   LHI   RF,GSB-SYSWORK            MOVE TEXT TO GRID SCREEN BLOCK         
         A     RF,ASYSWORK                                                      
         AH    RF,DLCBNUMC                                                      
         MVI   0(RF),C'"'                WITH QUOTES AROUND IT                  
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(R5)                                                    
         LA    RF,2(R1,RF)                                                      
         MVI   0(RF),C'"'                                                       
*                                                                               
         LA    R1,4(R1)                                                         
         AH    R1,DLCBNUMC                                                      
         STH   R1,DLCBNUMC               BUMP NUM OF CHARS THIS LINE            
         LH    R1,DLCBNUMF                                                      
         LA    R1,1(R1)                                                         
         STH   R1,DLCBNUMF               BUMP NUM OF FIELDS THIS LINE           
         B     GR182                     CONTINUE PROCESSING COLUMNS            
*                                                                               
* FORMAT DATES                                                                  
*                                                                               
FGRDAT   OC    0(2,R5),0(R5)                                                    
         BZ    GRBLANK                                                          
         TM    GRCGIN,GRCGTXT           DATE IN TEXT FORM?                      
         BO    FGRTXT                    . YES, TREAT AS TEXT                   
         SR    RF,RF                                                            
         IC    RF,GRCDDTY                DATCON INPUT FORMAT NUMBER             
         MVC   RWORK(10),0(R5)                                                  
         TM    GRCGIN,GRCGCOM            COMPLEMENTED?                          
         BZ    FGRDA10                                                          
         XC    RWORK(10),=10X'FF'                                               
FGRDA10  GOTO1 VDATCON,APPARM,((RF),RWORK),(21,DLCBFLX)                         
         B     GRTEXT                                                           
*                                                                               
* FORMAT BOOLEAN (Y/N)                                                          
*                                                                               
FGRBOO   IC    RF,GRCDFLG                 BIT(S) WE NEED TO TEST                
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(R5),0                                                          
         BZ    FGRBO10                                                          
*                                                                               
         TM    GRCGIN,GRCGYON                                                   
         BZ    *+14                                                             
         MVC   DLCBFLX(L'CT@YES),CT@YES                                         
         B     GRTEXT                                                           
         MVC   DLCBFLX(L'GRCNAME),GRCNAME                                       
         L     RF,ACOM                                                          
         L     RF,CDICTATE-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,C'SL  ',DLCBFLX                                      
         B     GRTEXT                                                           
*                                                                               
FGRBO10  TM    GRCGIN,GRCGYON                                                   
         BZ    *+14                                                             
         MVC   DLCBFLX(L'CT@NO),CT@NO                                           
         B     GRTEXT                                                           
         B     GRBLANK                                                          
                                                                                
*-----------------------------------                                            
* DISPLAY DATA                                                                  
*-----------------------------------                                            
GRBLANK  LHI   R1,DWNBLANK               DISPLAY A BLANK                        
         TM    GRCCIN,GRCCOMB            OMIT COLUMN IF BLANK?                  
         BO    GR182                                                            
         B     GR180                                                            
*                                                                               
GRNUM    LHI   R1,DWNNUM                 DISPLAY A NUMBER                       
         B     GR180                                                            
*                                                                               
GRTEXT   TM    GRCCIN,GRCCOM0            OMIT COLUMN IF ZERO?                   
         BZ    *+12                                                             
         CLI   DLCBFLX,0                                                        
         BE    GR182                                                            
         TM    GRCCIN,GRCCOMB            OMIT COLUMN IF BLANK?                  
         BZ    *+12                                                             
         CLI   DLCBFLX,C' '                                                     
         BE    GR182                                                            
         CLI   DLCBFLX,C' '                                                     
         BNH   GRBLANK                                                          
*                                                                               
         LHI   R1,DWNTEXT                TEXT                                   
         TM    RFLAG,RFG1STQ             FIRST COLUMN                           
         BZ    GR180                                                            
         MVC   RIO(L'DLCBFLX),DLCBFLX    SHIFT DLCBFLX                          
         MVI   DLCBFLX,C' '                                                     
         MVC   DLCBFLX+1(L'DLCBFLX-1),RIO                                       
*                                                                               
GR180    GOTOR DWNL,(R1)                                                        
         BNE   GRERX                                                            
GR181    NI    RFLAG,X'FF'-RFG1STQ       NO LONGER FIRST COLUMN                 
*                                                                               
GR182    TM    RFLAG,RFGRCCQ             RECURRING COLUMN?                      
         BZ    GR183                                                            
         SR    R1,R1                                                            
         ICM   R1,3,RGRCID#              INCREASE RECURRING COL COUNT           
         AHI   R1,1                                                             
         CHI   R1,2000                                                          
         BNH   *+6                                                              
         DC    H'0'                      SELF IMPOSED 2000 COLUMN LIMIT         
         STCM  R1,3,RGRCID#                                                     
         B     GR020                                                            
*                                                                               
GR183    AHI   R4,GRCLNQ                 NEXT COLUMN                            
         B     GR020                                                            
                                                                                
*----------------------------------------                                       
* END OF ROW AND EXIT                                                           
*----------------------------------------                                       
GR200    GOTOR DWNL,DWNEOL                                                      
         BNE   GRERX                                                            
*                                                                               
         TM    PCGRIDS,PCGCOFQ           COLUMN NAMES DISPLAYED?                
         BO    GROKX                     . YES                                  
         OI    PCGRIDS,PCGCOFQ           SET LOCAL BIT                          
         OI    RFLAG,RFG1STQ             SPECIAL PROCESS FOR 1ST COL            
         B     GR010                                                            
                                                                                
*----------------------------------------                                       
* END OF REPORT / GRIDS                                                         
*----------------------------------------                                       
GR290    GOTOR DWNL,DWNEOR               END OF REPORT (FIRST POSITION)         
         BNE   GRERX                                                            
         B     GR310                                                            
*                                                                               
GR300    GOTOR DWNL,DWNEND               END OF REPORT (WITHIN A LINE)          
         BNE   GRERX                                                            
GR310    OI    PCGRIDS,PCGFINQ                                                  
         NI    PCGRIDS,X'FF'-PCGBEGQ                                            
*                                                                               
GROKX    MVI   PCGRCN,0                  START FROM FIRST COLUMN                
         CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
GRERX    MVC   PCGRCN,RGRCID             FIELD DID NOT FIT, KEEP TRACK          
         TM    PCGRIDS,PCGCOFQ                                                  
         BO    *+8                                                              
         OI    PCGRIDS,PCGCOPQ                                                  
         LTR   RB,RB                                                            
         J     EXIT                                                             
         DROP  R2,R3,R4                                                         
         LTORG                                                                  
                                                                                
*----------------------------------------                                       
* GRIDS ROUTINE RFLAG EQUATES                                                   
*----------------------------------------                                       
RFG1STQ  EQU   X'80'                     . SPECIAL PROCESS FOR 1ST COL          
RFGRCCQ  EQU   X'08'                     . RECURRING COLUMN                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS GRIDS DESCRIPTION LINE DATA                                           
***********************************************************************         
         USING DLCBD,R2                                                         
         USING GRIDLD,R4                                                        
PGRDLDA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ATWA                                                          
         AHI   RF,PCDRIVEN-TWAD                                                 
         USING PCDRIVEN,RF                                                      
         TM    PCGRIDS,PCGCOFQ+PCGCOPQ   BUILD THESE LINES, ONLY IF NO          
         BNZ   PGRDX                     COLUMNS BUILT YET                      
         DROP  RF                                                               
*                                                                               
PGRD008  LA    R3,DLCBFLX                R3=A(DOWNLOAD FIELD)                   
         BAS   RE,PGRDSLEN               DLCBLEN=MAX LEN OF DATA LINE           
*                                                                               
PGRD010  CLI   GRLID,GRLDQ               ANY DESCRIPTION LINE DATA?             
         BNE   PGRD050                   . NO, PRINT WHAT WE HAVE               
*----------------------------------------                                       
* DESCRIPTION NAME                                                              
*----------------------------------------                                       
         XC    RIO(L'DLCBFLX),RIO                                               
         LA    RE,RIO                                                           
*                                                                               
         L     RF,ATWA                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,GRLNADI                                                     
         AR    RF,R0                     RF=A(TWA FIELD HEADER)                 
         SR    R1,R1                                                            
         ICM   R1,1,GRLNALN              R1=LENGTH OF FIELD DATA                
         BZ    PGRD040                                                          
         CLI   0(RF),C' '                                                       
         BNH   PGRD040                                                          
*                                                                               
         SHI   R1,1                                                             
         BM    PGRD040                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
         LA    R1,0(R1,RE)                                                      
PGRD020  CLI   0(R1),C' '                                                       
         BH    PGRD022                                                          
         MVI   0(R1),C' '                                                       
         AHI   R1,-1                                                            
         CR    R1,RE                                                            
         BH    PGRD020                                                          
PGRD022  MVI   1(R1),C'='                                                       
         LA    R1,2(R1)                                                         
*                                                                               
         SR    R1,RE                     R1=LENGTH OF NAME                      
         AR    RE,R1                     RE=A(NEXT OUTPUT LOCATION)             
         LR    R8,R1                                                            
*                                                                               
*----------------------------------------                                       
* DESCRIPTION DATA                                                              
*----------------------------------------                                       
         L     RF,ATWA                                                          
         ICM   R0,3,GRLDADI                                                     
         AR    RF,R0                     RF=A(TWA FIELD HEADER)                 
         SR    R1,R1                                                            
         ICM   R1,1,GRLDALN              R1=LENGTH OF FIELD DATA                
         BZ    PGRD040                                                          
         CLI   0(RF),C' '                                                       
         BNH   PGRD040                                                          
*                                                                               
         SHI   R1,1                                                             
         BM    PGRD040                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
         LA    R1,0(R1,RE)                                                      
PGRD026  CLI   0(R1),C' '                                                       
         BH    PGRD028                                                          
         MVI   0(R1),C' '                                                       
         AHI   R1,-1                                                            
         CR    R1,RE                                                            
         BH    PGRD026                                                          
*                                                                               
PGRD028  DS    0H                                                               
         LA    R1,1(R1)                                                         
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         MVI   0(R1),C' '                                                       
         SR    R1,RE                     R1=LENGTH OF NAME                      
         AR    R1,R8                     RE=A(NEXT OUTPUT LOCATION)             
         AHI   R1,1                                                             
*----------------------------------------                                       
* MOVE TO OUTPUT LINE                                                           
*----------------------------------------                                       
         SR    R0,R0                                                            
         IC    R0,DLCBLEN                                                       
         CR    R1,R0                                                            
         BH    PGRD050                                                          
         SR    R0,R1                                                            
         STC   R0,DLCBLEN                                                       
*                                                                               
         AHI   R1,-1                                                            
         BM    PGRD040                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RIO                                                      
         LA    R3,1(R1,R3)                                                      
*                                                                               
PGRD040  LA    R4,GRLLNQ(R4)                                                    
         B     PGRD010                                                          
*                                                                               
PGRD050  LA    R1,DLCBFLX                R3=A(DOWNLOAD FIELD)                   
         CR    R3,R1                     WAS THERE ANY DATA?                    
         BNH   PGRDX                     . NO                                   
         AHI   R3,-2                                                            
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
*                                                                               
         BAS   RE,PGRDSLEN               DLCBLEN=MAX LEN OF DATA LINE           
         GOTOR DWNL,DWNTEXT                                                     
         GOTOR DWNL,DWNEOL                                                      
*                                                                               
         LA    RF,FVOMSG+(GRMCODA1-GRMSG1)                                      
         PACK  RFULL,0(L'GRMCODA1,RF)                                           
         AP    RFULL,=P'1'                                                      
         EDIT  (P4,RFULL),(L'GRMCODA1,(RF)),WRK=RWORK,DUB=RDUB                  
*                                                                               
         CLI   GRLID,GRLDQ                                                      
         BE    PGRD008                                                          
*                                                                               
PGRDX    XC    DLCBLEN,DLCBLEN                                                  
         CR    RB,RB                                                            
         J     EXIT                                                             
                                                                                
*----------------------------------------                                       
* SET DLCBLEN                                                                   
*----------------------------------------                                       
         USING GRIDFD,R1                                                        
PGRDSLEN L     R1,RGPARM                                                        
         SR    R0,R0                                                            
         IC    R0,GRFLILEN               LENGTH OF SCREEN LINE                  
         AHI   R0,-5                                                            
         STC   R0,DLCBLEN                DLCBLEN=MAX LEN OF DATA LINE           
         BR    RE                                                               
         DROP  R1,R2                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS GRIDS COLUMN NAME                                                     
***********************************************************************         
         USING DLCBD,R2                                                         
         USING GRIDCD,R4                                                        
PGRCON   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GRCCIN,GRCCVNM            VARIABLE COLUMN NAME?                  
         BZ    PGRC012                   . NO                                   
         L     RF,AIOAREA3                                                      
         TM    GRCNIN,GRCNIO3                                                   
         BO    PGRC006                                                          
         L     RF,AIOAREA2                                                      
         TM    GRCNIN,GRCNIO2                                                   
         BO    PGRC006                                                          
         L     RF,ATWA                                                          
         TM    GRCNIN,GRCNTWA                                                   
         BO    PGRC006                                                          
         LA    RF,WORKD                                                         
         TM    GRCNIN,GRCNWS                                                    
         BO    PGRC006                                                          
         L     RF,AIOAREA1                                                      
PGRC006  SR    R0,R0                                                            
         ICM   R0,3,GRCNADI                                                     
         AR    RF,R0                                                            
*                                                                               
         TM    GRCNIN,GRCNROT            ROUTINE TO HANDLE CUSTOM DATA          
         BZ    PGRC008                                                          
         L     RF,0(RF)                                                         
         GOTO1 (RF),APPARM,(C'N',DLCBFLX),GRIDCD,RGRCID#                        
         BE    PGRC010                                                          
         NI    RFLAG,X'FF'-RFGRCCQ       <> MEANS NO MORE RECURRING             
         B     PGRCERX                                                          
*                                                                               
PGRC008  SR    R1,R1                                                            
         IC    R1,GRCNALN                                                       
         SHI   R1,1                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
*                                                                               
PGRC010  CLI   DLCBFLX,C' '              IF NO NAME THEN DON'T BUILD            
         BNH   PGRCOKX                                                          
         B     PGRC018                                                          
*                                                                               
PGRC012  MVC   DLCBFLX(L'GRCNAME),GRCNAME                                       
         L     RF,ACOM                                                          
         L     RF,CDICTATE-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,C'SL  ',DLCBFLX                                      
*                                                                               
PGRC018  CLI   CULANG,LANGGER            GERMAN                                 
         BE    PGRC030                   DO NOT SPLIT HEADLINES                 
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,GRCSPOS              SPLIT POSITION                         
         BNZ   *+8                                                              
         LHI   R1,10                     USE 10 IF NONE GIVEN                   
         LA    R3,DLCBFLX                                                       
         AR    R1,R3                                                            
PGRC020  CR    R1,R3                                                            
         BNH   PGRC030                                                          
         CLI   0(R1),C' '                FIND BLANK IN HEADING                  
         BE    *+12                                                             
         SHI   R1,1                                                             
         B     PGRC020                                                          
*                                                                               
         LA    R1,1(R1)                                                         
         AHI   R3,L'DLCBFLX-1            SHIFT DATA OVER FOR SPLIT CHAR         
         SR    R3,R1                                                            
         SHI   R3,1                                                             
         BNP   PGRC030                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   RIO(0),0(R1)                                                     
         SHI   R3,1                                                             
         BNP   PGRC030                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),RIO                                                      
         MVI   0(R1),GRCSEPQ             AND REPLACE WITH SPLIT CHAR            
*                                                                               
PGRC030  LA    R3,DLCBFLX+L'DLCBFLX-1                                           
PGRC032  CLI   0(R3),GRCSEPQ                                                    
         BE    *+12                                                             
         CLI   0(R3),C' '                                                       
         BH    PGRC040                                                          
         MVI   0(R3),C' '                                                       
         BCTR  R3,0                                                             
         B     PGRC032                                                          
*                                                                               
PGRC040  MVI   1(R3),C'*'                                                       
         LA    R3,2(R3)                                                         
         MVC   0(L'RGRCID,R3),RGRCID     GRIDS COLUMN ID                        
         LA    R3,L'RGRCID(R3)                                                  
         CLI   RGRCID+2,C' '             NO SPACE IN 2 BYTE COLUMN ID           
         BH    *+8                                                              
         AHI   R3,-1                                                            
         MVI   0(R3),C'*'                                                       
*                                                                               
         CLI   GRCTYPE,GRCTDAT           DATE                                   
         BNE   PGRC050                                                          
         MVI   1(R3),C'D'                                                       
         LA    R3,1(R3)                                                         
         B     PGRC060                                                          
*                                                                               
PGRC050  CLI   GRCTYPE,GRCTNUM           NUMBER                                 
         BNE   PGRC060                                                          
         MVI   1(R3),C'N'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
PGRC060  TM    GRCFORM,GRCFHID           HIDE COLUMN                            
         BZ    PGRC070                                                          
         MVI   1(R3),C'H'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
PGRC070  TM    GRCFORM,GRCFRGT           RIGHT JUSTIFY                          
         BZ    PGRC080                                                          
         MVI   1(R3),C'R'                                                       
         LA    R3,1(R3)                                                         
         B     PGRC090                                                          
*                                                                               
PGRC080  TM    GRCFORM,GRCFCEN           CENTER                                 
         BZ    PGRC090                                                          
         MVI   1(R3),C'C'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
PGRC090  TM    GRCFORM,GRCFSIZ           SIZED (2")                             
         BZ    PGRC100                                                          
         MVI   1(R3),C'S'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
PGRC100  CLI   0(R3),C'*'                IF NO ATTRIBUTES                       
         BNE   *+8                                                              
         MVI   0(R3),C' '                THEN REMOVE ASTERISK                   
*                                                                               
PGRCOKX  CR    RB,RB                                                            
         J     EXIT                                                             
PGRCERX  LTR   RB,RB                                                            
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* GRIDS OUTPUT MODULE                                                           
*   ON ENTRY R1=OUTPUT MODE                                                     
***********************************************************************         
         USING DLCBD,R2                                                         
         USING GRIDFD,R3                                                        
DWNL     NTR1  BASE=*,LABEL=*                                                   
         L     R3,RGPARM                                                        
         STC   R1,RBYTE                                                         
*                                                                               
         LHI   R2,DLCB-SYSWORK           DOWN-LOAD CONTROL BLOCK                
         A     R2,ASYSWORK                                                      
*                                                                               
         MVC   RHALF,DLCBNUMC                                                   
*                                                                               
         CHI   R1,DWNINIT                INITIALIZE                             
         BE    DWNL010                                                          
         CHI   R1,DWNCLEAR               CLEAR GRIDS OUTPUT SCREEN              
         BE    DWNL015                                                          
         CHI   R1,DWNTEXT                OUTPUT TEXT                            
         BE    DWNL030                                                          
         CHI   R1,DWNBLANK               OUTPUT A BLANK                         
         BE    DWNL040                                                          
         CHI   R1,DWNNUM                 OUTPUT NUMBER                          
         BE    DWNL045                                                          
         CHI   R1,DWNEOL                 END OF LINE                            
         BE    DWNL050                                                          
         CHI   R1,DWNEND                 END OF OUTPUT                          
         BE    DWNL060                                                          
         CHI   R1,DWNEOR                 END OF OUTPUT (ONLY)                   
         BE    DWNL065                                                          
         DC    H'0'                                                             
                                                                                
*----------------------------------------                                       
* INITIALIZATION                                                                
*----------------------------------------                                       
DWNL010  XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT          DOWN LOAD ACTION IS START              
*                                                                               
         LHI   R0,GSB-SYSWORK            GRIDS SCREEN BLOCK                     
         A     R0,ASYSWORK               FIRST OUTPUT LINE                      
         ST    R0,DLCBAPL                                                       
*                                                                               
         LHI   R1,L'GSB                  CLEAR THE GSB                          
         SR    RE,RE                                                            
         LHI   RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,DWNHOOK                HOOK                                   
         ST    RF,DLCBAPR                                                       
*                                                                               
         LH    RE,GRFLINEF               FIRST GRIDS SCREEN LINE                
         LH    RF,GRFLINEL               LAST GRIDS SCREEN LINE                 
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         SR    R0,R0                                                            
         IC    R0,GRFLIDIS               DISPL BETWEEN SCREEN LINES             
         DR    RE,R0                                                            
         SR    RE,RE                                                            
         IC    R0,GRFLILEN               LENGTH OF SCREEN LINE                  
         MR    RE,R0                                                            
         AR    RF,R0                                                            
         STH   RF,DLCXMAXL                                                      
*                                                                               
         MVI   DLCXDELC,C' '             DELIMITER                              
         MVI   DLCXEOTC,C'"'             TEXT DELIMITER                         
         MVI   DLCXEOLC,C':'             END-OF-LINE                            
         MVI   DLCXEORC,C':'             END-OF-REPORT                          
         L     RF,ACOM                                                          
         L     RF,CDLFLD-COMFACSD(RF)                                           
         GOTO1 (RF),(R2)                                                        
         BNE   DWNLHX                                                           
         MVI   DLCBFLD,C' '              MUST CLEAR FIRST TIME IN               
         MVC   DLCBFLD+1(L'DLCBFLD-1),DLCBFLD                                   
         XC    DLCBLEN,DLCBLEN                                                  
         MVI   DLCBFLX,C' '                                                     
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         LHI   RF,PCDRIVEN-TWAD                                                 
         A     RF,ATWA                                                          
         USING PCDRIVEN,RF                                                      
         TM    PCGRIDS,PCGBEGQ           GRIDS ALREADY BEGUN?                   
         BO    DWNL015                   . YES                                  
         OI    PCGRIDS,PCGBEGQ           IT HAS NOW                             
         NI    PCGRIDS,X'FF'-PCGCOFQ-PCGCOPQ-PCGFINQ                            
         MVI   PCGRCN,0                  START WITH FIRST COLUMN                
         DROP  RF                                                               
                                                                                
*-----------------------------------                                            
*  CLEAR AND TRANSMIT SCREEN                                                    
*-----------------------------------                                            
DWNL015  L     R0,ATWA                                                          
         LR    R4,R0                                                            
         AH    R4,GRFLINEF               FIRST LINE                             
         AH    R0,GRFLINEL               LAST LINE                              
         SHI   R4,L'FVIHDR               BUMP BACK TO HEADERS                   
         SHI   R0,L'FVIHDR                                                      
*                                                                               
         SR    RE,RE                                                            
         IC    RE,GRFLIDIS               DISPLACEMENT BETWEEN LINES             
         SR    RF,RF                                                            
         IC    RF,GRFLILEN               LINE LENGTH                            
         SHI   RF,1                                                             
*                                                                               
FH       USING FVIHDR,R4                                                        
DWNL16   EX    RF,*+8                    CLEAR THE SCREEN                       
         B     *+10                                                             
         MVC   FH.FVIFLD(0),DWSPACES                                            
         OI    FH.FVOIND,FVOXMT                                                 
*                                                                               
         CR    R4,R0                     REACHED THE LAST LINE?                 
         BNL   DWNL18                    . YES                                  
         AR    R4,RE                     BUMP TO NEX LINE                       
         B     DWNL16                                                           
         DROP  FH                                                               
                                                                                
*-----------------------------------                                            
* GRIDS COMMANDS IN MESSAGE FIELD                                               
*-----------------------------------                                            
DWNL18   CLI   RBYTE,DWNCLEAR            IF CLEAR THEN EXIT                     
         BE    DWNLX                                                            
*                                                                               
         SR    RF,RF                     CALCULATE # OF FIELDS                  
         SR    R5,R5                                                            
         IC    R5,GRFDORA                                                       
         L     R1,ATWA                                                          
         AH    R1,GRFLINEF               FIRST GRIDS LINE                       
         LHI   R4,TWASCR-TWAD                                                   
         A     R4,ATWA                                                          
*                                                                               
FH       USING FVIHDR,R4                                                        
DWNL20   CR    R4,R1                                                            
         BH    DWNL22                                                           
         IC    RF,FH.FVTLEN                                                     
         AR    R4,RF                                                            
         AHI   R5,1                                                             
         B     DWNL20                                                           
         DROP  FH                                                               
*                                                                               
DWNL22   MVC   FVOMSG,DWSPACES           PROCESS MESSAGE FIELD                  
*                                                                               
         LHI   RF,PCDRIVEN-TWAD                                                 
         A     RF,ATWA                                                          
         USING PCDRIVEN,RF                                                      
         TM    PCGRIDS,PCGCOPQ           COLUMNS STARTED?                       
         BO    *+12                      . YES                                  
         TM    PCGRIDS,PCGCOFQ           ALREADY HAVE COLUMNS?                  
         BZ    DWNL26                    . NO, THEN NEED 1ST GRID MESS          
         DROP  RF                                                               
*                                                                               
         MVC   FVOMSG(GRMLNQ),GRMSG                                             
         LA    R4,FVOMSG+(GRMCODA-GRMSG)                                        
         EDIT  (R5),(L'GRMCODA,(R4)),WRK=RWORK,DUB=RDUB                         
         B     DWNLX                                                            
*                                                                               
DWNL26   MVC   FVOMSG(GRM1LNQ),GRMSG1                                           
         LA    R4,FVOMSG+(GRMDOR-GRMSG1)                                        
         EDIT  (R5),(L'GRMDOR,(R4)),WRK=RWORK,DUB=RDUB                          
*        SR    RF,RF                                                            
*        IC    RF,GRFNODL                                                       
*        AR    R5,RF                                                            
         LA    R4,FVOMSG+(GRMCODA1-GRMSG1)                                      
         EDIT  (R5),(L'GRMCODA1,(R4)),WRK=RWORK,DUB=RDUB                        
         CLI   GRFFIX,0                                                         
         BE    *+10                                                             
         MVC   FVOMSG+(GRMFIX-GRMSG1)(L'GRFFIX),GRFFIX                          
*                                                                               
         MVC   FVOMSG+(GRMFID-GRMSG1)(L'GRFID),GRFID                            
         OC    RGPARM+4(4),RGPARM+4      RGPARM+4 = A(GRIDS COLUMN SEL)         
         BZ    DWNLX                                                            
         L     R1,RGPARM+4                                                      
         CLI   0(R1),0                                                          
         BE    DWNLX                                                            
         MVC   FVOMSG+(GRMFCS-GRMSG1)(L'GRFCS),0(R1)                            
         B     DWNLX                                                            
                                                                                
*-----------------------------------                                            
* DOWNLOAD A RECORD - TEXT                                                      
*-----------------------------------                                            
DWNL030  OI    DLCBFLG1,DLCBFXFL         USE EXTENDED FIELD                     
         MVI   DLCBACT,DLCBPUT           ACTION IS PUT                          
         MVI   DLCBTYP,DLCBTXT           TYPE IS TEXT                           
         B     DWNL070                   DOWN-LOAD FIELD                        
                                                                                
*-----------------------------------                                            
* DOWNLOAD A RECORD - BLANK (TEXT)                                              
*-----------------------------------                                            
DWNL040  MVI   DLCBACT,DLCBPUT           ACTION IS PUT                          
         MVI   DLCBTYP,DLCBTXT           TYPE IS TEXT                           
         MVI   DLCBLEN,1                                                        
         MVI   DLCBFLD,C' '                                                     
         B     DWNL070                   DOWN-LOAD FIELD                        
                                                                                
*-----------------------------------                                            
* DOWNLOAD A RECORD - NUMBER                                                    
*-----------------------------------                                            
DWNL045  MVI   DLCBACT,DLCBPUT           ACTION IS PUT                          
         MVI   DLCBTYP,DLCBNUM           TYPE IS NUMBER                         
         B     DWNL070                   DOWN-LOAD FIELD                        
                                                                                
*-----------------------------------                                            
* END OF ROW                                                                    
*-----------------------------------                                            
DWNL050  MVI   DLCBACT,DLCBPUT           ACTION IS PUT                          
         MVI   DLCBTYP,DLCBNUM           NUMBER (SO NO DELIMITERS)              
         MVI   DLCBFLD,C';'                                                     
         B     DWNL070                                                          
                                                                                
*-----------------------------------                                            
* END OF OUTPUT (WITHIN A LINE)                                                 
*-----------------------------------                                            
DWNL060  MVI   DLCBACT,DLCBEOL           END OF RECORD (USE EOL)                
         B     DWNL070                                                          
                                                                                
*-----------------------------------                                            
* END OF OUTPUT (ONLY)                                                          
*-----------------------------------                                            
DWNL065  MVI   DLCBACT,DLCBEOR           END OF RECORD                          
                                                                                
*-----------------------------------                                            
* OUTPUT DOWNLOAD DATA                                                          
*-----------------------------------                                            
DWNL070  L     RF,ACOM                                                          
         L     RF,CDLFLD-COMFACSD(RF)                                           
         GOTO1 (RF),(R2)                                                        
         NI    DLCBFLG1,X'FF'-DLCBFXFL                                          
*                                                                               
         TM    DLCBRETC,DLCBRCPR                                                
         BO    DWNLHX                                                           
*                                                                               
DWNLX    CR    RB,RB                                                            
         J     EXIT                                                             
DWNLHX   LTR   RB,RB                                                            
         J     EXIT                                                             
                                                                                
*----------------------------------------                                       
* DOWNLOAD HOOK                                                                 
*----------------------------------------                                       
DWNHOOK  ST    RE,RFULL                                                         
*                                                                               
DWNHO10  LHI   R1,GSB-SYSWORK                                                   
         A     R1,ASYSWORK                                                      
         LR    RF,R1                                                            
         AH    RF,DLCXMAXL                                                      
         SHI   RF,1                      LAST BYTE IN GSB                       
*                                                                               
         LR    RE,R1                                                            
         AH    RE,RHALF                                                         
         AHI   RE,1                      BYTE AFTER DATA THAT FITS              
*                                                                               
DWNHO20  CR    RE,RF                     CLEAR GSB AFTER DATA                   
         JNL   DWNHO30                                                          
         MVI   0(RE),C' '                                                       
         LA    RE,1(RE)                                                         
         J     DWNHO20                                                          
*                                                                               
DWNHO30  L     R0,ATWA                   OUTPUT LINES TO SCREEN                 
         LR    R4,R0                                                            
         AH    R4,GRFLINEF               FIRST LINE                             
         AH    R0,GRFLINEL               LAST LINE                              
         SHI   R4,L'FVIHDR               BUMP BACK TO HEADERS                   
         SHI   R0,L'FVIHDR                                                      
*                                                                               
         SR    RE,RE                                                            
         IC    RE,GRFLIDIS               DISPLACEMENT BETWEEN LINES             
         SR    RF,RF                                                            
         IC    RF,GRFLILEN               LINE LENGTH                            
         SHI   RF,1                                                             
*                                                                               
FH       USING FVIHDR,R4                                                        
DWNHO40  OI    FVOIND,FVOXMT                                                    
         EX    RF,*+8                    OUTPUT TO SCREEN                       
         J     *+10                                                             
         MVC   FH.FVIFLD(0),0(R1)                                               
         LA    R1,1(RF,R1)                                                      
*                                                                               
         CR    R4,R0                     REACHED THE LAST LINE?                 
         JNL   DWNHOOKX                  . YES                                  
         AR    R4,RE                     BUMP TO NEX LINE                       
         J     DWNHO40                                                          
         DROP  FH                                                               
*                                                                               
DWNHOOKX L     RE,RFULL                                                         
         BR    RE                                                               
         DROP  R2,R3                                                            
         LTORG                                                                  
*                                                                               
DWSPACES DC    CL78' '                                                          
*                                                                               
GRMSG1   DC    C'GRID NEXTL '            GRID MESSAGE FOR FIRST SCREEN          
         DC    C'DOR '                                                          
GRMDOR   DC    C'17',C' '                START OF GRIDS INFORMATION             
         DC    C'DATA '                                                         
GRMCODA1 DC    C'20',C' '                START OF GRIDS COLUMN DATA             
         DC    C'EID '                                                          
GRMFID   DC    C'08'                     GRIDS FORMAT IDENTIFIER                
GRMFCS   DC    C'0',C' '                 FORMAT COLUMN SELECTOR                 
         DC    C'FC',C' '                                                       
GRMFIX   DC    C'1',C' '                 NUMBER OF FIXED COLUMNS                
         DC    C'WR ',AL1(GRCSEPQ)                                              
GRM1LNQ  EQU   *-GRMSG1                                                         
*                                                                               
GRMSG    DC    C'GRID NEXTL '                                                   
         DC    C'DATA '                                                         
GRMCODA  DC    C'17',C' '                                                       
         DC    C'WR ',AL1(GRCSEPQ)                                              
GRMLNQ   EQU   *-GRMSG                                                          
*                                                                               
GRMSGPR  DC    CL60'GRID NEXTL PR'                                              
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT GRIDS DATA TO SCREEN                                                   
***********************************************************************         
         USING DLCBD,R2                                                         
         USING GRIDFD,R3                                                        
OUTGRITS NTR1  BASE=*,LABEL=*                                                   
         L     R3,RGPARM                                                        
         LHI   R2,DLCB-SYSWORK           DOWN-LOAD CONTROL BLOCK                
         A     R2,ASYSWORK                                                      
         LHI   R1,GSB-SYSWORK                                                   
         A     R1,ASYSWORK                                                      
*                                                                               
         L     R0,ATWA                   OUTPUT LINES TO SCREEN                 
         LR    R4,R0                                                            
         AH    R4,GRFLINEF               FIRST LINE                             
         AH    R0,GRFLINEL               LAST LINE                              
         SHI   R4,L'FVIHDR               BUMP BACK TO HEADERS                   
         SHI   R0,L'FVIHDR                                                      
*                                                                               
         SR    RE,RE                                                            
         IC    RE,GRFLIDIS               DISPLACEMENT BETWEEN LINES             
         SR    RF,RF                                                            
         IC    RF,GRFLILEN               LINE LENGTH                            
         SHI   RF,1                                                             
*                                                                               
FH       USING FVIHDR,R4                                                        
OUTG020  OI    FVOIND,FVOXMT                                                    
         EX    RF,*+8                    OUTPUT TO SCREEN                       
         B     *+10                                                             
         MVC   FH.FVIFLD(0),0(R1)                                               
         LA    R1,1(RF,R1)                                                      
*                                                                               
         CR    R4,R0                     REACHED THE LAST LINE?                 
         BNL   OUTGX                     . YES                                  
         AR    R4,RE                     BUMP TO NEX LINE                       
         B     OUTG020                                                          
         DROP  FH                                                               
*                                                                               
OUTGX    J     EXIT                                                             
         DROP  R2,R3                                                            
         LTORG                                                                  
                                                                                
***********************************************************************         
* SET GENFIL ACTIVITY ELEMENT                                                   
***********************************************************************         
SETGACT  NTR1  BASE=*,LABEL=*                                                   
         ST    R1,SCFULL           SAVE A(RECORD)                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'03',SCDUB)                           
         L     R1,SCFULL           RESTORE A(RECORD)                            
*                                                                               
         LA    R2,ACTELG           GENFIL ACTIVITY ELEMENT                      
         USING GACTELD,R2                                                       
         XC    ACTELG,ACTELG                                                    
         XC    APELEM,APELEM                                                    
         MVI   APELEM,GACTELQ      X'FE' ACTIVITY ELEMENT                       
         GOTO1 AGETELS                                                          
         ICM   R1,15,APPARM                                                     
         BZ    SETGA10                                                          
         MVC   ACTELG,0(R1)        SAVE ELEMENT IN WORKING STORAGE              
         B     SETGA12                                                          
*                                                                               
SETGA10  MVI   GACTEL,GACTELQ      ELEMENT CODE                                 
         MVI   GACTLN,GACTLNQ      ELEMENT LENGTH                               
         MVC   GACTADT,SCDUB       DATE RECORD ADDED (BINARY)                   
         MVC   GACTAAG,CUAALF      ADDED AGENCY CODE                            
         MVC   GACTAPW,CUPASS      ADDED PASSWORD NUMBER                        
*                                                                               
SETGA12  MVC   GACTCDT,SCDUB       DATE RECORD LAST CHANGED (BINARY)            
         MVC   GACTCAG,CUAALF      CHANGED AGENCY CODE                          
         MVC   GACTCPW,CUPASS      CHANGED PASSWORD NUMBER                      
         TIME  BIN                 TIME IS ON MY SIDE                           
         STCM  R0,7,GACTTIM        ACTIVITY TIME                                
         L     R1,SCFULL           RESTORE A(RECORD)                            
*                                                                               
         XC    APELEM,APELEM                                                    
         MVC   APELEM(1),ACTELG    SET ACTIVITY ELEMENT CODE                    
         GOTO1 ADELELS             DELETE AND RE-ADD ELEMENT                    
         MVC   APELEM(L'ACTELG),ACTELG                                          
         GOTO1 AADDELS                                                          
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
                                                                                
*&&US                                                                           
***********************************************************************         
* US MEDIA LIMIT ACCESS FOR SPOT/NET/PRINT/TRAFFIC                              
***********************************************************************         
VALUSMLA NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RHALF,C'S'                                                       
         CLI   RWORK,2             SPOT                                         
         BE    VUSM050                                                          
         CLI   RWORK,13            SPOT TRAFFIC                                 
         BE    VUSM050                                                          
         MVI   RHALF,C'N'                                                       
         CLI   RWORK,3             NETWORK                                      
         BE    VUSM050                                                          
         MVI   RHALF,C'P'                                                       
         CLI   RWORK,4             PRINT                                        
         BNE   VUSMOKX             NOT A MEDIA SYSTEM, LEAVE IT                 
*                                                                               
VUSM050  CLC   =C'CG=',FVIFLD      NEW STYLE CLIENT GROUP                       
         BE    VUSM60              YES                                          
         CLI   FVIFLD,C'$'         OFFICE LIST?                                 
         BE    VUSMOKX             YES: LEAVE IT                                
         CLI   FVIFLD,C'+'         MARKET?                                      
         BE    VUSMOKX             YES: LEAVE IT                                
         CLI   FVIFLD,C'*'         OFFICE OR CLIENT GROUP?                      
         BNE   VUSM090             NO: PROCESS SINGLE CLIENT                    
         CLI   FVIFLD+1,C'*'       TWO CHARACTER OFFICE?                        
         BE    VUSM080             YES: PROCESS TWO CHARACTER OFFICE            
         OC    FVIFLD+2(2),FVIFLD+2 ONE BYTE OFFICE                             
         BZ    VUSMOKX             YES: LEAVE IT                                
         CLC   FVIFLD+2(2),=C'  '  ONE BYTE OFFICE                              
         BE    VUSMOKX             YES: LEAVE IT                                
         CLI   FVIFLD+2,C'0'       CHARACTER #, OLD STYLE CLIENT GROUP?         
         BNL   VUSMOKX             YES: LEAVE IT                                
         B     VUSMNOX                                                          
*----------------------------------------------------------------------         
* CLIENT GROUP LIMIT ACCESS                                                     
*----------------------------------------------------------------------         
VUSM60   LHI   RE,9                MAXIMUM FIELD LENGTH FOR CLIENT GRP          
         LHI   RF,2-1              START WITH TWO CHARACTER GROUP ID            
         CLI   FVIFLD+4,C'0'       IS NEXT CHARACTER A NUMBER?                  
         BL    VUSM062             NO: MUST BE A TWO CHAR GROUP ID              
         LHI   RF,1-1              YES: THEN MUST BE A 1 CHAR GROUP ID          
         LHI   RE,8                8 IS MAX IF ONE CHARACTER ID                 
VUSM062  LLC   R1,FVILEN                                                        
         CR    R1,RE               COMPARE LENGTH TO MAX                        
         BH    VUSMNOX             TOO BIG, INVALID CLIENT GROUP                
*                                                                               
         LA    R1,SPCGRTAB         TOP OF VALID CLIENT GROUP TABLE              
VUSM064  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),FVIFLD+3    COMPARE 1 OR 2 CHARACTERS                    
         BE    VUSM066             FOUND A MATCH                                
         CLI   0(R1),C'Z'          C'Z' MEANS END OF TABLE                      
         BE    VUSMNOX             EOT: INVALID CLIENT GROUP                    
         LA    R1,L'SPCGRTAB(,R1)  BUMP TO NEXT ENTRY                           
         B     VUSM064                                                          
VUSM066  CLI   2(R1),C'*'          DON'T ALLOW ID=GG (X'5C', C'*')              
         BE    VUSMNOX             IT'S CONFUSING: INVALID CLIENT GROUP         
*                                                                               
         XC    0(L'CTSYSLMT,R3),0(R3)                                           
         MVI   0(R3),C'*'                                                       
         MVC   1(1,R3),2(R1)       MOVE INTERNAL CLIENT GROUP ID                
         LA    R1,FVIFLD+4                                                      
         AR    R1,RF               NUMBER PORTION OF CLIENT GROUP               
         MVC   RFULL,0(R1)                                                      
*                                                                               
         LA    R1,RFULL                                                         
         LHI   RE,4                MAXIMUM OF 4 DIGITS                          
VUSM070  CLI   0(R1),C' '          END OF NUMBERS?                              
         BNH   VUSM075             YES                                          
         CLI   0(R1),C'0'          NUMBER?                                      
         BL    VUSMNOX             NO: INVALID CLIENT GROUP                     
         CLI   0(R1),C'9'          NUMBER?                                      
         BH    VUSMNOX             NO: INVALID CLIENT GROUP                     
*                                                                               
         CHI   RE,4                GET CLIENT GROUP NUMBER AND CONVERT          
         BNE   VUSM071                                                          
         IC    RF,0(R1)                                                         
         SLL   RF,4                                                             
         STC   RF,RBYTE                                                         
         NI    RBYTE,X'F0'         LAST SIGNIFICANT DIGIT THEN X'F'             
         OC    2(1,R3),RBYTE                                                    
         B     VUSM074                                                          
VUSM071  CHI   RE,3                                                             
         BNE   VUSM072                                                          
         MVC   RBYTE,0(R1)                                                      
         NI    RBYTE,X'0F'                                                      
         OC    2(1,R3),RBYTE                                                    
         B     VUSM074                                                          
VUSM072  CHI   RE,2                                                             
         BNE   VUSM073                                                          
         IC    RF,0(R1)                                                         
         SLL   RF,4                                                             
         STC   RF,RBYTE                                                         
         NI    RBYTE,X'F0'                                                      
         OC    3(1,R3),RBYTE                                                    
         B     VUSM074                                                          
VUSM073  CHI   RE,1                                                             
         BNE   VUSM073                                                          
         MVC   RBYTE,0(R1)                                                      
         NI    RBYTE,X'0F'                                                      
         OC    3(1,R3),RBYTE                                                    
*                                                                               
VUSM074  LA    R1,1(,R1)                                                        
         BCT   RE,VUSM070                                                       
         B     VUSMOKX                                                          
*                                                                               
VUSM075  CHI   RE,4                                                             
         BE    VUSMNOX          MUST HAVE A NUMBER                              
         CHI   RE,3                                                             
         BNE   VUSM076                                                          
         OI    2(R3),X'0F'                                                      
         B     VUSMOKX                                                          
VUSM076  CHI   RE,2                                                             
         BNE   VUSM077                                                          
         OI    3(R3),X'F0'                                                      
         B     VUSMOKX                                                          
VUSM077  CHI   RE,1                                                             
         BNE   VUSMOKX                                                          
         OI    3(R3),X'0F'                                                      
         B     VUSMOKX                                                          
                                                                                
*----------------------------------------------------------------------         
* TWO CHARACTER OFFICE LIMIT ACCESS                                             
*----------------------------------------------------------------------         
VUSM080  LA    RF,ACWORK                                                        
         USING OFFICED,RF                                                       
         XC    ACWORK,ACWORK                                                    
         MVC   OFCSYS,RHALF        SYSTEM ID                                    
         MVC   OFCAGY,CUAALF       ALPHA AGENCY                                 
         MVC   OFCOFC2,FVIFLD+2    TWO BYTE OFFICE                              
         OC    OFCOFC2,=CL2' '                                                  
         DROP  RF                                                               
*                                                                               
         USING COMFACSD,RE                                                      
         L     RE,ACOM                                                          
         XC    ACPARM(8),ACPARM                                                 
         MVC   ACPARM+4(4),=X'D9000A38' GET OFFICER ADDRESS                     
         GOTO1 CCALLOV,ACPARM                                                   
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         DROP  RE                                                               
*                                                                               
         L     RF,ACPARM                                                        
         GOTO1 (RF),ACPARM,(C'2',ACWORK),ACOM                                   
*                                                                               
         LA    R1,ACWORK                                                        
         USING OFFICED,R1                                                       
         TM    OFCINDS,OFCINOLA+OFCIOINV  NOT USING 2 OFFS OR INVALID           
         BNZ   VUSMNOX                                                          
         XC    0(L'CTSYSLMT,R3),0(R3)                                           
         MVI   0(R3),C'*'                                                       
         MVC   1(L'OFCOFC,R3),OFCOFC      INTERNAL 1 BYTE OFFICE                
         B     VUSMOKX                                                          
         DROP  R1                                                               
*----------------------------------------------------------------------         
* CLIENT LIMIT ACCESS                                                           
*----------------------------------------------------------------------         
VUSM090  CLI   RWORK,4             PRINT DOES NOT PACK CLIENTS                  
         BE    VUSMOKX                                                          
*                                                                               
         MVC   APPARM+4(4),=X'D9000A14' CLPACK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),FVIFLD,(R3)                                            
         CLI   0(R1),0                                                          
         BNE   VUSMNOX                                                          
         J     VUSMOKX                                                          
*                                                                               
VUSMNOX  LTR   RB,RB                                                            
         J     EXIT                                                             
VUSMOKX  CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
*&&                                                                             
                                                                                
***********************************************************************         
* VALIDATE LIMIT ACCESS CLIENT CODE FORMAT                            *         
* CC OR CCC WHERE C ALPHANUMERIC                                      *         
*                                                                     *         
* NTRY: FVIFLD CONTAINS CODE                                          *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
VALCC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   FVILEN,2                                                         
         BL    VACCNO                                                           
         CLI   FVILEN,3                                                         
         BH    VACCNO                                                           
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTOR VALAN,RPARM,FVIFLD,(RF)                                          
         BNE   VACCNO                                                           
         B     VACCOK                                                           
*                                                                               
VACCNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VACCOK   CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
*&&US                                                                           
***********************************************************************         
* VALIDATE LIMIT ACCESS OFFICE CODE FORMAT                            *         
* *C OR *CC (OPTIONAL) WHERE C ALPHANUMERIC                           *         
*                                                                     *         
* NTRY: FVIFLD CONTAINS CODE                                          *         
*       RBYTE CONTAINS FLAG - 0-1 C MAX, NON 0-2 C MAX                *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
VALOC    NTR1  BASE=*,LABEL=*                                                   
         CLI   FVILEN,2                                                         
         BL    VAOCNO                                                           
         CLI   FVIFLD,C'*'                                                      
         BNE   VAOCNO                                                           
         CLI   RBYTE,0                                                          
         BNE   VAOC010                                                          
         CLI   FVILEN,2                                                         
         BH    VAOCNO                                                           
         GOTOR VALAN,RPARM,FVIFLD+1,1                                           
         BNE   VAOCNO                                                           
         B     VAOCOK                                                           
*                                                                               
VAOC010  CLI   FVILEN,3                                                         
         BH    VAOCNO                                                           
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         GOTOR VALAN,RPARM,FVIFLD+1,(RF)                                        
         BNE   VAOCNO                                                           
         B     VAOCOK                                                           
*                                                                               
VAOCNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VAOCOK   CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* VALIDATE LIMIT ACCESS OFFICE CODE FORMAT (2 CHARACTER FOR MEDIA)    *         
* **C OR **CC (OPTIONAL) WHERE C ALPHANUMERIC/KEYBOARD SYMBOL         *         
*                                                                     *         
* NTRY: FVIFLD CONTAINS CODE                                          *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
VALCGRP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   FVILEN,3                                                         
         BL    VALCGNO                                                          
         CLC   =C'CG=',FVIFLD                                                   
         BE    VALCG10                                                          
         CLI   FVIFLD,C'*'         OFFICE OR CLIENT GROUP?                      
         BNE   VALCGNO             NO: PROCESS SINGLE CLIENT                    
         CLI   FVIFLD+1,C'A'       LOOK FOR OLD STYLE CLIENT GROUP              
         BL    VALCGNO             C'*AN'                                       
         CLI   FVIFLD+1,C'Z'       .                                            
         BH    VALCGNO             .                                            
         CLI   FVIFLD+2,C'0'       .                                            
         BNH   VALCGNO             .                                            
         CLI   FVIFLD+3,C'0'       .                                            
         BH    VALCGOK             .                                            
         CLI   FVIFLD+3,C' '       .                                            
         BNH   VALCGOK             .                                            
         B     VALCGNO                                                          
*                                                                               
VALCG10  LHI   RE,9                MAXIMUM FIELD LENGTH FOR CLIENT GRP          
         LHI   RF,2-1              START WITH TWO CHARACTER GROUP ID            
         CLI   FVIFLD+4,C'0'       IS NEXT CHARACTER A NUMBER?                  
         BL    VALCG062            NO: MUST BE A TWO CHAR GROUP ID              
         LHI   RF,1-1              YES: THEN MUST BE A 1 CHAR GROUP ID          
         LHI   RE,8                6 IS MAX IF ONE CHARACTER ID                 
VALCG062 LLC   R1,FVILEN                                                        
         CR    R1,RE               COMPARE LENGTH TO MAX                        
         BH    VALCGNO             TOO BIG, INVALID CLIENT GROUP                
*                                                                               
         LA    R1,SPCGRTAB         TOP OF VALID CLIENT GROUP TABLE              
VALCG064 EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),FVIFLD+3    COMPARE 1 OR 2 CHARACTERS                    
         BE    VALCG066            FOUND A MATCH                                
         CLI   0(R1),C'Z'          C'Z' MEANS END OF TABLE                      
         BE    VALCGNO             EOT: INVALID CLIENT GROUP                    
         LA    R1,L'SPCGRTAB(,R1)  BUMP TO NEXT ENTRY                           
         B     VALCG064                                                         
VALCG066 CLI   2(R1),C'*'          DON'T ALLOW ID=GG (X'5C', C'*')              
         BE    VALCGNO             IT'S CONFUSING: INVALID CLIENT GROUP         
*                                                                               
         LA    R1,FVIFLD+4                                                      
         AR    R1,RF               NUMBER PORTION OF CLIENT GROUP               
         LHI   RE,4                MAXIMUM OF 4 DIGITS                          
VALCG070 CLI   0(R1),C' '          END OF NUMBERS?                              
         BNH   VALCG075            YES: LEGITIMATE CLIENT GROUP                 
         CLI   0(R1),C'0'          NUMBER?                                      
         BL    VALCGNO             NO: INVALID CLIENT GROUP                     
         CLI   0(R1),C'9'          NUMBER?                                      
         BH    VALCGNO             NO: INVALID CLIENT GROUP                     
*                                                                               
VALCG074 LA    R1,1(,R1)                                                        
         BCT   RE,VALCG070                                                      
         B     VALCGOK                                                          
*                                                                               
VALCG075 CHI   RE,4                STILL ON 4?  MUST BE AT LEAST 1 LESS         
         BNE   VALCGOK                                                          
*                                                                               
VALCGNO  LTR   RB,RB                                                            
         J     EXIT                                                             
VALCGOK  CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* VALIDATE LIMIT ACCESS OFFICE CODE FORMAT (2 CHARACTER FOR MEDIA)    *         
* **C OR **CC (OPTIONAL) WHERE C ALPHANUMERIC/KEYBOARD SYMBOL         *         
*                                                                     *         
* NTRY: FVIFLD CONTAINS CODE                                          *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
VALOC2M  NTR1  BASE=*,LABEL=*                                                   
         CLI   FVILEN,3                                                         
         BL    VA2MNO                                                           
         CLI   FVIFLD,C'*'                                                      
         BNE   VA2MNO                                                           
         CLI   FVIFLD+1,C'*'                                                    
         BNE   VA2MNO                                                           
         CLI   FVIFLD+2,C'A'                                                    
         BL    VA2MNO                                                           
         B     VA2MOK                                                           
*                                                                               
VA2MNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VA2MOK   CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* VALIDATE LIMIT ACCESS OFFICE LIST CODE FORMAT                                 
* $C WHERE C IS VALID OFFICE LIST CHARACTER                                     
*  ENTRY: FVIFLD CONTAINS CODE                                                  
*  EXIT:  CC=ZERO IF OK                                                         
***********************************************************************         
VALOLL   NTR1  BASE=*,LABEL=*                                                   
         CLI   FVILEN,2                                                         
         BE    VAOLL10                                                          
         CLI   FVILEN,3                                                         
         BNE   VAOLLNO                                                          
VAOLL10  CLI   0(R3),C'$'                                                       
         BNE   VAOLLNO                                                          
*                                                                               
         LA    RF,ACWORK                                                        
         USING OFFICED,RF                                                       
         XC    ACWORK,ACWORK                                                    
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         CLI   RWORK,4             TEST PRINT                                   
         BNE   *+8                                                              
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,CUAALF                                                    
         MVC   OFCMOL2,1(R3)       OFFICE LIST                                  
         OI    OFCINDS,OFCIMOLC    OFFICE LIST CONVERSION                       
         DROP  RF                                                               
*                                                                               
         USING COMFACSD,RE                                                      
         L     RE,ACOM                                                          
         XC    ACPARM(8),ACPARM                                                 
         MVC   ACPARM+4(4),=X'D9000A38' GET OFFICER ADDRESS                     
         GOTO1 CCALLOV,ACPARM                                                   
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         DROP  RE                                                               
*                                                                               
         L     RF,ACPARM                CALL OFFICER                            
         GOTO1 (RF),ACPARM,(C'2',ACWORK),ACOM                                   
         BE    VAOLLOK                                                          
*                                                                               
VAOLLNO  LTR   RB,RB                                                            
         J     EXIT                                                             
VAOLLOK  CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* VALIDATE LIMIT ACCESS OFFICE LIST CODE FORMAT                                 
* $C WHERE C IS VALID OFFICE LIST CHARACTER                                     
*  ENTRY: FVIFLD CONTAINS CODE                                                  
*  EXIT:  CC=ZERO IF OK                                                         
***********************************************************************         
VALOL    NTR1  BASE=*,LABEL=*                                                   
         CLI   FVILEN,2                                                         
         BNE   VAOLNO                                                           
         CLI   FVIFLD,X'5B'            WATCH OUT FOR UK/US HEX                  
         BNE   VAOLNO                                                           
         CLI   FVIFLD+1,C'#'           HASH IS ALLOWED                          
         BE    VAOLOK                                                           
         CLI   FVIFLD+1,C'<'           LESS THAN IS ALLOWED                     
         BE    VAOLOK                                                           
         CLI   FVIFLD+1,C'>'           GREATER THAN IS ALLOWED                  
         BE    VAOLOK                                                           
         CLI   FVIFLD+1,C'@'           AT SYMBOL IS ALLOWED                     
         BE    VAOLOK                                                           
         GOTOR VALAN,RPARM,FVIFLD+1,1                                           
         BNE   VAOLNO                                                           
         B     VAOLOK                                                           
*                                                                               
VAOLNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VAOLOK   CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
*&&                                                                             
***********************************************************************         
* VALIDATE LIMIT ACCESS MARKET CODE FORMAT                            *         
* +C WHERE C ALPHANUMERIC                                             *         
*                                                                     *         
* NTRY: FVIFLD CONTAINS CODE                                          *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
                                                                                
VALMC    NTR1  BASE=*,LABEL=*                                                   
         CLI   FVILEN,2                                                         
         BNE   VAMCNO                                                           
         CLI   FVIFLD,C'+'                                                      
         BNE   VAMCNO                                                           
         GOTOR VALAN,RPARM,FVIFLD+1,1                                           
         BNE   VAMCNO                                                           
         B     VAMCOK                                                           
*                                                                               
VAMCNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VAMCOK   CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIMIT ACCESS OFFICE CODE FORMAT (2 CHAR CODE)                        
* **CC WHERE C ALPHANUMERIC                                           *         
*                                                                     *         
* NTRY: FVIFLD CONTAINS CODE                                          *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
VALOF2   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   FVILEN,4                                                         
         BNE   VOF2NO                                                           
         CLC   FVIFLD(2),=CL2'**'                                               
         BNE   VOF2NO                                                           
         GOTOR VALAN,RPARM,FVIFLD+2,2                                           
         BNE   VOF2NO                                                           
         B     VOF2OK                                                           
*                                                                               
VOF2NO   LTR   RB,RB                                                            
         J     EXIT                                                             
VOF2OK   CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE ALPHANUMERIC FIELD                                         *         
*                                                                     *         
* NTRY: 0(R1) = FIELD                                                 *         
*       4(R1) = LENGTH                                                *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
VALAN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RE,15,0(R1)                                                      
         SR    RF,RF                                                            
         IC    RF,7(R1)                                                         
         LTR   RF,RF                                                            
         BZ    VAANOK                                                           
VAAN010  CLI   0(RE),C'A'                                                       
         BL    VAAN020                                                          
         CLI   0(RE),C'Z'                                                       
         BH    VAAN020                                                          
         B     VAAN030                                                          
VAAN020  CLI   0(RE),C'0'                                                       
         BL    VAANNO                                                           
         CLI   0(RE),C'9'                                                       
         BH    VAANNO                                                           
VAAN030  LA    RE,1(RE)                                                         
         BCT   RF,VAAN010                                                       
         B     VAANOK                                                           
*                                                                               
VAANNO   LTR   RB,RB                                                            
         J     EXIT                                                             
VAANOK   CR    RB,RB                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* ENTRY POINT FOR OPTION ROUTINES                                     *         
***********************************************************************         
                                                                                
OBASE    NTR1  BASE=*,LABEL=*                                                   
         LR    R8,R7                                                            
         AHI   R8,SYSWORK-WORKD    SETUP SYSWORK FOR SEACS                      
         USING SYSWORK,R8                                                       
*                                                                               
         SRL   RF,24               DETERMINE OPTION NUMBER (1 BASED)            
         SLL   RF,2                                                             
         CH    RF,=AL2(OPTMAX)                                                  
         BNH   *+2(RF)                                                          
         DC    H'0'                OPTION NUMBER OUSIDE RANGE                   
                                                                                
OBRANCH1 B     OVALAGY                                                          
         B     OVALUID                                                          
         B     OVALPWS                                                          
         B     OVALTER                                                          
OPTMAX   EQU   *-OBRANCH1                                                       
         EJECT                                                                  
*----------------------------------                                             
* VALIDATE AGENCY ALPHA                                                         
*----------------------------------                                             
OVALAGY  EQU   *                                                                
*&&UK*&& CLC   =C'#E',FVIFLD                                                    
*&&US*&& CLC   =C'#N',FVIFLD                                                    
         BE    OVAGYBAD            CAN'T SEE THIS AGY'S FILE                    
*                                                                               
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           R2=A(ID RECORD)                              
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   OVAGYBAD                                                         
         MVC   SCWORK(L'OPTAGY),FVIFLD   SAVE AGENCY ALPHA ID                   
         B     OVALAGYX                                                         
*                                                                               
OVAGYBAD MVC   FVMSGNO,=AL2(CE#INVAA)                                           
*                                                                               
OVALAGYX J     EXIT                                                             
*----------------------------------                                             
* VALIDATE USER ID                                                              
*  RETURN ASSOCIATED AGENCY ALPHA ID                                            
*----------------------------------                                             
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
OVUID30  EQU   *                                                                
*&&UK*&& CLC   =C'#E',CTAGYID                                                   
*&&US*&& CLC   =C'#N',CTAGYID                                                   
         BE    OVUIDBAD            CAN'T SEE THIS AGY'S FILE                    
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
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
OVALUIDX J     EXIT                                                             
         DROP  R3                                                               
*----------------------------------                                             
* VALIDATE PASSWORD=YES SWITCH                                                  
*----------------------------------                                             
OVALPWS  EQU   *                                                                
         CLC   FVIFLD(1),CT@YES                                                 
         BNE   OVPWSBAD                                                         
         MVI   SCWORK,C'Y'         SAVE PW= Y                                   
         B     OVPWSX                                                           
*                                                                               
OVPWSBAD MVC   FVMSGNO,=AL2(FVFNOTV)                                            
OVPWSX   J     EXIT                                                             
                                                                                
*----------------------------------                                             
* VALIDATE TERM=, EXP= PERSONID SWITCH                                          
*----------------------------------                                             
OVALTER  EQU   *                                                                
         CLC   FVIFLD(1),CT@YES                                                 
         BE    OVTER10                                                          
         CLC   FVIFLD(1),CT@NO                                                  
         BE    OVTER12                                                          
         CLC   FVIFLD(1),CT@ONLY                                                
         BE    OVTER14                                                          
         B     OVTERBAD                                                         
*                                                                               
OVTER10  MVI   SCWORK,C'Y'      SAVE TERM= Y (YES)                              
         B     OVTERX                                                           
OVTER12  MVI   SCWORK,C'N'      SAVE TERM= N (NO)                               
         B     OVTERX                                                           
OVTER14  MVI   SCWORK,C'O'      SAVE TERM= O (ONLY)                             
         B     OVTERX                                                           
*                                                                               
OVTERBAD MVC   FVMSGNO,=AL2(FVFNOTV)                                            
OVTERX   J     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
* DDMASTD                                                                       
       ++INCLUDE DDMASTD                                                        
* FAUTL                                                                         
       ++INCLUDE FAUTL                                                          
* FAXTRAINF                                                                     
       ++INCLUDE FAXTRAINF                                                      
* DDOFFICED                                                                     
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTS S/R LOCAL W/S                                                 *         
***********************************************************************         
RWRKD    DSECT                                                                  
*                                                                               
APFKHDR1 DS    A                                                                
PFKLEN1  DS    H                                                                
APFKHDR2 DS    A                                                                
PFKLEN2  DS    H                                                                
*                                                                               
RDUB     DS    D                                                                
RWORK    DS    XL64                                                             
RBITT    DS    XL32                                                             
RPARM    DS    8F                                                               
RIPARM   DS    8F                                                               
RGPARM   DS    8F                                                               
RFULL    DS    F                                                                
RHALF    DS    H                                                                
RGRCID   DS    XL(L'GRCID)         TEMP GRIDS COLUMN ID                         
RGRCID#  DS    XL2                 TEMP GRIDS RECURRING COL ID #                
RGRFLN   DS    XL1                 GRIDS FIELD LENGTH                           
RBYTE    DS    XL1                                                              
RFLAG    DS    XL1                                                              
*                                                                               
RPFKNAM  DS    CL9                                                              
RPFKAREA DS    XL170               PFKEY DISPLAY BIULD AREA                     
MAXPFLEN EQU   158                                                              
*                                                                               
RIOSAVE  DS    XL(IOAREAX-IOAREA)                                               
*                                                                               
RBLOCK   DS    20CL32                                                           
RIO      DS    2000C                                                            
RWRKX    EQU   *                                                                
         EJECT                                                                  
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SEACS00   09/17/19'                                      
         END                                                                    
