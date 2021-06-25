*          DATA SET GEPAK00    AT LEVEL 011 AS OF 07/10/19                      
*PHASE TF3600A                                                                  
*NOTE: PHASE=TF35 IN UK, PHASE=TF36 IN US                                       
*                                                                               
*INCLUDE KHDUMMY                   (PAN DDKHDUMMY)   SET A(LOAD POINT)          
*                                                                               
*&&      SET   NOP=N               COMMENTED OUT CODE                           
*                                                                               
         TITLE 'TF3600 - PCPAK DATA FILE TRANSFER PROGRAM CONTROLLER'           
         SPACE 1                                                                
*WHO  LVL DATE    CHANGE                                                        
*DCHA 001 10DEC03 START!                                                        
         EJECT                                                                  
         PRINT NOGEN                                                            
PAK00    CSECT                                                                  
         NMODL PAKWRKLQ,**PK00**,R8,R9,RR=RE                                    
         LR    R2,RC               CLEAR WORKING STORAGE                        
         L     R3,=AL4(PAKWRKLQ)                                                
         SR    R5,R5                                                            
         MVCL  R2,R4                                                            
*                                                                               
         LR    R0,RC               R0=A(SPOOLD)                                 
         LA    RC,SPOOLEND-SPOOLD(RC)                                           
         USING GEND,RC             RC=A(GENCONS GLOBAL STORAGE)                 
         LA    R7,IO               SYSTEM STORAGE STARTS AFTER IO AREAS         
         AHI   R7,IOTLNQ                                                        
         USING SYSWORKD,R7                                                      
*                                                                               
         STM   R8,RE,S#REGS        STORE CONTROLLER REGS                        
*                                                                               
***********************************************************************         
* CONTROL                                                             *         
***********************************************************************         
CONTROL  BAS   RE,SYSINIT          INITIALISE                                   
         L     RA,S#RA                                                          
         USING PAKFFD,RA                                                        
         BNE   CTLERR                                                           
         SPACE 1                                                                
CTL010   BAS   RE,TMPREAD          READ TMPSTR PAGE 4 INTO TIA                  
         GOTO1 VGENCON,PARAS,ASPOOLD                                            
         BAS   RE,TMPWRITE         WRITE TMPSTR PAGE 4 FROM TIA                 
         B     CTLEXIT                                                          
*                                                                               
CTLERR   BAS   RE,SETERR                                                        
         B     CTLEXIT                                                          
*                                                                               
CTLEXIT  BAS   RE,FINAL                                                         
         B     EXIT                                                             
*                                                                               
EXITHIGH CR    R8,RB               SET CC HIGH R8=2ND BASE                      
         B     *+12                                                             
EXITBAD  CR    RB,R8               SET CC LOW                                   
         B     *+6                                                              
EXITOK   CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SYSINIT - INITIALISATION                                            *         
***********************************************************************         
SYSINIT  NTR1  ,                                                                
*                                                                               
         ST    RD,SYSRD            SET FOR GENCON EXIT ROUTINE                  
         ST    R0,ASPOOLD                                                       
         ST    RB,SYSRB                                                         
         ST    R1,SYSPARMS         FACPAK PARAMETERS                            
         ST    R7,ASYSD            A(SYSTEM WIDE STORAGE)                       
*                                                                               
         MVC   S#ATIOB,0(R1)       NB GET SAME PARM IN CTL AS ELSEWHERE         
         L     RA,4(R1)            RA=A(TWA)                                    
         MVC   ATWA,4(R1)                                                       
         MVC   S#RA,4(R1)                                                       
         MVC   S#ATIA,12(R1)                                                    
         MVC   ATIA,12(R1)                                                      
         MVC   S#ACFACS,16(R1)     A(COMFACS)                                   
         MVC   S#AXTRA,20(R1)      A(XTRAINFO)                                  
*                                                                               
         L     R1,S#ACFACS         EXTRACT COMFACS ADDRESSES                    
         USING COMFACSD,R1                                                      
         MVC   ADDAY,CADDAY                                                     
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   GETDAY,CGETDAY                                                   
         MVC   GETFACT,CGETFACT                                                 
         MVC   GETTXT,CGETTXT                                                   
         MVC   SCANNER,CSCANNER                                                 
         MVC   UNSCAN,CUNSCAN                                                   
         MVC   SWITCH,CSWITCH                                                   
         MVC   VDLFLD,CDLFLD                                                    
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         DROP  R1                                                               
         SPACE 1                                                                
         GOTO1 SWITCH,PARAS,X'FFFFFFFF',0      GET A(UTL)                       
         L     R1,0(R1)                                                         
         STCM  R1,15,S#AUTL                                                     
         MVC   S#OVSYS,TOVSYS-UTLD(R1)                                          
*                                                                               
         GOTO1 GETFACT,PARAS,0     GETFACT ADDRESSES                            
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   S#ASYSL,FASYSLST                                                 
         DROP  RF                                                               
*                                                                               
SYIN004  GOTO1 DATCON,PARAS,(5,0),S#YYMMDD                                      
         GOTO1 GETDAY,PARAS,S#YYMMDD,DUB                                        
         MVC   S#DAY,0(R1)                                                      
*                                                                               
         MVI   SPACEPAD,SPACEQ     OWN SPACES (ELSE ASPOOLD NEEDED)             
         MVC   SPACEPAD+1(L'SPACEPAD-1),SPACEPAD                                
         MVI   HIGHVALS,X'FF'                                                   
         MVC   HIGHVALS+1(L'HIGHVALS-1),HIGHVALS                                
         SPACE 1                                                                
         LA    R2,SYVCONS          RESOLVE VTYPE LIST (*INCS ETC)               
         LA    R3,VCONLIST         ADDRESS LIST TO RELOCATE                     
         LA    R4,NVCONS           N'ENTRIES                                    
         SPACE 1                                                                
SYIN010  ICM   R1,15,0(R3)                                                      
         BZ    SYIN012                                                          
         A     R1,S#REL00                                                       
         ST    R1,0(R2)                                                         
SYIN012  LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYIN010                                                       
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON RTN'S ADDRESS AND NO'S         
         SR    R3,R3               RELATIVE ROUTINE NUMBER                      
         LA    R4,SYCOMM                                                        
         LA    RF,VCOUNT                                                        
         SPACE 1                                                                
SYIN020  ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,SYIN020                                                       
         SPACE 1                                                                
SYIN021  LA    R2,SYCADDRS         CORE RESIDENT PHASE ADDRESSES                
         LA    R3,CRESLST          CORE RESIDENT PHASE NUMBERS                  
         LA    R4,NCRESQ           N'CORE RESIDENT PHASES                       
         SPACE 2                                                                
         XC    PARAS(12),PARAS                                                  
         MVC   PARAS+4(4),=X'D9000A00'                                          
         MVC   PARAS+7(1),0(R3)                                                 
         GOTO1 CALLOV,PARAS                                                     
         ORG   *-2                                                              
SYIN022  CLI   0(R3),NULLQ                                                      
         BE    *+12                                                             
         BASR  RE,RF                                                            
         MVC   0(4,R2),PARAS                                                    
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         MVC   PARAS+7(1),0(R3)                                                 
         XC    PARAS(4),PARAS                                                   
         BCT   R4,SYIN022                                                       
*                                                                               
SYIN023  LA    R2,TABADDRS         TABLE ADDRESSES                              
         LA    RF,NTABSQ                                                        
         LA    R4,SYSTABS                                                       
*                                                                               
SYIN024  ICM   R1,15,0(R2)                                                      
         BZ    SYIN026                                                          
         A     R1,S#REL00                                                       
         ST    R1,0(R4)                                                         
SYIN026  LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,SYIN024                                                       
*                                                                               
         MVC   S#VCLTRL,VCOMLTRL   ALL ROOT RTNS W/S LITERAL                    
         LA    R1,SVAREA           SET SAVE STORAGE ADDRESSES                   
         ST    R1,ASTARTSV                                                      
         XC    AENDSV,AENDSV       PREVENT GENCON CLEARING REGSAVE              
*                                                                               
         LA    R1,IO               SET AIO1 FOR PRE GENCON USAGE                
         ST    R1,AIO1                                                          
*                                                                               
         LR    R1,R7               SET A(TEMPSTORE) ETC                         
         AHI   R1,(SVPAGE1-SYSWORKD)                                            
         ST    R1,S#ASVP1                                                       
         LR    R1,R7                                                            
         AHI   R1,(SVPAGE2-SYSWORKD)                                            
         ST    R1,S#ASVP2                                                       
         LR    R1,R7                                                            
         AHI   R1,(SVPAGE3-SYSWORKD)                                            
         ST    R1,S#ASVP3                                                       
         LR    R1,R7                                                            
         AHI   R1,(TSARBLK-SYSWORKD)                                            
         ST    R1,S#ATSBLK                                                      
*                                                                               
         OI    GENSTAT1,EFHKYMRG+USKYMRG  MERGE ON FLD NOS,EVEN IF LIST         
         OI    GENSTAT1,NOSETEFH   SET MY OWN A(FIELD HEADERS)                  
         OI    GENSTAT1,APPLIC     CALL OVERLAY TO USE PHASE STRUCTURE          
         OI    GENSTAT2,STLCOK+DISTHSPG   L/C MSGS+SAME LIST AFTER SEL          
         OI    GENSTAT3,IGNNONXK+OKVALSEL IGN NON EXT KEY FLDS+VAL SEL          
         MVI   ACTELOPT,NOQ        PREVENT GENCON ADDING ACTIVITY ELEMS         
         MVI   GETMSYS,GTGENSYS    GENERAL MESSAGE SYSTEM                       
         MVI   SYSTEM,C'X'         NOT CONTROL (FAC PARMS)                      
*                                                                               
         MVI   MAXIOS,IONQ         NUMBER OF IO AREAS                           
         MVC   SIZEIO,=AL4(IOLNQ)  SET SINGLE IOAREA LENGTH                     
         MVI   NTWA,TWANQ+NTWA14K  NO+SIZE OF TWAS FOR SAVE                     
         MVC   SYSDUMMY,VDUMMY     END OF ROOT (IE LOAD POINT FOR OLAY)         
         MVC   GETUSER,VGETUSR     ROUTINE TO GET USER NAME AND ADDRESS         
*                                                                               
         MVC   REQFILE,=C'CTLREQ  '                                             
         CLI   TWAFLID,NULLQ       RESTORE/DEFINE FILE-ID                       
         BNE   *+8                                                              
         MVI   TWAFLID,GENDIRQ     SET DEFAULT IO VARIABLES                     
         MVC   SYFLID,TWAFLID                                                   
         BAS   RE,SETFLID                                                       
*                                                                               
         MVI   REQPRI1,C'X'        SET DEFAULT JOB CLASS FOR SPOON              
         MVI   REQPRI2,C'5'        SET DEFAULT JOB PRIORITY FOR SPOON           
*                                                                               
         MVC   LWORK,=AL4(PAKWRKLQ)  TOTAL SIZE OF NMOD AREA                    
         MVC   SYSPHASE,=X'D90F3600' SET FOR CALLOV READ OF TF36 PHASES         
*                                                                               
         L     R1,ASPOOLD          PRESET SPACES ETC                            
         USING SPOOLD,R1                                                        
         MVC   RCPROG(2),=C'CT'    PREFIX FOR REPORT ID                         
         MVC   SPACES,SPACEPAD                                                  
         DROP  R1                                                               
         SPACE 1                                                                
         LA    R1,DMYRECH          SET DUMMY FIELD HEADER ADDRS                 
         LA    RE,EFHREC                                                        
         LA    R0,(EFHOTH-EFHREC)/4                                             
         ST    R1,0(RE)                                                         
         LA    R1,DMYACTH-DMYRECH(R1)                                           
         LA    RE,4(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         LA    R1,CONTAGH          SCREEN TAG IS A REAL ADDRESS                 
         ST    R1,EFHTAG                                                        
         OI    CONSERVH+FHOID,FHOITR+FHOIMO FORCE FATI3270 TO MERGE             
         SPACE 1                                                                
         LA    R1,DDSRECS          RECORD/ACTION DIRECTORY FOR DDS              
         CLI   TWAMODE,TWAMOFFQ    OFFLINE - FULL LIST ANYWAY                   
         BNE   SYIN100                                                          
         ST    R1,ARECACT                                                       
         SPACE 1                                                                
* OFFLINE INITIALISATION                                                        
*                                                                               
         MVI   TWAOFFC,C'*'        DDS PRIVILEGES OFFLINE                       
         MVC   TWAAUTH,HIGHVALS    DDS PRIVILEGES OFFLINE                       
         LA    R1,S#TIOB           DEFINE A DUMMY TIOB                          
         XC    S#TIOB,S#TIOB       ENSURE NO RANDOM SETTINGS                    
         ST    R1,S#ATIOB                                                       
*                                                                               
         ICM   R3,15,TWAMASTC      GET A(UTL)/A(SSB) OFFLINE                    
         USING MASTD,R3                                                         
         ICM   R1,15,MCUTL                                                      
         STCM  R1,15,S#AUTL                                                     
         ICM   R1,15,MCSSB                                                      
         STCM  R1,15,S#ASSB                                                     
*                                                                               
         CLI   TWACOMMN,1          IGNORE SPOOF'S 'RESOLVE HDRS MODE'           
         BE    SYSINITX                                                         
         SPACE 2                                                                
* ONLINE INITIALISATION                                                         
*                                                                               
SYIN100  CLI   TWAOFFC,C'*'        R1=A(DDSRECS)                                
         BE    *+8                                                              
         LA    R1,USERRECS         RECORD/ACTION DIRECTORY FOR USERS            
         ST    R1,ARECACT                                                       
         LA    R2,CONKEYH                                                       
         ST    R2,ACURFORC                                                      
         OC    TWARCAC,TWARCAC     TEST IF RECORD/ACTION DEFINED                
         BNZ   SYIN120                                                          
         SPACE 1                                                                
* SET RECORD ACTION FROM INFO PASSED IN DATA AT CONNECT                         
* ALL RECORD TYPES HAVE A SINGLE ACTION WITH THE SAME NAME. THEY ALL            
* INVOKE GENCONS 'OTHERS' ACTION SO OVERLAYS DO WHAT THEY WANT.                 
*                                                                               
* UPLOAD/SYS/LANG(/REBUILD) OR (/OVERRIDE)                                      
* DOWNLOAD/SYS/LANG/VERSION                                                     
* VERSION/SYS/LANG                                                              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,CONKEYH+FHILD                                               
         BZ    SYINERR1                                                         
         LA    R1,CONKEY                                                        
         LR    RE,R1                                                            
         CLI   0(R1),C'/'          EXTRACT ACTION                               
         BE    SYIN102                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     SYINERR2                                                         
*                                                                               
SYIN102  SR    R1,RE                                                            
         SHI   R1,1                R1=EX L'TEXT                                 
         BM    SYINERR1                                                         
*                                                                               
         L     RE,ARECACT          FIND RECORD TYPE                             
         USING GCRATABD,RE                                                      
SYIN104  CLI   GCRECENT,GCRECQ                                                  
         BNE   SYINERR2                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONKEY(0),GCRECNAM                                               
         BE    *+12                                                             
         LA    RE,GCRECLQ(RE)                                                   
         B     SYIN104                                                          
*                                                                               
         MVC   DMYREC,GCRECNAM                                                  
         MVC   DMYACT,GCRECNAM                                                  
         MVC   TWARECT,GCRECNUM                                                 
         MVC   TWAACTN,GCRECACT                                                 
         DROP  RE                                                               
*                                                                               
         MVI   DMYRECH+FHILD,L'DMYREC                                           
         MVI   DMYACTH+FHILD,L'DMYACT                                           
*                                                                               
         MVI   TWALACT,NULLQ       RESET GENCON LAST TIME VALUES                
         MVI   TWALREC,NULLQ                                                    
         MVI   TWASCR,NULLQ        FORCE SCREEN LOAD                            
         MVI   TWAXIOS,NULLQ       UNSET MAXIO EXCEEDED                         
         OI    CONKEYH+FHOID,FHOITR+FHOICU  SET CURSOR IF LOAD FAILS            
         SPACE 1                                                                
SYIN120  L     R1,S#ATIOB          DETECT ANY INPUT                             
         OC    TIOBCNT-TIOBD(,R1),TIOBCNT-TIOBD(R1)                             
         BZ    SYIN122                                                          
         CLC   TIOBCNT-TIOBD(,R1),=Y(1)                                         
         BNH   *+8                                                              
         OI    SYINPIND,ANYINPQ    SOMETHING INPUT                              
         SPACE 1                                                                
SYIN122  SR    R0,R0               DETERMINE IF PF KEY DETECTED                 
         ICM   R0,1,TIOBAID-TIOBD(R1)                                           
         BZ    SYIN124                                                          
         OI    SYINPIND,PFKQ       SET PF KEY ENTERED                           
         STC   R0,S#PFKEY                                                       
SYIN124  EQU   *                                                                
         SPACE 1                                                                
* ON AND OFFLINE INITIALISATION CONTINUED                                       
*                                                                               
SYIN200  EQU   *                                                                
         SPACE 1                                                                
SYSINITX B     EXITOK                                                           
         SPACE 1                                                                
SYINERR1 MVI   SYERRNO+1,MISSING                                                
         B     SYINERRX                                                         
SYINERR2 MVI   SYERRNO+1,INVINPQ                                                
         B     SYINERRX                                                         
         SPACE 1                                                                
SYINERRX LA    R1,CONKEYH                                                       
         ST    R1,SYACURSR                                                      
         OI    FHOID(R1),FHOICU+FHOITR                                          
         B     EXITBAD                                                          
         EJECT                                                                  
***********************************************************************         
* FINAL - EXIT ROUTINE RETURNING TO MONITOR/SPOOF                     *         
***********************************************************************         
         SPACE 1                                                                
FINAL    ST    RE,SAVERE                                                        
         CLI   TWAMODE,TWAMOFFQ                                                 
         BE    FIN040                                                           
*                                                                               
         ICM   RE,15,SYACURSR      OVERRIDE CURSOR POSN                         
         BZ    FIN020                                                           
         OI    FHOID(RE),FHOICU                                                 
*                                                                               
         SR    R1,R1               UNSET ELSEWHERE                              
FIN010   ICM   R1,1,0(RE)                                                       
         BZ    FIN020                                                           
         AR    RE,R1                                                            
         NI    FHOID(RE),X'FF'-FHOICU                                           
         B     FIN010                                                           
         SPACE 1                                                                
* GENCON FORCES INPUT NEXT TIME IF AN ADD FOR AN EXISTING REC IS                
* ATTEMPTED - THIS PREVENTS OUR PFKEY ACTION BEING TAKEN.                       
*                                                                               
FIN020   CLI   TWAACTN,ACTADD                                                   
         BNE   FIN030                                                           
         CLI   MODE,VALKEY                                                      
         BNE   FIN030                                                           
         CLI   ERROR,RECEXIST                                                   
         BE    *+12                                                             
         CLI   ERROR,DELEXIST                                                   
         BNE   FIN030                                                           
         L     RE,AFRSTKEY         UNSET MODIFY FOR NEXT TIME                   
         NI    FHOID(RE),X'FF'-FHOIMO                                           
*                                                                               
FIN030   TM    SYOVCALL,SYOVYESQ   SAVE LAST TIME VALUES IF OLAY CALLED         
         BZ    FIN040                                                           
         OC    TWARCAC,TWARCAC     SAVE LAST TIME VALUES (IF SET)               
         BZ    FIN040                                                           
         MVC   TWALRCAC,TWARCAC                                                 
         SPACE 1                                                                
* IDENTIFY TO OVERLAY THAT SCREEN CURRENTLY LOADED IS 'KNOWN'                   
* TO THE ROOT - IE WAS LOADED BY GENCON/OVERLAY ON SOME EARLIER                 
* TRANSACTION SO OLAY CAN RELY ON IT'S OWN TWA SAVED FIELDS NEXT TRN            
*                                                                               
         OI    CONTAGH+FHIID,FHIIVA                                             
         SPACE 1                                                                
FIN040   EQU   *                                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SYSTEM ROUTINES ENTERABLE FROM CONTROLLER OR OVERLAY                *         
***********************************************************************         
         SPACE 1                                                                
VCOMMON  NTR1  LABEL=NO            NB:- RC MUST BE A(GEND)                      
         ST    RD,COMMRD                                                        
         L     R7,ASYSD            ENSURE R7=A(SYSD)                            
         LM    R8,RB,S#REGS        RESTORE BASE REGS ETC                        
         L     RE,4(RD)            INSERT LABEL IN W/S CHAIN                    
         MVC   0(4,RE),S#VCLTRL    DIFFERENTIATE FROM GENCONS +VCO              
         L     RE,4(RE)            BACK UP TO PREVIOUS W/S CHAIN                
         CLC   0(4,RE),S#VCLTRL    TEST INTERNAL CALL FROM VCOMMON RTN          
         BNE   COMM05                                                           
         IC    RE,SCCALLNO         INCREMENT RECURSION LEVEL                    
         LA    RE,1(RE)                                                         
         STC   RE,SCCALLNO                                                      
         B     COMM10                                                           
         SPACE 1                                                                
COMM05   MVI   SCCALLNO,NULLQ      SET AS OLAY CALL (NO RECURSION)              
         MVC   OVKEYS,KEY          SAVE OVERLAY VALUES                          
         MVC   OVAELEM,SYAELEM                                                  
         MVC   OVAIO,AIO                                                        
         MVC   OVSAVERS,SAVER1     SAVE SAVER1/RE/PARM                          
         MVC   OVERRFLG,SYERRFLG                                                
*                                                                               
COMM10   SRL   RF,24               SHIFT TO GET ROUTINE NUMBER                  
         SLL   RF,2                SHIFT TO GET BRANCH OFFSET                   
         B     CBRANCH(RF)                                                      
         SPACE 1                                                                
*                                  HIGH-ADDREC MUST MAP TO DDSPLWORKD           
CBRANCH  B     VCIORTN             HIGH                                         
         B     VCIORTN             SEQ                                          
         B     VCIORTN             READ                                         
         B     VCIORTN             WRITE                                        
         B     VCIORTN             ADD                                          
         B     VCIORTN             GETREC                                       
         B     VCIORTN             PUTREC                                       
         B     VCIORTN             ADDREC                                       
*                                                                               
         B     VCGETEL             GET FIRST/NEXT ELEMENT                       
         B     VCGETUSR            GET USER ID                                  
         B     VCERRXIT            ERROR EXIT                                   
         SPACE 1                                                                
VCOUNT   EQU   (*-CBRANCH)/4                                                    
         EJECT                                                                  
***********************************************************************         
* VCOMMON EXIT ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
* EXIT POINT TO RETURN A NOT EQUAL (HIGH) CONDITION CODE                        
*                                                                               
VCEXITHI LA    RE,1(RB)            WILL FORCE CC HIGH AT VCEXIT                 
         B     VCEXIT01                                                         
* EXIT POINT TO RETURN A NOT EQUAL (LOW) CONDITION CODE                         
*                                                                               
VCEXITLO EQU   *                                                                
VCEXITNE SR    RE,RE               WILL FORCE CC LOW AT VCEXIT                  
         B     *+6                                                              
         SPACE 1                                                                
* EXIT POINT TO RETURN AN EQUAL CONDITION CODE                                  
*                                                                               
VCEXITOK LR    RE,RB               WILL FORCE CC EQUAL AT VCEXIT                
         SPACE 1                                                                
VCEXIT01 SR    R1,R1               DECREMENT RECURSION LEVEL                    
         ICM   R1,1,SCCALLNO                                                    
         BZ    VCOLAYX             NOT A RECURSIVE CALL                         
         BCTR  R1,0                                                             
         STC   R1,SCCALLNO                                                      
         TM    SYTRNFLG,NORESETQ   TEST PRESERVING TRANSITORY FLAGS -           
         MVI   SYTRNFLG,$RESETQ    NORESETQ ONLY SET ON RECURSIVE CALLS         
         BNZ   VCEXITX                                                          
         B     VCEXIT                                                           
         SPACE 1                                                                
* OVERLAY CALL EXIT                                                             
*                                                                               
VCOLAYX  MVC   KEY(OVKEYSLQ),OVKEYS          RESTORE OVERLAY VALUES             
         MVC   SYAELEM,OVAELEM                                                  
         MVC   AIO,OVAIO                                                        
         MVC   SAVER1(L'OVSAVERS),OVSAVERS   RESTORE SAVER1/RE/PARMS            
         SPACE 2                                                                
VCEXIT   NI    SYERRFLG,X'FF'-NON$ERRQ       RESET TRANSITORY FLAGS             
         NI    SYIOFLG,X'FF'-NON$IOQ                                            
VCEXITX  CR    RE,RB                                                            
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ERROR EXIT ROUTINE - IF SYERRNO SET IT OVERRIDES ERROR              *         
***********************************************************************         
         SPACE 1                                                                
VCERRXIT CLI   SCCALLNO,NULLQ      TEST IF RECURSIVE CALL                       
         BNE   ERRX02                                                           
         TM    OVERRFLG,RETNERRQ   TEST IF OVERLAY HANDLES ERROR                
         BNZ   VCEXITNE            FORCE CC NE                                  
         B     ERRX10                                                           
ERRX02   TM    SYERRFLG,RETNERRQ   TEST IF VCOM ROUTINE WANTS CNTL              
         BNZ   VCEXITNE            FORCE CC NE                                  
         SPACE 1                                                                
ERRX10   MVI   TWAFLID,NULLQ       FORCE BACK TO INITIAL STATE                  
         SPACE 1                                                                
* FORCE INPUT FOR NEXT TRANSACTION IF LIST/SELECT ACTIVE                        
* GENCON WIL NOT CALL OLAY IF NO 'DATA FIELDS' CHANGE BUT GIVES                 
* THE NEXT SELECTION                                                            
*                                                                               
         CLI   ACTNUM,ACTSEL       FORCE INP NEXT TIME IN LIST/SEL              
         BNE   ERRX12                                                           
         OI    CONKEYH+FHOID,FHOIMO+FHOITR                                      
         SPACE 1                                                                
* FORCE KEY MODIFIED IF DISPLAY FOR CHANGE - TO FORCE REDISPLAY ELSE            
* GENCON JUST GIVES VALREC (WITH NO SCREEN FILLED IN)                           
*                                                                               
ERRX12   CLI   TWAACTN,ACTCHA                                                   
         BNE   ERRX14                                                           
         CLI   MODE,DISPREC                                                     
         BNE   ERRX14                                                           
         ICM   R1,15,AFRSTKEY                                                   
         BZ    *+8                                                              
         OI    FHOID(R1),FHOIMO+FHOITR                                          
*                                                                               
ERRX14   TM    GENSTAT2,USGETTXT   TEST CALLER HASN'T SET                       
         BNZ   ERRX30                                                           
         OC    SYERRNO,SYERRNO     TEST IF ERROR IS CTL/PAK                     
         BNZ   ERRX20                                                           
         CLI   ERROR,NULLQ         MAY BE A GENERAL MESSAGE ETC                 
         BNE   *+8                                                              
         MVI   ERROR,INVALID       MAKE SURE THERE IS A ERROR SET               
         CLI   SYFLDIX,NULLQ                                                    
         BE    ERRX30              TAKE GENCON ERROR EXIT                       
*                                                                               
ERRX20   OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         MVI   GETTXTCB+(GTMSYS-GETTXTD),GTGENSYS                               
         MVC   GETTXTCB+(GTMSGNO-GETTXTD)(L'GTMSGNO),SYERRNO                    
         MVC   GETTXTCB+(GTINDX-GETTXTD)(L'GTINDX),SYFLDIX                      
         CLI   TWAMODE,TWAMOFFQ    SET A(MSGTXT) OFFLINE                        
         BNE   *+12 CAN                                                         
         LA    R1,CONHEADH                                                      
         STCM  R1,7,GETTXTCB+(GTAOUT-GETTXTD)                                   
         OC    SYERRNO,SYERRNO                                                  
         BNZ   ERRX30                                                           
         MVC   GETTXTCB+(GTMSGNO1-GETTXTD)(L'GTMSGNO1),ERROR                    
*                                                                               
ERRX30   OC    SYACURSR,SYACURSR                                                
         BZ    *+8                                                              
         ICM   R2,15,SYACURSR                                                   
*                                                                               
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,CONSERVH         ENSURE CURSOR IS ON SCREEN                   
*                                                                               
ERRX40   MVI   TWAERFLG,YESQ       FLAG ERROR FOR NEXT TRANSACTION              
         LR    RF,R2               SAVE OFFSET INTO TWA                         
         SR    RF,RA                                                            
         STCM  RF,15,TWAERFLD                                                   
         GOTO1 ERREX                                                            
         DC    H'0'                ERREX DOESN'T RETURN                         
         EJECT                                                                  
***********************************************************************         
* GENERAL IO ROUTINE TO ALLOW FOR MULTIPLE FILES AND MAXIO CHECKS.    *         
* INTERFACES WITH STD GENCON IO ROUTINES.                             *         
***********************************************************************         
         SPACE 1                                                                
VCIORTN  LR    R3,RF               R3=OFFSET INTO GEND BRANCH TABLE             
         BAS   RE,SETFLID          SET UP FILE CHARACTERISTICS                  
*                                                                               
IORTN05  CLI   OFFLINE,YESQ        TEST FOR MAXIO IF REQUIRED                   
         BE    IORTN10             NO IO COUNT REQUIRED OFFLINE                 
         TM    SYIOFLG,$COUNTQ     TEST IF IO COUNT REQUESTED                   
         BZ    IORTN10                                                          
*                                                                               
         GOTO1 GETFACT,PARAS,(X'80',0),F#TSTATU  GET MAXIO STATUS               
         ICM   RE,15,0(R1)                                                      
         TM    0(RE),TSTATMIO      SKIP IOCHECK IF TRANS CAN EXCEED             
         BNZ   IORTN10                                                          
*                                                                               
         OI    GENSTAT1,CATCHIOR   ENSURE GENCON RETURNS                        
         MVI   ERROR,NULLQ                                                      
         GOTO1 CATCHIOS                                                         
         CLI   ERROR,NULLQ                                                      
         BNE   IORTN20                                                          
         SPACE 1                                                                
IORTN10  LA    RF,HIGH             RF=A(1ST IO ROUTINE ADDRESS)                 
         AR    RF,R3               A(REQUIRED IO ROUTINE ADDRESS)               
         L     RF,0(RF)                                                         
         TM    SYERRFLG,RETNERRQ   CALLER WISHES TO HANDLE ERRORS               
         BZ    *+8                                                              
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         TM    SYIOFLG,RETDELQ     TEST RETURN DELETED RECS                     
         BZ    *+8                                                              
         OI    DMINBTS,X'08'       SET READ DELETES                             
         MVC   S#UPDATE,RDUPDATE   SAVE Y/N READ FOR UPDATE                     
         GOTO1 (RF)                                                             
         MVC   RDUPDATE,S#UPDATE   GENCON SETS TO NO IF APPL CONTROLS !         
*                                                                               
         MVI   ERROPT,NULLQ        RESET GENCON ERROR OPTIONS                   
         NI    DMINBTS,X'FF'-X'08' RESET TO NOT RETURN DELETES                  
         CLI   SCCALLNO,NULLQ      RETURN KEY VALUES IF OVERLAY CALL            
         BNE   *+10                                                             
         MVC   OVKEYS,KEY                                                       
*                                                                               
         TM    SYERRFLG,RETNERRQ   SET CC IF CALLER HANDLES ERROR               
         BZ    VCEXITOK                                                         
         MVC   DUB(1),DMCB+8       DO OWN CHECKING OF IO RETURN                 
         TM    SYIOFLG,RETDELQ     DELETED IS NOT AN ERROR IF REQUESTED         
         BZ    *+8                                                              
         NI    DUB,X'FF'-X'02'                                                  
         NC    DUB(1),DMOUTBTS                                                  
         BNZ   VCEXITNE            IO ERROR (MAY JUST BE NOT FOUND)             
         B     VCEXITOK                                                         
         SPACE 2                                                                
IORTN20  MVI   TWAXIOS,YESQ        FLAG MAXIO EXCEEDED                          
         MVC   SYERRNO,=Y(MAXIOQ)                                               
         B     VCERRXIT                                                         
         EJECT                                                                  
***********************************************************************         
* GET ELEMENT (CODE=ELCODE)  FROM FILE DEFINED BY SYFLID              *         
* ASSUMES ELEMENTS ARE IN ASCENDING SEQUENCE                          *         
* ENTRY - R1=A(IOAREA) ONLY USED FOR 1ST ELEMENT                      *         
*       - ELCODE - CODE OF ELEMENT TO BE RETURNED                     *         
*       - SYAELEM - 1ST ELEMENT IF NULL ELSE NEXT ELEMENT             *         
* EXIT  - SYAELEM A(REQUIRED ELEMENT) IF CC=                          *         
*       - SYAELEM A(PREVIOUS ELEMENT) OR NULL IF CC NE                *         
***********************************************************************         
         SPACE 1                                                                
VCGETEL  ICM   RE,15,SYAELEM                                                    
         BNZ   GTEL020             GET NEXT ELEMENT                             
*                                                                               
         L     RE,0(R1)            RE=A(IO AREA)                                
         SR    RF,RF                                                            
         IC    RF,SYFLID           FIND DISP OF FIRST ELEMENT                   
         BCTR  RF,0                                                             
         MH    RF,=Y(SYSFLENQ)     DISP TO FILE ID TABLE                        
         LA    RF,SYSFLST(RF)                                                   
         SR    R1,R1                                                            
         ICM   R1,3,SYSFDSP-SYSFLSTD(RF)                                        
*                                                                               
GTEL010  AR    RE,R1               RE=A(FIRST/NEXT ELEMENT)                     
         CLI   0(RE),NULLQ         TEST END OF RECORD                           
         BE    VCEXITNE            ELEMENT NOT FOUND RETURN CC NE               
         CLC   0(1,RE),ELCODE                                                   
         BNE   GTEL020                                                          
         ST    RE,SYAELEM                                                       
         CLI   SCCALLNO,NULLQ      OVERWRITE OVERLAY VALUE IF OV CALL           
         BNE   VCEXITOK                                                         
         STCM  RE,15,OVAELEM                                                    
         B     VCEXITOK            RETURN CC= AND A(ELEMENT)                    
*                                                                               
GTEL020  SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         B     GTEL010                                                          
         EJECT                                                                  
***********************************************************************         
* GET USER INFO                                                       *         
***********************************************************************         
         SPACE 1                                                                
VCGETUSR CLI   TWARECT,RECUPLQ     GET PID IF UPLOADING                         
         BNE   GTUSXIT                                                          
         OC    TWAPERID,TWAPERID   ALREADY GOT IT                               
         BNZ   GTUSXIT                                                          
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#UTLD                                    
         L     R1,0(R1)                                                         
         USING F@UTLD,R1                                                        
         CLI   F@TSYS,CTLSYSQ      MUST BE IN CONTROL SYTEM FOR UPDATES         
         BNE   GTUERR01                                                         
*                                                                               
         LA    RF,KEY                                                           
         USING SA0REC,RF                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,F@TAGYSC                                                 
         OC    F@TAGYPE,F@TAGYPE                                                
         BZ    *+10                                                             
         MVC   SA0KAGY,F@TAGYPE                                                 
         MVC   SA0KNUM,F@TPERS                                                  
         MVI   SYFLID,CTFILEQ                                                   
         MVI   USEIO,YESQ                                                       
         GOTO1 VREAD                                                            
         BNE   GTUERR02                                                         
         DROP  R1,RF                                                            
*                                                                               
         MVI   ELCODE,SAPALELQ     FIND PERSONAL ID ELEMENT                     
         XC    SYAELEM,SYAELEM                                                  
         GOTO1 VGETEL,AIO                                                       
         BNE   GTUERR02                                                         
*                                                                               
         ICM   R1,15,SYAELEM                                                    
         MVC   TWAPERID,SAPALPID-SAPALD(R1)                                     
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#SSBD                                    
         L     R1,0(R1)                                                         
         MVC   S#SYSFL,F@SSYSFL-F@SSBD(R1)                                      
         TM    S#SYSFL,X'80'       DON'T VALIDATE PID ON TEST SYSTEMS           
         BO    GTU020                                                           
*                                                                               
GTU015   L     R4,ATWA             SECURITY CHECK FOR INTERNAL CONTROLS         
         AHI   R4,(TWSECBLK-PAKFFD) R4=A(SECRET BLOCK)                          
         USING SECD,R4                                                          
         XC    SECD(256),SECD                                                   
*                                                                               
         XC    FULL,FULL                                                        
         MVI   FULL+0,X'0A'                                                     
         MVI   FULL+1,X'C0'                                                     
*                                                                               
         GOTO1 SECRET,DMCB,('SECPINIT+SECPOSP',SECD),0,(0,FULL)                 
         BNE   GTUERR03            CAN'T INIT THEN UNAUTHORIZED                 
*                                                                               
         CLC   SECOAGPE,MOSECAGY   INTERNAL SECURITY AGENCY ONLY                
         BNE   GTUERR03                                                         
*                                                                               
         NI    SECINDS,X'FF'-SECIOLD TURN OFF OLD SECURITY FOR OVERRIDE         
         GOTO1 SECRET,DMCB,('SECPRACT+SECPOSP',SECD),(1,=F'1'),        +        
               (0,FULL)                                                         
         BNE   GTUERR03            UNAUTHORIZED                                 
         XC    SECD(256),SECD                                                   
*                                                                               
GTU020   XC    SYAELEM,SYAELEM     RESET INIT VALUES                            
         MVC   SYFLID,TWAFLID                                                   
         MVI   USEIO,NOQ           RESET TO IS/DA DEFAULT                       
         BAS   RE,SETFLID                                                       
*                                                                               
GTUSXIT  B     VCEXITOK                                                         
*                                                                               
GTUERR01 MVI   ERROR,CTLERRQ       MUST BE CONNECTED TO CONTROL SYSTEM          
         B     GTUERRX                                                          
*                                                                               
GTUERR02 MVI   ERROR,INVPIDQ       INVALID/MISSING PID                          
         B     GTUERRX                                                          
*                                                                               
GTUERR03 MVC   SYERRNO,=AL2(SECLOCKQ)  SECURITY LOCKOUT                         
         XC    TWAPERID,TWAPERID   FORCE REVALIDATION                           
         XC    SECD(256),SECD      CLEAR SECRET BLOCK                           
         B     GTUERRX                                                          
*                                                                               
GTUERRX  GOTO1 VERRXIT                                                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
* VARIOUS SHORT ROUTINES                                              *         
***********************************************************************         
TMPWRITE LA    RF,DMWRITE                                                       
         B     TMP010                                                           
*                                                                               
TMPREAD  LA    RF,DMREAD                                                        
         CLI   TWAFIRST,TWAOLAYQ   MAY NEED TO READ TEMPSTR                     
         BE    TMP010                                                           
         L     R0,S#ATIA           LEAVE TIA CLEAR 1ST TIME IN                  
         LHI   R1,TWAXTLNQ                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE               NB MVCL LEAVES RE UNCHANGED                  
         CLI   TWAMODE,TWAMOFFQ                                                 
         BER   RE                                                               
         MVI   TWAFIRST,TWAOLAYQ   MAY NEED TEMPSTR FROM NOW ON                 
         BR    RE                                                               
*                                                                               
TMP010   CLI   TWAMODE,TWAMOFFQ                                                 
         BER   RE                                                               
         TM    TWARECFL,GCSVTIAQ   SAVE/RESTORE IF IN USE                       
         BZR   RE                                                               
         MVC   PARAS+20(2),=C'L='  REQUEST 14K PAGES                            
         MVC   PARAS+22(2),=Y(TWAXTLNQ)                                         
         ST    RE,SAVERE                                                        
         SR    R0,R0                                                            
         ICM   R0,3,TWATRM         SET TERMINAL NUMBER                          
         GOTO1 DATAMGR,PARAS,(RF),DMTMPSTR,(4,(R0)),S#ATIA                      
*                                                                               
TMPXIT   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SET FILE DEPENDANT SYSTEM VALUES                                              
*                                                                               
SETFLID  SR    RF,RF               GET FILE ID                                  
         IC    RF,SYFLID                                                        
         CLI   SCCALLNO,NULLQ      ONLY SAVE SYFLID IF OVERLAY CALL             
         BNE   *+8                                                              
         STC   RF,TWAFLID          SAVE LAST USER FILE ID                       
         LTR   RF,RF               TEMP FOR TESTING                             
         BP    *+6                                                              
         DC    H'0'                INVALID FILE ID                              
         BCTR  RF,0                                                             
         MH    RF,=Y(SYSFLENQ)     DISP TO FILE ID TABLE                        
         LA    RF,SYSFLST(RF)                                                   
         USING SYSFLSTD,RF                                                      
         MVC   SYSDIR,SYSFDIR                                                   
         MVC   SYSFIL,SYSFFIL                                                   
         MVC   LKEY,SYSFKEYL                                                    
         MVC   LSTATUS,SYSFSTL                                                  
         MVC   DATADISP,SYSFDSP                                                 
         MVI   USEIO,NOQ                                                        
         CLI   SYFLID,CTFILEQ                                                   
         BNE   *+8                                                              
         MVI   USEIO,YESQ                                                       
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* MOVE FROM DSPWORK TO TWA AND XMIT IF REQUIRED - CLEARING DSPWORK              
**********************************************************************          
         SPACE 1                                                                
GENDSP   ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         IC    RE,FHLND(R1)                                                     
         SHI   RE,(FHDAD+1)                                                     
         TM    FHATD(R1),FHATXH    TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SHI   RE,FHDAD                                                         
         EX    RE,GENCLC           IS DATA ON SCREEN ALREADY                    
         BE    GENDSP10            YES- RETURN (CC=)                            
         EX    RE,GENMVC           NO - SO MOVE IT THERE                        
         OI    FHOID(R1),FHOITR                                                 
GENDSP10 EX    RE,GENCLR                                                        
         L     RE,SAVERE                                                        
         BR    RE                  RETURN (CC NOT=)                             
         SPACE 1                                                                
GENCLC   CLC   FHDAD(0,R1),DSPWORK                                              
GENMVC   MVC   FHDAD(0,R1),DSPWORK                                              
GENCLR   MVC   DSPWORK(0),SPACEPAD                                              
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET ERROR MESSAGE - NOTE THIS ROUTINE IS FOR ERROR EXITS *         
* WHEN GENCON HAS NOT BEEN INVOLVED AND ROOT IS RETURNING TO MONITOR  *         
***********************************************************************         
         SPACE 1                                                                
SETERR   ST    RE,SAVERE                                                        
         OC    TWARCAC,TWARCAC     FORCE INPUT NEXT TRN OF OLAY PRESENT         
         BZ    SETERR05                                                         
         OI    CONKEYH+FHOID,FHOIMO+FHOITR                                      
*                                                                               
SETERR05 LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         CLI   TWAMODE,TWAMOFFQ    SET A(MSGTXT) OFFLINE                        
         BNE   *+12                                                             
         LA    RE,CONHEADH                                                      
         STCM  RE,7,GTAOUT                                                      
         MVI   GTMSYS,GTGENSYS                                                  
         MVC   GTMSGNO,SYERRNO                                                  
         OC    GTMSGNO,GTMSGNO                                                  
         BNZ   SETERR10                                                         
         MVC   GTMSGNO1,ERROR                                                   
         CLI   GTMSGNO1,NULLQ                                                   
         BE    SETERRX                                                          
         DROP  R1                                                               
SETERR10 GOTO1 GETTXT                                                           
         SPACE 1                                                                
SETERRX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL EQUATED VALUES                                              *         
***********************************************************************         
*&&US                                                                           
MOSECAGY DC    CL2'#N'                                                          
*&&                                                                             
*&&UK                                                                           
MOSECAGY DC    CL2'#E'                                                          
*&&                                                                             
                                                                                
***********************************************************************         
* LITERAL POOL FOR MAIN CSECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
       ++INCLUDE GEPAK00TC                                                      
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE GEPAKDS                                                        
         SPACE 2                                                                
* ADDITIONAL DSECTS REQUIRED FOR APPLICATION CONTROLLER ONLY                    
*                                                                               
* CTGENFILE                                                                     
* DDCOREQUS                                                                     
* FASECRETD                                                                     
* FATBHD                                                                        
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FATBHD                                                         
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECTS                                                       *         
***********************************************************************         
         SPACE 1                                                                
PAKFFD   DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE GEPAKFED                                                       
         EJECT                                                                  
***********************************************************************         
* LOCALLY DEFINED DSECTS                                              *         
***********************************************************************         
         SPACE 1                                                                
SYSFLSTD DSECT                     SYSTEM FILE LIST                             
SYSFNUM  DS    AL1                 SYSTEM FILE NUMBER                           
SYSFDIR  DS    CL8                 DIRECTORY NAME                               
SYSFFIL  DS    CL8                 FILE NAME                                    
SYSFKEYL DS    AL2                 KEY LENGTH                                   
SYSFSTL  DS    AL2                 STATUS LENGTH                                
SYSFDSP  DS    AL2                 DISP TO FIRST ELEMENT                        
SYSFLENQ EQU   *-SYSFLSTD                                                       
         SPACE 2                                                                
GCRATABD DSECT                     GENCON RECORD/ACTION TABLE                   
GCRECENT DS    XL1                 TABLE ENTRY TYPE                             
GCRECQ   EQU   X'01'                                                            
GCRECNAM DS    CL8                 EXPANDED RECORD NAME                         
GCRECNUM DS    XL1                 RECORD NUMBER                                
         DS    XL1                 N/D - (DATA DICTIONARY PHASE)                
GCRECACT DS    XL1                 ACTION (ALL RECS HAVE SINGLE ACTION)         
GCSVTIAQ EQU   X'80'               REC TYPE NEEDS TIA SAVED/RESTORED            
GCRECLQ  EQU   *-GCRECENT                                                       
         SPACE 1                                                                
         ORG   GCRATABD            REDEFINE FOR ACTION ENTRIES                  
GCACTENT DS    XL1                 TABLE ENTRY TYPE                             
GCACTQ   EQU   X'02'                                                            
GCACTNAM DS    CL8                 EXPANDED ACTION NAME                         
GCACTNUM DS    XL1                 ACTION NUMBER  (EG ADD)                      
GCACTEQU DS    XL1                 ACTION EQUATE  (EG MAINT)                    
GCACTIND DS    XL1                 INDICATORS                                   
GCACTLQ  EQU   *-GCACTENT                                                       
         ORG   GCRATABD            REDEFINE FOR RECORD/ACTION ENTRIES           
GCCMBENT DS    XL1                 TABLE ENTRY TYPE                             
GCCMBQ   EQU   X'03'                                                            
GCCMBREC DS    XL1                 RECORD NUMBER                                
GCCMBEQU DS    XL1                 ACTION EQUATE (EG MAINT)                     
GCCMBSCR DS    XL1                 PHASE NUMBER FOR SCREEN                      
GCCMBEDT DS    XL1                 PHASE NUMBER FOR EDIT                        
GCCMBSPC DS    XL1                 PHASE NUMBER FOR REPORT SPECS                
GCCMBREP DS    XL1                 PHASE NUMBER FOR REPORT GENERATION           
GCCMBWHN DS    XL1                 WHENOK VALUES                                
GCCMBPRF DS    XL2                 REPORT AND JCL PREFIX                        
GCCMBJCL DS    XL2                 JCL SUFFIX                                   
GCCMBLQ  EQU   *-GCCMBENT                                                       
         SPACE 2                                                                
***********************************************************************         
* VARIOUS EQUATED VALUES                                              *         
***********************************************************************         
         SPACE 1                                                                
* ONLINE TWAFIRST UASGE (OFFLINE IS SPOOFS '1ST FOR REPORT' FLAG)               
TWAFRSTQ EQU   X'00'               1ST TIME IN (NO INPUT REQUIRED)              
TWAOLAYQ EQU   X'02'               TEMPSTR READ MAY BE REQUIRED                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011GEPAK00   07/10/19'                                      
         END                                                                    
