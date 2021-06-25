*          DATA SET GEGEN00    AT LEVEL 008 AS OF 10/08/20                      
*PHASE T00AF0B,*                                                                
*INCLUDE TWABLD                                                                 
GENERAL  TITLE '- GENERALISED ONLINE/OFFLINE CONTROLLER'                        
* TCLE 08 01JUL20 DDLINK SUPPORT: DICTATE ALL FIELDS SPEC-47681                 
* TCLE 07 01JUN20 DDLINK SUPPORT: FIX CHANGE ACTION SPEC-46678                  
* TCLE 06 21FEB18 DDLINK SUPPORT                                                
* TSMY 05 04FEB10 INCLUDE NEW TWABLD                         LO01-9604          
* TCLE 04 02APR09 USE CPU CLOCK DATE IF ONLINE, NOT SSB DATE MR#10769           
* RCRI 03 02APR09 ALLOW SHORT NAME IN HELP                                      
* RCRI 02 02APR09 SET DDS TERMINAL - SET REPORT SECURITY IN REMOTED             
*                                                                               
GENERAL  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN0**,RA,R9,R8,R6,RR=RE                                     
         LR    R7,R1                                                            
         USING WORKD,R7            R7=A(CONTROL WORK AREA)                      
         EJECT                                                                  
***********************************************************************         
* INITIALISE ADDRESSES                                                *         
***********************************************************************         
         SPACE 1                                                                
INITAD   L     R1,ACPARMA                                                       
         MVC   AINP,00(R1)         A(TIOB)                                      
         MVC   ATWA,04(R1)         A(TWA)                                       
         MVC   ASYS,08(R1)         A(SYSTEM FACILITY LIST)                      
         MVC   ATIA,12(R1)         A(TIA)                                       
         MVC   ACOM,16(R1)         A(COMMON FACILITY LIST)                      
         ST    RE,SCRELO                                                        
         ST    RB,SCBASE1                                                       
         ST    RA,SCBASE2                                                       
         ST    R9,SCBASE3                                                       
         ST    R8,SCBASE4                                                       
         ST    R6,SCBASE5                                                       
*                                                                               
         L     R1,ACOM                                                          
         USING COMFACSD,R1                                                      
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VCOLY,CCALLOV                                                    
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VSCUNKEY,CSCUNKEY                                                
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VMINIO,CMINIO                                                    
         MVC   VPARSNIP,CPARSNIP                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   VDICTAT,CDICTATE                                                 
         MVC   VSECRET,CSECRET                                                  
         MVC   VBILLIT,CBILLIT                                                  
         MVC   VPROTON,CPROTON                                                  
         MVC   VPROTOFF,CPROTOFF                                                
*                                                                               
         L     R1,=A(PHASES)       R1=A(PHASE LIST)                             
         A     R1,SCRELO                                                        
         LA    R3,CONLODS          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAXIMUM NUMBER OF PHASES                  
         BAS   RE,LOADPHS                                                       
*                                  SET ADDRESSES IN W/S                         
         ICM   R2,15,ACPHSLST      R2=A(PHASE LIST)                             
         BZ    INITAD8                                                          
         LA    R3,COREFACS         R3=A(ADDRESS LIST)                           
         LA    R4,COREFACN         R4=MAXIMUM NUMBER OF PHASES                  
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,SCPARM                                                        
         L     RF,VCOLY                                                         
INITAD6  CLI   0(R2),X'FF'                                                      
         BE    INITAD8                                                          
         ICM   R0,1,0(R2)                                                       
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,INITAD6                                                       
*                                                                               
INITAD8  LA    R0,CONADDRN         SET CONTROLLER ADDRESSES                     
         LTR   R0,R0                                                            
         BZ    INITAD10                                                         
         SR    RE,RE               RE=INDEX TO ADDRESS VALUE                    
         L     RF,=A(CONADDRS)                                                  
         A     RF,SCRELO                                                        
         L     R1,0(RF,RE)                                                      
         A     R1,SCRELO           RELOCATE AND STORE IN W/S                    
         ST    R1,CONFACS(RE)                                                   
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
INITAD10 ICM   R1,15,ACAROUT       SET COMMON CONTROLLER ROUTINES               
         BZ    INITAD12                                                         
         LA    R0,AROUTN                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ST    R1,AROUTS(RE)                                                    
         STC   RF,AROUTS(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
INITAD12 L     R1,ATWA             SET ADDRESSES OF TWA FIELDS                  
         LA    R1,TWASCR-TWAD(R1)                                               
         ST    R1,AMSGHDR          A(MESSAGE HEADER)                            
         SR    R1,R1                                                            
         ICM   R1,3,ACENDTWA                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         A     R1,ATWA                                                          
         ST    R1,ATWAEND          A(CALLOV POINT FOR SCREEN OVERLAY)           
         OC    ACTWAFLD,ACTWAFLD   SET DEFAULT FIELD MAP NUMBERS                
         BNZ   *+10                                                             
         MVC   ACTWAFLD,=X'01020304'                                            
         CLI   ACTWAFLD,X'FF'      TEST APPL SETS TWA ADDRS                     
         BE    INITADX                                                          
*                                                                               
         LA    R2,ACTWAFLD         R2=A(LIST OF FIELD NUMBERS)                  
         LA    R3,ARECHDR          R3=A(LIST OF TWA ADDRESSES)                  
         LA    R0,L'ACTWAFLD       R0=NUMBER OF TWA ADDRESSES                   
INITAD14 SR    R4,R4                                                            
         ICM   R4,1,0(R2)          R4=RELATIVE TWA FIELD NUMBER                 
         BZ    INITAD18                                                         
         LA    R4,1(R4)            IGNORE SERVICE REQUEST FIELD                 
         L     R1,AMSGHDR          LOOP TO FIND RELATIVE FIELD N                
         L     RF,ATWAEND          RF=A(TAB FIELD)                              
         SR    RE,RE                                                            
INITAD16 TM    1(R1),FVAPROT       TEST PROTECTED FIELD                         
         BNZ   *+16                                                             
         BCT   R4,*+12             NO - LOOP TO RELATIVE UNPROT FIELD           
         ST    R1,0(R3)            SET A(FIELD HEADER)                          
         B     INITAD18                                                         
         ICM   RE,1,0(R1)          BUMP TO NEXT TWA FIELD                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BXLE  R1,RE,INITAD16                                                   
         DC    H'0'                                                             
INITAD18 LA    R3,4(R3)            BUMP TO NEXT ADDRESS                         
         LA    R2,1(R2)            BUMP TO NEXT FIELD NUMBER                    
         BCT   R0,INITAD14                                                      
INITADX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE GENERAL VALUES                                           *         
***********************************************************************         
         SPACE 1                                                                
INITGE   L     R1,ACPARMA          R1=A(FACPAK PARAMETER LIST)                  
         MVC   CUABIN,0(R1)        AGENCY BINARY VALUE                          
         MVI   IOSWSE,0            SET NO USER SUPPLIED SE #                    
         L     R5,ATWA             R5=A(TWA)                                    
         USING TWAD,R5                                                          
         MVC   CUTRMN,TWATRM       FACPAK TERMINAL NUMBER                       
         MVC   CUACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   CUUSER,TWAUSRID     USER-ID NUMBER                               
         MVC   CUAUTH,TWAAUTH      AUTHORIZATION CODE                           
         MVC   CUAALF,TWAAGY       AGENCY ALPHA-ID                              
         MVC   SCOMODE,TWAOMODE    SAVE OFFLINE MODE FROM SPOOF                 
         MVC   SCTABNAM,=C'CORETAB'                                             
         MVC   SCDICONE,=C'SU  '                                                
         MVC   SCDICMLT,=C'TU  '                                                
         MVCDD SCHELP,GE#HELP      GET EXPANDED HELP WORD                       
         GOTO1 VDICTAT,SCPARM,SCDICONE,SCHELP,0                                 
         GOTO1 VGETFACT,SCPARM,0                                                
         L     R2,0(R1)                                                         
         USING FACTSD,R2           R1=A(SYSTEM DEFINITION BLOCK)                
         L     RF,FASYSLST         LOCATE SYSTEM LIST ENTRY                     
         ST    RF,ASYSLST          SET A(SYSTEM NAME TABLE)                     
         MVC   ACTRY,FAACTRY       SET A(COUNTRY & LANGUAGE TABLES)             
         MVC   ALANG,FAALANG                                                    
         MVI   ASONOFF,ASON        SET ONLINE OR OFFLINE SWITCH                 
         MVC   CUAGYSEC,FATAGYSC   SECURITY AGENCY                              
                                                                                
INITGEX  TM    FATFLAG,1           TEST OFFLINE                                 
         BZ    INITON                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR OFFLINE PROCESSING                                   *         
***********************************************************************         
         SPACE 1                                                                
INITOF   L     R1,=A(PHASOFF)      R1=A(OFFLINE PHASE LIST)                     
         A     R1,SCRELO                                                        
         LA    R3,CONLDOFF         R3=A(ADDRESS LIST)                           
         LA    R4,PHASOFFN         R4=MAXIMUM NUMBER OF PHASES                  
         BAS   RE,LOADPHS                                                       
         MVI   ASONOFF,ASOFF       SET OFFLINE & EXTRACT VALUES                 
         MVI   CUOFFC,C' '                                                      
         MVC   CUPASS,FAPASSWD                                                  
         ZAP   ASTIME,=PL4'0'                                                   
         L     R1,TWAOVMST                                                      
         USING MASTD,R1            EXTRACT MASTER CONTROLLER VALUES             
         MVC   CUUSER,MCORIGID     USER-ID NUMBER                               
         MVC   CULANG,MCLANG                                                    
         MVC   CUCTRY,MCCTRY                                                    
         MVC   SCAUTL,MCUTL                                                     
         MVC   ASEDAT,MCDATE                                                    
*                                                                               
         TM    MCRHINFO,MCRHONL+MCRHDDS                                         
         BZ    *+12                                                             
         MVI   CUOFFC,C'*'         SET DDS TERMINAL OFFICE CODE                 
         OI    CUSTAT,CUSDDS                                                    
         XC    CUADDR,CUADDR       NO TERMINAL LU OFFLINE                       
*                                                                               
         L     RF,=A(SYSLIST)      LOCATE OFFLINE SYSTEM ENTRY                  
         A     RF,SCRELO                                                        
INITOF2  CLI   0(RF),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID SYSTEM (UPDATE SYSLIST)              
         CLC   MCSYSTEM,0(RF)      MATCH ON SYSTEM CHARACTERS                   
         BE    *+12                                                             
         LA    RF,L'SYSLIST(RF)    BUMP TO NEXT TABLE ENTRY                     
         B     INITOF2                                                          
         MVC   ASSYSO,2(RF)        SET LOGICAL SYSTEM NUMBER                    
         DROP  R1                                                               
*                                                                               
         GOTO1 VDATCON,SCPARM,(4,ASEDAT),(3,ASBDAT)                             
*                                                                               
         LA    R1,IOKEY            BUILD KEY OF ID RECORD                       
         USING CTIREC,R1                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         MVC   IOADDR,ACIOADD      SET TO USE FIRST USER I/O AREA               
         GOTO1 AIO,IORD+IOCTFILE                                                
         BNE   INITOF8                                                          
         L     R1,ACIOADD                                                       
         LA    R1,CTIDATA                                                       
         USING CTSYSD,R1           R1=A(FIRST ELEMENT)                          
         SR    R0,R0                                                            
         LA    RE,ASSWTAB          RE=A(SYSTEM SWITCH TABLE)                    
         USING SYSSWTAB,RE                                                      
INITOF4  CLI   CTSYSEL,0           TEST E-O-R                                   
         BE    INITOF8                                                          
         CLI   CTSYSEL,CTSYSELQ    TEST SYSTEM ELEMENT                          
         BNE   INITOF6                                                          
         MVC   SYSSWSYS,CTSYSSE    BUILD SWITCH TABLE ENTRY                     
         MVC   SYSSWSOV,CTSYSNUM                                                
         MVC   SYSSWAGB,CTSYSAGB                                                
         MVC   SYSSWACS,CTSYSLMT                                                
         LA    RE,SYSSWLEN(RE)     BUMP TO NEXT TABLE ENTRY                     
INITOF6  IC    R0,CTSYSLEN         BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     INITOF4                                                          
*                                                                               
INITOF8  L     R1,ACOM             EXTRACT A(REQTWA) FROM COMFACS               
         MVC   VREQTWA,CREQTWA-COMFACSD(R1)                                     
*                                                                               
INITOFX  B     INITGV                                                           
         DROP  R1,RE                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR ONLINE PROCESSING                                    *         
***********************************************************************         
         SPACE 1                                                                
INITON   MVC   CUCTRY,FACTRY       SET COUNTRY/LANGUAGE                         
         MVC   CULANG,FALANG                                                    
         MVC   CUADDR,FALINE       TERMINAL LINE/ADDRESS                        
         MVC   ASEDAT,FADATE       SYSTEM DATES (VARIOUS FORMATS)               
         MVC   ASBDAT,FADATEB      OVERRIDEN LATER BY DATCON IN UK              
         MVC   CUJOBI(L'CUJOBI+L'CUOFFC),FAOFFICE                               
         CLI   CUOFFC,C'*'         TEST DDS OFFICE CODE                         
         BNE   *+8                                                              
         MVI   CUSTAT,CUSDDS       SET DDS STATUS INDICATOR                     
         TM    FATSTAT6,X'80'      TEST RUNNING UNDER PC PAK                    
         BZ    *+8                                                              
         OI    CUSTAT,CUSPCP       YES - USER CONNECTED WITH PC PAK             
         OC    CUPASS,FAPASSWD     TEST CONNECTED WITH PASSWORD                 
         BZ    INITON2                                                          
         TM    FATFLAG,X'08'       YES - TEST PERSONAL PASSWORD                 
         BZ    *+12                                                             
         OI    CUSTAT,CUSPER                                                    
         B     INITON2                                                          
         OI    CUSTAT,CUSTRM       NO - MUST BE TERMINAL PASSWORD               
INITON2  MVC   ASTIME,FATIME       SYSTEM TIME (STANDARD FORMAT)                
         MVC   ASSYSN,FASYS        SYSTEM NUMBER                                
         MVC   ASSYSO,FAOVSYS      SYSTEM NUMBER (FOR CALLOV)                   
         MVC   ASSIN,FASIN         SYSTEM INPUT NUMBER                          
         MVC   ASIOASTR,FAIOASTR   SYSTEM EXTRA AREA ADDRESS                    
         MVC   ASIOALEN,FAIOALEN   SYSTEM EXTRA AREA LENGTH                     
         DROP  R2                  DROP GETFACT BLOCK                           
*&&UK                                                                           
         GOTO1 VDATCON,SCPARM,(5,0),(3,ASBDAT)                                  
         GOTO1 (RF),(R1),(3,ASBDAT),(10,ASEDAT)                                 
*&&                                                                             
         MVC   SCPARM(4),EFFS      SET A(UTL ENTRY)                             
         GOTO1 VSWITCH,SCPARM                                                   
         MVC   SCAUTL,0(R1)                                                     
*                                                                               
         GOTO1 VGETFACT,SCPARM,(X'80',0),F#TCBD                                 
         L     R1,0(R1)            EXTRACT TCB INFO                             
*                                                                               
         USING F@TCBD,R1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,F@BSWNUM       R0=N'ENTRIES IN TCBSWTAB                     
         BZ    INITON6                                                          
         LA    R1,F@BSWTAB                                                      
         USING F@BSWTAB,R1         R1=A(TCB SWITCH TABLE)                       
         LA    RE,ASSWTAB                                                       
         USING SYSSWTAB,RE         RE=A(LOCAL SWITCH TABLE)                     
INITON4  MVC   SYSSWSYS,F@BSWSYS                                                
         MVC   SYSSWSOV,F@BSWSOV                                                
         MVC   SYSSWAGB,F@BSWAGB                                                
         MVC   SYSSWACS,F@BSWACS                                                
         LA    R1,F@BSWLEN(R1)     BUMP TO NEXT                                 
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,INITON4          DO FOR NUMBER OF ENTRIES                     
         DROP  R1,RE                                                            
*                                                                               
INITON6  L     R1,AINP             ADJUST PFKEY NUMBER                          
         SR    RE,RE                                                            
         IC    RE,TIOBAID-TIOBD(R1)                                             
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         OI    SCINDS,SCIANYPF     SET USER ENTERED PF KEY THIS TIME            
         LA    RF,PFK12            PF'S 13 THRU 24 BECOME 1 THRU 12             
         TM    ACINDS,ACI24PFK     TEST 24 PFKEY SUPPORT                        
         BZ    *+8                                                              
         LA    RF,24                                                            
         CR    RE,RF                                                            
         BNH   *+6                                                              
         SR    RE,RF                                                            
         STC   RE,SCPFKEY                                                       
         STC   RE,APPFKEY                                                       
*                                                                               
         TM    TWAMODE,TWAMNXT     NO - TEST FIRST FOR SESSION                  
         BZ    INITONX                                                          
         LH    R1,=Y(RECVALSL)     NO - RESTORE SAVED RECORD VALUES             
         LA    R0,RECVALS                                                       
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
INITONX  B     INITGLOB                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK IF WE WERE GLOBBED TO AND TEST DDLINK UPLOAD CALL (ONLINE)    *         
***********************************************************************         
         SPACE 1                                                                
INITGLOB LAY   R2,GLOBSAVE                                                      
         XC    0(L'GLOBSAVE,R2),0(R2) CLEAR GLOBBER SAVE AREA                   
         XC    SCDDLBLK,SCDDLBLK   CLEAR DDLINK CONTROLS                        
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         LA    R1,SCPARM                                                        
         GOTO1 (RF),(R1),=C'GETD',SCWORK,24,GLVXCTL,0                           
         CLI   8(R1),GLEGNF                                                     
         BE    INITGLOX            NOT GLOBBER CALLED (SO NOT DDLINK)           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         LA    RE,SCWORK                                                        
         USING GLVXFRSY,RE                                                      
         USING GLOBSAVE,R2                                                      
         MVC   GLOBINSY,GLVXTOSY   SAVE DETAILS OF CALLED SYS/PROG              
         MVC   GLOBINPR,GLVXTOPR   (I.E. US)                                    
         MVC   GLOBFRSY,GLVXFRSY   SAVE DETAILS OF CALLING SYS/PROG             
         MVC   GLOBFRPR,GLVXFRPR                                                
         MVC   GLOBFLG1,GLVXFLG1                                                
         DROP  RE,R2                                                            
*                                                                               
         GOTOX (RF),(R1),=C'GETD',ACIOADD,10,GLVDLUWF                           
         CLI   8(R1),GLEGNF        TEST WORKER FILE GLOBAL PRESENT              
         BNE   INITGLO4            YES, SKIP (LINKIO CLEARS ALL GLVARS)         
*                                                                               
         GOTO1 (RF),(R1),=C'DELE',,,GLVXCTL,0 DELETE XCTL ENTRY                 
         J     INITGLOX                                                         
*                                                                               
INITGLO4 CLI   8(R1),0             MAKE SURE NO ERROR                           
         JNE   *+2                                                              
*                                                                               
         OI    CUSTAT,CUSDDL       SET DDLINK IN CONTROL                        
*                                                                               
         L     R2,ATIA             LIOB GOES IN TIA                             
         USING LIOBD,R2                                                         
*                                                                               
         LR    R0,R2               CLEAR THE TIA                                
         SR    R1,R1                                                            
         LHI   R1,18*1024          FOR THE LENGTH OF THE TIA                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LR    R0,R2               SET UP TO GET R/T AND ACTION                 
         AHI   R0,L'LIOB                                                        
         ST    R0,LIOBAREC                                                      
         AHI   R0,4*1024-L'LIOB                                                 
         ST    R0,LIOBABUF                                                      
         LA    R0,LINKMAP                                                       
         ST    R0,LIOBAMAP                                                      
         LA    R0,WORKD                                                         
         ST    R0,LIOBASB1         ????                                         
         MVC   LIOBACOM,ACOM                                                    
         MVI   LIOBMSYS,X'FF'      ????                                         
         MVI   LIOBINDS,LIOBISUB                                                
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),SCPARM,('LIOAINI',LIOBD)                                    
         GOTOR (RF),(R1),('LIOAGET',LIOBD)                                      
*                                                                               
W        USING FAWSSVRD,SCPARM     SAVE CURRENT LIOB DATA                       
         XC    W.FAWSSVRD(FAWSSVRL),W.FAWSSVRD                                  
         LARL  RE,LIOBTOKN                                                      
         MVC   W.FAWSTOKN,0(RE)                                                 
         ST    R2,W.FAWSADR                                                     
         MVC   W.FAWSLEN,=AL2(LIOBTLEN)                                         
         MVI   W.FAWSACTN,FAWSASVE                                              
         L     RF,ACOM                                                          
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),W.FAWSSVRD                                                  
         CLI   W.FAWSRTN,0                                                      
         JNE   *+2                 DIE IF CAN'T SAVE                            
         DROP  W,R2                                                             
*                                                                               
INITGLOX B     INITGV                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE OTHER GLOBAL VALUES                                      *         
***********************************************************************         
         SPACE 1                                                                
INITGV   CLI   CULANG,0            SET DEFAULT LANGUAGE                         
         BNE   *+8                                                              
*&&UK*&& MVI   CULANG,LANGEUK                                                   
*&&US*&& MVI   CULANG,LANGEUS                                                   
         SR    R1,R1                                                            
         IC    R1,CULANG                                                        
         SLL   R1,2                                                             
         L     R0,=A(LANGCHAR-4)                                                
         A     R0,SCRELO                                                        
         AR    R1,R0                                                            
         MVC   SCCHARS,0(R1)       SET SPECIAL CHARACTERS                       
         CLI   SCDELIM,0                                                        
         BNE   *+10                                                             
         MVC   SCDELIM,SCCOMMA     SET KEY DELIMITER DEFAULT                    
         CLI   CUCTRY,0            SET DEFAULT COUNTRY                          
         BNE   *+8                                                              
*&&UK*&& MVI   CUCTRY,CTRYGBR                                                   
*&&US*&& MVI   CUCTRY,CTRYUSA                                                   
         L     RF,ASYSLST          A(SYSTEM NAME TABLE)                         
         LA    RF,6(RF)            FIND ENTRY FOR CONNECTED SYSTEM              
         USING SYSLSTD,RF          RF=A(SYSTEM TABLE)                           
INITGV2  CLI   SYSLNUM,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ASSYSO,SYSLNUM      MATCH ON OVSYS NUMBER                        
         BE    *+12                                                             
         LA    RF,SYSLLEN(RF)                                                   
         B     INITGV2                                                          
         MVC   ASNAME,SYSLNAME     SYSTEM NAME                                  
         MVC   ASSHRT,SYSLSHRT     SYSTEM SHORT NAME                            
         MVC   ASSYSE,SYSLEQU      SYSTEM EQUATE VALUE                          
         MVC   ASRPLT,SYSLRPLT     SYSTEM SUNDRY VALUES                         
         MVC   ASUSLT,SYSLUSLT                                                  
         MVC   ASIND1,SYSLIND1                                                  
         MVC   ASIND2,SYSLIND2                                                  
*                                                                               
         MVC   SCSWSYSN,ASSYSO     SET SWITCHED SYSTEM INFO                     
         MVC   SCSWSYSC,ASSYSO                                                  
         MVC   SCSWSYSP,ASSYSO                                                  
*                                                                               
         GOTO1 VDATCON,SCPARM,(3,ASBDAT),(1,ASPDAT)                             
         GOTO1 (RF),(R1),(3,ASBDAT),(2,ASCDAT)                                  
*                                                                               
         L     R1,AFILTAB          FIND FILE TABLE ENTRY (FOR IOEX)             
         SR    RE,RE                                                            
INITGV4  CLI   0(R1),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,R1),ASSYSO      MATCH ON OVSYS NUMBER                        
         BE    *+16                                                             
         ICM   RE,3,4(R1)                                                       
         LA    R1,5(RE,R1)                                                      
         B     INITGV4                                                          
         LA    R1,6(R1)                                                         
         ST    R1,AFILNTRY         SAVE A(FILE TABLE ENTRY)                     
*                                                                               
         CLI   ACFSTIND,0          TEST CONTROLLER HOOK REQUIRED                
         BE    INITGVX                                                          
         GOTO1 ACHOOK,ACMFRST                                                   
         BNE   FVERR                                                            
*                                                                               
INITGVX  B     VALREC                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD TYPE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALREC   TM    ACRECIND,ACHKBEF    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALREC1                                                          
         GOTO1 ACHOOK,ACMRECR                                                   
         BNE   FVERR                                                            
*                                                                               
VALREC1  TM    ACRECIND,ACNOVAL    TEST SKIP VALIDATION                         
         BNZ   VALRECX                                                          
         L     R2,ACRECTAB                                                      
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    VALREC4             YES, MATCH ON RECORD NUMBER                  
         MVI   FVHELP,HELPREC      RECORD TYPE HELP REQUIRED                    
         MVI   FVMINL,1            SET FIELD IS OPTIONAL                        
         MVI   FVMAXL,L'SCRECNAM                                                
         GOTO1 SCFVAL,ARECHDR                                                   
         MVI   SCRECSN,0           FLAG SHORT NAME PASS REQUIRED                
         NI    SCINDS,X'FF'-SCINOACC                                            
VALREC4  ZIC   R4,FVXLEN           R4=L'INPUT-1                                 
VALREC6  CLI   RECTABD,EOT         TEST E-O-T (I.E. NOT FIRST PASS)             
         BNE   VALREC8                                                          
*                                  NON-FIRST PASS LOGIC                         
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    VALREC6D            YES, SKIP                                    
         CLI   FVXLEN,2            TEST MORE THAN 3 CHRS MATCH                  
         BH    VALREC7             SET TO MATCH 3 FOR SECOND PASS               
         CLI   SCRECSN,0           TEST MATCHED SHORT NAME                      
         BE    VALREC6A                                                         
VALREC6D MVC   FVMSGNO,=AL2(FVFNOTV) NO - SET FIELD INVALID ERROR               
         TM    SCINDS,SCINOACC                                                  
         BZ    FVERR                                                            
         MVC   FVMSGNO,=AL2(FVFNAREC) OR NO AUTHORIZATION FOR RECORD            
         L     R1,ARECNTRY                                                      
         GOTO1 AEXPREC,(R1)                                                     
         L     R1,ARECHDR                                                       
         MVC   L'FVIHDR(L'SCRECNAM,R1),SCRECNAM                                 
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         B     FVERR                                                            
VALREC6A MVI   SCRECSN,X'FF'       SET TO MATCH SHORT NAME THIRD PASS           
         B     *+8                 LEAVE L'INPUT AS IS (3 OR LESS)              
VALREC7  MVI   FVXLEN,2            FORCE 3 CHRS MATCH ON FULL NAME              
         L     R2,ACRECTAB                                                      
         B     VALREC4                                                          
*                                                                               
VALREC8  SR    RE,RE               APPLY COUNTRY FILTER (IF ANY)                
         ICM   RE,1,ACRECNDX                                                    
         BZ    VALREC9                                                          
         LA    RE,RECUSER-1(RE)    ACRECNDX IS 1 BASED VALUE                    
         CLI   0(RE),0             TEST VALID FOR ALL COUNTRIES                 
         BE    VALREC9                                                          
         CLC   CUCTRY,0(RE)        MATCH ON COUNTRY CODE                        
         BNE   VALREC12                                                         
*                                                                               
VALREC9  GOTO1 AEXPREC,RECTABD     EXPAND RECORD NAMES                          
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    VALREC9A                                                         
         CLC   SCDDLREC,RECNUMB    YES, WE ONLY HAVE THE RECORD NUMBER          
         BE    VALREC11            IT MATCHES                                   
         B     VALREC12            ELSE NEXT ENTRY                              
*                                                                               
VALREC9A CLI   SCRECSN,X'FF'       TEST IF MATCHING ON SHORT NAME               
         BE    VALREC10                                                         
         EX    R4,*+8                                                           
         BE    VALREC11                                                         
         CLC   SCRECNAM(0),FVIFLD  MATCH INPUT AGAINST FULL NAME                
         B     VALREC12            SET FOR NEXT ENTRY                           
*                                                                               
VALREC10 LA    RF,L'SCRECSHT       FIND LENGTH OF SHORT NAME                    
         LA    R1,SCRECSHT+L'SCRECSHT-1                                         
         CLI   0(R1),C' '                                                       
         BH    *+14                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         B     VALREC12            NO SHORT NAME                                
         BCTR  RF,0                                                             
         CLM   RF,1,FVXLEN         MUST BE FULL LENGTH                          
         BH    VALREC12                                                         
         EX    RF,*+8                                                           
         BNE   VALREC12                                                         
         CLC   FVIFLD(0),SCRECSHT  MATCH ON FULL LENGTH OF SHORT NAME           
*                                  TEST IF THIS ENTRY IS VALID                  
VALREC11 TM    RECINDS,RECINOH     TEST NO HELP FOR RECORD                      
         BNZ   VALREC12                                                         
         GOTO1 TSTAUTH,RECAUTH     TEST USER AUTHORISED                         
         BNE   VALREC12                                                         
         TM    RECINDS,RECIDDS     TEST DDS ONLY RECORD TYPE                    
         BZ    *+16                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALREC12                                                         
         B     VALREC14                                                         
         GOTO1 TSTSECR,RECNUMB                                                  
         BE    VALREC14                                                         
         ST    R2,ARECNTRY         SAVE A(ENTRY) IF ACCESS NOT ALLOWED          
VALREC12 SR    R0,R0               BUMP TO NEXT TABLE ENTRY                     
         IC    R0,RECELEN                                                       
         AR    R2,R0                                                            
         B     VALREC4                                                          
*                                                                               
VALREC14 ST    R2,ARECNTRY         SET A(RECORD TYPE TABLE ENTRY)               
         MVC   INREC,RECNUMB       EXTRACT TABLE VALUES INTO W/S                
         MVC   INOPTR,RECOPTR                                                   
         MVC   INOPTX,RECOPTX                                                   
         L     R1,ARECHDR                                                       
         CLC   SCRECNAM,L'FVIHDR(R1)                                            
         BE    *+14                                                             
         MVC   L'FVIHDR(L'SCRECNAM,R1),SCRECNAM                                 
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         CLC   INREC,TWALREC       TEST CHANGE OF RECORD TYPE                   
         BE    *+8                                                              
         NI    TWAMODE,TWAMNXT     YES - RESET MODE                             
*                                                                               
VALRECX  TM    ACRECIND,ACHKAFT    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALACT                                                           
         GOTO1 ACHOOK,ACMRECR                                                   
         BNE   FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION FIELD & RECORD/ACTION COMBINATION                   *         
***********************************************************************         
         SPACE 1                                                                
VALACT   TM    ACACTIND,ACHKBEF    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALACT1                                                          
         GOTO1 ACHOOK,ACMACTR                                                   
         BNE   FVERR                                                            
*                                                                               
VALACT1  TM    ACACTIND,ACNOVAL    TEST SKIP VALIDATION                         
         BNZ   VALACT34                                                         
         L     R2,ACACTTAB                                                      
         USING ACTTABD,R2          R2=A(RECORD TYPE TABLE)                      
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    VALACT4             YES, MATCH ON ACTION NUMBER                  
         MVI   FVHELP,HELPACT      ACTION HELP REQUIRED                         
         MVI   FVMINL,1            SET FIELD IS OPTIONAL                        
         MVI   FVMAXL,L'SCACTNAM                                                
         GOTO1 SCFVAL,AACTHDR                                                   
         NI    SCINDS,X'FF'-SCINOACC                                            
*                                                                               
         CLC   FVIFLD(4),=C'PFM '                                               
         BNE   VALACT2                                                          
         LA    RF,SAVAREA                                                       
         L     R1,AACTHDR                                                       
         MVC   8(8,R1),SAVACTN-SAVAREA(RF)                                      
         BAS   RE,PFMGO                                                         
         B     EXIT                                                             
*                                  MATCH ON KEYWORD INPUT                       
VALACT2  LA    RF,SAVAREA                                                       
         MVC   SAVACTN-SAVAREA(8,RF),FVIFLD                                     
*                                  MATCH ON KEYWORD INPUT                       
         ZIC   R4,FVXLEN           R4=L'INPUT-1                                 
VALACT4  CLI   ACTTABD,EOT         TEST E-O-T                                   
         BNE   VALACT8                                                          
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    VALACT5             YES, SKIP                                    
         CLI   FVXLEN,2            TEST MORE THAN 3 CHRS MATCH                  
         BNH   *+16                                                             
         MVI   FVXLEN,2            YES - SET TO DO 3 CHRS MATCH                 
         L     R2,ACACTTAB                                                      
         B     VALACT2                                                          
VALACT5  MVC   FVMSGNO,=AL2(FVFNOTV) NO - SET INVALID ACTION                    
         ICM   R1,15,AACTNTRY                                                   
         BZ    FVERR                                                            
         GOTO1 AEXPACT,(R1)                                                     
         L     R1,AACTHDR                                                       
         MVC   L'FVIHDR(L'SCACTNAM,R1),SCACTNAM                                 
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         MVC   FVMSGNO,=AL2(FVFMIXI) OR INVALID RECORD/ACTION COMBO             
         TM    SCINDS,SCINOACC                                                  
         BZ    FVERR                                                            
         MVC   FVMSGNO,=AL2(FVFNAACT) OR NOT AUTHORISED FOR ACTION              
         B     FVERR                                                            
*                                  TEST IF THIS ENTRY IS VALID                  
*                                                                               
VALACT8  GOTO1 AEXPACT,ACTTABD     EXPAND ACTION NAME                           
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    VALACT9                                                          
         CLI   ACTNUMB,ACTRES      YES, WE ONLY HAVE THE ACTION NUMBER          
         BH    VALACT13            AND ALLOW ONLY ADD,DIS.CHA,DEL,RES           
         CLC   SCDDLACT,ACTNUMB                                                 
         BE    VALACT10            IT MATCHES                                   
         B     VALACT13            ELSE NEXT ENTRY                              
*                                                                               
VALACT9  EX    R4,*+8                                                           
         BNE   VALACT13                                                         
         CLC   SCACTNAM(0),FVIFLD  MATCH ON INPUT NAME                          
*                                  TEST IF THIS ENTRY IS VALID                  
VALACT10 TM    ACTINDS,ACTIDDS     TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALACT13                                                         
         GOTO1 TSTAUTH,ACTAUTH     TEST USER AUTHORISED                         
         BNE   VALACT13                                                         
         MVC   SCRECN,INREC                                                     
         MVC   SCACTN,ACTNUMB                                                   
         ST    R2,AACTNTRY                                                      
VALACT11 BAS   RE,VALSUBM          VALIDATE RECORD/ACTION COMBO                 
         BNE   VALACT13                                                         
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3          R3=A(RECORD ACTION TABLE ENTRY)              
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    VALACT11A                                                        
         TM    MIXINDS,MIXILFM     YES, MUST BE MAINTENANCE ACTION              
         BZ    VALACT13                                                         
         B     VALACT14                                                         
VALACT11A TM    MIXINDS,MIXILST+MIXISEL                                         
         BNZ   VALACT12                                                         
         TM    TWAMODE,TWAMLSM     TEST LSM ACTIVE                              
         BZ    VALACT12                                                         
         NI    TWAMODE,255-TWAMLSM REVALIDATE AS NON-LIST ACTION                
         MVI   TWALACT,0           FORCE CHANGE OF ACTION                       
         B     VALACT11                                                         
VALACT12 TM    MIXINDS,MIXISEL     TEST THIS IS A SELECT ACTION                 
         BZ    *+12                                                             
         TM    TWAMODE,TWAMSEL     YES - TEST IN SELECT MODE                    
         BZ    VALACT13                                                         
         B     VALACT14                                                         
VALACT13 LA    R2,ACTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALACT2                                                          
*                                  EXTRACT ACTION VALUES INTO W/S               
VALACT14 ST    R2,AACTNTRY         SET A(ACTION TABLE ENTRY)                    
         MVC   INACT,ACTNUMB       EXTRACT TABLE VALUES INTO W/S                
         MVC   INACTLFM,ACTUSER                                                 
         OC    INOPTR,ACTOPTR                                                   
         OC    INOPTX,ACTOPTX                                                   
         L     R1,AACTHDR                                                       
         CLC   SCACTNAM,L'FVIHDR(R1)                                            
         BE    *+14                                                             
         MVC   L'FVIHDR(L'SCACTNAM,R1),SCACTNAM                                 
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
*                                                                               
         CLI   MIXOVER,0           TEST OVERLAY NUMBER SET                      
         BNE   VALACT18                                                         
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MONITOR ACTIVE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SCRECN,TWALSREC                                                  
         MVC   SCACTN,TWALSACT                                                  
         BAS   RE,VALSUBR          LOCATE RECORD TABLE ENTRY                    
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,VALSUBA          LOCATE ACTION TABLE ENTRY                    
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,VALSUBM          LOCATE COMBO ENTRY FOR RECORD/ACTION         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AMIXNTRY                                                      
         MVI   INMIX1,MIXISEL      SET SELECT ACTION                            
*                                                                               
VALACT18 OC    INOPTR,MIXOPTR                                                   
         OC    INOPTX,MIXOPTX                                                   
         OC    INMIX1,MIXINDS      SAVE RECORD/ACTION INDICATORS                
         OC    INMIX2,MIXINDS2                                                  
         MVC   INSCRN,MIXSCRN      SAVE SCREEN/OVERLAY NUMBERS                  
         MVC   INOVER,MIXOVER                                                   
         MVC   INKEYN,MIXKEYN      SAVE KEY VALIDATION FIELDS                   
         MVC   INKEYC,MIXKEYC                                                   
         MVC   INKEYT,MIXKEYT                                                   
         TM    MIXINDS,MIXIREP     TEST REPORT OVERLAY                          
         BZ    *+10                                                             
         MVC   INREPORT,MIXUSER    YES - SET SET REPORT VALUES                  
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE MODE                            
         BNE   VALACT22                                                         
         CLI   SCOMODE,TWAOLOAD                                                 
         BE    VALACT24            LOAD SCREEN & EXIT                           
         B     VALACT26            LOAD PHASE & PROCESS                         
*                                                                               
VALACT22 TM    INMIX2,MIXIDRV      ONLINE - TEST DROOL ACTION                   
         BZ    *+10                                                             
         MVC   INDRLIND,MIXINDRL   YES- SET DROOL INDICATOR                     
         CLC   TWASCRN,INSCRN      LOAD SCREEN IF REQUIRED                      
         BNE   VALACT24                                                         
         TM    INMIX1,MIXILST      TEST LIST/SELECT                             
         BZ    VALACT26                                                         
         CLC   TWALACT,INACT       YES - CHECK ACTION TOO                       
         BE    VALACT26                                                         
         MVI   TWASCRN,0           FORCE NO SCREEN MATCH THIS TIME              
*                                                                               
VALACT24 CLI   INSCRN,0            TEST SCREEN LOAD REQUIRED                    
         BNE   *+14                                                             
         MVC   INSCRN,TWASCRN      NO - SET SCREEN NUMBER                       
         B     VALACT26                                                         
         TM    TWALSCTL,TWALSMSV   SHOULD WE SAVE LSM MODE ?                    
         BZ    VALACT25            NO,  CLEAR LSM MODE                          
         TM    INMIX1,MIXISEL      IN   SELECT ACTION ?                         
         BZ    VALACT25            NO,  CLEAR LSM MODE                          
         NI    TWAMODE,TWAMNXT+TWAMLSM+TWAMSEL                                  
         OI    INFLAG1,INFDISR     FORCE DISPLAY RECORD AFTER VALREC            
         B     VALAC25A                                                         
*                                                                               
VALACT25 NI    TWAMODE,TWAMNXT                                                  
         XC    SAVRECK,SAVRECK                                                  
*                                                                               
VALAC25A NI    TWALSCTL,X'FF'-TWALSMSV  CLEAR LSM MODE SAVE                     
         GOTO1 SCOVSCR,ATWAEND                                                  
         BNE   FVERR                                                            
         OI    SCINDS,SCINEWSC                                                  
         CLI   ASONOFF,ASOFF       EXIT IF OFFLINE                              
         BE    GENX                                                             
*                                                                               
VALACT26 CLI   INOVER,0            ENSURE OVERLAY NUMBER IS RESOLVED            
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VCOLY,SCPARM,(INOVER,0),0,0                                      
         BAS   RE,CHKOLY           TEST OVERLAY LOADED OK                       
         BNE   FVERR                                                            
         MVC   APNTRYA,0(R1)                                                    
         TM    INMIX1,MIXILST+MIXISEL                                           
         BNZ   VALACT28                                                         
         TM    TWAMODE,TWAMLSM                                                  
         BZ    VALACT28                                                         
         MVI   TWALACT,0           FORCE CHANGE OF ACTION                       
         NI    TWAMODE,255-TWAMLSM TURN OFF LSM IF NOT LIST/SELECT              
*                                                                               
VALACT28 TM    INMIX1,MIXILFM      TEST THIS IS A MAINT  SCREEN                 
         BNZ   VALACT29                                                         
         TM    INMIX1,MIXIREP+MIXILST   TEST THIS REPORT/LST ACTION             
         BZ    VALACT34                                                         
         TM    INMIX2,MIXISET      APMSETT REQUIRED                             
         BZ    VALACT34                                                         
         TM    SCINDS,SCINEWSC                                                  
         BNZ   VALACT32            TEST NEW SCREEN LOADED                       
         B     VALACT34                                                         
VALACT29 L     RF,ARECNTRY                                                      
         TM    RECINDS-RECTABD(RF),RECISET2                                     
         BNZ   VALACT32                                                         
         TM    RECINDS-RECTABD(RF),RECISET                                      
         BZ    *+12                                                             
         TM    SCINDS,SCINEWSC                                                  
         BNZ   VALACT32                                                         
*                                                                               
         TM    MIXINDS2,MIXISET    TEST APMSETT REQUIRED ON NEW ACTION          
         BZ    VALACT34                                                         
         TM    SCINDS,SCINEWSC     NEW ACTION IF NEW SCREEN LOADED              
         BNZ   VALACT32                                                         
         TM    TWAMODE,TWAMLSM     TEST LSM ACTIVE                              
         BZ    VALACT30                                                         
         CLI   INACT,ACTSEL        TEST SELECT ACTION                           
         BNE   VALACT30                                                         
         CLC   TWALACT,TWALSACT    TEST CHANGE OF LSM ACTION                    
         BNE   VALACT32                                                         
         B     VALACT34                                                         
*                                                                               
VALACT30 CLC   TWALACT,INACT       TEST CHANGE OF LFM ACTION                    
         BE    VALACT34                                                         
*                                                                               
VALACT32 GOTO1 APHOOK,APMSETT                                                   
*                                                                               
VALACT34 TM    TWAMODE,TWAMLSM                                                  
         BNZ   VALACTX                                                          
         CLC   TWALREC,INREC       RESET HOLD BITS ON REC/ACT CHANGE            
         BNE   *+10                                                             
         CLC   TWALACT,INACT                                                    
         BE    VALACTX                                                          
         NI    TWALSCTL,255-(TWALSHLD+TWALSRTN)                                 
*                                                                               
VALACTX  OI    SCINDS,SCIACT       SET RECORD/ACTION HAS BEEN VALIDATED         
         TM    ACACTIND,ACHKAFT    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALKEY                                                           
         GOTO1 ACHOOK,ACMACTR                                                   
         BNE   FVERR                                                            
         B     VALKEY                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE KEY FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   TM    ACKEYIND,ACHKBEF    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALKEY1                                                          
         GOTO1 ACHOOK,ACMKEYR                                                   
         BNE   FVERR                                                            
*                                                                               
VALKEY1  TM    ACKEYIND,ACNOVAL    TEST SKIP VALIDATION                         
         BNZ   VALKEYX                                                          
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    VALKEYX                                                          
         OC    AKEYHDR,AKEYHDR     TEST KEY FIELD ON SCREEN                     
         BZ    VALKEYX                                                          
         MVI   FVHELP,HELPKEY      KEY HELP REQUIRED                            
         GOTO1 SCFVAL,AKEYHDR                                                   
*                                                                               
VALKEYX  TM    ACKEYIND,ACHKAFT    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALOPT                                                           
         GOTO1 ACHOOK,ACMKEYR                                                   
         BNE   FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   ST    RE,SCCINTRE         MAY BE A NESTED CALL                         
         TM    ACOPTIND,ACHKBEF    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALOPT1                                                          
         GOTO1 ACHOOK,ACMOPTR                                                   
         BNE   VALOPTER                                                         
*                                                                               
VALOPT1  TM    ACOPTIND,ACNOVAL    TEST OPTIONS VALIDATION REQUIRED             
         BNZ   VALOPTX                                                          
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    VALOPTX                                                          
         MVI   FVHELP,HELPOPT                                                   
         GOTO1 SCFVAL,AOPTHDR      PRE-VALIDATE OPTIONS FIELD                   
         L     R1,ACOPTTAB                                                      
         GOTO1 AVALOPTS            CALL GENERAL OPTION VALIDATOR                
         BNE   VALOPTER                                                         
*                                                                               
VALOPTX  TM    ACOPTIND,ACHKAFT    TEST CONTROLLER HOOK REQUESTED               
         BZ    VALOPTXX                                                         
         GOTO1 ACHOOK,ACMOPTR                                                   
         BNE   VALOPTER                                                         
*                                                                               
VALOPTXX CLI   SCCTYP,SCCINTQ      GOOD EXIT                                    
         BNE   GO                                                               
         LR    RE,RB               FORCE CC EQUAL                               
VALOPTER CLI   SCCTYP,SCCINTQ      TEST INTERNAL CALL OF ROUTINE                
         BNE   FVERR                                                            
         CR    RB,RE                                                            
         L     RE,SCCINTRE         ALWAYS RETURN TO CALLER                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD SCREEN & GO TO CONTROLLER                                      *         
***********************************************************************         
         SPACE 1                                                                
GO       OI    SCINDS,SCIGO        SET GO POINT REACHED                         
         BAS   RE,SETFLD           SET FIELD HEADER ADDRESSES                   
*                                                                               
         TM    INMIX1,MIXILST      LIST MODE                                    
         BNZ   LSM                                                              
*                                                                               
         TM    INMIX1,MIXISEL      SELECT MODE                                  
         BNZ   LSM                                                              
*                                                                               
         TM    INMIX1,MIXILFM      FILE MAINTENANCE MODE                        
         BNZ   FIL                                                              
*                                                                               
         TM    INMIX1,MIXIREP      REPORTING MODE                               
         BZ    *+12                                                             
         NI    TWAMODE,TWAMNXT     ENSURE LSM INACTIVE NEXT TIME                
         B     REP                                                              
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE OFFLINE & ONLINE REPORTING                       *         
***********************************************************************         
         SPACE 1                                                                
REP      CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   *+12                                                             
         CLI   SCOMODE,TWAOLOAD    IGNORE LOAD MODE FROM SPOOF                  
         BE    EXIT                                                             
         TM    ACREPIND,ACHKBEF    TEST CONTROLLER HOOK REQUESTED               
         BZ    REP1                                                             
         GOTO1 ACHOOK,ACMREPR                                                   
         BNE   FVERR                                                            
*                                                                               
REP1     BAS   RE,REP2             CALL REPORT MONITOR                          
         TM    ACREPIND,ACHKAFT    TEST CONTROLLER HOOK REQUESTED               
         BZ    FVERRX                                                           
         GOTO1 ACHOOK,ACMREPR                                                   
         B     FVERRX                                                           
*                                                                               
         PUSH  USING                                                            
REP2     NMOD1 REPX-REPD,**REP**,CLEAR=YES                                      
         POP   USING                                                            
         L     RB,SCBASE1                                                       
         ST    RC,AREP             SET REPORT STORAGE IS AVAILABLE              
         USING REPD,RC             RC=A(LOCAL W/S)                              
*                                                                               
         MVC   REPACOM,ACOM        INITIALIZE REPBLK VALUES                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVI   REPACTN,REPAINI                                                  
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         MVI   REPWIDTH,REPWREGQ                                                
         MVC   REPCTRY,CUCTRY                                                   
         MVC   REPLANG,CULANG                                                   
         MVC   INDEST,CUUSER                                                    
*                                                                               
         CLI   ASONOFF,ASON        TEST ONLINE                                  
         BE    REP3                                                             
         MVC   REPAMST,TWAOVMST                                                 
         MVC   REPABOX,TWAOVBOX                                                 
         GOTO1 APHOOK,APMVALQ      HANDLE OFFLINE REPORT                        
         BE    REP5                                                             
         OI    SCINDS,SCIRQERR     SET REQUEST ERROR OCCURRED                   
         ICM   RF,15,APCURSOR      GET CURSOR POSN                              
         BNZ   *+12                                                             
         ICM   RF,15,FVADDR                                                     
         BZ    FVERR                                                            
         SR    RE,RE                                                            
         ICM   RE,3,FVABSA-FVIHDR(RF)                                           
         SRDL  RE,32                                                            
         D     RE,=F'80'                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX           ROW NUMBER  (01-24)                          
         LA    RE,1(RE)                                                         
         STC   RE,FVSUBX           COL NUMBER  (01-80)                          
         B     FVERR                                                            
*                                                                               
REP3     BAS   RE,SETFLD                                                        
         TM    INDRLIND,MIXIDRLS   TEST DROOL ON-SCREEN REPORT                  
         BZ    REP3A                                                            
         CLI   INKEYC,0            YES - TEST ANY KEY COMPONENTS                
         BE    REP4                                                             
         GOTO1 ASETKEY,0           YES - GO SET KEY VALUES                      
         BE    REP4                                                             
         TM    INMIX1,MIXKREQ      TEST KEY FIELDS REQUIRED                     
         BZ    REP4                                                             
         MVC   FVMSGNO,=AL2(FVFEKEY) YES - TELL USER TO ENTER KEY               
         TM    INMIX1,MIXDREQ      AND DATA (AS REQUIRED)                       
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFEKEYD) TELL USER TO ENTER KEY & DATA             
         MVC   FVADDR,AINKHDR                                                   
         B     FVERR                                                            
*                                                                               
REP3A    TM    SCINDS,SCINEWSC     TEST NEW SCREEN LOADED                       
         BZ    REP4                                                             
         MVC   FVADDR,AINDHDR      SET CURSOR TO FIRST UNPROT                   
         MVI   FVOMTYP,GTMINF      INFO MESSAGE                                 
         MVC   FVMSGNO,=AL2(INFREP4)                                            
         B     FVERR0                                                           
*                                                                               
REP4     MVC   REPDATE,ASBDAT      SET ONLINE VALUES                            
         MVC   REPAPQB,ATIA                                                     
         MVC   REPSYSID,INSYSID                                                 
         MVC   REPPRGID,INPRGID                                                 
*                                                                               
         GOTO1 APHOOK,APMVALQ      VALIDATE THE REQUEST SCREEN                  
         BNE   FVERR                                                            
         BAS   RE,SWAP             CHECK FOR SWAP/RETURN                        
         BE    SWAP2                                                            
         MVC   REPUSRID,INDEST                                                  
         CLI   INWIDTH,C'N'        TEST NARROW LINE REQUESTED                   
         BNE   REP4A                                                            
         MVI   REPHEADN,REPHNN                                                  
         MVI   REPMIDSN,REPMNN                                                  
         MVI   REPPRNTN,REPPNN                                                  
         MVI   REPFOOTN,REPFNN                                                  
         MVI   REPWIDTH,REPWNARQ                                                
         B     REP4B                                                            
*                                                                               
REP4A    CLI   INWIDTH,C'W'        TEST WIDE LINES REQUESTED                    
         BNE   REP4B                                                            
         MVI   REPHEADN,REPHWN                                                  
         MVI   REPMIDSN,REPMWN                                                  
         MVI   REPPRNTN,REPPWN                                                  
         MVI   REPFOOTN,REPFWN                                                  
         MVI   REPWIDTH,REPWIDEQ                                                
*                                                                               
REP4B    CLI   INWHEN,MIXIOKN      TEST PRINTING NOW                            
         BNE   REP9                                                             
*                                                                               
REP5     MVC   REPSUBID,INUSER                                                  
         TM    INMIX2,MIXIDRV      TEST DRIVER/DROOL APPLICATION                
         BZ    REP5B                                                            
         CLI   ASONOFF,ASON        YES-TEST ONLINE                              
         BNE   REP5B                                                            
         GOTO1 ADRIVER             YES-CALL DROOL                               
         BNE   FVERR                                                            
         TM    REPIND1,REPIPUT     TEST ANY LINES PUT                           
         BO    REP5A                                                            
         MVC   FVADDR,AACTHDR      NO-OUTPUT SPECIAL MESSAGE                    
         MVI   FVOMTYP,GTMWRN                                                   
         MVC   FVMSGNO,=AL2(WRNMSG1)                                            
         B     FVERR0                                                           
*                                                                               
REP5A    TM    INDRLIND,MIXIDRLS   YES-TEST DROOL ON-SCREEN REPORT              
         BZ    REP8A                                                            
         MVC   FVADDR,AACTHDR      YES-OUTPUT 'REPORT DISPLAYED' MSG            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(INFREP5)                                            
         B     FVERR0                                                           
*                                                                               
REP5B    MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         GOTO1 VREPORT,REPBLK                                                   
*                                                                               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   REP6                                                             
         TM    INMIX2,MIXIORP      TEST SEPARATE OFFLINE REPORT PHASE           
         BZ    REP5BA                                                           
         CLI   INOFFPH,0                                                        
         BE    REP5BA                                                           
         GOTO1 VCOLY,SCPARM,(INOFFPH,0),0,0                                     
         BAS   RE,CHKOLY                                                        
         BNE   FVERR                                                            
         MVC   APNTRYA,0(R1)                                                    
*                                                                               
REP5BA   OC    REPAPRT,REPAPRT     TEST PRINT RESOLVED                          
         BZ    REP6                                                             
*                                                                               
         L     R1,TWAOVMST         LOCATE REMOTEC                               
         L     R1,MCVREMOT-MASTD(R1)                                            
         USING REMOTED,R1                                                       
         OC    REMOTKEY,REMOTKEY   TEST REMOTE PRINTING                         
         BZ    REP5D                                                            
         CLI   INOTYP,C'@'         TEST FOR REFORM ID IN DESCRIPTION            
         BE    REP5CA                                                           
         CLI   REMOTFLG,X'FA'      TEST NEW STYLE LAYOUT                        
         BL    REP5C                NO                                          
         OC    REPDESC,REPDESC     OVERIDE REPORT SHORT DESCRIPTION ?           
         BZ    *+10                                                             
         MVC   REMOTDSC,REPDESC                                                 
         B     *+10                                                             
*                                                                               
REP5C    MVC   REMOTKEY(L'REPDESC),REPDESC                                      
*                                                                               
REP5CA   MVC   REMOTPAS,REPPSWD    OVERIDE REPORT SECURITY INFO                 
         MVC   REMOTSF1,REPSECF1                                                
         MVC   REMOTSF2,REPSECF2                                                
         DROP  R1                                                               
*                                                                               
REP5D    SR    R0,R0               PRINT REQUEST DETAILS PAGE                   
         ICM   RF,15,REPABOX                                                    
         BZ    *+8                                                              
         LA    R0,C'B'                                                          
         GOTO1 VREQTWA,SCPARM,(3,TWAD),(X'FF',ACOM),REPAPRT,((R0),(RF))         
*                                                                               
REP6     TM    INMIX2,MIXIDRV      TEST DRIVER APPLICATION                      
         BZ    REP6A                                                            
         GOTO1 ADRIVER             YES-CALL DRIVER                              
         BNE   FVERR                                                            
         B     REP8                                                             
*                                                                               
REP6A    MVI   REPACTN,REPAPUT                                                  
         GOTO1 APHOOK,APMREPP      GENERATE THE REPORT                          
         BE    REP7                                                             
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    REP8                                                             
         MVI   REPACTN,REPACLOP    YES - CLOSE/PURGE THE REPORT                 
         GOTO1 VREPORT,REPBLK                                                   
         BE    FVERR0                                                           
         DC    H'0'                CLOSE ERROR                                  
*                                                                               
REP7     TM    REPIND1,REPIPUT     TEST ANY LINES PUT                           
         BNZ   REP8                                                             
         MVI   REPACTN,REPACLO     NO - CLOSE REPORT                            
         GOTO1 VREPORT,REPBLK                                                   
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    FVERRX                                                           
         MVC   FVADDR,AACTHDR      NO - OUTPUT SPECIAL MESSAGE                  
         MVI   FVOMTYP,GTMWRN                                                   
         MVC   FVMSGNO,=AL2(WRNMSG1)                                            
         B     FVERR0                                                           
*                                                                               
REP8     MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTO1 VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
         CLI   ASONOFF,ASOFF                                                    
         BE    FVERRX                                                           
*                                                                               
REP8A    MVC   FVADDR,AACTHDR                                                   
         LA    RF,SCWORK           SET UP XXX,99999                             
         MVC   0(3,RF),REPSUBID                                                 
         MVI   3(RF),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,REPREPNO                                                    
         CVD   R0,SCDUB                                                         
         OI    SCDUB+7,X'0F'                                                    
         UNPK  SCWORK+4(5),SCDUB                                                
         LA    R1,4                                                             
         LA    RE,SCWORK+4                                                      
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),0(RE)                                                    
         LA    R1,5(R1)            L'TEXT                                       
         LA    RE,SCPARM           DEFINE GETTEXT BLOCK                         
         USING GETTXTD,RE                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         STC   R1,GTLTXT                                                        
         STCM  RF,7,GTATXT                                                      
         MVC   GTMSGNO,=AL2(INFREP3) REPORT XXX,99999 HAS BEEN SPOOLED          
         MVI   GTMTYP,GTMINF                                                    
         B     REPXIT                                                           
         DROP  RE                                                               
*                                                                               
REP9     LA    R2,APELEM           BUILD SPOOK                                  
         USING SPOOK,R2                                                         
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKUID,CUUSER                                                  
         MVC   SPOOKDES,INDEST                                                  
         MVC   SPOOKTID,CUTRMN                                                  
         MVC   SPOOKAGY,CUAALF                                                  
         MVC   SPOOKAGX,CUABIN                                                  
         MVC   SPOOKDID,INUSER                                                  
         MVC   SPOOKSYS,INSYSID                                                 
         MVC   SPOOKEOD,INPRGID                                                 
         MVC   SPOOKJCL,INJCLID                                                 
         MVC   SPOOKPR1,INPRTY1                                                 
         MVC   SPOOKPR2,INPRTY2                                                 
         MVC   SPOOKWEN,INWHEN                                                  
*                                                                               
         MVC   SPOOKXT,=C'XT='     SPOOK EXTENSION                              
*                                                                               
         TM    INMIX2,MIXIORP      TEST SEPARATE OFFLINE REPORT PHASE           
         BZ    *+8                                                              
         MVI   SPOOKPR2,C' '       MIXPRTY2 BORROWED FOR PHASE NO               
*                                                                               
         CLC   INOTYP,=CL6'DOWN'   TEST FOR DOWNLOAD TYPE                       
         BNE   *+8                                                              
         OI    SPOOKTY,X'10'                                                    
*                                                                               
         CLI   INOTYP,C'@'                                                      
         BNE   REP9A                                                            
         MVC   SPOOKSQL,INOTYP+1                                                
         OI    SPOOKTY,X'08'                                                    
         B     REP9B                                                            
REP9A    CLI   INOTYP,C'/'                                                      
         BNE   REP9B                                                            
         CLC   INOTYP+3(3),=CL3'   '                                            
         BNE   *+14                                                             
         MVC   SPOOKSUB,INOTYP+1   /XX INPUT                                    
         B     REP9B                                                            
         MVC   SPOOKSUB,INOTYP+4   /SPPXX INPUT                                 
         B     REP9B                                                            
*                                                                               
REP9B    CLI   INSML,C'L'          SMALL/MEDIUM/LARGE REPORT                    
         BE    REP9B1                                                           
         CLI   INSML,C'M'                                                       
         BE    REP9B1                                                           
         CLI   INSML,C'S'                                                       
         BE    REP9B1                                                           
         B     REP9C                                                            
REP9B1   MVC   SPOOKSML,INSML                                                   
*                                                                               
REP9C    EQU   *                                                                
*                                                                               
         L     R3,ACIOADD          BUILD REQUEST HEADER                         
         USING REQHDR,R3                                                        
         XC    REQHDR(REQEOH-REQHDR),REQHDR                                     
         MVC   REQOUT,INOTYP                                                    
         MVC   REQDEST,INDEST                                                   
         MVI   REQUEST,C' '        BUILD VIRGIN REQUEST CARD                    
         MVC   REQUEST+1(L'REQUEST-1),REQUEST                                   
         MVC   REQJCLID,INJCLID                                                 
         MVC   REQAGYID,CUAALF                                                  
         ICM   R0,15,ASSIN                                                      
         CVD   R0,SCDUB                                                         
         OI    SCDUB+7,X'0F'                                                    
         UNPK  REQSIN,SCDUB                                                     
*                                                                               
         GOTO1 VREQTWA,SCPARM,TWAD,REQHDR,VDMGR,ACOM,SPOOK                      
         DROP  R3                                                               
         MVC   FVADDR,AACTHDR      SET CURSOR TO ACTION FIELD                   
*                                                                               
         CLI   INWHEN,MIXIOKO      TEST OVERNIGHT REQUEST                       
         BNE   REP10                                                            
         LA    RE,SCPARM           DEFINE GETTXT BLOCK                          
         USING GETTXTD,RE                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(INFREP1) REPORT WILL BE PROCCESSED O/N              
         MVI   GTMTYP,GTMINF                                                    
         B     REPXIT                                                           
         DROP  RE                                                               
*                                                                               
REP10    CLI   INWHEN,MIXIOKS      TEST SOON REQUEST                            
         BE    REP11                                                            
         CLI   INWHEN,MIXIOKU      OR UPDATIVE SOON                             
         BE    REP11                                                            
         CLI   INWHEN,MIXIOKL      OR LATE                                      
         BE    REP11                                                            
         DC    H'0'                                                             
*                                                                               
REP11    CLI   8(R1),X'FE'         TEST TERMINAL QUEUE FULL                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTFUL)                                            
         B     FVERR                                                            
         CLI   8(R1),X'FF'         TEST PRINT QUEUE FULL                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFQFUL)                                            
         B     FVERR                                                            
         L     RE,8(R1)                                                         
         OC    0(7,RE),0(RE)       TEST JCL ERROR (NO KEY RETURNED)             
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFEJCL)                                            
         B     FVERR                                                            
*                                                                               
         LA    RF,SCWORK                                                        
         MVC   0(3,RF),INUSER                                                   
         MVI   3(RF),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         CVD   R0,SCDUB                                                         
         OI    SCDUB+7,X'0F'                                                    
         UNPK  SCWORK+4(5),SCDUB                                                
         LA    R1,4                                                             
         LA    RE,SCWORK+4                                                      
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),0(RE)                                                    
         LA    R1,5(R1)            L'TEXT                                       
         LA    RE,SCPARM           DEFINE GETTXT BLOCK                          
         USING GETTXTD,RE                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         STC   R1,GTLTXT                                                        
         STCM  RF,7,GTATXT                                                      
         MVC   GTMSGNO,=AL2(INFREP2) REP XXX,99999 WILL BE PROC SOON            
         CLI   INWHEN,MIXIOKL                                                   
         BNE   *+10                                                             
         MVC   GTMSGNO,=AL2(INFREP6) REP XXX,99999 WILL BE PROC LATE            
         MVI   GTMTYP,GTMINF                                                    
         B     REPXIT                                                           
         DROP  RE                                                               
*                                                                               
REPXIT   BAS   RE,SWAP                                                          
         BE    SWAP2                                                            
         B     FVOKEX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE FILE MAINTENANCE OVERLAYS                        *         
***********************************************************************         
         SPACE 1                                                                
FIL      TM    ACLFMIND,ACHKBEF    TEST CONTROLLER HOOK REQUESTED               
         BZ    FIL1                                                             
         GOTO1 ACHOOK,ACMLFMR                                                   
         BNE   FVERR                                                            
*                                                                               
FIL1     NI    TWAMODE,TWAMNXT+TWAMDFR                                          
         CLI   INKEYC,0            TEST ANY KEY COMPONENTS                      
         BE    FILR                                                             
         TM    CUSTAT,CUSDDL       YES, TEST DDLINK IN CONTROL                  
         BZ    FIL2                                                             
         BRAS  RE,DDLKGET          YES, GET KEY DATA                            
         BNE   FILN                                                             
         B     FILR                                                             
FIL2     GOTO1 ASETKEY,0           NO, GO SET KEY VALUES                        
         BE    FILR                                                             
         TM    INMIX1,MIXKREQ      TEST KEY FIELDS REQUIRED                     
         BZ    FILR                                                             
         MVC   FVMSGNO,=AL2(FVFEKEY) YES - TELL USER TO ENTER KEY               
         TM    INMIX1,MIXDREQ      AND DATA (AS REQUIRED)                       
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFEKEYD) TELL USER TO ENTER KEY & DATA             
         MVC   FVADDR,AINKHDR                                                   
         B     FVERR                                                            
*                                                                               
FILCLR   XC    SAVRECK,SAVRECK     CLEAR SAVED VALUES ON KEY ERROR              
         XC    SAVRECD,SAVRECD                                                  
         XC    SAVRECI,SAVRECI                                                  
         XC    SAVRECN,SAVRECN                                                  
*                                                                               
FILN     NI    INFLAG1,X'FF'-INFDISR    TURN OFF FORCE DISPLAY RECORD           
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    FVERR                                                            
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MONITOR ACTIVE              
         BZ    FVERR                                                            
         OI    TWAMODE,TWAMERR     SET LIST/SELECT ERROR                        
         B     LSMXIT                                                           
*                                                                               
FILXIT   NI    INFLAG1,X'FF'-INFDISR    TURN OFF FORCE DISPLAY RECORD           
         LA    R1,INACT            POINT TO ACTION NUMBER                       
         CLI   0(R1),ACTLFM8       TEST VALID LFM ACTION                        
         BNH   *+8                                                              
         LA    R1,INACTLFM         NO - POINT TO LFM ACTION NUMBER              
         CLI   0(R1),ACTCPY        RESET TWAMDFR IF NOT COPY/RENAME             
         BE    *+8                                                              
         CLI   0(R1),ACTREN                                                     
         BE    *+8                                                              
         NI    TWAMODE,255-TWAMDFR                                              
*                                                                               
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BO    *+12                                                             
         BAS   RE,SWAP             CHECK FOR SWAP/RETURN                        
         BE    SWAP2               YES                                          
*                                                                               
         MVC   SAVRECK,APRECKEY    SAVE THIS TIME RECORD VALUES                 
         MVC   SAVRECD,APRECDA                                                  
         MVC   SAVRECI,APRECID                                                  
         MVC   SAVRECN,APRECNUM                                                 
         TM    ACLFMIND,ACHKAFT    TEST CONTROLLER HOOK REQUESTED               
         BZ    FILXIT2                                                          
         GOTO1 ACHOOK,ACMLFMR                                                   
         BNE   FVERR                                                            
*                                                                               
FILXIT2  TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    FILXIT3                                                          
         TM    SCDDLFLG,SCDDLFEQ   YES, WANT TO SIMULATE USER ENTERING          
         BZ    FVERR                    CHANGES?                                
         NI    SCDDLFLG,255-SCDDLFEQ YES, CLEAR FLAG                            
         BRAS  RE,DDLDGET          GET NEW DATA                                 
         BNE   FVERR                                                            
         B     FILR                AND GO ROUND AGAIN                           
*                                                                               
FILXIT3  TM    TWAMODE,TWAMLSM     TEST LIST SELECT MODE                        
         BZ    FVERR                                                            
         TM    TWALSCTL,TWALSRTN   OVERLAY WANTS A RETURN CALL                  
         BO    FILXITX                                                          
         CLI   SCFLAG,0            TEST SPECIAL MAINTENANCE                     
         BE    *+8                                                              
         NI    TWAMODE,255-(TWAMDFC+TWAMDFR)                                    
*                                                                               
FILXITX  B     ACTX                                                             
         EJECT                                                                  
FILR     MVI   APINDS,0            ** REGULAR MAINTENANCE **                    
         MVI   SCFLAG,0                                                         
         MVI   SCPFKEY,0                                                        
         NI    TWAMODE,255-(TWAMDFC+TWAMDFN)                                    
         NI    TWALSCTL,255-(TWALSHLD+TWALSRTN)                                 
         ZIC   RF,INACT                                                         
         CLI   INACT,ACTLFM8                                                    
         BNH   *+8                                                              
         IC    RF,INACTLFM                                                      
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     ADD                                                              
         B     DIS                                                              
         B     CHA                                                              
         B     DEL                                                              
         B     RES                                                              
         B     CPY                                                              
         B     REN                                                              
         B     RPT                                                              
         SPACE 2                                                                
FILS     TM    ACLSMIND,ACLSMICI   ** LIST/SELECT FILE MAINTENANCE **           
         BZ    *+14                                                             
         CLC   APRECID,SAVRECI     TEST APRECID CHANGED                         
         BNE   *+10                                                             
         MVC   APRECID,SAVRECI                                                  
         MVC   APRECKEY,SAVRECK                                                 
         MVC   APRECDA,SAVRECD                                                  
         MVC   APRECNUM,SAVRECN                                                 
FILS2    MVI   SCFLAG,1            ** SPECIAL ENTRY FOR COPY/RENAME **          
         MVI   SCPFKEY,0                                                        
         NI    TWALSCTL,255-(TWALSHLD+TWALSRTN)                                 
         ZIC   RF,INACT                                                         
         CLI   INACT,ACTLFM8                                                    
         BNH   *+8                                                              
         IC    RF,INACTLFM                                                      
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     ADD                                                              
         B     DIS                                                              
         B     CHA2                                                             
         B     DEL2                                                             
         B     RES2                                                             
         B     CPY8                                                             
         B     CPY8                                                             
         B     RPT2                                                             
         EJECT                                                                  
ADD      GOTO1 APHOOK,APMVALK      ** ADD A RECORD **                           
         BNE   FILCLR                                                           
         TM    APINDS,APIOKADD                                                  
         BNZ   ADD2                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     FILN                                                             
*                                                                               
ADD2     TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    *+12                                                             
         BRAS  RE,DDLDGET          YES, GET DATA                                
         BNE   ADDX                                                             
         BRAS  RE,SOX                                                           
         BNE   FILN                                                             
         GOTO1 APHOOK,APMVALR                                                   
         BNE   FILN                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ADDX                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(INFADD1)                                            
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
ADDX     B     FILXIT                                                           
         SPACE 2                                                                
DIS      GOTO1 APHOOK,APMVALK      ** DISPLAY A RECORD **                       
         BNE   FILCLR                                                           
         TM    APINDS,APIOKDIS                                                  
         BNZ   DIS2                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     FILCLR                                                           
*                                                                               
DIS2     GOTO1 APHOOK,APMDISR                                                   
         BNE   FILN                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISX                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFDIS1)                                            
         MVI   FVOMTYP,GTMINF                                                   
         TM    APINDS,APIOKRES                                                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(INFDIS2)                                            
*                                                                               
DISX     B     FILXIT                                                           
         EJECT                                                                  
CHA      GOTO1 APHOOK,APMVALK      ** CHANGE A RECORD **                        
         BNE   FILCLR                                                           
         TM    APINDS,APIOKDIS                                                  
         BNZ   CHA2                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     FILCLR                                                           
*                                                                               
CHA2     TM    APINDS,APIOKCHA                                                  
         BNZ   CHA4                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFXCHA)                                            
         TM    APINDS,APIOKRES                                                  
         BZ    FILN                                                             
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
         B     FILN                                                             
*                                                                               
CHA4     CLC   APRECKEY,SAVRECK                                                 
         BNE   CHA5                NOT   SAME RECORD KEY, REDISPLAY             
         TM    INFLAG1,INFDISR     FORCE DISPLAY RECORD ?                       
         BO    CHA5                YES,  DISPLAY RECORD                         
         CLC   APRECID,SAVRECI                                                  
         BE    CHA6                KEY UNCHANGED, GO VALIDATE SCREEN            
*                                                                               
CHA5     GOTO1 APHOOK,APMDISR      KEY CHANGED, DISPLAY RECORD                  
         BNE   FILN                                                             
         OI    TWAMODE,TWAMDFC     DISPLAYED FOR CHANGE                         
         CLC   FVMSGNO,=AL2(FVFOK) IF DISPLAYED OK, GET NEW DATA                
         BNE   CHAX                ERROR DISPLAYING                             
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    CHA5A                                                            
         OI    SCDDLFLG,SCDDLFEQ   YES, SIMULATE USER ENTERING CHANGES          
         B     FILXIT              AND REPEAT KEY VAL (AS IF NEW I/P)           
CHA5A    MVC   FVADDR,AINDHDR      NO, ASK USER FOR CHANGES                     
         MVC   FVMSGNO,=AL2(INFCHA1)                                            
         MVI   FVOMTYP,GTMINF                                                   
         B     CHAX                                                             
*                                                                               
CHA6     BRAS  RE,SOX              APPLY THE CHANGES                            
         BNE   FILN                                                             
         GOTO1 APHOOK,APMVALR                                                   
         BNE   FILN                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   CHAX                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFCHA2)                                            
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    CHAX                                                             
         XC    APRECKEY,APRECKEY   YES, ENSURE FINISHED WITH THIS KEY           
*                                                                               
CHAX     B     FILXIT                                                           
         EJECT                                                                  
DEL      GOTO1 APHOOK,APMVALK      ** DELETE A RECORD **                        
         BNE   FILCLR                                                           
         TM    APINDS,APIOKDIS                                                  
         BNZ   DEL2                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     FILCLR                                                           
*                                                                               
DEL2     TM    APINDS,APIOKDEL                                                  
         BNZ   DEL4                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFXDEL)                                            
         B     FILN                                                             
*                                                                               
DEL4     CLC   APRECKEY,SAVRECK                                                 
         BNE   DEL5                NOT   SAME RECORD KEY, REDISPLAY             
         TM    INFLAG1,INFDISR     FORCE DISPLAY RECORD ?                       
         BO    DEL5                YES,  DISPLAY RECORD                         
         CLC   APRECID,SAVRECI                                                  
         BE    DEL6                                                             
*                                                                               
DEL5     GOTO1 APHOOK,APMDISR                                                   
         BNE   FILN                                                             
         OI    TWAMODE,TWAMDFC                                                  
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DELX                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFDEL1)                                            
         MVI   FVOMTYP,GTMINF                                                   
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    DELX                                                             
*                                                                               
DEL6     BRAS  RE,SOX                                                           
         BNE   FILN                                                             
         GOTO1 APHOOK,APMDELR                                                   
         BNE   FILN                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DELX                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFDEL2)                                            
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
DELX     B     FILXIT                                                           
         EJECT                                                                  
RES      GOTO1 APHOOK,APMVALK      ** RESTORE A RECORD **                       
         BNE   FILCLR                                                           
         TM    APINDS,APIOKDIS                                                  
         BNZ   RES2                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     FILCLR                                                           
*                                                                               
RES2     TM    APINDS,APIOKRES                                                  
         BNZ   RES4                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFXRES)                                            
         B     FILN                                                             
*                                                                               
RES4     CLC   APRECKEY,SAVRECK                                                 
         BNE   RES5                NOT   SAME RECORD KEY, REDISPLAY             
         TM    INFLAG1,INFDISR     FORCE DISPLAY RECORD ?                       
         BO    RES5                YES,  DISPLAY RECORD                         
         CLC   APRECID,SAVRECI                                                  
         BE    RES6                                                             
*                                                                               
RES5     GOTO1 APHOOK,APMDISR                                                   
         BNE   FILN                                                             
         OI    TWAMODE,TWAMDFC                                                  
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   RESX                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFRES1)                                            
         MVI   FVOMTYP,GTMINF                                                   
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    RESX                                                             
*                                                                               
RES6     BRAS  RE,SOX                                                           
         BNE   FILN                                                             
         GOTO1 APHOOK,APMRESR                                                   
         BNE   FILN                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   RESX                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFRES2)                                            
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
RESX     B     FILXIT                                                           
         EJECT                                                                  
REN      DS    0H                  ** RENAME A RECORD **                        
CPY      TM    TWAMODE,TWAMDFR     ** COPY A RECORD **                          
         BNZ   CPY6                                                             
         OI    ACLFMIND,ACLFMIFK                                                
         GOTO1 APHOOK,APMVALK                                                   
         BNE   FILCLR                                                           
         TM    APINDS,APIOKDIS                                                  
         BNZ   CPY2                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     FILCLR                                                           
*                                                                               
CPY2     TM    APINDS,APIOKRES                                                  
         BZ    CPY4                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFCRDR)                                            
         B     FILN                                                             
*                                                                               
CPY4     GOTO1 APHOOK,APMDISR                                                   
         BNE   FILN                                                             
         OI    TWAMODE,TWAMDFR                                                  
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILXIT                                                           
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFENEW)                                            
         B     FILXIT                                                           
*                                                                               
CPY6     GOTO1 APHOOK,APMVALK                                                   
         BNE   FILCLR                                                           
         TM    APINDS,APIOKADD                                                  
         BNZ   CPY8                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   FILN                                                             
         MVC   FVADDR,AINKHDR                                                   
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     FILN                                                             
*                                                                               
CPY8     BRAS  RE,SOX                                                           
         BNE   FILN                                                             
         GOTO1 APHOOK,APMNEWK                                                   
         BNE   FILN                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   CPYX                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFCHA2)                                            
         LA    R1,INACT                                                         
         CLI   INACT,ACTLFM8                                                    
         BNH   *+8                                                              
         LA    R1,INACTLFM                                                      
         CLI   0(R1),ACTREN                                                     
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(INFADD1)                                            
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
CPYX     B     FILXIT                                                           
         EJECT                                                                  
RPT      GOTO1 APHOOK,APMVALK      ** DISPLAY A REPORT **                       
         BNE   FILCLR                                                           
         CLC   APRECKEY,SAVRECK                                                 
         BNE   *+14                                                             
         CLC   INOPTS,SAVOPTS                                                   
         BE    RPT2                                                             
         GOTO1 APHOOK,APMFRP                                                    
         BNE   FILN                                                             
*                                                                               
RPT2     GOTO1 APHOOK,APMDISR                                                   
         BNE   FILN                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   RPT4                                                             
         MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(INFDIS1)                                            
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
RPT4     CLI   APMODE,APMLRP                                                    
         BE    *+12                                                             
         OI    TWAMODE,TWAMDFN                                                  
         B     RPTX                                                             
         NI    TWAMODE,255-TWAMDFN                                              
         MVI   SCFLAG,1                                                         
*                                                                               
RPTX     B     FILXIT                                                           
         EJECT                                                                  
***********************************************************************         
* SARBANES-OXLEY RULES - READ ONLY ACCESS                                       
***********************************************************************         
         USING COMFACSD,R1                                                      
         USING XTRAINFD,RF                                                      
SOX      L     R1,ACOM             A(COMFACS)                                   
         L     RF,CXTRAINF         A(XTRAINFO) IN COMFACS                       
         DROP  R1                                                               
                                                                                
         SR    R1,R1                                                            
         TM    XIFLAG1,XIROMODE+XIROSYS+XIWRONGF                                
         JZ    SOXXIT              OKAY TO KEEP GOING                           
         TM    XIFLAG1,XIROSYS+XIWRONGF                                         
         JNZ   SOX20               CAN'T WRITE REGARDLESS OF APINOSOX           
         TM    APINDS2,APINOSOX                                                 
         JO    SOXXIT                                                           
                                                                                
SOX20    MVI   FVOSYS,X'FF'        SET TO GENRAL SYSTEM                         
         LHI   R1,FVFNAUTH         USER NOT AUTHORIZED FOR UPDATE               
         TM    XIFLAG1,XIROMODE    READ ONLY MODE                               
         JO    SOXXIT              USER NOT AUTHORIZED                          
         LHI   R1,FVFNAVIL         SYSTEM NO AVAILABLE                          
         TM    XIFLAG1,XIWRONGF    WRONG ADV#                                   
         JZ    SOXXIT              SYSTEM MUST BE SET TO READ ONLY              
         LHI   R1,FVFWGADV         WRONG ADV                                    
         MVC   FVXTRA(4),XIUPDFAC                                               
                                                                                
SOXXIT   NI    APINDS2,X'FF'-APINOSOX                                           
         LTR   R1,R1               SET CC                                       
         BZR   RE                                                               
         STCM  R1,3,FVMSGNO                                                     
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE LIST/SELECT                                       *         
***********************************************************************         
         SPACE 1                                                                
LSM      TM    ACLSMIND,ACHKBEF    TEST CONTROLLER HOOK REQUESTED               
         BZ    LSM1                                                             
         GOTO1 ACHOOK,ACMLSMR                                                   
         BNE   FVERR                                                            
*                                                                               
LSM1     BAS   RE,LSM2             CALL LIST/SELECT MONITOR                     
         TM    ACLSMIND,ACHKAFT    TEST CONTROLLER HOOK REQUESTED               
         BZ    FVERRX                                                           
         GOTO1 ACHOOK,ACMLSMR                                                   
         B     FVERRX                                                           
*                                                                               
         PUSH  USING                                                            
LSM2     NMOD1 LSMX-LSMD,**LSM**,CLEAR=YES                                      
         POP   USING                                                            
         L     RB,SCBASE1                                                       
         ST    RC,ALSM             SET LIST/SELECT PAGE IS AVAILABLE            
         USING LSMD,RC             RC=A(LOCAL W/S)                              
*                                                                               
         CLI   ACPFFST,0           PRESET PFKEY NUMBERS                         
         BNE   *+8                                                              
         MVI   ACPFFST,PFK04                                                    
         CLI   ACPFCUR,0                                                        
         BNE   *+8                                                              
         MVI   ACPFCUR,PFK05                                                    
         CLI   ACPFNXT,0                                                        
         BNE   *+8                                                              
         MVI   ACPFNXT,PFK06                                                    
         CLI   ACPFXIT,0                                                        
         BNE   *+8                                                              
         MVI   ACPFXIT,PFK12                                                    
         CLI   ACPFRET,0                                                        
         BNE   *+8                                                              
         MVI   ACPFRET,PFK11                                                    
*                                                                               
         L     R1,ARECNTRY                                                      
         TM    RECINDS-RECTABD(R1),RECINOP                                      
         BZ    LSM03                                                            
         CLC   SCPFKEY,ACPFXIT     DON'T DISABLE QUIT PF KEY                    
         BE    *+8                                                              
         MVI   SCPFKEY,0           RESET LIST PF KEY VALUE                      
*                                                                               
LSM03    MVC   SCSELLVL,TWAMODE2   EXTRACT LIST LEVEL                           
         NI    SCSELLVL,TWAMLVL                                                 
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MONITOR ACTIVE              
         BNZ   LSM07                                                            
         OI    TWAMODE,TWAMLSM     NO - INITIALIZE                              
         MVI   SCSELLVL,0                                                       
*                                                                               
LSM04    ZIC   R1,SCSELLVL         BUMP TO NEXT LIST NEST LEVEL                 
         LA    R1,1(R1)                                                         
         LA    R0,TWAMLVL                                                       
         CR    R1,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,SCSELLVL         SET CURRENT LIST/SELECT NEST LEVEL           
         NI    TWAMODE2,255-TWAMLVL                                             
         OC    TWAMODE2,SCSELLVL                                                
         GOTO1 APHOOK,APMFLST      FIRST TIME HOOK TO OVERLAY                   
         MVC   LSMLREC,INREC       SAVE RECORD/ACTION ETC.                      
         MVC   LSMLACT,INACT                                                    
         MVC   LSMLSCR,INSCRN                                                   
         MVC   LSMLOVR,INOVER                                                   
         MVI   LSMINDS,0           RESET LIST/SELECT INDICATORS                 
         CLI   INKEYC,0            TEST ANY KEY FIELDS                          
         BE    LSM12                                                            
         CLI   SCSELLVL,1          TEST NESTED LIST CALL                        
         BE    LSM06                                                            
         ICM   RF,15,ASELNTRY      TEST SELECT TABLE ENTRY SET                  
         BZ    *+8                                                              
         TM    ACLSMIND,ACLSMISK   TEST USER HAS KEYS IN TABLE                  
         BZ    *+10                                                             
         MVC   INKEYT,SELKEYS-SELTABD(RF)                                       
         CLI   INKEYN,0            TEST ANY KEY FIELDS ON SCREEN                
         BE    LSM05                                                            
         GOTO1 AGETKEY             GET LIST KEY FROM SAVED KEYS                 
         B     LSM12                                                            
*                                                                               
LSM05    ICM   R1,15,AKEYHDR       SAVE INPUT KEY IN LSMKEYSV                   
         BZ    LSM06                                                            
         LH    RE,=Y(LSMKEYSV-LSMD)                                             
         LA    RE,LSMD(RE)                                                      
         MVC   0(L'LSMKEYSV,RE),L'FVIHDR(R1)                                    
         BAS   RE,CLRFLD           CLEAR DOWN THE KEY FIELD IN TWA              
*                                                                               
LSM06    GOTO1 ASETKEY,0           SET KEY FIELDS                               
         BE    LSM12                                                            
         MVC   FVADDR,AINKHDR      POSITION CURSOR AND ASK FOR KEY              
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     LSMXIT                                                           
*                                                                               
LSM07    CLC   SCPFKEY,ACPFXIT     TEST FOR QUIT AT NTH LIST LEVEL              
         BNE   LSM10                                                            
         TM    INMIX1,MIXILST                                                   
         BZ    LSM10                                                            
         CLI   SCSELLVL,1                                                       
         BNH   LSM10                                                            
         BAS   RE,READTWA          RESTORE TWA FOR SAVED KEY                    
*                                                                               
LSM08    CLI   SCSELLVL,1                                                       
         BNH   LSM10                                                            
         IC    R1,SCSELLVL         GO BACK TO PREVIOUS LIST LEVEL               
         BCTR  R1,0                                                             
         STC   R1,SCSELLVL                                                      
         NI    TWAMODE2,255-TWAMLVL                                             
         OC    TWAMODE2,SCSELLVL                                                
         LH    RE,=Y(LSMKEYSV-LSMD)                                             
         LA    RE,LSMD(RE)         TEST IF KEY WAS SAVED                        
         OC    0(L'LSMKEYSV,RE),0(RE)                                           
         BZ    LSM09                                                            
         ICM   R1,15,AKEYHDR       R1=A(KEY FIELD HEADER IN TWA)                
         BZ    LSM09                                                            
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         BCTR  RF,0                RF=L'KEY FIELD-1                             
         EX    RF,*+8              RESTORE SAVED KEY FIELD & TRANSMIT           
         B     *+10                                                             
         MVC   L'FVIHDR(0,R1),0(RE)                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         GOTO1 ASETKEY,1                                                        
*                                                                               
LSM09    BAS   RE,READTWA          READ LSM PAGE AND SET VALUES                 
         LH    R4,LSMRDSP                                                       
         LA    R4,LSMD(R4)                                                      
         BAS   RE,SETRECV          RESTORE RECORD VALUES (SEE LFM9)             
         MVI   SCFLAG,1                                                         
*                                                                               
         TM    APINDS2,APINXOPT                                                 
         BZ    LSM09A                                                           
         L     R1,AOPTHDR                                                       
         OI    FVIIND-FVIHDR(R1),FVITHIS                                        
         SR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R1)                                             
         MVI   L'FVIHDR(R1),C' '   CLEAR OPTIONS FIELD                          
         LA    RE,L'FVIHDR+1+1                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         LA    RE,8(RE)                                                         
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR+1(0,R1),L'FVIHDR(R1)                                    
         XC    INOPTS,INOPTS                                                    
*                                                                               
         LH    RE,=Y(LSMOPTS-LSMD)                                              
         LA    RE,LSMD(RE)                                                      
         SR    RF,RF               RESTORE ANY OPTIONS                          
         ICM   RF,1,(FVILEN-FVIHDR)(RE)                                         
         BZ    LSM09A                                                           
         BCTR  RF,0                                                             
         LA    R1,L'FVIHDR(R1)                                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),L'FVIHDR(RE)                                             
         LH    RE,=Y(LSMINOPT-LSMD)                                             
         LA    RE,LSMD(RE)                                                      
         MVC   INOPTS,0(RE)        RESTORE INTERNAL VALUES                      
LSM09A   B     ACTX                                                             
*                                                                               
LSM10    BAS   RE,READTWA          READ LSM PAGE AND SET VALUES                 
         CLI   INKEYC,0            TEST ANY KEY FIELDS                          
         BE    LSM12                                                            
         TM    INMIX1,MIXISEL      NO AUTO KEY IF SELECT                        
         BNZ   LSM12                                                            
         GOTO1 ASETKEY,0                                                        
*                                                                               
LSM12    TM    INMIX1,MIXILST      TEST LIST OR SELECT MODE                     
         BZ    LFM2                                                             
         B     INI2                                                             
*                                                                               
LSMXIT   GOTO1 VDMGR,SCPARM,DMWRITE,TEMPSTR,(SCSELLVL,0),LSMD                   
         B     FVERR                                                            
         EJECT                                                                  
INI2     GOTO1 APHOOK,APMVALP      VALIDATE LIST/SELECT KEY                     
         BNE   LSMXIT                                                           
         CLI   APMODE,APMRET       TEST RETURN TO PREVIOUS REC/ACT              
         BE    LSM08                                                            
         BAS   RE,SWAP             TEST SWAP TO ANOTHER REC/ACT                 
         BE    SWAP2               YES                                          
         MVI   LSMTWNUM,0          SET ACTUAL TWA LINES TO ZERO                 
         L     RF,APPARM+0         APPARM+0(4)=A(FIRST TWA SEL FLD)             
         L     R1,ATWAEND                                                       
         SR    RE,RE                                                            
INI4     AR    R1,RE                                                            
         CR    R1,RF                                                            
         BE    INI6                                                             
         BL    *+6                                                              
         DC    H'0'                BAD INITIAL SELECT FIELD ADDRESS             
         ICM   RE,1,0(R1)                                                       
         BNZ   INI4                                                             
         DC    H'0'                INVALID TWA LINKAGE                          
INI6     LR    R1,RF                                                            
         S     RF,ATWA                                                          
         STH   RF,LSMTWFST                                                      
*                                                                               
         ZIC   RE,0(R1)            SET LENGTH OF SELECT FIELD                   
         LA    R0,L'FVIHDR                                                      
         SR    RE,R0                                                            
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+6                                                              
         SR    RE,R0                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    INI7                                                             
         TM    APINDS2,APSETSLN                                                 
         BNZ   INI7                ALWAYS SET SELECT LENGTH                     
         SR    RE,RE                                                            
INI7     STC   RE,LSMSELLN         SET LENGTH OF SELECT FIELD                   
*                                                                               
         ICM   RF,3,APPARM+6       APPARM+6(2)=L'LIST/SELECT LINE               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STH   RF,LSMTWLEN         SET L'LIST/SELECT LINE                       
         AR    RF,R1                                                            
         SR    R0,R0                                                            
         ICM   R0,1,APPARM+4       APPARM+4(1)=N'LIST/SELECT LINES              
         BNZ   *+8                                                              
         LA    R0,LSMTWMAX                                                      
         LA    RE,LSMTWMAX                                                      
         CR    R0,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
INI8     AR    R1,RE                                                            
         CR    R1,RF                                                            
         BE    INI10                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         ICM   RE,1,0(R1)                                                       
         BNZ   INI8                                                             
         DC    H'0'                INVALID TWA                                  
INI10    IC    RE,LSMTWNUM                                                      
         LA    RE,1(RE)                                                         
         STC   RE,LSMTWNUM                                                      
         MVC   TWALSMN,LSMTWNUM                                                 
         LR    R1,RF                                                            
         AH    RF,LSMTWLEN                                                      
         SR    RE,RE                                                            
         CLI   0(R1),EOT           TEST END-OF-TWA                              
         BE    *+10                                                             
         BCT   R0,INI8                                                          
         BCTR  R0,0                MAYBE NOT END-OF-TWA                         
         S     R1,ATWA                                                          
         STH   R1,LSMTWEND         SET DISPLACEMENT TO END OF TWA               
*                                                                               
INIX     CLC   LSMUKEY,APRECKEY    TEST CHANGE OF KEY PARAMETERS                
         MVC   LSMUKEY,APRECKEY                                                 
         BNE   BLDFST                                                           
         CLC   LSMUOPT,INOPTS      TEST CHANGE OF OPTIONS                       
         BNE   BLDFST                                                           
         TM    LSMINDS,LSMISEL     TEST USER INVITED TO ENTER SELECTS           
         BZ    BLDNXT                                                           
*                                                                               
         CLC   SCPFKEY,ACPFXIT     TEST QUIT PFKEY                              
         BE    SEL2                                                             
         B     NXT2                                                             
         EJECT                                                                  
BLDSEL   BAS   RE,SAVLIST          SAVE LIST SCREEN                             
         BAS   RE,XMTLIST          TRANSMIT LIST SCREEN                         
*                                                                               
BLDSEL2  LH    R1,LSMTWFST         SET CURSOR TO FIRST UNPROT FIELD             
         A     R1,ATWA                                                          
         LH    RF,LSMTWLEN                                                      
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    BLDSEL4                                                          
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-12                                                       
         L     R1,AACTHDR          ELSE POINT TO ACTION FIELD                   
         SR    RE,RE               CLEAR RE IF NO INPUT FIELDS ON LINE          
BLDSEL4  ST    R1,FVADDR                                                        
         NI    LSMINDS,255-LSMISEOS-LSMISEOL                                    
         OI    LSMINDS,LSMISEL                                                  
         TM    ACLSMIND,ACLSMIMP   TEST APPLICATION PROVIDED MESSAGE            
         BNZ   LSMXIT                                                           
         MVI   FVOMTYP,GTMINF                                                   
         LTR   RE,RE               TEST SELECT FIELD PRESENT                    
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(INFCHA1)                                            
         B     LSMXIT                                                           
         MVC   FVMSGNO,=AL2(INFSEL2)                                            
         TM    LSMINDS,LSMIEOL                                                  
         BZ    LSMXIT                                                           
         MVC   FVMSGNO,=AL2(INFSEL3)                                            
         B     LSMXIT                                                           
*                                                                               
BLDFST   MVC   LSMLKEY,LSMUKEY     SET-UP FOR START OF LIST                     
         MVC   LSMUOPT,INOPTS                                                   
         MVI   LSMINDS,0                                                        
         LH    RE,=Y(LSMPRK-LSMD)                                               
         LA    RE,LSMD(RE)                                                      
         LR    R0,RE                                                            
         LH    R1,=Y(L'LSMPRK)                                                  
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    APRECNUM(APPFKEY-APRECNUM),APRECNUM                              
         MVI   APINDS,APILFLST     FIRST LINE OF FIRST SCREEN                   
         GOTO1 APHOOK,APMFSCR                                                   
         BNE   LSMXIT                                                           
         B     BLD4                                                             
*                                                                               
BLDNXT   MVI   APINDS,APILNLST     FIRST LINE OF NOT FIRST SCREEN               
         NI    LSMINDS,255-LSMISEOS                                             
         CLI   ACPFPRE,0                                                        
         BE    *+14                                                             
         CLC   SCPFKEY,ACPFPRE     TEST PREVIOUS SCREEN REQUESTED               
         BE    BLD2                                                             
         TM    LSMINDS,LSMIEOL     TEST END OF LIST ON THIS SCREEN              
         BZ    BLD4                                                             
         CLI   SCSELLVL,1          TEST IF A NESTED LIST                        
         BNH   BLDFST              NO  - RESTART CURRENT LIST                   
         TM    TWALSCTL,TWALSHSL   YES - TEST HOLD THIS LIST LEVEL              
         BO    BLDFST              YES - RESTART CURRENT SCREEN                 
         B     LSM08               NO - RETURN TO PREVIOUS LIST LEVEL           
*                                                                               
BLD2     LH    RE,=Y(LSMPRKNO-LSMD)                                             
         LA    RE,LSMD(RE)                                                      
         CLI   0(RE),1                                                          
         BH    BLD4                                                             
         MVI   SCPFKEY,0           TREAT AS FIRST                               
         B     BLDFST                                                           
*                                                                               
BLD4     BAS   RE,CLRLIST          CLEAR & TRANSMIT LIST                        
         LH    R2,LSMTWFST         POINT TO FIRST TWA LINE                      
         A     R2,ATWA                                                          
         ZIC   R3,LSMTWNUM         R3=N'LIST/SELECT SCREEN LINES                
         LH    R4,=Y(LSMRTAB-LSMD)                                              
         LA    R4,LSMD(R4)                                                      
         USING LSMRTAB,R4          R4=A(RECORD TABLE)                           
*                                                                               
         MVC   APRECKEY,LSMLKEY    SET READ HIGH KEY VALUE                      
         CLI   ACPFPRE,0           TEST PREVIOUS PF KEY DEFINED                 
         BE    BLD8                                                             
         CLC   SCPFKEY,ACPFPRE     TEST PREVIOUS SCREEN REQUESTED               
         BNE   BLD8                                                             
         SR    R1,R1                                                            
         ICM   R1,1,LSMPRKNO                                                    
         BZ    BLD8                AT TOP OF LIST ALREADY                       
         BCTR  R1,0                                                             
         CLM   R1,1,LSMPRKLO       TEST OUTSIDE STACK                           
         BNL   *+12                                                             
         MVI   SCPFKEY,0           TREAT AS 'NEXT'                              
         B     BLDNXT                                                           
         STC   R1,LSMPRKNO                                                      
         LTR   R1,R1                                                            
         BZ    BLD8                WANT TOP OF LIST                             
         CLI   LSMPRKLO,0                                                       
         BE    BLD6                SIMPLE STACK                                 
         CLI   LSMPRKNO,LSMPRKMQ-1                                              
         BL    BLD6                                                             
         SR    RE,RE                                                            
         IC    RE,LSMPRKLO                                                      
         SR    R1,RE                                                            
         B     *+6                                                              
BLD6     BCTR  R1,0                                                             
         MH    R1,=Y(L'LSMPRKYS)                                                
         LA    RE,LSMPRKYS                                                      
         AR    RE,R1                                                            
         MVC   APRECKEY,0(RE)                                                   
*                                                                               
BLD8     CLI   ACLSMSV,0           TEST SCROLL VALUE SET                        
         BE    BLD10                                                            
         CLC   ACLSMSV,LSMTWACT    TEST SCROLL WITHIN DISPLAY RANGE             
         BNL   BLD10               <== WAS BH (BUG FIX RMOR 31/3/93)            
         ZIC   RF,ACLSMSV                                                       
         LA    RE,LSMRTABL                                                      
         MR    RE,RE                                                            
         LA    RF,LSMRTAB(RF)                                                   
         MVC   LSMLKEY,LSMRKEY-LSMRTAB(RF)                                      
         MVC   APRECKEY,LSMLKEY    SET READ HIGH KEY VALUE                      
*                                                                               
BLD10    MVI   LSMTWACT,0          RESET ACTUAL & CURRENT LINE NUMBERS          
         MVI   LSMTWCUR,0                                                       
         NI    LSMINDS,255-LSMIEOS                                              
*                                                                               
BLD12    GOTO1 APHOOK,APMGETS      GET NEXT LIST/SELECT RECORD                  
         BE    *+12                                                             
         TM    ACLSMIND,ACLSMIMP   TEST APPLICATION PROVIDED MESSAGE            
         BZ    LSMXIT                                                           
*                                                                               
         MVI   APINDS,APILNSEQ     SET NEXT GET (READ SEQUENTIAL)               
         CLI   APMODE,APMEOFS      TEST APPLICATION SET END OF LIST             
         BNE   *+12                                                             
         OI    LSMINDS,LSMIEOL     SET END OF LIST                              
         B     BLD24                                                            
         CLI   ACPFPRE,0                                                        
         BE    BLD18               NOT INTERESTED IN 'PREVIOUSES'               
         CLC   SCPFKEY,ACPFPRE     TEST PREVIOUS SCREEN REQUESTED               
         BE    BLD18               LEAVE STACK ALONE                            
         CLI   LSMTWACT,0          TEST FIRST GET FOR THIS LIST SCREEN          
         BNE   BLD18                                                            
*                                                                               
         CLI   LSMPRKNO,LSMPRKMQ                                                
         BNL   BLD16               STACK ALREADY FULL                           
BLD14    SR    R1,R1               ADD THIS KEY TO STACK                        
         IC    R1,LSMPRKNO                                                      
         LR    R0,R1                                                            
         CLI   LSMPRKNO,LSMPRKMQ                                                
         BL    *+8                                                              
         LA    R1,LSMPRKMQ-1       ALWAYS ADD AT END                            
         MH    R1,=Y(L'LSMPRKYS)                                                
         LA    RF,LSMPRKYS                                                      
         AR    RF,R1                                                            
         MVC   0(L'APRECKEY,RF),APRECKEY                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,LSMPRKNO                                                      
         B     BLD18                                                            
BLD16    LA    R0,LSMPRKYS                                                      
         LA    RE,LSMPRKYS+L'APRECKEY                                           
         LA    R1,(LSMPRKMQ-1)*L'LSMPRKYS                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SR    R1,R1                                                            
         IC    R1,LSMPRKNO                                                      
         LA    R1,2(R1)                                                         
         SH    R1,=Y(LSMPRKMQ)                                                  
         STC   R1,LSMPRKLO         SET STACK LOW PAGE                           
         B     BLD14                                                            
*                                                                               
BLD18    MVC   LSMLKEY,APRECKEY    SAVE KEY OF RECORD                           
         TM    LSMINDS,LSMIEOS     TEST END OF SCREEN                           
         BNZ   BLD24                                                            
*                                                                               
         BAS   RE,SAVRECV          SAVE RECORD VALUES                           
         ST    R2,APPARM           SET A(DISPLAY LINE FOR APPLICATION)          
         ST    R2,APLSTADD         IN APPARM(4) AND APLSTADD                    
         GOTO1 APHOOK,APMDISS      DISPLAY LIST/SELECT RECORD                   
         BNE   LSMXIT                                                           
         ZIC   R1,LSMTWACT         BUMP N'DISPLAYED LINES ON SCREEN             
         LA    R1,1(R1)                                                         
         STC   R1,LSMTWACT                                                      
         MVC   TWALSMA,LSMTWACT                                                 
*                                                                               
         SR    RE,RE               TURN PREVIOUSLY VALIDATED BITS ON            
         LH    R1,LSMTWLEN                                                      
         AR    R1,R2                                                            
BLD20    IC    RE,0(R2)            BUMP TO NEXT TWA FIELD                       
         AR    R2,RE                                                            
         CR    R2,R1               TEST END OF SELECT LINE                      
         BE    BLD22                                                            
         TM    FVATRB-FVIHDR(R2),FVAPROT                                        
         BNZ   BLD20                                                            
         TM    ACLSMIND,ACLSMIDV   TEST NOT SETTING VALIDATED BITS              
         BNZ   BLD20                                                            
         OI    FVIIND-FVIHDR(R2),FVIVAL                                         
         B     BLD20                                                            
*                                                                               
BLD22    LA    R4,LSMRTABL(R4)                                                  
         BCT   R3,BLD12            DO FOR NUMBER OF LINES ON SCREEN             
         OI    LSMINDS,LSMIEOS     SET END OF SCREEN                            
         B     BLD12               AND GET NEXT LIST/DISPLAY RECORD             
*                                                                               
BLD24    CLI   LSMTWACT,0          TEST N'DISPLAYED LINES NON-ZERO              
         BNE   BLDSEL                                                           
         TM    LSMINDS,LSMISEL     TEST ANYTHING DISPLAYED PREVIOUSLY           
         BNZ   BLDNXT                                                           
         MVC   FVADDR,ARECHDR      NO - SET CURSOR TO RECORD FIELD              
         MVC   FVMSGNO,=AL2(INFSEL1)                                            
         MVI   FVOMTYP,GTMINF                                                   
         B     LSMXIT                                                           
         EJECT                                                                  
LFM2     CLC   SCPFKEY,ACPFXIT     TEST QUIT PF KEY ENTERED                     
         BE    *+14                                                             
         CLC   SCPFKEY,ACPFRET                                                  
         BNE   *+12                                                             
         NI    TWALSCTL,255-(TWALSHLD+TWALSRTN) TURN OFF HOLD BITS              
         B     SEL2                                                             
         TM    TWALSCTL,TWALSRTN   TEST IF OVERLAY WANTS CONTROL                
         BNZ   *+8                 AFTER A DISPLAY                              
         TM    TWAMODE,TWAMDFC+TWAMDFR+TWAMERR+TWAMDFN                          
         BNZ   *+12                                                             
         MVI   SCPFKEY,255         LAST ACTION OK GET NEXT RECORD               
         B     SEL2                                                             
         NI    TWAMODE,255-TWAMERR                                              
         GOTO1 OVERLAY,LSMSOVR                                                  
         BNE   LSMXIT                                                           
         MVC   INACT,LSMSACT       SET RECORD ACTION                            
         MVC   INACTLFM,LSMSACTL   AND LFM ACTION NUMBER                        
         LH    R4,LSMRDSP                                                       
         LA    R4,LSMD(R4)                                                      
         BAS   RE,SETRECV          SET RECORD VALUES                            
         GOTO1 APHOOK,APMVALK      VALIDATE KEY OF DISPLAYED RECORD             
         BNE   LSMXIT                                                           
         CLI   APMODE,APMPFKS      TEST OVERLAY SET PF KEY VALUE                
         BNE   *+14                                                             
         CLC   SCPFKEY,ACPFXIT     YES - TEST FOR QUIT                          
         BE    SEL2                                                             
         MVC   SCWORK(1),INACT     TEST SELECT ACTION IS COPY/RENAME            
         CLI   INACT,ACTLFM8                                                    
         BNH   *+10                                                             
         MVC   SCWORK(1),INACTLFM                                               
         CLI   SCWORK,ACTCPY                                                    
         BE    *+12                                                             
         CLI   SCWORK,ACTREN                                                    
         BNE   LFM3                                                             
         TM    APINDS,APIOKADD     YES - TEST OK TO ADD RECORD                  
         BZ    *+12                                                             
         OI    TWAMODE,TWAMSEL                                                  
         B     LFM42                                                            
         MVC   FVADDR,AINKHDR      ERROR - RECORD ALREADY EXISITS               
         MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     LSMXIT                                                           
*                                                                               
LFM3     CLC   APRECKEY,SAVRECK    TEST SELECTED RECORD KEY CHANGED             
         BE    LFM4                                                             
         MVC   FVADDR,AACTHDR      YES - OUTPUT ERROR MESSAGE                   
         MVC   FVMSGNO,=AL2(FVFSKHC)                                            
         B     LSMXIT                                                           
*                                                                               
LFM4     OI    TWAMODE,TWAMSEL                                                  
         TM    APINDS,APIOKDIS     TEST OK TO DISPLAY RECORD                    
         BNZ   *+6                                                              
         DC    H'0'                MUST BE ABLE TO DISPLAY RECORD               
*                                                                               
LFM42    TM    ACLFMIND,ACHKBEF    TEST CONTROLLER BEFORE HOOK REQD             
         BZ    LFM44                                                            
         GOTO1 ACHOOK,ACMLFMR                                                   
         BNE   FILN                                                             
*                                                                               
LFM44    LA    RF,INACT                                                         
         CLI   INACT,ACTLFM8                                                    
         BNH   *+8                                                              
         LA    RF,INACTLFM                                                      
         CLI   0(RF),ACTCPY        COPY/RENAME DON'T RESTORE SAVED KEY          
         BE    FILS2                                                            
         CLI   0(RF),ACTREN                                                     
         BE    FILS2                                                            
         B     FILS                ALL OTHER ACTIONS DO                         
*                                                                               
LFM5     BAS   RE,SAVLIST          SAVE LIST SCREEN                             
         CLI   LSMSSCR,0           OVERLAY MAINTENANCE SCREEN                   
         BE    LFM5A               (IF REQUIRED)                                
         MVC   INSCRN,LSMSSCR                                                   
         GOTO1 SCOVSCR,ATWAEND                                                  
         BNE   LSMXIT                                                           
LFM5A    GOTO1 OVERLAY,LSMSOVR     AND MAINTENENCE OVERLAY                      
         BNE   LSMXIT                                                           
         BAS   RE,SETFLD           SET TWA FIELD ADDRESSES                      
         L     RF,ARECNTRY                                                      
         TM    RECINDS-RECTABD(RF),RECISET+RECISET2                             
         BZ    LFM6                                                             
         GOTO1 APHOOK,APMSETT      GIVE AMPSETT MODE IF REQUESTED               
*                                                                               
LFM6     GOTO1 APHOOK,APMDISK      DISPLAY RECORD KEY                           
         XC    SAVRECK,SAVRECK     CLEAR LAST TIME KEY VALUE                    
         MVC   INACT,LSMSACT       SET ACTION                                   
         L     R1,ARECHDR                                                       
         BAS   RE,CLRFLD                                                        
         L     R1,ARECNTRY                                                      
         GOTO1 AEXPREC                                                          
         L     R1,ARECHDR                                                       
         MVC   L'FVIHDR(L'SCRECNAM,R1),SCRECNAM                                 
         MVI   SCACTN,ACTSEL       SET ACTION TO 'SELECT'                       
         OI    TWAMODE,TWAMSEL                                                  
         TM    ACACTIND,ACHKBEF                                                 
         BO    LFM7                                                             
         L     R1,AACTHDR                                                       
         BAS   RE,CLRFLD                                                        
         BAS   RE,VALSUBA                                                       
         BE    *+6                                                              
         DC    H'0'                NO 'SELECT' ACTION TABLE ENTRY FOUND         
         L     R1,AACTNTRY                                                      
         GOTO1 AEXPACT                                                          
         L     R1,AACTHDR                                                       
         MVC   L'FVIHDR(L'SCACTNAM,R1),SCACTNAM                                 
         LA    RF,SAVAREA                                                       
         MVC   SAVACTN-SAVAREA(8,RF),SCACTNAM                                   
         B     LFM8                                                             
*                                                                               
LFM7     GOTO1 ACHOOK,ACMRECA      CONTROLLER HOOK TO HANDLE ACTION             
         BE    LFM8                                                             
         DC    H'0'                ACTION NOT RECOGNISED                        
*                                                                               
LFM8     MVC   INACTLFM,LSMSACTL   SET LFM ACTION NUMBER                        
         TM    ACLFMIND,ACHKBEF    TEST CONTROLLER HOOK REQUIRED                
         BZ    FILR                                                             
         GOTO1 ACHOOK,ACMLFMR                                                   
         BNE   FILN                                                             
         B     FILR                                                             
*                                                                               
ACTX     DS    0H                                                               
         LH    R1,LSMTWDSP         MAINTENANCE ACTION COMPLETE                  
         LA    R1,LSMTWSV(R1)                                                   
         LA    R0,L'FVIHDR                                                      
         SR    RE,RE                                                            
         ICM   RE,1,LSMSELLN                                                    
         BZ    LFM9                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
         MVI   L'FVIHDR(R1),C'*'   INDICATE ACTION COMPLETE                     
         CLI   APLSMSEL,0          TEST OVERRIDE COMPLETION CHARACTER           
         BE    *+14                                                             
         MVC   L'FVIHDR(1,R1),APLSMSEL                                          
         MVI   APLSMSEL,0          RESET COMPLETION CHARACTER                   
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         OI    FVATRB-FVIHDR(R1),FVAHIGH                                        
*                                                                               
LFM9     LH    R4,LSMRDSP                                                       
         LA    R4,LSMD(R4)                                                      
         BAS   RE,SAVRECV          SAVE RECORD VALUES                           
         CLI   SCFLAG,0            TEST RECORD DISPLAYED                        
         BE    LFMX                                                             
         TM    TWAMODE,TWAMDFN                                                  
         BNZ   LFMX                                                             
         TM    TWALSCTL,TWALSHLD   OVERLAY WANTS SCREEN HELD ON CRT             
         BO    LFM10                                                            
         CLI   SCPFKEY,0           ALLOW APPLICATION TO OVERRIDE PF KEY         
         BNE   *+8                                                              
         MVI   SCPFKEY,255         NO - GET NEXT RECORD                         
         NI    TWAMODE,255-(TWAMDFC+TWAMDFR)                                    
         NI    TWALSCTL,255-TWALSRTN                                            
         NI    LSMINDS,255-LSMISEL                                              
         OI    LSMINDS,LSMIDIS2    PASS APMDISS2 MODE IF REQUIRED               
         B     SEL2                                                             
*                                                                               
LFM10    MVC   FVADDR,AACTHDR                                                   
         TM    TWAMODE,TWAMDFC     TEST RECORD DISPLAYED FOR CHANGE             
         BZ    *+10                                                             
         MVC   FVADDR,AINDHDR      POINT TO FIRST DATA FIELD HEADER             
*                                                                               
LFMX     B     LSMXIT                                                           
         EJECT                                                                  
SEL2     BAS   RE,RESLIST          RESTORE LIST/SELECT SCREEN                   
         BAS   RE,XMTLIST                                                       
         NI    TWAMODE,TWAMNXT+TWAMLSM                                          
         MVC   INREC,LSMLREC       RESET RECORD/ACTION VALUES                   
         MVC   INACT,LSMLACT                                                    
         MVC   INSCRN,LSMLSCR                                                   
         GOTO1 OVERLAY,LSMLOVR     LOAD LIST OVERLAY                            
         BNE   LSMXIT                                                           
*                                                                               
         TM    LSMINDS,LSMIDIS2    TEST LFM ACTION COMPLETE                     
         BZ    SEL3                                                             
         NI    LSMINDS,255-LSMIDIS2                                             
         L     RF,ARECNTRY                                                      
         TM    RECINDS-RECTABD(RF),RECIDIS2                                     
         BNZ   *+12                                                             
         TM    APINDS2,APIMDIS2    TEST APPLICATION WANTS APMDISS2 MODE         
         BZ    SEL3                                                             
         L     R1,ATWAEND                                                       
         AH    R1,LSMTWDSP                                                      
         ST    R1,APPARM           SET A(TWA LINE FOR APPLICATION)              
         ST    R1,APLSTADD         IN APPARM(4) AND APLSTADD                    
         LA    R4,LSMD                                                          
         AH    R4,LSMRDSP                                                       
         BAS   RE,SETRECV          SET RECORD VALUES FOR APPLICATION            
         GOTO1 APHOOK,APMDISS2     PASS POST DISPLAY MODE TO APPLIC             
         BNE   LSMXIT                                                           
*                                                                               
SEL3     MVC   SCRECN,INREC        SET RECORD TYPE                              
         MVC   SCACTN,INACT                                                     
         TM    ACACTIND,ACHKBEF    CONTROLLER HOOK HANDLES ACTION               
         BO    SEL4                (AND RECORD TYPE)                            
         BAS   RE,VALSUBR                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ARECHDR                                                       
         BAS   RE,CLRFLD                                                        
         L     R1,ARECNTRY                                                      
         GOTO1 AEXPREC                                                          
         L     R1,ARECHDR                                                       
         MVC   L'FVIHDR(L'SCRECNAM,R1),SCRECNAM                                 
*                                                                               
         BAS   RE,VALSUBA          SET RECORD ACTION                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AACTHDR                                                       
         BAS   RE,CLRFLD                                                        
         L     R1,AACTNTRY                                                      
         GOTO1 AEXPACT                                                          
         L     R1,AACTHDR                                                       
         MVC   L'FVIHDR(L'SCACTNAM,R1),SCACTNAM                                 
         B     SEL6                                                             
*                                                                               
SEL4     GOTO1 ACHOOK,ACMRECA      CONTROLLER HOOK TO HANDLE ACTION             
         BE    SEL6                                                             
         DC    H'0'                ACTION NOT RECOGNISED                        
*                                                                               
SEL6     GOTO1 APHOOK,APMVALP      VALIDATE KEY                                 
         BNE   LSMXIT                                                           
         CLC   LSMUKEY,APRECKEY    TEST KEY CHANGED                             
         BE    *+14                                                             
         MVC   LSMUKEY,APRECKEY    YES - SET NEW KEY                            
         B     BLDFST                                                           
*                                                                               
         CLC   SCPFKEY,ACPFFST     TEST FIRST SCREEN REQUESTED                  
         BE    BLDFST                                                           
         CLC   SCPFKEY,ACPFCUR     TEST CURRENT SCREEN REQUESTED                
         BE    BLDSEL2                                                          
         CLC   SCPFKEY,ACPFNXT     TEST NEXT SCREEN REQUESTED                   
         BE    BLDNXT                                                           
         CLC   SCPFKEY,ACPFPRE     TEST PREVIOUS SCREEN REQUESTED               
         BE    BLDNXT                                                           
         CLC   SCPFKEY,ACPFXIT     TEST QUIT TO HIGHER LEVEL                    
         BE    BLDSEL2                                                          
         CLC   SCPFKEY,ACPFRET     TEST QUIT RECORD & PROCESS NEXT              
         BE    *+12                                                             
         CLI   SCPFKEY,255         TEST NEXT RECORD REQUIRED                    
         BNE   BLDNXT                                                           
         OI    LSMINDS,LSMISEL                                                  
         B     NXT2                                                             
         EJECT                                                                  
NXT2     NI    TWAMODE,255-TWAMSEL TURN OFF SELECT MODE                         
         LH    R2,LSMTWFST                                                      
         A     R2,ATWA             R2=A(FIRST LIST/SELECT LINE)                 
         LH    R3,LSMTWLEN                                                      
         AR    R3,R2               R3=A(END OF FIRST LIST/SELECT LINE)          
         LH    R4,=Y(LSMRTAB-LSMD)                                              
         LA    R4,LSMD(R4)                                                      
         USING LSMRTAB,R4          R4=A(RECORD TABLE)                           
         MVI   LSMTWCUR,0          LINE COUNTER                                 
*                                                                               
NXT4     ZIC   R1,LSMTWCUR         BUMP LINE COUNTER                            
         LA    R1,1(R1)                                                         
         CLM   R1,1,LSMTWACT                                                    
         BNH   NXT5                                                             
         GOTO1 APHOOK,APMLSCR                                                   
         BNE   LSMXIT                                                           
         CLI   APMODE,APMRET       TEST RETURN TO PREVIOUS REC/ACT              
         BE    LSM08                                                            
         CLI   SCPFKEY,255         TEST RETURN TO LIST SCREEN                   
         BE    BLDSEL                                                           
         CLI   APMODE,APMPFKS      TEST OVERLAY SET PF KEY VALUE                
         BE    *+8                                                              
         MVI   SCPFKEY,0                                                        
         CLC   SCPFKEY,ACPFFST                                                  
         BE    BLDFST                                                           
         CLC   SCPFKEY,ACPFNXT                                                  
         BE    BLDNXT                                                           
         CLC   SCPFKEY,ACPFPRE     TEST PREVIOUS SCREEN REQUESTED               
         BE    BLDNXT                                                           
         CLC   SCPFKEY,ACPFCUR                                                  
         BE    BLDSEL                                                           
         TM    SCFLAG,SCIANYPF     TEST USER ENTERED PF KEY                     
         BZ    BLDNXT              NO - SCROLL FORWARD                          
         B     BLDSEL                                                           
*                                                                               
NXT5     STC   R1,LSMTWCUR         SET CURRENT LINE NUMBER                      
         BAS   RE,SETRECV          SET RECORD VALUES                            
*                                                                               
         ST    R2,FVADDR                                                        
         LR    R1,R2               TEST FOR UNPS ON SELECT LINE                 
         SR    RE,RE                                                            
NXT6     IC    RE,0(R1)            BUMP TO NEXT TWA FIELD                       
         AR    R1,RE                                                            
         CR    R1,R3               TEST END OF SELECT LINE                      
         BE    NXT10                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   NXT6                                                             
         TM    ACLSMIND,ACLSMIDV   TEST NOT TESTING VALIDATED BITS              
         BNZ   *+12                                                             
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BNZ   NXT6                                                             
*                                                                               
         ST    R2,APPARM           SET A(TWA SELECT LINE)                       
         ST    R2,APLSTADD         IN APPARM(4) AND APLSTADD                    
         GOTO1 APHOOK,APMVALS      VALIDATE LIST/SELECT DATA                    
         BNE   LSMXIT                                                           
         BAS   RE,SAVRECV          SAVE RECORD VALUES                           
         ST    R2,FVADDR                                                        
*                                                                               
         LR    R1,R2               SET PREVIOUSLY VALIDATED BITS IN TWA         
         SR    RE,RE                                                            
NXT8     IC    RE,0(R1)            BUMP TO NEXT TWA FIELD                       
         AR    R1,RE                                                            
         CR    R1,R3               TEST END OF DISPLAY LINE                     
         BE    NXT10                                                            
         TM    ACLSMIND,ACLSMIDN   TEST DON'T TOUCH INTENSITY                   
         BNZ   *+8                                                              
         OI    FVATRB-FVIHDR(R1),FVAHIGH                                        
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   NXT8                                                             
         TM    ACLSMIND,ACLSMIDV   TEST NOT SETTING VALIDATED BITS              
         BNZ   *+8                                                              
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         B     NXT8                                                             
*                                                                               
NXT10    TM    FVATRB-FVIHDR(R2),FVAPROT                                        
         BNZ   NXT12                                                            
         MVC   SCRECN,LSMLREC      SET RECORD NUMBER (TYPE)                     
         CLI   LSMRNUM,0                                                        
         BE    *+10                                                             
         MVC   SCRECN,LSMRNUM                                                   
         NI    SCINDS,255-SCIALL   SET ALL NOT ACTIVE FOR THIS LINE             
         MVC   SCSELCOD,FVIFLD-FVIHDR(R2)                                       
         CLC   SCSELCOD(1),SCQUEST TEST HELP REQUIRED                           
         BNE   *+8                                                              
         BAS   RE,HLP              HELP HOOK                                    
         CLI   SCSELCOD,C'-'       IGNORE ALL BUT                               
         BE    NXT12                                                            
         CLI   SCSELCOD,C'*'       IGNORE ALREADY PROCESSED                     
         BE    NXT12                                                            
         CLI   FVIFLD+1-FVIHDR(R2),C'+'                                         
         BNE   NXT11                                                            
         MVC   LSMALLCD,SCSELCOD   SET ALL SELECT CODE                          
         OI    LSMINDS,LSMISEOS    SET SELECT TO END-OF-SCREEN                  
         MVC   LSMAACT,LSMSACT                                                  
         LR    R1,R2                                                            
         S     R1,ATWA                                                          
         STH   R1,LSMADSP          SET DISPLACEMENT TO THIS LINE                
         B     NXT14                                                            
*                                                                               
NXT11    CLI   SCSELCOD,C' '                                                    
         BNH   *+12                                                             
         CLI   FVILEN-FVIHDR(R2),0                                              
         BNE   NXT14                                                            
         TM    LSMINDS,LSMISEOS    TEST 'SELECT ALL' SET                        
         BZ    NXT12                                                            
         LR    R1,R2               YES - TEST THIS FIELD AFTER ACTION+          
         S     R1,ATWA                                                          
         CH    R1,LSMADSP                                                       
         BL    NXT12                                                            
         OI    SCINDS,SCIALL       SET ALL ACTIVE FOR THIS LINE                 
         MVC   SCSELCOD,LSMALLCD   SET SELECTION CODE                           
         B     NXT14                                                            
*                                                                               
NXT12    LR    R2,R3               BUMP TO NEXT SELECT LINE                     
         AH    R3,LSMTWLEN                                                      
         LA    R4,LSMRTABL(R4)                                                  
         B     NXT4                                                             
*                                                                               
NXT14    L     RF,ACSELTAB         VALIDATE SELECTION CODE                      
         NI    SCINDS,X'FF'-SCINOACC                                            
         USING SELTABD,RF                                                       
NXT16    CLI   SELCODE,EOT         TEST END OF SELECT TABLE                     
         BE    NXT30                                                            
         CLC   SELRECB,SCRECN      MATCH ON RECORD/ACTION                       
         BNE   NXT20                                                            
         CLC   SELACTB,LSMLACT                                                  
         BNE   NXT20                                                            
         CLC   SCSELCOD,SELCODE                                                 
         BNE   NXT20                                                            
         TM    ACLSMIND,ACLSMISK   TEST EXTENDED SELECT TABLE                   
         BZ    NXT18                                                            
         CLI   SELLANG,0           TEST LANGUAGE FILTER SET                     
         BE    *+14                                                             
         CLC   SELLANG,CULANG                                                   
         BNE   NXT20                                                            
         OI    SCINDS,SCISELOK     SET SELECT ACTION OK (IN PRINCIPAL)          
         ICM   RE,3,SELMASK        TEST RECORD MASK PRESENT                     
         BZ    NXT18                                                            
         MVC   SCWORK(L'SELMASK),SELMASK                                        
         NC    SCWORK(L'SELMASK),APRECID                                        
         CLC   SCWORK(L'SELMASK),SELMASK                                        
         BNE   NXT20                                                            
NXT18    ST    RF,ASELNTRY         SAVE A(SELECT TABLE ENTRY)                   
         MVC   SCACTN,SELACTN      SAVE RECORD/ACTION NUMBER                    
         MVC   SCRECN,SELRECN                                                   
         B     NXT28                                                            
NXT20    LA    R0,SELTABL                                                       
         TM    ACLSMIND,ACLSMISK   TEST TABLE CONTAINS KEYS                     
         BZ    *+8                                                              
         LA    R0,SELTAB2L                                                      
         AR    RF,R0               BUMP TO NEXT SELTAB ENTRY                    
         B     NXT16                                                            
*                                                                               
NXT28    TM    ACACTIND,ACHKBEF    VALIDATE RECORD TYPE                         
         BO    NXT29               CONTROLLER HOOK TO BE USED                   
         BAS   RE,VALSUBR                                                       
         BNE   NXT30                                                            
         BAS   RE,VALSUBA          VALIDATE ACTION                              
         BNE   NXT30                                                            
         BAS   RE,VALSUBM          VALIDATE RECORD/ACTION COMBO                 
         BE    NXT34                                                            
         B     NXT30                                                            
*                                                                               
NXT29    GOTO1 ACHOOK,ACMRECA      CONTROLLER HOOK RECORD/ACTION                
         BE    NXT34                                                            
*                                                                               
NXT30    TM    SCINDS,SCIALL       TEST SELECT ALL ACTIVE                       
         BNZ   NXT12                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLC   SCSELCOD(1),SCQUEST NOTHING VALID IF QUESTION MARK INPUT         
         BE    LSMXIT                                                           
         MVC   FVMSGNO,=AL2(FVFESEL)                                            
         TM    SCINDS,SCISELOK     TEST SELECT ACTION OK                        
         BZ    LSMXIT                                                           
         TM    SCINDS,SCINOACC     TEST NO AUTHORIZATION FOR SELECTION          
         BZ    NXT32                                                            
         MVC   FVMSGNO,=AL2(FVFNASEL)                                           
         NI    LSMINDS,X'FF'-LSMISEOS                                           
         B     LSMXIT                                                           
NXT32    MVC   FVMSGNO,=AL2(FVFESEL2)                                           
         TM    LSMINDS,LSMISEOS    TEST SELECT TO END-OF-SCREEN                 
         BNZ   NXT12                                                            
         B     LSMXIT                                                           
*                                                                               
NXT34    L     RF,AMIXNTRY         ENSURE OPTIONS ARE OK                        
         USING MIXTABD,RF          RF=A(MIX TABLE ENTRY)                        
         TM    ACOPTIND,ACNOVAL                                                 
         BNZ   NXT40                                                            
         MVC   SCWORK(L'INOPTR),MIXOPTX                                         
         NC    SCWORK(L'INOPTR),INOPTI                                          
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKINV)                                            
         B     NXT36                                                            
         MVC   SCWORK(L'INOPTR),INOPTI                                          
         NC    SCWORK(L'INOPTR),MIXOPTR                                         
         XC    SCWORK(L'INOPTR),MIXOPTR                                         
         BZ    NXT40                                                            
         MVC   FVMSGNO,=AL2(FVFREQD)                                            
*                                                                               
NXT36    L     R1,ACOPTTAB         LOCATE MISSING/INVALID OPTION                
         USING OPTTABD,R1                                                       
         SR    R0,R0                                                            
NXT38    CLI   OPTTABD,EOT         END OF TABLE IS BAD                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SCWORK+L'INOPTR(L'INOPTR),OPTOPTB                                
         NC    SCWORK+L'INOPTR(L'INOPTR),SCWORK                                 
         BNZ   *+14                                                             
         IC    R0,OPTELEN                                                       
         AR    R1,R0               BUMP TO NEXT TABLE ENTRY                     
         B     NXT38                                                            
         GOTO1 AEXPOPT                                                          
         MVI   FVXTRA,C'-'                                                      
         MVC   FVXTRA+2(L'SCOPTNAM),SCOPTNAM                                    
         B     LSMXIT                                                           
         DROP  R1                                                               
*                                                                               
NXT40    MVC   TWALSREC,SCRECN     SAVE RECORD/ACTION VALUES                    
         MVC   TWALSACT,SCACTN                                                  
         MVC   LSMSREC,SCRECN                                                   
         MVC   LSMSACT,SCACTN                                                   
         MVC   LSMSACTL,INACTLFM                                                
         MVC   LSMSSCR,MIXSCRN                                                  
         MVC   LSMSOVR,MIXOVER                                                  
         MVC   LSMSMIX1,MIXINDS                                                 
         MVC   LSMSMIX2,MIXINDS2                                                
*                                                                               
         MVC   INREC,MIXRECB       SET RECORD/ACTION VALUES                     
         MVC   INMIX1,MIXINDS                                                   
         MVC   INMIX2,MIXINDS2                                                  
         MVC   INKEYN,MIXKEYN                                                   
         MVC   INKEYC,MIXKEYC                                                   
         MVC   INKEYT,MIXKEYT                                                   
*                                                                               
         LR    R1,R2                                                            
         L     R0,ATWAEND                                                       
         SR    R1,R0                                                            
         STH   R1,LSMTWDSP         SET DISPLACEMENT TO TWA LINE                 
         LR    R1,R4                                                            
         LA    R0,LSMD                                                          
         SR    R1,R0                                                            
         STH   R1,LSMRDSP          SET DISPLACEMENT TO LSMRTAB NTRY             
         DROP  RF                                                               
         BAS   RE,SETRECV          SET RECORD VALUES                            
*                                                                               
         CLI   LSMSELLN,2          TEST 2 BYTE SELECT ACTION                    
         BL    NXT42                                                            
         CLI   FVIFLD+1-FVIHDR(R2),C'-'                                         
         BNE   NXT42                                                            
         NI    LSMINDS,255-LSMISEOS                                             
*                                                                               
NXT42    TM    INMIX1,MIXILST                                                   
         BZ    NXT42A                                                           
         TM    ACLSMIND,ACLSMISK   TEST EXTENDED SELECT TABLE                   
         BZ    NXT45                                                            
         L     RF,ASELNTRY                                                      
         TM    SELINDS-SELTABD(RF),SELIPROC TEST APMPROC MODE REQUIRED          
         BZ    NXT45                                                            
         MVC   INACT,LSMSACT       PROCESS LIST SELECT ACTION                   
         MVC   INACTLFM,LSMSACTL                                                
         ST    R2,APPARM           SET A(DISPLAY LINE) FOR APPLICATION          
         ST    R2,APLSTADD         IN APPARM(4) AND APLSTADD                    
         GOTO1 APHOOK,APMPROC                                                   
         BE    NXT45                                                            
         B     NXT44                                                            
NXT42A   TM    INMIX1,MIXISEL+MIXILFM                                           
         BZ    NXT45                                                            
         TM    INMIX1,MIXILFM      TEST FILE MAINTENANCE ACTION                 
         BZ    NXT43                                                            
         TM    ACLSMIND,ACLSMISK   TEST EXTENDED SELECT TABLE                   
         BZ    LFM5                                                             
         L     RF,ASELNTRY                                                      
         TM    SELINDS-SELTABD(RF),SELIPROC TEST APMPROC MODE REQUIRED          
         BZ    LFM5                                                             
         MVC   INACT,LSMSACT       PROCESS LIST SELECT ACTION                   
         MVC   INACTLFM,LSMSACTL                                                
         ST    R2,APPARM           SET A(DISPLAY LINE) FOR APPLICATION          
         ST    R2,APLSTADD         IN APPARM(4) AND APLSTADD                    
         GOTO1 APHOOK,APMPROC                                                   
         BE    LFM5                                                             
         B     NXT44                                                            
*                                                                               
NXT43    MVC   INACT,LSMSACT       PROCESS LIST SELECT ACTION                   
         MVC   INACTLFM,LSMSACTL                                                
         ST    R2,APPARM           SET A(DISPLAY LINE) FOR APPLICATION          
         ST    R2,APLSTADD         IN APPARM(4) AND APLSTADD                    
         GOTO1 APHOOK,APMPROC                                                   
         BNE   NXT44                                                            
         BAS   RE,SAVRECV          SAVE RECORD VALUES                           
         CLI   APMODE,APMPLFM      TEST FOR LFM TYPE ACTION NOW                 
         BE    LFM5                YES - GO BACK AND DO IT                      
         BAS   RE,SAVLIST          SAVE LIST SCREEN                             
         MVI   SCFLAG,1                                                         
         B     ACTX                                                             
*                                                                               
NXT44    MVC   INREC,LSMLREC       RESTORE RECORD & ACTION NUMBER               
         MVC   INACT,LSMLACT                                                    
         B     LSMXIT                                                           
*                                                                               
NXT45    TM    INMIX1,MIXILST      TEST LIST OVERLAY                            
         BZ    NXT50                                                            
         GOTO1 APHOOK,APMPUTK      PUT KEY COMPONENTS                           
         BAS   RE,SAVLIST          SAVE CURRENT LIST                            
*                                                                               
         TM    APINDS2,APINXOPT                                                 
         BZ    NXT48                                                            
         LH    RE,=Y(LSMINOPT-LSMD)                                             
         LA    RE,LSMD(RE)                                                      
         MVC   0(L'LSMINOPT,RE),INOPTS  SAVE INTERNAL VALUES                    
         LH    RE,=Y(LSMOPTS-LSMD)                                              
         LA    RE,LSMD(RE)                                                      
         L     R1,AOPTHDR                                                       
         SR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R1)                                             
         BCTR  RF,0                                                             
         EX    RF,*+8              SAVE OPTIONS AS INPUT + HEADER               
         B     *+10                                                             
         MVC   0(0,RE),0(R1)                                                    
         MVI   L'FVIHDR(R1),C' '   CLEAR OPTIONS INPUT FIELD                    
         LA    RE,L'FVIHDR+1                                                    
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         LA    RE,8(RE)                                                         
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR+1(0,R1),L'FVIHDR(R1)                                    
         OI    FVIIND-FVIHDR(R1),FVITHIS                                        
*                                                                               
NXT48    GOTO1 VDMGR,SCPARM,DMWRITE,TEMPSTR,(SCSELLVL,0),LSMD                   
         BAS   RE,SETRECV          SET RECORD VALUES                            
         MVC   INACT,LSMSACT                                                    
         CLI   LSMSSCR,0           OVERLAY MAINTENANCE SCREEN                   
         BE    NXT48A              (IF REQUIRED)                                
         MVC   INSCRN,LSMSSCR                                                   
         GOTO1 SCOVSCR,ATWAEND                                                  
         BNE   LSMXIT                                                           
NXT48A   GOTO1 OVERLAY,LSMSOVR     OVERLAY SELECT PHASE                         
         BNE   LSMXIT                                                           
         BAS   RE,SETFLD           SET TWA FIELD ADDRESSES                      
         LA    R0,LSMD             CLEAR LSMD                                   
         LH    R1,=Y(LSMX-LSMD)                                                 
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    INOPTS,INOPTS       CLEAR OPTIONS TOO                            
         L     R1,ARECHDR                                                       
         BAS   RE,CLRFLD                                                        
         XC    INOPTI,INOPTI                                                    
         L     R1,ARECNTRY                                                      
*                                                                               
         MVC   INOPTR,RECOPTR-RECTABD(R1)                                       
         MVC   INOPTX,RECOPTX-RECTABD(R1)                                       
         GOTO1 AEXPREC                                                          
         L     R1,ARECHDR                                                       
         MVC   L'FVIHDR(L'SCRECNAM,R1),SCRECNAM                                 
         L     R1,AACTHDR                                                       
         BAS   RE,CLRFLD                                                        
         L     R1,AACTNTRY                                                      
         OC    INOPTR,ACTOPTR-ACTTABD(R1)                                       
         OC    INOPTX,ACTOPTX-ACTTABD(R1)                                       
         GOTO1 AEXPACT                                                          
         L     R1,AACTHDR                                                       
         MVC   L'FVIHDR(L'SCACTNAM,R1),SCACTNAM                                 
         L     R1,AMIXNTRY                                                      
         OC    INOPTR,MIXOPTR-MIXTABD(R1)                                       
         OC    INOPTX,MIXOPTX-MIXTABD(R1)                                       
*                                                                               
         MVI   SCCTYP,SCCINTQ                                                   
         BAS   RE,VALOPT                                                        
         BE    LSM04               GO TO NEXT LIST LEVEL                        
         ZIC   R1,SCSELLVL         BUMP TO NEXT LIST NEST LEVEL                 
         LA    R1,1(R1)                                                         
         LA    R0,TWAMLVL                                                       
         CR    R1,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,SCSELLVL         SET CURRENT LIST/SELECT NEST LEVEL           
         NI    TWAMODE2,255-TWAMLVL                                             
         OC    TWAMODE2,SCSELLVL                                                
         B     FVERR                                                            
*                                                                               
NXT50    TM    INMIX1,MIXIREP      TEST REPORT OVERLAY                          
         BNZ   REP                                                              
         DC    H'0'                OTHER                                        
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GIVE HELP FOR SELECT FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
HLP      ST    RE,SCFULL           SAVE RETURN ADDRESS (FOR ERROR)              
         XC    APELEM,APELEM                                                    
         LA    R1,APELEM           R1=A(OUTPUT AREA)                            
         MVC   SCDUB,SCRECN                                                     
*                                                                               
         L     R2,ACSELTAB         VALIDATE SELECTION CODE                      
         USING SELTABD,R2                                                       
HLP2     CLI   SELCODE,EOT         TEST END OF SELECT TABLE                     
         BE    HLP20                                                            
         CLC   SELRECB,SCDUB       MATCH ON RECORD NUMBER                       
         BNE   HLP18                                                            
         CLC   SELACTB,LSMLACT     AND ON INPUT ACTION NUMBER                   
         BNE   HLP18                                                            
         TM    ACLSMIND,ACLSMISK   TEST EXTENDED SELECT TABLE                   
         BZ    HLP4                                                             
         CLI   SELLANG,0           TEST LANGUAGE FILTER SET                     
         BE    *+14                                                             
         CLC   SELLANG,CULANG                                                   
         BNE   HLP18                                                            
         ICM   RE,3,SELMASK        TEST RECORD MASK PRESENT                     
         BZ    HLP4                                                             
         MVC   SCWORK(L'SELMASK),SELMASK                                        
         NC    SCWORK(L'SELMASK),APRECID                                        
         CLC   SCWORK(L'SELMASK),SELMASK                                        
         BNE   HLP18                                                            
*                                                                               
HLP4     MVC   SCRECN,SELRECN      SET RECORD TYPE & ACTION                     
         MVC   SCACTN,SELACTN                                                   
*                                                                               
         BAS   RE,VALSUBR          VALIDATE RECORD TYPE                         
         BNE   HLP18                                                            
         BAS   RE,VALSUBA          VALIDATE ACTION                              
         BNE   HLP18                                                            
         BAS   RE,VALSUBM          VALIDATE COMBO                               
         BNE   HLP18                                                            
*                                                                               
         MVC   0(1,R1),SELCODE     BUILD OUTPUT ENTRY                           
         MVC   1(1,R1),SCEQUAL                                                  
         CLC   SCRECN,SCDUB        TEST DIFFERENT RECORD TYPE                   
         BE    HLP14                                                            
         LR    R0,R1                                                            
         L     R1,ARECNTRY                                                      
         GOTO1 AEXPREC                                                          
         LR    R1,R0                                                            
         MVC   2(L'SCRECNAM,R1),SCRECNAM                                        
         LA    R1,2+L'SCRECNAM-1(R1)                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(1,R1),SCSLASH                                                  
HLP14    LR    R0,R1                                                            
         L     R1,AACTNTRY                                                      
         GOTO1 AEXPACT                                                          
         LR    R1,R0                                                            
         MVC   2(L'SCACTNAM,R1),SCACTNAM                                        
         LA    R1,2+L'SCACTNAM-1(R1)                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         CLC   0(1,R1),SCEQUAL     TEST FOR BLANK ACTION NAME                   
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   0(1,R1),SCCOMMA                                                  
         LA    R1,1(R1)                                                         
*                                                                               
HLP18    LA    R0,SELTABL                                                       
         TM    ACLSMIND,ACLSMISK   TEST TABLE CONTAINS KEYS                     
         BZ    *+8                                                              
         LA    R0,SELTAB2L                                                      
         AR    R2,R0               BUMP TO NEXT SELTAB ENTRY                    
         B     HLP2                                                             
         DROP  R2                                                               
*                                                                               
HLP20    LA    RE,APELEM                                                        
         CR    RE,R1                                                            
         BNE   *+10                                                             
         L     RE,SCFULL                                                        
         BR    RE                                                               
         BCTR  R1,0                                                             
         MVI   0(R1),C' '          REMOVE DELIMITER                             
         L     RF,AMSGHDR                                                       
         MVC   L'FVIHDR(60,RF),0(RE)                                            
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     LSMXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXECUTE SWAPPING BETWEEN SCREENS                         *         
* IF APMODE IS SET TO APMSWP OR APMRET, SWAPPING WILL BE ATTEMPTED.   *         
* APPARM(1) MUST BE SET TO THE RECORD CODE AND APPARM+1(1) TO THE     *         
* ACTION CODE.                                                        *         
* EXIT - CC=NOT EQUAL IF NOT SWAPPING                                 *         
***********************************************************************         
         SPACE 1                                                                
SWAP     LR    R0,RE                                                            
         CLI   APMODE,APMRET       TEST RETURN TO PREVIOUS REC/ACT              
         BE    *+12                                                             
         CLI   APMODE,APMSWP       OR SWAP TO NEW RECORD/ACTION                 
         BNE   SWAPNO              NO-EXIT                                      
         MVC   SCRECN,APPARM                                                    
         MVC   SCACTN,APPARM+1                                                  
         TM    ACACTIND,ACHKBEF                                                 
         BO    SWAP02                                                           
         BAS   RE,VALSUBR                                                       
         BNE   SWAPNO                                                           
         BAS   RE,VALSUBA                                                       
         BNE   SWAPNO                                                           
         BAS   RE,VALSUBM                                                       
         BNE   SWAPNO                                                           
         B     SWAP04                                                           
*                                                                               
SWAP02   GOTO1 ACHOOK,ACMRECA                                                   
         BNE   SWAPNO                                                           
*                                                                               
SWAP04   L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3                                                       
         CLI   APMODE,APMSWP       CAN ONLY SWAP TO LFM, REP OR                 
         BNE   *+16                                      LIST SCREEN            
         TM    MIXINDS,MIXILFM+MIXIREP+MIXILST                                  
         BZ    SWAPNO                                                           
         B     SWAP06                                                           
         TM    MIXINDS,MIXILFM+MIXILST  CAN ONLY RETURN TO LFM OR               
         BZ    SWAPNO                   LIST SCREEN                             
*                                                                               
SWAP06   LR    RE,R0               SWAP IS OK - CC EQ                           
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
SWAPNO   LTR   RE,R0               SWAP IS NOT OK - CC NE                       
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* THIS CODE ACTUALLY EXECUTES THE SWAP OR RETURN                      *         
***********************************************************************         
         SPACE 1                                                                
SWAP2    NI    TWAMODE,255-TWAMDFR-TWAMDFN                                      
         MVC   INREC,SCRECN                                                     
         MVC   INACT,SCACTN                                                     
         MVC   INSCRN,MIXSCRN                                                   
         MVC   INMIX1,MIXINDS                                                   
         MVC   INMIX2,MIXINDS2                                                  
         MVC   INKEYN,MIXKEYN                                                   
         MVC   INKEYC,MIXKEYC                                                   
         MVC   INKEYT,MIXKEYT                                                   
         MVC   INDRLIND,MIXINDRL                                                
*                                                                               
         TM    TWAMODE,TWAMLSM                                                  
         BZ    SWAP4                                                            
         L     RC,ALSM                                                          
         USING LSMD,RC                                                          
         MVC   LSMSREC,SCRECN                                                   
         MVC   LSMSACT,SCACTN                                                   
         MVC   LSMSACTL,INACTLFM                                                
         MVC   LSMSOVR,MIXOVER                                                  
         MVC   LSMSMIX1,INMIX1                                                  
         MVC   LSMSMIX2,INMIX2                                                  
*                                                                               
SWAP4    CLI   APMODE,APMRET                                                    
         BE    SWAP6                                                            
         GOTO1 SCOVSCR,ATWAEND                                                  
*                                                                               
SWAP6    GOTO1 OVERLAY,MIXOVER                                                  
         DROP  R3                                                               
*                                                                               
         ICM   R1,15,ARECHDR                                                    
         BZ    SWAP8                                                            
         BAS   RE,CLRFLD                                                        
         L     R1,ARECNTRY                                                      
         GOTO1 AEXPREC                                                          
         L     R1,ARECHDR                                                       
         MVC   L'FVIHDR(L'SCRECNAM,R1),SCRECNAM                                 
*                                                                               
SWAP8    ICM   R1,15,AACTHDR                                                    
         BZ    SWAP12                                                           
         BAS   RE,CLRFLD                                                        
         TM    TWAMODE,TWAMLSM     TEST IN LSM MODE                             
         BZ    SWAP10                                                           
         TM    INMIX1,MIXILFM      AND SWAPPING TO LFM SCREEN                   
         BZ    SWAP10                                                           
         MVI   SCACTN,ACTSEL       YES-CHANGE ACTION TO SELECT                  
         BAS   RE,VALSUBA                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TWALSREC,SCRECN     SAVE SELECT RECORD/ACTION VALUES             
         MVC   TWALSACT,SCACTN                                                  
*                                                                               
SWAP10   L     R1,AACTNTRY                                                      
         GOTO1 AEXPACT                                                          
         L     R1,AACTHDR                                                       
         MVC   L'FVIHDR(L'SCACTNAM,R1),SCACTNAM                                 
*                                                                               
SWAP12   TM    INMIX1,MIXILFM      SWAP/RETURN TO LFM SCREEN                    
         BZ    SWAP20                                                           
         CLI   APMODE,APMSWP                                                    
         BNE   SWAP14                                                           
         GOTO1 APHOOK,APMDISK                                                   
         XC    SAVRECK,SAVRECK                                                  
         B     SWAP16                                                           
*                                                                               
SWAP14   GOTO1 APHOOK,APMVALK                                                   
         BNE   FILN                                                             
*                                                                               
SWAP16   TM    ACLFMIND,ACHKBEF                                                 
         BZ    SWAP18                                                           
         GOTO1 ACHOOK,ACMLFMR                                                   
         BNE   FILN                                                             
*                                                                               
SWAP18   CLI   APMODE,APMDISK                                                   
         BE    FILR                                                             
         B     FILS                                                             
*                                                                               
SWAP20   TM    INMIX1,MIXIREP      SWAP TO REPORT ACTION                        
         BZ    SWAP22                                                           
         CLI   APMODE,APMSWP                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    INMIX2,MIXIDRV                                                   
         BZ    REP                                                              
         TM    INDRLIND,MIXIDRLS                                                
         BZ    REP                                                              
         GOTO1 APHOOK,APMDISK                                                   
         XC    SAVRECK,SAVRECK                                                  
         B     REP                                                              
*                                                                               
SWAP22   TM    INMIX1,MIXILST      SWAP/RETURN TO LIST SCREEN                   
         BO    LSM                                                              
         DS    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SUNDRY LIST/SELECT MONITOR SUBROUTINES                              *         
***********************************************************************         
         SPACE 1                                                                
CLRFLD   ZIC   RF,0(R1)                                                         
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
CLRFLDX  BR    RE                                                               
         SPACE 2                                                                
         USING LSMRTAB,R4                                                       
SAVRECV  MVC   LSMRKEY,APRECKEY    SAVE RECORD VALUES                           
         MVC   LSMRDA,APRECDA                                                   
         MVC   LSMRID,APRECID                                                   
         MVC   LSMRNUM,APRECNUM                                                 
SAVRECVX BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
         USING LSMRTAB,R4                                                       
SETRECV  MVC   APRECKEY,LSMRKEY    SET RECORD VALUES                            
         MVC   APRECDA,LSMRDA                                                   
         MVC   APRECID,LSMRID                                                   
         MVC   APRECNUM,LSMRNUM                                                 
SETRECVX BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
         USING UTLD,RE                                                          
SAVLIST  STM   RE,R1,SCSVRER1      SAVE LIST SCREEN                             
         L     RE,SCAUTL                                                        
         LA    RF,SAVAREA                                                       
         MVC   SAVSCRM-SAVAREA(L'SAVSCRM,RF),TSCRNE                             
         LA    R0,LSMTWSV                                                       
         L     RE,ATWAEND                                                       
         B     SAVRESL                                                          
         SPACE 1                                                                
RESLIST  STM   RE,R1,SCSVRER1      RESTORE LIST SCREEN                          
         GOTO1 VPROTOFF                                                         
         L     RE,SCAUTL                                                        
         LA    RF,SAVAREA                                                       
         MVC   TSCRNE(L'SAVSCRM),SAVSCRM-SAVAREA(RF)                            
         GOTO1 VPROTON                                                          
         L     R0,ATWAEND                                                       
         LA    RE,LSMTWSV                                                       
         SPACE 1                                                                
SAVRESL  LH    R1,=Y(LSMTWSVL)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
SAVRESLX LM    RE,R1,SCSVRER1                                                   
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUNDRY LIST/SELECT MONITOR SUBROUTINES - CONTINUED                  *         
***********************************************************************         
         SPACE 1                                                                
XMTLIST  LR    R0,RE               TRANSMIT LIST/SELECT SCREEN                  
         L     R1,AMSGHDR                                                       
         LH    RF,=Y(LSMTWSVL)                                                  
         A     RF,ATWAEND                                                       
         SR    RE,RE                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,0(R1)                                                       
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         XC    1(2,R1),1(R1)                                                    
         CLC   TWASCRN,LSMLSCR     TEST CLEAR BEFORE/AFTER REQUIRED             
         BE    *+12                                                             
         MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
XMTLISTX LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLRLIST  ST    RE,12(RD)           CLEAR & TRANSMIT LIST SCREEN                 
         LH    R1,LSMTWFST                                                      
         A     R1,ATWA             R1=A(START OF LIST SCREEN)                   
         LH    RF,LSMTWEND                                                      
         A     RF,ATWA                                                          
         BCTR  RF,0                RF=A(END OF LIST SCREEN-1)                   
         LA    R0,L'FVIHDR                                                      
         SR    RE,RE                                                            
CLRLIST2 ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RE,R0                                                            
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+6                                                              
         SR    RE,R0                                                            
         BCTR  RE,0                RE=FIELD EXECUTE LENGTH                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
         MVI   FVIIND,0                                                         
         MVI   FVILEN,0                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         NI    FVOIND-FVIHDR(R1),255-FVOCUR                                     
         TM    ACLSMIND,ACLSMIDN   TEST DON'T TOUCH INTENSITY                   
         BNZ   *+8                                                              
         NI    FVATRB-FVIHDR(R1),255-FVAHIGH                                    
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,CLRLIST2                                                   
CLRLISTX L     RE,12(RD)                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUNDRY LIST/SELECT MONITOR SUBROUTINES - CONTINUED                  *         
***********************************************************************         
         SPACE 1                                                                
VALSUBR  NTR1  ,                                                                
         L     R2,ACRECTAB                                                      
         USING RECTABD,R2          LOCATE RECTAB ENTRY                          
*                                                                               
VALSUBR2 CLI   RECTABD,EOT         TEST E-O-T                                   
         BE    VALSUBN                                                          
         SR    RE,RE               APPLY COUNTRY FILTER (IF ANY)                
         ICM   RE,1,ACRECNDX                                                    
         BZ    VALSUBR4                                                         
         LA    RE,RECUSER-1(RE)    ACRECNDX IS 1 BASED VALUE                    
         CLI   0(RE),0             TEST VALID FOR ALL COUNTRIES                 
         BE    VALSUBR4                                                         
         CLC   CUCTRY,0(RE)        MATCH ON COUNTRY CODE                        
         BNE   VALSUBR6                                                         
*                                                                               
VALSUBR4 CLC   RECNUMB,SCRECN                                                   
         BNE   VALSUBR6                                                         
         GOTO1 TSTAUTH,RECAUTH     TEST USER AUTHORISED                         
         BNE   VALSUBR6                                                         
         TM    RECINDS,RECIDDS     TEST DDS ONLY RECORD TYPE                    
         BZ    *+16                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALSUBR6                                                         
         B     VALSUBR5                                                         
         GOTO1 TSTSECR,RECNUMB                                                  
         BNE   VALSUBR6                                                         
VALSUBR5 ST    R2,ARECNTRY                                                      
         B     VALSUBY                                                          
*                                                                               
VALSUBR6 SR    R0,R0               BUMP TO NEXT TABLE ENTRY                     
         IC    R0,RECELEN                                                       
         AR    R2,R0                                                            
         B     VALSUBR2                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SUNDRY LIST/SELECT MONITOR SUBROUTINES - CONTINUED                  *         
***********************************************************************         
         SPACE 1                                                                
VALSUBA  NTR1  ,                                                                
         L     R2,ACACTTAB                                                      
         USING ACTTABD,R2          LOCATE ACTTAB ENTRY                          
*                                                                               
VALSUBA2 CLI   ACTTABD,EOT                                                      
         BE    VALSUBN                                                          
         CLC   ACTNUMB,SCACTN      MATCH ACTION NUMBER                          
         BNE   VALSUBA4                                                         
         TM    ACTINDS,ACTIDDS     TEST DDS ONLY ACTION TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALSUBA4                                                         
         GOTO1 TSTAUTH,ACTAUTH     TEST USER AUTHORISED                         
         BNE   VALSUBA4                                                         
         ST    R2,AACTNTRY                                                      
         MVC   INACTLFM,ACTUSER    SET LFM ACTION NUMBER                        
         B     VALSUBY                                                          
*                                                                               
VALSUBA4 LA    R2,ACTTABL(R2)                                                   
         B     VALSUBA2                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SUNDRY LIST/SELECT MONITOR SUBROUTINES - CONTINUED                  *         
***********************************************************************         
         SPACE 1                                                                
VALSUBM  NTR1  ,                                                                
         L     R2,ACMIXTAB                                                      
         USING MIXTABD,R2          LOCATE MIXTAB ENTRY                          
*                                                                               
VALSUBM2 CLI   MIXELEN,EOT         TEST END OF TABLE                            
         BE    VALSUBN                                                          
         CLC   MIXRECB,SCRECN      MATCH RECORD                                 
         BNE   VALSUBM8                                                         
         TM    MIXINDS,MIXIDDS     TEST DDS ONLY COMBO                          
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST IF A DDS TERMINAL                 
         BZ    VALSUBM8                                                         
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MONITOR ACTIVE              
         BNZ   VALSUBM3                                                         
         TM    MIXINDS,MIXIREP     DON'T SUPPORT LSM ONLY REPORT                
         BNZ   VALSUBM3            (MIXILSM REUSED FOR REPORT ACTIONS)          
         TM    MIXINDS2,MIXILSM    TEST LIST/SELECT COMBO ONLY                  
         BNZ   VALSUBM8                                                         
VALSUBM3 LA    R1,MIXACTB          POINT TO ACTION NUMBER                       
         LA    R0,1                                                             
         CLI   MIXACTB,0           TEST IF ACTION LIST IN MIXTAB ENTRY          
         BNE   *+12                                                             
         LA    R1,MIXACTS          POINT TO ACTION LIST                         
         LA    R0,L'MIXACTS                                                     
*                                                                               
VALSUBM4 CLI   0(R1),0             TEST END-OF-LIST                             
         BE    VALSUBM8                                                         
         CLC   SCACTN,0(R1)        MATCH ACTION NUMBER                          
         BE    *+16                                                             
         LA    R1,1(R1)            BUMP TO NEXT ACTION LIST ENTRY               
         BCT   R0,VALSUBM4                                                      
         B     VALSUBM8                                                         
         GOTO1 TSTAUTH,MIXAUTH     TEST IF USER IS AUTHORISED                   
         BNE   VALSUBM8                                                         
         TM    MIXINDS,MIXIDDS     DON'T TEST SECURITY IF DDS ONLY              
         BO    VALSUBM6                                                         
         GOTO1 TSTSECM,SCRECN                                                   
         BNE   VALSUBM8                                                         
VALSUBM6 ST    R2,AMIXNTRY                                                      
         B     VALSUBY                                                          
*                                                                               
VALSUBM8 SR    R0,R0               BUMP TO NEXT TABLE ENTRY                     
         IC    R0,MIXELEN                                                       
         AR    R2,R0                                                            
         B     VALSUBM2                                                         
         DROP  R2                                                               
         SPACE 2                                                                
VALSUBY  CR    RB,RB                                                            
         XIT1  ,                                                                
         SPACE 1                                                                
VALSUBN  LTR   RB,RB                                                            
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL APPLICATION CONTROLLER HOOK                         *         
*                                                                     *         
* ON ENTRY TO APPLICATION CONTROLLER THE FOLLOWING VALUES ARE SET     *         
*                                                                     *         
* ACMODE     - CALLING MODE (ACMODE EQUATE)                           *         
***********************************************************************         
         SPACE 1                                                                
ACHOOK   NTR1  ,                   CALL APPLICATION CONTROLLER HOOK             
         STC   R1,ACMODE                                                        
         ICM   RF,15,ACAHOOK       RF=A(CONTROLLER HOOK)                        
         BZ    ACHOOKX                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         ICM   RE,15,ACWORKA       RE=A(CONTROLLER RD VALUE)                    
         BZ    ACHOOKX                                                          
         LM    R0,RC,20(RE)        RESTORE REGISTERS 0 THRU 12                  
         BASR  RE,RF                                                            
ACHOOKX  XIT1  ,                                                                
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO CALL APPLICATION OVERLAY                                 *         
*                                                                     *         
* ON ENTRY TO APPLICATION OVERLAY THE FOLLOWING VALUES ARE SET        *         
*                                                                     *         
* R5         - ADDRESS OF TERMINAL WORK AREA     (TWAD)               *         
* R6         - ADDRESS OF GLOBAL SAVE STORAGE    (SAVAREA)            *         
* R7         - ADDRESS OF GLOBAL WORKING STORAGE (WORKD)              *         
* APMODE     - CALLING MODE                      (APMODE EQUATE)      *         
* APACTN     - CALLING ACTION                    (ACTNUMB EQUATE)     *         
***********************************************************************         
         SPACE 1                                                                
APHOOK   LR    R0,RE               CALL APPLICATION OVERLAY                     
         STC   R1,APMODE                                                        
         TM    APINDS2,APIXCKEY    TEST IF CLEAR OF APRECKEY REQUIRED           
         BZ    APHOOK2                                                          
         CLI   APMODE,APMVALK      IF VALIDATE KEY                              
         BE    *+8                                                              
         CLI   APMODE,APMVALP      OR VALIDATE LIST/SELECT PARAMETERS           
         BNE   *+10                                                             
         XC    APRECKEY,APRECKEY   CLEAR RECORD KEY BEFORE CALL                 
                                                                                
APHOOK2  MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   APMODE,APMVALK      IF VALIDATE KEY                              
         BNE   *+8                                                              
         NI    APINDS2,X'FF'-APINOSOX                                           
         MVC   APACTN,INACT        SET ACTION CODE                              
         LA    R6,SAVAREA                                                       
         GOTO1 APNTRYA                                                          
         L     R6,SCBASE5                                                       
         TM    APINDS2,APIOVROK                                                 
         BZ    *+16                                                             
         NI    APINDS2,X'FF'-(APIOVROK)                                         
         CLI   *+1,0                                                            
         B     APHOOKX                                                          
         CLI   APMODE,APMFMOK                                                   
         BE    *+10                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
APHOOKX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ TWA PAGE INTO WORKING STORAGE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING LSMD,RC                                                          
READTWA  LR    R0,RE                                                            
         MVC   SCPARM+20(2),=C'L='                                              
         MVC   SCPARM+22(2),=Y(14*1024)                                         
         GOTO1 VDMGR,SCPARM,DMREAD,TEMPSTR,(SCSELLVL,0),LSMD                    
         MVC   TWALSMN,LSMTWNUM                                                 
         MVC   TWALSMA,LSMTWACT                                                 
         LR    RE,R0                                                            
READTWAX BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO TEST USER AUTHORISATION                                  *         
*                                                                     *         
* NTRY - R1=A(TABLE AUTHORISATION BYTES)                              *         
* EXIT - CC=EQUAL IF AUTHORISED, NOT EQUAL IF NOT AUTHORISED          *         
***********************************************************************         
         SPACE 1                                                                
TSTAUTH  CLI   ASONOFF,ASOFF       SUPPRESS AUTH TESTING OFFLINE                
         BER   RE                                                               
         OC    ACASEC,ACASEC                                                    
         BZ    *+10                                                             
         CLI   *+1,0                                                            
         BR    RE                                                               
         MVC   SCWORK(2),0(R1)                                                  
         NC    SCWORK(2),CUAUTH                                                 
         CLC   SCWORK(2),0(R1)                                                  
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO TEST SECURTIY ACCESS FOR RECORD TYPE                     *         
*                                                                     *         
* NTRY - R1=A(RECORD TYPE)                                            *         
* EXIT - CC=EQUAL IF AUTHORISED, NOT EQUAL IF NOT AUTHORISED          *         
***********************************************************************         
         SPACE 1                                                                
TSTSECR  OC    ACASEC,ACASEC                                                    
         BZR   RE                                                               
         CLI   ASONOFF,ASOFF       SUPPRESS AUTH TESTING OFFLINE                
         BER   RE                                                               
         LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTO1 VSECRET,SCPARM,('SECPRCD',ACASEC),(RF)                           
         BE    *+8                                                              
         OI    SCINDS,SCINOACC                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO TEST SECURTIY ACCESS FOR RECORD/ACTION COMBINATION       *         
*                                                                     *         
* NTRY - R1=A(RECORD/ACTION)                                          *         
* EXIT - CC=EQUAL IF AUTHORISED, NOT EQUAL IF NOT AUTHORISED          *         
***********************************************************************         
         SPACE 1                                                                
TSTSECM  OC    ACASEC,ACASEC                                                    
         BZR   RE                                                               
         CLI   ASONOFF,ASOFF       SUPPRESS AUTH TESTING OFFLINE                
         BER   RE                                                               
         CLI   1(R1),ACTSEL        SUPPRESS TEST FOR ACTION SELECT              
         BER   RE                                                               
         LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTO1 VSECRET,SCPARM,('SECPRACT',ACASEC),(0(RF),1(RF))                 
         BE    *+8                                                              
         OI    SCINDS,SCINOACC                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO TEST SECURTIY ACCESS FOR OPTION                          *         
*                                                                     *         
* NTRY - R1=A(OPTION NUMBER)                                          *         
* EXIT - CC=EQUAL IF AUTHORISED, NOT EQUAL IF NOT AUTHORISED          *         
***********************************************************************         
         SPACE 1                                                                
TSTSECO  OC    ACASEC,ACASEC                                                    
         BZR   RE                                                               
         CLI   ASONOFF,ASOFF       SUPPRESS AUTH TESTING OFFLINE                
         BER   RE                                                               
         LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTO1 VSECRET,SCPARM,('SECPOPTP',ACASEC),(RF)                          
         BE    *+8                                                              
         OI    SCINDS,SCINOACC                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO LOAD APPLICATION OVERLAY                                 *         
*                                                                     *         
* NTRY - R1=A(PHASE NUMBER TO BE LOADED)                              *         
***********************************************************************         
         SPACE 1                                                                
OVERLAY  LR    R0,RE                                                            
         CLI   0(R1),0             ENSURE OVERLAY NUMBER IS RESOLVED            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   INOVER,0(R1)        TEST OVERLAY ALREADY LOADED                  
         BER   RE                                                               
         MVC   INOVER,0(R1)                                                     
         GOTO1 VCOLY,SCPARM,(INOVER,0),0,0                                      
         BAS   RE,CHKOLY           TEST OVERLAY LOADED OK                       
         BNE   *+10                                                             
         MVC   APNTRYA,0(R1)                                                    
OVERLAYX LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO LOAD CORE RESIDENT PHASES                                *         
*                                                                     *         
* NTRY - R1=A(PHASE NUMBER LIST)                                      *         
*        R3=A(PHASE ADDRESS LIST)                                     *         
*        R4=N'PHASES IN LIST                                          *         
***********************************************************************         
         SPACE 1                                                                
LOADPHS  NTR1  ,                                                                
         ICM   R0,14,=X'D9000A'                                                 
         LR    R2,R1                                                            
         LA    R1,SCPARM                                                        
         L     RF,VCOLY                                                         
*                                                                               
LOADPHS2 CLI   0(R2),X'FF'                                                      
         BE    LOADPHSX                                                         
         ICM   R0,1,0(R2)                                                       
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,LOADPHS2                                                      
*                                                                               
LOADPHSX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT AND PRE-VALIDATE A TWA INPUT FIELD               *         
*                                                                     *         
* NTRY - R1=A(TWA FIELD HEADER)                                       *         
*        FVMINL=MINIMUM INPUT FIELD LENGTH (ZERO=OPTIONAL FIELD)      *         
*        FVMAXL=MAXIMUM INPUT FIELD LENGTH (ZERO=MAXIMUM LENGTH)      *         
*        FVHELP=HELP OVERLAY FIELD NUMBER  (ZERO=NO HELP AVAILABLE)   *         
*        FVXTRA=NARRATIVE TO BE ATTACHED TO ERROR IF FIELD IS INVALID *         
*                                                                     *         
* NTR AT APFVAL FOR NORMAL TWA FIELD PROCESSING                       *         
*        FVALSCAN FOR SPECIAL FIELD PROCESSING (SEE OPTVAL)           *         
*                                                                     *         
* EXIT - FVADDR=A(TWA FIELD HEADER)                                   *         
*        FVINDX=ZERO                                                  *         
*        FVSUBX=ZERO                                                  *         
*        FVMINL=ZERO                                                  *         
*        FVMAXL=ZERO                                                  *         
*        FVHELP=ZERO                                                  *         
*        FVXTRA=SPACES                                                *         
*        FVIHDR=EXTRACTED INPUT FIELD HEADER (SEE FVIHDR IN WORKD)    *         
*        FVIFLD=EXTRACTED & SPACE FILLED INPUT FIELD                  *         
*        FVMSGNO=SET TO STANDARD ERROR NUMBER (SEE FVMSGNO EQUATES)   *         
*        CC=LOW IF FIELD IS NOT INPUT                                 *         
*        CC=EQUAL IF FIELD IS INPUT AND VALID                         *         
*        CC=HIGH IF INPUT TOO SHORT/LONG ETC.                         *         
*                                                                     *         
* NOTE - HELP ROUTINE WILL BE ENTERED DIRECTLY IF A QUESTION MARK OR  *         
*        'HE(LP)' IS INPUT ONLY IF FVHELP IS NON-ZERO. ENTRY POINT    *         
*        SCFVAL CAN BE USED ONLY BY THE SYSTEM CONTROLLER.            *         
***********************************************************************         
         SPACE 1                                                                
SCFVAL   MVI   FVFLAG,X'00'                                                     
         L     R1,0(R1)                                                         
         B     FVAL                                                             
         SPACE 1                                                                
APFVAL   NTR1  BASE=SCBASE1,LABEL=NO                                            
         L     RA,SCBASE2                                                       
         L     R9,SCBASE3                                                       
         L     R8,SCBASE4                                                       
         L     R6,SCBASE5                                                       
         MVI   FVFLAG,X'01'                                                     
         LTR   R1,R1               TEST A(TWA FIELD HEADER) PASSED              
         BNZ   FVAL                                                             
         LR    RF,R0               SET FIELD LENGTH IN R0                       
         B     FVALSCAN                                                         
         SPACE 1                                                                
FVAL     ST    R1,FVADDR           SET A(INPUT FIELD HEADER)                    
         MVI   FVINDX,0            RESET INDEX & SUB-INDEX VALUES               
         MVI   FVSUBX,0                                                         
         MVI   FVOMTYP,0           RESET MESSAGE TYPE                           
         MVI   FVIFLD,C' '                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         MVC   FVIHDR,0(R1)        EXTRACT FIELD HEADER                         
         ZIC   RF,FVTLEN                                                        
         LA    R0,L'FVIHDR+1                                                    
         TM    FVATRB,FVAXTND                                                   
         BZ    *+8                                                              
         LA    R0,L'FVIHDR+L'FVIHDR+1                                           
         SR    RF,R0               RF=MAXIMUM INPUT LENGTH-1                    
         BNM   *+6                                                              
         DC    H'0'                THIS IS A BAD TWA FIELD                      
         LA    R1,L'FVIHDR(R1)                                                  
         TM    FVATRB,FVADDTB      TEST DELETE TRAILING BLANKS                  
         BNZ   FVAL1                                                            
         TM    ACVALIND,ACVALIDB   TEST DELETE LEADING BLANKS                   
         BZ    FVAL1                                                            
         LTR   RF,RF               TEST ONE BYTE FIELD                          
         BZ    FVAL1                                                            
         CLI   0(R1),C' '          LOOK FOR FIRST NON-BLANK BYTE                
         BH    FVAL1                                                            
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
FVAL1    EX    RF,*+8              EXTRACT FIELD DATA                           
         B     FVALSCAN                                                         
         MVC   FVIFLD(0),0(R1)                                                  
*                                                                               
FVALSCAN LA    R1,FVIFLD(RF)       R1=A(END OF INPUT FIELD)                     
         LA    RF,1(RF)            RF=LOOP COUNT                                
FVAL2    CLI   0(R1),C' '          LOCATE LAST INPUT CHARACTER IN FIELD         
         BH    FVAL4                                                            
         MVI   0(R1),C' '          SET FUNNIES TO SPACES                        
         BCTR  R1,0                                                             
         BCT   RF,FVAL2                                                         
FVAL4    STC   RF,FVILEN           SET ACTUAL INPUT LENGTH                      
         MVC   FVMSGNO,=AL2(FVFSHRT) ENSURE NOT TOO SHORT OR LONG               
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         CLM   RF,1,FVMINL                                                      
         BL    FVALERR                                                          
         CLI   FVMAXL,0            IF FVMAXL=ZERO DON'T TEST LONG               
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         CLM   RF,1,FVMAXL                                                      
         BH    FVALERR                                                          
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         LTR   RF,RF               EXIT IF NO INPUT IN FIELD                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     FVAL14                                                           
*                                  SET FIELD VALIDITY BITS                      
         MVC   FVMSGNO,=AL2(FVFOK) INDICATE FIELD IS OK                         
         OI    FVIIND,FVINUM+FVIALF+FVIHEX                                      
FVAL6    TM    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BZ    FVAL10                                                           
         CLI   0(R1),C'A'                                                       
         BNL   *+12                                                             
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         B     FVAL8                                                            
         CLI   0(R1),C'Z'                                                       
         BNH   *+12                                                             
         NI    FVIIND,255-FVIALF                                                
         B     FVAL8                                                            
         NI    FVIIND,255-FVINUM                                                
         CLI   0(R1),C'F'                                                       
         BNH   *+8                                                              
         NI    FVIIND,255-FVIHEX                                                
FVAL8    BCTR  R1,0                                                             
         BCT   RF,FVAL6                                                         
FVAL10   IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SET EXECUTE LENGTH (INPUT LENGTH-1)          
         TM    FVIIND,FVINUM                                                    
         BZ    FVAL12                                                           
         CLI   FVILEN,8            TEST INPUT NOT LONGER THAN 8 BYTES           
         BNH   *+12                                                             
         NI    FVIIND,255-FVINUM                                                
         B     FVAL12                                                           
         EX    RF,*+8              SET PACKED/BINARY NUMERIC VALUES             
         B     *+10                                                             
         PACK  SCDUB,FVIFLD(0)                                                  
         CVB   R0,SCDUB                                                         
         ST    R0,SCFULL                                                        
FVAL12   MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    FVFLAG,X'01'        TEST SYSTEM CONTROLLER CALL                  
         BNZ   FVAL14              YES - HE HANDLES HIS OWN HELP                
         CLI   FVHELP,0            TEST HELP SUPPORTED FOR THIS FIELD           
         BE    FVAL14                                                           
         TM    ACINDS,ACINOHLP     TEST APPL CONTROL SAYS NO HELP               
         BO    FVAL14                                                           
         CLC   FVIFLD(1),SCQUEST   TEST IF QUESTION MARK WAS INPUT              
         BE    HELP                                                             
         CLI   FVILEN,2                                                         
         BL    FVAL14                                                           
         CLI   FVILEN,L'SCHELP                                                  
         BH    FVAL14                                                           
         EX    RF,*+8                                                           
         BE    HELP                                                             
         CLC   FVIFLD(0),SCHELP    OR 'HE(LP)' WAS INPUT                        
*                                                                               
FVAL14   MVI   FVHELP,0            RESET THIS TIME VALUES                       
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVI   FVXTRA,C' '                                                      
         MVC   FVXTRA+1(L'FVXTRA-1),FVXTRA                                      
         B     FVALX                                                            
*                                  HANDLE ERRORS HERE                           
FVALERR  TM    FVFLAG,X'01'        TEST SYSTEM CONTROLLER FIELD                 
         BZ    FVERR               YES - GO TO ERROR ROUTINE                    
*                                                                               
FVALX    TM    FVFLAG,X'01'        TEST SYSTEM CONTROLLER CALL                  
         BZ    *+8                                                              
         LA    RE,EXIT             NO - SET RE=A(EXIT)                          
         CLC   FVMSGNO,=AL2(FVFNONE)                                            
         BE    FVALX2                                                           
         MVI   FVFLAG,0                                                         
         CLI   FVILEN,0                                                         
         BE    FVALXX                                                           
         MVI   FVFLAG,1                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    FVALXX                                                           
FVALX2   MVI   FVFLAG,2                                                         
FVALXX   CLI   FVFLAG,1            SET CONDITION CODE FOR CALLER                
         MVI   FVFLAG,0                                                         
         BR    RE                  RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE, FIELD INDEX INFO & EXTRA MESSAGE   *         
*                                                                     *         
* NTRY - FVADDR=A(FIELD HEADER OF FIELD IN ERROR)                     *         
*        FVMSGNO=FIELD ERROR NUMBER                                   *         
*        FVFLAG=ZERO IF A STANDARD CONTROLLER ERROR MESSAGE REQUIRED  *         
*        FVOSYS=OVERRIDE SYSTEM FOR GETTXT CALL (ZERO=STANDARD)       *         
*        FVINDX=MULTIPLE FIELD INDEX NUMBER                           *         
*        FVSUBX=MULTIPLE FIELD SUB-INDEX NUMBER                       *         
*        FVXTRA=USER SUPPLIED MESSAGE TO TACK ONTO GENERAL MESSAGE    *         
*                                                                     *         
* NTR AT FVERR0 TO SET MULTIPLE FIELD INDEX VALUES TO ZERO            *         
*        FVERR  FOR REGULAR MESSAGE BUILDING                          *         
*        FVERRX ONLY TO SET CURSOR TO FIELD ADDRESSED BY FVADDR       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
FVERR0   MVI   FVINDX,0            ENTRY POINT FOR NO INDEX INFO                
         MVI   FVSUBX,0                                                         
FVERR    CLC   FVMSGNO,=AL2(FVFSET) TEST USER HAS SUPPLIED MESSAGE              
         BE    FVERRX                                                           
         LA    R3,SCPARM           DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R3                                                       
         CLC   FVMSGNO,=AL2(FVFGTSET)   TEST APPL SET GETTXT BLOCK              
         BNE   *+12                                                             
         LA    R3,APPARM           APPLICATION HAS DEFINED BLOCK                
         B     FVGTTXT                                                          
*                                                                               
         XC    GTBLOCK,GTBLOCK                                                  
         CLI   ASONOFF,ASOFF                                                    
         BNE   *+10                                                             
         MVC   GTAOUT,AMSGHDR+1                                                 
         MVC   GTINDX,FVINDX                                                    
         MVC   GTSUBX,FVSUBX                                                    
         MVC   GTMSGNO,FVMSGNO                                                  
         MVC   GTMSYS,FVOSYS       OVERRIDE SYSTEM (IF SET)                     
         CLI   GTMSYS,0            TEST OVERRIDE SYSTEM SET                     
         BNE   *+10                                                             
         MVC   GTMSYS,ASSYSO       NO - SET NATIVE SYSTEM                       
         MVC   GTMTYP,FVOMTYP      OVERRIDE MESSAGE TYPE (IF SET)               
         CLI   GTMSGNO,X'FF'       STD CONTROLLER MSG (1-253)                   
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,X'FF'        GENERAL SYSTEM MESSAGE                       
         OC    GTMSGNO,GTMSGNO     MESSAGE 0 = DATAMGR ERR                      
         BNZ   FVERR10                                                          
         LA    R1,IODMCB                                                        
         STCM  R1,7,GTADMCB                                                     
         OI    GT1INDS,GT1DMGRE                                                 
*                                                                               
FVERR10  LA    R1,FVXTRA+L'FVXTRA-1                                             
         LA    R0,L'FVXTRA                                                      
         CLI   0(R1),C' '                                                       
         BH    *+14                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         B     FVERR20                                                          
         LA    R1,FVXTRA                                                        
         STCM  R1,7,GTATXT         SET LENGTH & ADDRESS OF EXTRA TEXT           
         STCM  R0,1,GTLTXT                                                      
*                                                                               
FVERR20  CLC   FVMSGNO,=AL2(FVFEKEY)   TEST 'ENTER KEY' MESSAGE                 
         BE    *+14                                                             
         CLC   FVMSGNO,=AL2(FVFEKEYD)  TEST 'ENTER KEY & DATA' MESSAGE          
         BNE   FVGTTXT                                                          
         L     R1,SCAUTL                                                        
         TM    TSTAT6-UTLD(R1),TST6STFU                                         
         BNZ   FVGTTXT             LEAVE PREFIX IF FULL-BLOWN STEREO            
         OI    GT1INDS,GT1NOREF    SWITCH OFF REFERENCE                         
         B     FVGTTXT                                                          
         SPACE 1                                                                
FVOKEX   LA    R3,SCPARM           SCPARM DEFINED INTERNALLY                    
         CLI   GTMSGNO,X'FF'       CHECK FOR GENERAL MESSAGES                   
         BNE   FVGTTXT                                                          
         MVI   GTMSYS,X'FF'        FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
         SPACE 1                                                                
FVGTTXT  GOTO1 VGETTXT,GETTXTD                                                  
         DROP  R3                                                               
FVERRX   L     R1,AMSGHDR          TRANSMIT HEADER MESSAGE                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         L     R1,AINP             TEST IF OVERLAY SET CURSOR                   
         TM    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         BNZ   GENX                                                             
         ICM   R1,15,APCURSOR      TEST CURSOR ADDRESS SET                      
         BNZ   *+12                                                             
         ICM   R1,15,FVADDR        TEST IF OVERLAY SET FIELD ADDRESS            
         BZ    GENX                                                             
         OI    FVOIND-FVIHDR(R1),FVOCUR                                         
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         AR    R1,RE               POINT TO NEXT FIELD HEADER                   
         LA    RF,4095(R1)         RF=POINT PAST END OF TWA                     
         ICM   RE,1,0(R1)          TURN OFF CURSORS TO BOTTOM OF TWA            
         BZ    GENX                                                             
         NI    FVOIND-FVIHDR(R1),255-FVOCUR                                     
         BXLE  R1,RE,*-12                                                       
         B     GENX                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GIVE HELP FOR AN INPUT FIELD VALIDATED BY CONTROLLER     *         
*                                                                     *         
* NTRY - FVFLAG=ZERO IF A CONTROLLER VALIDATED FIELD (EG RECORD)      *         
*        FVHELP=FIELD NUMBER THAT HELP IS REQUIRED FOR                *         
***********************************************************************         
         SPACE 1                                                                
HELP     L     R1,FVADDR                                                        
         S     R1,ATWA             R1=DISPLACEMENT TO FIELD IN TWA              
         CLI   FVHELP,HELPKEY      TEST HELP FOR KEY REQUIRED                   
         BE    HELP40              YES - NO NEED TO OVERLAY HELP SCREEN         
         CLI   FVHELP,HELPOPT      HELP FOR OPTIONS ?                           
         BNE   HELP2               NO,  CONTINUE                                
         NI    TWAMODE,TWAMNXT+TWAMLSM+TWAMSEL                                  
         TM    TWAMODE,TWAMLSM     IN   LIST/SELECT MODE ?                      
         BZ    HELP3               NO,  CONTINUE                                
         OI    TWALSCTL,TWALSMSV   SAVE LIST/SELECT MODE                        
         B     HELP3               CONTINUE                                     
*                                                                               
HELP2    NI    TWAMODE,TWAMNXT                                                  
*                                                                               
HELP3    OI    SCINDS,SCIGO                                                     
         CLC   TWASCRN,ACHLPSCR    TEST HELP SCREEN IS LOADED                   
         BE    *+10                                                             
         XC    TWAHDISP,TWAHDISP                                                
         CLM   R1,3,TWAHDISP                                                    
         BE    *+8                                                              
         MVI   TWAHMORE,0          SET TO HELP START                            
         STCM  R1,3,TWAHDISP                                                    
         MVI   TWASCRN,0           FORCE NO MATCH                               
         MVC   INSCRN,ACHLPSCR     LOAD HELP SCREEN                             
         GOTO1 SCOVSCR,ATWAEND                                                  
         BNE   FVERR                                                            
*                                                                               
HELP4    CLI   FVFLAG,0            TEST CONTROLLER VALIDATED FIELD              
         BE    *+6                                                              
         DC    H'0'                YES - THIS SHOULD NOT HAPPEN                 
         SR    R4,R4                                                            
         ICM   R4,1,TWAHMORE       R4=INDEX TO NEXT HELP ENTRY                  
         LR    R0,R4               R0=INDEX TO NEXT HELP ENTRY                  
         L     R3,ATWAEND                                                       
         USING HELPTWAD,R3         R3=A(HELP SCREEN)                            
         SPACE 1                                                                
         ZIC   R1,FVHELP           FIND HELP TEXT NUMBER                        
         SLL   R1,1                2 LINES OF HEADINDGS                         
         AH    R1,=AL2(GENHELP)    GENHELP+2=START NO OF GENERAL HELP           
         LA    R2,SCPARM           DEFINE GETTXT BLOCK                          
         USING GETTXTD,R2                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         STCM  R1,3,GTMSGNO                                                     
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GTMSYS,X'FF'        GENERAL SCREEN MESSAGE                       
         LA    R1,HELPH1H                                                       
         STCM  R1,7,GTAOUT                                                      
*                                                                               
         GOTO1 VGETTXT,GETTXTD                                                  
         SPACE 1                                                                
         CLI   GTMAXL,0            TEST RETURNED 'NOT FOUND'                    
         BE    HELP5               CAN'T PUT OUT HELP HEADINGS                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,GTMSGNO        GET SECOND TEXT LINE                         
         LA    R1,1(R1)                                                         
         STCM  R1,3,GTMSGNO                                                     
         LA    R1,HELPH2H                                                       
         STCM  R1,7,GTAOUT                                                      
         MVI   GTMAXL,0            WLL BE SET TO L'MSG (1ST CALL)               
         MVI   GT1INDS,0           MAY BE SET ON RETURN FROM 1ST CALL           
         MVI   GT2INDS,0                                                        
         STCM  R1,7,GTAOUT                                                      
         GOTO1 (RF),GETTXTD                                                     
         DROP  R2                                                               
         SPACE 1                                                                
HELP5    CLI   FVHELP,HELPREC                                                   
         BE    HELP6                                                            
         CLI   FVHELP,HELPACT                                                   
         BE    HELP20                                                           
         CLI   FVHELP,HELPOPT                                                   
         BE    HELP60                                                           
         DC    H'0'                INVALID FVHELP SETTING                       
         EJECT                                                                  
***********************************************************************         
* HELP FOR RECORD TYPE                                                *         
***********************************************************************         
         SPACE 1                                                                
HELP6    L     R2,ACRECTAB                                                      
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
         SR    R1,R1                                                            
         LTR   R4,R4               TEST HELP NEXT                               
         BZ    *+14                                                             
         IC    R1,RECELEN          YES - BUMP TO NEXT ENTRY                     
         AR    R2,R1                                                            
         BCT   R0,*-6                                                           
         LA    R3,HELPL1H          R3=A(FIRST HELP DATA LINE HEADER)            
HELP8    CLI   RECTABD,EOT         TEST E-O-T                                   
         BNE   *+10                                                             
         SR    R4,R4               NO MORE TO COME                              
         B     HELPX                                                            
         TM    RECINDS,RECINOH     TEST DO NOT GIVE HELP                        
         BNZ   HELP18                                                           
         MVC   SCRECN,RECNUMB                                                   
         BAS   RE,VALSUBR          VALIDATE RECORD TYPE                         
         BNE   HELP18                                                           
HELP10   CLI   0(R3),L'HELPL1H+L'HELPL1                                         
         BL    HELPX               EXIT IF END OF TWA REACHED                   
         LR    R0,R1                                                            
         GOTO1 AEXPREC,RECTABD                                                  
         LR    R1,R0                                                            
         MVC   8(3,R3),SCRECNAM                                                 
         LA    R1,9(R3)            SET FOR 3-BYTE NAME DISP                     
         CLI   SCRECNAM+3,C' '     TEST 3-BYTE RECORD NAME                      
         BE    HELP14                                                           
         MVI   11(R3),C'('         NO - SHOW REMAINDER IN BRACKETS              
         MVC   12(5,R3),SCRECNAM+3                                              
         LA    R1,16(R3)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
HELP14   CLI   SCRECSHT,C' '       TEST FOR SHORT NAME                          
         BE    HELP15                                                           
         CLC   SCRECSHT,8(R3)      DIFFERENT TO LONG                            
         BE    HELP15                                                           
         MVC   2(1,R1),SCSLASH                                                  
         MVC   3(L'SCRECSHT,R1),SCRECSHT                                        
HELP15   TM    RECINDS,RECITXT     TEST GETTXT HELP                             
         BZ    HELP16                                                           
         LA    R1,SCPARM                                                        
         USING GETTXTD,R1          BUILD GETTXT BLOCK                           
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,RECHELP                                                  
         MVI   GTMAXL,L'RECHELP                                                 
         LA    R0,SCWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT                                                          
         MVC   24(L'RECHELP,R3),SCWORK                                          
         B     HELP17                                                           
         DROP  R1                                                               
HELP16   ZIC   R1,RECELEN          MOVE TEXT TO HELP LINE                       
         SH    R1,=Y(RECTABL+1)                                                 
         BM    HELP17                                                           
         EX    R1,*+8                                                           
         B     HELP17                                                           
         MVC   24(0,R3),RECHELP                                                 
HELP17   LA    R3,L'HELPL1H+L'HELPL1(R3)                                        
HELP18   SR    R0,R0               BUMP TO NEXT TABLE ENTRY                     
         IC    R0,RECELEN                                                       
         AR    R2,R0                                                            
         LA    R4,1(R4)                                                         
         B     HELP8                                                            
         EJECT                                                                  
***********************************************************************         
* HELP FOR ACTION                                                     *         
***********************************************************************         
         SPACE 1                                                                
HELP20   L     R2,ACACTTAB                                                      
         USING ACTTABD,R2          R2=A(ACTION TABLE)                           
         LTR   R4,R4               TEST HELP NEXT                               
         BZ    *+12                                                             
         LA    R2,ACTTABL(R2)      YES - BUMP TO NEXT ENTRY                     
         BCT   R0,*-4                                                           
         LA    R3,HELPL1H          R3=A(FIRST HELP DATA LINE HEADER)            
HELP22   CLI   ACTTABD,EOT         TEST E-O-T                                   
         BNE   *+10                                                             
         SR    R4,R4               NO MORE TO COME                              
         B     HELPX                                                            
         MVC   SCRECN,INREC                                                     
         MVC   SCACTN,ACTNUMB                                                   
         BAS   RE,VALSUBA          VALIDATE ACTION                              
         BNE   HELP30                                                           
         BAS   RE,VALSUBM          VALIDATE COMBO                               
         BNE   HELP30                                                           
         L     RF,AMIXNTRY                                                      
         USING MIXTABD,RF          RF=A(RECORD TYPE/ACTION TABLE)               
         TM    MIXINDS,MIXISEL     TEST SELECT ACTION                           
         BNZ   HELP30              YES - NO HELP FOR THESE                      
         TM    MIXINDS2,MIXINOH    TEST DO NOT GIVE HELP                        
         BNZ   HELP30                                                           
         CLI   0(R3),L'HELPL1H+L'HELPL1                                         
         BL    HELPX               EXIT IF END OF TWA REACHED                   
         LR    R0,RF                                                            
         GOTO1 AEXPACT,ACTTABD                                                  
         LR    RF,R0                                                            
         MVC   8(3,R3),SCACTNAM                                                 
         CLI   SCACTNAM+3,C' '     TEST 3-BYTE ACTION NAME                      
         BE    HELP24                                                           
         MVI   11(R3),C'('         NO - SHOW REMAINDER IN BRACKETS              
         MVC   12(5,R3),SCACTNAM+3                                              
         LA    R1,16(R3)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
HELP24   TM    MIXINDS2,MIXITXT    TEST GETTXT HELP                             
         BZ    HELP26                                                           
         LA    R1,SCPARM                                                        
         USING GETTXTD,R1          BUILD GETTXT BLOCK                           
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,MIXHELP                                                  
         MVI   GTMAXL,L'MIXHELP                                                 
         LA    R0,SCWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT                                                          
         MVC   24(L'MIXHELP,R3),SCWORK                                          
         B     HELP28                                                           
         DROP  R1                                                               
HELP26   ZIC   R1,MIXELEN                                                       
         SH    R1,=Y(MIXTABL+1)    R1=L'HELP TEXT-1                             
         BM    HELP28                                                           
         EX    R1,*+8                                                           
         B     HELP28                                                           
         MVC   24(0,R3),MIXHELP                                                 
HELP28   LA    R3,L'HELPL1H+L'HELPL1(R3)                                        
HELP30   LA    R2,ACTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         LA    R4,1(R4)                                                         
         B     HELP22                                                           
         EJECT                                                                  
***********************************************************************         
* HELP FOR KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
HELP40   L     R3,AKEYHDR          HELP DISPLAYED IN KEY FIELD                  
         SR    R1,R1                                                            
         IC    R1,FVTLEN-FVIHDR(R3)                                             
         SH    R1,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         MVI   SCWORK,C' '         CLEAR WORK TO SPACES                         
         MVC   SCWORK+1(L'SCWORK-1),SCWORK                                      
         STC   R1,SCWORK           SAVE EXECUTE LENGTH                          
         LA    R2,INKEYT           R2=A(KEY COMPONENT TABLE)                    
         LA    R3,SCWORK+1         R3=A(WORK AREA TO BUILD HELP)                
         SR    R4,R4               R4=ZERO FOR HELPX                            
         SR    R0,R0                                                            
         ICM   R0,1,INKEYC         R0=NUMBER OF KEY COMPONENTS                  
         BNZ   HELP42                                                           
         LA    R2,SCPARM           GET 'NO KEY REQUIRED'                        
         USING GETTXTD,R2                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(INFHLPNK)                                           
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMSYS,X'FF'                                                     
         OI    GT1INDS,GT1OWRK+GT1RETNF                                         
         STCM  R3,7,GTAOUT                                                      
         MVI   GTMAXL,L'SCWORK-1                                                
         GOTO1 VGETTXT,GETTXTD                                                  
         DROP  R2                                                               
         B     HELP48                                                           
HELP42   CLI   0(R2),0             TEST KEY COMPONENT HERE                      
         BE    HELP46              NO - IGNORE                                  
         L     R1,ACKEYTAB                                                      
         USING KEYTABD,R1          R1=A(KEY COMPONENT TABLE)                    
HELP44   CLI   KEYTABD,EOT         TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   KEYNUMB,0(R2)       MATCH ON KEY COMPONENT NUMBER                
         BE    *+12                                                             
         LA    R1,KEYTABL(R1)                                                   
         B     HELP44                                                           
         GOTO1 AEXPKEY                                                          
         MVC   0(L'SCKEYNAM,R3),SCKEYNAM                                        
         LA    R3,L'SCKEYNAM(R3)                                                
         CLI   0(R3),C' '          FIND END OF KEY NAME                         
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVC   1(1,R3),SCDELIM     INSERT KEYWORD DELIMITER                     
         LA    R3,2(R3)                                                         
HELP46   LA    R2,1(R2)            BUMP TO NEXT KEY COMPONENT                   
         BCT   R0,HELP42                                                        
         BCTR  R3,0                                                             
         CLC   0(1,R3),SCDELIM     REMOVE TRAILING DELIMITER (IF ANY)           
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
HELP48   L     RE,AKEYHDR          MOVE KEY HELP TO TWA KEY FIELD               
         ZIC   R1,SCWORK                                                        
         EX    R1,*+8                                                           
         B     HELPX                                                            
         MVC   L'FVIHDR(0,RE),SCWORK+1                                          
         EJECT                                                                  
***********************************************************************         
* HELP FOR OPTIONS                                                    *         
***********************************************************************         
         SPACE 1                                                                
HELP60   L     R2,ACOPTTAB                                                      
         USING OPTTABD,R2          R2=A(OPTIONS TABLE)                          
         SR    R1,R1                                                            
         LTR   R4,R4               TEST HELP NEXT                               
         BZ    *+14                                                             
         IC    R1,OPTELEN                                                       
         AR    R2,R1               YES - BUMP TO NEXT ENTRY                     
         BCT   R0,*-6                                                           
         LA    R3,HELPL1H          R3=A(FIRST HELP DATA LINE HEADER)            
HELP62   CLI   OPTTABD,EOT         TEST E-O-T                                   
         BNE   *+10                                                             
         SR    R4,R4               NO MORE TO COME                              
         B     HELPX                                                            
                                                                                
         TM    OPTINDS2,OPTNOHLP   NO HELP FOR THIS OPTION                      
         BO    HELP94                                                           
         GOTO1 AEXPOPT,OPTTABD                                                  
         MVI   SCFLAG,0            FLAG FOR LINE 2 DISPLAY                      
         GOTO1 TSTAUTH,OPTAUTH     TEST AUTHORISATION SET                       
         BNE   HELP92                                                           
         TM    OPTINDS,OPTIDDS     TEST DDS ONLY OPTION                         
         BZ    HELP62A                                                          
         TM    CUSTAT,CUSDDS       YES - TEST THIS IS A DDS TERMINAL            
         BZ    HELP92                                                           
         B     HELP63                                                           
                                                                                
HELP62A  TM    OPTINDS2,OPTNOSEC   TEST FOR NO SECURITY TEST                    
         BO    HELP63                                                           
         GOTO1 TSTSECO,OPTOPTN     TEST SECURITY ACCESS                         
         BNE   HELP92                                                           
HELP63   CLI   OPTRECB,0           TEST VALID FOR ALL RECORDS                   
         BE    *+14                                                             
         CLC   INREC,OPTRECB       NO - MATCH ON RECORD NUMBER                  
         BNE   HELP92                                                           
         CLI   OPTACTB,0           TEST VALID FOR ALL ACTIONS                   
         BE    HELP63B                                                          
         CLC   INACT,OPTACTB       YES - MATCH ON ACTION NUMBER                 
         BE    HELP63B                                                          
         TM    TWAMODE,TWAMSEL     NO  - TEST IN SELECT MODE                    
         BZ    HELP92                                                           
         TM    OPTINDS2,OPTISEL    NO  - VALID IN SELECT MODE                   
         BZ    HELP92                                                           
         CLC   SCACTN,OPTACTB      NO  - MATCH ON ACTION NUMBER                 
         BNE   HELP92                                                           
*                                                                               
HELP63B  MVC   SCWORK(L'INOPTR),INOPTX                                          
         NC    SCWORK(L'INOPTR),OPTOPTB                                         
         BNZ   HELP92                                                           
*                                  TEST ENOUGH ROOM ON SCREEN (2 LINES)         
         LA    R0,L'HELPL1H+L'HELPL1                                            
         CLM   R0,1,0(R3)                                                       
         BL    HELPX                                                            
         CLM   R0,1,L'HELPL1H+L'HELPL1(R3)                                      
         BL    HELPX                                                            
*                                  CALCULATE L'OPTNAME                          
         LA    R1,SCOPTNAM+L'SCOPTNAM-1                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,SCOPTNAM                                                      
         SR    R1,R0                                                            
         LA    R1,1(R1)            R1=ACTUAL OPTNAME LENGTH                     
         CLM   R1,1,OPTMINKL       TEST SAME AS MINIMUM INPUT                   
         BH    HELP64                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),SCOPTNAM                                                 
         LA    R1,8+1(R3,R1)                                                    
         B     HELP65                                                           
*                                                                               
HELP64   ZIC   RE,OPTMINKL         RE=MINIMUM INPUT LENGTH                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),SCOPTNAM    MOVE MINUMUM PART OF KEYWORD                 
         LA    RE,1(RE)                                                         
         SR    R1,RE                                                            
         LA    RF,8(RE,R3)                                                      
         LA    RE,SCOPTNAM(RE)                                                  
         MVI   0(RF),C'('          PARENTHESISE OPTIONAL PIECE                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RE)                                                    
         LA    RF,2(RF,R1)                                                      
         MVI   0(RF),C')'                                                       
         LA    R1,1(RF)                                                         
*                                                                               
HELP65   CLI   OPTMAXDL,0          TEST IF A STAND ALONE KEYWORD                
         BE    *+10                                                             
         MVC   0(1,R1),SCEQUAL                                                  
*                                                                               
         TM    OPTINDS,OPTITXT     TEST GETTXT HELP                             
         BZ    HELP66                                                           
         CLC   OPTHELP(L'GTMSGNO),=AL2(0)                                       
         BZ    HELP68              DON'T CALL GETTXT - NO NUMBER                
         LA    R1,SCPARM                                                        
         USING GETTXTD,R1          BUILD GETTXT BLOCK                           
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,OPTHELP                                                  
         MVI   GTMAXL,L'OPTHELP                                                 
         LA    R0,SCWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT                                                          
         MVC   8+13(L'OPTHELP,R3),SCWORK                                        
         LA    R3,L'HELPL1H+L'HELPL1(R3)                                        
         B     HELP68                                                           
         DROP  R1                                                               
*                                                                               
HELP66   ZIC   R1,OPTHELP          L'1ST HELP TEXT                              
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    HELP68                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8+13(0,R3),OPTHELP+1                                             
         LA    R3,L'HELPL1H+L'HELPL1(R3)                                        
*                                                                               
HELP68   CLI   SCOPTSHT,C' '       TEST SHORT KEYWORD PRESENT                   
         BE    HELP74                                                           
         MVC   8+0(1,R3),SCSLASH                                                
         MVC   8+1(L'SCOPTSHT,R3),SCOPTSHT                                      
         LA    R1,8+1+L'SCOPTSHT-1(R3)                                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         CLI   OPTMAXDL,0          TEST STAND ALONE KEYWORD                     
         BE    *+10                                                             
         MVC   1(1,R1),SCEQUAL                                                  
         MVI   SCFLAG,1            SET LINE 2 USED                              
*                                                                               
HELP74   CLI   OPTMAXDL,0          TEST STAND ALONE KEYWORD                     
         BE    HELP90                                                           
         TM    OPTINDS,OPTARTN+OPTNRTN  OPTION A(RTN) OR NUMBER OF RTN          
         BZ    HELP82                                                           
*                                                                               
         TM    OPTINDS,OPTITXT                                                  
         BZ    HELP80                                                           
         CLI   OPTELEN,OPTTABL+(L'GTMSGNO*2)                                    
         BNE   HELP90                                                           
         CLC   OPTHELP(L'GTMSGNO),=AL2(0)                                       
         BZ    HELP90              DON'T CALL GETTXT - NO NUMBER                
*                                                                               
         LA    R1,SCPARM                                                        
         USING GETTXTD,R1          BUILD GETTXT BLOCK FOR LINE 2                
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,OPTHELP+2                                                
         MVI   GTMAXL,L'OPTHELP                                                 
         LA    R0,SCWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT                                                          
         MVC   8+13(L'OPTHELP,R3),SCWORK                                        
         MVI   SCFLAG,1            SET LINE 2 USED                              
         B     HELP90                                                           
         DROP  R1                                                               
*                                                                               
HELP80   ZIC   RF,OPTHELP          L'1ST HELP TEXT                              
         LA    RF,OPTHELP(RF)      2ND HELP ELEMENT                             
         ZIC   RE,0(RF)            L'2ND TEXT                                   
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    HELP90              NO 2ND HELP ELEMENT                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8+13(0,R3),1(RF)                                                 
         MVI   SCFLAG,1            SET LINE 2 USED                              
         B     HELP90                                                           
*                                                                               
HELP82   TM    OPTINDS,OPTATAB     TEST OPTION TABLE                            
         BNZ   *+6                                                              
         DC    H'0'                OPTION TABLE IS SCREWY                       
         LA    R1,8+13(R3)                                                      
         SR    RF,RF                                                            
         ICM   RF,3,OPTIADDR                                                    
         TM    ACOPTIND,ACOPTITB   TEST USE ACTBASE NOT ACOBASE                 
         BZ    *+12                                                             
         A     RF,ACTBASE                                                       
         B     *+8                                                              
         A     RF,ACOBASE                                                       
         MVI   SCFLAG,1            SET LINE 2 USED                              
         TM    OPTINDS,OPTTABH     DOES TABLE HAVE HELP PREFIX                  
         BZ    *+14                                                             
         MVC   0(L'OPTHELP,R1),0(RF)                                            
         B     HELP90                                                           
*                                                                               
         LA    R0,L'HELPL1H+L'HELPL1(R3)                                        
         ST    R0,SCFULL           SCFULL=A(NEXT TWA FIELD HEADER)              
         ZIC   R0,0(RF)            R0=L'LHS OF TABLE                            
         ZIC   RE,1(RF)            RE=L'RHS OF TABLE                            
         AR    RE,R0                                                            
         ST    R0,SCDUB            SCDUB+0(4)=L'LHS OF TABLE                    
         ST    RE,SCDUB+4          SCDUB+4(4)=TOTAL TABLE LENGTH                
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
HELP84   CLI   0(RF),EOT           TEST E-O-T                                   
         BE    HELP88                                                           
         LA    R0,11(R1)           TEST VALUE WILL FIT ON LINE                  
         C     R0,SCFULL                                                        
         BNL   HELP88                                                           
         LR    RE,RF                                                            
         A     RE,SCDUB                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8              RE=A(LAST CHARACTER OF OPTION NAME)          
         LR    R0,RF                                                            
         SR    RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RF)       MOVE OUT FULL OPTION NAME                    
         LA    RE,1(RE)                                                         
         LA    R1,0(RE,R1)                                                      
         CLM   RE,1,OPTMINDL                                                    
         BNH   HELP86              TEST EQUAL TO MINIMUM ALLOWED                
         SR    R1,RE               NO - FORMAT OPTIONAL BIT IN PARENS           
         SR    R0,R0                                                            
         ICM   R0,1,OPTMINDL                                                    
         BNZ   *+8                                                              
         LA    R0,1                SET MINIMUM TO 1 IF ZERO                     
         AR    R1,R0                                                            
         MVI   0(R1),C'('                                                       
         SR    RE,R0                                                            
         BCTR  RE,0                                                             
         AR    RF,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),0(RF)                                                    
         SR    RF,R0                                                            
         LA    R1,2(RE,R1)                                                      
         MVI   0(R1),C')'                                                       
         LA    R1,1(R1)                                                         
HELP86   MVC   0(1,R1),SCSLASH     INSERT DELIMITER                             
         LA    R1,1(R1)                                                         
         A     RF,SCDUB+4          BUMP TO NEXT TABLE ENTRY                     
         B     HELP84                                                           
HELP88   BCTR  R1,0                REMOVE TRAILING VALUES DELIMITER             
         MVI   0(R1),C' '                                                       
*                                                                               
HELP90   CLI   SCFLAG,0            TEST LINE 2 USED                             
         BE    *+8                                                              
         LA    R3,L'HELPL1H+L'HELPL1(R3)                                        
*                                                                               
HELP92   AHI   R4,1                                                             
HELP94   SR    R0,R0               BUMP TO NEXT TABLE ENTRY                     
         IC    R0,OPTELEN                                                       
         AR    R2,R0                                                            
         B     HELP62                                                           
*                                                                               
HELPX    STC   R4,TWAHMORE         SET NEXT TIME HELP INDEX                     
         L     R1,FVADDR           INSERT CURSOR TO HELP FIELD                  
         OI    FVOIND-FVIHDR(R1),FVOCUR                                         
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(INFHLP1)                                            
         CLI   TWAHMORE,0          TEST MORE HELP TO COME                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(INFHLP2)                                            
         B     HELPXX                                                           
         CLI   FVHELP,HELPKEY                                                   
         BE    HELPXX                                                           
         L     R1,FVADDR           CLEAR & TRANSMIT INPUT FIELD                 
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
HELPXX   B     FVERR0                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OVERLAY A SCREEN INTO TWA                                *         
*                                                                     *         
* NTRY - INSCRN=SCREEN OVERLAY NUMBER                                 *         
*        R1=A(LOAD POINT)                                             *         
*                                                                     *         
* NOTE - ENTRY POINT SCOVSCR CAN ONLY BE USED BY THE SYSTEM           *         
*        CONTROLLER. AOVSCR IS SET TO APOVSCR.                        *         
***********************************************************************         
         SPACE 1                                                                
SCOVSCR  LR    R0,RE                                                            
         L     R1,0(R1)                                                         
         B     OVSCR                                                            
         SPACE 1                                                                
APOVSCR  NTR1  BASE=SCBASE1,LABEL=NO                                            
         L     RA,SCBASE2                                                       
         L     R9,SCBASE3                                                       
         L     R8,SCBASE4                                                       
         L     R6,SCBASE5                                                       
         LA    R0,EXIT                                                          
         SPACE 1                                                                
OVSCR    ST    R1,SCPARM           SET A(LOAD POINT)                            
         MVI   SCPARM+4,C'R'                                                    
         MVC   SCPARM+5(2),ACSYSPGM                                             
         MVC   SCPARM+7(1),INSCRN  SET SCREEN OVERLAY NUMBER                    
         XC    SCPARM+8(8),SCPARM+8                                             
         GOTO1 VCOLY,SCPARM                                                     
         BAS   RE,CHKOLY           TEST SCREEN LOADED OK                        
         BNE   OVSCRX                                                           
         L     R1,AMSGHDR                                                       
         L     RF,ATWAEND                                                       
         AH    RF,=Y(LSMTWSVL)                                                  
         SR    RE,RE                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,0(R1)                                                       
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                SCREEN TOO LARGE                             
         CLC   INSCRN,TWASCRN      TEST SCREEN SAME AS LAST                     
         BNE   *+10                                                             
         XC    1(2,R1),1(R1)       YES - DON'T SET BEFORE/AFTER                 
         CR    RE,RE               SET CONDITION CODE TO EQUAL                  
OVSCRX   LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IF OVERLAY IS LOADED OK AND BUILD ERROR MESSAGE    *         
* NTRY - R1=A(CALL OVERLAY PARAMETER LIST)                            *         
* EXIT - CC=EQUAL IF LOADED OK, CC=NOT EQUAL IF NOT OK AND ERROR SET  *         
***********************************************************************         
         SPACE 1                                                                
CHKOLY   MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   4(R1),X'FF'         TEST PHASE LOADED OK                         
         BNE   CHKOLYX                                                          
         MVC   FVMSGNO,=AL2(FVFEOLY) NO - SET ERROR                             
CHKOLYX  CLC   FVMSGNO,=AL2(FVFOK)                                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET TWA FIELD ADDRESSES OF FIRST KEY & DATA FIELDS       *         
***********************************************************************         
         SPACE 1                                                                
SETFLD   STM   RE,R1,12(RD)                                                     
         ICM   R1,15,AKEYHDR       IF THERE ARE ANY KEY FIELDS,                 
         BNZ   *+8                                                              
         L     R1,AACTHDR                                                       
         TM    INDRLIND,MIXIDRLS                                                
         BNZ   *+12                                                             
         TM    INMIX1,MIXIREP                                                   
         BNZ   *+12                                                             
         CLI   INKEYN,0                                                         
         BE    SETFLD1                                                          
         L     R1,ATWAEND          FIND FIRST UNPROTECTED FIELD (KEY)           
         SR    RE,RE                                                            
         LA    RF,TWAD                                                          
         AH    RF,=Y(SAVAREA-TWAD)                                              
         TM    1(R1),FVAPROT                                                    
         BZ    *+16                                                             
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-12                                                       
         B     SETFLDX                                                          
         TM    INDRLIND,MIXIDRLS                                                
         BNZ   SETFLD1                                                          
         TM    INMIX1,MIXIREP                                                   
         BZ    SETFLD1                                                          
         ST    R1,AINDHDR                                                       
         B     SETFLDX                                                          
SETFLD1  ST    R1,AINKHDR          SAVE A(FIRST KEY FIELD)                      
         ST    R1,AINDHDR          SET AS FIRST DATA FIELD TOO                  
         SR    R0,R0                                                            
         ICM   R0,1,INKEYN         R0=NUMBER OF KEY FIELDS ON SCREEN            
         BZ    SETFLDX                                                          
         AH    R0,=H'1'                                                         
SETFLD2  TM    1(R1),FVAPROT       FIND FIRST DATA FIELD                        
         BO    *+12                                                             
         BCT   R0,*+8                                                           
         B     *+14                                                             
         ICM   RE,1,0(R1)                                                       
         BXLE  R1,RE,SETFLD2                                                    
         DC    H'0'                                                             
         ST    R1,AINDHDR          SAVE A(FIRST DATA FIELD)                     
SETFLDX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TRANSFER CONTROL TO PFM                                             *         
***********************************************************************         
         SPACE 1                                                                
PFMGO    NTR1                                                                   
         XC    APELEM,APELEM                                                    
         LA    R1,APELEM                                                        
*                                                                               
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMKEY(4),=C'*   '                                             
         XC    GLPFMDA,GLPFMDA                                                  
         LA    RF,SAVAREA                                                       
         MVC   GLPFMFIL(6),SAVFILE-SAVAREA(RF)                                  
         CLC   GLPFMFIL(3),=C'GEN'                                              
         BNE   *+14                                                             
         MVC   GLPFMDA,SAVRECD                                                  
         B     *+10                                                             
         MVC   GLPFMKEY,SAVRECK                                                 
         L     R1,ACOM                                                          
         L     RF,CGLOBBER-COMFACSD(R1)                                         
         GOTO1 (RF),IODMCB,=C'PUTD',APELEM,54,GLPFMCDQ                          
*                                                                               
PFMGO20  XC    APELEM,APELEM                                                    
         LA    R1,APELEM                                                        
         USING GLVXFRSY,R1                                                      
*                                                                               
PFMGO22  MVC   GLVXFRSY,=C'CON'                                                 
         MVC   GLVXTOSY,=C'CON'                                                 
*                                                                               
PFMGO24  MVC   GLVXFRPR,=C'GEN'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                                                               
PFMGO30  L     R1,ACOM                                                          
         L     RF,CGLOBBER-COMFACSD(R1)                                         
         GOTO1 (RF),IODMCB,=C'PUTD',APELEM,24,GLVXCTL                           
         L     R2,ARECHDR                                                       
         OI    6(R2),X'40'         POSITION CURSOR                              
PFMGOX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SAVE VALUES IN TWA0 AND RETURN TO MONITOR                           *         
***********************************************************************         
         SPACE 1                                                                
GENX     CLI   ACLSTIND,0          TEST CONTROLLER HOOK REQUESTED               
         BE    GENX2                                                            
         GOTO1 ACHOOK,ACMLAST                                                   
*                                                                               
GENX2    CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    GENX4                                                            
         TM    SCINDS,SCIACT       TEST RECORD/ACTION VALIDATED                 
         BZ    GENX6                                                            
         MVC   TWASCRN,INSCRN                                                   
         MVC   TWALREC,INREC                                                    
         MVC   TWALACT,INACT                                                    
         TM    SCINDS,SCIGO        EXIT IF GO POINT NOT REACHED                 
         BZ    GENX3                                                            
         TM    TWAMODE,TWAMLSM     SAVE THIS TIME VALUES IN TWA                 
         BNZ   *+10                                                             
         MVC   SAVOPTS,INOPTS                                                   
         B     *+12                                                             
GENX3    TM    ACLFMIND,ACLFMIRV   SAVE RECVALS FOR APPLICATION                 
         BZ    GENX6                                                            
         LH    R1,=Y(RECVALSL)                                                  
         LA    R0,SAVVALS                                                       
         LA    RE,RECVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         OI    TWAMODE,TWAMNXT     SET SESSION CONTINUATION MODE                
         B     GENX6                                                            
*                                                                               
GENX4    TM    SCINDS,SCIRQERR     TEST REPORT REQUEST VALIDATION ERR           
         BZ    GENX6                                                            
         NI    SCINDS,X'FF'-SCIRQERR                                            
         ICM   RC,15,AREP          TEST REPORT STORAGE IS AVAILABLE             
         BZ    GENX6                                                            
         USING REPD,RC                                                          
         MVC   REPSUBID,INUSER     GENERATE ERROR DETAILS                       
         MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         GOTO1 VREPORT,REPBLK                                                   
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    REPAPRT,REPAPRT     TEST PRINT RESOLVED                          
         BZ    GENX6                                                            
*                                                                               
         L     R2,TWAOVMST         LOCATE REMOTEC                               
         L     R2,MCVREMOT-MASTD(R2)                                            
         USING REMOTED,R2                                                       
         OC    REMOTKEY,REMOTKEY   TEST REMOTE PRINTING                         
         BZ    GENX5                                                            
         MVCDD REMOTKEY(#ERRORLQ),GE#ERROR                                      
         GOTO1 VDICTAT,SCPARM,SCDICONE,REMOTKEY,0                               
*                                                                               
GENX5    MVC   REMOTPAS,REPPSWD    OVERIDE REPORT SECURITY INFO                 
         MVC   REMOTSF1,REPSECF1                                                
         MVC   REMOTSF2,REPSECF2                                                
         DROP  R2                                                               
         SR    R0,R0               PRINT REQUEST DETAILS PAGE                   
         ICM   RF,15,REPABOX                                                    
         BZ    *+8                                                              
         LA    R0,C'B'                                                          
         GOTO1 VREQTWA,SCPARM,(3,TWAD),(X'FF',ACOM),REPAPRT,((R0),(RF))         
*                                                                               
         MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTO1 VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    GENX6                                                            
         DC    H'0'                CLOSE ERROR                                  
         DROP  RC                                                               
*                                                                               
GENX6    TM    FVERRIND,FVEUNWND   TEST UNWIND THE TRANSACTION                  
         BZ    GENX8                                                            
         DC    H'0',C'$ABEND'                                                   
*                                                                               
GENX8    CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    EXIT                YES                                          
*                                                                               
         TM    CUSTAT,CUSDDL       TEST DDLINK IN CONTROL                       
         BZ    GENX9                                                            
*                                                                               
         BRAS  RE,DDLPUT           YES, RETURN MESSAGE ETC.                     
*                                                                               
         BRAS  RE,DDLSWAP          SWAP LIOB INTO TIA                           
*                                                                               
         L     R1,ATIA                                                          
         USING LIOBD,R1                                                         
         TM    LIOBFLG2,LIOBFEOF   WAS PREVIOUS AN EOF                          
         JO    GENX8B              YES, ALL DONE                                
         TM    LIOBFLG2,LIOBFEOR   WAS PREVIOUS AN EOR                          
         JZ    *+2                 NO, WHY ARE WE HERE?                         
*                                                                               
         L     RF,ACOM             GET NEXT REC/ACT AND FIRST FIELD             
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),SCPARM,('LIOAGET',ATIA)                                     
         CLI   0(R1),LIOCDONE      TEST EOF                                     
         JE    GENX8B                                                           
         CLI   0(R1),0                                                          
         JNE   *+2                 SOMETHING ELSE WRONG                         
*                                                                               
GENX8A   BRAS  RE,DDLSWAP          SWAP LIOB OUT OF TIA                         
*                                                                               
         DC    H'0'                CAN'T HANDLE MORE THAN ONE YET               
*                                                                               
GENX8B   BRAS  RE,DDLSWAP          SWAP LIOB OUT OF TIA                         
         DROP  R1                                                               
         B     EXIT                                                             
*                                                                               
GENX9    LAY   R2,GLOBSAVE                                                      
         OC    0(L'GLOBSAVE,R2),0(R2) TEST GLOBBER CALLED US                    
         BZ    EXIT                NO, EXIT                                     
*                                                                               
         LA    RE,SCWORK                                                        
         XC    SCWORK,SCWORK                                                    
         USING GLOBSAVE,R2                                                      
         USING GLVXFRSY,RE                                                      
         MVC   GLVXFRSY,GLOBINSY   FROM THE GLOBIN SYSTEM                       
         MVC   GLVXFRPR,GLOBINPR   AND      GLOBIN PROGRAM                      
         MVC   GLVXTOSY,GLOBFRSY   TO THE CALLING SYSTEM                        
         MVC   GLVXTOPR,GLOBFRPR   AND PROGRAM                                  
         OI    GLVXFLG1,GLV1RETN   RETURN CALL                                  
         TM    GLOBFLG1,GLV1SEPS   TEST CALLED FROM ANOTHER SESSION             
         BZ    *+8                                                              
         OI    GLVXFLG1,GLV1SEPS   RETURN TO OTHER SESSION                      
         DROP  RE,R2                                                            
         L     RF,ACOM             SET UP THE TRANSFER CONTROL BLOCK            
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         LA    R1,SCPARM                                                        
         GOTO1 (RF),(R1),=C'PUTD',SCWORK,22,GLVXCTL,0                           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         XC    SCWORK(2),SCWORK    RETURN ERROR MSG# ZERO FOR NOW               
*&&UK*&& GOTO1 (RF),(R1),=C'PUTD',SCWORK,2,GLVMERTN,0                           
*&&US*&& GOTO1 (RF),(R1),=C'PUTD',SCWORK,2,GLVPRRTN,0                           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         SPACE 2                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
EFFS     DC    X'FFFFFFFF'                                                      
DMREAD   DC    C'DMREAD '                                                       
DMWRITE  DC    C'DMWRT  '                                                       
TEMPSTR  DC    C'TEMPSTR'                                                       
         SPACE 1                                                                
LANGCHAR DS    0CL4                ** LANGUAGE DEPENDANT CHARACTERS **          
         DC    C',=?/'             ENGLISH                                      
         DC    C',=?/'             AMERICAN                                     
         DC    C'#=?/'             GERMAN                                       
         DC    C',=?/'             FRENCH                                       
         DC    C',=?/'             SPANISH                                      
         DC    C',=?/'             ITALIAN                                      
         DC    C',=?/'             ???                                          
         DC    C',=?/'             ???                                          
         DC    C',=?/'             ???                                          
         DC    C',=?/'             ???                                          
         DC    C',=?/'             ???                                          
         DC    C',=?/'             ???                                          
         SPACE 1                                                                
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QGETIDS)                                                     
         DC    AL1(QREPORT)                                                     
         DC    AL1(QREQTWA)                                                     
         DC    AL1(QSPOON)                                                      
         DC    AL1(QDROOL)                                                      
PHASESN  EQU   *-PHASES                                                         
         SPACE 1                                                                
PHASOFF  DS    0X                  ** OFFLINE LOADED PHASE LIST **              
         DC    AL1(QDRIVER)                                                     
PHASOFFN EQU   *-PHASOFF                                                        
         SPACE 1                                                                
CONADDRS DS    0F                  ** CONTROLLER FACILITIES **                  
         DC    V(TWABLD)                                                        
         DC    A(FILTAB)           SYSTEM FILES                                 
         DC    A(SYSTAB)           GLOBAL SYSTEM FILES                          
         DC    A(CMDTAB)           FILE COMMANDS                                
         DC    A(APFVAL)           APPLICATION ENTRY POINT                      
         DC    A(APIOEX)           APPLICATION ENTRY POINT                      
         DC    A(APOVSCR)          APPLICATION ENTRY POINT                      
         DC    A(BLDTWA)           BUILD DYNAMIC SCREENS                        
         DC    A(VALWHEN)          VALIDATE WHEN FIELD                          
         DC    A(VALOTYP)          VALIDATE OUTPUT TYPE FIELD                   
         DC    A(VALDEST)          VALIDATE DESTINATION FIELD                   
         DC    A(VALOPTS)          VALIDATE OPTIONS FIELD                       
         DC    A(PUTKEY)           PUT KEY COMPONENTS                           
         DC    A(SETKEY)           SET KEY COMPONENT VALUES                     
         DC    A(GETKEY)           GET KEY COMPONENT VALUES                     
         DC    A(EXPREC)           EXPAND RECORD NAME                           
         DC    A(EXPACT)           EXPAND ACTION NAME                           
         DC    A(EXPKEY)           EXPAND KEY NAME                              
         DC    A(EXPOPT)           EXPAND OPTION NAME                           
         DC    A(DRIVER)           DRIVER/DROOL ROUTINE                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         EJECT                                                                  
INFHLP1  EQU   100+X'FF00'         HELP DISPLAYED -SELECT NOW                   
INFHLP2  EQU   101+X'FF00'         DITTO -OR ENTER FOR NEXT                     
INFHLPNK EQU   102                 NO KEY REQUIRED                              
*                                                                               
INFADD1  EQU   006+X'FF00'         RECORD ADDED                                 
INFDIS1  EQU   003+X'FF00'         RECORD DISPLAYED                             
INFDIS2  EQU   007+X'FF00'         RECORD DISPLAYED IS DELETED                  
INFCHA1  EQU   004+X'FF00'         RECORD DISPLAYED - ENTER CHANGES             
INFCHA2  EQU   005+X'FF00'         RECORD CHANGED                               
INFDEL1  EQU   024+X'FF00'         RECORD DISPLAYED ENTER TO DELETE             
INFDEL2  EQU   007+X'FF00'         RECORD DELETED                               
INFRES1  EQU   025+X'FF00'         RECORD DISPLAYED ENTER TO RESTORE            
INFRES2  EQU   008+X'FF00'         RECORD RESTORED                              
INFSEL1  EQU   026+X'FF00'         NOTHING TO LIST - ENTER NEXT REQUEST         
INFSEL2  EQU   009+X'FF00'         LIST DISPLAYED-SELECT/ENTER FOR NXT          
INFSEL3  EQU   010+X'FF00'         END OF LIST - SELECT/ENTER FOR FRST          
INFSEL4  EQU   029+X'FF00'         SELECT NOW                                   
*                                                                               
INFREP1  EQU   020+X'FF00'         REPORT WILL BE PROCESSED OVERNIGHT           
INFREP2  EQU   021+X'FF00'         REPORT &T WILL BE PROCESSED SOON             
INFREP3  EQU   027+X'FF00'         REPORT &T HAS BEEN SPOOLED                   
INFREP4  EQU   028+X'FF00'         ENTER DATA                                   
INFREP5  EQU   031+X'FF00'         REPORT DISPLAYED                             
INFREP6  EQU   046+X'FF00'         REPORT &T WILL BE RUN LATE                   
*                                                                               
WRNMSG1  EQU   002+X'FF00'         NO DATA GENERATED - ENTER NEXT REQ           
         SPACE 1                                                                
GENHELP  EQU   98                  GEGEN00 HELP BASE. HLPREC1=100&101           
         SPACE 1                                                                
ESCHIGHQ EQU   X'30'               ESCAPE SEQUENCE HIGH VALUE                   
         EJECT                                                                  
QSSPT    EQU   X'02'                                                            
QSNET    EQU   X'03'                                                            
QSMED    EQU   X'04'                                                            
QSPRT    EQU   X'04'                                                            
QSMPL    EQU   X'05'                                                            
QSACC    EQU   X'06'                                                            
QSFEE    EQU   X'07'                                                            
QSREP    EQU   X'08'                                                            
QSMBA    EQU   X'09'                                                            
QSCON    EQU   X'0A'                                                            
QSPER    EQU   X'0E'                                                            
QSMEDZ   EQU   X'14'                                                            
         SPACE 2                                                                
* OFFLINE SYSTEM LIST                                                           
*                                                                               
SYSLIST  DS    0XL3                                                             
*                                                                               
*&&US*&& DC    C'SP',AL1(QSSPT)                                                 
*&&US*&& DC    C'RE',AL1(QSREP)                                                 
         DC    C'AC',AL1(QSACC)                                                 
*&&UK*&& DC    C'ME',AL1(QSMED)                                                 
*&&US*&& DC    C'PP',AL1(QSPRT)                                                 
*&&US*&& DC    C'PR',AL1(QSPRT)                                                 
         DC    C'MP',AL1(QSMPL)                                                 
*&&UK*&& DC    C'FE',AL1(QSFEE)                                                 
         DC    C'CT',AL1(QSCON)                                                 
         DC    C'CO',AL1(QSCON)                                                 
         DC    C'PE',AL1(QSPER)                                                 
         DC    C'MB',AL1(QSMBA)                                                 
*&&US*&& DC    C'NE',AL1(QSNET)                                                 
*                                                                               
SYSLISTX DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* AREAS FOR LINKIO                                                    *         
***********************************************************************         
         SPACE 1                                                                
LINKMAP  DS    0XL(LIORL)          ** LINKIO RECORD MAP **                      
         DC    X'FEF5',X'FEF5',AL2(LINKHDR-LINKMAP)                             
         DC    X'FEF4',X'FEF4',AL2(LINKDTA-LINKMAP)                             
LINKMAPX DC    AL2(0)                                                           
                                                                                
LINKHDR  DS    0XL(LIODL)          ** HEADER RECORD FIELDS **                   
                                                                                
         DC    AL2(1)              RECORD NUMBER                                
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(SCDDLREC-WORKD)                                              
         DC    AL1(L'SCDDLREC)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL2(2)              ACTION NUMBER                                
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(SCDDLACT-WORKD)                                              
         DC    AL1(L'SCDDLACT)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
LINKHDRX DC    AL2(0)                                                           
*                                                                               
LINKDTA  DS    0X                  ** DATA RECORD FIELDS **                     
                                                                                
         DC    AL2(1)              FIELD NUMBER                                 
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(SCDDLFLN-WORKD)                                              
         DC    AL1(L'SCDDLFLN)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL2(2)              RELATIVE FIELD NUMBER                        
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(SCDDLREL-WORKD)                                              
         DC    AL1(L'SCDDLREL)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL2(3)              FIELD DATA                                   
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(SCDDLDTA-WORKD)                                              
         DC    AL1(0)                                                           
         DC    AL1(LIODINDX)                                                    
         DC    AL1(0)                                                           
                                                                                
LINKDTAX DC    AL2(0)                                                           
*                                                                               
         DS    0H                  FOR LARL ALIGN                               
LIOBTOKN DC    C'LOSV'             WSSVR TOKEN FOR LIOB SAVE AREA               
LIOBTLEN EQU   18*1024             LENGTH OF LIOB AND BUFFERS                   
         EJECT                                                                  
* SYSTEM FILE NAMES TABLE (FILE NUMBERS 1 THRU 9)                               
*                                                                               
FILTAB   DS    0X                                                               
*                                  ** SPOT SYSTEM FILES **                      
*&&US                                                                           
FILSPT   DC    AL1(QSSPT),C'SPT',AL2(FILSPTX-*)                                 
*                                                                               
         DC    AL1(IOSPTDIR/256),C'SPTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOSPTFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOSPTFIL/256),C'SPTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOSPTDIR/256,13,24),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOSTAFIL/256),C'STATION'                                     
         DC    AL1(FILIIS+FILIVL,0)                                             
         DC    AL1(0,15,17),AL2(512)                                            
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOXSPDIR/256),C'XSPDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOXSPFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOXSPFIL/256),C'XSPFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOXSPDIR/256,32,42),AL2(4000)                                
         DC    XL5'00'                                                          
*                                                                               
FILSPTX  DC    AL1(EOT)                                                         
*                                                                               
*                                   **  NETWORK SYSTEM FILES **                 
FILNET   DC    AL1(QSNET),C'NET',AL2(FILNETX-*)                                 
*                                                                               
         DC    AL1(IOSPTDIR/256),C'SPTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOSPTFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOSPTFIL/256),C'SPTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOSPTDIR/256,13,24),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
FILNETX  DC    AL1(EOT)                                                         
*                                                                               
*                                 **  PRINT SYSTEM FILES **                     
FILPRT   DC    AL1(QSPRT),C'PRT',AL2(FILPRTX-*)                                 
*                                                                               
         DC    AL1(IOPRTDIR/256),C'PRTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOPRTFIL/256,25,02),AL2(30)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOPRTFIL/256),C'PRTFILE'                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOPRTDIR/256,25,33),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
FILPRTX  DC    AL1(EOT)                                                         
*                                                                               
*&&                                                                             
*                                  ** MEDIA SYSTEM FILES **                     
*&&UK                                                                           
FILMED   DC    AL1(QSMED),C'MED',AL2(FILMEDX-*)                                 
*                                                                               
         DC    AL1(IOMEDDIR/256),C'MEDDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOMEDFIL/256,20,08),AL2(32)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMEDFIL/256),C'MEDFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOMEDDIR/256,20,34),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMEDZDR/256),XL7'00'                                        
         DC    AL1(QSMEDZ,IOMEDDIR/256)                                         
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMEDZFL/256),XL7'00'                                        
         DC    AL1(QSMEDZ,IOMEDFIL/256)                                         
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
FILMEDX  DC    AL1(EOT)                                                         
*&&                                                                             
*                                  ** MEDIA PLANNING SYSTEM FILES **            
FILMPL   DC    AL1(QSMPL),C'MPL',AL2(FILMPLX-*)                                 
*                                                                               
         DC    AL1(IOMPLDIR/256),C'MPLDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOMPLFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMPLFIL/256),C'MPLFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOMPLDIR/256,32,42),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMPRDRA/256),C'MPRDRA '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOMPRFLA/256,19,09),AL2(32)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMPRFLA/256),C'MPRFLA '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOMPRDRA/256,19,34),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
FILMPLX  DC    AL1(EOT)                                                         
*&&UK                                                                           
*                                  ** ARTISTE FEES SYSTEM FILES **              
FILFEE   DC    AL1(QSFEE),C'FEE',AL2(FILFEEX-*)                                 
*                                                                               
         DC    AL1(IOFEEDIR/256),C'FEEDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOFEEFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOFEEFIL/256),C'FEEFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOFEEDIR/256,32,42),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOFEEGDR/256),XL7'00'                                        
         DC    AL1(QSCON,IOGENDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOFEEGFL/256),XL7'00'                                        
         DC    AL1(QSCON,IOGENFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOFEEMDR/256),XL7'00'                                        
         DC    AL1(QSMED,IOMEDDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOFEEMFL/256),XL7'00'                                        
         DC    AL1(QSMED,IOMEDFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOFEEACC/256),XL7'00'                                        
         DC    AL1(QSACC,IOACCFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCDIR/256),XL7'00'                                        
         DC    AL1(QSACC,IOACCDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCMST/256),XL7'00'                                        
         DC    AL1(QSACC,IOACCMST/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
FILFEEX  DC    AL1(EOT)                                                         
*                                                                               
FILMEDZ  DC    AL1(QSMEDZ),C'MED',AL2(FILMEDZX-*)                               
*                                                                               
         DC    AL1(IOMEDDIR/256),C'MEDDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOMEDZFL/256,20,08),AL2(32)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMEDFIL/256),C'MEDFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOMEDZDR/256,20,34),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
FILMEDZX DC    AL1(EOT)                                                         
*&&                                                                             
*                                  ** ACCOUNT SYSTEM FILES **                   
FILACC   DC    AL1(QSACC),C'ACC',AL2(FILACCX-*)                                 
*                                                                               
         DC    AL1(IOACCFIL/256),C'ACCFIL '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,42,49),AL2(1000)                                           
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCSPD/256),XL7'00'                                        
         DC    AL1(QSSPT,IOSPTDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCSPF/256),XL7'00'                                        
         DC    AL1(QSSPT,IOSPTFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCNTD/256),XL7'00'                                        
         DC    AL1(QSNET,IOSPTDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCNTF/256),XL7'00'                                        
         DC    AL1(QSNET,IOSPTFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCPTD/256),XL7'00'                                        
         DC    AL1(QSPRT,IOPRTDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCPTF/256),XL7'00'                                        
         DC    AL1(QSPRT,IOPRTFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCDIR/256),C'ACCDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOACCMST/256,42,08),AL2(54)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCMST/256),C'ACCMST '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOACCDIR/256,42,56),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
FILACCX  DC    AL1(EOT)                                                         
*                                  ** MBASE SYSTEM FILES **                     
FILMBA   DC    AL1(QSMBA),C'MBA',AL2(FILMBAX-*)                                 
*                                                                               
         DC    AL1(IOMBADIR/256),C'MBADIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOMBAFIL/256,32,08),AL2(44)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMBANDX/256),C'MBANDX '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOMBAFIL/256,32,08),AL2(44)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMBAFIL/256),C'MBAFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOMBADIR/256,32,46),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMBUDIR/256),C'MBUDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOMBUFIL/256,32,08),AL2(44)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMBUFIL/256),C'MBUFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOMBUDIR/256,32,46),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMBAGDR/256),XL7'00'                                        
         DC    AL1(QSCON,IOGENDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOMBAGFL/256),XL7'00'                                        
         DC    AL1(QSCON,IOGENFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
FILMBAX  DC    AL1(EOT)                                                         
*                                  ** CONTROL SYSTEM FILES **                   
FILCON   DC    AL1(QSCON),C'CON',AL2(FILCONX-*)                                 
*                                                                               
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,42),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOCONACD/256),XL7'00'                                        
         DC    AL1(QSACC,IOACCDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOCONACM/256),XL7'00'                                        
         DC    AL1(QSACC,IOACCMST/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOCONMDR/256),XL7'00'                                        
         DC    AL1(QSMED,IOMEDDIR/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOCONMFL/256),XL7'00'                                        
         DC    AL1(QSMED,IOMEDFIL/256)                                          
         DC    AL1(0,0,0),AL2(0)                                                
         DC    XL5'00'                                                          
*                                                                               
FILCONX  DC    AL1(EOT)                                                         
*                                                                               
FILTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* TABLE OF GLOBAL SYSTEM FILES (FILE NUMBERS 10 THRU 15)                        
*                                                                               
SYSTAB   DS    0X                                                               
*                                                                               
         DC    AL1(15),C'CTFILE '                                               
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
*                                                                               
SYSTABX  DS    AL1(EOT)                                                         
         EJECT                                                                  
* SYSTEM FILE COMMANDS TABLE                                                    
*                                                                               
CMDTAB   DS    0X                                                               
*                                  INDEX SEQUENTIAL COMMANDS                    
CMDIS    DC    AL1(FILIVL+FILIIS,0),AL2(CMDISX-*)                               
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,0,0)                                        
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
         DC    C'DMADD  ',AL1(IOSOXADD,0,0)                                     
         DC    C'DMWRT  ',AL1(IOSOXWRT,0,0)                                     
CMDISX   DC    AL1(EOT)                                                         
*                                  DIRECT ACCESS COMMANDS                       
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX-*)                                      
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ,0)                              
         DC    C'ADDREC ',AL1(IOSOXADD,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOSOXPUT,CMDIDARQ,0)                              
CMDDAX   DC    AL1(EOT)                                                         
*                                                                               
CMDTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE AN I/O TO ANY STANDARD SYSTEM FILE                 *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTES (LOW ORDER 2 BYTES) SET FROM IO EQUATES *         
*           CONTAINS - FILE NUMBER       (ZERO=USE IOFILE)            *         
*                      COMMAND NUMBER    (ZERO=USE IOCMND)            *         
*                      COMMAND QUALIFIER (READ LOCK/READ DELETES)     *         
*                      I/O AREA NUMBER   (ZERO=USE IOADDR)            *         
*                                                                     *         
* EXIT - CC=LOW IF A HARD I/O ERROR OCCURED                           *         
*        CC=EQUAL IF I/O SUCCESSFUL (NO ERRORS)                       *         
*        CC=HIGH IF A SOFT ERROR (EOF/NOT FOUND/DELETED)              *         
*        IOADDR=A(I/O AREA USED)                                      *         
*        IOERR=DATAMGR ERROR BYTE                                     *         
*        IOKEYSV=SAVE IOKEY VALUE (BEFORE I/O IS EXECUTED)            *         
*        IODA=DISK ADDRESS EXTRACTED FOR I/S RECORD (I/S D/A PAIR)    *         
*                                                                     *         
* NOTE - FOR INDEX SEQUENTIAL I/O'S IOKEY IS ALWAYS SAVED IN IOKEYSV  *         
*        BEFORE I/O IS EXECUTED. FOR D/A FILE I/O'S IF IODA IS ZERO   *         
*        AND FILE HAS A DIRECTORY ATTACHED (I/S D/A PAIR) THE READ    *         
*        SPECIFIED TO THE FILE (HIGH/READ) IS EXECUTED TO THE         *         
*        DIRECTORY.                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R6,R8,R9,RA,RB                                                   
APIOEX   NMOD1 IOWORKX-IOWORKD,**IOEX**,CLEAR=YES                               
         USING IOWORKD,RC          RC=A(LOCAL WORKING STORAGE)                  
         L     R5,ATWA             R5=A(TWA)                                    
         ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOFLAG,0            RESET I/O FLAG BYTE                          
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         TM    IOCTRL+3,IOLOCK     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
         TM    IOCTRL+3,IORDEL     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
         LA    R1,IO1+IO2          ESTABLISH I/O AREA ADDRESS                   
         N     R1,IOCTRL                                                        
         BZ    IOEX2                                                            
*                                                                               
         SRL   R1,6                R1=I/O AREA NUMBER                           
         CLM   R1,1,ACIONUM                                                     
         BNH   *+6                                                              
         DC    H'0'                I/O AREA NUMBER INVALID                      
         BCTR  R1,0                                                             
         MH    R1,ACIOLEN                                                       
         A     R1,ACIOADD                                                       
         ST    R1,IOAREAD          SAVE I/O AREA ADDRESS                        
*                                                                               
         TM    ACIOIND,ACIOIDA     TEST D/A IN I/O AREA                         
         BZ    *+8                                                              
         LA    R1,L'IODA(R1)                                                    
         TM    ACIOIND,ACIOIWK     TEST WORK IN I/O AREA                        
         BZ    *+8                                                              
         LA    R1,L'IOWORK(R1)                                                  
         STCM  R1,15,IOADDR        SET REAL I/O ADDRESS                         
*                                                                               
IOEX2    LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   IOEX4                                                            
         OC    IOFILE,IOFILE       CALLER MUST SUPPLY FILE NAME                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         B     IOEX20                                                           
*                                                                               
IOEX4    SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILNTRY         POINT TO LOCAL SYSTEM FILES                  
         LA    R0,IOFILGLB         FILES 14-15 ARE GLOBAL                       
         CR    R1,R0                                                            
         BL    *+8                                                              
         L     RE,ASYSTAB          POINT TO GLOBAL SYSTEM FILES                 
*                                                                               
         USING FILTABD,RE                                                       
IOEX6    CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX6                                                            
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IOFILNM,IOFILNM     TEST NATIVE SYSTEM FILE                      
         BZ    IOEX600                                                          
         GOTO1 IOSWITCH,SCSWSYSN   SWITCH TO NATIVE SYSTEM IF REQUIRED          
         BNE   IOEXX                                                            
         B     IOEX7                                                            
*                                                                               
IOEX600  MVC   IOSWSYS(L'IOSWSYS+L'IOSWFIL),FILSYSN                             
         L     RE,AFILTAB                                                       
         SR    R1,R1                                                            
IOEX602  CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,RE),IOSWSYS     MATCH ON SYSTEM SWITCH NUMBER                
         BE    *+16                                                             
         ICM   R1,3,4(RE)                                                       
         LA    RE,5(R1,RE)                                                      
         B     IOEX602                                                          
         MVC   IOSWSYSN,1(RE)      SAVE SWITCH-TO SYSTEM NAME                   
         LA    RE,6(RE)            POINT TO FIRST FILE ENTRY                    
IOEX604  CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLC   FILNUM,IOSWFIL      MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX604                                                          
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IOFILNM,IOFILNM     TEST NATIVE FILE TO THIS SYSTEM              
         BNZ   *+6                                                              
         DC    H'0'                NO - KILL THE APPLICATION                    
         GOTO1 IOSWITCH,IOSWSYS    SWITCH TO CORRECT SYSTEM                     
         BE    IOEX7                                                            
         GOTO1 IOSW,SCSWSYSN       CAN'T SWITCH - SWITCH BACK TO NATIVE         
         MVI   IOERR,X'FF'         SET SWITCH FAILURE ERROR BITS                
         B     IOEXX2                                                           
*                                                                               
IOEX7    L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IOCTRL                                                        
         BNZ   IOEX8                                                            
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     IOEX20                                                           
*                                                                               
IOEX8    CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),0(RE)                                                   
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),IOFILI                                                  
         BNE   *+12                                                             
         LA    RE,4(RE)                                                         
         B     IOEX10                                                           
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IOEX8                                                            
*                                                                               
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
IOEX10   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDTABL(RE)                                                   
         B     IOEX10                                                           
                                                                                
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
         CLI   CMDNUMB,IOSOXADD                                                 
         BE    *+8                                                              
         CLI   CMDNUMB,IOSOXWRT                                                 
         BNE   *+8                                                              
         OI    IOQ,X'24'           SPECIAL SOX WRITE COMMAND                    
*                                                                               
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IOEX16                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX18                                                           
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    IOEX11                                                           
         MVC   IODA,IODAOVER       YES - SET D/A AND CLEAR OVERRIDE             
         XC    IODAOVER,IODAOVER                                                
         B     IOEX16                                                           
*                                                                               
IOEX11   ICM   R1,15,IOAREAD       R1=A(I/O AREA)                               
         BZ    IOEX14                                                           
         TM    ACIOIND,ACIOIDA     TEST D/A IN I/O AREA                         
         BZ    IOEX12                                                           
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
         OC    0(L'IODA,R1),0(R1)                                               
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         LA    R1,L'IODA(R1)                                                    
*                                                                               
IOEX12   TM    ACIOIND,ACIOIWK     TEST WORK IN I/O AREA                        
         BZ    IOEX14                                                           
         OC    0(L'IOWORK,R1),0(R1)                                             
         BZ    *+10                                                             
         MVC   IOWORK,0(R1)        YES - SET WORK                               
         LA    R1,L'IOWORK(R1)                                                  
*                                                                               
IOEX14   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX16                                                           
*                                                                               
         TM    IOFILI,FILIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,FILIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       TURN OFF FILE INDICATORS                     
         L     R0,IODUB                                                         
         ZIC   R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTO1 AIO                 RECURSE FOR DIRECTORY I/O                    
         BE    IOEX16              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
*                                                                               
IOEX16   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO I/O AREA ADDRESS                          
         LA    RF,SAVAREA                                                       
         MVC   SAVFILE-SAVAREA(7,RF),IOFILNM                                    
         MVC   IOWFILE,IOFILNM     SET FILE NAME IN WORK AREA                   
         TM    IOINDS1,IOIDMP6     SET 6TH PARAMETER IF REQUIRED                
         BZ    *+10                                                             
         MVC   IODMCB+20(4),IODMP6                                              
         GOTO1 VDMGR,IODMCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWDMWK             
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         TM    ACIOIND,ACIOIDA     TEST D/A IN I/O AREA                         
         BZ    *+14                                                             
         MVC   0(L'IODA,R1),IODA                                                
         LA    R1,L'IODA(R1)       YES - BUMP BY D/A LENGTH                     
         TM    ACIOIND,ACIOIWK     TEST WORK IN I/O AREA                        
         BZ    *+10                                                             
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOEXX               EXIT TO CALLER                               
*                                                                               
IOEX18   TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX20                                                           
         MVC   IOWFILE,IOFILNM     SET FILE NAME IN WORK AREA                   
         LA    RF,SAVAREA                                                       
         MVC   SAVFILE-SAVAREA(7,RF),IOFILNM                                    
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,FILIID      TEST I/S FILE HAS D/A ATTACHED               
         BZ    *+12                YES - MUST READ INTO IOAREA                  
         TM    IOFILI,FILIVL                                                    
         BZ    *+14                                                             
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDMGR,IODMCB,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
         TM    IOFILI2,FILIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IOEX182                                                          
         ZIC   R1,IOFILKL          YES - EXTRACT DISK ADDRESS                   
         ZIC   R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)        POINT TO DISK ADDRESS                        
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         TM    ACIOIND,ACIOIDA     TEST D/A IN I/O AREA                         
         BZ    *+10                                                             
         MVC   0(L'IODA,R1),IODA                                                
         B     IOEXX                                                            
*                                                                               
IOEX182  ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         TM    ACIOIND,ACIOIWK     TEST WORK IN I/O AREA                        
         BZ    IOEXX                                                            
         TM    ACIOIND,ACIOIDA     TEST D/A IN I/O AREA                         
         BZ    *+8                                                              
         LA    R1,L'IODA(R1)       YES - BUMP BY D/A LENGTH                     
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOEXX                                                            
*                                                                               
IOEX20   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDMGR,IODMCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEXX    TM    IOINDS1,IOISWAUT    TEST AUTO SWITCH BACK AFTER I/O              
         BZ    IOEXX2                                                           
         TM    IOFLAG,IOFSWTCH     TEST SYSTEM SWITCH OCCURRED                  
         BZ    IOEXX2                                                           
         GOTO1 IOSWITCH,SCSWSYSP   SWITCH TO PREVIOUS SYSTEM                    
*                                                                               
IOEXX2   MVI   IOQ,1               SET I/O COMPLETED OK                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    IOERR,IOERRS                                                     
         BZ    IOEXXX                                                           
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         MVI   IOQ,2               SET IRRECOVERABLE ERROR                      
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   IOEXXX                                                           
         MVI   IOQ,0               SET LOGICAL I/O ERROR                        
*                                                                               
IOEXXX   CLI   IOQ,1               SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SWITCH TO A SYSTEM                                       *         
*                                                                     *         
* NTRY - R1=A(LOGICAL SYSTEM NUMBER)                                  *         
* EXIT - CC=LOW   - USER NOT AUTHORISED FOR SYSTEM                    *         
*        CC=EQUAL - SWITCH SUCCESSFUL                                 *         
*        CC=HIGH  - SYSTEM NOT AVAILABLE (ONLINE ONLY)                *         
* NOTE - IF ERROR OCCURRS IOERR IS SET TO X'FF' WHICH WILL RETURN A   *         
*        CC OF HIGH FROM I/O ROUTINE WITH FVMSGNO SET TO FVFIOER. IT  *         
*        IS THE CALLER'S RESPONSIBILITY TO DEAL WITH THIS OTHERWISE   *         
*        A RANDOM DATAMGR ERROR WILL BE REPORTED.                     *         
***********************************************************************         
         SPACE 1                                                                
IOSWITCH CLC   SCSWSYSC,0(R1)      TEST SWITCHED TO CORRECT SYSTEM              
         BNE   IOSW                                                             
         CLC   SCSWSE,IOSWSE       TEST CORRECT SE NUMBER                       
         BER   RE                                                               
IOSW     LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   IOBYTE,0(R1)        SAVE SYSTEM NUMBER                           
         CLI   0(R1),QSCON         TEST SWITCH TO CONTROL SYSTEM                
         BE    IOSW5                                                            
*&&UK*&& CLI   0(R1),QSMEDZ        TEST SWITCH TO MEDZ SYSTEM                   
*&&UK*&& BE    IOSW5                                                            
         LA    RE,ASSWTAB                                                       
         USING SYSSWTAB,RE         RE=A(SYSTEM SWITCH TABLE)                    
         LA    RF,SYSSWMAX                                                      
IOSW2    CLC   SYSSWSOV,IOBYTE     MATCH ON LOGICAL SYSTEM NUMBER               
         BNE   IOSW4                                                            
         LA    R1,SYSSWSYS         FOUND - POINT R1 TO ACTUAL SE NUMBER         
         CLI   IOSWSE,0            SPECIFIC USER SE NUMBER SUPPLIED?            
         BE    IOSW5                                                            
         CLC   SYSSWSYS,IOSWSE     MATCH ON USER SE NUMBER                      
         BE    IOSW5                                                            
*                                                                               
IOSW4    LA    RE,SYSSWLEN(RE)     BUMP TO NEXT SWITCH TABLE ENTRY              
         BCT   RF,IOSW2                                                         
         MVI   IOBYTE,0            SET CC=LOW FOR INVALID SYSTEM                
         B     IOSWX                                                            
*                                                                               
IOSW5    MVC   SCSWSE,IOSWSE       SAVE USER SUPPLIED SE NUMBER                 
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   IOSW6                                                            
         ICM   RF,15,SCAUTL        YES - MOVE SE NUMBER TO UTL                  
         MVC   TSYS-UTLD(,RF),0(R1)                                             
         B     IOSW8                                                            
*                                                                               
IOSW6    MVC   IODMCB(1),0(R1)     SWITCH TO A SYSTEM                           
         MVC   IODMCB+1(3),=X'FFFFFF'                                           
         XC    IODMCB+4(4),IODMCB+4                                             
         GOTO1 VSWITCH,IODMCB                                                   
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BE    IOSW8                                                            
         MVI   IOBYTE,2            SET CC=HIGH FOR CAN'T SWITCH                 
         B     IOSWX                                                            
*                                                                               
IOSW8    MVC   SCSWSYSP,SCSWSYSC   SAVE PREVIOUS SYSTEM NUMBER                  
         MVC   SCSWSYSC,IOBYTE     SAVE CURRENT SYSTEM NUMBER                   
         OI    IOFLAG,IOFSWTCH     SET SYSTEM SWITCH OCCURRED                   
*                                                                               
IOSW10   MVI   IOBYTE,1            SET CC=EQUAL FOR OK                          
*                                                                               
IOSWX    CLI   IOBYTE,1            SET CC FOR CALLER                            
         BE    *+8                                                              
         MVI   IOERR,X'FF'         SET ALL ERROR BITS ON                        
         LR    RE,R0                                                            
         BR    RE                  RETURN TO CALLER                             
         DROP  RE                                                               
         SPACE 1                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN OPTIONS FIELD                                *         
*                                                                     *         
* NTRY - FVAL MUST HAVE BEEN CALLED TO EXTRACT TWA FIELD INTO FVIHDR  *         
*        AND FVIFLD.                                                  *         
*        R1=A(OPTIONS VALIDATION TABLE)                               *         
*                                                                     *         
* EXIT - CC=EQUAL IF OPTIONS FIELD IS OK                              *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET                       *         
***********************************************************************         
         SPACE 1                                                                
VALOPTS  NMOD1 VOPTX-VOPTD,**VOPT**,CLEAR=YES                                   
         USING VOPTD,RC            RC=A(LOCAL W/S)                              
         ST    R1,VOATAB                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   FVILEN,0            TEST ANY INPUT                               
         BE    VALOPT26                                                         
         MVC   SCPARM+08(2),=C',='                                              
         MVC   SCPARM+10(2),SCCHARS                                             
         GOTO1 VSCANNER,SCPARM,(30,FVIHDR),VOAREA                               
         MVC   INOPTN,4(R1)        SAVE NUMBER OF LINES INPUT                   
         CLI   INOPTN,0            TEST FOR INVALID INPUT                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALOPTSX                                                         
         MVI   FVINDX,1            SET FIRST FIELD INDEX NUMBER                 
         LA    R3,VOAREA           R3=A(SCANNER TABLE)                          
         B     VALOPT4                                                          
*                                  BUMP TO NEXT BLOCK ENTRY                     
VALOPT2  ZIC   R1,FVINDX           BUMP MULTIPLE FIELD INDEX                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,INOPTN         TEST ALL OPTIONS PROCESSED                   
         BH    VALOPT26                                                         
         STC   R1,FVINDX                                                        
         LA    R3,22+30(R3)                                                     
*                                                                               
VALOPT4  CLI   0(R3),0             TEST VALID KEYWORD LENGTH                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VALOPTSX                                                         
         CLI   0(R3),L'SCOPTNAM+1                                               
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALOPTSX                                                         
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                R1=KEYWORD LENGTH-1                          
         LA    RE,12(R3,R1)        POINT TO LAST CHARACTER OF KEYWORD           
         MVI   4(R3),0                                                          
         CLI   0(RE),C'<'          TEST LESS THAN CHARACTER                     
         BNE   *+8                                                              
         OI    4(R3),OPTLEQ                                                     
         CLI   0(RE),C'>'          TEST GREATER THAN CHARACTER                  
         BNE   *+8                                                              
         OI    4(R3),OPTGEQ                                                     
         CLC   0(1,RE),SCSLASH     TEST NOT EQUALS CHARACTER                    
         BNE   *+8                                                              
         OI    4(R3),OPTNEQ                                                     
         CLI   4(R3),0             TEST ANY SPECIAL CHARACTERS                  
         BE    *+6                                                              
         BCTR  R1,0                YES - SUBTRACT 1 FROM KEYWORD LENGTH         
         L     R2,VOATAB           SAVE A(OPTIONS VALIDATION TABLE)             
         USING OPTTABD,R2          R2=A(OPTIONS VALIDATION TABLE)               
         NI    SCINDS,X'FF'-SCINOACC                                            
*                                  SEARCH TABLE FOR KEYWORD                     
VALOPT6  CLI   OPTTABD,EOT         TEST E-O-T                                   
         BNE   VALOPT7                                                          
         MVC   FVMSGNO,=AL2(FVFKINV) YES - INVALID KEYWORD                      
         TM    SCINDS,SCINOACC                                                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNAOPT) OR NO AUTHORIZATION                       
         B     VALOPTSX                                                         
VALOPT7  LR    R0,R1                                                            
         GOTO1 AEXPOPT,OPTTABD     EXPAND OPTION NAME                           
         LR    R1,R0                                                            
         CLI   SCOPTSHT,C' '       TEST OPTION HAS A SHORT FORM                 
         BE    VALOPT8                                                          
         LA    RE,2                                                             
         CLI   SCOPTSHT+2,C' '     TEST 2 OR 3 BYTE SHORT FORM                  
         BNE   *+8                                                              
         LA    RE,1                                                             
         CLI   SCOPTSHT+1,C' '     TEST 1 OR 2 BYTE SHORT NAME                  
         BNE   *+6                                                              
         SR    RE,RE                                                            
         CR    R1,RE               TEST L'INPUT IS L'SHORT NAME                 
         BNE   VALOPT8                                                          
         EX    R1,*+8                                                           
         BE    VALOPT10                                                         
         CLC   SCOPTSHT(0),12(R3)  MATCH ON SHORT OPTION NAME                   
VALOPT8  LA    RE,1(R1)                                                         
         CLM   RE,1,OPTMINKL       TEST L'INPUT LESS THAN MIN ALLOWED           
         BL    VALOPT14                                                         
         EX    R1,*+8                                                           
         BNE   VALOPT14                                                         
         CLC   SCOPTNAM(0),12(R3)  MATCH ON FULL OPTION NAME                    
VALOPT10 CLI   ASONOFF,ASOFF       TEST OFF-LINE                                
         BE    VALOPT12                                                         
         MVC   SCWORK(2),OPTAUTH                                                
         NC    SCWORK(2),CUAUTH                                                 
         CLC   SCWORK(2),OPTAUTH                                                
         BNE   VALOPT14                                                         
         TM    OPTINDS,OPTIDDS     TEST DDS ONLY OPTION                         
         BZ    VALOPT11                                                         
         TM    CUSTAT,CUSDDS       YES - TEST THIS IS A DDS TERMINAL            
         BZ    VALOPT14                                                         
         B     VALOPT12                                                         
VALOPT11 TM    OPTINDS2,OPTNOSEC   TEST NO SECURITY FOR OPTION                  
         BO    VALOPT12                                                         
         OC    ACASEC,ACASEC       TEST SECURITY ACCESS                         
         BZ    VALOPT12                                                         
         GOTO1 VSECRET,SCPARM,('SECPOPTP',ACASEC),OPTOPTN                       
         BE    VALOPT12                                                         
         OI    SCINDS,SCINOACC                                                  
         B     VALOPT14                                                         
*                                  TEST RECORD/ACTION VALID                     
VALOPT12 CLI   OPTRECB,0           TEST VALID FOR ALL RECORDS                   
         BE    *+14                                                             
         CLC   INREC,OPTRECB       NO - MATCH ON RECORD NUMBER                  
         BNE   VALOPT14                                                         
         CLI   OPTACTB,0           TEST VALID FOR ALL ACTIONS                   
         BE    VALOPT16                                                         
         CLC   INACT,OPTACTB       YES - MATCH ON ACTION NUMBER                 
         BE    VALOPT16                                                         
         TM    TWAMODE,TWAMSEL     NO  - TEST IN SELECT MODE                    
         BZ    VALOPT14                                                         
         TM    OPTINDS2,OPTISEL    NO  - VALID IN SELECT MODE                   
         BZ    VALOPT14                                                         
         CLC   SCACTN,OPTACTB      YES - MATCH ON ACTION NUMBER                 
         BE    VALOPT16                                                         
*                                                                               
VALOPT14 ZIC   R0,OPTELEN          BUMP TO NEXT TABLE ENTRY                     
         AR    R2,R0                                                            
         B     VALOPT6                                                          
*                                                                               
VALOPT16 ICM   RE,1,4(R3)          TEST SPECIAL CHARACTER INPUT                 
         BZ    VALOPT18                                                         
         EX    RE,*+8                                                           
         BNZ   VALOPT18                                                         
         TM    OPTINDS2,0          TEST SPECIAL CHARACTER ALLOWED               
         MVC   FVMSGNO,=AL2(FVFIGLE) NO - INVALID                               
         B     VALOPTSX                                                         
*                                                                               
VALOPT18 MVC   SCWORK(L'INOPTR),INOPTX                                          
         NC    SCWORK(L'INOPTR),OPTOPTB                                         
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKINV)                                            
         B     VALOPTSX                                                         
         MVC   SCWORK(L'INOPTR),INOPTI                                          
         NC    SCWORK(L'INOPTR),OPTOPTB                                         
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKDUP)                                            
         B     VALOPTSX                                                         
         OC    INOPTI,OPTOPTB      SET THIS OPTION HAS BEEN INPUT               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R3)          TEST DATA INPUT AFTER EQUALS SIGN            
         BNZ   VALOPT19                                                         
         CLI   OPTMINDL,0          NO - TEST IF THAT'S OK                       
         BE    VALOPT19                                                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         MVI   FVSUBX,1                                                         
         B     VALOPTSX                                                         
*                                                                               
VALOPT19 MVI   FVIFLD,C' '                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         LTR   RF,RF                                                            
         BZ    *+20                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),22(R3)                                                 
         XC    FVIHDR,FVIHDR                                                    
         LA    RE,L'FVIHDR+1(RF)                                                
         STC   RE,FVIHDR                                                        
         LR    R0,RF               R0=LENGTH OF FIELD-1                         
         GOTO1 AFVAL,0                                                          
*                                                                               
         CLC   FVILEN,OPTMINDL     TEST L'DATA IS VALID                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALOPTSX                                                         
         CLC   FVILEN,OPTMAXDL                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALOPTSX                                                         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,3,OPTIADDR                                                    
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION RTN                
         BZ    *+16                                                             
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         ICM   RF,7,ACOBASE+1                                                   
         B     VALOPT21                                                         
*                                                                               
         TM    OPTINDS,OPTATAB                                                  
         BZ    VALOPT20                                                         
         TM    ACOPTIND,ACOPTITB   TEST USE ACTBASE NOT ACOBASE                 
         BZ    *+12                                                             
         A     RF,ACTBASE                                                       
         B     *+8                                                              
         A     RF,ACOBASE                                                       
         B     VALOPT22                                                         
VALOPT20 TM    OPTINDS,OPTARTN     TEST A(VALIDATION ROUTINE)                   
         BNZ   *+6                                                              
         DC    H'0'                NO - TABLE IS SCREWY                         
         A     RF,ACOBASE                                                       
VALOPT21 XC    SCWORK,SCWORK                                                    
         BASR  RE,RF               GO TO ROUTINE                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALOPT24                                                         
         MVI   FVFLAG,X'01'        INDICATE A USER ERROR MESSAGE                
         BNE   VALOPTSX                                                         
*                                                                               
VALOPT22 TM    OPTINDS,OPTTABH     LOOK-UP VALUES TABLE                         
         BZ    *+8                                                              
         LA    RF,60(RF)                                                        
         ZIC   R0,0(RF)            R0=L'LHS OF TABLE                            
         ZIC   R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'TABLE                                   
         ZIC   RE,FVXLEN           RE=L'DATA-1                                  
VALOPT23 CLI   0(RF),EOT           TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFDINV) YES - INVALID DATA VALUE                   
         B     VALOPTSX                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RF)     MATCH INPUT WITH TABLE                       
         BE    *+10                                                             
         AR    RF,R0               BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT23                                                         
         AR    RF,R0               EXTRACT RHS OF TABLE INTO WORK               
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VALOPT24                                                         
         MVC   SCWORK(0),0(RF)                                                  
*                                                                               
VALOPT24 BAS   RE,SETOPT           MOVE DATA TO OPTION AREA                     
         B     VALOPT2                                                          
*                                                                               
VALOPT26 MVI   FVINDX,0            RESET INDEX/SUB-INDEX                        
         MVI   FVSUBX,0                                                         
         L     R2,VOATAB           APPLY ANY DEFAULT OPTION VALUES              
         LA    R3,VOAREA                                                        
         XC    0(12,R3),0(R3)      TEST ALL REQUIRED OPTIONS WERE INPUT         
         MVC   SCWORK(L'INOPTR),INOPTR                                          
         NC    SCWORK(L'INOPTR),INOPTI                                          
         CLC   SCWORK(L'INOPTR),INOPTR                                          
         BE    VALOPT30                                                         
         SR    R0,R0                                                            
*                                  FIND FIRST MISSING REQUIRED OPTION           
VALOPT28 CLI   OPTTABD,EOT         TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                REQUIRED OPTION DOESN'T EXIST                
         MVC   SCWORK(L'INOPTR),INOPTR                                          
         NC    SCWORK(L'INOPTR),OPTOPTB                                         
         BNZ   *+14                                                             
         IC    R0,OPTELEN                                                       
         AR    R2,R0                                                            
         B     VALOPT28                                                         
         GOTO1 AEXPOPT,OPTTABD                                                  
         MVC   FVXTRA(L'SCOPTNAM),SCOPTNAM                                      
         MVC   FVMSGNO,=AL2(FVFREQD) REQUIRED OPTION IS MISSING                 
         B     VALOPTSX                                                         
*                                  APPLY ANY DEFAULT OPTION VALUES              
VALOPT30 CLI   OPTTABD,EOT         TEST E-O-T                                   
         BE    VALOPTSX                                                         
         CLI   OPTRECB,0           ONLY IF RECORD IS VALID                      
         BE    *+14                                                             
         CLC   INREC,OPTRECB                                                    
         BNE   VALOPT32                                                         
         CLI   OPTACTB,0           AND ACTION IS VALID                          
         BE    VALOPT31                                                         
         CLC   INACT,OPTACTB                                                    
         BE    VALOPT31                                                         
         TM    TWAMODE,TWAMSEL     NO  - TEST IN SELECT MODE                    
         BZ    VALOPT32                                                         
         TM    OPTINDS2,OPTISEL    NO  - VALID IN SELECT MODE                   
         BZ    VALOPT32                                                         
         CLC   SCACTN,OPTACTB      NO  - MATCH ON ACTION NUMBER                 
         BNE   VALOPT32                                                         
*                                                                               
VALOPT31 MVC   SCWORK(L'INOPTR),INOPTI                                          
         NC    SCWORK(L'INOPTR),OPTOPTB                                         
         BNZ   VALOPT32                                                         
         MVC   SCWORK(L'INOPTR),INOPTX                                          
         NC    SCWORK(L'INOPTR),OPTOPTB                                         
         BNZ   VALOPT32                                                         
         TM    OPTINDS,OPTDFLT     AND THERE IS A DEFAULT VALUE (WHEW)          
         BZ    VALOPT32                                                         
         MVC   SCWORK(L'OPTDVAL),OPTDVAL                                        
         BAS   RE,SETOPT           SET DEFAULT OPTION VALUE                     
VALOPT32 ZIC   R0,OPTELEN                                                       
         AR    R2,R0               BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT30                                                         
*                                                                               
VALOPTSX CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         DROP  RC                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MOVE OPTION VALUE FROM WORK INTO OPTION/FORMAT AREA      *         
*                                                                     *         
* NTRY - R2=A(OPTION TABLE ENTRY)                                     *         
*        R3=A(SCANNER BLOCK ENTRY)                                    *         
*        SCWORK=OPTION VALUE TO BE SET                                *         
***********************************************************************         
         SPACE 1                                                                
SETOPT   SR    RF,RF                                                            
         ICM   RF,3,OPTOADDR                                                    
         LA    RF,WORKD(RF)                                                     
         ZIC   R1,OPTOUTDL         R1=L'DATA (EXCLUDING QUALIFIER)              
         BCTR  R1,0                                                             
         TM    OPTINDS2,OPTGEQ+OPTLEQ+OPTNEQ                                    
         BZ    *+14                                                             
         MVC   0(1,RF),4(R3)       MOVE DATA QUALIFIER                          
         LA    RF,1(RF)                                                         
         TM    OPTINDS,OPTBOOL     TEST IF A BIT VALUE                          
         BZ    *+12                                                             
         EX    R1,SETOPTOR         OR VALUE INTO OUTPUT AREA                    
         B     *+8                                                              
         EX    R1,SETOPTMV         MOVE VALUE TO OUTPUT AREA                    
         OC    INOPTR,OPTOPTR      MERGE OPTION INDICATORS                      
         OC    INOPTX,OPTOPTX                                                   
         BR    RE                                                               
         SPACE 1                                                                
SETOPTOR OC    0(0,RF),SCWORK                                                   
SETOPTMV MVC   0(0,RF),SCWORK                                                   
         SPACE 1                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET-UP KEY VALUES IN SCREEN                              *         
*                                                                     *         
* NTRY - R1=ZERO TO SET TWA KEY VALUES, N-Z TO SAVE TWA KEY VALUES    *         
*        INKEYN=NUMBER OF KEY FIELDS ON CURRENT SCREEN                *         
*        INKEYC=NUMBER OF KEY COMPONENTS                              *         
*        INKEYT=KEY COMPONENT TABLE (MAXIMUM OF 8 ENTRIES)            *         
*        AINKHDR=A(FIRST SCREEN KEY FIELD HEADER)                     *         
*                                                                     *         
* EXIT - CC=EQUAL IF ANY KEY VALUES WERE INPUT                        *         
*        CC=NOT EQUAL IF NO KEY FIELDS WERE GIVEN                     *         
*                                                                     *         
* NOTE - THIS ROUTINE MAINTAINS A TABLE OF KEY COMPONENT VALUES IN    *         
*        SAVED (SAVKEYS). EACH TIME A KEY FIELD IS INPUT BY THE       *         
*        USER AN ENTRY IS ADDED OR UPDATED IN SYSKEYS WITH THE        *         
*        CURRENT COMPONENT VALUE (EVEN IF FIELD IS NOT VALID). USER   *         
*        MAY INPUT ',' OR ','DATA IN KEY FIELD AT TOP OF SCREEN TO    *         
*        INDICATE THAT A PREVIOUS KEY COMPONENT IS TO BE USED. IF     *         
*        LOADED SCREEN DOES NOT HAVE ANY KEY FIELDS A STRING OF KEY   *         
*        COMPONENTS WITH ',' DELIMITERS IS GENERATED IN KEY FIELD.    *         
***********************************************************************         
         SPACE 1                                                                
SETKEY   NMOD1 0,**SETK**                                                       
         LR    R0,R1                                                            
         MVI   SCWORK,0            SET NO KEY INPUT                             
         ICM   R1,15,AKEYHDR                                                    
         BZ    SETKEYN                                                          
         LTR   R0,R0                                                            
         BZ    SETKEY1                                                          
         CLI   INKEYN,0                                                         
         BE    SETKEY10                                                         
         B     SETKEY15                                                         
*                                                                               
SETKEY1  GOTO1 AFVAL               EXTRACT KEY FIELD                            
         BE    SETKEY2                                                          
         CLI   INKEYN,0            TEST ANY KEY FIELDS ON SCREEN                
         BNE   *+14                                                             
         MVC   FVIFLD(1),SCDELIM   SET TO AUTO GENERATE KEY                     
         B     SETKEY2                                                          
         ICM   R3,15,AINKHDR                                                    
         BZ    SETKEYN                                                          
         B     SETKEY15                                                         
SETKEY2  LA    R2,INKEYT           R2=A(KEY TABLE)                              
         L     R3,AKEYHDR                                                       
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R0,L'FVIHDR+1                                                    
         SR    R1,R0                                                            
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         SR    R4,R4                                                            
         IC    R4,INKEYC           R4=NUMBER OF KEY COMPONENTS                  
         XC    SCWORK,SCWORK                                                    
         LA    R3,SCWORK                                                        
         CLC   FVIFLD(1),SCDELIM   TEST USER WANTS KEY GENERATED                
         BE    SETKEY3                                                          
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
         LA    R3,1(RF,R3)                                                      
         B     SETKEY9                                                          
*                                                                               
SETKEY3  LA    R1,SAVKEYS                                                       
         SR    RF,RF                                                            
SETKEY4  CLI   0(R1),EOT           TEST END-OF-LIST                             
         BNE   SETKEY6                                                          
         MVC   0(1,R3),SCDELIM     YES - THIS IS A NULL FIELD                   
         LA    R3,1(R3)                                                         
         B     SETKEY8                                                          
SETKEY6  CLC   0(1,R1),0(R2)       MATCH KEY COMPONENT NUMBER TO TABLE          
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     SETKEY4                                                          
         IC    RF,1(R1)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R1)       MOVE KEY TO OUTPUT AREA                      
         LA    R3,1(RF,R3)                                                      
         MVC   0(1,R3),SCDELIM                                                  
         LA    R3,1(R3)                                                         
SETKEY8  LA    R2,1(R2)            BUMP TO NEXT KEY COMPONENT                   
         BCT   R4,SETKEY3          DO FOR NUMBER OF KEY COMPONENTS              
         LA    R2,INKEYT           RESET R2 & R4 VALUES                         
         SR    R4,R4                                                            
         IC    R4,INKEYC                                                        
         CLI   FVILEN,1            TEST ANY DATA GIVEN WITH KEY                 
         BNH   SETKEY9             NO - GO OUTPUT KEY FIELDS                    
         BCTR  R3,0                                                             
         CLC   0(1,R3),SCDELIM     TEST FOR TRAILING DELIMITERS                 
         BNE   *+8                                                              
         BCT   R3,*-10             YES - CUT BACK TILL NO MORE FOUND            
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),FVIFLD                                                   
         LA    R3,2(RF,R3)                                                      
SETKEY9  BCTR  R3,0                                                             
         CLC   0(1,R3),SCDELIM     TEST FOR TRAILING DELIMITERS                 
         BNE   *+8                                                              
         BCT   R3,*-10             YES - CUT BACK TILL NO MORE FOUND            
         LA    RF,SCWORK                                                        
         SR    R3,RF               R3=L'KEY-1                                   
         BM    SETKEYN                                                          
         CLI   INKEYN,0            TEST ANY KEY FIELDS ON SCREEN                
         BNE   SETKEY14                                                         
         L     R1,AKEYHDR                                                       
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         LA    R0,L'FVIHDR+1                                                    
         SR    RF,R0               RF=MAXIMUM L'KEY FIELD-1                     
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         CR    R3,RF               TEST STRING WILL FIT IN KEY FIELD            
         BNH   *+6                                                              
         LR    R3,RF               NO - OUTPUT MAXIMUM KEY                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,R1),SCWORK                                            
         LA    R3,1(R3)                                                         
         STC   R3,FVILEN-FVIHDR(R1)                                             
*                                                                               
SETKEY10 MVC   SCPARM+08(2),=C',='                                              
         MVC   SCPARM+10(2),SCCHARS                                             
         GOTO1 VSCANNER,SCPARM,AKEYHDR,ACIOADD                                  
         SR    R4,R4                                                            
         ICM   R4,1,4(R1)                                                       
         BZ    SETKEYN                                                          
         LA    R2,INKEYT                                                        
         L     R3,ACIOADD                                                       
         MVI   SCWORK,0            SET NO KEY INPUT                             
         MVI   SCFULL,0                                                         
SETKEY11 SR    RF,RF                                                            
         ICM   RF,1,0(R3)          TEST ANY DATA IN THIS KEY POSITION           
         BZ    SETKEY13            NO - GO TO NEXT COMPONENT                    
         CLI   0(R2),0             KEY NUMBER CAN'T BE ZERO                     
         BE    SETKEY13                                                         
         MVC   SCWORK(1),0(R2)     YES - ADD TO TABLE OF KEY VALUES             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCWORK+2(0),12(R3)                                               
         LA    RF,3(RF)                                                         
         STC   RF,SCWORK+1                                                      
         OC    SAVKEYL,SAVKEYL                                                  
         BNZ   *+12                                                             
         MVI   SAVKEYL+1,3                                                      
         B     SETKEY12                                                         
         GOTO1 VHELLO,SCPARM,(C'D',SCTABNAM),(SCWORK,SAVKEYL),0                 
SETKEY12 GOTO1 VHELLO,SCPARM,(C'P',SCTABNAM),SAVKEYL,SCWORK,0                   
         OI    SCFULL,1                                                         
SETKEY13 LA    R2,1(R2)            BUMP TO NEXT KEY COMPONENT                   
         LA    R3,32(R3)           NEXT KEY POSITION                            
         BCT   R4,SETKEY11         DO FOR NUMBER OF KEY COMPONENTS              
         CLI   SCFULL,0                                                         
         BE    SETKEYN                                                          
         B     SETKEYY                                                          
*                                                                               
SETKEY14 EX    R3,*+8              OUTPUT KEY INTO KEY FIELDS                   
         B     *+10                                                             
         MVC   FVIFLD(0),SCWORK                                                 
         XC    FVIHDR,FVIHDR                                                    
         LA    R3,1(R3)                                                         
         STC   R3,FVILEN                                                        
         LA    R3,L'FVIHDR(R3)                                                  
         STC   R3,FVTLEN                                                        
         GOTO1 VSCUNKEY,SCPARM,(SCDELIM,FVIHDR),(INKEYN,AINKHDR)                
*                                                                               
SETKEY15 LA    R2,INKEYT                                                        
         ICM   R3,15,AINKHDR       R3=A(FIRST TWA FIELD)                        
         BNZ   *+8                                                              
         L     R3,ATWAEND                                                       
         SR    R4,R4                                                            
         ICM   R4,1,INKEYN                                                      
         BZ    SETKEYN                                                          
         MVI   SCWORK,0            SET NO KEY INPUT                             
         MVI   SCFULL,0                                                         
SETKEY16 TM    1(R3),FVAPROT       POSITION TO NEXT UNPROTECTED FIELD           
         BZ    SETKEY19                                                         
SETKEY18 ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         B     SETKEY16                                                         
SETKEY19 CLI   0(R2),0             KEY NUMBER CAN'T BE ZERO                     
         BE    SETKEY28                                                         
         LR    R1,R3               TEST ANY INPUT IN THIS KEY FIELD             
         GOTO1 AFVAL                                                            
         BE    SETKEY20            OK                                           
         TM    APINDS2,APMGKEY0    NO INPUT - SHOULD WE SAVE IT                 
         BZ    SETKEY28            NO - GO TO NEXT COMPONENT                    
         ICM   RF,15,FVADDR                                                     
         TM    FVIIND-FVIHDR(RF),FVITHIS                                        
         BZ    SETKEY28            NO                                           
         XC    SCWORK,SCWORK       CREATE NULL SAVEKEY ENTRY                    
         MVC   SCWORK(1),0(R2)     ADD TO TABLE OF KEY VALUES                   
         SR    RF,RF                                                            
         B     SETKEY25                                                         
SETKEY20 ZIC   RF,FVILEN                                                        
         LA    RE,L'FVIHDR-1(RF,R3) LOOK FOR SEARCH NAME STRING                 
         CLI   0(RE),C'>'          TEST ENDS WITH '>'                           
         BNE   SETKEY24            NO, ASSUME NOT A SEARCH STRING               
         LA    RE,L'FVIHDR(R3)     LOOK FOR START OF NAME STRING                
         LR    R0,RF                                                            
SETKEY21 CLI   0(RE),C'<'                                                       
         BE    SETKEY22                                                         
         LA    RE,1(,RE)                                                        
         BCT   R0,SETKEY21                                                      
         B     SETKEY24            START NOT FOUND, ASSUME NOT SEARCH           
SETKEY22 SR    RF,R0               FOUND, REMOVE IT FROM LENGTH                 
         BZ    SETKEY28            NOTHING ELSE THERE                           
SETKEY23 BCTR  RE,0                NOW LOOK FOR NEW END OF FIELD                
         CLI   0(RE),C' '                                                       
         BH    SETKEY24                                                         
         BCT   RF,SETKEY23                                                      
         B     SETKEY28            NOTHING ELSE THERE                           
SETKEY24 MVC   SCWORK(1),0(R2)     ADD TO TABLE OF KEY VALUES                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     SETKEY25                                                         
         MVC   SCWORK+2(0),L'FVIHDR(R3)                                         
SETKEY25 LA    RF,3(RF)                                                         
         STC   RF,SCWORK+1                                                      
         OC    SAVKEYL,SAVKEYL                                                  
         BNZ   *+12                                                             
         MVI   SAVKEYL+1,3                                                      
         B     SETKEY26                                                         
         GOTO1 VHELLO,SCPARM,(C'D',SCTABNAM),(SCWORK,SAVKEYL),0                 
SETKEY26 GOTO1 VHELLO,SCPARM,(C'P',SCTABNAM),SAVKEYL,SCWORK,0                   
         CLI   SCWORK+2,0          IF KEY NULL                                  
         BE    *+8                 DONT SET KEY INPUT                           
         OI    SCFULL,1                                                         
SETKEY28 LA    R2,1(R2)            BUMP TO NEXT KEY COMPONENT                   
         BCT   R4,SETKEY18         DO FOR NUMBER OF KEY COMPONENTS              
         CLI   SCFULL,0            TEST ANY KEY INPUT                           
         BE    SETKEYN                                                          
         SPACE 1                                                                
SETKEYY  CR    RB,RB               CC=EQUAL IF KEY INPUT                        
         B     *+6                                                              
SETKEYN  LTR   RB,RB               CC=NOT EQUAL IF NO KEY INPUT                 
         XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WHEN FIELD                                                 *         
*                                                                     *         
* NTRY - R1=A(WHEN TWA FIELD HEADER)                                  *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE INWHEN CONTAINS WHEN VALUE        *         
***********************************************************************         
         SPACE 1                                                                
VALWHEN  NMOD1 0,**VALW**                                                       
         MVI   FVMINL,1            SET FIELD REQUIRED                           
         GOTO1 AFVAL               VALIDATE WHEN INPUT FIELD                    
         BNE   VALWHENX                                                         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    R2,R2                                                            
         IC    R2,FVXLEN           R2=L'INPUT-1                                 
         LA    R3,WHENTAB          R3=A(WHEN TABLE)                             
VALWHEN2 CLI   0(R3),EOT           TEST E-O-T                                   
         BE    VALWHEN6                                                         
         MVC   SCWORK(L'WHENTAB-1),1(R3)                                        
         GOTO1 VDICTAT,SCPARM,SCDICONE,SCWORK,0                                 
         EX    R2,*+8                                                           
         BNE   VALWHEN4                                                         
         CLC   FVIFLD(0),SCWORK    MATCH INPUT TO TABLE                         
         IC    R1,0(R3)                                                         
         STC   R1,INWHEN           SET INTERNAL WHEN VALUE                      
         EX    R1,*+8              TEST VALID FOR THIS OVERLAY                  
         BNZ   VALWHEN5                                                         
         TM    INMIX2,0                                                         
VALWHEN4 LA    R3,L'WHENTAB(R3)    BUMP TO NEXT TABLE ENTRY                     
         B     VALWHEN2                                                         
*                                                                               
VALWHEN5 CLI   INWHEN,MIXIOKS      IF INWHEN IS SOON                            
         BNE   VALWHENX                                                         
         CLC   1(4,R3),WHENLTE                                                  
         BNE   *+12                                                             
         MVI   INWHEN,MIXIOKL                                                   
         B     VALWHENX                                                         
*                                                                               
         CLI   INACT,ACTUPD        AND ACTION IS UPDATE                         
         BNE   VALWHENX                                                         
         MVI   INWHEN,MIXIOKU      THEN INWHEN IS UPDATIVE SOON                 
         B     VALWHENX                                                         
*                                                                               
VALWHEN6 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VALWHENX CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         SPACE 2                                                                
WHENTAB  DS    0CL5                ** TABLE OF WHEN VALUES **                   
         DC    AL1(MIXIOKN)        NOW                                          
         DCDDL GE#NOW,9                                                         
         DC    AL1(MIXIOKS)        SOON                                         
         DCDDL GE#SOON,9                                                        
         DC    AL1(MIXIOKS)        SOON                                         
         DCDDL GE#SOON2,9                                                       
         DC    AL1(MIXIOKS)        ASAP (SOON)                                  
         DCDDL GE#ASAP,9                                                        
         DC    AL1(MIXIOKO)        OV (OVERNIGHT)                               
         DCDDL GE#OV,9                                                          
         DC    AL1(MIXIOKO)        ON (OVERNIGHT)                               
         DCDDL GE#ON,9                                                          
         DC    AL1(MIXIOKO)        ON (OVERNIGHT)                               
         DCDDL GE#ONT,9                                                         
         DC    AL1(MIXIOKS)        LATE                                         
         DCDDL GE#LATE,9                                                        
WHENTABX DC    AL1(EOT)                                                         
         SPACE 1                                                                
WHENLTE  DCDDL GE#LATE,9                                                        
         LTORG                                                                  
         DS    0H                                                               
***********************************************************************         
* VALIDATE DESTINATION FIELD                                          *         
*                                                                     *         
* NTRY - R1=A(DESTINATION TWA FIELD HEADER)                           *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE INDEST CONTAINS VALUE             *         
***********************************************************************         
         SPACE 1                                                                
VALDEST  NMOD1 VDSTX-VDSTD,**VALD**,CLEAR=YES                                   
         USING VDSTD,RC            RC=A(LOCAL W/S)                              
         ST    R1,VDSAVER1                                                      
         L     R9,AREP                                                          
         USING REPD,R9                                                          
*                                                                               
         MVC   INDEST,CUUSER                                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R1,IOKEY                                                         
         USING CTIKEY,R1           BUILD ID RECORD KEY                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         LA    R0,VDREC                                                         
         ST    R0,IOADDR                                                        
         GOTO1 AIO,IORD+IOCTFILE                                                
         BE    *+6                                                              
         DC    H'0'                CAN'T READ USER ID RECORD                    
*                                                                               
         LTR   R9,R9               TEST REPORT WORK AVAILABLE                   
         BZ    VDEST04                                                          
         L     R1,IOADDR           YES - EXTRACT ORIGIN DETAILS                 
         LA    R1,CTIDATA                                                       
         XR    R0,R0                                                            
         USING CTORGD,R1                                                        
VDEST02  CLI   CTORGEL,0           TEST E-O-R                                   
         BE    VDEST04                                                          
         CLI   CTORGEL,CTORGELQ    TEST ORIGIN NAME                             
         BE    *+12                                                             
         IC    R0,CTORGLEN                                                      
         BXH   R1,R0,VDEST02                                                    
         MVC   REPUSRN,CTORGNAM    SET TO ORIGIN NAME & ADDRESS                 
         MVC   REPUSRA,CTORGADD                                                 
*                                                                               
VDEST04  L     R1,VDSAVER1                                                      
         GOTO1 AFVAL               VALIDATE DESTINATION FIELD                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK) NO INPUT IS VALID                            
         B     VDEST10                                                          
         GOTO1 VGETIDS,VDPARM,(C'D',IOADDR),0,VDMGR                             
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)          RE=NUMBER OF DESTINATION ID'S                
         BZ    VDEST08                                                          
         L     RF,4(R1)            RF=A(LIST OF VALID DESTINATIONS)             
VDEST06  CLC   FVIFLD(10),0(RF)    MATCH INPUT TO DESTINATION LIST              
         BE    VDEST09                                                          
         LA    RF,12(RF)                                                        
         BCT   RE,VDEST06                                                       
VDEST08  MVC   FVMSGNO,=AL2(FVFEDST)                                            
         B     VALDESTX                                                         
VDEST09  MVC   INDEST,10(RF)       SET DESTINATION ID FROM LIST                 
*                                                                               
VDEST10  TM    ACINDS,ACIORIG      TEST USE ORIGIN NAME                         
         BO    VALDESTX                                                         
         TM    ACINDS,ACIDEST      TEST USE DESTINATION NAME                    
         BZ    VALDESTX            (ORIGIN NAME IS DEFAULT)                     
*                                                                               
         LTR   R9,R9               TEST REPORT WORK AVAILABLE                   
         BZ    VALDESTX                                                         
         LA    R1,IOKEY            GET DESTINATION ID RECORD                    
         USING CTIREC,R1                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,INDEST                                                   
         LA    R0,VDREC                                                         
         ST    R0,IOADDR                                                        
         GOTO1 AIO,IORD+IOCTFILE                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VDEST12  L     R1,IOADDR                                                        
         LA    R1,CTIDATA                                                       
         USING CTDSTD,R1           GET DESTINATION DETAILS                      
         XR    R0,R0                                                            
VDEST14  CLI   CTDSTEL,0                                                        
         BE    VALDESTX                                                         
         CLI   CTDSTEL,CTDSTELQ                                                 
         BE    *+12                                                             
         IC    R0,CTDSTLEN                                                      
         BXH   R1,R0,VDEST14                                                    
         MVC   REPUSRN,CTDSTNAM                                                 
         MVC   REPUSRA,CTDSTADD                                                 
         CLI   ASONOFF,ASOFF       TEST OFF-LINE                                
         BNE   VALDESTX                                                         
         L     RF,REPAMST          OVERRIDE OFF-LINE ORIGIN DETAILS             
         USING MASTD,RF                                                         
         MVC   MCORIGIN,REPUSRN                                                 
         MVC   MCORIGAD,REPUSRA                                                 
         DROP  RF                                                               
*                                                                               
VALDESTX CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         DROP  R1,R9                                                            
         SPACE 1                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OUTPUT TYPE FIELD                                          *         
*                                                                     *         
* NTRY - R1=A(OUTPUT TYPE TWA FIELD HEADER)                           *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE INOTYP CONTAINS VALUE             *         
***********************************************************************         
         SPACE 1                                                                
VALOTYP  NMOD1 VOTYPX-VOTYPD,**VALO**                                           
         USING VOTYPD,RC           RC=A(LOCAL W/S)                              
         GOTO1 AFVAL               VALIDATE OUTPUT TYPE FIELD                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK) NO INPUT IS VALID                            
         B     VALOTYPX                                                         
*                                                                               
         CLI   FVIFLD,C'0'         TEST STARTS WITH A-I,J-R,S-Z,0-9             
         BL    *+12                                                             
         CLI   FVIFLD,C'9'                                                      
         BNH   VALOTY1                                                          
         CLI   FVIFLD,C'S'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'Z'                                                      
         BNH   VALOTY1                                                          
         CLI   FVIFLD,C'J'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'R'                                                      
         BNH   VALOTY1                                                          
         CLI   FVIFLD,C'A'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'I'                                                      
         BNH   VALOTY1                                                          
         CLI   FVIFLD,C'+'         TEST OUTPUT TYPE SPECIALS +,&,#              
         BE    VALOTY1                                                          
         CLI   FVIFLD,C'&&'                                                     
         BE    VALOTY1                                                          
         CLI   FVIFLD,C'#'                                                      
         BE    VALOTY1                                                          
*                                                                               
         MVC   INOTYP,FVIFLD       SAVE SPECIAL CODE IN OUTPUT TYPE             
         CLI   FVIFLD,C'@'         TEST IF SQL TRANSFORM                        
         BE    VALOTY4                                                          
         CLI   FVIFLD,C'/'         TEST IF REMOTE AFP NAME                      
         BE    VALOTY5                                                          
         B     VALOTYER                                                         
*                                                                               
VALOTY1  EQU   *                   VALIDATE OUTPUT TYPE                         
         LA    R1,IOKEY            BUILD OUTPUT TYPE RECORD KEY                 
         USING CTOKEY,R1                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKTYP,C'O'                                                     
         MVC   CTOKID,FVIFLD                                                    
         LA    R0,VOREC                                                         
         ST    R0,IOADDR           SET TO USE MY I/O AREA                       
         GOTO1 AIO,IORD+IOCTFILE                                                
         BNE   VALOTYER            CAN'T READ RECORD                            
*                                                                               
         L     R1,IOADDR                                                        
         LA    R1,CTODATA          POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         USING CTOUTD,R1                                                        
VALOTY2  CLI   CTOUTEL,0           LOCATE OUTPUT TYPE ELEMENT                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTOUTEL,CTOUTELQ                                                 
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALOTY2                                                          
         TM    CTOUTSTA,X'80'      TEST VALID OUTPUT TYPE                       
         BZ    VALOTYER                                                         
         MVC   INOTYP,FVIFLD       SET OUTPUT TYPE IN W/S                       
         B     VALOTYPX                                                         
*                                                                               
VALOTY4  MVI   SCWORK,0            VALIDATE SQL TRANSFORM @ABCDEF               
         OC    VSWITCH,VSWITCH      CHECK SWITCH NOT RESOLVED OFFLINE           
         BZ    VALOTY4D                                                         
         GOTO1 VSWITCH,SCPARM,(X'FF',X'FFFFFFFF'),0                             
         L     R1,0(R1)                                                         
         MVC   SCDUB(1),TSYS-UTLD(R1)                                           
         CLI   SCDUB,X'0A'                                                      
         BE    VALOTY4A                                                         
         GOTO1 VSWITCH,SCPARM,(X'0A',X'FFFFFFFF'),0                             
         CLI   4(R1),0                                                          
         BE    VALOTY4A                                                         
         B     VALOTYES                                                         
*                                                                               
VALOTY4A LA    R4,IOKEY            SET KEY FOR SQL REFORM RECORD                
         USING GREFD,R4                                                         
         XC    GREFKEY,GREFKEY                                                  
         MVI   GREFKREC,GREFRECQ                                                
         MVC   GREFAGY,CUAALF                                                   
         MVC   GREFID,FVIFLD+1                                                  
         LA    RF,VOREC                                                         
         ST    RF,IOADDR           SET TO USE MY I/O AREA                       
*                                  DO DIRECT READ AS MAY BE CALLED              
*                                  FROM NON CONTROL SYSTEM - E.G MEDIA          
         MVC   0(L'IOKEY,RF),IOKEY                                              
         MVC   IOFILE,=CL7'GENDIR '                                             
         MVC   IOCMND,=CL7'DMREAD '                                             
         GOTO1 AIO,0                                                            
         BE    *+8                                                              
         MVI   SCWORK,INVSQL                                                    
*                                                                               
VALOTY4B CLI   SCDUB,X'0A'         TEST IF NEED TO SWITCH BACK                  
         BE    VALOTY4C                                                         
         GOTO1 VSWITCH,SCPARM,(SCDUB,X'FFFFFFFF'),0                             
VALOTY4C CLI   SCWORK,0                                                         
         BNE   VALOTYES                                                         
VALOTY4D MVC   INOTYP,FVIFLD       SAVE SQL FORMULA IN OUTPUT TYPE              
         B     VALOTYPX                                                         
*                                                                               
VALOTY5  CLI   FVILEN,3            REMOTE AFP INPUT AS /XX                      
         BE    VALOTY5X                                                         
         CLI   FVILEN,6            REMOTE AFP INPUT AS /SPPXX                   
         BE    VALOTY5X                                                         
         B     VALOTYER                                                         
VALOTY5X MVC   INOTYP,FVIFLD       SAVE SQL FORMULA IN OUTPUT TYPE              
         B     VALOTYPX                                                         
*                                                                               
VALOTYES MVC   FVMSGNO,=AL2(INVSQL)  SET INVALID SQL REFORM ID                  
         MVI   FVOSYS,X'FF'          SET GENERAL SYSTEM MESSAGE                 
         B     VALOTYPX                                                         
VALOTYER MVC   FVMSGNO,=AL2(FVFEOUT) SET INVALID OUTPUT TYPE                    
VALOTYPX CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         DROP  R1                                                               
         SPACE 1                                                                
         LTORG                                                                  
INVSQL   EQU   36                                                               
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD DYNAMIC SCREENS                                    *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) AS FOLLOWS -                            *         
*                                                                     *         
*        P1    BYTE  0   = NUMBER OF LINE REPETITIONS (ZERO=1)        *         
*                    1-3 = A(FIRST SCREEN BUILD ELEMENT)              *         
*                                                                     *         
*        P2    BYTES 1-4 = A(TWA ADDRESS WHERE SCREEN IS TO BE BUILT) *         
*                                                                     *         
* EXIT - P2=A(NEXT AVAILABLE TWA ADDRESS)                             *         
*        CC=EQUAL IF ACTION COMPLETED                                 *         
*        CC=NOT EQUAL IF ACTION IN ERROR WITH P1 BYTE 0 SET TO ERROR  *         
***********************************************************************         
         SPACE 1                                                                
BLDTWA   NMOD1 0,**BLDT**                                                       
         LR    R2,R1               R2=A(CALLING PARAMETER LIST)                 
         SR    R0,R0                                                            
         ICM   R0,1,0(R2)          R0=N'REPETITIONS                             
         BNZ   *+8                                                              
         LA    R0,1                                                             
         MVI   0(R2),0                                                          
         LA    R1,SCPARM                                                        
         USING TWAPARMD,R1         R1=A(BLDTWA PARAMETER LIST)                  
         XC    TWAPARMD(TWAPARML),TWAPARMD                                      
         MVC   TWAPATWA,ATWA                                                    
         MVC   TWAPAFST,0(R2)                                                   
         MVC   TWAPAMAX+2(2),=Y(SAVAREA-TWAD)                                   
BLDTWA2  MVC   TWAPAOUT,4(R2)      SET A(NEXT TWA FIELD)                        
         GOTO1 VTWABLD,TWAPARMD                                                 
         MVC   0(1,R2),TWAPERRS    SAVE ERROR BYTE                              
         CLI   TWAPERRS,0                                                       
         BNE   BLDTWAX                                                          
         MVC   4(4,R2),TWAPANXT    RETURN A(NEXT TWA FIELD)                     
         BCT   R0,BLDTWA2                                                       
BLDTWAX  CLI   0(R2),0             SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD KEY VALUES TO TABLE OF SAVED KEY COMPONENTS          *         
*                                                                     *         
* NTRY - APELEM CONTAINS A LIST OF ELEMENTS AS FOLLOWS -              *         
*        BYTE 0   - KEY COMPONENT NUMBER                              *         
*        BYTE 1   - LENGTH OF ELEMENT                                 *         
*        BYTE 2-N - KEY COMPONENT VALUES (MAY HAVE TRAILING SPACES)   *         
***********************************************************************         
         SPACE 1                                                                
PUTKEY   NMOD1 0,**PUTK**                                                       
         LA    R2,APELEM           R2=A(ELEMENT LIST)                           
         XC    SAVKEYS,SAVKEYS                                                  
PUTKEY2  CLI   0(R2),0             TEST END OF ELEMENT LIST                     
         BE    PUTKEYX                                                          
         ZIC   R1,1(R2)            R1=MAXIMUM LENGTH                            
         BCTR  R1,0                                                             
         LA    R1,0(R2,R1)                                                      
         CLI   0(R1),C' '          LOOK FOR LAST NON-BLANK CHARACTER            
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         SR    R1,R2                                                            
         CH    R1,=H'1'            TEST SPACE COMPONENT                         
         BE    PUTKEY6                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCWORK(0),0(R2)                                                  
         LA    R1,1(R1)                                                         
         STC   R1,SCWORK+1                                                      
         OC    SAVKEYL,SAVKEYL     TEST KEY COMPONENT TABLE EMPTY               
         BNZ   *+12                                                             
         MVI   SAVKEYL+1,3         YES INITIALISE                               
         B     PUTKEY4                                                          
         GOTO1 VHELLO,SCPARM,(C'D',SCTABNAM),(SCWORK,SAVKEYL),0                 
PUTKEY4  GOTO1 VHELLO,SCPARM,(C'P',SCTABNAM),SAVKEYL,SCWORK,0                   
PUTKEY6  ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     PUTKEY2                                                          
PUTKEYX  XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET KEY FOR NESTED LIST MONITOR CALLS                    *         
***********************************************************************         
         SPACE 1                                                                
GETKEY   NMOD1 0,**GETK**                                                       
         ICM   R3,15,AKEYHDR                                                    
         BZ    GETKEY2                                                          
         ZIC   RF,FVTLEN-FVIHDR(R3)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
GETKEY2  ICM   R3,15,AINKHDR                                                    
         BZ    GETKEYX                                                          
         SR    R4,R4                                                            
         ICM   R4,1,INKEYC                                                      
         BZ    GETKEYX                                                          
         LA    R2,INKEYT                                                        
GETKEY4  ZIC   RE,FVTLEN-FVIHDR(R3)                                             
         SH    RE,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    *+8                                                              
         SH    RE,=Y(L'FVIHDR)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         CLI   0(R2),0             IGNORE KEY COMPONENT IF ZERO                 
         BE    GETKEY6                                                          
         GOTO1 VHELLO,SCPARM,(C'G',SCTABNAM),(0(R2),SAVKEYL),0                  
         CLI   12(R1),0                                                         
         BNE   GETKEY6                                                          
         L     R1,12(R1)                                                        
         ZIC   RE,1(R1)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,R3),2(R1)                                             
         LA    RE,1(RE)                                                         
         STC   RE,FVILEN-FVIHDR(R3)                                             
GETKEY6  ZIC   RE,FVTLEN-FVIHDR(R3)                                             
         AR    R3,RE                                                            
         TM    FVATRB-FVIHDR(R3),FVAPROT                                        
         BNZ   GETKEY6                                                          
         LA    R2,1(R2)                                                         
         BCT   R4,GETKEY4                                                       
GETKEYX  XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXPAND RECORD NAMES                                      *         
*                                                                     *         
* NTRY - R1=A(RECORD TABLE ENTRY)                                     *         
* EXIT - SCRECNAM CONTAINS EXPANDED RECORD NAME                       *         
*        SCRECSHT CONTAINS EXPANDED RECORD SHORT NAME                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RECTABD,R1                                                       
EXPREC   MVC   SCRECNAM,RECNAME                                                 
         MVC   SCRECSHT,RECSHRT                                                 
         CLI   SCRECNAM,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
EXPRECN  STM   RE,R1,SCSVRER1                                                   
         GOTO1 VDICTAT,SCPARM,SCDICMLT,('SCRECLNQ',SCRECNAM),0                  
EXPRECX  LM    RE,R1,SCSVRER1                                                   
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO EXPAND ACTION NAME                                       *         
*                                                                     *         
* NTRY - R1=A(ACTION TABLE ENTRY)                                     *         
* EXIT - SCACTNAM CONTAINS EXPANDED ACTION NAME                       *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTTABD,R1                                                       
EXPACT   MVC   SCACTNAM,ACTNAME                                                 
         CLI   SCACTNAM,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
EXPACTN  STM   RE,R1,SCSVRER1                                                   
         GOTO1 VDICTAT,SCPARM,SCDICONE,SCACTNAM,0                               
EXPACTX  LM    RE,R1,SCSVRER1                                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXPAND KEY NAME                                          *         
*                                                                     *         
* NTRY - R1=A(KEY TABLE ENTRY)                                        *         
* EXIT - SCKEYNAM CONTAINS EXPANDED KEY NAME                          *         
***********************************************************************         
         SPACE 1                                                                
         USING KEYTABD,R1                                                       
EXPKEY   MVC   SCKEYNAM,KEYNAME                                                 
         CLI   SCKEYNAM,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
EXPKEYN  STM   RE,R1,SCSVRER1                                                   
         GOTO1 VDICTAT,SCPARM,SCDICONE,SCKEYNAM,0                               
EXPKEYX  LM    RE,R1,SCSVRER1                                                   
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO EXPAND OPTION NAMES                                      *         
*                                                                     *         
* NTRY - R1=A(OPTION TABLE ENTRY)                                     *         
* EXIT - SCOPTNAM CONTAINS EXPANDED OPTION NAME                       *         
*        SCOPTSHT CONTAINS EXPANDED OPTION SHORT NAME                 *         
***********************************************************************         
         SPACE 1                                                                
EXPOPT   MVC   SCOPTNAM,OPTNAME-OPTTABD(R1)                                     
         MVC   SCOPTSHT,OPTSHRT-OPTTABD(R1)                                     
         CLI   SCOPTNAM,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         SPACE 1                                                                
EXPOPTN  STM   RE,R1,SCSVRER1                                                   
         GOTO1 VDICTAT,SCPARM,SCDICMLT,('SCOPTLNQ',SCOPTNAM),0                  
EXPOPTX  LM    RE,R1,SCSVRER1                                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER/DROOL CALLING ROUTINE                                        *         
* ONLINE,  DROOL IS CALLED                                            *         
* OFFLINE, DRIVER IS CALLED                                           *         
* NOTE:- FOR DROOL, AGLOBAL MUST BE SET BY THE APPLICATION AND THE    *         
*        AREA MUST CONTAIN ENOUGH SPACE FOR DRGLOBAL + DROOLLOCAL.    *         
*        THE APPLICATION MUST ALSO SET THE FIELD GLADTAB AND GLSIZE   *         
*        IN DRGLOBAL. GLADTAB MUST POINT TO AN AREA LARGE ENOUGH FOR  *         
*        DROOL'S DRIVE TABLE AND ITS SIZE MUST BE PASSED IN GLSIZE.   *         
*        FOR DRIVER, GENERAL GETS THE STORAGE FOR DRIVER'S GLOBAL AREA*         
***********************************************************************         
         SPACE 1                                                                
DRIVER   NMOD1 0,**GEDR**                                                       
         CLI   ASONOFF,ASOFF       OFFLINE - GETMAIN FOR DRIVER STORAGE         
         BNE   DRV1                                                             
         LA    R4,SCWORK                                                        
         MVC   0(4,R4),=A(DRWORKL)                                              
         MVC   4(4,R4),=A(DRWORKL)                                              
         LA    R3,SCWORK+8                                                      
         GETMAIN VC,LA=(4),A=(3)                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R4,SCWORK+8                                                      
         ST    R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVC   GLSIZE,SCWORK+12                                                 
         B     DRV2                                                             
*                                                                               
DRV1     DS    0H                  ONLINE - ACQUIRE DROOL STORAGE               
         LH    R1,=Y(DROOLWKX-DROOLWKD)                                         
         LA    R1,7(R1)                                                         
         SRL   R1,3                                                             
         SLL   R1,3                                                             
         L     RE,4(RD)                                                         
         LR    R0,RD                                                            
         AR    RD,R1                                                            
         ST    RD,8(RE)                                                         
         ST    RE,4(RD)                                                         
         LA    RC,72(RE)                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR ACQUIRED STORAGE AREA                  
         USING DROOLWKD,RC                                                      
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
DRV2     MVC   0(8,R4),=C'*GLOBAL*'                                             
         MVC   GLCOMFAC,ACOM                                                    
         LA    R0,DRHOOK                                                        
         ST    R0,GLAHOOK                                                       
         GOTO1 DRAPHOOK,APMDRINI   INITIALIZE                                   
         BNE   DRVX                                                             
         CLI   ASONOFF,ASOFF                                                    
         BE    DRV4                                                             
         LA    R3,SCPARM                                                        
         USING DROOLD,R3           (SCPARM, SCFULL, SCDUB & SCWORK)             
         XC    DROOLBLK,DROOLBLK                                                
         ST    R4,DROGLOBL                                                      
         MVC   DRORPBLK,AREP                                                    
         LA    R0,TSARWRK                                                       
         STCM  R0,7,DROTSARD                                                    
         LA    R0,TSARBUF                                                       
         ST    R0,DROTSBUF                                                      
         TM    INDRLIND,MIXIDRLS   TEST DROOL ON-SCREEN REPORT                  
         BZ    *+10                                                             
         MVC   DROTWAH,APPARM      YES-PASS A(TWA FLDHDR OF RPT START)          
         LA    RF,SAVAREA                                                       
         MVC   DROSAVE,SAVDROOL-SAVAREA(RF)                                     
*                                                                               
DRV4     GOTO1 CALLDRV,GLINIT      INITIALISE                                   
*                                                                               
         CLI   ASONOFF,ASON        TEST CALLING DROOL                           
         BNE   DRV6                                                             
         CLI   DROERROR,DROERRAF   AND THERE'S A TSAR TEMPEST                   
         BNE   DRV6                ALLOCATION FAILURE                           
         MVC   FVMSGNO,=AL2(FVFTSAF)  YES-GET OUT WITH MESSAGE NOW              
         B     DRVX                                                             
*                                                                               
DRV6     GOTO1 DRAPHOOK,APMDRINP   INPUT                                        
         BNE   DRVX                                                             
         CLI   APMODE,APMDREND                                                  
         BE    DRV8                                                             
         GOTO1 CALLDRV,GLINPUT                                                  
         CLI   ASONOFF,ASON        TEST CALLING DROOL                           
         BNE   DRV6                                                             
         CLI   DROERROR,DROERREF   AND TEMPEST END-OF-FILE                      
         BNE   DRV6                                                             
         MVC   FVMSGNO,=AL2(FVFTSEF)  YES-GET OUT WITH MESSAGE NOW              
         B     DRVX                                                             
*                                                                               
DRV8     GOTO1 DRAPHOOK,APMDROUT   OUTPUT                                       
         BNE   DRVX                                                             
         GOTO1 CALLDRV,GLOUTPUT                                                 
         LA    RF,SAVAREA                                                       
         CLI   ASONOFF,ASON        TEST ONLINE                                  
         BNE   *+14                                                             
         MVC   SAVDROOL-SAVAREA(L'SAVDROOL,RF),DROSAVE                          
         B     DRVX                                                             
         L     R3,GLSIZE           OFFLINE-FREE UP DRIVER'S                     
         FREEMAIN R,LV=(3),A=(4)   GLOBAL STORAGE                               
*                                                                               
DRVX     CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1  ,                                                                
         DROP  R3,RC                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CALL DRIVER OR DROOL (NTRY - R1=GLMODE SETTING)          *         
***********************************************************************         
         SPACE 1                                                                
CALLDRV  LR    R0,RE                                                            
         STC   R1,GLMODE                                                        
         CLI   ASONOFF,ASOFF                                                    
         BNE   CALLDRV2                                                         
         GOTO1 VDRIVER,SCPARM,AGLOBAL                                           
         B     CALLDRVX                                                         
CALLDRV2 GOTO1 VDROOL,SCPARM                                                    
CALLDRVX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HOOK TO APPLICATION WITH DRIVER/DROOL HOOK                          *         
***********************************************************************         
         SPACE 1                                                                
DRHOOK   STM   RE,R1,SCSVRER1                                                   
         GOTO1 DRAPHOOK,APMDRHK                                                 
DRHOOKX  LM    RE,R1,SCSVRER1                                                   
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CALL APPLICATION OVERLAY                                 *         
***********************************************************************         
         SPACE 1                                                                
DRAPHOOK LR    R0,RE                                                            
         STC   R1,APMODE                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APACTN,INACT                                                     
         LA    R6,SAVAREA                                                       
         GOTO1 APNTRYA                                                          
         L     R6,SCBASE5                                                       
         CLI   APMODE,APMFMOK                                                   
         BE    *+10                                                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
DRAPKHX  LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
DRWORKL  EQU   40000               SIZE OF DRIVER OFFLINE GLOBAL WORK           
         EJECT                                                                  
***********************************************************************         
* DDLINK SUBROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
***********************************************************************         
* RETURN VALUES TO DDLINK. NOTE ANY MAY CONTAIN DICTATE STRINGS       *         
***********************************************************************         
         SPACE 1                                                                
DDLPUT   NMOD1 0,**DDLP**                                                       
         USING DDLFTD,R2                                                        
*                                                                               
         BRAS  RE,DDLSWAP          SWAP LIOB INTO TIA                           
*                                                                               
*        FLUSH REMAINING DDLINK DATA (MAY BE SOME IF ERROR)                     
*                                                                               
         L     R3,ATIA                                                          
         USING LIOBD,R3                                                         
         L     RF,ACOM                                                          
         L     RF,CLINKIO-COMFACSD(RF)                                          
DDLPUT02 TM    LIOBFLG2,LIOBFEOF+LIOBFEOR WAS LAST FIELD EOR OR EOF?            
         BNZ   DDLPUT10            YES, END OF DATA                             
         GOTOR (RF),SCPARM,('LIOAGET',(R3))                                     
         B     DDLPUT02                                                         
         DROP  R3                                                               
*                                                                               
*        RETURN THE ERROR/WARNING/INFO MESSAGE                                  
*                                                                               
DDLPUT10 LA    R3,64(,R5)          R3=MESSAGE FIELD HEADER                      
         XR    R4,R4                                                            
         IC    R4,0(R3)            R4=LENGTH OF MESSAGE FIELD                   
         AHI   R4,-8                                                            
         TM    1(R3),FVAXTND                                                    
         BZ    *+8                                                              
         AHI   R4,-8                                                            
*                                                                               
         GOTO1 VDICTAT,SCPARM,SCDICMLT,((R4),8(R3)),0                           
*                                                                               
         LA    R4,8(R4,R3)                                                      
         BCTR  R4,0                R4=END OF MESSAGE FIELD                      
         CLI   0(R4),C' '                                                       
         BNH   *-6                                                              
         SR    R4,R3               R4=DISP TO LAST NON-BLANK                    
         SHI   R4,8-1              +1 PAST NON-BLANK, -8 FOR L'HEADER           
*                                                                               
         L     R1,ATIA             MSG FIELD IS DATA FIELD 1                    
         USING LIOBD,R1                                                         
         MVC   LIOBMAP#,=X'FEF4'   I#UPLDTA                                     
         DROP  R1                                                               
*                                                                               
         L     RF,ACOM             WRITE MESSAGE FIELD                          
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),SCPARM,('LIOAPUT',ATIA),('LIOTRAW',1),             X        
               ('LD_CHARQ',8(R3)),((R4),0)                                      
*                                                                               
*        RETURN FVADDR OR ERROR FIELD ID                                        
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,FVDDLERR       IS DDLINK ERROR FIELD SET?                   
         BNZ   DDLPUT18            YES, RETURN IT                               
         ICM   RE,15,FVADDR        DO WE HAVE A CURSOR FIELD                    
         BZ    DDLPUT20            NO, CAN'T RETURN FIELD NUMBER                
         LT    R2,APDDLTAB         NEED DDLINK TABLE ADDRESS                    
         BZ    DDLPUT20            CAN'T RETURN FIELD NUMBER WITHOUT            
*                                                                               
         SR    RE,R5               GET DISPLACEMENT OF FIELD                    
DDLPUT12 OC    DDLFTNUM,DDLFTNUM   TEST END OF FIELD TABLE                      
         BZ    DDLPUT20            NO FIELD NUMBER                              
         TM    DDLFTIND,DDLFTIWQ   IS THIS A WORK FIELD                         
         BO    DDLPUT14            YES, IGNORE                                  
         CLM   RE,3,DDLFTDSP       IS THIS THE FVADDR FIELD                     
         BE    DDLPUT16            YES, SEND ITS ID                             
DDLPUT14 LA    R2,DDLFTDLQ(,R2)                                                 
         B     DDLPUT12            NEXT FIELD                                   
DDLPUT16 ICM   R0,3,DDLFTNUM       SET FIELD NUMBER                             
*                                                                               
DDLPUT18 CVD   R0,SCDUB                                                         
         OI    SCDUB+7,X'0F'                                                    
         UNPK  SCWORK(5),SCDUB     RETURN FIELD NUMBER                          
*                                                                               
         L     RF,ACOM             WRITE MESSAGE FIELD                          
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),SCPARM,('LIOAPUT',ATIA),('LIOTRAW',2),             X        
               ('LD_CHARQ',SCWORK),(5,0)                                        
*                                                                               
*        RETURN REMAINING FIELDS                                                
*                                                                               
DDLPUT20 LT    R2,APDDLTAB         MUST HAVE DDLINK TABLE ADDRESS               
         BZ    DDLPUT30            ELSE CAN'T RETURN ANY FIELDS                 
*                                                                               
DDLPUT22 OC    DDLFTNUM,DDLFTNUM   TEST END OF FIELDS                           
         BZ    DDLPUT30                                                         
*                                  LOCATE FIELD                                 
         LR    R3,R7               R3=WORK ADDRESS                              
         TM    DDLFTIND,DDLFTIWQ   IS THIS A WORK FIELD                         
         BO    *+8                 YES, SKIP, OTHERWISE TWA FIELD               
         LA    R3,8(,R5)           R3=A(TWA)+8 (ALLOWS FOR FIELD HDR)           
         AH    R3,DDLFTDSP         ADD FIELD DISP SO R3=A(FIELD)                
         LLC   R4,DDLFTMLN         R4=FIELD LENGTH                              
*                                  TRANSLATE ANY DD STRINGS                     
         GOTO1 VDICTAT,SCPARM,SCDICMLT,((R4),(R3)),0                            
*                                  WORK OUT ACTUAL LENGTH                       
         LA    RE,0(R4,R3)         RE=LAST BYTE + 1                             
DDLPUT24 BCTR  RE,0                LOOK FOR END                                 
         CLI   0(RE),C' '                                                       
         BH    DDLPUT26                                                         
         BCT   R4,DDLPUT24                                                      
         B     DDLPUT28            FIELD IS EMPTY, IGNORE                       
*                                                                               
DDLPUT26 GOTOR ,SCPARM,('LIOAPUT',ATIA),('LIOTRAW',0),                 X        
               ('LD_CHARQ',(R3)),((R4),0)                                       
         MVC   SCPARM+6(2),DDLFTNUM FIELD CODE                                  
         L     RF,ACOM                                                          
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),(R1)                                                        
*                                                                               
DDLPUT28 LA    R2,DDLFTDLQ(,R2)                                                 
         B     DDLPUT22            NEXT FIELD                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
DDLPUT30 BRAS  RE,DDLSWAP          SWAP LIOB OUT OF TIA                         
*                                                                               
         XIT1                                                                   
         SPACE 1                                                                
***********************************************************************         
* GET DATA FROM DDLINK                                                *         
* - ON ENTRY, FIRST FIELD HAS ALREADY HAVE BEEN FETCHED FROM DDLINKIO *         
***********************************************************************         
         SPACE 1                                                                
DDLKGET  MVI   SCCTYP,0            ZERO IF GETTING KEY DATA                     
         J     DDLGET                                                           
DDLDGET  MVI   SCCTYP,1            NON-ZERO FOR GETTING NON KEY DATA            
DDLGET   NMOD1 0,**DDLG**                                                       
         XC    FVDDLERR,FVDDLERR   CLEAR DDLINK ERROR FIELD                     
         OC    APDDLTAB,APDDLTAB   MUST HAVE DDLINK TABLE ADDRESS               
         JZ    *+2                                                              
         USING DDLFTD,R2                                                        
*                                                                               
         GOTO1 VGETFACT,SCPARM,0                                                
         L     R1,0(,R1)                                                        
         USING FACTSD,R1           R1=A(SYSTEM DEFINITION BLOCK)                
         MVC   SCFULL,FAXLATES     IN CASE WE NEED UPPER CASE                   
         DROP  R1                                                               
*                                                                               
*        WE ARE EMULATING THE USER ENTERING NEW DATA VIA THE INPUT              
*        TRANSLATOR SO WE MUST FIRST CLEAR FVITHIS IN ALL INPUT FIELDS.         
*        NO OTHER INPUT TRANSLATOR FUNCTIONS ARE PERFORMED, SUCH AS             
*        TESTING FIELD TYPE, BUT THAT'S ALL DONE BY FVAL.                       
*                                                                               
         L     R1,AINKHDR          FIRST KEY FIELD                              
         XR    RE,RE                                                            
DDLGET01 TM    1(R1),FVAPROT                                                    
         BNZ   *+8                                                              
         NI    4(R1),255-FVITHIS   CLEAR FVITHIS FOR ALL UNPROT                 
         IC    RE,0(,R1)                                                        
         AR    R1,RE                                                            
         CLI   0(R1),0             LOOP TO END OF TWA                           
         BNE   DDLGET01                                                         
*                                                                               
*        CLEAR ANY INPUT WORK FIELDS HANDLED BY DDLINK                          
*                                                                               
         L     R2,APDDLTAB         RESET DDLINK TABLE ADDRESS                   
*                                                                               
DDLGET02 OC    DDLFTNUM,DDLFTNUM   TEST END OF FIELDS                           
         BZ    DDLGET10                                                         
         TM    DDLFTIND,DDLFTIWQ   IS THIS A WORK FIELD                         
         BZ    DDLGET04                                                         
         TM    DDLFTIND,DDLFTIOQ   YES, IS IT OUTPUT ONLY                       
         BO    DDLGET04                                                         
         TM    DDLFTIND,DDLFTIKQ   NO, IS IT A KEY FIELD                        
         BZ    DDLGET03                                                         
         CLI   SCCTYP,0            YES, TEST GETTING KEY FIELDS                 
         BNE   DDLGET04            NO, SO DON'T CLEAR THIS                      
DDLGET03 LR    RF,R7               POINT TO WORKD                               
         AH    RF,DDLFTDSP         ADD DISPLACEMENT                             
         MVI   0(RF),C' '          CLEAR TO SPACES                              
         LLC   R1,DDLFTMLN         FIELD LENGTH                                 
         AHI   R1,-2                                                            
         BM    DDLGET04                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RF)                                                    
DDLGET04 LA    R2,DDLFTDLQ(,R2)                                                 
         B     DDLGET02            NEXT FIELD                                   
*                                                                               
DDLGET10 BRAS  RE,DDLSWAP          SWAP LIOB INTO TIA                           
*                                                                               
         XR    R3,R3               FIRST BAD FIELD                              
*                                                                               
DDLGET12 LLC   R4,SCDDLFLN         USE FLD AND REL TOGETHER                     
         MHI   R4,100                                                           
         LLC   R1,SCDDLREL                                                      
         AR    R4,R1                                                            
         BZ    DDLGET34            IGNORE ZERO ENTRIES                          
*                                                                               
         L     R2,APDDLTAB         POINT TO FIRST FIELD IN DDLINK TABLE         
*                                                                               
DDLGET14 XR    RE,RE               RE=0 FOR UNKNOWN FIELD                       
         OC    DDLFTNUM,DDLFTNUM   TEST END OF FIELDS                           
         BNZ   *+12                NO                                           
         LA    RE,=AL2(FVFNOTV)    'FIELD NOT VALID' WILL DO AS ERROR           
         B     DDLGET32            NOT A KNOWN FIELD                            
*                                                                               
         CLM   R4,3,DDLFTNUM       IS THIS THE RIGHT FIELD                      
         BNE   DDLGET18            NO, NEXT                                     
*                                                                               
         CLI   SCCTYP,0            LOOKING FOR KEY FIELDS?                      
         BNE   DDLGET15                                                         
         TM    DDLFTIND,DDLFTIKQ   YES,                                         
         BZ    DDLGET40            EXIT ON FIRST NON-KEY FIELD                  
         B     DDLGET16            ELSE GO PROCESS SCREEN KEY FIELD             
DDLGET15 TM    DDLFTIND,DDLFTIKQ   NO, LOOKING FOR DATA FIELDS                  
         JNZ   *+2                 SHOULD HAVE HAD ALL KEY FIELDS               
*                                                                               
DDLGET16 TM    DDLFTIND,DDLFTIWQ   IS THIS A WORK FIELD                         
         BO    DDLGET28            YES, GO PROCESS IT                           
*                                                                               
         LR    RF,R5               POINT TO TWA FOR SCREEN FIELD                
         AH    RF,DDLFTDSP         ADD DISPLACEMENT                             
         TM    4(RF),FVITHIS       HAS IT HAD DATA ALREADY                      
         BZ    DDLGET20            NO, USE IT, ELSE MAY BE ANOTHER              
*                                                                               
DDLGET18 LA    R2,DDLFTDLQ(,R2)    NEXT FIELD                                   
         B     DDLGET14                                                         
*                                                                               
DDLGET20 ST    RF,FVADDR                                                        
         OI    4(RF),FVITHIS       INDICATE DATA RECEIVED FOR FIELD             
         MVI   FVIFLD,C' '         CLEAR TO SPACES                              
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         SR    RE,RE                                                            
         ICM   R1,15,SCDDLDTA                                                   
         BZ    DDLGET26            SKIP IF NO DATA                              
         ICM   RE,3,1(R1)          GET DATA EXECUTE LENGTH                      
         AHI   RE,-(6+1)                                                        
         CHI   RE,256                                                           
         BNL   DDLGET30            INPUT IS TOO LONG FOR ANY FIELD              
         CLM   RE,1,DDLFTMLN                                                    
         BNL   DDLGET30            INPUT IS TOO LONG FOR FIELD                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),6(R1)     MOVE DATA TO FVIFLD                          
         TM    1(RF),FVALOWC       TEST FIELD IS LOWER CASE                     
         BO    DDLGET24            YES, SKIP                                    
         XR    R1,R1               NO, TRANSLATE TO UPPER CASE                  
         IC    R1,CUCTRY           USING U/C XLT TABLE FOR COUNTRY              
         SLL   R1,4                                                             
         A     R1,SCFULL                                                        
         L     R1,8(,R1)                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    FVIFLD(0),0(R1)                                                  
DDLGET24 LA    RE,1(RE)                                                         
DDLGET26 STC   RE,5(RF)            SET INPUT LENGTH                             
         IC    R1,DDLFTMLN         GET MAXIMUM LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),FVIFLD      COPY NEW DATA                                
         B     DDLGET34                                                         
*                                                                               
DDLGET28 XC    FVADDR,FVADDR                                                    
         LR    RF,R7               GET WORK ADDRESS                             
         AH    RF,DDLFTDSP         ADD DISPLACEMENT                             
         MVI   0(RF),C' '          CLEAR FIELD                                  
         LLC   RE,DDLFTMLN         FIELD LENGTH                                 
         AHI   RE,-2               MINUS 1 FOR SPACE AND 1 FOR EX LEN           
         BM    *+18                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RF)                                                    
         ICM   R1,15,SCDDLDTA      GET INPUT DATA ADDRESS                       
         BZ    DDLGET34            SKIP IF EMPTY                                
         SR    RE,RE                                                            
         ICM   RE,3,1(R1)          GET DATA EXECUTE LENGTH                      
         AHI   RE,-(6+1)                                                        
         CHI   RE,256                                                           
         BNL   DDLGET30            INPUT IS TOO LONG FOR ANY FIELD              
         CLM   RE,1,DDLFTMLN                                                    
         BH    DDLGET30            INPUT IS TOO LONG FOR FIELD                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),6(R1)       MOVE INPUT DATA                              
         B     DDLGET34            AND GET NEXT FIELD                           
*                                                                               
DDLGET30 LA    RE,=AL2(FVFLONG)    TOO MUCH DATA ERROR                          
DDLGET32 LTR   R3,R3               FIELD ERROR                                  
         BNZ   DDLGET34            IGNORE IF NOT FIRST                          
         MVC   FVMSGNO,0(RE)       STORE ERROR MESSAGE                          
         STCM  R4,3,FVDDLERR       SET FIELD NUMBER IN ERROR                    
         LR    R3,R4               SAVE FIELD ID IN R3 SO NONZERO               
*                                                                               
DDLGET34 L     R1,ATIA                                                          
         USING LIOBD,R1                                                         
         TM    LIOBFLG2,LIOBFEOF+LIOBFEOR WAS LAST FIELD EOR OR EOF?            
         BNZ   DDLGET40            YES, END OF DATA                             
         DROP  R1                                                               
         L     RF,ACOM             GET NEXT FIELD FROM LINKIO                   
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),SCPARM,('LIOAGET',ATIA)                                     
         B     DDLGET12                                                         
*                                                                               
DDLGET40 BRAS  RE,DDLSWAP          SWAP LIOB OUT OF TIA                         
*                                                                               
         LTR   R3,R3               CC ZERO/EQUAL IF NO ERRORS                   
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* SWAP LIOB BETWEEN TIA AND WSSVR                                     *         
***********************************************************************         
         SPACE 1                                                                
DDLSWAP  NTRL  WORK=(R2,LIOBTLEN)  GRAB TEMP STORAGE                            
*                                                                               
         LR    R0,R2               AND COPY CURRENT TIA CONTENT TO IT           
         LHI   R1,LIOBTLEN                                                      
         L     RE,ATIA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
W        USING FAWSSVRD,SCPARM     RESTORE TIA FROM WSSVR                       
         LARL  RE,LIOBTOKN                                                      
         MVC   W.FAWSTOKN,0(RE)                                                 
         MVC   W.FAWSADR,ATIA                                                   
         MVC   W.FAWSLEN,=AL2(LIOBTLEN)                                         
         MVI   W.FAWSACTN,FAWSARST                                              
         L     RF,ACOM                                                          
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),W.FAWSSVRD                                                  
         CLI   W.FAWSRTN,0                                                      
         JNE   *+2                 DIE IF CAN'T RESTORE                         
*                                                                               
         MVI   W.FAWSACTN,FAWSADEL DELETE THE RESTORED BUFFER                   
         GOTOR (RF),(R1)                                                        
*                                                                               
         ST    R2,W.FAWSADR        SAVE WHAT WAS CURRENT TIA CONTENT            
         MVI   W.FAWSACTN,FAWSASVE                                              
         GOTOR (RF),W.FAWSSVRD                                                  
         CLI   W.FAWSRTN,0                                                      
         JNE   *+2                 DIE IF CAN'T RESTORE                         
*                                                                               
         XIT1                                                                   
         DROP  W                                                                
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDTWABLDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSPOOK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DMREQHDR                                                                      
         PRINT OFF                                                              
REQHDR   DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80               REQUEST CARD LAYOUT                          
REQJCLID DS    CL2                 JCL ID                                       
REQAGYID DS    CL2                 AGENCY ID                                    
         DS    CL1                 N/D                                          
REQSIN   DS    CL6                 SYSTEM INPUT NUMBER                          
         ORG   REQUEST+L'REQUEST                                                
         PRINT ON                                                               
         SPACE 1                                                                
* GEDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DRGLOBAL                                                                      
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDDROOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDROOLD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
DROOLWKD DSECT                                                                  
TSARWRK  DS    (TSARDL)X                                                        
         DS    0D                                                               
TSARBUF  DS    6144X                                                            
DROOLWKX EQU   *                                                                
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         EJECT                                                                  
* GEGENWRK                                                                      
       ++INCLUDE GEGENWRK                                                       
         EJECT                                                                  
* GEGENREF                                                                      
       ++INCLUDE GEGENREF                                                       
         EJECT                                                                  
IOWORKD  DSECT                     ** IO S/R LOCAL W/S **                       
IODUB    DS    D                   GENERAL WORK AREA                            
IOAREAD  DS    A                   I/O AREA ADDRESS                             
IOCTRL   DS    F                   I/O COMMAND WORD                             
IOBYTE   DS    X                   I/O BYTE                                     
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IOFILV   DS    0XL15               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILGLB EQU   14                  GLOBAL FILES ARE 14-15                       
IOFILNM  DS    CL7                 FILE NAME                                    
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOSWSYS  DS    XL1                 SWITCH SYSTEM NUMBER                         
IOSWFIL  DS    XL1                 SWITCH FILE NUMBER                           
IOSWSYSN DS    CL3                 SWITCH SYSTEM NAME                           
IOWORKX  EQU   *                                                                
         SPACE 1                                                                
VOTYPD   DSECT                     ** VALOTYP S/R LOCAL W/S **                  
VOREC    DS    XL1000              OUTPUT TYPE RECORD I/O AREA                  
VOTYPX   EQU   *                                                                
         SPACE 1                                                                
VDSTD    DSECT                     ** VALDEST S/R LOCAL W/S **                  
VDSAVER1 DS    A                   SAVED R1 VALUE                               
VDPARM   DS    6F                  GETIDS PARAMETER LIST                        
VDREC    DS    XL1000              OUTPUT TYPE RECORD I/O AREA                  
VDSTX    EQU   *                                                                
         SPACE 1                                                                
VOPTD    DSECT                     ** VALOPTS S/R LOCAL W/S **                  
VOATAB   DS    A                   A(OPTIONS VALIDATION TABLE)                  
VOAREA   DS    20XL(22+30)         SCANNER TABLE                                
VOPTX    EQU   *                                                                
         SPACE 1                                                                
HELPTWAD DSECT                     ** HELP SCREEN LAYOUT **                     
HELPH1H  DS    XL8                                                              
HELPH1   DS    CL79                HELP HEAD LINE 1                             
HELPH2H  DS    XL8                                                              
HELPH2   DS    CL79                HELP HEAD LINE 2                             
HELPL1H  DS    XL8                                                              
HELPL1   DS    CL79                HELP DATA LINE 1                             
         EJECT                                                                  
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
* DDGLPFMD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGLPFMD                                                       
         PRINT ON                                                               
* FAXTRAINF                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
* DDLINKD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLINKD                                                        
         PRINT ON                                                               
* DDLINKIOD                                                                     
         PRINT OFF                                                              
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008GEGEN00   10/08/20'                                      
         END                                                                    
