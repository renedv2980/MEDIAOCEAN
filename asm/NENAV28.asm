*          DATA SET NENAV28    AT LEVEL 003 AS OF 03/19/18                      
*PHASE T31828A                                                                  
NENAV28  TITLE '- Net Traffic Commercial Upload'                                
         PRINT NOGEN                                                            
                                                                                
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* SMUR  10SEP15 001 INITIAL DEVELOPMENT - NET COMMERCIAL UPLOAD                 
* SMUR  15MAR16 002 CHECK PROFILE FOR SINGLE PRODUCT ISCII                      
* SMUR  05APR16 003 FIX COMMERCIAL LEN VALIDATION ROUTINE                       
*=====================================================================*         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=500,REQUEST=*,WORKERKEY=NCMU,    +        
               LINKIO=Y,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),          +        
               SYSTEM=NETSYSQ                                                   
                                                                                
B#CMLREC EQU   3                   I/O 3 used for commercial record             
#CMLREC  EQU   IO3                 COMMERCIAL RECORD I/O AREA                   
ACMLREC  EQU   AIO3                A(COMMERCIAL RECORD)                         
ACMLREQ  EQU   IO3LQ               BUFFER LENGTH                                
                                                                                
                                                                                
ENTRY    NMOD1 0,**NN28**,RR=RE,CLEAR=Y                                         
         LR    R6,R1               R1=A(LP_D)                                   
         USING LP_D,R6                                                          
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     RF,LP_ARUNP                                                      
         MVC   BYTE1,RUNPMODE-RUNPARMD(RF)                                      
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
*                                                                               
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         L     RA,AGLOBALS                                                      
         A     RA,SRVRRELO                                                      
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         CLI   BYTE1,RRUNSTRQ      TEST 'FIRST' MODE                            
         JE    FIRST                                                            
         CLI   BYTE1,RINIREQQ      TEST 'INITIALIZE' MODE                       
         JE    INIT                                                             
         CLI   BYTE1,RRUNREQQ      TEST 'RUN REQUEST' MODE                      
         JE    INPUT                                                            
         CLI   BYTE1,RRUNENDQ      TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
*                                                                               
         J     EXITY                                                            
*                                                                               
AGLOBALS DC    A(GLOBALS)                                                       
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                                     
***********************************************************************         
                                                                                
FIRST    DS    0H                                                               
         LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(3,BTODAY)                                    
*                                                                               
         MVC   LP_BLKS+((B#CMLREC-1)*L'LP_BLKS)(AIOLAST-AIO3),AIO3              
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* INIT - NOTE FIRST REC PROCESSED *BEFORE* INIT CALL!                           
***********************************************************************         
*                                                                               
INIT     DS    0H                                                               
         MVI   DATADISP+1,24                                                    
*                                                                               
         MVC   WVALUES(WVALUESL),LVALUES                                        
         LHI   R0,ADCONSN                                                       
         CHI   R0,0                                                             
         JE    INIT10                                                           
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
*                                                                               
INIT10   MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         LA    RF,ALIOB                                                         
         USING LIOBD,RF                                                         
         OI    LIOBINDS,LIOBINRM   INHIBIT AUTO RETURN MAP ELEMENT              
*                                                                               
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         DROP  RF                                                               
*                                                                               
         MVC   ALINKIO,CLINKIO-COMFACSD(RF)                                     
*                                  EXTRACT A(RECUP) FROM COMFACS                
         MVC   ARECUP,CRECUP-COMFACSD(RF)                                       
*                                  EXTRACT A(DATVAL) FROM COMFACS               
         MVC   VDATVAL,CDATVAL-COMFACSD(RF)                                     
*                                  EXTRACT A(DATVAL) FROM COMFACS               
         MVC   VADDAY,CADDAY-COMFACSD(RF)                                       
*                                  EXTRACT A(DATVAL) FROM COMFACS               
         MVC   VSCANNER,CSCANNER-COMFACSD(RF)                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A0E'    GET ADDRESS OF TIMVAL                  
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB                                                        
         MVC   VTIMVAL,0(R1)                                                    
*                                                                               
         J     EXITY                                                            
*                                                                               
*                                                                               
***********************************************************************         
* PROCESS AN UPLOAD RECORD                                                      
***********************************************************************         
*                                                                               
INPUT    LA    RE,RECTAB                                                        
         USING RECTABD,RE                                                       
         LHI   R0,RECTABN                                                       
*                                                                               
INPUT10  CLC   RECTMAP#,LP_QMAPN   LOOK UP RECORD MAP CODE IN TABLE             
         JE    INPUT20                                                          
         AHI   RE,RECTABL                                                       
         JCT   R0,INPUT10                                                       
         DC    H'0'                                                             
*                                                                               
INPUT20  SR    R0,R0                                                            
         ICM   R0,7,RECTPROG                                                    
         A     R0,SRVRRELO                                                      
         ST    R0,RECADDR                                                       
*                                                                               
         GOTOR RECADDR                                                          
         J     EXITY               EXIT BACK TO DDLINK                          
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* Commercial upload                                                             
***********************************************************************         
CMLUPLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   BYTE1,0             INIT GENERATE PAT T/A                        
*                                                                               
         ICM   RE,15,I$CAM                                                      
         JZ    ERR00258            MISSING MEDIA                                
*                                                                               
         USING LW_D,RE                                                          
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
         GOTOR (#VALMED,AVALMED),DMCB,LW_DATA1,(R3),SVBAGM                      
         JNE   ERR01023            INVALID MEDIA                                
*                                                                               
         ICM   RE,15,I$CCLT                                                     
         JZ    ERR00966            MISSING CLIENT                               
*                                                                               
         MVC   QCLTA,LW_DATA1                                                   
         SR    R3,R3                                                            
         ICM   R3,3,LW_LN                                                       
         AHI   R3,-(LW_LN1Q)                                                    
*                                                                               
         CHI   R3,2                                                             
         JL    ERR00014            INVALID CLIENT                               
         JH    *+8                                                              
         MVI   QCLTA+2,C' '                                                     
*                                                                               
         GOTOR (#VALCLT,AVALCLT),DMCB,LW_DATA1,(R3),SVBCLT                      
         JNE   ERR00014            INVALID CLIENT                               
         DROP  RE                                                               
*                                                                               
         BRAS  RE,GPROF                                                         
*                                                                               
         LA    R2,QCOMML                                                        
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCML                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
CMLU005  LA    R4,IOKEY                                                         
         USING CMLRECD,R4                                                       
         XC    CMLKEY,CMLKEY                                                    
         MVI   CMLKID,X'0A'        X'0A21'                                      
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,SVBAGM                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,WORK                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#CMLREC'                        
         JE    CMLU010                                                          
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   CMLKID+1,X'C1'      Try passive pointer                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#CMLREC'                        
         JNE   CMLU015                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
CMLU010  GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+#CMLREC'                    
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,ACMLREC          PREPARE IOAREA FOR THE ADD                   
*                                                                               
         CLI   RECACT,C'X'         DELETE                                       
         JE    CMLU012                                                          
         CLI   RECACT,C'R'         RESTORE                                      
         JE    CMLU012                                                          
         CLI   RECACT,C'C'                                                      
         JNE   ERR00049            RECORD ALREADY EXISTS                        
*                                                                               
CMLU012  L     RE,AIO8                                                          
         LHI   RF,IO8LQ                                                         
         XCEFL                                                                  
*                                                                               
         L     RE,ACMLREC          SAVE ORIGINAL RECORD IN IO8                  
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,AIO8                                                          
         LA    R1,2(RF)                                                         
         MVCL  R0,RE                                                            
*                                                                               
         OC    QCKSM,QCKSM         ANY CHECKSUM PASSED                          
         JNZ   *+16                 YES, CHECK                                  
         CLI   RECACT,C'C'                                                      
         JE    ERR00814            NEED CHECK SUM FOR ACTION CHANGE             
         J     CMLU018                                                          
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCKSUM           VALIDATE CHECK SUM                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         J     CMLU018                                                          
*                                                                               
CMLU015  CLI   RECACT,C'A'                                                      
         JNE   ERR00971            RECORD NOT FOUND                             
*                                                                               
         L     RE,ACMLREC          PREPARE IOAREA FOR THE ADD                   
         LHI   RF,ACMLREQ                                                       
         XCEFL                                                                  
*                                                                               
         L     R4,ACMLREC                                                       
         USING CMLRECD,R4                                                       
         MVI   CMLKID,X'0A'        X'0A21'                                      
         MVI   CMLKID+1,X'21'                                                   
         MVC   CMLKAM,SVBAGM                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,WORK        PACKED CML                                   
         MVI   CMLRLEN+1,24        RECORD LENGTH                                
         MVC   CMLAGYA,LP_AGY      SET AGENCY ALPHA ID                          
         DROP  R4                                                               
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),0(R4)                                                  
*                                                                               
CMLU018  MVC   HOLDCML,WORK                                                     
         MVC   SVKEY,IOKEY                                                      
*                                                                               
         CLI   RECACT,C'X'         IF ACTION DELETE                             
         JE    CMLU025                                                          
         CLI   RECACT,C'R'         OR RESTORE                                   
         JE    CMLU025             PROCESS X'10' ELEM                           
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VPRDL            VALIDATE PROD LIST & BUILD ELEM              
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*===========================================================                    
* COMMERCIAL ACTUAL/COVERED                                                     
*===========================================================                    
*                                                                               
CMLU025  L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    CMLU030                                                          
*                                                                               
         CLI   RECACT,C'A'                                                      
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'10'                                         1             
         MVI   ELEM+1,CMLDTAX-CMLDTAEL                                          
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         J     CMLU025             GO FIND ELEM JUST INSERTED                   
*                                                                               
         USING CMLDTAEL,R4                                                      
CMLU030  CLC   HOLDCML,=8C'9'       CML ID ALL 9'S (PROD HSE KEY)               
         JE    ERR00002             INVALID ENTRY                               
*                                                                               
         OC    CMLCOVCT,CMLCOVCT   IS THIS A COVD (ACT) CML?                    
         JZ    *+8                                                              
         OI    FLAGS,ISCOVERD                                                   
*                                                                               
         CLI   RECACT,C'X'         SOFT DELETE THIS CML                         
         JNE   CMLU040               NO                                         
*                                                                               
CMLU035  TM    FLAGS,ISCOVERD      IS CMML COVERED?                             
         JO    ERR00661             NO                                          
*                                                                               
         OI    CMLSTAT,X'80'       SOFT DELETE RECORD                           
         OI    FLAGS,SOFTDEL                                                    
         J     CMLU202                                                          
*                                                                               
CMLU040  CLI   RECACT,C'R'         RESTORE SOFT DELETE?                         
         JNE   CMLU045               NO                                         
         TM    CMLSTAT,X'80'       WAS CML SOFT DELETED                         
         JZ    ERR00581            CAN'T RESTORE                                
         NI    CMLSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         OI    FLAGS,SOFTREST                                                   
         J     CMLU202                                                          
*                                                                               
*===========================================================                    
* COMMERCIAL TITLES                                                             
*===========================================================                    
*                                                                               
CMLU045  MVI   ERRFLG,0                                                         
*                                                                               
         OC    QCMLTTLE,QCMLTTLE   ANY CML TITLE?                               
         JZ    ERR00001                                                         
*                                                                               
         BRAS  RE,VALTTL                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
CMLU050  L     R4,ACMLREC          MOVE X'10' ELEM OUT OF RECORD                
         MVI   ELCODE,X'10'        IT NOW CONTAINS NEW TITLE1                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CMLTITLE,QCMLTTLE   IT NOW CONTAINS NEW TITLE1                   
*                                                                               
         BAS   RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         LA    R4,ELEM                                                          
*                                                                               
*==================================================================             
* VALIDATE COMMERCIAL LEN                                                       
*==================================================================             
*                                                                               
CMLU055  CLI   QCMLSLN,0           ANY ENTRY                                    
         JE    ERR00243            INVALID                                      
*                                                                               
         MVC   SVSLN,QCMLSLN                                                    
         MVI   ERRFLG,0                                                         
         BRAS  RE,VSLN                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VOVRD            GO GET PRINTABLE OVERRIDE SPOT LENS          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*==================================================================             
* VALIDATE START (RELEASE) AND END (RECALL) DATES                               
*==================================================================             
*                                                                               
         CLC   CMLRLSE,QCMLRLSE                                                 
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_SDAT                                              
         MVC   CMLRLSE,QCMLRLSE                                                 
*                                                                               
         CLC   =C'99/99/99',QCMLRCL  ]FN                                        
         JNE   CMLU060             NO, PROCESS IT                               
         CLI   SVTN1PRC,C'Y'       UFN IS INVALID                               
         JE    ERR00632            YES, ERROR                                   
         MVC   WORK(3),=X'FFFFFF'  FORCE IT                                     
         J     CMLU065                                                          
*                                                                               
CMLU060  GOTOR VDATVAL,DMCB,(0,QCMLRCL),DATE                                    
         OC    DMCB(4),DMCB                                                     
         JZ    ERR00020                                                         
         GOTOR VDATCON,(R1),(0,DATE),(3,WORK)                                   
*                                                                               
CMLU065  CLC   CMLRCL,WORK                                                      
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_EDAT                                              
         MVC   CMLRCL,WORK                                                      
*                                                                               
         CLC   CMLRLSE,CMLRCL      CAN'T RECALL BEFORE RELEASE                  
         JH    ERR00020                                                         
*                                                                               
         MVC   WORK(3),CMLRLSE                                                  
         MVC   WORK+3(3),CMLRCL                                                 
*                                                                               
         XC    CMLLMTCD(11),CMLLMTCD CLEAR RUN LIMIT CODE                       
*                                                                               
*============================================================                   
* CLIENT COMMERCIAL NUMBER                                                      
*============================================================                   
*                                                                               
         CLC   QCMLCLTN,SPACES                                                  
         JNE   *+10                                                             
         XC    QCMLCLTN,QCMLCLTN                                                
*                                                                               
         CLC   CMLCLTNO,QCMLCLTN                                                
         JE    CMLU070                                                          
         OI    CHGFLAG2,CMLCH_CLNO                                              
         MVC   CMLCLTNO,QCMLCLTN                                                
         OC    CMLCLTNO,SPACES                                                  
*                                                                               
*==================================================================             
* VALIDATE P/B-SOLO OPTION                                                      
*==================================================================             
*                                                                               
CMLU070  CLI   QCMLSOLO,0                                                       
         JE    CMLU072                                                          
         CLI   QCMLSOLO,C'S'                                                    
         JE    CMLU072                                                          
         CLI   QCMLSOLO,C'P'                                                    
         JNE   ERR00580                                                         
*                                                                               
CMLU072  CLC   CMLSOLO,QCMLSOLO                                                 
         JE    *+8                                                              
         OI    CHGFLAG3,CMLCH_OTHR                                              
         MVC   CMLSOLO,QCMLSOLO                                                 
*                                                                               
         NI    CMLSTAT,X'FF'-X'40' INIT COMMENT EXISTS FLAG                     
*                                                                               
*==================================================================             
* TALENT TRANSFER EXCLUDE OPTION                                                
*==================================================================             
*                                                                               
         CLI   QCMLTALX,0                                                       
         JE    CMLU075                                                          
         CLI   QCMLTALX,C'Y'       YES, EXCLUDE FROM TALENT TRANSFER            
         JE    CMLU075                                                          
         CLI   QCMLTALX,C'N'                                                    
         JNE   ERR00742                                                         
*                                                                               
CMLU075  CLC   CMLTALEX,QCMLTALX                                                
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_TAL                                               
         MVC   CMLTALEX,QCMLTALX                                                
*                                                                               
CMLU080  XC    CMLCLASS,CMLCLASS  CLEAR FOR NOW                                 
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   *+12                                                             
         OI    CMLOPFLG,CMLOPADD   CML ADDED IN OPTICA                          
         J     CMLU085                                                          
*                                                                               
         OI    CMLOPFLG,CMLOPCHG   CML CHANGED IN OPTICA                        
*                                                                               
CMLU085  MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL            INSERT X'10' ELEMENT NOW!                    
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   CMLU090                                                          
         MVI   ERRFLG,0                                                         
         BRAS  RE,DUPCHK           MAKE SURE THIS CML DOES NOT EXIST            
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         CLI   ADIDFLAG,C'Y'       TEST CMML IS AN ADID                         
         JNE   *+8                 NO                                           
         BRAS  RE,AADIDEL          ELSE ADD ADID ELEM FOR CMML                  
*                                                                               
*==================================================================             
* VALIDATE MATCH DATES, TIMES                                                   
*==================================================================             
*                                                                               
CMLU090  MVI   ERRFLG,0                                                         
         BRAS  RE,VALMAT                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
*=================================================================              
* VALIDATE HIDEF COMMERCIAL                                                     
*=================================================================              
*                                                                               
VRHD     DS    0H                                                               
         MVI   BYTE,0              INIT INPUT LEN                               
         XC    SVHIDEF,SVHIDEF                                                  
         XC    SVHIDEFX,SVHIDEFX                                                
         XC    SVHDADDK,SVHDADDK   CLEAR HIDEF KEY TO ADD                       
         XC    SVHDDELK,SVHDDELK   HIDEF KEY TO DELETE                          
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   VRHD003                                                          
*                                                                               
         CLI   QFORMAT,0           IS THERE A HIDEF FLAG                        
         JE    VRPC                 NO, DONE                                    
*                                                                               
VRHD003  XC    WORK,WORK                                                        
         LA    RF,QCOMML                                                        
         LA    RE,L'QCOMML-1                                                    
         BRAS  R1,FLEN             FIND INPUT LEN-1                             
*                                                                               
         STC   RE,BYTE             SAVE IT                                      
*                                                                               
         CLI   QFORMAT,0           IS THERE A HIDEF FLAG                        
         JE    VRHD005                                                          
*                                                                               
         CLI   QFORMAT,C'H'        H IS THE ONLY VALID ENTRY                    
         JNE   ERR00815                                                         
*                                                                               
         CLI   BYTE,11             CML LEN MUST BE LESS THAN 12                 
         JNL   ERR00765                                                         
*                                                                               
         BASR  R1,0                                                             
         MVC   WORK(0),QCOMML      MOVE CML TO WORK                             
         EX    RE,0(R1)                                                         
         OC    WORK(12),SPACES                                                  
*                                                                               
         LA    RF,WORK                                                          
         AHI   RE,1                ACTUAL INPUT LEN                             
         STC   RE,BYTE                                                          
         AR    RF,RE               POINT TO END OF CML                          
         MVI   0(RF),C'H'          H TO THE END OF CML FOR HIDEF                
*                                                                               
         MVC   SVHIDEF,WORK                                                     
         OC    SVHIDEF,SPACES                                                   
*                                                                               
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JE    VRHD010                                                          
*                                                                               
VRHD005  BRAS  RE,GETHIDEF         LOOK UP HIDEF IN EXISTING REC                
         JE    VRPC                PASSIVE ALREADY MAINTAINED                   
*                                                                               
         CLI   QFORMAT,0                                                        
         JE    VRHD050             DELETE OLD HIDEF                             
*                                                                               
VRHD010  XC    NWHIDEFX,NWHIDEFX   INITIALIZE NEW HIDEF CODE                    
         LA    R2,WORK                                                          
         CLI   BYTE,0              INPUT LEN?                                   
         JE    VRHD050                                                          
         CLI   BYTE,8                                                           
         JL    ERR00765                                                         
*                                                                               
         LLC   R0,BYTE             MAKE SURE NO SPCL CHARS                      
         LA    R1,WORK                                                          
*                                                                               
VRHD020  CLI   0(R1),C'A'                                                       
         JL    ERR00734                                                         
         CLI   0(R1),C'Z'                                                       
         JNH   VRHD030                                                          
         CLI   0(R1),C'0'                                                       
         JL    ERR00734                                                         
         CLI   0(R1),C'9'                                                       
         JH    ERR00734                                                         
VRHD030  LA    R1,1(R1)                                                         
         JCT   R0,VRHD020                                                       
*                                                                               
         GOTOR VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         JNE   ERR00734                                                         
*                                                                               
         MVC   NWHIDEFX,WORK+12                                                 
*                                                                               
         BRAS  RE,HDDUPS                                                        
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    ERR00756                                                         
*                                                                               
VRHD050  DS    0H                                                               
         OC    SVHIDEFX,SVHIDEFX                                                
         JZ    VRHD070                                                          
         MVC   SVHDDELK(2),=X'0AC2' SET HIDEF PASSIVE KEY FOR DELETED           
         MVC   SVHDDELK+2(3),SVBAGM                                             
         MVC   SVHDDELK+5(8),SVHIDEFX                                           
*                                                                               
VRHD070  DS    0H                                                               
         CLI   QFORMAT,0                                                        
         JE    VRPC                NO HIDEF                                     
*                                                                               
         MVC   SVHDADDK(2),=X'0AC2'  SET HIDEF PASSIVE KEY FOR ADD              
         MVC   SVHDADDK+2(3),SVBAGM                                             
         MVC   SVHDADDK+5(8),NWHIDEFX                                           
*                                                                               
*=================================================================              
* VALIDATE PARENT COMMERCIAL                                                    
*=================================================================              
*                                                                               
VRPC     DS    0H                                                               
         OC    QCMLPRNT,QCMLPRNT   ANY PARENT CML                               
         JZ    VRPCX                                                            
         MVI   ERRFLG,0                                                         
         BRAS  RE,VALCMML                                                       
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
VRPCX    DS    0H                                                               
*                                                                               
*                                                                               
*======================================================================         
* VALIDATE CLASS                                                                
*======================================================================         
*                                                                               
         OC    QCMLCLAS,QCMLCLAS                                                
         JNZ   VCLS10                                                           
         CLI   SVTN1PR8,C'Y'       IS COMMERCIAL CLASS REQUIRED                 
         JE    ERR00579                                                         
         CLI   SVTN1PR8,C'V'       IS COMML CLASS REQUIRED & VALIDATED          
         JE    ERR00579                                                         
         J     VCLSX                                                            
*                                                                               
VCLS10   CLI   SVTN1PR8,C'V'       IS COMMERCIAL CLASS VALIDATED                
         JNE   VCLS12                                                           
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCLASS                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
VCLS12   L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   CMLCLASS-CMLDTAEL(4,R4),QCMLCLAS                                 
VCLSX    DS    0H                                                               
*                                                                               
*                                                                               
*======================================================================         
* VALIDATE DESTROY DATE AND TIME                                                
*======================================================================         
*                                                                               
         XC    DSTRYDAT(5),DSTRYDAT   CLEAR DATE/TIME SAVE AREA                 
*                                                                               
         OC    QCMLDSDT,QCMLDSDT                                                
         JZ    CMLU100                                                          
*                                                                               
         MVC   DSTRYDAT,QCMLDSDT                                                
*                                                                               
         OC    QCMLDSTM,QCMLDSTM                                                
         JZ    CMLU100                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    RF,QCMLDSTM         FIND INPUT LEN                               
         LA    RE,L'QCMLDSTM-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                                                             
         STC   RE,BYTE             SAVE LEN                                     
*                                                                               
         LA    R4,QCMLDSTM                                                      
         ICM   R4,8,BYTE                                                        
         GOTOR VTIMVAL,DMCB,(R4),WORK                                           
         CLI   0(R1),X'FF'                                                      
         JE    ERR00747                                                         
         MVC   DSTRYTIM,WORK                                                    
         OC    DSTRYTIM,DSTRYTIM   TEST FOR MIDNIGHT                            
         JNZ   *+10                                                             
         MVC   DSTRYTIM,=H'2400'                                                
*                                                                               
CMLU100  L     R4,ACMLREC                                                       
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'24'        UPDATE EXTENDED DATA ELEM                    
         BRAS  RE,GETEL                                                         
         JNE   CMLU100C                                                         
*                                                                               
         BAS   RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
CMLU100C LA    R4,ELEM                                                          
         USING CMLXDTEL,R4                                                      
*                                                                               
         CLC   CMLXPRNT,QCMLPRNT                                                
         JE    *+8                                                              
         OI    CHGFLAG3,CMLCH_PRNT                                              
*                                                                               
         CLC   CMLXHDEF,SVHIDEF                                                 
         JE    *+8                                                              
         OI    CHGFLAG3,CMLCH_HDEF                                              
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   CMLXDTEL,X'24'                                                   
         MVI   CMLXDTLN,CMLXDTX-CMLXDTEL                                        
         MVC   CMLXHDEF,SVHIDEF                                                 
         MVC   CMLXPRNT,QCMLPRNT                                                
         MVC   CMLXDSDT,DSTRYDAT                                                
         MVC   CMLXDSTM,DSTRYTIM                                                
         MVC   CMLXHDPK,NWHIDEFX                                                
*                                                                               
         LLC   R1,CMLXDTLN                                                      
         SHI   R1,3                                                             
*                                                                               
         BASR  RF,0                                                             
         OC    ELEM+2(0),ELEM+2                                                 
         EX    R1,0(RF)                                                         
         JZ    CMLU101             DO NOT ADD AN EMPTY ELEMENT                  
*                                                                               
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         DROP  R4                                                               
*                                                                               
*                                                                               
*======================================================================         
*  VALIDATE NETWORKS TO INCLUDE/EXCLUDE                                         
*======================================================================         
*                                                                               
* SAVE 22 ELEMENT IN SVNETS TO PRESERVE DATES                                   
CMLU101  XC    SVNETS,SVNETS                                                    
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'22'        COMMERCIAL NETWORK ELEMENT                   
         BRAS  RE,GETEL                                                         
         JNE   CMLU106                                                          
*                                                                               
         LA    R3,SVNETS           SAVE 4 ELEMENTS MAX                          
CMLU105  LLC   R1,1(R4)            ELEM LENGTH                                  
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R3),0(R4)       SAVE ELEMENT                                 
         EX    R1,0(RF)                                                         
         AR    R3,R1               BUMP TO NEXT                                 
         LA    R3,1(R3)            AVAILABLE SLOT                               
         BRAS  RE,NEXTEL                                                        
         JE    CMLU105                                                          
*                                                                               
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,REMEL            DELETE ELEMENT (X'22')                       
*                                                                               
CMLU106  MVC   SVKEY,IOKEY         SAVE KEY                                     
         MVI   BYTE,C'X'           PRESET FLAG TO NET EXCLUDE                   
         OC    ANETI,ANETI         INCLUDE NETWORKS                             
         JZ    CMLU107                                                          
         MVI   BYTE,C'I'           SET FLAG NET INCLUDE                         
         OC    ANETX,ANETX         OR EXCLUDE NETWORKS                          
         JNZ   EXNETERR            BUT NOT BOTH                                 
*                                                                               
CMLU107  OC    ANETX,ANETX                                                      
         JNZ   *+8                                                              
         MVI   BYTE,0                                                           
*                                                                               
         ICM   R2,15,QNETIIND      ANY NETWORKS                                 
         JZ    CMLU108                                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,ANETI          LIST OF NETWORKS                             
         USING LW_D,R2                                                          
         MVC   NUMNET,LW_NUMN                                                   
         J     CMLU109                                                          
*                                                                               
CMLU108  ICM   R2,15,QNETXIND      ANY NETWORKS TO EXCLUDE                      
         JZ    CMLU110                                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,ANETX          LIST OF NETWORKS                             
         USING LW_D,R2                                                          
         MVC   NUMNET,LW_NUMN                                                   
*                                                                               
CMLU109  AHI   R2,LW_LN2Q          POINT TO THE FIRST NET IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMNET         # OF NETS                                    
         STC   R0,LOOPCT           # OF NETS LEFT TO PROCESS                    
         J     CMLU115                                                          
         DROP  R2                                                               
*                                                                               
CMLU110  DS    0H                                                               
         ICM   R2,15,QNETDIND      ANY NETWORKS TO DELETE                       
         JZ    CMLU202                                                          
*                                                                               
*NOP     BAS   RE,CHKDUP                                                        
*                                                                               
         MVI   BYTE,C'D'           PROCESS DELETED NETS                         
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,ANETD          LIST OF NETWORKS TO DELETE                   
         USING LW_D,R2                                                          
         MVC   NUMNETD,LW_NUMN                                                  
                                                                                
         AHI   R2,LW_LN2Q          POINT TO THE FIRST NET IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMNETD        # OF NETS TO DELETE                          
         STC   R0,LOOPCT           # OF NETS LEFT TO PROCESS                    
         DROP  R2                                                               
*                                                                               
CMLU115  STC   R0,LOOPCT           # OF NETS LEFT TO PROCESS                    
         MVC   SVQNET,0(R2)                                                     
         BRAS  RE,VNET             VALIDATE NETWORK                             
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R4,ELEM                                                          
         USING CMLNETEL,R4                                                      
*                                                                               
         CLI   BYTE,C'D'           ARE WE PROCESS DELETED NETS?                 
         JE    CMLU118                                                          
*                                                                               
         MVI   CMLNETEL,X'22'      ELEMENT ID                                   
         MVI   CMLNETLN,CMLNETLE   AND ELEMENT LENGTH                           
         MVC   CMLDATE,BTODAY      PRESET ADD DATE TO TODAY'S DATE              
*                                                                               
         MVC   CMLNET,SVQNET                                                    
         OC    CMLNET,SPACES                                                    
*                                                                               
         OC    ANETX,ANETX         EXCLUDE NETWORKS ?                           
         JZ    CMLU119                                                          
         OI    CMLFLG,CMLEXNET                                                  
         J     CMLU119                                                          
*                                                                               
* SAVE DELETED NETWORK                                                          
CMLU118  DS    0H                                                               
         MVI   CMLNETEL,X'23'      DELETED NETWORK ELEMENT                      
         MVI   CMLNETLN,CMLDNTLE   ELEMENT LENGTH                               
         MVC   CMLDDATE,BTODAY     SET DELETE DATE TO TODAY'S DATE              
         MVC   CMLNET,SVQNET                                                    
         OC    CMLNET,SPACES                                                    
*                                                                               
* GET NETWORK ADD DATE                                                          
*                                                                               
CMLU119  LA    RE,SVNETS                                                        
*                                                                               
CMLU120  CLC   CMLNET,2(RE)        IS THIS THE NETWORK                          
         JNE   CMLU122                                                          
         OC    7(3,RE),7(RE)       ANY DATE                                     
         JZ    CMLU124                                                          
         MVC   CMLDATE,7(RE)       MOVE NETWORK ADD DATE                        
         J     CMLU124                                                          
*                                                                               
CMLU122  LLC   R1,1(RE)            ENTRY LENGTH                                 
         AR    RE,R1               BUMP TO NEXT ENTRY IN ELEM                   
         CLI   0(RE),0             ANY MORE ELEMS                               
         JNE   CMLU120              YES                                         
*                                                                               
         MVC   IOKEY,SVKEY         RESTORE KEY                                  
*                                                                               
CMLU124  MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
CMLU125  LA    R2,L'SVQNET(R2)     BUMP TO NEXT NETWORK                         
         IC    R0,LOOPCT           # OF NETS LEFT TO PROCESS                    
         JCT   R0,CMLU115                                                       
                                                                                
CMLU130  DS    0H                                                               
         CLI   BYTE,C'D'           DID WE PROCESS DELETED NETS?                 
         JNE   CMLU110                                                          
*                                                                               
*                                                                               
*======================================================================         
* VALIDATE ACTUAL COMMERCIALS                                                   
* IF DELETE, LEAVE TITLES ALONE                                                 
* IF RESTORE, VALIDATE ACT CMLS IF COV                                          
*======================================================================         
*                                                                               
CMLU202  TM    FLAGS,SOFTDEL       SOFT DELETE THIS CML                         
         JNZ   CMLU210             YES - NO OTHER CHANGES                       
         TM    FLAGS,SOFTREST      RESTORE SOFT DELETE?                         
         JZ    CMLU204             NO, CONTINUE                                 
         TM    FLAGS,ISCOVCML      IS THIS A COVER?                             
         JNZ   CMLU205             YES - MAKE SURE ACTUALS STILL VALID          
         J     CMLU210             NO - DISALLOW OTHER CHANGES                  
*                                                                               
* ACCEPT NO CHANGES ON A DELETED CMML                                           
*                                                                               
CMLU204  L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    CMLSTAT-CMLDTAEL(R4),X'80'                                       
         JNZ   ERR00535                                                         
*                                                                               
CMLU205  DS    0H                                                               
         MVC   BYTE4,ADIDFLAG                                                   
         MVI   ERRFLG,0                                                         
         BRAS  RE,VALACT                                                        
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         MVC   ADIDFLAG,BYTE4                                                   
*                                                                               
CMLU210  MVC   IOKEY,SVKEY                                                      
*                                                                               
*===============================================================                
* ADD CHANGE ELEMENT TO RECORD                                                  
*===============================================================                
*                                                                               
CMLU215  BRAS  RE,SETCHGEL                                                      
*                                                                               
         L     RE,ACMLREC                                                       
         ST    RE,SVAREC                                                        
         BRAS  RE,AACTELEM         ADD ACTIVITY ELEMENT                         
*                                                                               
         L     R4,ACMLREC                                                       
         CLI   ADIDFLAG,C'Y'       TEST CMML PACKED                             
         JNE   *+12                NO                                           
         OI    15(R4),X'01'        TURN ON STATUS IN RECORD                     
         OI    IOKEY+13,X'01'      AND IN KEY FOR PACKED CML                    
*                                                                               
         CLI   RECACT,C'X'                                                      
         JE    CMLU218                                                          
         CLI   RECACT,C'R'                                                      
         JE    CMLU218                                                          
         CLI   RECACT,C'C'                                                      
         JNE   CMLU220                                                          
*                                                                               
CMLU217  MVI   ERRFLG,0                                                         
         BRAS  RE,PUT                                                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
CMLU218  GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+#CMLREC'                    
         JNE   CLTIODIE                                                         
*                                                                               
         MVC   SVDSKADR,IODA       SAVE DISK ADDR OF CHANGED REC                
*                                                                               
         MVI   ERRFLG,0                                                         
         BRAS  RE,ACH               AFTER PUT CHK ACTUAL CML REWRITE            
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
         J     CMLUX                                                            
*                                                                               
CMLU220  MVC   SVIOKEY,IOKEY       SAVE KEY BEFORE ADDREC                       
*                                                                               
         BRAS  RE,PAREC            PRIOR TO ADD RECORD GET SEQ#                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+#CMLREC'                    
         JNE   CLTIODIE                                                         
*                                                                               
         BRAS  RE,AAREC            AFTER ADD UPDATE SEQ# ADD PASSIVE            
CMLUX    J     EXITY                                                            
*                                                                               
CLTIODIE DC    H'0'                                                             
*                                                                               
EXNETERR LA    RF,L'NETINEX        NET INCLUDE EXCLUDE ERROR                    
         LA    R1,NETINEX                                                       
         J     ERR                 ERROR                                        
*                                                                               
         GETEL R4,24,ELCODE                                                     
*                                                                               
*---------------------------------------------------                            
* MOVE ELEMENT TO ELEM                                                          
*---------------------------------------------------                            
MVELEM   XC    ELEM,ELEM                                                        
         LLC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         MVC   ELEM(0),0(R4)                                                    
         EX    R1,0(RF)                                                         
         BR    RE                                                               
*---------------------------------------------------                            
* FIND INPUT LEN-1                                                              
* (ON ENTRY RF=FIELD, RE=MAX FIELD LEN                                          
*---------------------------------------------------                            
*                                                                               
FLEN     AR    RE,RF               POINT TO THE END OF FIELD                    
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         SR    RE,RF               LEN -1                                       
         BR    R1                                                               
*                                                                               
*                                                                               
*---------------------------------------------------                            
* VALIDATE RECORD CHECK SUM                                                     
*---------------------------------------------------                            
VCKSUM   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACMLREC          COMMERCIAL RECORD                            
         SR    RF,RF                                                            
         ICM   RF,3,CMLRLEN-CMLRECD(RE)                                         
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,FULL                                                       
*                                                                               
         CLC   FULL,QCKSM                                                       
         JNE   ERR00814                                                         
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
*==================================================                             
* VALIDATE NETWORK                                                              
*==================================================                             
*                                                                               
         DS    0H                                                               
VNET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VNET10   STC   R0,LOOPCT                                                        
*                                                                               
         MVI   IOKEY,C'0'                                                       
         MVC   IOKEY+1(16),IOKEY   PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,IOKEY                                                         
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),SVQNET                                               
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,LP_AGY                                                   
*NOP     MVC   STAKCLT,SVBCLT                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSTAFIL+IO7'                            
         CLC   IOKEY(STAKEYLN),IOKEYSAV                                         
         JNE   ERR00503            NETWORK NOT FOUND                            
*                                                                               
         CLI   RECACT,C'C'                                                      
         JNE   VNETX                NO, DONE                                    
*                                                                               
         MVI   BYTE1,1             GENERATE PATTERN T/A REQ                     
*                                                                               
VNETX    XIT1                                                                   
         DROP  R4                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GET PROFILES                                                                  
***********************************************************************         
*                                                                               
GPROF    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,CPROFTAB         PRESET TO COMMERCIAL PROFILES                
         CLC   LP_QMAPN,=AL2(M#NECMLU)                                          
         JE    GPROF01                                                          
         DC    H'0'                BUG CATCHER                                  
*                                                                               
GPROF01  XC    FULL,FULL           INIT OUTPUT PROFILE NAME                     
         XC    WORK,WORK           INIT OUTPUT PROFILE VALUES                   
*                                                                               
         MVC   FULL(4),0(R4)       SET OUTPUT PROFILE NAME                      
         XC    TEMP,TEMP                                                        
         MVC   TEMP+00(2),=C'S0'                                                
         MVC   TEMP+02(2),0(R4)                                                 
         CLI   2(R4),C' '          3 character profile?                         
         JNH   GPROF04                                                          
         MVI   TEMP+00,C's'        Yes                                          
         MVC   TEMP+01(3),0(R4)                                                 
GPROF04  MVC   TEMP+04(2),LP_AGY                                                
         MVC   TEMP+06(L'QMEDA),QMEDA                                           
         MVC   TEMP+07(L'QCLTA),QCLTA                                           
         MVI   TEMP+10,C'*'                                                     
         MVC   TEMP+11(L'SVCLTOFF),SVCLTOFF                                     
         GOTOR VGETPROF,DMCB,(FULL+3,TEMP),WORK,VDATAMGR                        
*                                                                               
         CLC   =C'TN ',0(R4)                                                    
         JNE   GPROF10                                                          
*NOP     MVC   SVPROF,WORK       <<< PER RAMUNE ->NO CML PADDED '-'             
         J     GPROF30                                                          
*                                                                               
GPROF10  CLC   =C'TN1',0(R4)                                                    
         JNE   GPROF20                                                          
         MVC   SVT1PROF,WORK                                                    
         J     GPROF30                                                          
*                                                                               
GPROF20  CLC   =C'TN2',0(R4)                                                    
         JE    *+6                                                              
         DC    H'0'                BUG CATCHER                                  
         MVC   SVTN2PR5,WORK+4                                                  
         MVC   SVTN2PR9,WORK+8                                                  
         J     GPROF30                                                          
*                                                                               
GPROF30  LA    R4,PROFTABL(R4)     NEXT ENTRY IN TABLE                          
         CLC   0(2,R4),=X'0000'                                                 
         JNE   GPROF01                                                          
*                                                                               
         J     EXITY                                                            
*                                                                               
* COMMERCIAL PROFILES TABLE                                                     
*                                                                               
CPROFTAB DC    C'TN ',X'C0'                                                     
PROFTABT DC    C'TN1',X'C0'                                                     
         DC    C'TN2',X'C0'                                                     
         DC    X'0000'                                                          
PROFTABL EQU   PROFTABT-CPROFTAB                                                
*                                                                               
*                                                                               
REMEL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VHELLO,DMCB,(C'D',SYSFIL),(ELCODE,(R7)),0,0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
*                                                                               
ADDEL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VHELLO,DMCB,(C'P',SYSFIL),(R7),ELEM,0                            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
*                                                                               
*---------------------------------------------------------                      
* GET HIDEF COMMERCIAL FROM EXISTING RECORD                                     
*---------------------------------------------------------                      
*                                                                               
GETHIDEF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AIO8                                                          
         MVI   ELCODE,X'24'        EXTENDED DATA ELEMENT                        
         BRAS  RE,GETEL                                                         
         JNE   EXITN                                                            
*                                                                               
         USING CMLXDTEL,R4                                                      
         OC    CMLXHDEF,CMLXHDEF   ANY HIDEF  ?                                 
         JZ    GETHI10                                                          
*                                                                               
         MVC   SVHIDEFX,CMLXHDPK   SAVE HEX HIDEF                               
*                                                                               
GETHI10  CLC   SVHIDEF,CMLXHDEF    SAME HIDEF CML?                              
         JE    EXITY                                                            
         JNE   EXITN                                                            
*                                                                               
*                                                                               
*============================================================                   
* VALIDATE DATA IN COMMERCIAL FIELD AT 0(R2)                                    
* FOR NOW, 8 CHARS=ISCI, 9-12=ADID                                              
*============================================================                   
*                                                                               
VALCMML  NTR1  BASE=*,LABEL=*                                                   
         CLI   QCMLPRNT+8,X'40'                                                 
         JH    VALC10                                                           
*                                                                               
         MVC   DUB,QCMLPRNT        PRESUMING IT'S GOOD                          
         LA    R0,8                                                             
         LA    R1,QCMLPRNT                                                      
*                                                                               
VALC2    CLI   0(R1),C'A'                                                       
         JL    ERR00577                                                         
         LA    R1,1(,R1)                                                        
         JCT   R0,VALC2                                                         
         J     VALC20                                                           
*                                                                               
VALC10   DS   0H                                                                
         LA    RF,QCMLPRNT                                                      
         LA    RE,L'QCMLPRNT-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                                                             
         STC   RE,BYTE                                                          
         LA    R1,QCMLPRNT                                                      
*                                                                               
VALC14   CLI   0(R1),C'A'                                                       
         JL    ERR00783                                                         
         CLI   0(R1),C'Z'                                                       
         JNH   VALC16                                                           
*                                                                               
         CLI   0(R1),C'0'                                                       
         JL    ERR00783                                                         
         CLI   0(R1),C'9'                                                       
         JH    ERR00783                                                         
*                                                                               
VALC16   LA    R1,1(R1)                                                         
         JCT   RE,VALC14                                                        
*                                                                               
         OC    QCMLPRNT,SPACES                                                  
         GOTOR VTRPACK,DMCB,(C'P',QCMLPRNT),DUB                                 
         JNE   ERR00783                                                         
*                                                                               
VALC20   CLC   DUB,SVKEY+CMLKCML-CMLKEY   TEST SAME AS THIS CMML                
         JE    EXIT                                                             
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0A21'                                                
         MVC   IOKEY+2(3),SVBAGM     A-M/CLT                                    
         MVC   IOKEY+5(8),DUB                                                   
*                                                                               
         CLI   BYTE,8             TEST INPUT IS ADID                            
         JNH   *+10                                                             
         MVC   IOKEY(2),=X'0AC1'                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00053                                                         
*                                                                               
* NEED TO CHECK FOR SOFT DELETE                                                 
*                                                                               
VALC30   DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
*                                                                               
         L     RE,AIO7                                                          
         TM    CMLSTAT-CMLRECD(RE),X'80'   TEST SOFT DELETED                    
         JO    ERR00535                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*=======================================================                        
* VALIDATE PRODUCT LIST AND BUILD PROD LIST ELEMENT                             
*=======================================================                        
*                                                                               
VPRDL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   R2,15,QPRDIND       ANY PROD ?                                   
         JZ    ERR00260             NO                                          
*                                                                               
         CLI   RECACT,C'C'         IF ACTION CHANGE                             
         JNE   VPRDL04                                                          
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'20'        FIND ORIGINAL ELEMENT                        
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OLDELEM,0(R4)       SAVE OLD ELEM                                
*                                                                               
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,REMEL            DELETE ELEMENT (X'20')                       
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'29'                                                     
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,REMEL            DELETE ELEMENT (X'29')                       
*                                                                               
VPRDL04  XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING CMLPRDEL,R4                                                      
         MVI   CMLPRDEL,X'20'      ELEM CODE                                    
*                                                                               
         XC    MORPRDEL,MORPRDEL   CLEAR FOR 3 CHAR PROD LIST                   
         MVI   MORPRDEL,X'29'                                                   
         MVI   MORPRDEL+1,02       ELEM LENGTH EMPTY                            
         LA    R1,MORPRDEL+2          STARTING ADDR FOR 3 CHAR PRODS            
         ST    R1,SVR1                                                          
*                                                                               
         USING LW_D,R2                                                          
*NOP     CLC   =C'ALL',LW_DATA2                                                 
         CLC   =C'PRD=ALL',LW_DATA2                                             
         JNE   VPRDL08                                                          
*                                                                               
         CLI   SVTN2PR9,C'Y'        LIMIT ONE PRD PER ISCII                     
         JE    ERR00749             YES ERROR                                   
*                                                                               
         MVI   CMLPRDLN,3                                                       
         MVI   CMLPRDS,X'FF'                                                    
         MVC   NEWELEM,ELEM                                                     
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
         MVI   MORPRDEL+1,03                                                    
         MVI   MORPRDEL+2,X'FF'                                                 
         MVC   ELEM,MORPRDEL                                                    
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         J     VPRDL30                                                          
*                                                                               
VPRDL08  MVI   CMLPRDLN,2          ELEM LENGTH EMPTY                            
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,APRD           LIST OF PRODUCTS                             
         USING LW_D,R2                                                          
         MVC   NUMPRD,LW_NUMN                                                   
*                                                                               
         CLI   NUMPRD+1,38                                                      
         JH    ERR00485            TOO MANY ENTRIES                             
*                                                                               
         CLI   SVTN2PR9,C'Y'        LIMIT ONE PRD PER ISCII                     
         JNE   *+12                                                             
         CLI   NUMPRD+1,1                                                       
         JNE   ERR00749             YES ERROR                                   
*                                                                               
*                                                                               
         LA    R5,ELEM+2                                                        
         AHI   R2,LW_LN2Q          POINT TO THE FIRST PRD IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMPRD         # OF PRDS                                    
*                                                                               
VPRDL10  STC   R0,LOOPCT           # OF PRDS LEFT TO PROCESS                    
         CLC   =C'POL',0(R2)       THIS IS ILLEGAL                              
         JE    ERR00102                                                         
         CLC   =C'AAA',0(R2)       THIS IS ILLEGAL                              
         JE    ERR00102                                                         
         CLI   3(R2),X'40'         PRODUCT CODE IS MAX 3 CHAR                   
         JH    ERR00102                                                         
*                                                                               
P        USING PRDRECD,IOKEY                                                    
         XC    P.PKEY,P.PKEY                                                    
         MVC   P.PKEYAM,QMEDX                                                   
         MVC   P.PKEYCLT,QCLTX                                                  
         MVC   P.PKEYPRD,0(R2)                                                  
         DROP  P                                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00073            INVALID PRODUCT                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO7                                                          
         USING PRDRECD,RE                                                       
*                                                                               
         MVC   0(1,R5),PCODE+1     MOVE BINARY PRODUCT CODE TO ELEM             
         LA    R5,1(R5)            BUMP IN X'20' ELEM                           
*                                                                               
         L     R1,SVR1             ADDRESS OF NEXT                              
VPRDL18  MVC   0(3,R1),0(R2)                                                    
         AHI   R1,3                                                             
         ST    R1,SVR1                                                          
         IC    RE,MORPRDEL+1                                                    
         AHI   RE,3                                                             
         STC   RE,MORPRDEL+1                                                    
         IC    R1,ELEM+1           GET ELEM LENGTH                              
         AHI   R1,1                                                             
         STC   R1,ELEM+1                                                        
*                                                                               
*Per Ramune disable for now                                                     
*&&DO                                                                           
         CLI   SVTN2PR5,C'1'       IF NET TALENT USER, NOT ALLOWED              
         JE    *+12                                                             
         CLI   SVTN2PR5,C'Y'       IF NET TALENT USER, NOT ALLOWED              
         JNE   *+8                                                              
         BAS   RE,CKTAL            SEE IF PROD HAS TAL AGENCY                   
*&&                                                                             
*                                                                               
         LA    RE,QPRDENT                                                       
         AR    R2,RE               BUMP TO NEXT PROD                            
         IC    R0,LOOPCT           # OF PRODS LEFT TO PROCESS                   
         JCT   R0,VPRDL10                                                       
*                                                                               
         CLI   CMLPRDLN,2          WERE ANY PROD CODES FOUND                    
         JE    ERR00260             NO                                          
*                                                                               
         MVC   NEWELEM,ELEM                                                     
*                                                                               
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
         MVC   ELEM,MORPRDEL                                                    
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
                                                                                
VPRDL30  CLI   RECACT,C'A'         IF ACTION ADD                                
         JE    VPRDLX                                                           
*                                                                               
         LA    RE,OLDELEM                                                       
         SR    RF,RF                                                            
         IC    RF,OLDELEM+1                                                     
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         CLC   OLDELEM(0),NEWELEM  COMPARE NEW/OLD PRD ELEMS                    
         EX    RF,0(R1)                                                         
         JE    VPRDLX                                                           
         OI    CHGFLAG1,CMLCH_PRDL                                              
         CLI   RECACT,C'A'         ACTION ADD                                   
         JE    VPRDLX               YES                                         
         MVI   BYTE1,1             GENERATE PATTERN T/A REQ                     
VPRDLX   J     EXIT                                                             
*                                                                               
*                                                                               
*&&DO                                                                           
*Per Ramune disabled for now                                                    
*=======================================================                        
* CHECK IF PRODUCT HAS A TALENT AGENCY *                                        
*=======================================================                        
*                                                                               
CKTAL    NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY+PKEYAM-PKEY(3),BAGYMD                                      
         MVC   IOKEY+PKEYPRD-PKEY(3),=C'POL' IF POL HAS ONE, THEN OK            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         JNE   INVALID PRODUCT MSG                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    PTALAGY,PTALAGY                                                  
         JZ    *+14                                                             
         CLC   PTALAGY,SPACES                                                   
         JNE   CKTAL10                                                          
*                                                                               
*NOP     MVC   IOKEYSAV,IOKEY                                                   
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY+PKEYAM-PKEY(3),BAGYMD                                      
         MVC   IOKEY+PKEYPRD-PKEY(3),0(R2)                                      
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         JNE   INVALID PRODUCT MSG                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
                                                                                
*Per Ramune disable for now.                                                    
*NOP     CLI   PFKEY,1                                                          
****     JE    CKTAL10                                                          
*        OC    PTALAGY,PTALAGY                                                  
*        JZ    NOTALER                                                          
*        CLC   PTALAGY,SPACES                                                   
******   JE    NOTALER                                                          
CKTAL10  MVC   IOKEY,IOKEYSAV                                                   
         J     VPRDLX                                                           
*&&                                                                             
*                                                                               
*===========================================================                    
* VALIDATE COMMERCIAL LENGTH                                                    
*===========================================================                    
*                                                                               
VSLN     NTR1  BASE=*,LABEL=*                                                   
         USING CMLDTAEL,R4                                                      
         CLI   SVSLN,0                                                          
         JE    ERR00673                                                         
         CLI   SVSLN,X'FF'                                                      
         JE    ERR00673                                                         
                                                                                
         CLI   RECACT,C'C'                                                      
         BNE   VSLN10                                                           
         CLC   CMLSLN,SVSLN                                                     
         BE    VSLN10                                                           
         OI    CHGFLAG1,CMLCH_SLN                                               
         MVI   BYTE1,1             GENERATE PATTERN T/A REQ                     
                                                                                
VSLN10   MVC   CMLSLN,SVSLN                                                     
         J     EXITY                                                            
*                                                                               
VNUM     NTR1  BASE=*,LABEL=*                                                   
         LLC   R1,BYTE             Len-1 of input                               
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVZ   WORK(0),0(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         JNE   ERR00673            Invalid number                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         JZ    ERR00673            Bad len                                      
         CHI   R1,255                                                           
         JH    ERR00673            Bad len                                      
         STC   R1,BYTE             Save actual                                  
         J     EXITY                                                            
*                                                                               
*                                                                               
*============================================================                   
* VALIDATE TITLE FIELDS                                                         
* NOTE THAT ON ENTRY R4 POINTS TO X'10' ELEMENT IN RECORD                       
*============================================================                   
*                                                                               
         USING CMLDTAEL,R4                                                      
VALTTL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM2,ELEM2                                                      
         MVC   ELEM2(24),QCMLTTLE                                               
         MVC   ELEM2+24(24),QCMLDSC2                                            
         MVC   ELEM2+48(24),QCMLDSC3                                            
         OC    ELEM2(72),SPACES                                                 
*                                                                               
         OC    QCMLDSC2,QCMLDSC2   ANY TITLE 2 ENTERED                          
         JNZ   VALTTL2             YES                                          
         OC    QCMLDSC3,QCMLDSC3   IF NOT, SHOULD BE NO  TITLE 3                
         JNZ   ERR00537                                                         
*                                                                               
VALTTL2  L     R4,ACMLREC          TEST FOR CHANGES IT TITLE2/3                 
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         JNE   VALTTL10                                                         
         LA    R1,ELEM2            DESCRIPTION 1/2/3                            
VALTTL03 OC    3(24,R4),SPACES                                                  
         CLC   0(24,R1),3(R4)                                                   
         JE    *+8                                                              
         OI    CHGFLAG1,CMLCH_DESC                                              
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   VALTTL05                                                         
         LA    R1,24(R1)           NEXT DESCRIPTION                             
         J     VALTTL03                                                         
*                                                                               
VALTTL05 MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,REMEL            DELETE ELEMENT (X'30')                       
*                                                                               
VALTTL10 XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING CMLDSCEL,R4                                                      
*                                                                               
         MVI   BYTE,0                                                           
         LA    R5,ELEM2                                                         
*                                                                               
VALTTL12 MVI   ELEM,X'30'          BUILD NEW X'30' ELEMS AND INSERT             
         MVI   ELEM+1,27                                                        
         MVC   ELEM+2,BYTE         SEQUENCE NUMBER                              
         MVC   ELEM+3(L'QCMLDSC2),0(R5)                                         
         OC    ELEM+3(L'QCMLDSC2+4),SPACES                                      
*                                                                               
         LLC   R1,BYTE                                                          
         AHI   R1,1                INCR SEQ NUMBER                              
         STC   R1,BYTE                                                          
         LA    R5,24(R5)           NEXT DESC                                    
*                                                                               
         LLC   R1,CMLDSCLN         ELEM LEN                                     
         SHI   R1,4                ADJ FOR EX (ELCODE/LEN/LIN#)                 
         BASR  RF,0                                                             
         CLC   ELEM+3(0),SPACES                                                 
         EX    R1,0(RF)                                                         
         JNH   EXIT                DO NOT ADD AN EMPTY ELEMENTS                 
*                                                                               
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
         CLI   BYTE,4                                                           
         JL    VALTTL12                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*======================================================================         
* VALIDATE COMMERCIAL CLASS                                                     
*======================================================================         
*                                                                               
VCLASS   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLG,0                                                         
*                                                                               
* SEE IF A CLASS RECORD EXISTS FOR SOME PRD OR THE CLIENT                       
         MVC   SVKEY,IOKEY                                                      
         XC    IOKEY,IOKEY                                                      
*                                                                               
         LA    R2,IOKEY                                                         
         USING CLSKEY,R2                                                        
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,SVBAGM                                                    
         MVC   CLSKCLAS,QCMLCLAS                                                
         OC    CLSKCLAS,SPACES                                                  
         MVC   CLSKCLT,SVBCLT                                                   
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'29'        GET PRODUCT ELEM                             
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R4),X'FF'         THIS ALL PRODS                               
         JE    VCLAS30                                                          
*                                                                               
         LA    R5,2(R4)            POINT TO PRODS                               
*                                                                               
         LLC   R0,1(R4)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'3'            GET NUMBER OF PRODUCTS                       
         LR    R3,R1                                                            
*                                                                               
VCLAS10  MVC   CLSKPROD,0(R5)                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    VCLAS40                                                          
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         LA    R5,3(R5)            POINT TO NEXT PRODUCT IN LIST                
         JCT   R3,VCLAS10                                                       
*                                                                               
         XC    CLSKPROD,CLSKPROD                                                
*                                                                               
VCLAS30  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    VCLAS40                                                          
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         XC    CLSKCLT,CLSKCLT                                                  
         DROP  R2                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00053            NOT FOUND                                    
*                                                                               
VCLAS40  MVC   IOKEY,SVKEY                                                      
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*============================================================                   
* VALIDATE PRINT OVERRIDE                                                       
*============================================================                   
*                                                                               
         USING CMLDTAEL,R4                                                      
VOVRD    NTR1  BASE=*,LABEL=*                                                   
         XC    CMLOVRD1(2),CMLOVRD1                                             
*                                                                               
         OC    QCMLOVRD,QCMLOVRD   ANY INPUT?                                   
         JZ    VOVRDX               NO                                          
*                                                                               
         LA    R2,FAKEFLDH                                                      
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
*                                                                               
         LA    RF,QCMLOVRD                                                      
         LA    RE,L'QCMLOVRD-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN-1                             
*                                                                               
         BASR  R1,0                                                             
         MVC   FAKEFLD(0),QCMLOVRD                                              
         EX    RE,0(R1)                                                         
*                                                                               
         AHI   RE,1                                                             
         STC   RE,FAKEFLDH+5       SET INPUT LENGTH IN OUR FAKE FLDH            
*                                                                               
         GOTOR VSCANNER,DMCB,(R2),(2,ELEM2),C',=/='                             
         CLI   DMCB+4,2            MUST BE 2 BLOCKS                             
         JNE   ERR00574                                                         
*                                                                               
         LA    R1,ELEM2                                                         
         TM    2(R1),X'80'                                                      
         JZ    ERR00003            MUST BE NUMERIC                              
         L     RE,4(R1)                                                         
         STC   RE,CMLOVRD1                                                      
*                                                                               
         LA    R1,32(R1)           NEXT SCANNER BLOCK                           
         TM    2(R1),X'80'                                                      
         JZ    ERR00003            MUST BE NUMERIC                              
         L     RF,4(R1)                                                         
         STC   RF,CMLOVRD2                                                      
*                                                                               
* MAKE SURE (OVRD1 + OVRD2 = SLN)                                               
*                                                                               
         AR    RE,RF                                                            
         IC    RF,CMLSLN                                                        
         CR    RE,RF                                                            
         JNE   ERR00575                                                         
*                                                                               
VOVRDX   J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
*---------------------------------------------------------------------          
* MAKE SURE THIS ADID DOES NOT EXIST AS ADID OR HIDEF ON ANOTHER REC            
*---------------------------------------------------------------------          
*                                                                               
DUPCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0AC1'                                                
         MVC   IOKEY+2(3),SVBAGM                                                
         MVC   IOKEY+5(8),HOLDCML  PACKED CML                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    ERR00049                                                         
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   IOKEY+1,X'C2'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    ERR00049                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*--------------------------------------------------------------                 
* FOR ACTION ADD, ADD AD-ID ELEMENT FOR PACKED CML                              
* SO ALWAYS HAVE PACKED AND UNPACKED ADID IN A0 ELEMENT FOR KATZ                
*--------------------------------------------------------------                 
*                                                                               
AADIDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'A0'        AD-ID  ELEM                                  
         LA    R4,ELEM                                                          
         USING CMLADIEL,R4                                                      
*                                                                               
         LA    R4,ELEM                                                          
         USING CMLADIEL,R4                                                      
         MVI   CMLADIEL,X'A0'                                                   
         MVI   CMLADILN,CMLADIDL-CMLADIEL                                       
         MVC   CMLADID,QCOMML                                                   
         OC    CMLADID,SPACES                                                   
*                                                                               
         GOTOR VTRPACK,DMCB,(C'P',CMLADID),CMLADIDP                             
         MVC   CMLADIDT,BTODAY        SAVE DATE                                 
*                                                                               
         TIME  DEC                                                              
         STCM  R0,15,CMLADITM      SAVE TIME                                    
         DROP  R4                                                               
*                                                                               
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
VALMAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    FLAGS,X'FF'-MATELEM                                              
         XC    FAKEFLD,FAKEFLD                                                  
         L     R4,ACMLREC                                                       
         MVI   ELCODE,CMLMATEQ     X'B0'                                        
         BRAS  RE,GETEL                                                         
         JNE   VALMAT05                                                         
*                                                                               
         BRAS  RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
         MVC   FAKEFLD,ELEM        SAVE PREVIOUS ELEMENT                        
*                                                                               
VALMAT05 XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING CMLMATCH,R4                                                      
         MVI   CMLMATEL,CMLMATEQ                                                
         MVI   CMLMATLN,CMLMATDL                                                
         LA    R5,CMLMPER1                                                      
*                                                                               
         ICM   R2,15,QMDTIND       ANY MATCH DATES                              
         JZ    VALMAT55             NO                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,AMDT           LIST OF MATCHING DATE                        
         USING LW_D,R2                                                          
         MVC   NUMMDT,LW_NUMN                                                   
*                                                                               
         CLI   NUMMDT+1,06                                                      
         JH    ERR00681   ???      TOO MANY ENTRIES                             
*                                                                               
         AHI   R2,LW_LN2Q          PT TO FIRST MATCHING DATE IN LIST            
         SR    R0,R0                                                            
         ICM   R0,3,NUMMDT         # OF MDATES                                  
*                                                                               
* PERIOD VALIDATION LOOP                                                        
VALMAT10 STC   R0,LOOPCT           # OF PRDS LEFT TO PROCESS                    
*                                                                               
         CLI   0(R2),0             ANY ENTRY                                    
         JE    VALMAT50            NO, GO TO NEXT FIELD                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CPERVAL-COMFACSD(RF)                                          
         MVI   BYTE,17             INPUT LEN                                    
         LA    R3,0(R2)                                                         
         ICM   R3,8,BYTE                                                        
         GOTOR (RF),DMCB,(R3),PERVALST                                          
         CLI   DMCB+4,0                                                         
         JNE   ERR00746                                                         
*                                                                               
         MVC   0(4,R5),PERVALST+PVALCSTA-PERVALD                                
         OI    FLAGS,MATELEM                                                    
*                                                                               
VALMAT50 DS    0H                                                               
         LA    R2,L'QCMLMDTE1(R2)                                               
         LA    R5,L'CMLMPER1(R5)                                                
         JCT   R0,VALMAT10                                                      
*                                                                               
VALMAT55 XC    CMLMSTIM,CMLMSTIM                                                
         MVC   CMLMETIM,=X'FFFF'                                                
*                                                                               
         OC    QCMLMSTM,QCMLMSTM                                                
         JZ    VALMAT60                                                         
*                                                                               
         LA    RF,QCMLMSTM                                                      
         LA    RE,L'QCMLMSTM-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                INPUT LEN                                    
         STC   RE,BYTE                                                          
*                                                                               
         LA    R5,QCMLMSTM                                                      
         ICM   R5,8,BYTE                                                        
         GOTOR VTIMVAL,DMCB,(R5),WORK                                           
         CLI   0(R1),X'FF'                                                      
         JE    ERR00747            INVALID TIME                                 
         MVC   CMLMSTIM,WORK                                                    
         OI    FLAGS,MATELEM                                                    
*                                                                               
VALMAT60 DS    0H                                                               
         OC    QCMLMETM,QCMLMETM Any input                                      
         JZ    VALMAT70                                                         
*                                                                               
         LA    RF,QCMLMETM                                                      
         LA    RE,L'QCMLMETM-1                                                  
         BRAS  R1,FLEN             FIND INPUT LEN                               
         AHI   RE,1                INPUT LEN                                    
         STC   RE,BYTE                                                          
*                                                                               
         LA    R5,QCMLMETM                                                      
         ICM   R5,8,BYTE                                                        
         GOTOR VTIMVAL,DMCB,(R5),WORK                                           
         CLI   0(R1),X'FF'                                                      
         JE    ERR00747                                                         
         MVC   CMLMETIM,WORK                                                    
         OI    FLAGS,MATELEM                                                    
*                                                                               
VALMAT70 DS    0H                                                               
         OC    QCMLMSTM,QCMLMSTM IF TIME ENTERED                                
         JNZ   VALMAT72            MUST INPUT DAILY FLAG                        
         CLC   QCMLMETM,=X'FFFF'                                                
         JE    VALMAT80                                                         
*                                                                               
VALMAT72 CLI   QDLYFLG,C'Y'                                                     
         JNE   VALMAT80                                                         
         OI    CMLMFLAG,CMLMFDAY                                                
         OI    FLAGS,MATELEM                                                    
*                                                                               
VALMAT80 DS    0H                                                               
         TM    FLAGS,MATELEM                                                    
         JNZ   VALMAT85                                                         
         OC    FAKEFLD,FAKEFLD                                                  
         JZ    VALMATX                                                          
*                                                                               
VALMAT85 DS    0H                                                               
E        USING CMLMATEL,FAKEFLD                                                 
         USING CMLMATEL,R4                                                      
         OC    E.CMLMETIM,E.CMLMETIM                                            
         JNZ   *+10                                                             
         MVC   E.CMLMETIM,=X'FFFF'                                              
*                                                                               
         CLC   E.CMLMSTIM(4),CMLMSTIM                                           
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_TIME                                              
*                                                                               
         CLC   E.CMLMFLAG,CMLMFLAG                                              
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_DLY                                               
*                                                                               
         CLC   E.CMLMPER1(24),CMLMPER1                                          
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_MDTS                                              
*                                                                               
         OI    FLAGS,MATELEM                                                    
         JZ    VALMATX                                                          
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
*                                                                               
VALMATX  J     EXIT                                                             
         DROP  E,R4                                                             
*                                                                               
*                                                                               
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF FIELD ABOUT TO BE ADDED                                
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
*----------------------------------------------------------------*              
HDDUPS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=XL2'0AC1'                                              
         MVC   IOKEY+2(3),SVBAGM                                                
         MVC   IOKEY+5(8),NWHIDEFX                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    HDDXIT                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   IOKEY+1,X'C2'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    HDDXIT                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         MVI   IOKEY+1,X'C3'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
*                                                                               
HDDXIT   J     EXIT                                                             
*                                                                               
*                                                                               
*======================================================================         
* VALIDATE ACTUAL COMMERCIALS AND INSERT ELEMENTS IN RECORD                     
*======================================================================         
*                                                                               
VALACT   NTR1  BASE=*,LABEL=*                                                   
         XC    OLDACTS(L'OLDACTS*NUMACTS),OLDACTS                               
         XC    NEWACTS(L'NEWACTS*NUMACTS),NEWACTS                               
         XC    ACTSLN,ACTSLN                                                    
*                                                                               
* SAVE OFF OLD ACTUALS                                                          
*                                                                               
         LA    R1,OLDACTS                                                       
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'60'                                                     
         USING CMLACTEL,R4                                                      
         BRAS  RE,GETEL                                                         
         JNE   VACT5                                                            
         J     VACT3                                                            
*                                                                               
VACT2    BRAS  RE,NEXTEL                                                        
         JNE   VACT4                                                            
VACT3    MVC   0(L'CMLACTID,R1),CMLACTID  8 CHAR CML                            
         CLI   CMLACTLN,CMLACTL1          OLD ELEM                              
         JE    *+10                                                             
         MVC   0(L'CMLACTCM,R1),CMLACTCM   NO, MOVE IN 12 CHAR CML              
         LA    R1,L'OLDACTS(R1)                                                 
         JCT   R0,VACT2                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
VACT4    DS    0H                                                               
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
* VALIDATE ID'S ON SCREEN & BUILD TABLE OF NEW ACTUAL ID'S                      
VACT5    ICM   R2,15,QACTIND       ANY ACTUAL CMLS                              
         JZ    EXIT                                                             
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,AACT           LIST OF ACTUAL COMMERCIALS                   
         USING LW_D,R2                                                          
         MVC   NUMACT,LW_NUMN                                                   
*                                                                               
         CLI   NUMACT+1,04                                                      
         JH    ERR00681   ???      TOO MANY ENTRIES                             
*                                                                               
         LA    R5,ELEM+2                                                        
         AHI   R2,LW_LN2Q          PT TO FIRST ACTUAL CMLS IN LIST              
         SR    R0,R0                                                            
         ICM   R0,3,NUMACT         # OF ACTUAL CMLS                             
         DROP  R2                                                               
*                                                                               
VACT5C   STC   R0,LOOPCT           # OF ACT CMLS LEFT TO PROCESS                
         CLC   =C'UNCOVER',0(R2)                                                
         JNE   *+12                                                             
         OI    FLAGS,UNCOVER                                                    
         J     VACT24                                                           
*                                                                               
         LA    R5,NEWACTS                                                       
*                                                                               
VACT6    OC    0(L'QCMLACT,R2),0(R2) ANY INPUT?                                 
         JZ    VACT10               NO                                          
         TM    FLAGS,ISCOVERD      IS THIS CMML COVERED?                        
         JNZ   ERR00667             NO                                          
*                                                                               
         MVC   QCMLACT,0(R2)                                                    
         MVI   ERRFLG,0                                                         
         BRAS  RE,VID                                                           
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
         MVC   0(L'NEWACTS,R5),0(R2)                                            
         OC    0(L'NEWACTS,R5),SPACES                                           
         LA    R5,L'NEWACTS(R5)                                                 
*                                                                               
VACT10   LA    R2,L'QCMLACT(R2)                                                 
         JCT   R0,VACT6                                                         
*                                                                               
* MAKE SURE 2+ ACTUALS USED                                                     
*                                                                               
         CLI   NUMACT+1,0           ANY ACTUALS?                                
         JNE   VACT12               YES                                         
         TM    FLAGS,ISCOVCML      WERE THERE ACTUALS BEFORE?                   
         JNZ   ERR00665             YES                                         
         J     VACT22                                                           
*                                                                               
VACT12   CLI   NUMACT+1,2                                                       
         JL    ERR00665                                                         
*                                                                               
* CHECK THAT ACCUM SLN'S = COV SLN                                              
*                                                                               
VACT16   L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   ACTSLN,CMLSLN-CMLDTAEL(R4)                                       
         JNE   ERR00666                                                         
*                                                                               
* ADD X'60' ELS                                                                 
*                                                                               
VACT20   XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING CMLACTEL,R1                                                      
*                                                                               
         LLC   R0,NUMACT+1         COUNT OF ACT CMMLS                           
         LA    R2,NEWACTS                                                       
*                                                                               
VACT22   MVI   CMLACTEL,X'60'                                                   
         MVI   CMLACTLN,CMLACTL2                                                
         MVC   CMLACTCM,0(R2)                                                   
         OC    CMLACTCM,SPACES                                                  
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL                                                         
         LA    R2,L'NEWACTS(R2)                                                 
         JCT   R0,VACT22                                                        
*                                                                               
         DROP    R1                                                             
*                                                                               
VACT24   SR    R1,R1                                                            
         LA    R0,NUMACTS                                                       
*                                                                               
VACT26   LA    RE,OLDACTS(R1)                                                   
         LA    RF,NEWACTS(R1)                                                   
         CLC   0(L'NEWACTS,RE),0(RF)  NEW MATCH OLD                             
         JE    *+8                                                              
         OI    CHGFLAG2,CMLCH_ACTS   SET CHANGE FLAG - NOT IN OLD LIST          
         LA    R1,L'OLDACTS(R1)                                                 
         JCT   R0,VACT26                                                        
         J     EXIT                                                             
*                                                                               
*                                                                               
* VALIDATE ACTUAL ID AND IT'S SLN TO ACTSLN                                     
*  R2 = POINTS TO ACTUL CML                                                     
*                                                                               
VID      NTR1  BASE=*,LABEL=*                                                   
         LR    RF,R2                                                            
         LA    RE,L'QCMLACT-1                                                   
         BRAS  R1,FLEN             MUST BE AT LEAST 8 CHARS                     
         AHI   RE,1                                                             
         STC   RE,BYTE             SAVE INPUT LEN                               
         CLI   BYTE,8                                                           
         JL    ERR00783                                                         
                                                                                
         MVI   ERRFLG,0                                                         
         BRAS  RE,VCML                                                          
         CLI   ERRFLG,1                                                         
         JE    EXITN                                                            
*                                                                               
* CMML CAN'T COVER ITSELF                                                       
*                                                                               
         L     RF,ACMLREC                                                       
         CLC   CMLKCML-CMLKEY(8,RF),WORK                                        
         JE    ERR00668                                                         
*                                                                               
* NOW READ REC                                                                  
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'       VCML SETS THIS FLAG                          
         JNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         MVC   CMLKAM(3),SVBAGM    A-M/CLT                                      
         MVC   CMLKCML,WORK        CMML FROM VCML                               
         DROP  R4                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00202                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
*                                                                               
* CHECK IF COV CMML                                                             
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         JE    ERR00662                                                         
*                                                                               
* CHECK IF CMML IS DELETED                                                      
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
         TM    CMLSTAT,X'80'                                                    
         JNZ   ERR00535                                                         
*                                                                               
* CHECK IF CMML COVERS DATE SPREAD                                              
*                                                                               
         LR    R2,R4               SAVE PTR TO ACT X'10'                        
         L     R4,ACMLREC          GET X'10' FROM COV                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CHECK IF CMML HAS SAME TYPE AS COVER                                          
*                                                                               
VID10    DS    0H                                                               
         CLC   CMLTYPE,CMLTYPE-CMLDTAEL(R2)                                     
         JNE   ERR00669                                                         
         LR    R4,R2                                                            
*                                                                               
* ADD SPOT LENGTH TO ACCUM                                                      
*                                                                               
         LLC   R1,ACTSLN                                                        
         LLC   RF,CMLSLN                                                        
         AR    R1,RF                                                            
         STC   R1,ACTSLN                                                        
         DROP  R4                                                               
*                                                                               
* CHECK IF CMML HAS PROD IN COV CMML                                            
*                                                                               
         L     R4,AIO7                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    R2,R4               SAVE ACTUAL'S X'20' EL                       
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         LLC   R1,1(R2)            OUTER LOOP COUNT                             
         SHI   R1,2                                                             
         LA    R2,2(R2)            R2 = A(1'ST PROD) IN ACTUAL                  
OUTER    IC    RF,1(R2)            INNER LOOP COUNT                             
         SHI   RF,2                                                             
         LA    RE,2(R4)            RE = A(1'ST PROD) IN COV                     
INNER    CLC   0(1,RE),0(R2)                                                    
         JE    HIT                                                              
         LA    RE,1(RE)                                                         
         JCT   RF,INNER                                                         
         LA    R2,1(R2)                                                         
         JCT   R1,OUTER                                                         
         J     ERR00664                                                         
*                                                                               
HIT      J     EXIT                                                             
*                                                                               
*                                                                               
*-----------------------------------------------                                
* ADD F1 ACTIVITY ELEMENT                                                       
*-----------------------------------------------                                
*                                                                               
AACTELEM NTR1  BASE=*,LABEL=*                                                   
         L     RF,ATWA                                                          
         USING TWAD,RF                                                          
         MVC   SVUID,TWAUSRID   SAVE IN BINARY SIGNON/ORIGIN ID                 
         DROP  RF                                                               
*                                                                               
         MVI   ELCODE,X'F1'                                                     
         L     R4,SVAREC                                                        
         BRAS  RE,GETEL                                                         
         JNE   ACTIV2                                                           
         USING ACTVD,R4                                                         
*                                                                               
         BRAS  RE,MVELEM           MOVE ELEMENT TO ELEM                         
         L     R7,SVAREC           POINT TO RECORD                              
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         BRAS  RE,REMEL            DELETE ELEMENT                               
*                                                                               
         LA    R4,ELEM                                                          
         MVC   ACTVCHDT,BTODAY                                                  
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
         AI    ACTVCHNM,1          UP THE CHANGE NUMBER                         
*                                                                               
         LA    R3,ACTVCHID                                                      
*                                                                               
         MVC   0(2,R3),SVUID      MOVE IN BINARY SIGNON/ORIGIN ID               
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    ACTIV8                                                           
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
         J     ACTIV8                                                           
*                                                                               
ACTIV2   DS    0H                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R4,ELEM                                                          
         USING ACTVD,R4                                                         
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVCHDT,BTODAY                                                  
         LA    R3,ACTVADID                                                      
*                                                                               
ACTIV6   MVC   0(2,R3),SVUID       MOVE IN ID                                   
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    ACTIV8                                                           
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
*                                                                               
ACTIV8   L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   ACTVSCID,SECPID-SECD(R1)                                         
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
*                                                                               
*                                                                               
ACTIV10  MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,SVAREC                                                        
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
*                                                                               
*                                                                               
*====================================================================           
* KEEP LAST 6 CHANGE ELEMENTS                                                   
* AIO3 HAS PREVIOUS VERSION OF RECORD                                           
* SAVE 8 PRODUCTS, START DATE, END DATE, AND SLN FROM IT                        
* ELEMENT LEN IS FIXED, SO FIELDS SAVED EVEN IF THEY DON'T CHANGE               
*====================================================================           
*                                                                               
SETCHGEL NTR1  BASE=*,LABEL=*                                                   
         OC    CHGFLAGS,CHGFLAGS    TEST ANY CHANGES                            
         JZ    EXIT                                                             
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JE    EXIT                OBVIOUSLY THIS IS NOT A CHANGE!              
*                                                                               
         XC    ELEM,ELEM                                                        
C        USING CMLCHEL,ELEM                                                     
         MVI   C.CMLCHEL,X'C0'                                                  
         MVI   C.CMLCHLEN,CMLCHELX-CMLCHEL                                      
*                                                                               
         MVC   C.CMLCHDAT,BTODAY                                                
         BRAS  RE,GETTIME                                                       
         MVC   C.CMLCHTIM,FULL                                                  
*                                                                               
         L     RF,ATWA                                                          
         USING TWAD,RF                                                          
         MVC   C.CMLCHWHO,TWAUSRID   MOVE IN BINARY SIGNON/ORIGIN ID            
         DROP  RF                                                               
*                                                                               
         GOTOR VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    SETCHG5                                                          
         MVC   C.CMLCHWHO(L'FAPASSWD),FAPASSWD YES SO USE THIS ID               
         OI    C.CMLCHFLG,X'80'    SET FLAG TO SAY THIS IS ID                   
         DROP  R1                                                               
*                                                                               
SETCHG5  DS    0H                                                               
         L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   C.CMLCHWHO,SECPID-SECD(R1)                                       
*                                                                               
SETCHG10 MVC   C.CMLCHDT1(3),CHGFLAGS MOVE CHANGE FLAGS                         
*                                                                               
         L     R4,AIO8             SAVE OLD DATA                                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
*                                                                               
         MVC   C.CMLCHSDT,CMLRLSE                                               
         MVC   C.CMLCHEDT,CMLRCL                                                
         MVC   C.CMLCHSLN,CMLSLN                                                
*                                                                               
         L     R4,AIO8                                                          
         MVI   ELCODE,X'20'        PRODUCT LIST                                 
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         CHI   RF,8                                                             
         JNH   *+8                                                              
         LHI   RF,8                                                             
         AHI   RF,-3               SET FOR EX                                   
*                                                                               
         BASR  R1,0                                                             
         MVC   C.CMLCHPRD(0),2(R4)                                              
         EX    RF,0(R1)                                                         
*                                                                               
         L     R4,AIO8              SAVE OLD DATA                               
         MVI   ELCODE,X'B0'                                                     
         BRAS  RE,GETEL                                                         
         JNE   SETCHG18                                                         
         USING CMLMATEL,R4                                                      
*                                                                               
         MVC   C.CMLCHSTM,CMLMSTIM                                              
         MVC   C.CMLCHETM,CMLMETIM                                              
*                                                                               
SETCHG18 L     R4,ACMLREC                                                       
         MVI   ELCODE,X'C0'        FIND MOST RECENT CHANGEL                     
         BRAS  RE,GETEL                                                         
         JNE   SETCHG30                                                         
         USING CMLCHEL,R4                                                       
*                                                                               
         CLC   CMLCHDAT(11),C.CMLCHDAT   TEST SAME DATE/PERSON                  
         JNE   SETCHG20                                                         
         MVC   CMLCHTIM,C.CMLCHTIM       MOVE IN LATEST TIME                    
         OC    CMLCHDT1(3),C.CMLCHDT1    'OR' IN NEW ACTIVITY                   
         OI    CMLCHDT2,CMLCH_OPC  Changed in optica                            
         J     EXIT                                                             
*                                                                               
SETCHG20 L     R4,ACMLREC          IF > 6 ELEMENTS, DELETE OLDEST               
         SR    R5,R5                                                            
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
*                                                                               
SETCHG22 BRAS  RE,NEXTEL                                                        
         JNE   SETCHG30                                                         
         AHI   R5,1                                                             
         CHI   R5,6                                                             
         JL    SETCHG22                                                         
         L     R7,ACMLREC                                                       
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         BRAS  RE,REMEL            DELETE ELEM                                  
*                                                                               
SETCHG30 L     R4,ACMLREC          ADD NEW BEFORE OTHER CHGELS                  
         BRAS  RE,GETEL               OR AT EOR                                 
         OI    CMLCHDT2,CMLCH_OPC  CHANGED IN OPTICA                            
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         L     R7,ACMLREC                                                       
         BRAS  RE,ADDEL            ADD ELEM                                     
         J     EXIT                                                             
*                                                                               
GETTIME  NTR1  BASE=*,LABEL=*                                                   
         XC    FULL,FULL                                                        
         THMS                      TIME R1=0HHMMSS+                             
         LR    R0,R1               CONVERT TO BINARY HMS                        
         SRDL  R0,12                                                            
         SRL   R1,20                                                            
         XC    DUB,DUB                                                          
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+2                                                        
         SRDL  R0,8                                                             
         SRL   R1,20                                                            
         LA    RF,X'0C'                                                         
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL+1                                                        
         SRDL  R0,28                                                            
         OR    R1,RF                                                            
         ST    R1,DUB+4                                                         
         CVB   RE,DUB                                                           
         STC   RE,FULL                                                          
         J     EXIT                                                             
*                                                                               
*                                                                               
*======================================================                         
* VALIDATE NETWORK SUB-FIELDS BEFORE PUTREC                                     
*======================================================                         
*                                                                               
PUT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         XC    IOKEY,IOKEY                                                      
*                                                                               
         L     R2,ACMLREC                                                       
         MVC   IOKEY(13),0(R2)                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO7                                                          
*                                                                               
PUT02    MVI   ELCODE,X'22'        GET CML NETWORK ELEMENT                      
         BRAS  RE,GETEL                                                         
         JNE   PUTX                                                             
*                                                                               
         USING CMLNETEL,R4                                                      
*                                                                               
*SAVE ALL "OLD" NETWORKS IN WORK                                                
         LA    R1,WORK                                                          
         MVC   WORK,SPACES                                                      
*                                                                               
         MVI   BYTE4,C'I'          PRESET FLAG TO NET INCLUDE                   
         CLI   CMLNETLN,6          OLD RECORD?                                  
         BZ    PUT02C               YES                                         
         TM    CMLFLG,CMLEXNET     EXCLUDE THIS NETWORK?                        
         BZ    PUT02C               NO                                          
         MVI   BYTE4,C'X'          SET FLAG TO NET EXCLUDE                      
*                                                                               
PUT02C   MVC   0(L'SVQNET,R1),CMLNET   SAVE NETWORK                             
         OC    0(L'SVQNET,R1),SPACES   MAKE SURE NETWORK SPACE PADDED           
         LA    R1,L'SVQNET(R1)     BUMP IN WORK                                 
         BRAS  RE,NEXTEL                                                        
         JE    PUT02C                                                           
*                                                                               
*SAVE ALL "NEW" NETWORK ENTRIES IN ELEM1                                        
         LA    R3,ELEM1                                                         
         MVC   ELEM1(L'SPACES),SPACES                                           
*                                                                               
         OC    ANETI,ANETI         NETS TO INCLUDE                              
         JZ    PUT04                                                            
*                                                                               
         CLI   BYTE4,C'I'          DID WE HAVE NETWORK INCLUDES BEFORE          
         JE    *+8                                                              
         MVI   BYTE4,C'D'          NO, LOOK FOR NETS TO DELETE                  
*                                                                               
         ICM   R1,7,ANETI          LIST OF NETWORKS                             
         USING LW_D,R1                                                          
         AHI   R1,LW_LN2Q          POINT TO THE FIRST NET IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMNET         # OF NETS                                    
*                                                                               
PUT02F   MVC   0(L'SVQNET,R3),0(R1) SAVE NET IN ELEM1                           
         LA    R3,L'SVQNET(R3)     BUMP IN ELEM1                                
         LA    R1,L'SVQNET(R1)     BUMP TO NEXT NETWORK                         
         JCT   R0,PUT02F                                                        
*                                                                               
PUT04    OC    ANETX,ANETX         NETS TO EXCLUDE                              
         JZ    PUT06                                                            
*                                                                               
         CLI   BYTE4,C'X'          DID WE HAVE NETS TO EXCLUDE BEFORE           
         JE    *+8                                                              
         MVI   BYTE4,C'D'          NO, LOOK FOR NETS TO DELETE                  
*                                                                               
         ICM   R1,7,ANETX          LIST OF NETWORKS                             
         USING LW_D,R1                                                          
         AHI   R1,LW_LN2Q          POINT TO THE FIRST NET IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMNET         # OF NETS                                    
*                                                                               
PUT04F   MVC   0(L'SVQNET,R3),0(R1) SAVE NET IN ELEM1                           
         LA    R3,L'SVQNET(R3)     BUMP IN ELEM1                                
         LA    R1,L'SVQNET(R1)     BUMP TO NEXT NETWORK                         
         JCT   R0,PUT04F                                                        
*                                                                               
PUT06    LA    R3,ELEM2                                                         
         MVC   ELEM2(L'SPACES),SPACES                                           
*                                                                               
         OC    ANETD,ANETD         NETS TO DELETE                               
         JZ    PUT07                                                            
         ICM   R1,7,ANETD          LIST OF NETWORKS                             
         USING LW_D,R1                                                          
         AHI   R1,LW_LN2Q          POINT TO THE FIRST NET IN LIST               
         SR    R0,R0                                                            
         ICM   R0,3,NUMNETD        # OF NETS TO DELETE                          
*                                                                               
PUT06F   MVC   0(L'SVQNET,R3),0(R1) SAVE NET IN ELEM1                           
         LA    R3,L'SVQNET(R3)     BUMP IN ELEM1                                
         LA    R1,L'SVQNET(R1)     BUMP TO NEXT NETWORK                         
         JCT   R0,PUT06F                                                        
*                                                                               
*CHECK THAT ALL NETS ARE ACCOUNTED FOR                                          
PUT07    LA    R1,WORK             POINT TO "OLD" NETS                          
         LA    R3,ELEM1            "NEW" NETS TO ADD                            
PUT07C   CLI   BYTE4,C'D'          LOOK FOR NETS TO DELETE?                     
         JNE   PUT08                                                            
         LA    R3,ELEM2            NETS TO DELETE                               
         CLI   0(R3),X'40'                                                      
         JE    ERR00698                                                         
*                                                                               
PUT08    CLC   0(L'SVQNET,R1),0(R3)                                             
         JE    PUT10                                                            
         LA    R3,L'SVQNET(R3)                                                  
         CLI   0(R3),X'40'                                                      
         JH    PUT08                                                            
         CLI   BYTE4,C'D'          LOOK FOR NETS TO DELETE?                     
         JE    ERR00698                                                         
         LA    R3,ELEM2            NETS TO DELETE                               
         CLI   0(R3),X'40'                                                      
         JE    ERR00698                                                         
PUT08C   CLC   0(L'SVQNET,R1),0(R3)                                             
         JE    PUT10                                                            
         LA    R3,L'SVQNET(R3)                                                  
         CLI   0(R3),X'40'                                                      
         JH    PUT08C                                                           
         JE    ERR00698                                                         
*                                                                               
* CHECK NEXT OLD NET                                                            
PUT10    LA    R1,L'SVQNET(R1)     BUMP TO NEXT OLD NET IN WORK                 
         LA    R3,ELEM1            START OF NEW NETS LIST                       
         CLI   0(R1),X'40'         DONE?                                        
         JH    PUT07C                                                           
PUTX     J     EXITY               YES                                          
*                                                                               
* THIS ROUTINE USES R3 TO BUMP THROUGH ELEMENTS                                 
GETELR3  AH    R3,DATADISP                                                      
*                                                                               
FRSTELR3 CLI   0(R3),0                                                          
         JNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLC   ELCODE,0(R3)                                                     
         BER   RE                                                               
NEXTELR3 SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         JNZ   *+10                                                             
         CLI   1(R3),1                                                          
         BR    RE                                                               
         AR    R3,RF                                                            
         J     FRSTELR3                                                         
*                                                                               
*                                                                               
*===============================================================                
* GET NEXT SEQUENCE NUMBER FROM MASTER RECORD FOR ADDS                          
*===============================================================                
*                                                                               
PAREC    NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,IOKEY                                                      
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),SVBAGM                                                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV  IF CMML SEQ REC FOUND                        
         JE    PAR10               GO SAVE CMML SEQ NUMBER                      
         DROP  R4                                                               
*                                                                               
* NOW MUST ADD CMML SEQ REC FOR AGENCY/MEDIA/CLT-ONCE FOR EACH A/M/CLT          
*                                                                               
         MVC   IOKEY(13),IOKEYSAV     RESTORE KEY                               
         L     R4,AIO7                                                          
         USING CMLKEY,R4                                                        
         XC    CMLRECD(256),CMLRECD                                             
         MVC   CMLKEY,IOKEY                                                     
         MVC   CMLAGYA,LP_AGY      SET AGENCY ALPHA ID                          
         XC    ELEM+2(CMLDTAX-CMLDTAEL),ELEM+2                                  
         LA    R4,ELEM                                                          
         USING CMLDTAEL,R4                                                      
         MVI   CMLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   CMLDTALN,CMLDTAX-CMLDTAEL                                        
         MVI   CMLSEQ+2,X'01'      START SEQ NUMBER ONE IN MASTER               
         MVC   HOLDSEQ,CMLSEQ      START SEQ NUMBER ONE IN DSECT                
         MVC   CMLTITLE,=CL15'CMML SEQ RECORD'                                  
*                                                                               
         L     R7,AIO7                                                          
         MVC   SYSFIL,=CL8'SPTFIL'                                              
         BRAS  RE,ADDEL            ADD CMML DATA ELEMENT                        
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOSPTFIL+IO7'                        
         J     PAR20                                                            
*                                                                               
PAR10    GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO7'                           
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
         MVC   HOLDSEQ,CMLSEQ                                                   
*                                                                               
PAR20    MVC   IOKEY(L'SVKEY),SVKEY  RESTORE KEY                                
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'10'        NOW GET DATA ELEM                            
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R4                                                      
         MVC   CMLSEQ,HOLDSEQ      AND PUT COMMERCIAL SEQ # IN IT               
         J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
*================================================================               
* AFTER ADDED RECORD - ADD PASSIVE KEYS, UPDATE SEQUENCE NUMBER                 
*================================================================               
*                                                                               
AAREC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVDSKAD,IODA        SAVE DISK ADDR OF ADDED REC                  
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING CMLKEY,R5                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),SVBAGM                                                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOSPTDIR+IO7'                          
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                JUST ADDED RECORD, MUST BE THERE             
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO7'                        
         L     R4,AIO7                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R4                                                      
         SR    R1,R1                                                            
         ICM   R1,7,CMLSEQ         GET SEQ                                      
         LA    R1,1(,R1)                   AND ADD 1                            
         STCM  R1,7,CMLSEQ                           FOR ADDED REC              
*                                                                               
         L     RE,AIO7                                                          
         ST    RE,SVAREC                                                        
         BRAS  RE,AACTELEM         ADD ACTIVITY ELEMENT TO REC IN IO7           
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO7'                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CMLKID(2),=X'0AA1'                                               
         MVC   CMLKAM(3),SVBAGM                                                 
         MVC   CMLKCML(3),HOLDSEQ                                               
         XC    CMLKCML+3(5),CMLKCML+3                                           
         CLI   ADIDFLAG,C'Y'                                                    
         JNE   *+8                                                              
         OI    CMLKSTAT,CMLKSTA_PCKD                                            
         MVC   IOKEY+14(4),SVDSKAD   MOVE IN SAVED DISK ADDR                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOSPTDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CREATE ADID POINTER FOR 8-12 CHAR CMML                                        
*                                                                               
         MVC   CMLKID(2),=X'0AC1'                                               
         MVC   CMLKAM(3),SVBAGM & BCLT                                          
         MVC   WORK(12),SPACES                                                  
         L     R4,ACMLREC                                                       
         MVC   CMLKCML,5(R4)       PRESET CML IN KEY                            
         TM    15(R4),X'01'        PACKED CML IN KEY                            
         JO    AAR02               YES, KEY IS SET                              
*                                                                               
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),5(R4)       PRESET CML IN KEY                            
         GOTOR VTRPACK,DMCB,(C'P',WORK),CMLKCML                                 
         JNE   AAR05                                                            
*                                                                               
AAR02    GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOSPTDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AAR05    L     R4,ACMLREC                                                       
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
AAR26    BRAS  RE,NEXTEL                                                        
         JNE   AAR30                                                            
         LA    R1,2(R4)                                                         
         LA    R0,1                                                             
*                                                                               
         USING CMLACTEL,R4                                                      
         MVI   ADIDFLAG,C'N'                                                    
         CLI   CMLACTLN,CMLACTL1   OLD ELEM                                     
         JE    AAR28                                                            
         CLI   8(R1),C' '          TEST ISCI                                    
         JNH   AAR28               YES                                          
         GOTOR VTRPACK,DMCB,(C'P',2(R4)),DUB   PACK 12 CHAR ADID                
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                         AND POINT TO IT                   
*                                                                               
AAR28    BRAS  RE,UPDACT                                                        
         J     AAR26                                                            
*                                                                               
AAR30    DS    0H                                                               
         XC    IOKEY,IOKEY                                                      
         OC    SVHDDELK,SVHDDELK                                                
         JZ    AAR35                                                            
         CLC   SVHDDELK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDDELK),SVHDDELK                                       
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
*                                                                               
AAR35    XC    IOKEY,IOKEY                                                      
         OC    SVHDADDK,SVHDADDK                                                
         JZ    AARX                                                             
         CLC   SVHDADDK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDADDK),SVHDADDK                                       
         MVC   IOKEY+14(4),SVDSKAD   MOVE IN SAVED DISK ADDR                    
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
*                                                                               
AARX     J     EXIT                                                             
         DROP  R4,R5,RB                                                         
*                                                                               
*                                                                               
*====================================================================           
* AFTER RE-WRITING RECORD - CHECK IF ACTUAL CMMLS NEED REWRITING                
*====================================================================           
*                                                                               
ACH      NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,1                                                             
         TM    FLAGS,SOFTDEL                                                    
         JZ    *+12                                                             
         LHI   R0,-1                                                            
         J     *+12                                                             
         TM    FLAGS,SOFTREST                                                   
         JZ    ACH20                                                            
*                                                                               
         L     R4,ACMLREC                                                       
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
*                                                                               
ACH10    BRAS  RE,NEXTEL                                                        
         JNE   ACH30                                                            
         LA    R3,2(R4)                                                         
         LA    R0,1                                                             
         LR    R1,R3               POINT TO CMML                                
*                                                                               
         USING CMLACTEL,R4                                                      
         MVI   ADIDFLAG,C'N'                                                    
         CLI   CMLACTLN,CMLACTL1   OLD ELEM                                     
         JE    ACH15                                                            
         CLI   8(R3),C' '          TEST ISCI                                    
         JNH   ACH15                                                            
*                                                                               
         OC    0(12,R3),SPACES                                                  
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         JNE   ERR00783                                                         
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
*                                                                               
ACH15    BRAS  RE,UPDACT                                                        
         J     ACH10                                                            
*                                                                               
ACH20    BRAS  RE,NPUT                                                          
*                                                                               
ACH30    DS    0H                                                               
         MVC   SVDSKAD,SVDSKADR                                                 
         XC    IOKEY,IOKEY                                                      
         OC    SVHDDELK,SVHDDELK                                                
         JZ    ACH40                                                            
         CLC   SVHDDELK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDDELK),SVHDDELK                                       
         BRAS  RE,DELPSSV                                                       
         XC    SVHDDELK,SVHDDELK                                                
*                                                                               
ACH40    XC    IOKEY,IOKEY                                                      
         OC    SVHDADDK,SVHDADDK                                                
         JZ    ACHX                                                             
         CLC   SVHDADDK(2),=X'0AC2'                                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'SVHDADDK),SVHDADDK                                       
         BRAS  RE,ADDPSSV                                                       
         XC    SVHDADDK,SVHDADDK                                                
*                                                                               
ACHX     J     EXIT                                                             
*                                                                               
*                                                                               
*====================================================================           
* UPDATE CMMLS IF NEEDED - CHANGE WAS OTHER THAN DELETE OR RESTORE              
*====================================================================           
*                                                                               
NPUT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* FOR ANY ENTRY THAT IS IN OLDACTS AND NOT IN NEWACTS, GOTO UPDACT              
* WITH R3=A(CMML) AND R0=H'-1'                                                  
*                                                                               
         LHI   R0,-1               SET FLAG FOR UPDACT                          
         LA    R2,NUMACTS                                                       
         LA    R3,OLDACTS                                                       
         OC    0(12,R3),0(R3)                                                   
         JZ    NP20                                                             
*                                                                               
NP01     LA    R4,NUMACTS                                                       
         LA    R5,NEWACTS                                                       
*                                                                               
NP02     CLC   0(12,R3),0(R5)                                                   
         JE    NP10                                                             
         LA    R5,12(R5)                                                        
         JCT   R4,NP02                                                          
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         LR    R1,R3               POINT TO CMML                                
         CLI   8(R3),C' '          TEST ISCI                                    
         JNH   NP05                MUST BE 8 CHAR ISCII                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
*                                                                               
NP05     BRAS  RE,UPDACT                                                        
*                                                                               
NP10     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         JZ    NP20                                                             
         JCT   R2,NP01                                                          
*                                                                               
* FOR ANY ENTRY THAT IS IN NEWACTS AND NOT IN OLDACTS, GOTO UPDACT              
* WITH R3=A(CMML) AND R0=H'1'                                                   
*                                                                               
NP20     DS    0H                                                               
         TM    FLAGS,UNCOVER                                                    
         JNZ   NPX                 NO NEWACTS                                   
         LA    R0,1                                                             
         LA    R2,NUMACTS                                                       
         LA    R3,NEWACTS                                                       
         OC    0(12,R3),0(R3)                                                   
         JZ    NPX                                                              
*                                                                               
NP21     LA    R4,NUMACTS                                                       
         LA    R5,OLDACTS                                                       
*                                                                               
NP24     CLC   0(12,R3),0(R5)                                                   
         JE    NP30                                                             
         LA    R5,12(R5)                                                        
         JCT   R4,NP24                                                          
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         LR    R1,R3               POINT TO CMML                                
         CLI   8(R3),C' '          TEST ISCI                                    
         JNH   NP26                MUST BE 8 CHAR ISCII                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),DUB  NO, PACK 12 CHAR ADID             
         MVI   ADIDFLAG,C'Y'                                                    
         LA    R1,DUB                     POINT TO PACKED ADID                  
*                                                                               
NP26     BRAS  RE,UPDACT                                                        
*                                                                               
NP30     LA    R3,12(R3)                                                        
         OC    0(12,R3),0(R3)                                                   
         JZ    NPX                                                              
         JCT   R2,NP21                                                          
NPX      J     EXIT                                                             
*                                                                               
*                                                                               
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF/CENTER CUT FIELD ABOUT TO BE ADDED                     
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
* ASSUME KEY IS SET TO THE KEY TO BE ADDED OR WRITTEN                           
*----------------------------------------------------------------*              
DELPSSV  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOSPTDIR+IO7'                           
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   EXITY                                                            
*                                                                               
         OI    IOKEY+13,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IO7'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXITY                                                            
*                                                                               
*                                                                               
*----------------------------------------------------------------*              
* VALIDATE DATA IN HIDEF FIELD ABOUT TO BE ADDED                                
* IS NOT A DUPLICATE - IF NOT ADD THE PASSIVE POINTER                           
* ASSUME KEY IS SET TO THE KEY TO BE ADDED OR WRITTEN                           
*----------------------------------------------------------------*              
ADDPSSV  NTR1  BASE=*,LABEL=*                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOSPTDIR+IO7'                         
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ADDPSS                                                           
*                                                                               
WRTPSS   DS    0H                                                               
         NI    IOKEY+13,X'FF'-X'80'                                             
         MVC   IOKEY+14(4),SVDSKADR                                             
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   *+10                                                             
         MVC   IOKEY+14(4),SVDSKAD   USE THIS FOR ACTION ADD                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IO7'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
*                                                                               
ADDPSS   DS    0H                                                               
         MVC   IOKEY(13),IOKEYSAV  RESTORE KEY                                  
         MVC   IOKEY+14(4),SVDSKADR                                             
         CLI   RECACT,C'A'         IF ACTION ADD                                
         JNE   *+10                                                             
         MVC   IOKEY+14(4),SVDSKAD   USE THIS FOR ACTION ADD                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOSPTDIR'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
* EXPECTS:                                                                      
*  R0 = H'1' FOR INC, H'-1' FOR DEC                                             
*  R1 = A(CMML ID)                                                              
*                                                                               
*                                                                               
UPDACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'       TEST TO READ ADID                            
         JNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         MVC   CMLKAM(3),SVBAGM                                                 
         MVC   CMLKCML,0(R1)                                                    
         DROP  R4                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOSPTDIR+IO7'                          
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO7'                        
*                                                                               
         L     R4,AIO7                                                          
         MVC   BYTE,ELCODE         SAVE ELCODE                                  
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELCODE,BYTE         RESTORE ELCODE                               
*                                                                               
         USING CMLDTAEL,R4                                                      
         SR    RE,RE                                                            
         ICM   RE,3,CMLCOVCT                                                    
         AR    RE,R0                                                            
         STCM  RE,3,CMLCOVCT                                                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOSPTFIL+IO7'                        
UAXIT    J     EXIT                                                             
*                                                                               
*                                                                               
*====================================================================           
* VALIDATE 8 CHARACTER COMMERCIAL CODE                                          
* FIRST 4 CHARS MUST BE ALPHA, 5TH NUMERIC OR -, LAST 3 NUMERIC *               
* ON EXIT, 8 CHARACTER CMNL WILL BE IN WORK!                                    
* (R2 POINTING TO COMML FIELD )                                                 
*====================================================================           
*                                                                               
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVI   ADIDFLAG,C'N'       ASSUME CML IS ISCII                          
*                                                                               
         CLI   8(R2),X'40'         MUST BE AT LEAST 8 CHARS                     
         JH    VCML50              MORE IS ADID                                 
*                                                                               
         CLI   RECACT,C'A'         IF ADD FULLY EDIT CML                        
         JNE   VCML10                                                           
         CLI   8(R2),X'40'         MUST BE LEN OF 8                             
         JNE   ERR00783            MUST BE 8 CHAR ISCII                         
*                                                                               
VCML10   LA    R0,4                                                             
         LR    R1,R2                                                            
VCML20   CLI   0(R1),C'A'                                                       
         JL    ERR00734            MUST BE ALPHA NUMERIC                        
         CLI   0(R1),C'Z'                                                       
         JH    ERR00734                                                         
         LA    R1,1(R1)                                                         
         JCT   R0,VCML20                                                        
*                                                                               
         LA    R0,4                                                             
         CLI   0(R1),C'-'                                                       
         JE    VCML34                                                           
VCML30   CLI   0(R1),C'0'                                                       
         JL    ERR00734            MUST BE ALPHA NUMERIC                        
         CLI   0(R1),C'9'                                                       
         JH    ERR00734                                                         
VCML34   LA    R1,1(R1)                                                         
         JCT   R0,VCML30                                                        
*                                                                               
         MVC   WORK(L'QCOMML),0(R2)                                             
         J     VCMLX                                                            
*                                                                               
*=================================================================              
* See if this is an ad-id code                                                  
*=================================================================              
*                                                                               
VCML50   DS    0H                                                               
         GOTOR VTRPACK,DMCB,(C'P',0(R2)),WORK                                   
         JNE   ERR00722                                                         
*                                                                               
         MVI   ADIDFLAG,C'Y'       SET CMML IS ADID                             
         CLI   RECACT,C'A'         IF ADD                                       
         JE    VCMLX                                                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),=X'0AC1'                                                
         MVC   IOKEY+2(3),SVBAGM     A-M/CLT                                    
         MVC   IOKEY+5(8),WORK                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO7'                            
         CLC   IOKEY(13),IOKEYSAV                                               
         JNE   ERR00202                                                         
*                                                                               
VCMLX    J     EXIT                                                             
*                                                                               
*                                                                               
*-----------------------------------------                                      
* ISSUE CUSTOMIZED ERROR MESSAGE                                                
*-----------------------------------------                                      
*                                                                               
ERR      XC    ERRORMSG,ERRORMSG                                                
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   ERRORMSG(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         BRAS  RE,PUTMYERR                                                      
         J     EXITN                                                            
*                                                                               
*                                                                               
ERR00020 GOTOR PUTERR,DMCB,('SE#SY002',SE#INDTE) INVALID DATE FORMAT            
         J     EXITN                                                            
ERR00243 GOTOR PUTERR,DMCB,('SE#SY002',243)  SPOT LEN MISSING                   
         J     EXITN                                                            
ERR00258 GOTOR PUTERR,DMCB,('SE#SY002',SE#MSGMD)  MEDIA MISSING                 
         J     EXITN                                                            
ERR00260 GOTOR PUTERR,DMCB,('SE#SY002',260)  PRODUCT MISSING                    
         J     EXITN                                                            
ERR01023 GOTOR PUTERR,DMCB,('SE#SY002',SE#INVMD)  INVALID MEDIA                 
         J     EXITN                                                            
ERR00966 GOTOR PUTERR,DMCB,('SE#SY002',SE#MSGCL)  MISSING CLIENT                
         J     EXITN                                                            
ERR00014 GOTOR PUTERR,DMCB,('SE#SY002',SE#INCLT)  INVALID CLIENT                
         J     EXITN                                                            
ERR00971 GOTOR PUTERR,DMCB,('SE#SY002',SE#RECNF)  RECORD NOT FOUND              
         J     EXITN                                                            
ERR00049 GOTOR PUTERR,DMCB,('SE#SY002',SE#RCXST)  REC ALREADY EXISTS            
         J     EXITN                                                            
ERR00053 GOTOR PUTERR,DMCB,('SE#SY002',53)  RECORD NOT FOUND                    
         J     EXITN                                                            
ERR00485 GOTOR PUTERR,DMCB,('SE#SY002',485) TOO MANY PRODUCTS                   
         J     EXITN                                                            
ERR00537 GOTOR PUTERR,DMCB,('SE#SY002',537)  1ST CHAR MUST BE ALPHA             
         J     EXITN                                                            
ERR00002 GOTOR PUTERR,DMCB,('SE#SY002',SE#ININP)  INVALID INPUT FIELD           
         J     EXITN                                                            
ERR00001 GOTOR PUTERR,DMCB,('SE#SY002',1) MISSING DATA                          
         J     EXITN                                                            
ERR00003 GOTOR PUTERR,DMCB,('SE#SY002',SE#NONUM) NOT VALID NUMERIC DATA         
         J     EXITN                                                            
********************                                                            
ERR00073 GOTOR PUTERR,DMCB,('SE#SY003',73) INVALID PRODUCT                      
         J     EXITN                                                            
ERR00503 GOTOR PUTERR,DMCB,('SE#SY003',503) NETWORK NOT FOUND                   
         J     EXITN                                                            
ERR00698 GOTOR PUTERR,DMCB,('SE#SY013',698) NETWORK CAN'T BE CHANGED            
         J     EXITN                                                            
********************                                                            
ERR00573 GOTOR PUTERR,DMCB,('SE#SY013',573) DESC 2 REQ FOR DESC 3               
         J     EXITN                                                            
ERR00673 GOTOR PUTERR,DMCB,('SE#SY002',673) SPOT LENGTH NOT VALID               
         J     EXITN                                                            
ERR00102 GOTOR PUTERR,DMCB,('SE#SY002',102) PRODUCT CANT BE POL                 
         J     EXITN                                                            
ERR00202 GOTOR PUTERR,DMCB,('SE#SY022',SE#NOCML) COMMERCIAL NOT FOUND           
         J     EXITN                                                            
*                                                                               
ERR00535 GOTOR PUTERR,DMCB,('SE#SY013',SE#CMLDL) DELETED CML                    
         J     EXITN                                                            
ERR00574 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDOVR) OVRRID FORAMT IS NN/NN         
         J     EXITN                                                            
ERR00575 GOTOR PUTERR,DMCB,('SE#SY013',SE#INVOV) OVRD MUST= LEN                 
         J     EXITN                                                            
ERR00577 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDCML) CML 4 ALHPA/4 NUM              
         J     EXITN                                                            
ERR00579 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDCLS) CLASS MUST BE 1-4 CHAR         
         J     EXITN                                                            
ERR00580 GOTOR PUTERR,DMCB,('SE#SY013',SE#BDSOL) BLANK/P/S                      
         J     EXITN                                                            
ERR00581 GOTOR PUTERR,DMCB,('SE#SY013',SE#NORES) CAN'T RESTORE DEL              
         J     EXITN                                                            
ERR00632 GOTOR PUTERR,DMCB,('SE#SY013',632) UFN INVALID                         
         J     EXITN                                                            
ERR00661 GOTOR PUTERR,DMCB,('SE#SY013',SE#CCOVR) CML USED BY COVER              
         J     EXITN                                                            
ERR00662 GOTOR PUTERR,DMCB,('SE#SY013',SE#CISCV) CML IS COV CML                 
         J     EXITN                                                            
ERR00664 GOTOR PUTERR,DMCB,('SE#SY013',SE#NPRDM) PRDS DO NOT MATCH              
         J     EXITN                                                            
ERR00665 GOTOR PUTERR,DMCB,('SE#SY013',SE#COV2) MUST BE 2+ ACT CML              
         J     EXITN                                                            
ERR00666 GOTOR PUTERR,DMCB,('SE#SY013',SE#SLNEQ) ACT SLN <> COV SLN             
         J     EXITN                                                            
ERR00667 GOTOR PUTERR,DMCB,('SE#SY013',SE#NOCOV) COVD CML CANT BE               
         J     EXITN                                                            
ERR00668 GOTOR PUTERR,DMCB,('SE#SY013',SE#SMCML) CML CANT COV ITSELF            
         J     EXITN                                                            
ERR00669 GOTOR PUTERR,DMCB,('SE#SY013',SE#TYPER) CML TYPE <>                    
         J     EXITN                                                            
ERR00681 GOTOR PUTERR,DMCB,('SE#SY013',681) MAX ENTRIES IN MKT/STA              
         J     EXITN                                                            
ERR00722 GOTOR PUTERR,DMCB,('SE#SY013',SE#ADIDX)  INVALID ADID                  
         J     EXITN                                                            
ERR00734 GOTOR PUTERR,DMCB,('SE#SY013',SE#ANCML) ALPHA NUMERIC CML ONLY         
         J     EXITN                                                            
ERR00742 GOTOR PUTERR,DMCB,('SE#SY013',SE#YNTAL) TAL TRNSFR Y/N                 
         J     EXITN                                                            
ERR00746 GOTOR PUTERR,DMCB,('SE#SY013',SE#INPER) INVALID PERIOD                 
         J     EXITN                                                            
ERR00747 GOTOR PUTERR,DMCB,('SE#SY013',SE#INTME) INVALID TIME                   
         J     EXITN                                                            
ERR00749 GOTOR PUTERR,DMCB,('SE#SY013',SE#ONEPR) ONE PRD PER CML                
         J     EXITN                                                            
ERR00756 GOTOR PUTERR,DMCB,('SE#SY013',SE#DUPHI) DUP HIDEF CML                  
         J     EXITN                                                            
ERR00765 GOTOR PUTERR,DMCB,('SE#SY013',765) CML S/B 9-12 CHAR                   
         J     EXITN                                                            
ERR00783 GOTOR PUTERR,DMCB,('SE#SY013',783) NOT 8 OR 9-12 ADID                  
         J     EXITN                                                            
ERR00814 GOTOR PUTERR,DMCB,('SE#SY013',814) CML CHKSUM VALIDATION ERR           
         J     EXITN                                                            
ERR00815 GOTOR PUTERR,DMCB,('SE#SY013',815) H IS THE ONLY VALID FORMAT          
         J     EXITN                                                            
*                                                                               
*                                                                               
*======================================================================         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                                     
*======================================================================         
*                                                                               
PUTERR   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLG,1            SET ERROR FLAG ON                            
         MVC   WORK(2),2(R1)                                                    
*                                                                               
         L     RF,ALIOB                                                         
         USING LIOBD,RF                                                         
         MVC   LIOBMSYS,0(R1)      GET ERROR MESSAGE FROM SYSTEM                
         DROP  RF                                                               
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',1)                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',10),          +        
               ('LD_CHARQ',QCLTA),(L'QCLTA,0)                                   
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',D#UPLERR),    +        
               WORK,0                                                           
*                                                                               
PUTERRX  J     EXITY                                                            
*                                                                               
* PUT OUT MY ERROR MESSAGE                                                      
*                                                                               
PUTMYERR NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLG,1            SET ERROR FLAG ON                            
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',1)                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',10),          +        
               ('LD_CHARQ',QCLTA),(L'QCLTA,0)                                   
         GOTOR ALINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#UPLERR),    +        
               ('LD_CHARQ',ERRORMSG),(L'ERRORMSG,0)                             
         J     EXITY                                                            
*                                                                               
D#UPLERR EQU   255                 UPLOAD ERROR TEXT                            
*                                                                               
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG                                                                  
*                                                                               
EXITN    LHI   RE,0                Set condition code to not equal              
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set condition code to equal                  
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                   General exit point                           
         EJECT                                                                  
*                                                                               
LVALUES  DS    0F                                                               
*                                                                               
RECTAB   DS    0XL(RECTABL)                ** RECORD TABLE **                   
         DC    AL2(M#NECMLU),AL3(CMLUPLD),X'FF'      X'0500'                    
RECTABX  EQU   *                                                                
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
PZERO    DC    P'0'                                                             
                                                                                
DMKER    DC    C'DMKEY   '                                                      
DMCOMMIT DC    C'COMMIT  '                                                      
STAFIL   DC    C'STATION'                                                       
                                                                                
FILES    DS    0X                  ** File info **                              
         DC    C'SPOT   '          System name for open                         
                                                                                
         DC    C'N'                                                             
SPTDIR   DC    C'SPTDIR '                                                       
         DC    C'N'                                                             
SPTFIL   DC    C'SPTFILE'                                                       
         DC    C'N'                                                             
XSPDIR   DC    C'XSPDIR '                                                       
         DC    C'N'                                                             
XSPFIL   DC    C'XSPFIL '                                                       
         DC    C'U'                                                             
UNTDIR   DC    C'UNTDIR '                                                       
         DC    C'U'                                                             
UNTFIL   DC    C'UNTFIL '                                                       
         DC    C'N'                                                             
         DC    C'STAFILE'                                                       
         DC    C'U'                                                             
         DC    C'RECV   '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
                                                                                
RECACTN  DC    C'Action'                                                        
*                                                                               
*COMMERCIAL FIELD NAMES                                                         
*                                                                               
TCMLBRND DC    C'Brand(s)'                                                      
TCMLFMT  DC    C'Format'                                                        
TCMLTTL  DC    C'Title'                                                         
TCMLTTL2 DC    C'Title 2'                                                       
TCMLTTL3 DC    C'Title 3'                                                       
TCMLPBSO DC    C'Piggyback/Solo'                                                
TCMLPRNT DC    C'Parent commercial'                                             
TCMLFDTE DC    C'First air date'                                                
TCMLLDTE DC    C'Last air date'                                                 
TCMLSTME DC    C'Air date start time'                                           
TCMLETME DC    C'Air date end time'                                             
TCMLDDTE DC    C'Destroy date'                                                  
TCMLDTME DC    C'Destroy time'                                                  
TCMLLEN  DC    C'Total length'                                                  
TCMLOVRD DC    C'Override'                                                      
TCMLTDLY DC    C'Time is daily'                                                 
TCMLMDTE DC    C'Matching dates'                                                
TCMLCLTN DC    C'CLT#'                                                          
TCMLCLAR DC    C'Exclude from Clarus'                                           
TCMLACML DC    C'Actual commercials'                                            
TCMLCLAS DC    C'Class'                                                         
TCMLNETI DC    C'Networks include'                                              
TCMLNETD DC    C'Networks delete'                                               
TCMLNETX DC    C'Networks exclude'                                              
TCKSM    DC    C'Record check sum'                                              
TCMLTEXT DC    C'Text present Y/N'                                              
*                                                                               
*                                                                               
*------------------                                                             
* ERRROR MESSAGES                                                               
*------------------                                                             
*                                                                               
NETINEX  DC    C'ENTER EITHER INCLUDED OR EXCLUDED NETWORKS'                    
*                                                                               
*                                                                               
***********************************************************************         
* KEY - NET TRAFFIC COMMERCIAL RECORD                                           
***********************************************************************         
*                                                                               
CMLHEAD  LKREQ H,M#NECMLU,0,NEWREC=Y         0500                               
RECACT   LKREQ F,100,(D,B#SAVED,RECACT),CHAR,TEXT=(*,RECACTN),COL=*             
MEDIA    LKREQ F,1,(I,B#SAVED,I$CAM),CHAR,                             +        
               MAXLEN=1,OLEN=1,TEXT=NE#MED,COL=*                                
CLIENT   LKREQ F,2,(I,B#SAVED,I$CCLT),VSTR,                            +        
               MAXLEN=L'QCLTA,OLEN=L'QCLTA,TEXT=NE#CLI,COL=*                    
COMML    LKREQ F,3,(D,B#SAVED,QCOMML),CHAR,TEXT=NE#CMML,COL=*                   
*                                                                               
FORMAT   LKREQ F,20,(D,B#SAVED,QFORMAT),CHAR,TEXT=(*,TCMLFMT),COL=*             
CMLPRODS LKREQ F,21,(I,B#SAVED,QPRDIND),CHAR,OLEN=QPRDENT,SORT=N,      +        
               LIST=(F,NOD),MAXLEN=QPRDENT,TEXT=(*,TCMLBRND),COL=*              
CMLTITLE LKREQ F,22,(D,B#SAVED,QCMLTTLE),CHAR,TEXT=(*,TCMLTTL),COL=*            
CMLDSC2  LKREQ F,23,(D,B#SAVED,QCMLDSC2),CHAR,TEXT=(*,TCMLTTL2),COL=*           
CMLDSC3  LKREQ F,24,(D,B#SAVED,QCMLDSC3),CHAR,TEXT=(*,TCMLTTL3),COL=*           
CMLSOLO  LKREQ F,25,(D,B#SAVED,QCMLSOLO),CHAR,TEXT=(*,TCMLPBSO),COL=*           
CMLXPRNT LKREQ F,26,(D,B#SAVED,QCMLPRNT),CHAR,TEXT=(*,TCMLPRNT),COL=*           
CMLRLSE  LKREQ F,27,(D,B#SAVED,QCMLRLSE),BDAT,TEXT=(*,TCMLFDTE),COL=*           
CMLRCL   LKREQ F,28,(D,B#SAVED,QCMLRCL),CHAR,TEXT=(*,TCMLLDTE),COL=*            
CMLMSTIM LKREQ F,29,(D,B#SAVED,QCMLMSTM),CHAR,TEXT=(*,TCMLSTME),COL=*           
CMLMETIM LKREQ F,30,(D,B#SAVED,QCMLMETM),CHAR,TEXT=(*,TCMLETME),COL=*           
CMLXDSDT LKREQ F,31,(D,B#SAVED,QCMLDSDT),BDAT,TEXT=(*,TCMLDDTE),COL=*           
CMLXDSTM LKREQ F,32,(D,B#SAVED,QCMLDSTM),CHAR,TEXT=(*,TCMLDTME),COL=*           
CMLSLN   LKREQ F,33,(D,B#SAVED,QCMLSLN),LBIN,TEXT=(*,TCMLLEN),COL=*             
CMLOVRD  LKREQ F,34,(D,B#SAVED,QCMLOVRD),CHAR,TEXT=(*,TCMLOVRD),COL=*           
CMLMFLAG LKREQ F,35,(D,B#SAVED,QDLYFLG),CHAR,TEXT=(*,TCMLTDLY),COL=*            
CMLMSDT1 LKREQ F,36,(I,B#SAVED,QMDTIND),CHAR,OLEN=17,LIST=(F,NOD),     +        
               MAXLEN=17,TEXT=(*,TCMLMDTE),SORT=N,COL=*                         
CMLNETI  LKREQ F,37,(I,B#SAVED,QNETIIND),CHAR,OLEN=4,LIST=(F,NOD),     +        
               MAXLEN=4,TEXT=(*,TCMLNETI),COL=*                                 
CMLNETX  LKREQ F,38,(I,B#SAVED,QNETXIND),CHAR,OLEN=4,LIST=(F,NOD),     +        
               MAXLEN=4,TEXT=(*,TCMLNETX),COL=*                                 
CMLNETD  LKREQ F,39,(I,B#SAVED,QNETDIND),CHAR,OLEN=4,LIST=(F,NOD),     +        
               MAXLEN=4,TEXT=(*,TCMLNETD),COL=*                                 
CMLCLTNO LKREQ F,40,(D,B#SAVED,QCMLCLTN),CHAR,TEXT=(*,TCMLCLTN),COL=*           
CMLTALEX LKREQ F,41,(D,B#SAVED,QCMLTALX),CHAR,TEXT=(*,TCMLCLAR),COL=*           
CMLACTUL LKREQ F,42,(I,B#SAVED,QACTIND),CHAR,OLEN=12,LIST=(F,NOD),     +        
               MAXLEN=12,TEXT=(*,TCMLACML),SORT=N,COL=*                         
CMLCLAS  LKREQ F,43,(D,B#SAVED,QCMLCLAS),CHAR,TEXT=(*,TCMLCLAS),COL=*           
CKSUM    LKREQ F,50,(D,B#SAVED,QCKSM),HEXD,TEXT=(*,TCKSM),COL=*                 
*                                                                               
         LKREQ E                                                                
*                                                                               
*                                                                               
SVRDEF   CSECT                                                                  
*                                                                               
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
*                                                                               
SAVED    DSECT                                                                  
WVALUES  DS    0X                                                               
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
*                                                                               
WVALUESL EQU   *-WVALUES                                                        
*                                                                               
ALIOB    DS    A                   A(ALIOB)                                     
ALINKIO  DS    A                   A(LINKIO)                                    
ARECUP   DS    A                   A(ARECUP)                                    
VTRPACK  DS    A                   A(TRPACK)                                    
VSCANNER DS    A                   A(SCANNER)                                   
RECADDR  DS    A                   A(RECORD HANDLING ROUTINE)                   
SRVRRELO DS    A                                                                
SVAREC   DS    A                   A(RECORD) TO PROCESS                         
SVR1     DS    A                                                                
I$CAM    DS    A                   A(CLIENT AGENCY MEDIA)                       
I$CCLT   DS    A                   A(CLIENT CLIENT CODE)                        
                                                                                
*                                                                               
ANET1    DS    A                   ADDRESS OF 1ST NETWORK                       
*                                                                               
DATADISP DS    H                                                                
*                                                                               
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
                                                                                
QVALUES  DS    0F                  ** Request values **                         
                                                                                
SVIOKEY  DS    XL(L'IOKEY)                                                      
OLDELEM  DS    XL(L'ELEM)                                                       
NEWELEM  DS    XL(L'ELEM)                                                       
MORPRDEL DS    XL(L'ELEM)                                                       
                                                                                
PERVALST DS    XL56                PERVAL STORAGE AREA                          
                                                                                
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL64                                                             
                                                                                
SVNETS   DS    CL53                4 NETWORK ELEMENTS+1 FOR END OF LIST         
ERRORMSG DS    CL60                                                             
                                                                                
QAGY     DS    CL(L'LP_AGY)                                                     
SVBAGM   DS    X                                                                
SVBCLT   DS    CL2                                                              
SVKEY    DS    CL24                                                             
SVSLN    DS    X                   SAVE SPOT LEN                                
ACTSLN   DS    CL1                                                              
OLDACTS  DS    (NUMACTS)CL12                                                    
NEWACTS  DS    (NUMACTS)CL12                                                    
NUMACTS  EQU   4                   NUMBER OF ACTUALS ALLOWED                    
MAXNETS  EQU   4                   MAX CML NETWORKS                             
*                                                                               
SVQNET   DS    CL4                                                              
QPRDA    DS    CL3                                                              
QPRDENT  EQU   7                   PRD=ALL                                      
*                                                                               
DSTRYDAT DS    XL3                                                              
DSTRYTIM DS    XL2                                                              
*                                                                               
SYSFIL   DS    CL8                                                              
ELCODE   DS    XL1                                                              
SVCLTOFF DS    CL1                 CLIENT OFFICE CODE                           
*                                                                               
SVT1PROF DS    CL16                                                             
SVTN1PR8 EQU   SVT1PROF+7   Y,V,N  COMML CLASS REQUIRED/VALIDATED               
SVTN1PRC EQU   SVT1PROF+11  Y,N    COMML END DATE-UFN INVALID                   
*                                                                               
SVTN2PR5 DS    CL1                                                              
SVTN2PR9 DS    CL1                                                              
*                                                                               
ERRFLG   DS    CL1                 ERROR FLAG                                   
*                                                                               
FLAGS    DS    X                   VARIOUS FLAGS                                
SOFTDEL  EQU   X'80'               JUST DID A SOFT DELETE                       
SOFTREST EQU   X'40'               JUST DID A SOFT RESTORE                      
ISCOVCML EQU   X'20'               THIS IS A COVER CMML                         
ISCOVERD EQU   X'10'               THIS CMML IS COVERED                         
UNCOVER  EQU   X'08'               ACTION UNCOVER                               
MATELEM  EQU   X'04'               ADDING MATCH ELEMENT (X'B0')                 
*                                                                               
*                                                                               
CHGFLAGS DS    0XL3                                                             
CHGFLAG1 DS    XL1                                                              
CHGFLAG2 DS    XL1                                                              
CHGFLAG3 DS    XL1                                                              
*                                                                               
BTODAY   DS    CL3                                                              
SVUID    DS    CL2                                                              
DATE     DS    CL6                                                              
HOLDCML  DS    CL8                                                              
HOLDSEQ  DS    XL3                                                              
ADIDFLAG DS    C                                                                
NWHIDEFX DS    XL8                 NEW HIDEF HEX                                
SVHIDEFX DS    XL8                 ORIGINAL HIDEF HEX                           
SVHIDEF  DS    CL12                ORIGINAL HIDEF CHAR                          
SVHDDELK DS    XL13                                                             
SVHDADDK DS    XL13                                                             
SVDSKAD  DS    CL4                                                              
SVDSKADR DS    XL4                 SAVE DISK ADDR OF COMML REC                  
                                                                                
QFORMAT  DS    CL1                                                              
QCOMML   DS    CL12                                                             
QCMLTTLE DS    CL24                CMML TITLE                                   
QCMLDSC2 DS    CL24                                                             
QCMLDSC3 DS    CL24                                                             
QCMLSLN  DS    XL1                 CMML LENGTH                                  
QCMLSOLO DS    CL1                 C'S'=SOLO/C'P'=P/J                           
QCMLPRNT DS    CL12                PARENT COMMERCIAL                            
QCMLRLSE DS    CL11                RELEASE DATE (YMD)                           
QCMLRCL  DS    CL11                RECALL DATE  (YMD)                           
QCMLMSTM DS    CL6                 START TIME                                   
QCMLMETM DS    CL6                 END TIME                                     
QCMLDSDT DS    CL11                DESTROY DATE                                 
QCMLDSTM DS    CL5                 DESTROY TIME (2400=12A,0=NONE)               
QCMLOVRD DS    CL6                                                              
QDLYFLG  DS    XL1                 CHECK TIMES DAILY (Y/N)                      
*                                                                               
QCMLMDTE1 DS   CL17                MATCH DATES                                  
*                                                                               
QCMLCLTN DS    CL20                CLIENT CMML NUMBER                           
QCMLTALX DS    CL1                 EXCL TAL TRANSFER                            
QCMLCLAS DS    CL4                 CLASS                                        
*                                                                               
QCMLACT DS     CL12                8-12 CHAR ACTUAL CML (SPACE PADDED)          
QCMLACT2 DS    CL12                                                             
QCMLACT3 DS    CL12                                                             
QCMLACT4 DS    CL12                                                             
*                                                                               
QCKSM    DS    XL4                 RECORD CHECK SUM                             
*                                                                               
RECACT   DS    X                   Record Action                                
LOOPCT   DS    XL1                                                              
*                                                                               
QPRDIND  DS    X                                                                
APRD     DS    AL3                 ARRAY OF PRODUCTS                            
NUMPRD   DS    XL(L'LW_NUMN)       N'PRODS TO PROCESS                           
*                                                                               
QNETIIND DS    X                                                                
ANETI    DS    AL3                 ARRAY OF NETWORKS                            
*                                                                               
QNETXIND DS    X                                                                
ANETX    DS    AL3                 ARRAY OF EXCLUDED NETWORKS                   
NUMNET   DS    XL(L'LW_NUMN)       N'NETS TO PROCESS (INC OR EXCLUDE)           
*                                                                               
QNETDIND DS    X                                                                
ANETD    DS    AL3                 ARRAY OF DELETED NETWORKS                    
NUMNETD  DS    XL(L'LW_NUMN)       N'NETS TO PROCESS                            
*                                                                               
QMDTIND  DS    X                                                                
AMDT     DS    AL3                 ARRAY OF MATCH DATES                         
NUMMDT   DS    XL(L'LW_NUMN)       N'MATCH DATES TO PROCESS                     
*                                                                               
QACTIND  DS    X                                                                
AACT     DS    AL3                 ARRAY OF ACTUAL COMMERCIALS                  
NUMACT   DS    XL(L'LW_NUMN)       N'ACTUALS TO PROCESS                         
*                                                                               
QVALUEL  EQU   *-QVALUES                                                        
*                                                                               
SAVEL    EQU   *-SAVED                                                          
*                                                                               
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTPROG DS    AL3                 A(ROUTINE)                                   
RECTFLAG DS    X                                                                
RECTABL  EQU   *-RECTABD                                                        
*                                                                               
         EJECT                                                                  
* Other included books                                                          
         PRINT OFF                                                              
       ++INCLUDE SPTRCMLCLS                                                     
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPGENESTD                                                      
       ++INCLUDE FALOCKETD                                                      
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
                                                                                
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NENAV28   03/19/18'                                      
         END                                                                    
