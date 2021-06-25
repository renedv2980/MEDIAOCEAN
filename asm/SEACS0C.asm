*          DATA SET SEACS0C    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA0D0CA,*                                                                
*INCLUDE SCINKEY                                                                
         TITLE 'SEACS0C - SYSTEM ACCESS PASSWORD RECORD LISTING'                
ACS0C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AS0C**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         L     R1,=V(SCINKEY)                                                   
         AR    R1,RE                                                            
         ST    R1,VSCINKEY                                                      
         L     R1,ACOM                                                          
         L     R1,CXSORT-COMFACSD(R1)                                           
         ST    R1,VXSORT                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                  GET AGENCY ACCESS DETAILS                    
         MVI   PIDREQD,C'N'                                                     
         GOTO1 AGETAAD,AGENCYID                                                 
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDTOUT,APWORK+1    PASSWORD TIMEOUT DAYS                        
         MVC   PWDMINLN,APWORK+2   PASSWORD MINIMUM LENGTH                      
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     EXIT                01 - APMVALK                                 
         B     EXIT                02 - APMVALR                                 
         B     EXIT                03 - APMDISK                                 
         B     EXIT                04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
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
         B     PUTKEY              18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)    *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R2                                                        
VALSEL   LA    R2,APRECKEY         BUILD FIRST PASSWORD RECORD KEY              
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SAVPARM(32),APPARM                                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE AND COMPLEMENT               
         XC    TODAYC,TODAY                                                     
         XC    SAVPWD,SAVPWD                                                    
         XC    SELOPT(SELOPTL),SELOPT       CLEAR FILTERS                       
*                                                                               
VSPWD    GOTO1 AFVAL,LSTPWDH       VALIDATE PASSWORD FILTER                     
         BNE   VSPWDX                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VSPWD1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VSPWD2              FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VSPWD1                                                        
VSPWD2   STC   RE,SELKEYCL                                                      
         MVC   SELPWD,FVIFLD                                                    
         MVC   SELPWDL,FVILEN                                                   
         MVC   SELPWDSP,0(RF)                                                   
VSPWDX   EQU   *                                                                
         MVC   SA0KCODE,SELPWD     MOVE TO KEY                                  
         MVC   SELDEF,TODAY                                                     
*                                                                               
VS010    GOTO1 AFVAL,LSTPIDH       GET PERSONAL ID FILTER                       
         BNE   VS020                                                            
         MVC   SELPID,FVIFLD                                                    
         MVC   SELPIDL,FVILEN                                                   
*                                                                               
VS020    GOTO1 AFVAL,LSTDEFH       GET EFFECTIVE DATE FILTER                    
         BNE   VS030                                                            
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BE    VS026                                                            
         SR    RE,RE                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL           'ALL' KEYWORD                                
         BNE   SAEIIF                                                           
         LA    R1,LSTDEFH                                                       
         BAS   RE,ECHOALL                                                       
         XC    SELDEF,SELDEF                                                    
         B     VS030                                                            
VS026    MVC   SELDEF,APWORK+PVALCSTA-PERVALD                                   
*                                                                               
VS030    GOTO1 AFVAL,LSTAGRH       VALIDATE ACCESS GROUP FILTER                 
         BNE   VS032                                                            
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY     CHECK ACCESS GROUP RECORD EXISTS             
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAGR,FVIFLD                                                    
*                                                                               
VS032    EQU   *                                                                
*                                                                               
VALSELY  MVC   APPARM(32),SAVPARM                                               
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         NI    APINDS,X'FF'-APILRERD                                            
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R2                                                        
GETSEL   LA    R2,IOKEY            READ NEXT PASSWORD RECORD                    
         MVC   SA0KEY,APRECKEY       FROM LAST KEY SAVED                        
         TM    APINDS,APILFLST     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GS010                                                            
         TM    GETSEQF,APILRERD    TEST GETSEL SEQUENCE BROKEN                  
         BZ    GS002                                                            
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GS004                                                            
GS002    TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GS010                                                            
         NI    APINDS,X'FF'-APILRERD                                            
GS004    GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GS020                                                            
         B     GSEND                                                            
GS010    TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOCONFIL+IOSQ+IO1  ELSE NEXT LIST LINE                        
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
         L     R2,AIOAREA1         CHECK PASSWORD RECORD TYPE                   
         MVC   APRECKEY(L'SA0KEY),SA0KEY  SAVE LAST RECORD KEY                  
         CLI   SA0KTYP,SA0KTYPQ                                                 
         BNE   GSEND                 IF NOT EXIT                                
         CLC   SA0KAGY,AGENCYID                                                 
         BNE   GSEND                                                            
         CLI   PIDREQD,C'Y'                                                     
         BE    GS022                                                            
         OC    SA0KEYS(20),SA0KEYS IGNORE PASSWORD NUMBER RECORD                
         BZ    GS020                                                            
         TM    SA0STAT,X'20'                                                    
         BNZ   GS020                                                            
         TM    SA0STAT,X'40'                                                    
         BZ    GS020                                                            
         B     GSPWD                                                            
GS022    EQU   *                                                                
         OC    SA0KEYS(20),SA0KEYS IGNORE PASSWORD CODE RECORD                  
         BNZ   GS020                                                            
         TM    SA0STAT,X'20'                                                    
         BNZ   GS020                                                            
         TM    SA0STAT,X'40'                                                    
         BZ    GS020                                                            
*                                                                               
GSPWD    EQU   *                                                                
         CLI   SELPWDSP,C' '       PASSWORD - FILTER ONLY IF IT                 
         BNH   GSPWDX                CONTAINS SPECIAL (WILD) CHARS.             
         MVC   PWDCODE,SPACES       SAVE PASSWORD CODE                          
         CLI   PIDREQD,C'Y'                                                     
         BE    GSPW010                                                          
         MVC   PWDCODE,SA0KCODE                                                 
         B     GSPW020                                                          
GSPW010  XC    APELEM,APELEM                                                    
         MVI   APELEM,X'03'        GET PASSWORD POINTER ELEMENT                 
         GOTO1 AGETELS,SA0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    GS020                                                            
         MVC   PWDCODE,SAPASDTA-SAPASD(R3)                                      
GSPW020  XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSPW100                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PWDCODE(0),SELPWD                                                
         BH    GSEND               (NO MORE RELEVENT RECORDS)                   
GSPW100  GOTO1 ATXTFLT,APPARM,(SELPWDL,SELPWD),(L'PWDCODE,PWDCODE)              
         BNE   GS020               READ NEXT RECORD                             
GSPWDX   EQU   *                                                                
*                                                                               
GS030    LA    R3,SA0DATA          GET ELEMENT DATA                             
         MVI   APFLAG,0            FLAG TO AVOID OLD TYPE PWD. RECS.            
GSLP1    CLI   0(R3),0             E-O-R                                        
         BE    GSLP1X                                                           
         CLI   0(R3),SAPALELQ                                                   
         BE    GSPID                                                            
         CLI   0(R3),SAPEFELQ                                                   
         BE    GSPEF                                                            
*                                                                               
GSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GSLP1                                                            
*                                  PERSONAL ID POINTER ELEMENT                  
         USING SAPALD,R3                                                        
GSPID    MVI   APFLAG,1            FLAG PERSONAL-ID TYPE PWD. RECORD            
         MVC   SAVPID,SAPALPID     SAVE PERSONAL-ID                             
         OC    SELPID,SELPID       FILTER ON PERSONAL-ID                        
         BZ    GSLP1A                                                           
         CLI   SAPALLN,SAPALLNQ                                                 
         BL    GS020                                                            
         SR    R1,R1                                                            
         IC    R1,SELPIDL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELPID(0),SAPALPID                                               
         BNE   GS020                                                            
         B     GSLP1A                                                           
*                                  EFFECTIVE DATES ELEMENT                      
         USING SAPEFD,R3                                                        
GSPEF    OC    SELDEF,SELDEF       FILTER EFFECTIVE DATE                        
         BZ    GSLP1A                                                           
         CLC   SAPEFSTA,SELDEF     START DATE                                   
         BH    GS020                                                            
         CLC   SAPEFEND,SELDEF     END DATE                                     
         BL    GS020                                                            
         B     GSLP1A                                                           
*                                                                               
GSLP1X   CLI   APFLAG,0            CHECK NEW STYLE PASSWORD RECORD              
         BE    GS020                 IF NOT GET NEXT                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CONTINUE GETSEL, FILTER ON DATA FROM ASSOCIATED PERSON ID RECORD    *         
***********************************************************************         
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(PERSON RECORD)                          
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,SAVPID      GET PERSONAL ID                              
         MVC   SAPEDEF,TODAYC                                                   
         OI    GETSEQF,APILRERD    FLAG GETSEL RDSEQ BROKEN                     
         GOTO1 AIO,IOHI+IOCONFIL+IO3                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R2,AIOAREA3                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),SAPEREC                                   
         BNE   GETSELOK            PERSON DELETED STILL SHOW PASSWORD           
*                                                                               
         LA    R3,SAPEDATA         FILTER ON DATA IN ELEMENTS                   
         MVI   SELAGRF,0                                                        
GSLP2    CLI   0(R3),0             E-O-R                                        
         BE    GS100                                                            
         CLI   0(R3),SAPERELQ                                                   
         BE    GSPER                                                            
         CLI   0(R3),SAAGCELQ                                                   
         BE    GSAGC                                                            
*                                                                               
GSLPSA   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GSLP2                                                            
*                                                                               
         USING SAPERD,R3                                                        
GSPER    EQU   *                   FILTER ON PERSONNEL DETAILS                  
         CLI   OPTTER,C'Y'         OPTION TERM=Y                                
         BE    GSPE002                                                          
         CLI   OPTTER,C'O'         OPTION TERM=O                                
         BE    GSPE001                                                          
         CLI   OPTTER,C'N'         DEFAULT TERM=Y                               
         BNE   GSPE002                                                          
         OC    SAPERDTE,SAPERDTE   OPTION TERM=N                                
         BZ    GSPE002                                                          
         CLC   SAPERDTE,TODAY                                                   
         BL    GETSEL                                                           
         B     GSPE002                                                          
GSPE001  OC    SAPERDTE,SAPERDTE                                                
         BZ    GETSEL                                                           
         CLC   SAPERDTE,TODAY                                                   
         BNL   GETSEL                                                           
         B     GSPE002                                                          
GSPE002  EQU   *                                                                
*&&UK                                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
         GOTO1 ATSTOMAN,SAPEROFF                                                
         BNE   GSPE010                                                          
         B     GSLPSA                                                           
*&&                                                                             
*&&US                                                                           
*                                  TEST OFFICE/DEPT MANAGER ACCESS              
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BE    GSLPSA                                                           
*&&                                                                             
GSPE010  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSEL                                                           
         DROP  R3                                                               
*                                                                               
         USING SAAGCD,R3                                                        
GSAGC    MVI   SELAGRF,1                                                        
         OC    SELAGR,SELAGR       FILTER ON ACCESS GROUP CODE                  
         BZ    GSLPSA                                                           
         CLI   SAAGCLN,SAAGCLNQ                                                 
         BL    GETSEL                                                           
         CLC   SELAGR,SAAGCCOD                                                  
         BNE   GETSEL                                                           
         B     GSLPSA                                                           
*                                                                               
GS100    CLI   SELAGRF,0           CHECK ACCESS GROUP FOUND                     
         BNE   *+14                                                             
         OC    SELAGR,SELAGR                                                    
         BNZ   GETSEL                                                           
         B     GETSELOK                                                         
*                                                                               
GETSELOK MVC   FVMSGNO,=AL2(FVFOK) EXIT OK                                      
         B     GETSELX                                                          
*                                                                               
GSEND    MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R2                                                        
DISSEL   L     R2,AIOAREA1                                                      
         MVC   SAVPARM(32),APPARM                                               
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         CLI   PIDREQD,C'Y'                                                     
         BE    *+10                                                             
         MVC   LISTPWD,SA0KCODE    DISPLAY PASSWORD                             
         MVI   APBYTE,C'L'         GET LONG SYSTEM NAMES FIRST                  
         MVI   APFLAG,0                                                         
DS101    LA    R8,WORK                                                          
         MVC   WORK,SPACES                                                      
         LA    R3,SA0DATA          GET SYSTEM ELEMENTS                          
DS110    CLI   0(R3),0             TEST END OF RECORD                           
         BE    DS180                                                            
         CLI   0(R3),SASYSELQ                                                   
         BE    DS130                                                            
         CLI   0(R3),SAPEFELQ                                                   
         BE    DSPEF                                                            
         CLI   0(R3),SAPALELQ                                                   
         BE    DSPID                                                            
         CLI   0(R3),SAPASELQ                                                   
         BE    DSPAS                                                            
DS120    ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DS110                                                            
*                                  SYSTEM ELEMENT                               
         USING SAPASD,R3                                                        
DSPAS    EQU   *                                                                
         CLI   PIDREQD,C'N'                                                     
         BE    DS120                                                            
         MVC   LISTPWD,SAPASDTA                                                 
         B     DS120                                                            
*                                  SYSTEM ELEMENT                               
         USING SASYSD,R3                                                        
DS130    GOTO1 ADISSYS,SASYSNUM                                                 
         CLI   APBYTE,C'S'         TEST SHORT NAMES REQUIRED                    
         BNE   DS140                                                            
         L     R1,APPARM                                                        
         MVC   APWORK(7),SPACES    MOVE SHORT NAME TO APWORK                    
         MVC   APWORK(3),9(R1)                                                  
DS140    MVC   0(7,R8),APWORK      INSERT NAME                                  
         LA    R8,6(R8)                                                         
         CLI   0(R8),C' '          SQUASH OUT SPACES                            
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         LA    R8,2(R8)                                                         
         B     DS120                                                            
*                                  EFFECTIVE DATES ELEMENT                      
         USING SAPEFD,R3                                                        
DSPEF    CLC   SAPEFSTA,FFILL                                                   
         BE    DSPEF10                                                          
         GOTO1 VDATCON,APPARM,(2,SAPEFSTA),(X'08',APWORK)                       
         MVC   LISTDEF1,APWORK     DISPLAY START DATE                           
DSPEF10  CLC   SAPEFEND,FFILL                                                   
         BE    DS120                                                            
         CLC   SAPEFEND,SAPEFSTA                                                
         BNL   DSPEF20                                                          
         GOTO1 VDATCON,APPARM,(2,SAPEFSTA),(X'08',APWORK)                       
         MVC   LISTDEF2,APWORK     DISPLAY END DATE<=START                      
         B     DS120                                                            
DSPEF20  GOTO1 VDATCON,APPARM,(2,SAPEFEND),(X'08',APWORK)                       
         MVC   LISTDEF2,APWORK     DISPLAY END DATE                             
         B     DS120                                                            
*                                  PERSONAL ID POINTER ELEMENT                  
         USING SAPALD,R3                                                        
DSPID    MVC   LISTPID,SAPALPID    DISPLAY PERSONAL ID                          
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY      READ PERSON RECORD                          
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,SAPALPID                                                 
         MVC   SAPEDEF,TODAYC                                                   
         GOTO1 AIO,IOHI+IOCONFIL+IO3                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    APINDS,APILRERD     FLAG LIST READ SEQUENCE BROKEN               
         MVI   APFLAG,1            FLAG PERSON RECORD FOUND                     
         B     DS120                                                            
*                                                                               
DS180    CLI   APFLAG,0            DISPLAY PERSON RECORD DATA                   
         BE    DS200                 IF FOUND OK                                
         L     R2,AIOAREA3                                                      
         LA    R3,SAPEDATA         GET ACCESS GROUP ELEMENT                     
DS182    CLI   0(R3),0             TEST END OF RECORD                           
         BE    DS200                                                            
         CLI   0(R3),SAAGCELQ                                                   
         BE    DSAGC                                                            
DS184    ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DS182                                                            
*                                  SYSTEM ELEMENT                               
         USING SAAGCD,R3                                                        
DSAGC    CLI   SAAGCLN,SAAGCLNQ    DISPLAY ACCESS GROUP CODE                    
         BL    DS184                                                            
         MVC   LISTAGR,SAAGCCOD                                                 
         B     DS200                                                            
*                                                                               
DS200    LA    R1,WORK+L'LISTSYST  SEE IF LIST IS TOO LONG                      
         CR    R8,R1                                                            
         BH    *+14                YES SO RERUN FOR SHORT NAMES                 
         MVC   LISTSYST,WORK                                                    
         B     DISSELX                                                          
         CLI   APBYTE,C'S'                                                      
         BE    *+12                                                             
         MVI   APBYTE,C'S'                                                      
         B     DS101                                                            
         MVC   LISTSYST,WORK                                                    
         CLI   LISTSYST+L'LISTSYST-1,C' '                                       
         BE    DISSELX                                                          
         MVI   LISTSYST+L'LISTSYST-1,C'>'                                       
         B     DISSELX                                                          
*                                                                               
DISSELX  MVC   APPARM(32),SAVPARM                                               
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R2                                                        
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
         MVI   0(R3),KEYPWD                                                     
         MVI   1(R3),12                                                         
         MVC   2(L'SA0KCODE,R3),SA0KCODE                                        
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
         MVI   0(R3),KEYPID                                                     
         MVI   1(R3),10                                                         
         MVC   2(8,R3),SAVPID                                                   
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
********************************************************************            
*   BUILD PROGRAM ACCESS LIST INTO BLOCK                           *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
BLCKACC  NTR1                                                                   
         USING SASYSD,R3                                                        
         MVC   SYSTEM,SASYSNUM                                                  
         MVC   SYSALL,SASYSALL     SAVE ALL= VALUE                              
         MVI   BLKCNT,0                                                         
         LA    R4,BLOCK            SET R4 TO START OF TABLE                     
         ZIC   R8,SASYSLN                                                       
         CLI   SASYSLN,16         ALL=VALUE ONLY                                
         BE    BLACX                                                            
         LA    R3,SASYSPGM                                                      
         DROP  R3                                                               
*                                                                               
BLAC1    CH    R8,=H'16'                                                        
         BNH   BLACX                                                            
         MVC   PROGRAM,0(R3)                                                    
         LA    R3,1(R3)                                                         
         GOTO1 ADISPGM,APPARM,(SYSTEM,PROGRAM)                                  
         ICM   R1,15,APPARM                                                     
         BZ    BLAC3               NO PROGRAM SO SKIP                           
         CLC   SYSALL,0(R3)                                                     
         BE    BLAC3               ACCESS SAME AS ALL= SO SKIP                  
         MVC   0(10,R4),SPACES                                                  
         MVC   0(4,R4),APWORK                                                   
         LR    R0,R4               SAVE R4 VALUE                                
BLACALL  LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'='                                                       
         CLC   YAUTH,0(R3)                                                      
         BNE   *+12                                                             
         MVI   2(R4),C'Y'          PROG=Y                                       
         B     BLAC2                                                            
*                                                                               
         CLC   NAUTH,0(R3)                                                      
         BNE   *+12                                                             
         MVI   2(R4),C'N'          PROG=N                                       
         B     BLAC2                                                            
*                                  PROG=XXXX                                    
         GOTO1 VHEXOUT,APPARM,(R3),2(R4),2,=C'TOG'                              
*                                                                               
BLAC2    LR    R4,R0               RESTORE R4                                   
         LA    R4,10(R4)           AND INDEX TO NEXT                            
         SR    R1,R1                                                            
         IC    R1,BLKCNT                                                        
         LA    R1,1(R1)            BUMP PROG COUNT                              
         STC   R1,BLKCNT                                                        
BLAC3    CLI   PROGRAM,0                                                        
         BE    BLACX1              ALL= VALUE OUTPUT                            
         LA    R3,2(R3)            NEXT PROGRAM                                 
         SH    R8,=H'3'                                                         
         B     BLAC1                                                            
*                                                                               
BLACX    LA    R3,SYSALL                                                        
         MVI   PROGRAM,0           SET TO OUTPUT ALL=                           
         MVC   0(10,R4),SPACES                                                  
         MVC   0(4,R4),=C'ALL '                                                 
         B     BLACALL                                                          
*                                                                               
BLACX1   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   BUILD ID LIST INTO BLOCK                                          *         
*   R2 = RECORD                                                       *         
***********************************************************************         
DISIDS   NTR1                                                                   
         MVI   BLKCNT,0                                                         
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'20'        GET ID ELEMENT                               
         GOTO1 AGETELS,SA0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DISIDX              NOT FOUND END                                
         LA    R4,BLOCK                                                         
DISID01  MVC   0(10,R4),SAID-SAIDD(R3)                                          
         OC    0(2,R4),0(R4)       TEST ID LIST                                 
         BNZ   *+10                                                             
         MVC   0(2,R4),=C'L='                                                   
         LA    R4,10(R4)                                                        
         SR    R1,R1                                                            
         IC    R1,BLKCNT                                                        
         LA    R1,1(R1)            BUMP COUNTER                                 
         STC   R1,BLKCNT                                                        
         BAS   RE,NEXTEL           GET NEXT ID                                  
         BE    DISID01                                                          
*                                                                               
DISIDX   B     EXIT                                                             
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
*                                  ERROR EXITS                                  
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
SAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
SAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
SAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
SAEDIF   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
SAESYS   MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     NO                  INVALID SYSTEM NAME                          
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
*                                                                               
ECHOALL  MVC   FVIFLD-FVIHDR(L'CT@ALL,R1),CT@ALL                                
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BR    RE                                                               
*                                                                               
FLDALL   CLC   FVIFLD(0),CT@ALL                                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    132C' '                                                          
FFILL    DC    32X'FF'                                                          
ASTS     DC    C'+',130C'-',C'+'                                                
*                                                                               
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
*                                                                               
* SEACSDICT                                                                     
       ++INCLUDE SEACSDICT                                                      
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD3D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTPWD  DS    CL10                PASSWORD CODE                                
         DS    CL1                                                              
LISTPID  DS    CL8                 PERSONAL ID                                  
         DS    CL1                                                              
LISTAGR  DS    CL8                 ACCESS GROUP                                 
         DS    CL1                                                              
LISTDEF1 DS    CL8                 START EFFECTIVE DATE                         
         DS    CL1                                                              
LISTDEF2 DS    CL8                 END EFFECTIVE DATE                           
         DS    CL1                                                              
LISTSYST DS    CL20                VALID SYSTEM NAMES LIST                      
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
*                                                                               
SAVPARM  DS    8F                                                               
VSCINKEY DS    A                                                                
VXSORT   DS    A                                                                
FIELD    DS    F                                                                
*                                                                               
PIDREQD  DS    XL1                 PERSONID REQUIRED WITH PASSWD=Y              
PWDTOUT  DS    XL1                 PASSWORD TIMEOUT IN DAYS                     
PWDMINLN DS    XL1                 PASSWORD MINIMUM LENGTH                      
*                                                                               
ASYSEL   DS    A                                                                
ASEPGMS  DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
RETURN   DS    A                                                                
WORK     DS    CL80                                                             
*                                                                               
SAVPID   DS    CL(L'SAPEPID)       SAVE LAST PID FOR LIST FILTER                
SAVPWD   DS    CL(L'SA0KCODE)      SAVE LAST PWD FOR LIST FILTER                
*                                                                               
PWDCODE  DS    CL10                PASSWORD CODE SAVE                           
PWDNUM   DS    XL2                 PASSWORD NUMBER SAVE                         
*                                                                               
TODAY    DS    XL2                 TODAYS DATE COMPRESSED BINARY                
TODAYC   DS    XL2                 COMPLEMENT OF IT                             
*                                                                               
GETSEQF  DS    XL1                 APINDS SAVE FLAG FOR GETSEL                  
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
SAPEKSAV DS    XL(L'SAPEKEY)       PERSONAL-ID RECORD KEY SAVE                  
*                                                                               
BLOCK    DS    20CL32              SCANNER BLOCKS                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLKCNT   DS    XL1                                                              
*                                                                               
SELOPT   DS    0X                  SELECT OPTIONS                               
SELPWD   DS    CL10                PASSWORD CODE FILTER                         
SELPWDL  DS    XL1                 PASSWORD CODE FILTER LENGTH                  
SELPWDSP DS    XL1                 PASSWORD FILTER SPECIAL CHAR                 
SELKEYCL DS    XL1                 PASSWORD CODE LENGTH                         
SELPID   DS    CL8                 PERSONAL-ID CODE FILTER                      
SELPIDL  DS    XL1                 PERSONAL-ID CODE FILTER LENGTH               
SELDEF   DS    XL2                 EFFECTIVE DATE FILTER                        
SELDEFF  DS    XL1                 EFFECTIVE DATE FILTER FLAG                   
SELSYS   DS    XL1                 SYSTEM SELECT                                
SELAGR   DS    CL8                 ACCESS GROUP FILTER                          
SELAGRF  DS    XL1                 ACCESS GROUP FILTER FLAG                     
SELSRT   DS    CL1                 SORT SEQUENCE:                               
*                                  C'1' - PERSONAL ID WITHIN DEPT               
*                                  C'2' - FIRST NAME                            
*                                  C'3' - FIRST NAME WITHIN DEPT                
*                                  C'4' - LAST NAME                             
*                                  C'5' - LAST NAME WITHIN DEPT                 
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
FLDCNT   DS    XL1                 FIELD COUNT                                  
COUNT    DS    XL1                 COUNTER                                      
PROGRAM  DS    CL1                 PROGRAM CODE                                 
PGNAME   DS    CL8                 PROGRAM NAME SAVE                            
SYSTEM   DS    CL1                 SYSTEM SE NUMBER                             
SYSALL   DS    CL(L'SASYSALL)      DEFAULT ALL PROGRAM ACCESS CODE              
*                                                                               
XSORTBLK DS    64XL(10)                                                         
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SEACS0C   05/01/02'                                      
         END                                                                    
