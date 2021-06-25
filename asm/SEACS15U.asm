*          DATA SET SEACS15U   AT LEVEL 108 AS OF 05/01/02                      
*PHASE TA0D15A                                                                  
*INCLUDE SCINKEY                                                                
         TITLE 'SEACS15 -  SYSTEM ACCESS - SYSTEM RECORD REPORTING'             
*&&      SET   NOP=N                                                            
ACS15    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC15**,RA,R9,RR=RE                                           
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
         B     EXIT                07 - APMVALP                                 
         B     EXIT                08 - APMGETS                                 
         B     EXIT                09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     EXIT                11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     EXIT                14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
VALREQ   EQU   *                                                                
*                                  GET AGENCY ACCESS DETAILS                    
         MVI   PIDREQD,C'N'                                                     
         GOTO1 AGETAAD,AGENCYID                                                 
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDTOUT,APWORK+1    PASSWORD TIMEOUT DAYS                        
         MVC   PWDMINLN,APWORK+2   PASSWORD MINIMUM LENGTH                      
*                                                                               
         L     R4,AREP                                                          
         USING REPD,R4             R4=A(REPORT WORK AREA)                       
         USING REPAD,REPP1                                                      
         XC    APRECKEY,APRECKEY                                                
         SPACE 1                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
         SPACE 1                                                                
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AFVAL,REPREQPH      VALIDATE REQUEST PASSWORD                    
         BNE   *+10                                                             
         MVC   REPPSWD,FVIFLD                                                   
         SPACE 1                                                                
         LA    R2,APRECKEY         BUILD KEY OF PERSON RECORD                   
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
*                                                                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE AND COMPLEMENT               
         XC    TODAYC,TODAY                                                     
         MVC   SAPEDEF,TODAYC                                                   
         XC    SAVPID,SAVPID                                                    
         MVC   FIRSTPID,SPACES                                                  
         GOTO1 AFVAL,REPPIDH       READ FIRST PERSONAL ID                       
         BNE   *+16                                                             
         MVC   SAPEPID,FVIFLD                                                   
         MVC   FIRSTPID,FVIFLD     FIRST PID FOR THIS SELECTION                 
         XC    SELOPT(SELOPTL),SELOPT                                           
         SPACE 1                                                                
VQ002    GOTO1 AFVAL,REPUSRH       GET USER ID FILTER                           
         BNE   VQ010                                                            
         MVC   FVMSGNO,=AL2(CE#NONOW)                                           
         CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    VALREQX             'NOW' REPORT CAN'T USE THIS FILTER           
         GOTO1 AVALUID,REPUSRH                                                  
         BNE   VALREQX                                                          
         MVC   SELUSR,FVIFLD                                                    
*                                                                               
VQ010    GOTO1 AFVAL,REPPWDH       GET PASSWORD FILTER                          
         BNE   VQ020                                                            
         MVC   SELPWD,FVIFLD                                                    
         MVC   SELPWDL,FVILEN                                                   
*                                                                               
VQ020    GOTO1 AFVAL,REPDEFH       GET EFFECTIVE DATE FILTER                    
         BNE   VQ030                                                            
         MVC   SELDEFC,FFILL                                                    
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BE    VQ026                                                            
         SR    RE,RE                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL           'ALL' KEYWORD                                
         BNE   SAEIIF                                                           
         LA    R1,REPDEFH                                                       
         BAS   RE,ECHOALL                                                       
         XC    SELDEFC,FFILL                                                    
         B     VQ028                                                            
VQ026    XC    SELDEFC,APWORK+PVALCSTA-PERVALD                                  
VQ028    MVC   SAPEDEF,SELDEFC                                                  
         MVI   SELDEFF,1           FLAG DATE FILTER SET                         
*                                                                               
VQ030    GOTO1 AFVAL,REPAGRH       VALIDATE ACCESS GROUP FILTER                 
         BNE   VQ040                                                            
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
VQ040    GOTO1 AFVAL,REPOFFH       VALIDATE OFFICE CODE FILTER                  
         BNE   VQ050                                                            
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOFF,FVIFLD                                                    
*                                                                               
VQ050    GOTO1 AFVAL,REPDIDH       VALIDATE DEPARTMENT CODE FILTER              
         BNE   VQ060                                                            
         OC    SELOFF,SELOFF                                                    
         BZ    SAEODF                                                           
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY     CHECK DEPARTMENT RECORD EXISTS               
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SELOFF                                                   
         MVC   SADPDID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELDID,FVIFLD                                                    
*                                                                               
VQ060    GOTO1 AFVAL,REPSYSH       VALIDATE SYSTEM NAME FILTER                  
         BNE   VQ070                                                            
         GOTO1 AVALSYS,REPSYSH     VALIDATE SYSTEM                              
         BNE   NO                                                               
         MVC   SELSYS,APWORK                                                    
*                                                                               
VQ070    GOTO1 AFVAL,REPPGMH       VALIDATE PROGRAM NAME FILTER                 
         BNE   VQ080                                                            
         OC    SELSYS,SELSYS                                                    
         BE    SAEPFS                                                           
         GOTO1 AVALPGM,APPARM,(SELSYS,REPPGMH)  VALIDATE PROGRAM                
         BNE   SAEPGM                                                           
         MVC   SELPGM,APWORK                                                    
*                                                                               
VQ080    EQU   *                                                                
*                                                                               
VQ090    MVI   RPTFMT,0                                                         
         GOTO1 AFVAL,REPFSYSH      SEE IF SYSTEM REPORT REQUIRED                
         BNE   VQ100                                                            
         CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    SAENOW              'NOW' REPORT CAN'T USE THIS FILTER           
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL                                                        
         BNE   VQ092                                                            
         LA    R1,REPFSYSH                                                      
         BAS   RE,ECHOALL                                                       
         MVI   RPTFMT,X'FF'        FLAG SYSTEM ALL                              
         B     VQ100                                                            
VQ092    GOTO1 AVALSYS,REPFSYSH    VALIDATE SYSTEM                              
         BNE   NO                                                               
         MVC   RPTFMT,APWORK       SET SYSTEM SELECT                            
*                                                                               
VQ100    MVCDD REPDESC,CT#SYSL     SET REPORT DESCRIPTION                       
         GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
         MVI   REPHEADI,REPHCLRA                                                
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC3         WITHOUT PASSWORD                             
         CLI   OPTPWS,C'Y'         INCLUDE PASSWORD ON REPORT?                  
         BNE   *+8                                                              
         LA    R0,REPSPEC2         YES                                          
         CLI   RPTFMT,0            SHORT FORMAT REPORT?                         
         BNE   VQ110               NO.                                          
         MVI   REPMIDSI,REPMCLRA                                                
         LA    R0,REPSPEC1                                                      
         CLI   OPTPWS,C'Y'         INCLUDE PASSWORD ON REPORT?                  
         BNE   *+8                                                              
         LA    R0,REPSPEC0         YES                                          
VQ110    ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
*                                  CHECK PASSWORD FIELD SECURITY                
         MVI   PWDAFLAG,0                                                       
         OC    ACASEC,ACASEC                                                    
         BZ    VQ300                                                            
         CLI   ASONOFF,ASOFF                                                    
         BE    VQ300                                                            
         MVI   APBYTE,FLDPWD                                                    
         LA    RF,APBYTE                                                        
         GOTO1 VSECRET,SCPARM,('SECPFLDP',ACASEC),(RF)                          
         BNL   VQ300               FULL READ/WRITE ACCESS                       
*                                    OR NO ACCESS                               
VQ200    MVI   PWDAFLAG,X'FF'                                                   
         CLI   OPTPWS,C'Y'                                                      
         BNE   VQ300                                                            
         LA    R1,ACSOPTH                                                       
         ST    R1,FVADDR                                                        
         B     SAEOPT                                                           
*                                                                               
VQ300    MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FURNISH NEXT APPLICABLE RECORD TO REPORT ON              *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   GOTO1 VDICTAT,APPARM,C'LL  ',DCLIST,DSLIST                             
         L     R4,AREP                                                          
         USING REPD,R4             R4=A(REPORT WORK AREA)                       
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         MVC   SAPEKEY,APRECKEY    SET INITIAL KEY VALUE                        
         GOTO1 AIO,IOHI+IOCONFIL+IO1                                            
         B     FR08                GET FIRST RECORD                             
         SPACE 2                                                                
***********************************************************************         
* GET NEXT RECORD (SEQUENCE BROKEN)                                   *         
***********************************************************************         
         SPACE 1                                                                
FR02     LA    R2,IOKEY            RESTORE SAVED RECORD                         
         MVC   SAPEKEY(L'SAPEKEY),APRECKEY                                      
         SPACE 1                                                                
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BE    FR04                                                             
         B     PRTREPX                                                          
         SPACE 2                                                                
***********************************************************************         
* GET NEXT RECORD (SEQUENCE NOT BROKEN)                               *         
***********************************************************************         
         SPACE 1                                                                
FR04     L     R2,AIOAREA1                                                      
         CLC   SAPEPID,FIRSTPID    WAS ONLY ONE ID REQUIRED?                    
         BE    PRTREPX             YES.                                         
         SPACE 1                                                                
FR06     GOTO1 AIO,IOSQ+IOCONFIL+IO1  - GET NEXT RECORD                         
         BE    FR08                                                             
         B     PRTREPX                                                          
         SPACE 2                                                                
***********************************************************************         
* FILTER ON THIS RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
FR08     L     R2,AIOAREA1         CHECK PERSON RECORD KEY TYPE                 
         CLI   SAPETYP,SAPETYPQ                                                 
         BNE   PRTREPX                                                          
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   PRTREPX                                                          
         CLC   SAPEAGY,AGENCYID    CHECK CORRECT AGENCY                         
         BNE   PRTREPX                                                          
*                                                                               
         CLI   SELDEFF,0           FILTER ON EFFECTIVE DATE?                    
         BZ    FR10                NO DATE FILTER                               
         CLC   SAPEDEF,SELDEFC                                                  
         BL    FR04                NOT HIGH ENOUGH - GET NEXT                   
         B     FR11                                                             
         SPACE 1                                                                
FR10     CLC   SAPEDEF,TODAYC      DEFAULT DATE IS TODAY                        
         BL    FR04                                                             
         CLC   SAPEPID,SAVPID      ALREADY SEEN THIS PID?                       
         BE    FR04                                                             
         MVC   SAVPID,SAPEPID      SAVE THIS PID TO PREVENT REPEAT              
         MVI   CURRPID,C'Y'        FLAG PERSON CURRENT EFFECTIVE DATE           
         MVI   EXPFLAG,C'N'        INITIALISE EXPIRED FLAG                      
         B     FR12                                                             
         SPACE 1                                                                
FR11     MVI   CURRPID,C'N'                                                     
         CLC   SAPEDEF,TODAYC                                                   
         BL    FR12                                                             
         CLC   SAPEPID,SAVPID                                                   
         BE    FR12                                                             
         MVC   SAVPID,SAPEPID                                                   
         MVI   CURRPID,C'Y'                                                     
         MVI   EXPFLAG,C'N'        INITILAISE EXPIRED FLAG                      
         B     FR12                                                             
         SPACE 1                                                                
FR12     MVC   APRECKEY(L'SAPEKEY),SAPEKEY  SAVE LAST RECORD KEY                
         MVC   RPTPID,SAPEPID      SAVE THIS PID FOR REPORT                     
         MVC   APHALF,FFILL                                                     
         XC    APHALF,SAPEDEF                                                   
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   RPTDEF1,APWORK      DISPLAY EFFECTIVE DATE                       
         MVI   APFLAG,0                                                         
         MVI   SELAGRF,0                                                        
         LA    R3,SAPEDATA         GET ELEMENT DATA                             
*                                                                               
FR14     CLI   0(R3),0             E-O-R                                        
         BE    FR18                                                             
         CLI   0(R3),SAPERELQ                                                   
         BE    PRPER                                                            
         CLI   0(R3),SAAGCELQ                                                   
         BE    PRAGC                                                            
         CLI   0(R3),SAPWDELQ                                                   
         BE    PRPWD                                                            
*                                                                               
FR16     ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         LA    R3,0(RF,R3)                                                      
         B     FR14                                                             
         SPACE 2                                                                
***********************************************************************         
* PERSONNEL DETAILS ELEMENT FILTERING                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPERD,R3           PERSONNEL DETAILS ELEMENT                    
PRPER    DS    0H                                                               
*&&UK                                                                           
         GOTO1 ATSTOMAN,SAPEROFF   CHECK OFFICE MANAGER ACCESS                  
         BE    PRPE04                                                           
         B     PRPE02              NOT ALLOWED                                  
*&&                                                                             
*&&US                                                                           
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BE    PRPE04              OFFICE/DEPT MANAGER ACCESS OK                
*&&                                                                             
PRPE02   MVC   FVMSGNO,=AL2(FVFOK) RESET MESSAGE                                
         B     FR02                ACCESS NOT ALLOWED                           
*                                                                               
PRPE04   OC    SELOFF,SELOFF       PERSONNEL DETAILS FILTER?                    
         BZ    PRPE06              NO                                           
         CLC   SELOFF,SAPEROFF                                                  
         BNE   FR02                FAILED FILTER                                
*                                                                               
PRPE06   OC    SELDID,SELDID       DEPT ID FILTER?                              
         BZ    PRPE08              NO                                           
         CLC   SELDID,SAPERDID                                                  
         BNE   FR02                FAILED FILTER                                
*                                                                               
PRPE08   CLI   SAPERLN,SAPERLNQ    FUNNY LENGTH                                 
         BL    FR02                                                             
         CLI   CURRPID,C'Y'        TEST CURRENT PERSON ID                       
         BNE   PRPE08A                                                          
         MVI   EXPFLAG,C'N'        SET EXPIRED FLAG                             
         OC    SAPERDTE,SAPERDTE   TERMINATION DATE FILTER?                     
         BZ    PRPE08A                                                          
         CLC   SAPERDTE,TODAY      TERM DATE < TODAY MEANS EXPIRED              
         BNL   PRPE08A                                                          
         MVI   EXPFLAG,C'Y'                                                     
*                                                                               
PRPE08A  CLI   OPTTER,C'Y'         OPTION TERM=Y - PASS ALL                     
         BE    PRPE10                                                           
         CLI   OPTTER,C'O'         OPTION TERM=O - ONLY TERMINATED              
         BE    PRPE08B                                                          
*                                                                               
         CLI   EXPFLAG,C'N'                                                     
         BE    PRPE10                                                           
         B     FR02                                                             
*                                                                               
PRPE08B  CLI   EXPFLAG,C'Y'                                                     
         BE    PRPE10                                                           
         B     FR02                                                             
*                                                                               
PRPE10   EQU   *                                                                
         B     PRPE14                                                           
*                                                                               
PRPE12   CLC   SAPERDTE,TODAY      PASS ONLY TERMINATED                         
         BNL   FR02                                                             
*                                                                               
PRPE14   B     FR16                                                             
         SPACE 2                                                                
***********************************************************************         
* ACCESS GROUP CODE FILTERING                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SAAGCD,R3                                                        
PRAGC    MVI   SELAGRF,1           FLAG ACCESS GROUP FOUND                      
         OC    SELAGR,SELAGR       ACCESS GROUP FILTER?                         
         BZ    FR16                NO                                           
         CLI   SAAGCLN,SAAGCLNQ                                                 
         BL    FR02                                                             
         CLC   SELAGR,SAAGCCOD     COMPARE ON ACCESS GROUP CODE                 
         BNE   FR02                                                             
         B     FR16                                                             
         SPACE 2                                                                
***********************************************************************         
* PASSWORD POINTER ELEMENT FILTERING                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPWDD,R3                                                        
PRPWD    XC    RPTCODE,RPTCODE                                                  
         OC    SELPWD,SELPWD       FILTERING ON PASSWORD?                       
         BZ    PPWD02              NO                                           
         CLI   SAPWDLN,SAPWDLNQ                                                 
         BL    FR02                                                             
*                                                                               
         XR    R1,R1                                                            
         IC    R1,SELPWDL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8              FILTER ON PASSWORD                           
         BNE   FR02                FAIL                                         
         CLC   SELPWD(0),SAPWDCOD                                               
*                                                                               
PPWD02   MVC   RPTCODE,SAPWDCOD    SAVE PASSWORD CODE                           
         MVC   APRECKEY,SAPEKEY    SAVE INITIAL KEY VALUE                       
*                                                                               
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY       READ PASSWORD RECORD                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         CLI   PIDREQD,C'Y'                                                     
         BE    *+14                                                             
         MVC   SA0KCODE,RPTCODE                                                 
         B     *+10                                                             
         MVC   SA0KNUM,SAPWDNUM                                                 
         OI    APINDS,APILRERD     FLAG READ SEQUENCE BROKEN                    
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
         BNE   FR16                IGNORE IF PASSWORD RECORD REMOVED            
*                                                                               
         MVI   APFLAG,1            FLAG PASSWORD RECORD FOUND                   
         B     FR16                                                             
         SPACE 2                                                                
***********************************************************************         
* END OF RECORD FILTERING                                             *         
***********************************************************************         
         SPACE 1                                                                
FR18     CLI   APFLAG,0            DID WE FIND PASSWORD RECORD?                 
*@@??    BE    GETINF              NO                                           
         BE    FR02             @@ IGNORE IF NO PASSWORD RECORD FOUND ?         
*                                                                               
         L     R2,AIOAREA3                                                      
         XC    COUNT,COUNT         COUNT OF SYSTEMS                             
         LA    R8,WORK                                                          
         MVC   WORK,SPACES                                                      
         MVI   SELSYSF,0           FLAG FOR SYSTEM FILTER                       
         LA    R3,SA0DATA          GET ELEMENT DATA                             
*                                                                               
FR20     CLI   0(R3),0             E-O-R                                        
         BE    FR24                                                             
         CLI   0(R3),SASYSELQ                                                   
         BE    FSYS                FILTER ON SYSTEM                             
FR22     ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     FR20                                                             
*                                                                               
FR24     OC    SELSYS,SELSYS       SYSTEM CODE FILTER INPUT?                    
         BZ    FR26                NO                                           
         CLI   SELSYSF,0           SYSTEM FOUND?                                
         BE    FR02                NO - NEXT RECORD & RESTART SEQUENCE          
*                                                                               
FR26     L     R2,AIOAREA3         FILTER ON COMPATIBLE USERID                  
         OC    SELUSR,SELUSR                                                    
         BZ    FR28                                                             
         MVC   APWORK,SELUSR                                                    
         BAS   RE,FILTUSER         FILTER USER ID COMPATIBLE                    
         BE    FR28                YES                                          
         B     FR02                NO - NEXT RECORD & RESTART SEQUENCE          
         SPACE 1                                                                
FR28     CLI   SELAGRF,0           CHECK ACCESS GROUP FOUND                     
         BNE   *+14                YES                                          
         OC    SELAGR,SELAGR                                                    
         BNZ   FR02                                                             
         B     BLDSYS                                                           
         SPACE 2                                                                
***********************************************************************         
* SYSTEM ELEMENT FILTERING - ASSUMES R3=A(ELEMENT)                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SASYSD,R3                                                        
FSYS     OC    SELSYS,SELSYS       ARE WE FILTERING ON A SYSTEM?                
         BZ    FSYS100             NO                                           
         CLC   SASYSNUM,SELSYS     IS THIS THE ONE WE WANT?                     
         BNE   FSYS100             NO                                           
         MVI   SELSYSF,1           FLAG SYSTEM FOUND                            
*                                                                               
         OC    SELPGM,SELPGM       FILTERING ON SYS/PRG IN SHORT REPORT         
         BZ    FSYS100                                                          
         MVC   PROGRAM,SELPGM      JUST PUT OUT PROG=XXXX                       
         ST    R3,ASYSEL                                                        
         BAS   RE,GETPAVAL         FIND PROGRAM IN SYSTEM ELEMENT               
         BNE   FSYS010             NOT FOUND                                    
*                                                                               
         CLC   PACCVAL,NAUTH       PROGRAM AUTHORISED?                          
         BE    FR02                NO - GET NEXT RECORD                         
         BAS   RE,PROGDIS                                                       
         B     FR24                YES - DISPLAY PROGRAM                        
*                                                                               
FSYS010  CLC   SASYSALL,NAUTH      PROGRAM NOT FOUND - TEST DEFAULT             
         BE    FR02                IF DEFAULT NOT AUTH - GET NEXT               
         BAS   RE,PROGDIS                                                       
         B     FR24                                                             
         SPACE  1                                                               
FSYS100  CLI   SELSYSF,1           FOUND SYSTEM?                                
         BE    FR24                YES                                          
         B     FR22                                                             
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST OF VALID SYSTEMS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SASYSD,R3                                                        
BLDSYS   L     R2,AIOAREA3                                                      
         LA    R8,WORK                                                          
         MVC   WORK,SPACES                                                      
         LA    R3,SA0DATA          GET ELEMENT DATA                             
*                                                                               
BSYS02   CLI   0(R3),0             E-O-R                                        
         BE    BSYS08                                                           
         CLI   0(R3),SASYSELQ                                                   
         BE    BSYS06                                                           
BSYS04   ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         LA    R3,0(RF,R3)                                                      
         B     BSYS02                                                           
         SPACE 1                                                                
BSYS06   GOTO1 ADISSYS,SASYSNUM                                                 
         MVC   0(7,R8),APWORK      INSERT NAME                                  
         LA    R8,6(R8)                                                         
         CLI   0(R8),C' '          SQUASH OUT SPACES                            
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C','          PUT A COMMA AFTER IT                         
         LA    R8,2(R8)                                                         
         B     BSYS04                                                           
*                                                                               
BSYS08   LA    RF,WORK                                                          
         CR    R8,RF                                                            
         BNH   *+10                                                             
         BCTR  R8,0                                                             
         MVI   0(R8),C' '                                                       
         MVC   RPTSYS,WORK         MOVE IN LIST OF SYSTEMS                      
         B     GETINF                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE GET INFORMATION FROM RECORD FOR ALL REPORTS                 *         
***********************************************************************         
         SPACE 1                                                                
GETINF   L     R2,AIOAREA1                                                      
         USING SAPEREC,R2                                                       
         LA    R3,SAPEDATA                                                      
GINF02   CLI   0(R3),0             E-O-R                                        
         BE    GINF06                                                           
         CLI   0(R3),SAACVELQ                                                   
         BE    GRACV                                                            
         CLI   0(R3),SANAMELQ                                                   
         BE    GRNAM                                                            
*                                                                               
GINF04   ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         LA    R3,0(RF,R3)                                                      
         B     GINF02                                                           
*                                                                               
GINF06   L     R2,AIOAREA3                                                      
         CLI   RPTFMT,0            TEST FULL REPORT REQUIRED                    
         BNE   PRX02               YES                                          
         B     PRS02                                                            
         SPACE 2                                                                
***********************************************************************         
* ACTIVITY ELEMENT                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SAACVD,R3                                                        
GRACV    GOTO1 VDATCON,APPARM,(3,SAACVDT),(8,RPTDATE)                           
         B     GINF04                                                           
         SPACE 2                                                                
***********************************************************************         
* NAME ELEMENT                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING SANAMD,R3                                                        
GRNAM    XR    RF,RF                                                            
         IC    RF,SANAMLN                                                       
         SH    RF,=Y(SANAMLNQ)                                                  
         LA    RE,SANAMES          NAMES FROM HERE ...                          
         MVC   RPTNAME,SPACES                                                   
         LA    R8,RPTNAME          TO HERE                                      
         XR    R1,R1                                                            
*                                                                               
NAM02    IC    R1,0(RE)            LENGTH OF NAME                               
         BCTR  R1,0                                                             
         EX    R1,*+4              MOVE IN NAME                                 
         MVC   0(0,R8),1(RE)                                                    
*                                                                               
         LA    RE,2(R1,RE)         NEXT NAME IN SERIES                          
         LA    R8,2(R1,R8)         SPACE BETWEEN NAMES ON REPORT                
*                                                                               
         LA    R1,2(R1)            R1=LENGTH OF NAME SUB-EL                     
         SR    RF,R1                                                            
         BP    NAM02               DO NEXT NAME                                 
         B     GINF04              NOTHING LEFT TO MOVE OUT                     
         SPACE 2                                                                
***********************************************************************         
* PRINT STANDARD REPORT HERE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R2                                                        
PRS02    TM    REPIND1,REPIOFF     BOXES ONLY ALLOWED OFF-LINE                  
         BZ    PRS06                                                            
*                                                                               
         L     RF,REPABOX                                                       
         USING BOXD,RF                                                          
         OI    BOXDDCTL,BOXDDLC                                                 
         MVI   BOXYORN,C'Y'        NEED BOTH OF THESE OR IT WON`T PRINT         
         MVI   BOXOFF,C'N'         BOXES AT ALL                                 
         MVI   BOXROWS+L'BOXROWS-1,C'B' FORCES LINE AT END OF PAGE              
         OI    REPHEADI,REPHOPEN   OPEN BOX AFTER HEADER                        
         OI    REPMIDSI,REPMMLIN   BOX HORIZONTAL AFTER MIDLINES                
P        USING REPAD,BOXCOLS                                                    
         CLI   OPTPWS,C'Y'         PASSWORD WANTED?                             
         BE    PRS04               YES                                          
         SPACE 1                                                                
         MVI   P.PRTC1,C'L'        COLUMNS FOR NO PASSWORD                      
         MVI   P.PRTC2,C'C'        IN SHORT REPORT                              
         MVI   P.PRTC3,C'C'                                                     
         MVI   P.PRTC4,C'C'                                                     
         MVI   P.PRTC5,C'R'                                                     
         B     PRS06                                                            
         SPACE 1                                                                
PRS04    MVI   P.PRPC1,C'L'        COLUMNS FOR PASSWORD ALLOWED                 
         MVI   P.PRPC2,C'C'        IN SHORT REPORT                              
         MVI   P.PRPC3,C'C'                                                     
         MVI   P.PRPC4,C'C'                                                     
         MVI   P.PRPC5,C'C'                                                     
         MVI   P.PRPC6,C'R'                                                     
         DROP  P                                                                
         DROP  RF                                                               
*                                                                               
         PUSH  USING                                                            
         USING REPAD,REPP1                                                      
PRS06    CLI   OPTPWS,C'Y'         PASSWORD?                                    
         BNE   PRS10               NO                                           
         MVC   PRPPID,RPTPID       MOVE FIELDS INTO LIST LINE                   
         CLI   PWDAFLAG,0          PASSWORD?                                    
         BNE   *+10                NO                                           
         MVC   PRPCODE,RPTCODE                                                  
         CLI   EXPFLAG,C'N'        CHECK EXPIRED FLAG                           
         BE    *+8                                                              
         MVI   PRPEXPF,C'*'                                                     
         MVC   PRPDEF1,RPTDEF1     EFFECTIVE DATE                               
         MVC   PRPDATE,RPTDATE     ACTIVITY DATE                                
         MVC   REPM1+62(L'CT@VALS),CT@VALS                                      
         OC    SELPGM,SELPGM       FILTER ON PROGRAM?                           
         BNZ   PRS08               YES                                          
*                                                                               
         MVC   PRPSYS,RPTSYS       VALID SYSTEMS IF NOT PROGRAM                 
         B     PRS14                                                            
*                                                                               
PRS08    MVC   REPM1+62(L'CT@VALUS),CT@VALUS                                    
         MVC   PRPSYS(L'RPTSPG),RPTSPG                                          
         B     PRS14                                                            
*                                                                               
PRS10    MVC   PRTPID,RPTPID       MOVE FIELDS INTO LIST LINE                   
         CLI   EXPFLAG,C'N'        CHECK EXPIRED FLAG                           
         BE    *+8                                                              
         MVI   PRTEXPF,C'*'                                                     
         MVC   PRTDEF1,RPTDEF1                                                  
         MVC   PRTDATE,RPTDATE                                                  
         MVC   REPM1+55(L'CT@VALS),CT@VALS                                      
         OC    SELPGM,SELPGM                                                    
         BZ    PRS12                                                            
         MVC   REPM1+55(L'CT@VALUS),CT@VALUS                                    
         MVC   PRTSYS(L'RPTSPG),RPTSPG                                          
         B     PRS14                                                            
*                                                                               
PRS12    MVC   PRTSYS,RPTSYS                                                    
*                                                                               
PRS14    GOTO1 VREPORT,REPD                                                     
         B     FR02                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT EXTENDED REPORT HERE                                          *         
***********************************************************************         
         SPACE 1                                                                
PRX02    XC    COUNT1,COUNT1       CLEAR PROGRAM COUNT                          
         XR    R3,R3                                                            
         GOTO1 VHELLO,APPARM,(C'G',=C'CTFBIG'),('SASYSELQ',SA0REC),0            
         CLI   12(R1),0                                                         
         BNE   PRX10               NO SYSTEM ELEMENTS TO BE FOUND               
         XR    RF,RF                                                            
         L     R3,12(R1)                                                        
         USING SASYSD,R3                                                        
*                                                                               
PRX04    CLI   SASYSEL,0           EOR                                          
         BNE   PRX05                                                            
         XR    R3,R3                                                            
         B     PRX10                                                            
*                                                                               
PRX05    CLI   SASYSEL,SASYSELQ    SYSTEM ELEMENT?                              
         BNE   PRX06                                                            
*                                                                               
         CLI   RPTFMT,X'FF'        'ALL' SYSTEMS                                
         BE    PRX08               YES - GET DETAILS                            
         CLC   RPTFMT,SASYSNUM     IS THIS SELECTED SYSTEM                      
         BE    PRX08               YES - GET DETAILS                            
*                                                                               
PRX06    IC    RF,SASYSLN                                                       
         LA    R3,0(R3,RF)                                                      
         B     PRX04               GET NEXT SYSTEM ELEMENT                      
*                                                                               
PRX08    MVC   SYSTEM,SASYSNUM                                                  
         BAS   RE,GETASF                                                        
         GOTO1 PRNTSYS             GET PROGRAM DETAILS FOR THIS SYSTEM          
*                                                                               
PRX10    ZIC   RF,COUNT1           NUMBER OF ENTRIES IN THIS BLOCK              
         SRL   RF,2                CALCULATE SIZE OF ENTRY                      
         LA    RF,16(RF)           FOR THE INFORMATION ABOVE THE BLOCK          
         XR    R1,R1                                                            
         IC    R1,REPLINE          ADD TO CURRENT LINE POSITION                 
         LA    RF,0(R1,RF)                                                      
         CLM   RF,1,REPMAXL        WILL IT FIT ON THE PAGE                      
         BNH   *+12                                                             
         OI    REPHEADI,REPHFRCE   NO SO FORCE NEW PAGE                         
         NI    REPHEADI,X'FF'-(REPHSPAC)                                        
*                                                                               
         LA    R8,REPP1                                                         
         USING PRLREPD,R8                                                       
         TM    REPIND1,REPIOFF     RUNNING OFFLINE?                             
         BZ    PRX12               NO                                           
*                                                                               
         L     RF,REPABOX          BOXES ONLY ALLOWED OFFLINE                   
         USING BOXD,RF                                                          
         TM    REPHEADI,REPHFRCE   NEW PAGE ?                                   
         BO    *+8                 YES                                          
         MVI   BOXREQ,C'B'         NO - CLOSE OFF AFTER LAST ONE                
*                                                                               
         OI    REPHEADI,REPHOPEN   OPEN BOX AFTER HEADER                        
         NI    REPMIDSI,X'FF'-(REPMSPAC)                                        
         MVI   BOXYORN,C'Y'        NEED BOTH OF THESE                           
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXROWS+L'BOXROWS-1,C'B'                                         
*                                                                               
         LA    RE,BOXCOLS          COLUMNS FOR THE MAIN DISPLAY BOX             
         MVI   0(RE),C'L'                                                       
         LA    RE,L'BOXCOLS-1(RE)                                               
         MVI   0(RE),C'R'                                                       
         DROP  RF                                                               
         TM    REPHEADI,REPHFRCE   NEW PAGE ?                                   
         BO    PRX12               YES                                          
         GOTO1 VREPORT,REPD        START THE BOX                                
***********************************************************************         
*              PERSON ID DEALT WITH HERE                              *         
***********************************************************************         
PRX12    MVC   PRLPIDD(L'CT@PID),CT@PID                                         
         MVC   PRLPID,RPTPID                                                    
         MVC   PRLNAME,RPTNAME                                                  
         GOTO1 VREPORT,REPD        PRINT PERSON ID                              
*                                                                               
         TM    REPIND1,REPIOFF     BOXES ONLY ALLOWED OFF-LINE                  
         BZ    PRX14                                                            
         L     RF,REPABOX                                                       
         USING BOXD,RF                                                          
         MVI   BOXREQ,C'B'         MID LINE WANTED UNDER PERSON ID              
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD        PRINT MIDLINE                                
         DROP  RF                                                               
***********************************************************************         
*              PASSWORD - OPTIONAL OUTPUT                             *         
***********************************************************************         
PRX14    CLI   OPTPWS,C'Y'         PASSWORD WANTED?                             
         BNE   PRX16               NO                                           
         MVC   PRLCODED(L'CT10PWD),CT10PWD     PASSWORD                         
         MVC   PRLCODE,RPTCODE                                                  
         MVC   PRLDATED(L'CT@LCHNG),CT@LCHNG   DATE LAST CHANGED                
         MVC   PRLDATE,RPTDATE                                                  
         CLI   EXPFLAG,C'N'        CHECK EXPIRED FLAG                           
         BE    *+8                                                              
         MVI   PRLEXPF,C'*'                                                     
         LA    R8,L'REPP1(R8)      NEXT LINE                                    
***********************************************************************         
*              USERID - DEFAULT IS 'ALL'                              *         
***********************************************************************         
PRX16    MVC   PRLUSRD(L'CT@USRIS),CT@USRIS                                     
         MVC   PRLUSR(L'CT@ALL),CT@ALL                                          
*                                                                               
         BAS   RE,DISIDS           GET USERID DETAILS INTO BLOCK                
         LA    RE,BLOCK            WHERE DETAILS ARE CURRENTLY                  
         LA    RF,PRLUSR           WHERE THEY ARE TO BE PRINTED                 
         XR    R1,R1                                                            
         ICM   R1,1,BLKCNT         COUNT OF USERIDS                             
         BZ    PRX22               NONE TO DISPLAY                              
*                                                                               
PRX18    LA    R0,PRLUSR+L'PRLUSR-10                                            
         CR    RF,R0               WILL NEXT USERID FIT?                        
         BNH   PRX20               YES                                          
         BCTR  RF,0                                                             
         MVI   0(RF),C' '          REMOVE COMMA FROM END OF THIS LINE           
         LA    R8,L'REPP1(R8)      GO DOWN TO START OF NEXT LINE                
         LA    RF,PRLUSR                                                        
         LA    R0,PRLUSR+L'PRLUSR-10                                            
*                                                                               
PRX20    MVC   0(10,RF),0(RE)      INSERT USERID INTO NEXT GAP ON LINE          
         LA    RF,9(RF)                                                         
         CLI   0(RF),C' '          SQUASH OUT SPACES                            
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C','          INSERT ',' AFTER THIS ID                     
         LA    RF,2(RF)            NEXT FREE SPACE ON LINE                      
         LA    RE,10(RE)           NEXT UNDISPLAYED USERID                      
         BCT   R1,PRX18            GET NEXT USERID                              
*                                                                               
         BCTR  RF,0                                                             
         MVI   0(RF),C' '          REMOVE ',' AFTER LAST ID                     
***********************************************************************         
*              VALID SYSTEM LIST                                      *         
***********************************************************************         
PRX22    LA    R8,L'REPP1(R8)      NEXT FREE PRINT LINE                         
         CLC   RPTSYS,SPACES       ANY VALID SYSTEMS?                           
         BNH   PRX30               NO                                           
*                                                                               
         MVC   PRLVSD(L'CT@VALS),CT@VALS  SYSTEM VALUES LINE                    
         MVC   PRLVS(L'RPTSYS),RPTSYS     MOVE IN SYSTEM NAMES                  
         LA    R8,L'REPP1(R8)      NEXT FREE LINE                               
         B     PRX30                                                            
*                                                                               
PRX26    GOTO1 PRNTSYS             PROGRAM DETAILS FOR THIS SYSTEM              
*                                                                               
PRX28    ZIC   RF,COUNT1           NUMBER OF ENTRIES IN THIS BLOCK              
         SRL   RF,2                CALCULATE SIZE OF ENTRY                      
         LA    RF,16(RF)           FOR THE INFORMATION ABOVE THE BLOCK          
*                                                                               
         XR    R1,R1               LINES FOR USERID/TITLES, ETC.                
         IC    R1,REPLINE          ADD TO CURRENT LINE POSITION                 
         LA    RF,0(R1,RF)                                                      
         CLM   RF,1,REPMAXL        WILL IT FIT ON THE PAGE                      
         BH    PRX29               NO                                           
         GOTO1 VREPORT,REPD        PRINT TITLES AND USERIDS                     
         B     PRX30                                                            
*                                                                               
PRX29    OI    REPHEADI,REPHFRCE   NO SO FORCE NEW PAGE                         
         NI    REPHEADI,X'FF'-(REPHSPAC)                                        
         OI    REPHEADI,REPHOPEN   OPEN BOX AFTER HEADER                        
         MVC   PRLPIDD(L'CT@PID),CT@PID                                         
         MVC   PRLPID,RPTPID                                                    
         MVC   PRLNAME,RPTNAME                                                  
         GOTO1 VREPORT,REPD        PRINT PERSON ID  AGAIN                       
*                                                                               
         TM    REPIND1,REPIOFF     BOXES ONLY ALLOWED OFF-LINE                  
         BZ    PRX30                                                            
         L     RF,REPABOX                                                       
         USING BOXD,RF                                                          
         MVI   BOXREQ,C'B'         MID LINE WANTED UNDER PERSON ID              
         GOTO1 VREPORT,REPD        PRINT MIDLINE                                
*                                                                               
PRX30    LA    R8,REPP1            PRINT A CLEAR LINE UNDERNEATH                
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         LTR   R3,R3               ANY SYSTEM ELEMENTS MATCHED?                 
         BZ    PRX54               NO                                           
***********************************************************************         
*              SYSTEM FOR THIS LIST OF PROGRAMS                       *         
***********************************************************************         
         GOTO1 ADISSYS,SASYSNUM                                                 
         MVC   PRLLISD(L'CT@PLIST),CT@PLIST                                     
         MVC   PRLLIS(7),APWORK    SYSTEM NAME                                  
***********************************************************************         
*              LIMIT ACCESS VALUE FOR THIS SYSTEM                     *         
***********************************************************************         
         GOTO1 ADISLACC,APPARM,(SASYSNUM,SASYSLMT) BUILD LIMIT ACCESS           
         OC    APWORK(12),APWORK                                                
         BZ    PRX32              'ALL' - NO NEED TO DISPLAY THIS               
         MVC   PRLLIMD(L'CT@LIMAC),CT@LIMAC                                     
         MVC   PRLLIM(12),APWORK   MOVE IN LIMIT ACCESS VALUES                  
*                                                                               
PRX32    GOTO1 VREPORT,REPD        PRINT THIS DATA                              
*                                                                               
         LA    R8,REPP1            PRINT A CLEAR LINE UNDERNEATH                
         OI    REPPRNTI,REPPSPAC                                                
         GOTO1 VREPORT,REPD                                                     
***********************************************************************         
*              PROGRAM/VALUE BOX HANDLED HERE                         *         
***********************************************************************         
         CLI   COUNT1,0                                                         
         BE    PRX52                                                            
         MVI   REPPRNSA,0          NO SPACES WANTED NOW                         
         TM    REPIND1,REPIOFF     RUNNING OFFLINE?                             
         BZ    PRX34               NO                                           
         L     RF,REPABOX          BOXES ONLY ALLOWED OFFLINE                   
         USING BOXD,RF                                                          
         MVI   BOXYORN,C'Y'        NEED BOTH OF THESE                           
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXREQ,C'N'         NEW BOX                                      
         MVI   BOXROWS+L'BOXROWS-1,C'B'                                         
         LA    RE,BOXCOLS          COLUMNS FOR THE DISPLAY BOX                  
         USING BLKDISP,RE                                                       
         MVI   BLKC1,C'L'                                                       
         MVI   BLKC2,C'C'                                                       
         MVI   BLKC3,C'C'                                                       
         MVI   BLKC4,C'C'                                                       
         MVI   BLKC5,C'C'                                                       
         MVI   BLKC6,C'C'                                                       
         MVI   BLKC7,C'C'                                                       
         MVI   BLKC8,C'C'                                                       
         MVI   BLKC9,C'R'                                                       
         DROP  RE,RF                                                            
         GOTO1 VREPORT,REPD                                                     
         SPACE 1                                                                
PRX34    LA    RF,REPP1                                                         
         USING BLKDISP,RF                                                       
         LA    R0,4                NUMBER OF COLUMN PAIRS                       
PRX36    MVC   BLKP1,CT@PROG                                                    
         MVC   BLKA1(L'CT@VALUE),CT@VALUE                                       
         LA    RF,BLKLEN(RF)                                                    
         BCT   R0,PRX36                                                         
         GOTO1 VREPORT,REPD        PRINT TITLE BLOCK                            
         DROP  RF                                                               
*                                                                               
         TM    REPIND1,REPIOFF     BOXES ONLY ALLOWED OFF-LINE                  
         BZ    PRX38                                                            
         L     RF,REPABOX                                                       
         USING BOXD,RF                                                          
         MVI   BOXBLANK,C'N'                                                    
         LA    RE,REPP1            SET COLUMNS TO PRINT MIDLINE                 
         USING BLKDISP,RE                                                       
         LA    R1,BLKC1                                                         
         LA    R0,BLKC9                                                         
         MVI   0(R1),BOTF                                                       
LP       CR    R1,R0                                                            
         BNL   LP1                                                              
         MVC   1(1,R1),0(R1)                                                    
         LA    R1,1(R1)                                                         
         B     LP                                                               
LP1      MVI   BLKC1,LEFTT                                                      
         MVI   BLKC2,CROSS                                                      
         MVI   BLKC3,CROSS                                                      
         MVI   BLKC4,CROSS                                                      
         MVI   BLKC5,CROSS                                                      
         MVI   BLKC6,CROSS                                                      
         MVI   BLKC7,CROSS                                                      
         MVI   BLKC8,CROSS                                                      
         MVI   BLKC9,RIGHTT                                                     
         DROP  RE,RF                                                            
         GOTO1 VREPORT,REPD        PRINT TITLE BLOCK                            
*                                                                               
PRX38    LA    RF,REPP1            PRINT PROGRAMS INTO COLUMNS                  
         USING BLKDISP,RF                                                       
         MVC   REPP1,SPACES                                                     
         ST    RF,LINEADR                                                       
         LA    RE,BLOCK1                                                        
         USING PRTBLKD,RE                                                       
         LA    R0,4                4 PAIRS PER LINE                             
         XR    R1,R1                                                            
         ICM   R1,1,COUNT1                                                      
         BZ    PRX48                                                            
PRX42    MVC   BLKP1(L'PGMNAME),PRTB                                            
         MVC   BLKA1,PRTA                                                       
         BCT   R0,PRX44                                                         
         LA    R0,4                                                             
         L     RF,LINEADR                                                       
         LA    RF,L'REPP1(RF)                                                   
         MVC   0(L'REPP1,RF),SPACES                                             
         ST    RF,LINEADR                                                       
         B     PRX46                                                            
         SPACE 1                                                                
PRX44    LA    RF,BLKLEN(RF)       4 TO DISPLAY PER LINE                        
         SPACE 1                                                                
PRX46    LA    RE,PRTBLEN(RE)                                                   
         BCT   R1,PRX42                                                         
*                                                                               
PRX48    GOTO1 VREPORT,REPD        PRINT THIS BLOCK                             
*                                                                               
         TM    REPIND1,REPIOFF     BOXES ONLY ALLOWED OFF-LINE                  
         BZ    PRX50                                                            
         L     RF,REPABOX                                                       
         USING BOXD,RF                                                          
         MVI   BOXREQ,C'N'         CLOSE THE BOX                                
         MVC   BOXCOLS,SPACES      RESET COLUMNS                                
         LA    RE,BOXCOLS          COLUMNS FOR THE MAIN BOX                     
         MVI   0(RE),C'L'                                                       
         LA    RE,L'BOXCOLS-1(RE)                                               
         MVI   0(RE),C'R'                                                       
         DROP  RF                                                               
*                                                                               
PRX50    OI    REPPRNTI,REPPSPAC   PRINT A CLEAR LINE UNDERNEATH                
         GOTO1 VREPORT,REPD                                                     
*                                                                               
PRX52    XR    RF,RF               GET NEXT SYSTEM ELEMENT                      
         IC    RF,SASYSLN                                                       
         LA    R3,0(RF,R3)                                                      
         CLI   SASYSEL,0           EOR?                                         
         BE    PRX54                                                            
         CLI   SASYSEL,SASYSELQ    SYSTEM ELEMENT?                              
         BNE   PRX52               NO                                           
*                                                                               
         CLI   RPTFMT,X'FF'        IS SYSTEM 'ALL'                              
         BE    PRX26               YES - GET DETAILS                            
         CLC   RPTFMT,SASYSNUM     IS THIS SELECTED SYSTEM                      
         BE    PRX26               YES - GET DETAILS                            
         B     PRX52               GET NEXT SYSTEM ELEMENT                      
*                                                                               
PRX54    GOTO1 VREPORT,REPD                                                     
         B     FR02                                                             
*                                                                               
PRX56    MVC   REPP1,ASTS          BUILD LINE OF ASTS                           
         GOTO1 VREPORT,REPD                                                     
         B     FR02                READ SEQUENCE BROKEN                         
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
*        TM    REPIND1,REPIOFF     BOXES ONLY ALLOWED OFF-LINE                  
*        BZ    EXIT                                                             
*        L     RF,REPABOX                                                       
*        USING BOXD,RF                                                          
*        MVI   BOXREQ,C'C'         CLOSE ANY BOXES LEFT OPEN                    
*        GOTO1 VREPORT,REPD                                                     
*        DROP  RF                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         MVI   0(R3),KEYPID                                                     
         MVI   1(R3),10                                                         
         MVC   2(8,R3),SAPEPID                                                  
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT PROGRAM=XXXX INTO RPTSPG FOR SHORT REPORT            *         
***********************************************************************         
         SPACE 1                                                                
PROGDIS  NTR1                                                                   
         MVC   RPTSPG,SPACES                                                    
         GOTO1 ADISPGM,APPARM,(SELSYS,PROGRAM)                                  
         BE    *+10                                                             
         MVC   APWORK(7),=C'???????'                                            
         MVC   RPTSPG(L'PGMNAME),APWORK                                         
         LA    RF,RPTSPG+L'PGMNAME                                              
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         BAS   RE,DISPAVAL                                                      
         MVI   1(RF),C'='                                                       
         MVC   2(4,RF),APWORK                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT PROGRAM NAME LIST AND AUTHORISATION VALUES         *         
* EXPECTS R3 TO BE POINTING TO SYSTEM ELEMENT                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SASYSD,R3                                                        
PRNTSYS  NTR1                                                                   
         ST    R3,ASYSEL           SAVE A(SYSTEM ELEMENT)                       
         MVC   SYSTEM,SASYSNUM     GET SYSTEM                                   
         GOTO1 ASETSEL,SYSTEM                                                   
         GOTO1 GSELST                                                           
         DROP  R3                                                               
         L     R1,ASE              ADDRESS OF SELIST IN ASE                     
         L     R1,SEPGMS-SELISTD(R1)                                            
         LH    RE,0(R1)                                                         
         STCM  RE,3,PLSTLEN        SAVE LENGTH PROGRAM LIST ENTRY               
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         ST    R1,APGMLST                                                       
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         LA    R4,XSORTBLK         SORT PROGRAM NAMES ALPHABETICALLY            
         XR    R8,R8               COUNT NUMBER OF PROGRAMS                     
         XC    PROGRAM,PROGRAM                                                  
         SPACE 1                                                                
PPRL02   TM    CUSTAT,CUSDDS                                                    
*        BO    *+12                IF NOT DDS TERMINAL                          
         TM    PGMIND,PGMIACC      IGNORE RESTRICTED ACCESS PROGRAM             
         BO    PPRL12                                                           
*&&UK                                                                           
         CLI   PGMCTRY,0           TEST COUNTRY CODE (NOT IN US)                
         BE    *+14                KEEP ENGLISH DEFAULT                         
         CLC   PGMCTRY,CUCTRY      AND CONNECT COUNTRY                          
         BNE   PPRL12                                                           
*&&                                                                             
         MVC   0(L'PGMNAME,R4),PGMNAME                                          
         MVC   L'PGMNAME(1,R4),PGMNUM                                           
         LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         LA    R8,1(R8)                                                         
         B     PPRL12                                                           
         SPACE 1                                                                
PPRL04   MVC   PROGRAM,PGMNUM      SAVE PROGRAM NUMBER                          
         MVC   PROGCTRY,PGMCTRY    SAVE PROGRAM COUNTRY CODE                    
         LR    R0,RF               SAVE RF                                      
         LR    RF,R1                                                            
         SR    RF,RE                                                            
         L     R3,APGMLST                                                       
         SPACE 1                                                                
PPRL06   DS    0H                                                               
*&&UK                                                                           
         CLC   CUCTRY,PGMCTRY-PGMLSTD(R3)                                       
         BE    *+14                NO SYNONYM CHECK ACROSS COUNTRIES            
         CLC   PROGCTRY,PGMCTRY-PGMLSTD(R3)                                     
         BNE   PPRL08                                                           
*&&                                                                             
         CLC   PROGRAM,PGMNUM-PGMLSTD(R3)                                       
         BE    PPRL10              AVOID SYNONOMOUS PROGRAMS                    
         B     PPRL08                                                           
         SPACE 1                                                                
PPRL08   BXLE  R3,RE,PPRL06                                                     
         LR    RF,R0                                                            
         B     PPRL02                                                           
*                                                                               
PPRL10   LR    RF,R0                                                            
*                                                                               
PPRL12   BXLE  R1,RE,PPRL04                                                     
*                                                                               
         ST    R1,APGMLSTX                                                      
         LA    R4,XSORTBLK                                                      
         LA    R0,L'PGMNAME                                                     
         LA    R3,L'PGMNAME+L'PGMNUM                                            
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R8),(R3),(R0),0                      
         LTR   R8,R8                                                            
         BZ    PPRLX               EXIT IF NO PROGRAMS IN LIST                  
         SPACE 1                                                                
         XC    COUNT1,COUNT1                                                    
         LA    R3,XSORTBLK                                                      
*                                                                               
         LA    R2,BLOCK1           BUILD PROGRAM CODE SAVE TABLE                
         USING PRTBLKD,R2                                                       
PPRL14   ICM   R1,15,APGMLST       GET A(PGMLST ENTRY)                          
*                                                                               
PPRL16   CLC   L'PGMNAME(1,R3),PGMNUM-PGMLSTD(R1)                               
         BE    PPRL18                                                           
         SR    RF,RF                                                            
         ICM   RF,3,PLSTLEN                                                     
         LA    R1,0(RF,R1)                                                      
         CLM   R1,15,APGMLSTX      GET A(END PGMLST)                            
         BL    PPRL16                                                           
         DC    H'0'                                                             
*                                                                               
PPRL18   GOTO1 TSTAGYLA,(R1)       TEST RESTRICTED AGENCY ACCESS LIST           
         BNE   PPRL26                                                           
*                                                                               
         MVC   APBYTE,L'PGMNAME(R3) PROGRAM NUMBER                              
         GOTO1 ATSTPGM,APBYTE      CHECK USER PROGRAM AUTH<>NO                  
         BE    PPRL22                                                           
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR RETURN MESSAGE                         
*                                  BRANCH OUT HERE IN UK ONLY                   
*&&UK*&& B     PPRL26                                                           
*                                  DO THIS BIT IN US ONLY                       
         OC    ASYSEL,ASYSEL                                                    
         BZ    PPRL26                                                           
         MVC   PROGRAM,APBYTE      CHECK AUTH VALUE ALREADY PRESENT             
         BAS   RE,GETPAVAL         IN SYSTEM ELEMENT                            
         BNE   PPRL20                                                           
         SPACE 1                                                                
         CLC   PACCVAL,NAUTH                                                    
         BNE   PPRL22              IF AUTH<>N DISPLAY PROGRAM                   
         B     PPRL26                                                           
         SPACE 1                                                                
PPRL20   L     RE,ASYSEL           CHECK AUTH TAKEN FROM DEFAULT                
         USING SASYSD,RE                                                        
         CLC   SASYSALL,NAUTH                                                   
         BE    PPRL26              IF AUTH<>N DISPLAY PROGRAM                   
         B     PPRL22                                                           
         SPACE 1                                                                
PPRL22   MVC   PRTB(L'PGMNAME),0(R3) DISPLAY PROGRAM NAME                       
         SPACE 1                                                                
         MVC   PROGRAM,APBYTE      CHECK AUTH VALUE ALREADY PRESENT             
         BAS   RE,GETPAVAL         IN SYSTEM ELEMENT                            
         BE    PPRL24                                                           
         SPACE 1                                                                
         BAS   RE,TSTASF           TEST FOR CONVERTED PROGRAM                   
         BNE   PPRL23                                                           
         XC    PRTA,PRTA                                                        
         MVI   PRTA,C'C'                                                        
         B     PPRL25                                                           
         SPACE 1                                                                
         USING SASYSD,RE                                                        
PPRL23   L     RE,ASYSEL           CHECK AUTH TAKEN FROM DEFAULT                
         MVC   PACCVAL,SASYSALL                                                 
         DROP  RE                                                               
         SPACE 1                                                                
PPRL24   BAS   RE,DISPAVAL                                                      
         MVC   PRTA,APWORK                                                      
                                                                                
PPRL25   LA    R2,L'PGMNAME+4(R2)                                               
         ZIC   RF,COUNT1                                                        
         LA    RF,1(RF)                                                         
         STC   RF,COUNT1                                                        
         SPACE 1                                                                
PPRL26   LA    R3,L'PGMNAME+L'PGMNUM(R3)                                        
         BCT   R8,PPRL14                                                        
*                                                                               
PPRLX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                      *         
***********************************************************************         
         SPACE 1                                                                
GSELST   NTR1                                                                   
         L     R3,ASYS                                                          
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
         EJECT                                                                  
***********************************************************************         
*  BUILD ID LIST INTO BLOCK                                           *         
*  R2 =  RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R2                                                        
DISIDS   NTR1                                                                   
         MVI   BLKCNT,0                                                         
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'20'        GET ID ELEMENT                               
         GOTO1 AGETELS,SA0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DISIDX              NOT FOUND END                                
         LA    R4,BLOCK                                                         
*                                                                               
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
* TEST RESTRICTED AGENCY ACCESS LIST IN PGMAGYLA                      *         
***********************************************************************         
         SPACE 1                                                                
TSTAGYLA NTR1                                                                   
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         SR    RF,RF                                                            
         ICM   RF,7,PGMAGYLA                                                    
         BZ    TALAOK              ALL AGENCIES VALID                           
*                                                                               
TALA010  CLC   0(2,RF),SPACES      END OF LIST, AGENCY INVALID                  
         BE    TALANO                                                           
         CLC   0(2,RF),AGENCYID    AGENCY FOUND IN LIST                         
         BE    TALAOK                                                           
         LA    RF,2(RF)                                                         
         B     TALA010                                                          
*                                                                               
TALANO   B     NO                                                               
TALAOK   B     YES                                                              
         DROP  R1                                                               
         SPACE 2                                                                
********************************************************************            
*   GET PROGRAM ACCESS CODE VALUE FROM SYSTEM ELEMENT              *            
*   ON INPUT PROGRAM 1 BYTE PROGRAM CODE, R3 POINTS TO SYSTEM ELEM.*            
*   ON OUTPUT PACCVAL 2 BYTE ACCESS CODE IF FOUND ELSE CC .NE.     *            
********************************************************************            
         SPACE 1                                                                
         USING SASYSD,R3                                                        
GETPAVAL NTR1                                                                   
         L     R3,ASYSEL                                                        
         XC    PACCVAL,PACCVAL                                                  
         LA    R1,SASYSPGM         POINT TO SYSTEM ELEMENT                      
         ZIC   RE,SASYSLN                                                       
*                                  FIND PROGRAM IN ELEMENT                      
GPAV010  CH    RE,=Y(SASYSLNQ)                                                  
         BNH   GPAVNO              END OF ELEMENT                               
         CLC   PROGRAM,0(R1)                                                    
         BE    GPAVYES             PROGRAM FOUND                                
         LA    R1,L'SASYSPGM(R1)   GET NEXT PROGRAM                             
         SH    RE,=Y(L'SASYSPGM)                                                
         B     GPAV010                                                          
*                                                                               
GPAVNO   MVC   PACCVAL,SASYSALL    PROGRAM NOT FOUND                            
         B     NO                                                               
*                                                                               
GPAVYES  MVC   PACCVAL,1(R1)       SAVE ACCESS CODE VALUE                       
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*   FORMAT PROGRAM ACCESS CODE VALUE FOR DISPLAY                      *         
*   PACCVAL 2 BYTE INPUT CODE, OUTPUT TEXT IN APWORK                  *         
***********************************************************************         
         SPACE 1                                                                
DISPAVAL NTR1                                                                   
         XC    APWORK,APWORK                                                    
         CLC   YAUTH,PACCVAL                                                    
         BNE   DPAV010                                                          
         MVC   APWORK(1),CT@YES    PROG=Y                                       
         B     DPAVX                                                            
DPAV010  CLC   NAUTH,PACCVAL                                                    
         BNE   DPAV020                                                          
         MVC   APWORK(1),CT@NO     PROG=N                                       
         B     DPAVX                                                            
*                                  OUTPUT HEX 4 CHAR VALUE                      
DPAV020  GOTO1 VHEXOUT,APPARM,PACCVAL,APWORK,2,=C'TOG'                          
*                                                                               
DPAVX    B     EXIT                                                             
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
***********************************************************************         
* ROUTINE TO FIND IF USER ID IN APWORK IS COMPATIBLE WITH RECORD      *         
* R2 = A(RECORD)                                                      *         
***********************************************************************         
         SPACE 1                                                                
FILTUSER NTR1                      BUILD COMPATIBLE ID TABLE                    
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),ATIA,VDMGR                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               IO ERROR                                     
         CLI   0(R1),0                                                          
         BE    YES                 NULL TABLE SO CAN ACCESS ANY                 
         L     R1,4(R1)                                                         
FUSER010 CLI   0(R1),X'FF'         TEST E-O-L                                   
         BE    NO                                                               
         CLC   0(10,R1),=CL10'ALL'                                              
         BNE   FUSER020                                                         
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BZ    FUSER030                                                         
         B     YES                                                              
FUSER020 CLC   0(10,R1),APWORK     MATCH ID WITH TABLE                          
         BE    YES                                                              
FUSER030 LA    R1,12(R1)           GET NEXT TABLE ENETRY                        
         B     FUSER010                                                         
         EJECT                                                                  
***********************************************************************         
* GET AGENCY SYSTEM SECURITY FLAGS AND SAVE IN SAVASF                 *         
***********************************************************************         
GETASF   NTR1  ,                                                                
         XC    SAVASF(L'SAVASF),SAVASF                                          
         L     R4,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCYID                                                
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GASFX                                                            
*                                                                               
         LA    R3,CT5DATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
         USING CTSYSEL,R3                                                       
GASF010  CLI   0(R3),0                                                          
         BE    GASFX                                                            
         CLI   CTSYSEL,CTSYSELQ                                                 
         BE    GASF030                                                          
*                                                                               
GASF020  IC    R0,1(R3)            DO NEXT ELEMENT                              
         AR    R3,R0                                                            
         B     GASF010                                                          
*                                                                               
GASF030  CLC   SYSTEM,CTSYSNUM                                                  
         BNE   GASF020                                                          
         CLI   CTSYSLEN,X'18'                                                   
         BNE   GASFX                                                            
         MVC   SAVASF,CTSYSPGM                                                  
         B     GASFX                                                            
*                                                                               
GASFX    MVC   FVMSGNO,=AL2(FVFOK) IGNORE IO ERROR MESSAGE                      
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* TEST IF PROGRAM AUTH IS TO BE DISPLAYED AS 'C' CONVERTED TO NEW SEC.*         
***********************************************************************         
TSTASF   NTR1  ,                                                                
*&&US*&& B     TASFNO              UK ONLY                                      
         CLI   SYSTEM,X'04'        MEDIA SYSTEM ONLY                            
         BNE   TASFNO                                                           
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         ICM   R1,15,APGMLST                                                    
         BZ    TASFNO                                                           
TASF010  CLC   PROGRAM,PGMNUM                                                   
         BE    TASF020                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PLSTLEN                                                     
         LA    R1,0(RF,R1)                                                      
         CLM   R1,15,APGMLSTX      GET A(END PGMLST)                            
         BL    TASF010                                                          
         DC    H'0'                                                             
*                                                                               
TASF020  TM    PGMIND2,PGMISECA    TEST PROGRAM SUPPORTS NEW SECURITY           
         BZ    TASFNO                                                           
         TM    PGMIND2,PGMISECB    TEST PROGRAM SUPPORTS OLD SECURITY           
         BZ    TASFOK              MUST BE CONVERTED                            
         DROP  R1                                                               
*                                                                               
         LA    RE,1                TEST AGENCY SYSTEM SECURITY FLAGS            
         SLL   RE,31               FOR PROGRAM IN 64 BIT MASK                   
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SRDL  RE,0(R1)                                                         
         LTR   RE,RE                                                            
         BZ    TASF030                                                          
         ICM   RF,15,SAVASF        FIRST GROUP OF FOUR BYTES                    
         NR    RE,RF                                                            
         BZ    TASFNO                                                           
         B     TASFOK                                                           
*                                                                               
TASF030  ICM   RE,15,SAVASF+4      SECOND GROUP OF FOUR BYTES                   
         NR    RF,RE                                                            
         BZ    TASFNO                                                           
         B     TASFOK                                                           
*                                                                               
TASFNO   B     NO                  EXIT NO NOT 'C' TYPE                         
TASFOK   B     YES                 EXIT OK 'C' TYPE                             
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
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
         B     NO                  SYSTEM NAME ERROR                            
SAEPGM   MVC   FVMSGNO,=AL2(FVFEPGM)                                            
         B     NO                  SYSTEM NAME ERROR                            
SAEPFS   MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PROGRAM FILTER INVALID WOUT SYS              
SAEODF   MVC   FVMSGNO,=AL2(CE#OFFDP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  MUST ENTER OFFICE CODE FOR DEPT.             
SAEFTB   MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE EXCEEDS MAXIMUM                  
SAEPED   MVC   FVMSGNO,=AL2(CE#PEDEL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PERSONAL ID RECORD IS DELETED                
SAEGI0   MVC   FVMSGNO,=AL2(CE#GID00)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  GETIDS RETURN CODE 00                        
SAEGIF   MVC   FVMSGNO,=AL2(CE#GIDFF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  GETID RETURN CODE FF                         
SAENOW   MVC   FVMSGNO,=AL2(CE#NONOW)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID OPTION KEYWORD                       
SAEOPT   MVC   FVMSGNO,=AL2(FVFKINV)                                            
         B     NO                  SYSTEM NAME ERROR                            
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
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
DCLIST   DS    0D                  DATA DICT CONSTANT LIST                      
         DCDDL CT#EXPR,8,L                                                      
         DCDDL CT#VALS,20,L                                                     
         DCDDL CT#VALUS,20,L                                                    
         DCDDL CT#USRIS,15,L                                                    
         DCDDL CT#PLIST,15,L                                                    
         DCDDL CT#LIMAC,14,L                                                    
         DCDDL CT#PROG,8,L                                                      
         DCDDL CT#VALUE,5,L                                                     
         DCDDL CT#PID,10,L                                                      
         DCDDL CT#PSWD,10,L,LABEL=CT10PWD                                       
         DCDDL CT#LCHNG,12,L                                                    
         DC    AL1(0)                                                           
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
REPSPEC0 DS    0X                  REPORT HEADINGS SPECIFICATIONS               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#SYSL,18,L                                               
         SPEC  H2,57,CT#SYSL,18,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,4,CT#PID,10,L                                                 
         SPEC  M1,15,CT#PSWD,10,L                                               
         SPEC  M1,29,CT#EFFD,18,L                                               
         SPEC  M1,48,CT#LCHNG,12,L                                              
         SPEC  END                                                              
         SPACE 2                                                                
REPSPEC1 DS    0X                  REPORT HEADINGS SPECIFICATIONS               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#SYSL,18,L                                               
         SPEC  H2,57,CT#SYSL,18,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,3,CT#PID,10,L                                                 
         SPEC  M1,16,CT#EFFD,18,L                                               
         SPEC  M1,36,CT#LCHNG,12,L                                              
         SPEC  END                                                              
         SPACE 2                                                                
REPSPEC2 DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#SYSL,18,L                                               
         SPEC  H2,57,CT#SYSL,18,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         SPACE 1                                                                
REPSPEC3 DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#SYSL,18,L                                               
         SPEC  H2,57,CT#SYSL,18,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
* DDBOXEQUS                                                                     
       ++INCLUDE DDBOXEQUS                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB5D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT SYSTEM                          
SAVASF   DS    XL8                                                              
*                                                                               
* TABLE OF SYSTEM PROGRAM CODES AND CORRESPONDING DISPLAY ADDRESSES             
* EACH ENTRY IS 1 BYTE PROGRAM CODE, 4 BYTES DISPLAY ADDRESS (-RELO)            
*               4 BYTES DISPLAY ADDRESS (-RELO) OR 0 IF USER AUTH=NO            
*               2 BYTES PROGRAM AUTH CODE                                       
PSAVTAB  DS    64XL7               TABLE ENTRIES                                
PWDAFLAG DS    XL1                 PASSWORD FIELD ACCESS FLAG                   
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
         SPACE 1                                                                
DUB      DS    D                                                                
SAVPARM  DS    8F                                                               
APGMLST  DS    A                                                                
APGMLSTX DS    A                                                                
VSCINKEY DS    A                                                                
VXSORT   DS    A                                                                
FIELD    DS    F                                                                
         SPACE 1                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
RETURN   DS    A                                                                
WORK     DS    CL80                                                             
PWORK    DS    CL132                                                            
         SPACE 1                                                                
SAVPID   DS    CL(L'SAPEPID)       SAVE LAST PID FOR LIST FILTER                
FIRSTPID DS    CL(L'SAPEPID)       SAVE FIRST PID FOR LIST FILTER               
SAVPWD   DS    CL(L'SA0KCODE)      SAVE LAST PWD FOR LIST FILTER                
         SPACE 1                                                                
PWDCODE  DS    CL10                PASSWORD CODE SAVE                           
PWDNUM   DS    XL2                 PASSWORD NUMBER SAVE                         
PLSTLEN  DS    XL2                                                              
         SPACE 1                                                                
EXPFLAG  DS    CL1                 EXPIRED/TERM PERSON FLAG FOR GETSEL          
CURRPID  DS    CL1                 CURRENT PERSONAL ID FLAG FOR GETSEL          
         SPACE 1                                                                
TODAY    DS    XL2                 TODAYS DATE COMPRESSED BINARY                
TODAYC   DS    XL2                 COMPLEMENT OF IT                             
         SPACE 1                                                                
*                                                                               
PIDREQD  DS    XL1                 PERSONID REQUIRED WITH PASSWD=Y              
PWDTOUT  DS    XL1                 PASSWORD TIMEOUT IN DAYS                     
PWDMINLN DS    XL1                 PASSWORD MINIMUM LENGTH                      
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
         SPACE 1                                                                
SA0KSAV  DS    XL(L'SA0KEY)        PASSWORD KEY SAVE                            
         SPACE 1                                                                
BLOCK    DS    20CL32              SCANNER BLOCKS                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLKCNT   DS    XL1                                                              
         SPACE 1                                                                
SELOPT   DS    0X                  SELECT OPTIONS                               
SELPWD   DS    CL10                PASSWORD CODE FILTER                         
SELPWDL  DS    XL1                 PASSWORD CODE FILTER LENGTH                  
SELPWDF  DS    XL1                 PASSWORD FILTER FLAG                         
SELPID   DS    CL8                 PERSONAL-ID CODE FILTER                      
SELPIDL  DS    XL1                 PERSONAL-ID CODE FILTER LENGTH               
SELPIDSP DS    XL1                 PID CODE SPECIAL CHARACTER                   
SELKEYCL DS    XL1                 PID CODE LENGTH                              
SELDEFC  DS    XL2                 EFFECTIVE DATE COMPLEMENT FILTER             
SELDEFF  DS    XL1                 EFFECTIVE DATE PRESENT FLAG                  
SELUSR   DS    XL(L'SAID)          USER ID FILTER                               
SELAGR   DS    XL(L'SAAGAGR)       ACCESS GROUP FILTER                          
SELAGRF  DS    XL1                 ACCESS GROUP FILTER FLAG                     
SELOFF   DS    CL(L'SAOFOID)       OFFICE CODE FILTER                           
SELDID   DS    CL(L'SADPDID)       DEPARTMENT CODE FILTER                       
SELSYS   DS    XL1                 SYSTEM SELECT                                
SELSYSF  DS    XL1                 SYSTEM FILTER FOUND FLAG                     
SELPGM   DS    XL1                 PROGRAM SELECT                               
SELSRT   DS    CL1                 SORT SEQUENCE:                               
*                                  C'1' - PERSONAL ID WITHIN DEPT               
*                                  C'2' - FIRST NAME                            
*                                  C'3' - FIRST NAME WITHIN DEPT                
*                                  C'4' - LAST NAME                             
*                                  C'5' - LAST NAME WITHIN DEPT                 
SELOPTL  EQU   *-SELOPT                                                         
         SPACE 1                                                                
RPTWORK  DS    0CL105              REPORT WORK                                  
RPTPID   DS    CL22                PERSONAL ID                                  
RPTCODE  DS    CL10                PASSWORD CODE                                
RPTDEF1  DS    CL8                 EFFECTIVE DATE                               
RPTDEF2  DS    CL8                                                              
RPTDATE  DS    CL8                 ACTIVITY DATE                                
RPTSYS   DS    CL60                VALID SYSTEM NAMES LIST                      
RPTSPG   DS    CL15                VALID PROGRAM=XXXX                           
RPTFMT   DS    CL1                 LONG/SHORT REPORT FORMAT                     
RPTNAME  DS    CL60                PERSON NAME                                  
         SPACE 1                                                                
GETSEQF  DS    XL1                 GETSEL READ SEQUENCE BROKEN FLAG             
FLDCNT   DS    XL1                 DISPLAY FIELD COUNT                          
COUNT    DS    XL1                 COUNTER                                      
COUNT1   DS    XL1                 COUNTER                                      
PROGRAM  DS    CL1                 PROGRAM CODE                                 
PROGCTRY DS    XL1                 PROGRAM COUNTRY CODE                         
SVSYSALL DS    XL(L'SASYSALL)      SAVE OLD PROGRAM DEFAULT ACCESS CODE         
PACCVAL  DS    XL(L'SASYSALL)      PROGRAM ACCESS CODE                          
PGNAME   DS    CL8                 PROGRAM NAME SAVE                            
SYSTEM   DS    CL1                 SYSTEM SE NUMBER SAVE                        
SYCNT    DS    CL1                                                              
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
LINEADR  DS    A                   DISPLAY PROGRAM LIST LINE ADDRESS            
LINEFLD  DS    XL1                 DISPLAY PROGRAM LIST FIELD COUNT             
ASYSEL   DS    A                                                                
         SPACE 1                                                                
DSLIST   DS    0D                                                               
         DSDDL PRINT=YES                                                        
XSORTBLK DS    64XL(10)            SORT BLOCK AREA                              
LOCALX   DS    0D                                                               
         SPACE 1                                                                
REPAD    DSECT                                                                  
PRTLIN   DS    0CL(L'REPP1)        DSECT FOR SHORT REPORT                       
PRTC1    DS    XL1                 COL 1                                        
         DS    XL1                                                              
PRTPID   DS    CL8                 PERSONAL ID                                  
         DS    XL3                                                              
PRTC2    DS    XL1                 COL 2                                        
PRTEXPF  DS    XL1                 EXPIRED FLAG                                 
PRTDEF1  DS    CL8                 EFFECTIVE DATE                               
         DS    CL10                                                             
PRTC3    DS    CL1                 COL 3                                        
         DS    CL1                                                              
PRTDATE  DS    CL8                 ACTIVITY DATE                                
         DS    CL10                                                             
PRTC4    DS    CL1                 COL 4                                        
         DS    CL1                                                              
PRTSYS   DS    CL60                VALID SYSTEM NAME LIST                       
         DS    CL1                                                              
PRTC5    DS    CL1                 COL 5                                        
         SPACE 2                                                                
         ORG   REPAD                                                            
PRPLIN   DS    0CL(L'REPP1)        DSECT FOR SHORT REPORT, WITH PASSWD          
         DS    CL1                                                              
PRPC1    DS    CL1                 COL 1                                        
         DS    CL1                                                              
PRPPID   DS    CL8                 PERSONAL ID                                  
         DS    CL1                                                              
PRPC2    DS    CL1                 COL 2                                        
         DS    CL1                                                              
PRPCODE  DS    CL10                PASSWORD CODE                                
         DS    CL2                                                              
PRPC3    DS    CL1                 COL 3                                        
PRPEXPF  DS    CL1                 EXPIRED FLAG                                 
PRPDEF1  DS    CL8                 EFFECTIVE DATE                               
         DS    CL9                                                              
PRPC4    DS    CL1                 COL 4                                        
         DS    CL1                                                              
PRPDATE  DS    CL8                 ACTIVITY DATE                                
         DS    CL5                                                              
PRPC5    DS    CL1                 COL 5                                        
         DS    CL1                                                              
PRPSYS   DS    CL60                VALID SYSTEM NAME LIST                       
         DS    CL5                                                              
PRPC6    DS    CL1                 COL 6                                        
         EJECT                                                                  
PRLREPD  DSECT                     DSECT FOR FULL REPORT                        
         DS    CL3                                                              
PRLPIDD  DS    CL20                PERSONAL ID DESCRIPTOR                       
PRLPID   DS    CL8                 PERSONAL ID                                  
         DS    CL3                                                              
PRLNAME  DS    CL60                PERSON NAME                                  
         ORG   PRLPIDD                                                          
PRLCODED DS    CL20                PASSWORD CODE DESCRIPTOR                     
PRLCODE  DS    CL10                PASSWORD CODE                                
         DS    CL3                                                              
PRLDATED DS    CL19                EFFECTIVE DATE DESCRIPTOR                    
PRLEXPF  DS    CL1                 EXPIRED FLAG                                 
PRLDATE  DS    CL8                 EFFECTIVE DATE                               
         ORG   PRLPIDD                                                          
PRLUSRD  DS    CL20                USER ID DESCRIPTOR                           
PRLUSR   DS    CL109               USER ID LIST                                 
         ORG   PRLPIDD                                                          
PRLVSD   DS    CL20                VALID SYSTEMS DESCRIPTOR                     
PRLVS    DS    CL109               VALID SYSTEM LIST                            
         ORG   PRLPIDD                                                          
PRLLISD  DS    CL20                SYSTEM DESCRIPTOR FOR PROGRAM LIST           
PRLLIS   DS    CL8                 SYSTEM LIST                                  
         DS    CL5                                                              
PRLLIMD  DS    CL20                LIMIT ACCESS DESCRIPTOR                      
PRLLIM   DS    CL20 LIMIT ACCESS                                                
         EJECT                                                                  
BLKDISP  DSECT                                                                  
         DS    XL21                                                             
BLKC1    DS    XL1                                                              
         DS    XL1                                                              
BLKP1    DS    XL8                 PROGRAM                                      
         DS    XL1                                                              
BLKC2    DS    XL1                                                              
         DS    XL1                                                              
BLKA1    DS    XL4                 AUTH                                         
         DS    XL1                                                              
BLKLEN   EQU   *-BLKC1                                                          
BLKC3    DS    XL1                                                              
         DS    XL1                                                              
BLKP2    DS    XL8                 PROGRAM                                      
         DS    XL1                                                              
BLKC4    DS    XL1                                                              
         DS    XL1                                                              
BLKA2    DS    XL4                 AUTH                                         
         DS    XL1                                                              
BLKC5    DS    XL1                                                              
         DS    XL1                                                              
BLKP3    DS    XL8                 PROGRAM                                      
         DS    XL1                                                              
BLKC6    DS    XL1                                                              
         DS    XL1                                                              
BLKA3    DS    XL4                 AUTH                                         
         DS    XL1                                                              
BLKC7    DS    XL1                                                              
         DS    XL1                                                              
BLKP4    DS    XL8                 PROGRAM                                      
         DS    XL1                                                              
BLKC8    DS    XL1                                                              
         DS    XL1                                                              
BLKA4    DS    XL4                 AUTH                                         
         DS    XL1                                                              
BLKC9    DS    XL1                                                              
         SPACE 1                                                                
PRTBLKD  DSECT                                                                  
PRTB     DS    XL(L'PGMNAME)                                                    
PRTA     DS    XL4                                                              
PRTBLEN  EQU   *-PRTBLKD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108SEACS15U  05/01/02'                                      
         END                                                                    
