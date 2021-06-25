*          DATA SET ACTRA00    AT LEVEL 055 AS OF 03/13/19                      
*PHASE T62200B                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SPFMTINO                                                               
         SPACE                                                                  
         TITLE 'ACTRA00 - (T62200) - BILLING TRANSFER CONTROLLER'               
T62200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T62200**,RA,R9,R8,CLEAR=YES,RR=RE                    
         LR    R7,RC                                                            
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         MVC   COMPANY,0(R1)       SAVE COMPANY CODE                            
         MVC   ATWA,4(R1)          SAVE ADDRESS OF TWA                          
         MVC   ASYS,8(R1)          SAVE ADDRESS OF SYSFACS                      
*                                                                               
         L     R5,ATWA                                                          
         USING TWAD,R5             R5=A(TWA)                                    
*                                                                               
         LH    R6,=Y(SAVAREA-TWAD)                                              
         LA    R6,TWAD(R6)                                                      
         USING SAVAREA,R6                                                       
*                                                                               
         ST    R1,ACPARMA          SET BASE ADDRESSES                           
         ST    RE,ACRELO                                                        
         ST    RB,ACBASE1                                                       
         ST    RA,ACBASE2                                                       
         ST    R9,ACBASE3                                                       
         ST    R8,ACBASE4                                                       
         ST    RD,ACWORKA                                                       
*                                                                               
         L     RF,ASYS             A(SYSFACS)                                   
         L     RF,VCALLOV-SYSFACD(RF)                                           
         GOTO1 (RF),ACPARM,(1,0),0,0                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD CONTROLLER TABLES                 
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)          A(LOAD POINT)                                
         LA    R0,(ACSELTAB-ACRECTAB)/4+1 N'TABLES                              
         SR    RE,RE                                                            
TRA2     L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACRECTAB(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,TRA2                                                          
*                                                                               
         L     R1,0(RE,RF)         TABLE BASE FOR OPTION VALIDATION             
         AR    R1,RF               TABLES                                       
         ST    R1,ACTBASE                                                       
         OI    ACOPTIND,ACOPTITB                                                
*                                                                               
         L     RF,ASYS             A(SYSFACS)                                   
         L     RF,VCALLOV-SYSFACD(RF)                                           
         GOTO1 (RF),ACPARM,(X'10',0),0,0                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ACPOSTER,0(R1)      SAVE A(ACPOSTER)                             
         EJECT                                                                  
         L     R1,=A(CONADDRS)         SET HOOK ADDRESSES                       
         SR    RE,RE                                                            
         LA    R0,CONADDRN                                                      
TRA4     L     RF,CONADDRS(RE)                                                  
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         A     RF,ACRELO                                                        
         ST    RF,ACPHSLST(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,TRA4                                                          
*                                  SET I/O VARIABLES                            
         LA    R0,WORKD                                                         
         AH    R0,=Y(IODA1-WORKD)                                               
         ST    R0,ACIOADD         ADDRESS OF FIRST IO AREA                      
         LH    R0,=Y(IODA2-IODA1)                                               
         STH   R0,ACIOLEN         LENGTH OF EACH IO AREA                        
         MVI   ACIONUM,3          NUMBER OF IO AREAS                            
         MVI   ACIOIND,ACIOIDA+ACIOIWK                                          
*                                                                               
         MVC   ACSYSPGM,=X'0622'                                                
         MVI   ACINDS,ACINOHLP    DON'T WANT GENERALS HELP MSGS                 
         MVI   ACTWAREC,1         RELATIVE FIELD FOR RECORD                     
         MVI   ACTWAACT,2                            ACTION                     
         MVI   ACTWAKEY,3                            KEY                        
         MVI   ACTWAOPT,8                            OPTION                     
*                                                                               
         LA    R1,ACCORFST        49 IS THE                                     
         STH   R1,DATADISP        DISP TO FIRST ACC ELEMENT                     
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         LH    R1,=Y(TRAPTGH-TWAD)                                              
         STCM  R1,3,ACENDTWA                                                    
*                                                                               
         LH    R1,=Y(IOAREA1-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA1                                                      
         LH    R1,=Y(IOAREA2-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA2                                                      
         LH    R1,=Y(IOAREA3-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA3                                                      
         LH    R1,=Y(APLOCAL-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,APALOCAL                                                      
         LH    R1,=Y(PSTTBL-WORKD)                                              
         LA    R1,WORKD(R1)                                                     
         ST    R1,APSTTBL                                                       
         LH    R1,=Y(CLITAB-WORKD)                                              
         LA    R1,WORKD(R1)                                                     
         ST    R1,ACLITAB                                                       
*                                                                               
         L     R1,ACPARMA                                                       
         L     R1,0(R1)            R1=A(TIOB)                                   
         MVC   ACCURD,TIOBCURD-TIOBD(R1)  SET CURSOR DISPLACEMENT               
         MVC   ACCURS,TIOBCURS-TIOBD(R1)  SET CURSOR ABS SCREEN ADDRESS         
*                                                                               
         MVI   ACFSTIND,ACHKBEF    CONTROLLER FIRST TIME HOOK                   
         MVI   ACLFMIND,ACHKBEF    CONTROLLER BEFORE LFM ACTION HOOK            
         MVI   ACRECIND,ACHKAFT    HOOK AFTER RECORD VALIDATION                 
         MVI   ACACTIND,ACHKAFT    HOOK AFTER ACTION VALIDATION                 
         MVI   ACLSTIND,ACHKBEF    CONTROLLER LAST TIME HOOK                    
*                                  EXTENDED SELECT TABLE + BEF LSM ACT          
         MVI   ACLSMIND,ACLSMISK+ACHKBEF                                        
*                                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)                                                        
         MVC   VGETPROF,CGETPROF-COMFACSD(RF)                                   
         L     RF,CGENERAL-COMFACSD(RF)                                         
         LA    R1,WORKD                                                         
         BASR  RE,RF              CALL GENERAL CONTROLLER                       
*                                                                               
EXIT     XIT1  ,                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*==================*                                                            
* GENERAL HOOK     *                                                            
*==================*                                                            
HOOK     NTR1                                                                   
*                                                                               
         L     R5,ATWA                                                          
                                                                                
         ZIC   RF,ACMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     FIRST                                                            
         B     RECORD                                                           
         B     ACTION                                                           
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     BEFLFM                                                           
         B     BEFLSM                                                           
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         B     LAST                                                             
*                                                                               
HOOKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*===========================================================*                   
* FIRST - READ COMPANY RECORD, REQUIRE PERSON, RESTORE TWA3 *                   
*===========================================================*                   
*                                                                               
FIRST    DS    0H                                                               
         MVI   ACPFRET,PFK09      SET PFK09 TO GET NEXT SELECT IN LIST          
         LA    R2,IOKEY                                                         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,COMPANY                                                  
         L     R2,AIOAREA1                                                      
         GOTO1 MIOACCL,APPARM,IORD+IOACCFIL+IO1,=C'SE1'                         
         CLI   MYIOERR,0                                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVINOCMP)                                           
         B     FIRSTX                                                           
*                                                                               
         SR    R0,R0                                                            
         LR    R1,R2                                                            
         AH    R1,DATADISP        PT TO ELEMENTS                                
         USING CPYELD,R1                                                        
         SR    R0,R0                                                            
FIRST5   CLI   0(R1),0            END OF RECORD?                                
         BNE   *+6                                                              
         DC    H'0'               MUST HAVE X'10' ELEMENT                       
         CLI   0(R1),CPYELQ       COMPANY ELEMENT?                              
         BE    FIRST10                                                          
         ICM   R0,1,1(R1)           LENGTH OF ELEMENT                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0              BUMP TO NEXT ELEMENT                          
         B     FIRST5                                                           
*                                                                               
FIRST10  MVC   SVPROD,CPYPROD     SAVE UNIT/LEDGER FOR CLT/PRODUCT              
         MVI   SVMI,C'Y'          MI RECORDS USED                               
         TM    CPYSTAT4,CPYSMINT                                                
         BO    *+8                                                              
         MVI   SVMI,C'N'                                                        
         MVI   SVCOST,C'Y'        USE COSTING,BILL,REV                          
         TM    CPYSTAT1,CPYSCOST                                                
         BO    *+8                                                              
         MVI   SVCOST,C'N'                                                      
         MVI   SVOFF,0                                                          
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    *+8                                                              
         OI    SVOFF,SVOFFNEW      SET NEW OFFICE CODES                         
         TM    CPYSTAT1,CPYSOROE                                                
         BZ    *+8                                                              
         OI    SVOFF,SVOFFREG      SET OFFICE CODES                             
*                                                                               
         CLI   TWASCRN,0          ONLY VALIDATE IF NO OTHER SCREEN              
         BNE   FIRST20            IS LOADED (JUST FF SCREEN)                    
         CLI   TWAMODE,0          IS IT REALLY FIRST TIME                       
         BNE   FIRST15                                                          
         LA    R1,TRAPERH                                                       
         MVI   FVMINL,1           SET REQUIRED FIELD                            
         MVI   FVMAXL,L'PERSON                                                  
         GOTO1 AFVAL                                                            
         BNE   FIRST20                                                          
         MVC   PERSON,FVIFLD                                                    
FIRST15  OC    PERSON,PERSON                                                    
         BNZ   FIRST20                                                          
                                                                                
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BE    FIRST20             YES, WE DONT HAVE THIS FIELD                 
                                                                                
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     FIRSTX                                                           
*                                                                               
FIRST20  BAS   RE,RDMXPROF        READ MX PROFILE RECORD                        
*                                                                               
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BE    FIRST35             NEED TO GET YOUR OWN STORAGE                 
*                                                                               
         L     R2,ATIA            READ IN TWA3 -> TIA FOR 14336                 
         MVC   APPARM+20(2),=C'L='                                              
         MVC   APPARM+22(2),=AL2(TWAMAXRL)                                      
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(3,0),(R2),0                         
*                                                                               
         LA    RE,MAXPNUM         MOVE FROM TIA -> PSTTBL                       
         L     RF,APSTTBL                                                       
FIRST30  MVC   0(ACPRTNL,RF),0(R2)                                              
         LA    RF,ACPRTNL(RF)                                                   
         LA    R2,ACPRTNL(R2)                                                   
         BCT   RE,FIRST30                                                       
*                                                                               
         L     R2,ACLITAB         MOVE FROM TIA+6000 -> CLITAB                  
         LH    R3,=AL2(MAXCLTS)                                                 
         MH    R3,=H'3'                                                         
         L     R0,ATIA                                                          
         AH    R0,=AL2(6000)                                                    
         LR    R1,R3                                                            
         MVCL  R2,R0                                                            
*                                                                               
*                                                                               
FIRST35  TM    STABFLG,STABSET    SYSTEM TABLE SAVED                            
         BNO   FIRSTX                                                           
         LA    R0,SYSSWMAX        YES - RESTORE IT                              
         LA    RE,ASSWTAB         GENERALS TABLE                                
         LA    R1,SASSWTAB        SAVED TABLE                                   
FIRST40  MVC   0(SYSSWLEN,RE),0(R1)                                             
         LA    RE,SYSSWLEN(RE)                                                  
         LA    R1,SYSSWLEN(R1)                                                  
         BCT   R0,FIRST40                                                       
*                                                                               
FIRSTX   B     HOOKX                                                            
         DROP  R2                                                               
         EJECT                                                                  
*-------------------------------------*                                         
* RDMXPROF - READS ACC MX PROFILE     *                                         
*-------------------------------------*                                         
*                                                                               
RDMXPROF NTR1                                                                   
         XC    APWORK,APWORK        READ MX PROFILE RECORD                      
         MVI   APWORK,C'A'-X'40'    LOWER CASE A                                
         MVC   APWORK+2(2),=C'MX'                                               
         MVC   APWORK+4(1),COMPANY  NATIVE COMPANY CODE                         
         MVC   APWORK+12(2),TWAAGY  AGENCY ID                                   
         GOTO1 VGETPROF,APPARM,APWORK,SVPROF,VDMGR                              
         B     EXIT                                                             
         EJECT                                                                  
*==========================================================*                    
* RECORD - AFTER RECORD VALIDATION - CHK SCRN RELOAD NECESS*                    
*==========================================================*                    
*                                                                               
RECORD   CLC   INREC,TWALREC     IF RECORD WAS CHANGED                          
         BE    *+8                                                              
         MVI   TWASCRN,0         FORCE SCREEN RELOAD                            
         B     HOOKX                                                            
         SPACE 2                                                                
*==========================================================*                    
* ACTION - AFTER ACTION VALIDATION - CHECK SECURITY ACCESS *                    
*==========================================================*                    
*                                                                               
ACTION   CLI   CUOFFC,C'*'       IF DDS TERMINAL                                
         BE    ACTIONX            - ALLOWED ACCESS TO ALL                       
         CLI   INREC,RECBILL     IF RECORD TYPE IS BILL                         
         BE    ACTIONX           ACCESS ALLOWED                                 
         MVC   APHALF,=X'8000'   CHECK ALLOW ACCESS FOR MAINTENANCE             
         NC    APHALF,CUAUTH                                                    
         CLC   APHALF,=X'8000'                                                  
         BE    ACTIONX           ALLOWED                                        
         MVC   FVMSGNO,=AL2(FVSECLOK)                                           
ACTIONX  B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
* BEFLFM - BEFORE FILE MAINT - IF LIST CHANGE ACTION SELECT TO *                
*          REAL ACTION NAME                                    *                
*==============================================================*                
*                                                                               
BEFLFM   L     R1,AACTHDR                                                       
         CLC   =C'SELECT',8(R1)                                                 
         BNE   BEFLFM10                                                         
         L     RE,ACACTTAB                                                      
         USING ACTTABD,RE                                                       
BEFLFM5  CLI   ACTTABD,EOT                                                      
         BE    BEFLFM10           END OF TABLE - EXIT                           
         CLC   ACTNUMB,INACT                                                    
         BE    BEFLFM8                                                          
         LA    RE,ACTTABL(RE)                                                   
         B     BEFLFM5                                                          
*                                                                               
BEFLFM8  MVC   8(L'ACTNAME,R1),ACTNAME                                          
*                                                                               
BEFLFM10 DS    0H                                                               
         CLI   INSCRN,X'FC'       POST TRACE SCREEN LOADED                      
         BNE   BEFLFMX                                                          
         MVC   TRCL1D(2),SVPROD                                                 
         OI    TRCL1DH+6,X'80'                                                  
*                                                                               
BEFLFMX  B     HOOKX                                                            
         SPACE 2                                                                
*        BEFORE LIST ACTION                                                     
*                                                                               
BEFLSM   DS    0H                                                               
         CLI   TWALACT,ACTLIS      IF LAST ACTION WASN'T LIST                   
         BE    BEFLSMX                                                          
         CLI   TWALACT,ACTSEL      OR SELECT                                    
         BE    BEFLSMX                                                          
         XC    QOFF,QOFF           RE-VALIDATE OFFICE(FOR ACLITAB)              
BEFLSMX  B     HOOKX                                                            
         EJECT                                                                  
*======================================================*                        
* LAST  - SAVES INFO TO TWA3 AND SETS PFKEYS TO SCREEN *                        
*======================================================*                        
*                                                                               
LAST     DS    0H                                                               
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BE    LAST35              NEED TO GET YOUR OWN STORAGE                 
                                                                                
         L     R2,APSTTBL                                                       
         LA    RE,MAXPNUM         MOVE FROM PSTTBL -> TIA                       
         L     RF,ATIA                                                          
LAST30   MVC   0(ACPRTNL,RF),0(R2)                                              
         LA    RF,ACPRTNL(RF)                                                   
         LA    R2,ACPRTNL(R2)                                                   
         BCT   RE,LAST30                                                        
*                                                                               
         L     R2,ATIA             MOVE FROM CLITAB -> TIA+6000                 
         AH    R2,=AL2(6000)                                                    
         LH    R3,=AL2(MAXCLTS)                                                 
         MH    R3,=H'3'                                                         
         L     R0,ACLITAB                                                       
         LR    R1,R3                                                            
         MVCL  R2,R0                                                            
*                                                                               
         L     R2,ATIA            SAVE TIA -> TWA3                              
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(3,0),(R2),0,0                      
*                                                                               
LAST35   CLI   INREC,RECBILL      IF BILL LIST                                  
         BNE   PFLOOKUP                                                         
         CLI   INACT,ACTLIS                                                     
         BNE   PFLOOKUP                                                         
         OC    SVSTRTKY,SVSTRTKY  TIMED OUT                                     
         BZ    PFLOOKUP                                                         
         LA    R2,APPARM                                                        
         USING GETTXTD,R2                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSYS,X'FF'       GENERAL SYSTEM MESSAGES                       
         MVI   GTMTYP,GTMINF      INFORMATIONAL MSG                             
         MVI   GTMSGNO1,X'82'     NUMBER 130                                    
         GOTO1 VGETTXT,GETTXTD                                                  
         DROP  R2                                                               
*                                                                               
PFLOOKUP DS    0H                                                               
         L     R2,AACTHDR                                                       
         SR    R1,R1                                                            
PFLK10   CLI   0(R2),0            END OF SCREEN?                                
         BE    PFLKX              NO PFK EXPLANATION NEEDED                     
         ICM   R1,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),FVAXTND      EXTENDED FIELD HEADER?                        
         BNZ   *+10                                                             
         AR    R2,R1                                                            
         B     PFLK10                                                           
*                                                                               
         SH    R1,=H'08'          YES - SUBTRACT LEN OF EXTND FIELD             
         LR    RE,R2                                                            
         AR    RE,R1              PT TO EXTENDED FIELD HEADER                   
         CLI   0(RE),X'FE'        LINE FOR PF KEY?                              
         BE    PFLK20                                                           
         ICM   R1,1,0(R2)                                                       
         AR    R2,R1                                                            
         B     PFLK10                                                           
*                                                                               
PFLK20   OI    6(R2),X'80'        TRANSMIT                                      
         OI    1(R2),X'20'        AND MAKE SURE PROTECTED                       
         LA    R2,8(R2)           PT TO FIELD ITSELF                            
         XC    0(78,R2),0(R2)     CLEAR PF AREA                                 
         LA    R3,PFKTBL          PFKEY TABLE                                   
         USING PFKD,R3                                                          
PFLK30   CLI   0(R3),0            END OF TABLE?                                 
         BE    PFLKX                                                            
         CLI   PFKREC1,0          ANY PARTICULAR RECORD?                        
         BE    PFLK31             NO                                            
         CLC   INREC,PFKREC1      MUST MATCH ON RECORD                          
         BNE   PFLK50                                                           
PFLK31   CLC   INACT,PFKACT1      MUST MATCH ON AN ACTION                       
         BE    PFLK32                                                           
         CLC   INACT,PFKACT2                                                    
         BNE   PFLK50                                                           
PFLK32   CLI   PFKLST,0           IS THIS ONLY FOR NESTED LIST?                 
         BNE   PFLK35             YES - CHECK TO MAKE SURE WE ARE LIST          
         CLI   PFKMNT,0           IS THIS ONLY FOR NESTED MAINT?                
         BE    PFLK40             NO - USE IT                                   
         TM    TWAFLAG,TWAFMAI    ARE WE IN NESTED MAINT?                       
         BO    PFLK40             YES - USE IT                                  
         B     PFLK50              NO - DON'T USE FOR THIS SCREEN               
*                                                                               
PFLK35   TM    TWAMODE,TWAMLSM    ARE WE IN LIST MODE?                          
         BNO   PFLK50             NO- DON'T USE FOR THIS SCREEN                 
PFLK40   ZIC   R1,PFKNAML         GET PFK NAME LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PFKNAME    MOVE NAME TO SCREEN                           
         LA    R1,1(R1)           GET BACK ORIGINAL LENGTH                      
         AR    R2,R1              BUMP PAST IT                                  
         LA    R2,1(R2)           PLUS SOME FOR SPACING                         
*                                                                               
PFLK50   ZIC   R1,0(R3)           GET NEXT PFK ENTRY                            
         AR    R3,R1              IN TABLE                                      
         B     PFLK30             GO TEST IF IT QUALIFIES FOR SCREEN            
*                                                                               
PFLKX    B     HOOKX                                                            
         SPACE 2                                                                
PFKTBL   DS    0H                                                               
         DC    AL1(29),XL1'00',AL1(ACTRACE),AL1(ACTTRC),AL1(9)                  
         DC    CL20'PF3=MAINT',XL1'00',XL1'01',XL2'00'                          
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTRACE),AL1(ACTTRC),AL1(8)                  
         DC    CL20'PF8=BACK',XL1'00',XL1'00',XL2'00'                           
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTMAI),AL1(ACTMNT),AL1(8)                   
         DC    CL20'PF8=BACK',XL1'00',XL1'00',XL2'00'                           
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTMAI),AL1(ACTMNT),AL1(8)                   
         DC    CL20'PF9=NEXT',XL1'01',XL1'00',XL2'00'                           
*                                                                               
         DC    AL1(29),AL1(RECBILL),AL1(ACTBDIS),AL1(ACTDIS),AL1(8)             
         DC    CL20'PF9=NEXT',XL1'01',XL1'00',XL2'00'                           
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTMAI),AL1(ACTMNT),AL1(10)                  
         DC    CL20'PF10=TRACE',XL1'00',XL1'00',XL2'00'                         
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTRACE),AL1(ACTTRC),AL1(12)                 
         DC    CL20'PF11=FORWARD',XL1'00',XL1'00',XL2'00'                       
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTMAI),AL1(ACTMNT),AL1(12)                  
         DC    CL20'PF11=FORWARD',XL1'00',XL1'00',XL2'00'                       
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTLIS),AL1(0),AL1(12)                       
         DC    CL20'PF11=FORWARD',XL1'01',XL1'00',XL2'00'                       
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTRACE),AL1(ACTTRC),AL1(9)                  
         DC    CL20'PF12=LIST',XL1'01',XL1'01',XL2'00'                          
*                                                                               
         DC    AL1(29),XL1'00',AL1(ACTMAI),AL1(ACTMAI),AL1(19)                  
         DC    CL20'PF12=LIST',XL1'01',XL1'01',XL2'00'                          
*                                                                               
         DC    AL1(29),AL1(RECBILL),AL1(ACTBDIS),AL1(ACTDIS2),AL1(19)           
         DC    CL20'PF12=LIST',XL1'01',XL1'00',XL2'00'                          
*                                                                               
         DC    AL1(29),AL1(RECBILL),AL1(ACTLIS),XL1'00',AL1(10)                 
         DC    CL20'S=TRANSFER',XL1'00',XL1'00',XL2'00'                         
         DC    AL1(29),AL1(RECBILL),AL1(ACTLIS),XL1'00',AL1(13)                 
         DC    CL20'R=RE-TRANSFER',XL1'00',XL1'00',XL2'00'                      
*                                                                               
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(6)                  
         DC    CL20'P=POST',XL1'00',XL1'00',XL2'00'                             
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(4)                  
         DC    CL20'C=UC',XL1'00',XL1'00',XL2'00'                               
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(4)                  
         DC    CL20'N=UN',XL1'00',XL1'00',XL2'00'                               
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'A=AOR',XL1'00',XL1'00',XL2'00'                              
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(4)                  
         DC    CL20'I=A2',XL1'00',XL1'00',XL2'00'                               
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'1=UAC',XL1'00',XL1'00',XL2'00'                              
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'2=UAN',XL1'00',XL1'00',XL2'00'                              
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'R=RET',XL1'00',XL1'00',XL2'00'                              
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'T=PST',XL1'00',XL1'00',XL2'00'                              
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'S=SPP',XL1'00',XL1'00',XL2'00'                              
         DC    AL1(29),AL1(RECPOST),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'D=DIF',XL1'00',XL1'00',XL2'00'                              
*                                                                               
         DC    AL1(29),AL1(RECPROF),AL1(ACTLIS),XL1'00',AL1(10)                 
         DC    CL20'P=REG PROF',XL1'00',XL1'00',XL2'00'                         
         DC    AL1(29),AL1(RECPROF),AL1(ACTLIS),XL1'00',AL1(8)                  
         DC    CL20'R=RETAIL',XL1'00',XL1'00',XL2'00'                           
         DC    AL1(29),AL1(RECPROF),AL1(ACTLIS),XL1'00',AL1(5)                  
         DC    CL20'S=SPP',XL1'00',XL1'00',XL2'00'                              
         DC    AL1(0)                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*=========================================================*                     
* COMMON ROUTINES AVAILABLE TO CONTROLLER AND OVERLAYS    *                     
* NTRY - R1=A(PARAMETER LIST)                             *                     
*        RF=ROUTINE NUMBER (HIGH ORDER BYTE)              *                     
* EXIT - FVMSGNO SET TO ERROR NUMBER WITH CC=NEQ ON ERROR *                     
*        FVMSGNO SET TO FVFOK WITH CC=EQ IF OK            *                     
*=========================================================*                     
*                                                                               
ROUTS    NTR1  BASE=ACBASE1,LABEL=NO                                            
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         L     R5,ATWA                                                          
         L     RE,4(RD)            MOVE REGISTER SAVE AREA                      
         LR    RC,RD               TO MAKE ROOM FOR WORK AREA                   
         AH    RD,=Y(RWRKX-RWRKD)                                               
         LA    RD,7(RD)                                                         
         SRL   RD,3                                                             
         SLL   RD,3                                                             
         ST    RE,4(RD)            BACKWARD POINTER                             
         ST    RD,8(RE)            FORWARD POINTER                              
*                                                                               
         USING RWRKD,RC            RC=A(LOCAL W/S)                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   RSPACES,C' '                                                     
         MVC   RSPACES+1(L'RSPACES-1),RSPACES                                   
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
*===============================================*                               
* TABLE OF BRANCH ADDRESSES TO ROUTS ROUTINES   *                               
*===============================================*                               
*                                                                               
ROUTSBR  B     VALSYS                                                           
         B     VALMED                                                           
         B     VALOFF                                                           
         B     VALCLT                                                           
         B     VALPRD                                                           
         B     DELL                                                             
         B     ADDL                                                             
         B     DOSPEC                                                           
         B     VALEST                                                           
         B     VALSRVM                                                          
         B     VALINV                                                           
         B     RDPROD                                                           
         B     RDMI                                                             
         B     RDSYS                                                            
         B     RDMED                                                            
         B     RDOFF                                                            
         B     RDCLT                                                            
         B     RDPRD                                                            
         B     RDCOMP                                                           
         B     DISOFF                                                           
         B     SETFILE                                                          
         B     MYIO                                                             
         B     VALIDT                                                           
         B     VALRDT                                                           
         B     VALFLD                                                           
         B     MIOACC                                                           
         DC    AL4(0)                                                           
         EJECT                                                                  
*==========================================================*                    
* VALSYS - VALIDATES SYSTEM CODE                           *                    
*   NTRY -R1=A(FIELD HEADER OF SYSTEM FIELD)               *                    
*   XIT - CC=EQUAL SYST VALUES EXTRACTED,CC<> FVMSGNO SET  *                    
*==========================================================*                    
*                                                                               
VALSYS   MVI   FVMINL,1           SET REQUIRED FIELD -MINIMUM LENGTH            
         MVI   FVMAXL,3           SYSTEM + ALPHA ID                             
         GOTO1 AFVAL                                                            
         BH    VALSYSX                                                          
         BE    VALSYS1                                                          
         BAS   RE,CLRSYSV                                                       
         B     VALSYSX                                                          
*                                                                               
VALSYS1  CLC   QSYS,FVIFLD        TEST CHANGE OF SYSTEM                         
         BNE   VALSYS2            NO                                            
         CLC   FVIFLD+1(2),SPACES IF NO MEDIA FILE INPUT                        
         BNE   *+14                                                             
         OC    QALPH,QALPH        AND NO ALPHA ID INPUT PREVIOUSL               
         BZ    VALSYSX            GET OUT                                       
         CLC   QALPH,FVIFLD+1     ELSE, TEST CHANGE OF ALPHA ID                 
         BE    VALSYSX                                                          
*                                                                               
VALSYS2  BAS   RE,CLRSYSV         CLEAR OUT SYSTEM VALUES                       
         LA    RE,SYSTBL          TABLE OF VALID SYSTEMS                        
*                                                                               
VALSYS5  CLI   0(RE),X'FF'        END OF TABLE?                                 
         BE    VALSYS30           YES - SYSTEM WASN'T VALID                     
         CLC   0(1,RE),FVIFLD     MATCH?                                        
         BE    VALSYS10                                                         
         LA    RE,1(RE)           NEXT VALID SYSTEM                             
         B     VALSYS5                                                          
*                                                                               
VALSYS10 MVC   QSYS,FVIFLD        SAVE REQUESTED SYSTEM                         
         MVI   STABFLG,0          TABLE NOT SAVED                               
         CLC   FVIFLD+1(2),SPACES ANY ALPHA ID                                  
         BE    VALSYS20                                                         
         MVC   RHALF,FVIFLD+1                                                   
         BAS   RE,CHKSPLT         CHECK IF SPLIT MEDIA FILES AUTHORIZED         
         BNE   VALSYS45                                                         
*                                                                               
VALSYS12 BAS   RE,SVTABLE         SAVE SYSTEM TABLE (IF SPLIT FILES)            
         MVC   QALPH,RHALF                                                      
         CLI   SCACTN,ACTREP      IS THIS A REPORT?                             
         BE    VALSYS25           YES                                           
         CLI   QSYS,C'P'                                                        
         BE    VALSYS25                                                         
         BAS   RE,RDAGY           GET AGYPRF7 FOR SPOT/NET                      
         BNE   VALSYS35                                                         
         B     VALSYS25                                                         
*                                 ONLY SYS -CHK MEDIA FILE REQUIRED             
VALSYS20 CLI   SCACTN,ACTREP      IS THIS A REPORT?                             
         BE    VALSYS25           YES                                           
         CLI   QSYS,C'P'                                                        
         BNE   VALSYS22                                                         
         CLI   SVPROF+4,C'Y'                                                    
         BE    VALSYS40           MISSING MEDIA FILE FOR PRINT                  
         B     VALSYS25                                                         
*                                                                               
VALSYS22 BAS   RE,RDAGY           GET AGYPRF7 FOR SPOT/NET                      
         BNE   VALSYS35                                                         
         CLI   QSYS,C'S'                                                        
         BNE   VALSYS24                                                         
         CLI   SVPROF+3,C'Y'                                                    
         BE    VALSYS40           MISSING MEDIA FILE FOR SPOT                   
         B     VALSYS25                                                         
VALSYS24 CLI   SVPROF+2,C'Y'                                                    
         BE    VALSYS40           MISSING MEDIA FILE FOR NET                    
*                                                                               
VALSYS25 MVI   APBYTE,C'S'                                                      
         BAS   RE,READB1X         READ SYSTEM LEVEL MEDIA B1X PROFILE           
         BAS   RE,READB1          READ SYSTEM LEVEL MEDIA B1 PROFILE            
         BAS   RE,MACCESS         SET MEDIA ACCESS                              
         B     VALSYSX                                                          
         SPACE                                                                  
VALSYS30 MVC   FVMSGNO,=AL2(FVFESYS) INVALID SYSTEM                             
         B     VALSYSX                                                          
VALSYS35 MVC   FVMSGNO,=AL2(FVINOAGY) NO MEDIA AGENCY RECORD                    
         BAS   RE,CLRSYSV                                                       
         B     VALSYSX                                                          
         SPACE                                                                  
VALSYS40 MVC   FVMSGNO,=AL2(FVINOFIL) NO MEDIA FILE SPECIFIED                   
         B     VALSYSX                                                          
VALSYS45 MVC   FVMSGNO,=AL2(FVISPLIT) SPLIT MEDIA ERROR                         
VALSYSX  B     ROUTSX                                                           
         SPACE                                                                  
CLRSYSV  XC    QVALS(QVALSX-QVALS),QVALS                                        
         XC    BVALS(BVALSX-BVALS),BVALS                                        
         XC    QALPH,QALPH                                                      
         BR    RE                                                               
         SPACE                                                                  
* -- TABLE OF VALID SYSTEMS                                                     
         SPACE 1                                                                
SYSTBL   DC    C'S'               SPOT SYSTEM                                   
         DC    C'N'               NET SYSTEM                                    
         DC    C'P'               PRINT SYSTEM                                  
         DC    X'FF'              END OF TABLE                                  
         EJECT                                                                  
*---------------------------------------------------*                           
* CHKSPLT - CHECK IF AGENCY IS ON SPLIT MEDIA FILES *                           
*---------------------------------------------------*                           
*                                                                               
CHKSPLT  NTR1                                                                   
         CLI   SCACTN,ACTREP      IS THIS A REPORT?                             
         BE    CHKSP40            YES                                           
         CLI   INREC,RECPROF      IF REC=PROF                                   
         BNE   CHKSPL5                                                          
         GOTO1 VALFLDL,RPARM,PROMEDH,4                                          
         CLI   RPARM,X'FF'        ANY OTHER INPUT?                              
         BNE   CHKSP40            NO OTHER INPUT SKIP READING 2F01              
*                                                                               
CHKSPL5  XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING MPRRECD,R2                                                       
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,RHALF     AGENCY ALPHA FOR SPECIAL MEDIA FILE           
         MVC   MPRKSYS,QSYS       SYSTEM                                        
         MVI   MPRKPRO,MPRKREG    REGULAR                                       
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE1'                          
         CLI   MYIOERR,0                                                        
         BNE   CHKSPNO                                                          
         DROP  R2                                                               
         SR    R0,R0                                                            
         L     R2,AIOAREA1                                                      
         LA    R1,ACCORFST                                                      
         AR    R2,R1                                                            
         USING MTPELD,R2                                                        
CHKSP10  CLI   0(R2),0                                                          
         BE    CHKSPNO                                                          
         CLI   0(R2),MTPELQ                                                     
         BNE   CHKSP15                                                          
         CLI   MTPFNUM,MTPFDMED     DDS MEDIA FILE?                             
         BE    CHKSP20                                                          
CHKSP15  ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     CHKSP10                                                          
*                                                                               
CHKSP20  ZIC   RE,MTPLN                                                         
         LA    RF,MTPFDATA-MTPEL                                                
         SR    RE,RF              RE=LENGTH OF DATA                             
         LA    R1,MTPFDATA        R1 = DATA                                     
CHKSP30  CLC   0(2,R1),RHALF                                                    
         BE    CHKSP40                                                          
         LA    R1,2(R1)                                                         
         BCT   RE,CHKSP30                                                       
         B     CHKSPNO            INVALID                                       
         DROP  R2                                                               
*                                                                               
CHKSP40  XC    IOKEY,IOKEY        READ CONTROL FILE FOR MEDIA FILE              
         LA    R3,IOKEY                                                         
         USING CT5REC,R3                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,RHALF                                                   
         GOTO1 AIO,IORD+IOCTFILE+IO1                                            
         BNE   CHKSPNO            INVALID                                       
         L     R3,AIOAREA1                                                      
         LA    R2,CT5DATA                                                       
         USING CTSYSD,R2                                                        
         SR    R0,R0                                                            
         MVI   RBYTE,2            SPOT SYSTEM NUMBER                            
         CLI   QSYS,C'S'                                                        
         BE    CHKSP50                                                          
         MVI   RBYTE,3            NET SYSTEM NUMBER                             
         CLI   QSYS,C'N'                                                        
         BE    CHKSP50                                                          
         MVI   RBYTE,4            PRINT SYSTEM NUMBER                           
*                                                                               
CHKSP50  CLI   CTSYSEL,0                                                        
         BE    CHKSPNO            INVALID                                       
         CLI   CTSYSEL,CTSYSELQ   SYSTEM ELEMENT                                
         BNE   CHKSP55                                                          
         CLC   CTSYSNUM,RBYTE     SPOT SYSTEM                                   
         BE    CHKSP60                                                          
CHKSP55  ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     CHKSP50                                                          
*                                                                               
CHKSP60  GOTO1 FUDGTAB,APPARM,(R2)  FUDGE GENERALS SWITCH TABL                  
         CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
CHKSPNO  LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*-------------------------------------------------------*                       
* RDAGY - READS AGENCY RECORD - XIT AGYPROF+7 & CC SET  *                       
*-------------------------------------------------------*                       
*                                                                               
RDAGY    NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING AGYHDRD,R2                                                       
         MVI   AGYKTYPE,6                                                       
         MVC   AGYKAGY,TWAAGY                                                   
         OC    QALPH,QALPH        ALPHA AGENCY OF OTHER MEDIA FILE              
         BZ    *+10                                                             
         MVC   AGYKAGY,QALPH      USED FOR MULTIPLE MEDIA FILES                 
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLC   IOKEY(3),IOKEYSAV                                                
         BNE   NO                                                               
         GOTO1 MYIOL,RPARM,IOGET+IO1,=C'F'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   AGYPRF7,0                                                        
         L     R2,AIOAREA1                                                      
         MVC   AGYPRF7,AGYPROF+7                                                
         B     YES                                                              
         SPACE                                                                  
*----------------------------------------*                                      
* SVTABLE - SAVE SYSTEM TABLE            *                                      
*----------------------------------------*                                      
*                                                                               
SVTABLE  NTR1                                                                   
         LA    R0,SYSSWMAX                                                      
         LA    RE,ASSWTAB         TABLE TO SAVE FROM                            
         LA    R1,SASSWTAB        TABLE TO SAVE TO                              
SVTBL5   MVC   0(SYSSWLEN,R1),0(RE)                                             
         LA    RE,SYSSWLEN(RE)                                                  
         LA    R1,SYSSWLEN(R1)                                                  
         BCT   R0,SVTBL5                                                        
         OI    STABFLG,STABSET    INDICATE SAVED                                
         B     EXIT                                                             
         EJECT                                                                  
*==========================================================*                    
* VALMED - VALIDATES MEDIA CODE                            *                    
*    NTRY- R1=A(FIELD HEADER OF MEDIA FIELD)               *                    
*    XIT - CC=EQUAL MEDIA VALUES EXTRACTED,CC<> FVMSGNO SET*                    
*==========================================================*                    
VALMED   MVI   FVMINL,0           SET FIELD NOT REQUIRED                        
         MVI   FVMAXL,L'QMED      MAXIMUM LENGTH                                
         GOTO1 AFVAL                                                            
         BH    VALMEDX            ERROR                                         
         BE    VALMED5                                                          
         BAS   RE,CLRMEDV         CLEAR VALUES                                  
         B     VALMEDX                                                          
*                                                                               
VALMED5  CLC   QMED,FVIFLD        TEST CHANGE IN MEDIA                          
         BE    VALMEDX                                                          
         BAS   RE,CLRMEDV         CLEAR VALUES                                  
         LA    R2,SMEDTBL         MEDIA VALUES FOR SPOT                         
         CLI   QSYS,C'S'          IF SPOT SYTEM                                 
         BE    VALMED10           YES                                           
         LA    R2,NMEDTBL         MEDIA VALUES FOR NET                          
         CLI   QSYS,C'N'          IF NET SYSTEM                                 
         BE    VALMED10                                                         
         LA    R2,PMEDTBL         PRINT ONLY SYSTEM LEFT                        
*                                                                               
VALMED10 CLI   0(R2),X'FF'        END OF TABLE?                                 
         BE    VALMED30           YES - INVALID MEDIA                           
         CLC   0(1,R2),FVIFLD     MATCH ON MEDIA                                
         BE    VALMED20                                                         
         LA    R2,L'SMEDTBL(R2)                                                 
         B     VALMED10                                                         
VALMED20 MVC   QMED,FVIFLD                                                      
         CLI   QSYS,C'P'          IF PRINT NO BINARY VALUE                      
         BE    VALMED25                                                         
         BAS   RE,GETBMED         SET BAGYMD                                    
         BNE   VALMED30                                                         
*                                                                               
VALMED25 MVI   APBYTE,C'M'                                                      
         BAS   RE,READB1X         READ MEDIA LEVEL MEDIA B1X PROFILE            
         BAS   RE,READB1          READ MEDIA LEVEL MEDIA B1 PROFILE             
         B     VALMEDX                                                          
*                                                                               
VALMED30 MVC   FVMSGNO,=AL2(FVIMED)                                             
VALMEDX  B     ROUTSX                                                           
*                                                                               
CLRMEDV  XC    QMED(QVALSX-QMED),QMED                                           
         XC    BAGYMD(BVALSX-BAGYMD),BAGYMD                                     
         BR    RE                                                               
*                                                                               
SMEDTBL  DS    0CL1               SPOT SYSTEM MEDIA VALUES                      
         DC    C'TNCRX'                                                         
         DC    X'FF'                                                            
*                                                                               
NMEDTBL  DS    0CL1               NET SYSTEM MEDIA VALUES                       
         DC    C'NCSODV'                                                        
         DC    X'FF'                                                            
*                                                                               
PMEDTBL  DS    0CL1               PRINT SYSTEM MEDIA VALUES                     
         DC    C'MNOSTILBVWD'                                                   
         DC    X'FF'                                                            
*                                                                               
* READB1X - READS B1X PROFILE                                                   
*                                                                               
READB1X  NTR1                                                                   
         XC    B1XPROF,B1XPROF                                                  
         XC    APWORK,APWORK                                                    
         MVI   APWORK,C'P'                                                      
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         MVI   APWORK,C'S'                                                      
         NI    APWORK,X'BF'                                                     
         MVC   APWORK+1(3),=C'B1X'                                              
         MVC   APWORK+4(2),TWAAGY                                               
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   APWORK+4(2),QALPH                                                
         CLI   APBYTE,C'S'                                                      
         BE    *+10                                                             
         MVC   APWORK+6(1),QMED                                                 
         GOTO1 VGETPROF,APPARM,APWORK,B1XPROF,VDMGR                             
         B     EXIT                                                             
         EJECT                                                                  
* READB1 - READS B1X PROFILE                                                    
*                                                                               
READB1   NTR1                                                                   
         XC    B1PROF,B1PROF                                                    
         XC    APWORK,APWORK                                                    
         MVI   APWORK,C'P'                                                      
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         MVI   APWORK,C'S'                                                      
         NI    APWORK,X'BF'                                                     
         MVC   APWORK+2(2),=C'B1'                                               
         MVC   APWORK+4(2),TWAAGY                                               
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   APWORK+4(2),QALPH                                                
         CLI   APBYTE,C'S'                                                      
         BE    *+10                                                             
         MVC   APWORK+6(1),QMED                                                 
         GOTO1 VGETPROF,APPARM,APWORK,B1PROF,VDMGR                              
         B     EXIT                                                             
         EJECT                                                                  
*=====================*                                                         
* GETBMED-SETS BAGYMD *                                                         
*=====================*                                                         
*                                                                               
GETBMED  NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING AGYHDRD,R2                                                       
         MVI   AGYKTYPE,6                                                       
         MVC   AGYKAGY,TWAAGY                                                   
         OC    QALPH,QALPH        ALPHA AGENCY OF OTHER  MEDIA FILE             
         BZ    *+10                                                             
         MVC   AGYKAGY,QALPH      IF SPLIT FILES                                
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(3),IOKEYSAV                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MYIOL,RPARM,IOGET+IO1,=C'F'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   BAGYMD,0                                                         
         MVI   AGYPRF7,0                                                        
         L     R2,AIOAREA1                                                      
         LA    R1,AGYEL                                                         
         SR    R0,R0                                                            
GETBMD10 CLI   0(R1),0                                                          
         BE    GETBMDN                                                          
         CLI   0(R1),2                                                          
         BNE   GETBMD14                                                         
         CLI   QSYS,C'N'                                                        
         BNE   GETBMD12                                                         
         CLI   2(R1),C'N'         COMPARE FOR NETWORK                           
         BNE   GETBMD14                                                         
         B     GETBMD15                                                         
GETBMD12 CLC   2(1,R1),QMED                                                     
         BE    GETBMD15                                                         
GETBMD14 ICM   R0,1,1(R1)         LENGTH OF ELEMENT                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0              BUMP TO NEXT ELEMENT                          
         B     GETBMD10                                                         
*                                                                               
GETBMD15 MVC   BAGYMD,3(R1)       SET BINARY AGENCY/MEDIA                       
         MVC   AGYPRF7,AGYPROF+7  SET COUNTRY                                   
         CR    RB,RB                                                            
         B     GETBMDX                                                          
GETBMDN  LTR   RB,RB                                                            
GETBMDX  XIT1                                                                   
         DROP  R2                                                               
                                                                                
***********************************************************************         
* DISOFF - THIS ROUTINE IS FOR OFFICES AND OFFICE GRPS                          
*        CALLS OFFICER - WILL CONVERT THE INTERNAL ONE BYTE                     
*        OFFICE/LIST CODE TO A DISPLAYABLE TWO CHARACTER OFFICE                 
*        NTRY - PARAMETER 1 - OFFICE CODE FROM RECORD                           
*               PARAMETER 2 - SCREEN OFFICE CODE FIELD                          
*        EXIT - CC=EQUAL - DISPLAYABLE OFFICE EXTRACTED                         
*               CC=NOT EQUAL - COULD NOT FIND OFFICE                            
***********************************************************************         
DISOFF   DS    0H                                                               
         LM    R2,R3,0(R1)        R2=OFFICE FROM RECORD                         
*                                 R3=FIELD HEADER TO DISPLAY OFFICE             
         XC    RWORK,RWORK        SET UP PARAMATER BLOCK                        
         LA    R4,RWORK                                                         
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'P'                                                      
         CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BE    *+8                                                              
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCOFC,0(R2)       PASS INTERNAL CLIENT OFFICE (1)               
*                                                                               
         CLI   0(R2),C'$'         IS THIS OFFICE GROUP                          
         BNE   DISOFF20           NO                                            
         MVC   OFCMOL,1(R2)       YES: THEN WE NEED TO CONVERT IT               
         OI    OFCINDS,OFCIMOLC   CONVERT OFFICE LIST FOR PRINTING              
*                                                                               
DISOFF20 MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH        USE ALPHA AGENCY OF OTHER MEDIA FILE          
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH       (FOR SPLIT FILES)                             
         GOTO1 VOFFICER,RPARM,(C'2',RWORK),(0,ACOM)                             
         BNE   DISOFFX                                                          
*                                                                               
         CLI   0(R2),C'$'         ARE WE WORKING WITH OFFICE LISTS?             
         BNE   DISOFF30           NO                                            
         MVI   0(R3),C'$'                                                       
         MVC   1(L'OFCMOL2,R3),OFCMOL2  YES: PRINT TWO CHARACTER VALUE          
         B     DISOFFX                                                          
*                                                                               
DISOFF30 TM    OFCINDS,OFCIOINV    IS OFFICE CODE INVALID?                      
         BZ    *+14                NO                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     DISOFFX                                                          
         MVC   0(L'OFCOFC2,R3),OFCOFC2  MOVE IN 2 BYTE OFFICE                   
DISOFFX  B     ROUTSX                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
* VALOFF - VALIDATES OFFICE CODE/OFFICE GROUP                                   
*        NTRY - R1=A(FIELD HEADER OF MEDIA FIELD)                               
*        EXIT - CC=EQUAL MEDIA VALUES EXTRACTED                                 
*               CC=NOT EQUAL ON ERROR WITH FVMSGNO SET                          
* NOTE - NEED TO SET OFFICE AFTER ALL VALID                                     
***********************************************************************         
VALOFF   MVI   FVMINL,0           FIELD NOT REQUIRED                            
         MVI   FVMAXL,L'QOFF+1    MAXIMUM LENGTH (MAY NEED CONVERSION)          
         GOTO1 AFVAL                                                            
         BH    VALOFFX            ERROR - EXIT                                  
         BE    VALOFF5                                                          
         CLI   FVILEN,L'QOFF      2 BYTES IS STILL GOOD                         
         BE    VALOFF5                                                          
         BAS   RE,CLROFFV         CLEAR VALUES                                  
         L     R1,ACLITAB                                                       
         XC    0(3,R1),0(R1)      SET NO ENTRIES IN CLIENT TABLE                
         B     VALOFFX                                                          
*                                                                               
VALOFF5  CLI   FVIFLD,C'$'        IS IT AN OFFICE GROUP?                        
         BE    VALOFF6            CALL OFFICER TO CONVERT                       
*                                                                               
         MVI   FVMINL,0           FIELD NOT REQUIRED                            
         MVI   FVMAXL,L'QOFF      MAXIMUM LENGTH FOR OFFICE                     
         GOTO1 AFVAL                                                            
         BH    VALOFFX            ERROR - EXIT                                  
*                                                                               
         XC    RWORK,RWORK        SET UP PARAMATER BLOCK                        
         LA    R1,RWORK                                                         
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BE    VALOFF5A                                                         
         MVI   OFCSYS,C'S'                                                      
         CLI   QSYS,C'S'          SPOT SYSTEM?                                  
         BE    VALOFF5A                                                         
         MVI   OFCSYS,C'N'        MUST BE NET                                   
VALOFF5A MVC   OFCOFC2,FVIFLD     PASS CLIENT OFFICE (ONE OR TWO BYTES)         
         MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH        USE ALPHA AGENCY OF OTHER MEDIA FILE          
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH       (FOR SPLIT FILES)                             
         GOTO1 VOFFICER,RPARM,(C'2',RWORK),(0,ACOM)                             
         LA    R1,RWORK                                                         
         TM    OFCINDS,OFCIOINV    IS OFFICE CODE INVALID?                      
         BO    VALOERR             YES ERROR                                    
         CLC   OFCOFC,QOFF         TEST CHANGE IN OFFICE                        
         BE    VALOFFX                                                          
         BAS   RE,CLROFFV                                                       
         MVC   QOFF(1),OFCOFC     SAVE 1 BYTE INTERNAL OFFICE CODE              
         MVI   QOFF+1,C' '                                                      
         B     VALOFF10                                                         
         DROP  R1                                                               
*                                                                               
VALOFF6  XC    RWORK,RWORK        FOR OFFICE LISTS/GROUPS                       
         LA    R1,RWORK                                                         
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BE    VALOFF6A                                                         
         MVI   OFCSYS,C'S'                                                      
         CLI   QSYS,C'S'          SPOT SYSTEM?                                  
         BE    VALOFF6A                                                         
         MVI   OFCSYS,C'N'        MUST BE NET                                   
VALOFF6A MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH        USE ALPHA AGENCY OF OTHER MEDIA FILE          
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH       (FOR SPLIT FILES)                             
*                                                                               
         MVC   OFCMOL2,FVIFLD+1   YES: THEN WE NEED TO CONVERT IT               
         OI    OFCINDS,OFCIMOLC   CONVERT OFFICE LIST FOR PRINTING              
         GOTO1 VOFFICER,RPARM,(C'2',RWORK),(0,ACOM)                             
         BNE   VALOERR                                                          
         LA    R1,RWORK                                                         
         CLC   QOFF+1(1),OFCMOL   TEST CHANGE IN OFFICE                         
         BE    VALOFFX            NO CHANGE: EXIT                               
         BAS   RE,CLROFFV                                                       
         MVI   QOFF,C'$'                                                        
         MVC   QOFF+1(1),OFCMOL   ONE BYTE OFFICE LIST VALUE                    
         CLI   QSYS,C'P'                                                        
         BNE   VALOFF7                                                          
         CLI   SVPROF+1,C'Y'      PRINT - USE OFFICE GROUPS                     
         BNE   VALOFGER           NO - SO INVALID                               
         B     VALOFF8                                                          
         DROP  R1                                                               
*                                                                               
VALOFF7  CLI   SVPROF,C'Y'        SPOT/NET DO THEY USE OFFICE GROUPS            
         BNE   VALOFGER           NO - SO INVALID                               
VALOFF8  MVI   QOFFIND,C'G'       INDICATE OFFICE GROUP                         
         MVC   SVOFFG,QOFF        ALSO SET SVOFFG                               
         BAS   RE,CKACCESS        IF LIMIT ACCESS                               
         BNE   *+12                                                             
         BAS   RE,OACCESS         CHECK OFFICE SECURITY                         
         BNE   VALOSEC                                                          
         B     VALOFF30                                                         
*                                                                               
VALOFF10 MVI   QOFFIND,C'O'       INDICATE OFFICE CODE                          
         MVC   TEMPOFF,QOFF                                                     
         BAS   RE,CKACCESS        IF LIMIT ACCESS                               
         BNE   VALOFF20                                                         
         BAS   RE,OACCESS         CHECK OFFICE SECURITY                         
         BNE   VALOSEC                                                          
         B     *+8                                                              
VALOFF20 BAS   RE,CHKOFG          ELSE, CHECK FOR OFFICE GROUP (IF NEC)         
*                                                                               
VALOFF30 BAS   RE,SETCLTS         BUFFER UP CLIENTS IF NECESSARY                
         BNE   VALOBIG            REQUEST TOO BIG                               
*                                                                               
VALOFFX  B     ROUTSX                                                           
*                                                                               
VALOERR  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     *+10                                                             
VALOFGER MVC   FVMSGNO,=AL2(FVINOOFG)                                           
         B     *+10                                                             
VALOSEC  MVC   FVMSGNO,=AL2(FVSECLOK)                                           
         B     *+10                                                             
VALOBIG  MVC   FVMSGNO,=AL2(FVITBIG)                                            
         BAS   RE,CLROFFV                                                       
         B     VALOFFX                                                          
*                                                                               
CLROFFV  XC    QOFF(QVALSX-QOFF),QOFF                                           
         XC    BOFF(BVALSX-BOFF),BOFF                                           
         XC    SVOFFG,SVOFFG                                                    
         XC    SVCOFF,SVCOFF                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* ROUTINE TO CHECK IF LIMIT ACCESS ON MEDIA ID                                  
***********************************************************************         
CKACCESS NTR1                                                                   
*                                                                               
         CLI   SVPROF+5,C'Y'      IF LIMIT ACCESS ENABLED                       
         BNE   NO                                                               
         MVI   RBYTE,2                                                          
         CLI   QSYS,C'S'                                                        
         BE    CKACC5                                                           
         MVI   RBYTE,3                                                          
         CLI   QSYS,C'N'                                                        
         BE    CKACC5                                                           
         MVI   RBYTE,4                                                          
CKACC5   LA    R0,SYSSWMAX                                                      
         LA    RE,ASSWTAB                                                       
         USING SYSSWTAB,RE                                                      
CKACC8   CLC   SYSSWSOV,RBYTE      LOOK FOR APPROPRIATE MEDIA ENTRY             
         BE    CKACC10                                                          
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,CKACC8                                                        
         B     NO                  SYSTEM NOT IN TAB - ASSUME NO LIMIT          
*                                                                               
CKACC10  OC    SYSSWACS(2),SYSSWACS IF LIMIT ACCESS SPECIFIED                   
         BZ    NO                                                               
         CLI   SYSSWACS,C'+'       OTHER THAN MARKET LOCKOUT                    
         BE    NO                                                               
         B     YES                 CHECK LIMIT ACCESS                           
         DROP  RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHECK OFFICE LIMIT ACCESS                                          
***********************************************************************         
OACCESS  NTR1                                                                   
*                                                                               
         CLI   QOFFIND,C'O'        IF INPUT IS OFFICE CODE                      
         BNE   OACCESS5                                                         
         CLI   SVACCESS,C'$'       AND LIMIT ACCESS BY OFFICE GROUP             
         BNE   OACCESS2                                                         
         BAS   RE,CALLOFF          CHECK OFFICE IN GRP (TEMPOFF SET)            
         BNE   NO                                                               
         MVC   SVOFFG,SVACCESS     SET OFFICE GROUP                             
         B     OACCESSX                                                         
*                                                                               
OACCESS2 CLI   SVACCESS,C'*'       IF LIMIT ACCESS BY CODE                      
         BNE   NO                                                               
         CLC   QOFF(1),SVACCESS+1  OFFICE CODES MUST MATCH                      
         BNE   NO                                                               
         MVC   TEMPOFF,QOFF                                                     
         BAS   RE,CHKOFG           SET SVOFFG                                   
         B     OACCESSX                                                         
*                                  ELSE, INPUT IS OFFICE GROUP                  
OACCESS5 CLI   SVACCESS,C'$'       IF LIMIT ACCESS BY OFFICE GROUP              
         BNE   NO                                                               
         CLC   QOFF,SVACCESS       MUST MATCH OFFICE GROUPS EXACTLY             
         BNE   NO                                                               
*                                                                               
OACCESSX B     YES                 OFFICE CODE OR OFFICE GROUP                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ THROUGH CLIENT HEADERS SAVING CLIENT CODES                               
* THAT FALL WITHIN OFFICE REQUEST                                               
***********************************************************************         
SETCLTS  NTR1                                                                   
*                                                                               
         CLI   INACT,ACTLIS        IF LIST (POST/PROF/BILL)                     
         BNE   SETCLTSX                                                         
         L     R3,ACLITAB          R3=CLIENT TABLE                              
         LH    R0,=AL2(MAXCLTS-1)  MAXIMUM CLIENTS TO SAVE                      
         CLI   QSYS,C'P'                                                        
         BE    *+12                                                             
         BAS   RE,SETSCLT          SET TABLE OF SPOT/NET CLIENTS                
         B     *+8                                                              
         BAS   RE,SETPCLT          SET TABLE OF PRINT CLIENTS                   
         BNE   NO                                                               
SETCLTSX MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO READ CLIENT HEADERS FOR SPOT/NET                                   
*        MUST NOT DO GETREC -WESTERN WILL DIE W/ I/O PROBLEMS                   
*        CAN'T SAVE CHAR CODES PROBLEM WITH ML6/MLF SITUATIONS                  
***********************************************************************         
SETSCLT  NTR1                                                                   
*                                                                               
         LA    R2,IOKEY            READ THROUGH CLIENT HEADERS                  
         USING CLTPHDRD,R2                                                      
         XC    CPKEY,CPKEY                                                      
         MVI   CPKEYTYP,CPKEYTYQ   X'0D'                                        
         MVI   CPKEYSUB,CPKEYSBQ   X'80'                                        
         MVC   CPKEYAM,BAGYMD      A/M                                          
         CLI   QOFFIND,C'O'        IF OFFICE CODE FIELD INPUT                   
         BNE   *+10                                                             
         MVC   CPKEYOFF,QOFF       SET OFFICE IN KEY                            
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         B     SETSCLT5                                                         
*                                                                               
SETSCLT2 GOTO1 MYIOL,RPARM,IOSEQ+IO1,=C'D'                                      
SETSCLT5 CLI   MYIOERR,0                                                        
         BNE   SETSCLTX                                                         
         CLC   0(CPKEYOFF-CPKEY,R2),IOKEYSAV  CHECK SAME TYP & A/M              
         BNE   SETSCLTX                                                         
*                                                                               
         CLI   QOFFIND,C'O'        IF OFFICE CODE REQUEST                       
         BNE   SETSCLT6                                                         
         CLC   CPKEYOFF,QOFF       CHECK CLT OFF IS OFFICE REQUESTED            
         BNE   SETSCLTX                                                         
         B     SETSCLT8                                                         
*                                                                               
SETSCLT6 MVC   TEMPOFF,CPKEYOFF    ELSE, FOR OFFICE GROUP INPUT                 
         BAS   RE,CALLOFF2         CHECK CLIENT OFFICE IN OFFICE GROUP          
         BNE   SETSCLT2                                                         
*                                                                               
SETSCLT8 DS    0H                  SAVE PACKED FOR BILL/LIST COMPARE            
**NO-OP**GOTO1 VCLUNPK,RPARM,(SVCPRF7,CKEYCLT),0(R3)                            
         MVC   0(2,R3),CPKEYCLT    SAVED PACKED CLIENT                          
         MVI   2(R3),0                                                          
         LA    R3,3(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,SETSCLT2                                                      
         B     NO                  ERROR - REQUEST TOO BIG                      
*                                                                               
SETSCLTX XC    0(3,R3),0(R3)       MARK END OF TABLE                            
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO READ CLIENT HEADERS FOR PRINT                         
SETPCLT  NTR1                                                                   
         LA    R2,IOKEY            READ OFFICE/CLIENT PASSIVE PTR               
         USING CLTPHDRD,R2                                                      
         XC    POFCKEY,POFCKEY                                                  
         MVC   POFCKAGY,TWAAGY                                                  
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   POFCKAGY,QALPH                                                   
         MVC   POFCKMED,QMED                                                    
         MVI   POFCKRCD,POFCKIDQ                                                
         CLI   QOFFIND,C'O'        IF OFFICE CODE FIELD INPUT                   
         BNE   *+10                                                             
         MVC   POFCKOFF,QOFF       SET OFFICE IN KEY                            
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         B     SETPCLT5                                                         
*                                                                               
SETPCLT2 GOTO1 MYIOL,RPARM,IOSEQ+IO1,=C'D'                                      
SETPCLT5 CLI   MYIOERR,0                                                        
         BNE   SETPCLTX                                                         
         CLC   IOKEY(POFCKOFF-POFCKEY),IOKEYSAV  CHECK OFF/CLI PASSIVE          
         BNE   SETPCLTX                                                         
*                                                                               
         CLI   QOFFIND,C'O'        IF OFFICE CODE REQUEST                       
         BNE   SETPCLT6                                                         
         CLC   POFCKOFF,QOFF       CHECK CLT OFF IS OFFICE REQUESTED            
         BNE   SETPCLTX                                                         
         B     SETPCLT8                                                         
*                                                                               
SETPCLT6 MVC   TEMPOFF,POFCKOFF    ELSE, FOR OFFICE GROUP INPUT                 
         BAS   RE,CALLOFF2         CHECK CLIENT OFFICE IN OFFICE GROUP          
         BNE   SETPCLT2                                                         
*                                                                               
SETPCLT8 MVC   0(3,R3),POFCKCLT    SET CLIENT IN TABLE                          
         LA    R3,3(R3)                                                         
         BCT   R0,SETPCLT2                                                      
         B     NO                  ERROR - REQUEST TOO BIG                      
*                                                                               
SETPCLTX XC    0(3,R3),0(R3)       MARK END OF TABLE                            
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE CHECKS CLIENT OFFICE IN OFFICE GROUP REQUESTED                        
*        NTRY - TEMPOFF(CLIENT OFFICE), QOFF(OFFICE GROUP REQ)                  
*        XIT  - CC SET                                                          
***********************************************************************         
CALLOFF2 NTR1                                                                   
*                                                                               
         XC    RDUB,RDUB           SET UP PARAMATER BLOCK                       
         LA    R1,RDUB                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'         PASS SYSTEM                                  
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QOFF        PASS LIMIT ACCESS                            
         MVC   OFCOFC,TEMPOFF      PASS CLIENT OFFICE                           
         MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH        USE ALPHA AGENCY OF OTHER MEDIA FILE          
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH       (FOR SPLIT FILES)                             
         DROP  R1                                                               
         GOTO1 VOFFICER,RPARM,RDUB,ACOM                                         
         CLI   0(R1),0                                                          
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALCLT  VALIDATES A CLIENT CODE                                               
*        NTRY - R1=A(FIELD HEADER OF CLIENT FIELD)                              
*        EXIT - CC=EQUAL WITH CLIENT VALUES EXTRACTED                           
*               CC=NOT EQUAL ON ERROR WITH FVMSGNO SET                          
***********************************************************************         
VALCLT   DS    0H                                                               
         MVI   FVMINL,0            SET FIELD NOT REQUIRED                       
         MVI   FVMAXL,L'QCLT                                                    
         GOTO1 AFVAL                                                            
         BH    VALCLTX            ERROR - EXIT                                  
         BE    VALCLT5                                                          
         BAS   RE,CLRCLTV         CLEAR VALUES                                  
         CLI   SVPROF+5,C'Y'      IF CHECKING LIMIT ACCESS                      
         BNE   VALCLTX                                                          
         OC    SVACCESS(2),SVACCESS  MAKE SURE CLIENT NOT REQUIRED              
         BZ    VALCLTX                                                          
         CLI   SVACCESS,C'*'                                                    
         BE    VALCLTX                                                          
         CLI   SVACCESS,C'+'                                                    
         BE    VALCLTX                                                          
         CLI   SVACCESS,C'$'                                                    
         BNE   CMISSERR                                                         
         B     VALCLTX            THEN EXIT                                     
*                                                                               
VALCLT5  CLC   QCLT,FVIFLD        TEST CHANGE IN CLIENT                         
         BE    VALCLTX                                                          
         BAS   RE,CLRCLTV         CLEAR VALUES                                  
         OC    QMED,QMED          MUST HAVE MEDIA TO HAVE CLIENT                
         BZ    VALCLT30                                                         
*                                                                               
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   VALCLT10                                                         
         BAS   RE,VALPCLT         VALIDATE CLIENT - PRINT SYSTEM WAY            
         BNE   VALCLTX                                                          
         B     VALCLT20                                                         
*                                                                               
VALCLT10 GOTO1 VCLPACK,RPARM,FVIFLD,BCLT                                        
         CLI   0(R1),0                                                          
         BNE   VALCLT30           INVALID CLIENT                                
         BAS   RE,VALSCLT         VALIDATE CLIENT - SPOT & NET WAY              
         BNE   VALCLTX                                                          
*                                                                               
VALCLT20 MVC   QCLT,FVIFLD                                                      
         B     VALCLTX                                                          
*                                                                               
VALCLT30 MVC   FVMSGNO,=AL2(FVFERNF)                                            
VALCLTX  B     ROUTSX                                                           
*                                                                               
CMISSERR MVC   FVMSGNO,=AL2(FVIMISS)                                            
         B     VALCLTX                                                          
*                                                                               
VALCSEC  MVC   FVMSGNO,=AL2(FVSECLOK)                                           
         B     ROUTSX                                                           
         SPACE 2                                                                
CLRCLTV  XC    QCLT(QVALSX-QCLT),QCLT                                           
         XC    BCLT(BVALSX-BCLT),BCLT                                           
         XC    SVCGST,SVCGST                                                    
         XC    SVCPRF7,SVCPRF7                                                  
         XC    SVCPST,SVCPST       CLEAR PST CODES                              
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALSCLT - VALIDATE SPOT/NET CLIENT                                            
*        XIT - SETS FVMSGNO & CC CODE                                           
***********************************************************************         
VALSCLT  NTR1                                                                   
*                                                                               
         LA    R2,IOKEY           BUILD KEY OF CLIENT RECORD                    
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLI   MYIOERR,0                                                        
         BNE   VALSCLT9           CC CODE SET - SET MSG                         
         CLC   IOKEY(4),IOKEYSAV                                                
         BNE   VALSCLT9           CC CODE SET - SET MSG                         
         GOTO1 MYIOL,RPARM,IOGET+IO1,=C'F'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   SVCOFF,COFFICE                                                   
*                                                                               
         MVC   TEMPOFF,COFFICE     SET CLIENT OFFICE                            
         BAS   RE,CKACCESS         IF LIMIT ACCESS                              
         BNE   VALSCLT5                                                         
         BAS   RE,CKCLTACC         CHECK CLIENT SECURITY                        
         BNE   VALCSEC                                                          
         B     *+8                                                              
VALSCLT5 BAS   RE,CHKOFG           ELSE,SET OFFICE GROUP (IF NECESSARY)         
*                                                                               
         MVC   SVCPRF7,CPROF+6    C'Y'=PRINT CLIENT AS AAN                      
         MVC   SVCGST,CEXTRA+11   GST CODE                                      
         CLI   SVCGST,C'0'                                                      
         BNE   *+8                                                              
         MVI   SVCGST,C'S'        STANDARD IS THE DEFAULT                       
         MVC   SVCPST,CPST        PST CODES                                     
         CR    RB,RB              SET CC =                                      
         B     VALSCLTX                                                         
*                                                                               
VALSCLT9 MVC   FVMSGNO,=AL2(FVFERNF)                                            
*                                                                               
VALSCLTX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALPCLT - VALIDATE PRINT CLIENT                                               
*         XIT - CC <> 0 SETS ERROR MSG                                          
*               CC =  0                                                         
***********************************************************************         
VALPCLT  NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PCLTREC,R2                                                       
         MVC   PCLTKAGY,TWAAGY                                                  
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   PCLTKAGY,QALPH     USE OVERRIDE ID                               
         MVC   PCLTKMED,QMED      MEDIA                                         
         MVI   PCLTKRCD,X'02'     CLIENT RECORD TYPE                            
         MVC   PCLTKCLT,FVIFLD    CLIENT REQUESTED                              
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLI   MYIOERR,0                                                        
         BNE   VALPCLT9           CC CODE SET - SET MSG                         
         CLC   IOKEY(L'PCLTKEY),IOKEYSAV                                        
         BNE   VALPCLT9           CC CODE SET - SET MSG                         
         GOTO1 MYIOL,RPARM,IOGET+IO1,=C'F'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   SVCOFF,PCLTOFF     CLIENT OFFICE                                 
*                                                                               
         MVC   TEMPOFF,PCLTOFF    SET CLIENT OFFICE                             
         BAS   RE,CKACCESS        IF LIMIT ACCESS                               
         BNE   VALPCLT5                                                         
         BAS   RE,CKCLTACC        CHECK CLIENT SECURITY                         
         BNE   VALCSEC                                                          
         B     *+8                                                              
VALPCLT5 BAS   RE,CHKOFG          SETS OFFICE GROUP (IF NECESSARY)              
*                                                                               
         MVC   SVCGST,PCLTGST     GST CODE                                      
         CLI   SVCGST,0                                                         
         BNE   *+8                                                              
         MVI   SVCGST,C'S'        STANDARD IS THE DEFAULT                       
*                                                                               
         XR    RE,RE                                                            
         LA    R1,PCLTELEM        R1=A(FIRST ELEMENT)                           
         USING PCLTPST,R1                                                       
*                                                                               
VALPCLT8 CLI   PCLTPST,0          TEST END OF RECORD                            
         BE    VALPCLTY                                                         
         CLI   PCLTPST,X'25'      TEST PST ELEMENT                              
         BNE   *+14                                                             
         MVC   SVCPST,PCLTPSTC    SAVE PST CODES                                
         B     VALPCLTY                                                         
*                                                                               
         ICM   RE,1,1(R1)         BUMP TO NEXT ELEMENT IN CLT REC               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,RE                                                            
         B     VALPCLT8                                                         
*                                                                               
VALPCLTY CR    RB,RB              SET CC =                                      
         B     VALPCLTX                                                         
*                                                                               
VALPCLT9 MVC   FVMSGNO,=AL2(FVFERNF)                                            
*                                                                               
VALPCLTX B     EXIT                                                             
         DROP  R2,R1                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHKOFG - SETS OFFICE GROUP IF ANY                                             
***********************************************************************         
CHKOFG   NTR1                                                                   
*                                                                               
         XC    SVOFFG,SVOFFG                                                    
         CLI   QSYS,C'P'                                                        
         BNE   CHKOFG10                                                         
         CLI   SVPROF+1,C'Y'      PRINT - USE OFFICE GROUP                      
         BNE   CHKOFGX                                                          
         B     CHKOFG20                                                         
CHKOFG10 CLI   SVPROF,C'Y'        SPOT/NET -USE OFFICE GROUPS?                  
         BNE   CHKOFGX                                                          
*                                                                               
CHKOFG20 LA    R3,GRPTBL                                                        
*                                                                               
CHKOFG30 CLI   0(R3),X'FF'        END OF POSSIBLE OFFICE GROUPS?                
         BE    CHKOFGX                                                          
*                                                                               
         XC    RWORK,RWORK        YES - SEE IF OFF IS IN OFFICE GRP             
         LA    R4,RWORK                                                         
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'P'                                                      
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         MVI   OFCSYS,C'S'                                                      
*                                                                               
         MVI   OFCAUTH,C'$'                                                     
         MVC   OFCAUTH+1(1),0(R3)                                               
*                                                                               
         MVC   OFCOFC,TEMPOFF                                                   
         MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH                                                     
         GOTO1 VOFFICER,RPARM,RWORK,ACOM                                        
         CLI   0(R1),0                                                          
         BE    CHKOFG40                                                         
         LA    R3,1(,R3)                                                        
         B     CHKOFG30                                                         
*                                                                               
CHKOFG40 MVC   SVOFFG,OFCAUTH     SAVE OFFICE GROUP                             
*                                                                               
CHKOFGX  B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
GRPTBL   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'  OFFICE GROUPS (LISTS)             
         DC    C'123456789'                   ..                                
         DC    C'<>#@'                        SPECIAL CHARACTERS                
         DC    X'41424344464748494C'          NEW 2 CHAR LIST'S 1 BYTE          
         DC    X'51525354565758595B5D5E5F'    HEX VALUE                         
         DC    X'61626364666768696B6C6D6F'    ..                                
         DC    X'71727374767778797B7C'        ..                                
         DC    X'81828384868788898B8C8D8E'    ..                                
         DC    X'91929394969798999B9C9D9E'    ..                                
         DC    X'A1A2A3A4A6A7A8A9ABACADAE'    ..                                
         DC    X'B1B2B3B4B6B7B8B9BBBCBDBE'    ..                                
         DC    X'CBCCCDCE'                    ..                                
         DC    X'DBDCDDDE'                    ..                                
         DC    X'EBECEDEE'                    ..                                
         DC    X'FBFCFDFE'                    ..                                
         DC    X'FF'                          END OF TABLE                      
                                                                                
***********************************************************************         
* ROUTINE TO CHECK LIMIT ACCESS BASED ON MEDIA CONTROL FILE ID                  
*        NTRY - TEMPOFF IS CLIENT OFFICE CODE                                   
***********************************************************************         
CKCLTACC NTR1                                                                   
*                                                                               
         CLI   SVACCESS,C'*'       TEST OFFICE LOCKOUT                          
         BE    CLTACC5                                                          
         CLI   SVACCESS,C'$'       TEST OFFICE LIST                             
         BE    CLTACC6                                                          
         CLC   BCLT,SVACCESS       NONE OF ABOVE - COMPARE CLIENT               
         BNE   NO                  ERROR                                        
         BAS   RE,CHKOFG           SET SVOFFG                                   
         B     CLTACCX                                                          
*                                                                               
CLTACC5  CLC   TEMPOFF,SVACCESS+1  MATCH ON OFFICE CODE                         
         BNE   NO                                                               
         BAS   RE,CHKOFG           SET SVOFFG                                   
         B     CLTACCX                                                          
*                                                                               
CLTACC6  BAS   RE,CALLOFF          CALL OFFICER TO SEE OFF IN OFF GRP           
         BNE   NO                                                               
         MVC   SVOFFG,SVACCESS     SET OFFICE GROUP                             
*                                                                               
CLTACCX  B     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CALL OFFICER TO CHECK IF OFFICE CODE IN GROUP                      
*        IF ROUTINE RETURNS TO CALLER OFFICE OKAY                               
*        NTRY - TEMPOFF IS THE CLIENT OFFICE                                    
***********************************************************************         
CALLOFF  NTR1                                                                   
*                                                                               
         XC    RDUB,RDUB           SET UP PARAMATER BLOCK                       
         LA    R1,RDUB                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'         PASS SYSTEM                                  
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,SVACCESS    PASS LIMIT ACCESS                            
         MVC   OFCOFC,TEMPOFF      PASS CLIENT OFFICE                           
         MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH        USE ALPHA AGENCY OF OTHER MEDIA FILE          
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH       (FOR SPLIT FILES)                             
         DROP  R1                                                               
         GOTO1 VOFFICER,RPARM,RDUB,ACOM                                         
         CLI   0(R1),0             RETURN CC CODE                               
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALPRD - VALIDATES A  PRODUCT CODE                                            
*        NTRY - R1=A(FIELD HEADER OF PRODUCT FIELD)                             
*        EXIT - CC=EQUAL AND EXTRACTS PRODUCT VALUES                            
*               CC=NOT EQUAL ON ERROR WITH FVMSGNO SET                          
***********************************************************************         
VALPRD   MVI   FVMINL,0            SET FIELD NOT REQUIRED                       
         MVI   FVMAXL,L'QPRD                                                    
         GOTO1 AFVAL                                                            
         BH    VALPRDX            ERROR - EXIT                                  
         BE    VALPRD5                                                          
         BAS   RE,CLRPRDV         CLEAR VALUES                                  
         B     VALPRDX            THEN EXIT                                     
*                                                                               
VALPRD5  CLC   QPRD,FVIFLD        TEST CHANGE OF PRODUCT                        
         BE    VALPRDX                                                          
         BAS   RE,CLRPRDV         CLEAR VALUES                                  
         OC    QCLT,QCLT          MUST HAVE CLIENT TO                           
         BZ    VALPRD30           ENTER PRODUCT                                 
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   VALPRD10                                                         
         BAS   RE,VALPPRD                                                       
         BNE   VALPRD30                                                         
         B     VALPRD20                                                         
*                                                                               
VALPRD10 BAS   RE,VALSPRD         VALIDATE PRD - SPOT/NET SYS WAY               
         BNE   VALPRD30                                                         
*                                                                               
VALPRD20 MVC   QPRD,FVIFLD                                                      
         B     VALPRDX                                                          
*                                                                               
VALPRD30 MVC   FVMSGNO,=AL2(FVFERNF)                                            
*                                                                               
VALPRDX  B     ROUTSX                                                           
         SPACE 2                                                                
CLRPRDV  XC    QPRD(QVALSX-QPRD),QPRD                                           
         XC    BPRD(BVALSX-BPRD),BPRD                                           
         XC    SVPGST,SVPGST                                                    
         XC    SVPPST,SVPPST                                                    
         BR    RE                                                               
         EJECT                                                                  
*======================================*                                        
* VALSPRD - VALIDATE SPOT/NET PRODUCT  *                                        
*         XIT - CC SET                 *                                        
*======================================*                                        
*                                                                               
VALSPRD  NTR1                                                                   
         LA    R2,IOKEY           BUILD KEY OF CLIENT RECORD                    
         USING PRDHDRD,R2                                                       
         XC    PPKEY,PPKEY                                                      
         MVI   PPKEYTYPE,0                                                      
         MVC   PPKEYAM,BAGYMD                                                   
         MVC   PPKEYCLT,BCLT                                                    
         MVC   PPKEYPRD,FVIFLD                                                  
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLI   MYIOERR,0                                                        
         BNE   VALSPRDX           CC CODE SET                                   
         CLC   IOKEY(7),IOKEYSAV                                                
         BNE   VALSPRDX           CC CODE SET                                   
         GOTO1 MYIOL,RPARM,IOGET+IO1,=C'F'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         CLI   PPGSTCODE,0                                                      
         BE    *+10                                                             
         MVC   SVPGST,PPGSTCODE                                                 
         MVC   SVPPST,PPPST                                                     
         CR    RB,RB                                                            
VALSPRDX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*======================================*                                        
* VALPPRD - VALIDATE PRINT PRODUCT     *                                        
*         XIT - CC <> 0 SETS ERROR MSG *                                        
*               CC =  0                *                                        
*======================================*                                        
*                                                                               
VALPPRD  NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PPRDREC,R2                                                       
         MVC   PPRDKAGY,TWAAGY    AGENCY CODE                                   
         OC    QALPH,QALPH        ALPHA AGENCY FOR OTHER MEDIA FILES            
         BZ    *+10                                                             
         MVC   PPRDKAGY,QALPH     IF USING SPLIT FILES                          
         MVC   PPRDKMED,QMED      MEDIA                                         
         MVI   PPRDKRCD,X'06'     RECORD TYPE                                   
         MVC   PPRDKCLT,QCLT      CLIENT                                        
         MVC   PPRDKPRD,FVIFLD    PRODUCT                                       
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLI   MYIOERR,0                                                        
         BNE   VALPPRDX           CC CODE SET                                   
         CLC   IOKEY(L'PPRDKEY),IOKEYSAV                                        
         BNE   VALPPRDX           CC CODE SET                                   
         GOTO1 MYIOL,RPARM,IOGET+IO1,=C'F'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         CLI   PPRDGST,C'0'       IF ZERO - NO PRODUCT OVERRIDE                 
         BNE   *+10                                                             
         MVC   SVPGST,PPRDGST     GST CODE                                      
*                                                                               
         XR    RE,RE                                                            
         LA    R1,PPRDELEM        R1=A(FIRST ELEMENT)                           
         USING PPRDPST,R1                                                       
*                                                                               
VALPPRD4 CLI   PPRDPST,0          TEST END OF RECORD                            
         BE    VALPPRDY                                                         
         CLI   PPRDPST,X'25'      TEST PST ELEMENT                              
         BNE   *+14                                                             
         MVC   SVPPST,PPRDPSTC    SAVE PST CODES                                
         B     VALPPRDY                                                         
*                                                                               
         ICM   RE,1,1(R1)         BUMP TO NEXT ELEMENT IN CLT REC               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,RE                                                            
         B     VALPPRD4                                                         
*                                                                               
VALPPRDY CR    RB,RB              SET CC CODE                                   
VALPPRDX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*================================================*                              
* DELL  - ROUTINE TO DELETE AN ELEMENT           *                              
*         NTRY - P1, BYTE 0 =L'SEARCH ARGUEMENT  *                              
*                   BYTES 1-3 = A(SEARCH ARGUMENT*                              
*               AIOAREA - SET A(RECORD)          *                              
*               APELCD -  SET TO ELEMENT CODE    *                              
*                         (FIRST BYTE OF APELEM) *                              
*================================================*                              
*                                                                               
DELL     L     R2,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
         GOTO1 VHELLO,APPARM,(C'D',ACC),(APELCD,AIOAREA),((R3),(R2))            
         B     ROUTSX                                                           
         SPACE 3                                                                
*================================================*                              
* ADDL  - ROUTINE TO ADD AN ELEMENT              *                              
*        NTRY - APELEM - SET TO NEW ELEMENT      *                              
*               AIOAREA -SET A(RECORD)           *                              
*================================================*                              
*                                                                               
ADDL     GOTO1 VHELLO,APPARM,(C'P',ACC),AIOAREA,APELEM                          
         CLI   12(R1),0                                                         
         BE    ROUTSX                                                           
         DC    H'0'               DIE ON ALL OTHER ERRORS                       
         EJECT                                                                  
*====================================================================*          
* DOSPECIF - SET UP RECORD SPECIFIC INFO (PMAXNUM, RECTYPE, APTABLE) *          
*====================================================================*          
*                                                                               
DOSPEC   LA    R1,1               POST MAINT = MPRKREG X'01'                    
         LA    R2,MAXRPOST        MAXIMUM POSTING FOR POST RECORD               
         MVC   APTABLE,=A(RPOSTLBL) SET RECORD ROW TABLE                        
         CLI   INREC,RECPOST      CURRENT RECORD POST?                          
         BE    DOSPEC30                                                         
*                                 PROF MAINT = MPRKREG X'01'                    
         LA    R2,MAXPROFS        MAXIMUM PROFILE FOR PROF RECORD               
         MVC   APTABLE,=A(PROFLBL) SET RECORD ROW TABLE                         
         CLI   INREC,RECPROF      CURRENT RECORD PROF?                          
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           APOST = MPRKAOR X'02'                         
         LA    R2,MAXAPSTS        MAXIMUM POSTING FOR APOST RECORD              
         MVC   APTABLE,=A(APSTLBL) SET RECORD ROW TABLE                         
         CLI   INREC,RECAPOST     CURRENT RECORD APOST?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R2,MAXAPST2        MAXIMUM POSTING FOR APOS2 RECORD              
         MVC   APTABLE,=A(APS2LBL) SET RECORD ROW TABLE                         
         CLI   INREC,RECAPOS2     CURRENT RECORD APOS2?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           RPOST = MPRKRTL X'03'                         
         LA    R2,MAXRPSTS        MAXIMUM POSTING FOR RPOST RECORD              
         MVC   APTABLE,=A(RPSTLBL) SET RECORD ROW TABLE                         
         CLI   INREC,RECRPOST     CURRENT RECORD RPOST?                         
         BE    DOSPEC30                                                         
*                                 RPROF = MPRKRTL X'03'                         
         LA    R2,MAXRPRFS        MAXIMUM PROFILE FOR RPROF RECORD              
         MVC   APTABLE,=A(RPRFLBL) SET RECORD ROW TABLE                         
         CLI   INREC,RECRPROF     CURRENT RECORD RPROF?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           SPROF = MPRKPPB X'04'                         
         LA    R2,MAXPPRFS        MAXIMUM PROFILE FOR SPROF RECORD              
         MVC   APTABLE,=A(PPRFLBL) SET RECORD ROW TABLE                         
         CLI   INREC,RECSPROF     CURRENT RECORD SPROF?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R2,MAXSPSTS        MAXIMUM POSTINGS FOR SPOST RECORD             
         MVC   APTABLE,=A(SPSTLBL) SET RECORD ROW TABLE                         
         CLI   INREC,RECSPOST     CURRENT RECORD SPOST?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           UCPOST = MPRKUC  X'05'                        
         LA    R2,MAXPOSTS        MAXIMUM POSTINGS FOR UCPOST RECORD            
         MVC   APTABLE,=A(POSTLBL) SET RECORD ROW TABLE (POST)                  
         CLI   INREC,RECUCPST     CURRENT RECORD UCPOST?                        
         BE    DOSPEC30                                                         
         LA    R1,1(R1)           UNPOST= MPRKUN X'06'                          
         CLI   INREC,RECUNPST     CURRENT RECORD UNPOST?                        
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           UACPOST = MPRKUAC X'07'                       
         LA    R2,MAXAPSTS        MAXIMUM POSTINGS FOR UACPOST RECORD           
         MVC   APTABLE,=A(APSTLBL) SET RECORD ROW TABLE (APOST)                 
         CLI   INREC,RECUACPT     CURRENT RECORD UACPOST?                       
         BE    DOSPEC30                                                         
         LA    R1,1(R1)           UANPOST = MPRKUAN X'08'                       
         CLI   INREC,RECUANPT     CURRENT RECORD UACPOST?                       
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           TPOST = MPRKPST X'09'                         
         LA    R2,MAXTPSTS        MAXIMUM POSTINGS FOR TPOST RECORD             
         MVC   APTABLE,=A(TPSTLBL) SET RECORD ROW TABLE (TPOST)                 
         CLI   INREC,RECTPOST     CURRENT RECORD TPOST?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           DPOST = MPRKDIF X'0A'                         
         LA    R2,MAXPOSTS        MAXIMUM POSTINGS                              
         MVC   APTABLE,=A(POSTLBL) RECORD ROW TABLE                             
         CLI   INREC,RECDPOST     CURRENT RECORD POST?                          
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           BPOST = MPRKRLMG X'0B'                        
         LA    R2,MAXRPSTS        MAXIMUM POSTINGS (USE RPOST #'S)              
         MVC   APTABLE,=A(RPSTLBL) RECORD ROW TABLE                             
         CLI   INREC,RECBPOST     CURRENT RECORD POST?                          
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           XPOST = MPRKTRAD X'0C'                        
         LA    R2,MAXXPST         MAXIMUM POSTINGS                              
         MVC   APTABLE,=A(XPSTLBL) RECORD ROW TABLE                             
         CLI   INREC,RECXPST      CURRENT RECORD XPOST?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R2,MAXXPRF         MAXIMUM PROFILES IN XPROF                     
         MVC   APTABLE,=A(XPRFTBL) RECORD ROW TABLE                             
         CLI   INREC,RECXPROF     CURRENT RECORD XPROF?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           XAPOST = MPRKTRAA X'0D'                       
         LA    R2,MAXXAPST        MAXIMUM POSTINGS                              
         MVC   APTABLE,=A(XAPSTTBL) RECORD ROW TABLE                            
         CLI   INREC,RECXAPST     CURRENT RECORD XAPOST?                        
         BE    DOSPEC30                                                         
*                                                                               
         LA    R2,MAXXAPS2        MAXIMUM POSTING FOR  XAPOS2 RECORD            
         MVC   APTABLE,=A(XAPS2TBL) SET RECORD ROW TABLE                        
         CLI   INREC,RECXAPS2     CURRENT RECORD XAOPS2?                        
         BE    DOSPEC30                                                         
*                                                                               
         LA    R1,1(R1)           MPOST = MPRKMTRA X'0E'                        
         LA    R2,MAXMPST         MAXIMUM POSTINGS                              
         MVC   APTABLE,=A(MPSTTBL) RECORD ROW TABLE                             
         CLI   INREC,RECMPST      CURRENT RECORD XPOST?                         
         BE    DOSPEC30                                                         
*                                                                               
         LA    R2,MAXXPRF         MAXIMUM PROFILES IN XPROF                     
         MVC   APTABLE,=A(MPROTBL) RECORD ROW TABLE                             
         CLI   INREC,RECMPROF     CURRENT RECORD XPROF?                         
         BE    DOSPEC30                                                         
         DC    H'0'                                                             
*                                 RECTYPE -> 1=REG, 2=AOR, 3=RET                
DOSPEC30 STC   R1,RECTYPE         4=PRINT PROD, 5=REG UPFR COMM, 6=REG          
*                                 UPF NET,7=AOR UPF COMM,8=AOR UPF NET          
*                                 9=PST,10=DIFF                                 
         STC   R2,PMAXNUM         MAX NUMBER OF POSTINGS FOR REC TYPE           
         AF    APTABLE,ACRELO     ADD RELO                                      
         B     ROUTSX                                                           
         EJECT                                                                  
*========================================*                                      
* VALEST  - VALIDATES ESTIMATE FIELD     *                                      
*========================================*                                      
*                                                                               
VALEST   DS    0H                                                               
         MVI   FVMINL,0           SET FIELD NOT REQUIRED                        
         MVI   FVMAXL,L'QEST                                                    
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         MVI   FVMAXL,3           FOR SPOT/NET                                  
         GOTO1 AFVAL                                                            
         BH    VALESTX            ERROR -EXIT                                   
         BE    VALEST5                                                          
         BAS   RE,CLRESTV         CLEAR ESTIMATE VALUES                         
         B     VALESTX                                                          
*                                                                               
VALEST5  OC    SCFULL,SCFULL      NO - ESTIMATE ZERO INVALID                    
         BZ    VALEST30                                                         
         TM    FVIIND,FVINUM      MUST BE NUMERIC                               
         BZ    VALEST50                                                         
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   VALEST10                                                         
         BAS   RE,CHKPEST         CHECK ESTIMATE PRINT WAY                      
         BNE   VALEST40                                                         
         B     VALESTX                                                          
*                                                                               
VALEST10 BAS   RE,CHKSEST         CHECK ESTIMATE NET/SPOT WAY                   
         BNE   VALEST40                                                         
         B     VALESTX                                                          
*                                                                               
VALEST30 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALESTX                                                          
VALEST40 MVC   FVMSGNO,=AL2(FVFERNF) RECORD NOT FOUND                           
         B     VALESTX                                                          
VALEST50 MVC   FVMSGNO,=AL2(FVINOTN)  NOT NUMERIC                               
*                                                                               
VALESTX  B     ROUTSX                                                           
         SPACE 3                                                                
CLRESTV  XC    QEST(QVALSX-QEST),QEST                                           
         XC    BEST(BVALSX-BEST),BEST                                           
         BR    RE                                                               
         EJECT                                                                  
*=======================================*                                       
* CHKSEST - CHECKS FOR ESTIMATE RECORD  *                                       
*           ON FILE                     *                                       
*          SETS CC CODE - ON EXIT       *                                       
*=======================================*                                       
*                                                                               
CHKSEST  NTR1                                                                   
         CLC   SCFULL+2(2),BEST   SAME ESTIMATE?                                
         BE    CHKSESTY                                                         
         BAS   RE,CLRESTV         CLEAR CURRENT VALUES                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,SCFULL+3                                                 
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLI   MYIOERR,0                                                        
         BNE   CHKSESTN                                                         
         CLC   IOKEY(8),IOKEYSAV                                                
         BNE   CHKSESTN                                                         
*                                                                               
         MVC   BEST,SCFULL+2      MOVE IN NEW VALUES                            
         SR    RE,RE                                                            
         ICM   RE,3,BEST                                                        
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  QEST,APDUB+6(2)                                                  
*                                                                               
CHKSESTY CR    RB,RB                                                            
         B     CHKSESTX                                                         
*                                                                               
CHKSESTN LTR   RB,RB                                                            
*                                                                               
CHKSESTX B     EXIT                                                             
         EJECT                                                                  
*=======================================*                                       
* CHKPEST- CHECKS FOR ESTIMATE RECORD   *                                       
*           ON PRINT FILE               *                                       
*          SETS CC CODE - ON EXIT       *                                       
*=======================================*                                       
*                                                                               
CHKPEST  NTR1                                                                   
         CLC   SCFULL+2(2),BEST   SAME ESTIMATE?                                
         BE    CHKPESTY                                                         
         BAS   RE,CLRESTV         CLEAR CURRENT VALUES                          
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PESTREC,R2                                                       
         MVC   PESTKAGY,TWAAGY    AGENCY ID                                     
         OC    QALPH,QALPH        ALPHA AGENCY FOR OTHER MEDIA FILE             
         BZ    *+10                                                             
         MVC   PESTKAGY,QALPH     IF USING SPLIT FILES                          
         MVC   PESTKMED,QMED      MEDIA                                         
         MVI   PESTKRCD,X'07'     ESTIMATE RECORD TYPE                          
         MVC   PESTKCLT,QCLT      CLIENT                                        
         MVC   PESTKPRD,QPRD      PRODUCT                                       
         MVC   PESTKEST,SCFULL+2  ESTIMATE NUMBER                               
         GOTO1 MYIOL,RPARM,IOHI+IO1,=C'D'                                       
         CLI   MYIOERR,0                                                        
         BNE   CHKPESTN                                                         
         CLC   IOKEY(L'PPRDKEY),IOKEYSAV                                        
         BNE   CHKPESTN                                                         
*                                                                               
         MVC   BEST(2),SCFULL+2   MOVE IN NEW VALUES                            
         SR    RE,RE                                                            
         ICM   RE,3,BEST                                                        
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  QEST,APDUB+6(2)                                                  
*                                                                               
CHKPESTY CR    RB,RB                                                            
         B     CHKPESTX                                                         
*                                                                               
CHKPESTN LTR   RB,RB                                                            
*                                                                               
CHKPESTX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*========================================*                                      
* VALSRVM - VALIDATE SERVICE MONTH MMM/YY*                                      
*========================================*                                      
*                                                                               
VALSRVM  DS    0H                                                               
         MVI   FVMINL,0            SET FIELD NOT REQUIRED                       
         MVI   FVMAXL,6                                                         
         GOTO1 AFVAL                                                            
         BH    VALSRVX            ERROR - EXIT                                  
         BE    VALSRV5                                                          
         XC    BSRVM,BSRVM                                                      
         B     VALSRVX            THEN EXIT                                     
*                                                                               
VALSRV5  XC    RWORK,RWORK                                                      
         CLC   =C'13/',FVIFLD     IF SPECIAL 13 MONTH                           
         BE    *+14                                                             
         MVC   RWORK(6),FVIFLD                                                  
         B     VALSRV10                                                         
*                                                                               
         MVC   RWORK(4),=C'DEC/'   FORCE IN DEC FOR VALIDATION                  
         MVC   RWORK+4(2),FVIFLD+3  PLUS YEAR                                   
*                                                                               
*        PACK  APDUB,FVIFLD+3(2)                                                
*        CVB   RE,APDUB                                                         
*        STC   RE,BSRVM           YEAR                                          
*        MVI   BSRVM+1,X'0D'      MONTH -13                                     
*        B     VALSRVX                                                          
VALSRV10 GOTO1 VDATVAL,RPARM,(2,RWORK),(X'80',RWORK+8)                          
         OC    RPARM,RPARM                                                      
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSRVX                                                          
*                                                                               
         XC    RWORK(6),RWORK                                                   
         GOTO1 VDATCON,APPARM,(0,RWORK+8),(3,RWORK)                             
         MVC   BSRVM,RWORK       GET Y/M ONLY                                   
         CLC   =C'13/',FVIFLD     IF SPECIAL 13 MONTH                           
         BNE   VALSRVX                                                          
         MVI   BSRVM+1,X'0D'      MONTH - 13                                    
*                                                                               
VALSRVX  B     ROUTSX                                                           
         EJECT                                                                  
*========================================*                                      
* VALINV - VALIDATE INVOICE(BILL NUMBER) *                                      
*========================================*                                      
*                                                                               
VALINV   DS    0H                                                               
         MVI   FVMINL,6            FIELD REQUIRED                               
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BNE   VALINVX                                                          
         XC    BINV(BVALSX-BINV),BINV                                           
         ZIC   RF,FVXLEN           INPUT LENGTH - 1                             
         LA    R1,FVIFLD                                                        
         AR    R1,RF                                                            
         AHI   R1,-3               BUMP BACK 3 TO GET TO 1ST CHAR               
         MVC   RFULL(4),0(R1)                                                   
         CLI   QSYS,C'P'             FOR SPOT/NET CALL SPFMTINO TO              
         BE    VINV50                PACK                                       
         GOTO1 =V(SPFMTINO),APPARM,0,(C'P',RFULL),0,0,RR=ACRELO                 
         L     R1,APPARM+4                                                      
         MVC   BINV,0(R1)                                                       
         B     ROUTSX                                                           
*                                                                               
VINV50   PACK  APDUB,RFULL(4)         GET BILL NUMBER ONLY                      
         CVB   R0,APDUB                                                         
         STCM  R0,3,BINV                                                        
VALINVX  B     ROUTSX                                                           
         EJECT                                                                  
*==========================================================*                    
* RDPROD - READS PRODUCTION LEDGER CLIENT/PRODUCT RECORD   *                    
*          TO GENERATE TABLE                               *                    
*            NTRY - 0(R1)=A(TYPE OF ACCOUNT REQUESTED)     *                    
*                 - 4(R1)=A( ACCOUNT TO PASS BACK )        *                    
*==========================================================*                    
*                                                                               
RDPROD   L     RE,0(R1)                                                         
         MVC   BYTE,0(RE)         ACCOUNT WANTED                                
         L     R4,4(R1)           ACCOUNT TO FIND                               
         XC    0(14,R4),0(R4)     CLEAR ACCOUNT AREA                            
         MVI   RWORK,C'C'         READING FOR CLIENT LEVEL FIRST                
RDPROD2  MVC   IOKEY,SPACES                                                     
         MVC   IOKEY(1),COMPANY2  OTHER ACCFILE(IF APPL)                        
         MVC   IOKEY+1(2),SVPROD                                                
         MVC   IOKEY+3(3),QCLT                                                  
         CLI   RWORK,C'C'         IF READING AT CLIENT LEVEL                    
         BE    *+10               SKIP MOVING PRODUCT IN KEY                    
         MVC   IOKEY+6(3),QPRD                                                  
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE2'                          
         CLI   MYIOERR,0                                                        
         BNE   RDPROD40                                                         
         L     R2,AIOAREA1                                                      
         AH    R2,DATADISP                                                      
         USING PPRELD,R2                                                        
         SR    R0,R0                                                            
RDPROD5  CLI   0(R2),0            END OF RECORD?                                
         BE    RDPROD40                                                         
         CLI   0(R2),PPRELQ       X'24'                                         
         BE    RDPROD10                                                         
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     RDPROD5                                                          
*                                                                               
RDPROD10 CLI   BYTE,MBTTRCV       RECEIVABLES ROW                               
         BNE   RDPROD20                                                         
         CLC   PPRRECV+1(L'PPRRECV-1),SPACES                                    
         BNH   RDPROD30                                                         
         MVC   0(L'PPRRECV-1,R4),PPRRECV+1                                      
         B     RDPROD30                                                         
*                                                                               
RDPROD20 CLI   BYTE,MBTTCOS       IF COSTINGS ROW                               
         BE    *+12                                                             
         CLI   BYTE,MBTTINTC                                                    
         BNE   RDPROD30                                                         
         CLC   PPRCOST+1(L'PPRCOST-1),SPACES                                    
         BNH   RDPROD30                                                         
         MVC   0(L'PPRCOST-1,R4),PPRCOST+1                                      
*                                                                               
RDPROD30 CLC   PPRGAOFF,SPACES                                                  
         BNH   *+10                                                             
         MVC   AOFF,PPRGAOFF      ACCOUNT OFFICE                                
*                                                                               
RDPROD40 CLI   RWORK,C'C'         IF CLIENT LEVEL?                              
         BNE   RDPROD50                                                         
         OC    QPRD,QPRD                                                        
         BZ    RDPROD50                                                         
         MVI   RWORK,C'P'         TRY PRODUCT LEVEL NEXT                        
         B     RDPROD2                                                          
*                                                                               
RDPROD50 MVC   FVMSGNO,=AL2(FVFOK)                                              
RDPRODX  B     ROUTSX                                                           
         SPACE 2                                                                
         EJECT                                                                  
*==========================================================*                    
* RDMI   - READS MEDIA INTERFACE RECORD                    *                    
*            NTRY - 0(R1)=A(TYPE OF ACCOUNT REQUESTED)     *                    
*                 - 4(R1)=A( ACCOUNT TO PASS BACK )        *                    
*==========================================================*                    
*                                                                               
RDMI     L     RE,0(R1)                                                         
         MVC   BYTE,0(RE)         ACCOUNT WANTED                                
         L     R4,4(R1)           ACCOUNT TO FIND                               
         XC    0(14,R4),0(R4)     ACCOUNT AREA                                  
         CLI   SVMI,C'Y'          DO THEY USE MI RECORDS?                       
         BNE   RDMI10                                                           
*                                                                               
         LA    R2,IOKEY                                                         
         USING MINRECD,R2                                                       
         MVC   MINKEY,RSPACES                                                   
         MVI   MINKTYP,MINKTYPQ    X'08'                                        
         MVC   MINKCPY,COMPANY2    OTHER ACCFILE (IF APPL)                      
         MVC   MINKMED(1),QSYS     FIRST BYTE IS SYSTEM                         
         MVC   MINKMED+1(1),QMED   SECOND BYTE IS MEDIA                         
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE2'                          
         CLI   MYIOERR,0                                                        
         BNE   RDMIX                                                            
         DROP  R2                                                               
         L     R2,AIOAREA1                                                      
         AH    R2,DATADISP                                                      
         USING MDIELD,R2                                                        
         SR    R0,R0                                                            
RDMI5    CLI   0(R2),0            END OF RECORD?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),MDIELQ       X'19'                                         
         BE    RDMI10                                                           
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     RDMI5                                                            
*                                                                               
RDMI10   CLI   BYTE,MBTTINC       INCOME ROW?                                   
         BNE   RDMI20                                                           
         CLI   SVMI,C'Y'                                                        
         BE    RDMI15                                                           
         MVC   0(2,R4),=C'SI'                                                   
         MVC   2(1,R4),QSYS                                                     
         MVC   3(1,R4),QMED                                                     
         B     RDMIX                                                            
RDMI15   MVC   0(L'MDICOMM,R4),MDICOMM                                          
         B     RDMIX                                                            
*                                                                               
RDMI20   CLI   BYTE,MBTTNET       NET ROW                                       
         BNE   RDMI30                                                           
         CLI   SVMI,C'Y'                                                        
         BE    RDMI25                                                           
         MVC   0(2,R4),=C'SZ'                                                   
         MVC   2(1,R4),QSYS                                                     
         MVC   3(1,R4),QMED                                                     
         B     RDMIX                                                            
RDMI25   MVC   0(L'MDICNTL,R4),MDICNTL                                          
         B     RDMIX                                                            
*                                                                               
RDMI30   CLI   BYTE,MBTTCD        CASH DISC ROW                                 
         BNE   RDMI40                                                           
         CLI   SVMI,C'Y'                                                        
         BE    RDMI35                                                           
         MVC   0(4,R4),=C'SIMP'                                                 
         B     RDMIX                                                            
RDMI35   MVC   0(L'MDICSHD,R4),MDICSHD                                          
         B     RDMIX                                                            
*                                                                               
RDMI40   CLI   BYTE,MBTTBIL       BILLING ROW                                   
         BNE   RDMI50                                                           
         MVC   0(2,R4),=C'11'                                                   
         CLI   SVMI,C'Y'                                                        
         BE    RDMI45                                                           
         BAS   RE,GETFILT                                                       
         MVC   2(1,R4),RBYTE                                                    
         B     RDMIX                                                            
RDMI45   MVC   2(L'MDICOST,R4),MDICOST                                          
         B     RDMIX                                                            
*                                                                               
RDMI50   CLI   BYTE,MBTTREV       REVENUE ROW                                   
         BNE   RDMIX                                                            
         MVC   0(2,R4),=C'12'                                                   
         CLI   SVMI,C'Y'                                                        
         BE    RDMI55                                                           
         BAS   RE,GETFILT                                                       
         MVC   2(1,R4),RBYTE                                                    
         B     RDMIX                                                            
RDMI55   MVC   2(L'MDICOST,R4),MDICOST                                          
*                                                                               
RDMIX    B     ROUTSX                                                           
         EJECT                                                                  
*================================*                                              
* GETFILT - GETS RSTFILT3 FOR    *                                              
*      DEFAULT IN BILLING & REV  *                                              
*      XIT - RBYTE SET           *                                              
*================================*                                              
*                                                                               
GETFILT  NTR1                                                                   
         MVI   RBYTE,C' '                                                       
         LA    R4,IOKEY                                                         
         USING ACTKEY,R4                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY2   OTHER ACCFILE (IF APPL)                       
         MVC   ACTKUNT(2),=C'SI'  *** USE INCOME UNIT/LEDGER ***                
         MVC   ACTKACT(1),QSYS    SYSTEM                                        
         MVC   ACTKACT+1(1),QMED  MEDIA                                         
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE2'                          
         CLI   MYIOERR,0                                                        
         BNE   GETFILTX                                                         
         DROP  R4                                                               
         L     R4,AIOAREA1                                                      
         AH    R4,DATADISP                                                      
         USING RSTELD,R4                                                        
GETFILT3 CLI   0(R4),0            END OF RECORD                                 
         BE    GETFILTX                                                         
         CLI   0(R4),RSTELQ       RECORD STATUS ELE?                            
         BE    GETFILT5                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETFILT3                                                         
*                                                                               
GETFILT5 MVC   RBYTE,RSTCOSTG     ACCOUNT FILTER 3                              
*                                                                               
GETFILTX XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*====================================================*                          
* RDSYS  -READS PROFILE RULES RECORD AT SYSTEM LEVEL *                          
*        NTRY - P1 - A(BILLING TYPE)                 *                          
*====================================================*                          
*                                                                               
RDSYS    L     RE,0(R1)                                                         
         LA    R2,IOKEY                                                         
         USING MPRRECD,R2                                                       
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,QALPH     SPECIAL MEDIA FILE(IF SET)                    
         MVC   MPRKSYS,QSYS       REQUESTED SYSTEM                              
         MVC   MPRKPRO,0(RE)      BILLING TYPE                                  
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE1'                          
RDSYSX   B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*====================================================*                          
* RDMED  -READS PROFILE RULES RECORD AT MEDIA  LEVEL *                          
*         NTRY  P1 - A(BILLING TYPE)                 *                          
*====================================================*                          
*                                                                               
RDMED    L     RE,0(R1)                                                         
         LA    R2,IOKEY                                                         
         USING MPRRECD,R2                                                       
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATVIE COMPANY CODE                           
         MVC   MPRKSYS,QSYS       REQUESTED SYSTEM                              
         MVC   MPRKALPH,QALPH     SPECIAL MEDIA FILE(IF SET)                    
         MVC   MPRKMED,QMED       MEDIA                                         
         MVC   MPRKPRO,0(RE)      BILLING TYPE                                  
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE1'                          
         B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*====================================================*                          
* RDOFF  -READS PROFILE MAINT RECORD AT OFFICE LEVEL *                          
*        NTRY - P1 - A(BILLING TYPE)                 *                          
*             - P2 - A(OFFICE CODE OR OFFICE GROUP)  *                          
*====================================================*                          
*                                                                               
RDOFF    L     RE,0(R1)                                                         
         L     RF,4(R1)                                                         
         LA    R2,IOKEY                                                         
         USING MPRRECD,R2                                                       
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,QALPH     SPECIAL MEDIA FILE(IF SET)                    
         MVC   MPRKSYS,QSYS       REQUESTED SYSTEM                              
         MVC   MPRKMED,QMED       MEDIA                                         
         MVC   MPRKOFC,0(RF)      OFFICE CODE/GROUP                             
         MVC   MPRKPRO,0(RE)      BILLING TYPE                                  
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE1'                          
         B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*====================================================*                          
* RDCLT  -READS PROFILE MAINT RECORD AT CLIENT LEVEL *                          
*        NTRY - P1 - A(BILLING TYPE)                 *                          
*====================================================*                          
*                                                                               
RDCLT    L     RE,0(R1)                                                         
         LA    R2,IOKEY                                                         
         USING MPRRECD,R2                                                       
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,QALPH     SPECIAL MEDIA FILE(IF SET)                    
         MVC   MPRKSYS,QSYS       REQUESTED SYSTEM                              
         MVC   MPRKMED,QMED       MEDIA                                         
         MVC   MPRKOFC,QOFF       OFFICE CODE/GROUP                             
         MVC   MPRKCLI,QCLT       CLIENT                                        
         MVC   MPRKPRO,0(RE)      BILLING TYPE                                  
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE1'                          
         B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*====================================================*                          
* RDPRD  -READS PROFILE MAINT RECORD AT PRODUCT LVL  *                          
*        NTRY - P1 - A(BILLING TYPE)                 *                          
*====================================================*                          
*                                                                               
RDPRD    L     RE,0(R1)                                                         
         LA    R2,IOKEY                                                         
         USING MPRRECD,R2                                                       
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,QALPH     SPECIAL MEDIA FILE(IF SET)                    
         MVC   MPRKSYS,QSYS       REQUESTED SYSTEM                              
         MVC   MPRKMED,QMED       MEDIA                                         
         MVC   MPRKOFC,QOFF       OFFICE CODE/GROUP                             
         MVC   MPRKCLI,QCLT       CLIENT                                        
         MVC   MPRKPRD,QPRD       PRODUCT                                       
         MVC   MPRKPRO,0(RE)      BILLING TYPE                                  
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE1'                          
         B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*====================================================*                          
* RDCOMP -READS COMPANY RECORD & SETS APPROP VALUES  *                          
*====================================================*                          
*                                                                               
RDCOMPL  NTR1                                                                   
         MVI   RENTRY,1            LOCAL ENTRY                                  
         B     RDCOMP5                                                          
*                                                                               
RDCOMP   MVI   RENTRY,0            GENERAL ENTRY                                
*                                                                               
RDCOMP5  LA    R2,IOKEY                                                         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,COMPANY2                                                 
         L     R2,AIOAREA1                                                      
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO1,=C'SE2'                          
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'               MUST HAVE COMPANY RECORD                      
         SR    R0,R0                                                            
         LR    R1,R2                                                            
         AH    R1,DATADISP        PT TO ELEMENTS                                
         USING CPYELD,R1                                                        
         SR    R0,R0                                                            
*                                                                               
RDCOMP10 CLI   0(R1),0            END OF RECORD?                                
         BNE   *+6                                                              
         DC    H'0'               MUST HAVE X'10' ELEMENT                       
         CLI   0(R1),CPYELQ       COMPANY ELEMENT?                              
         BE    RDCOMP15                                                         
         ICM   R0,1,1(R1)           LENGTH OF ELEMENT                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0              BUMP TO NEXT ELEMENT                          
         B     RDCOMP10                                                         
*                                                                               
RDCOMP15 MVC   SVPROD,CPYPROD     SAVE UNIT/LEDGER FOR CLT/PRODUCT              
         MVI   SVMI,C'Y'          MI RECORDS USED                               
         TM    CPYSTAT4,CPYSMINT                                                
         BO    *+8                                                              
         MVI   SVMI,C'N'                                                        
         MVI   SVCOST,C'Y'        USE COSTING,BILL,REV                          
         TM    CPYSTAT1,CPYSCOST                                                
         BO    *+8                                                              
         MVI   SVCOST,C'N'                                                      
         MVI   SVOFF,0                                                          
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    *+8                                                              
         OI    SVOFF,SVOFFNEW      SET NEW OFFICE CODES                         
         TM    CPYSTAT1,CPYSOROE                                                
         BZ    *+8                                                              
         OI    SVOFF,SVOFFREG      SET OFFICE CODES                             
*                                                                               
         CLI   RENTRY,1           LOCAL ENTRY                                   
         BNE   ROUTSX                                                           
         XIT1                                                                   
         DROP  R1,R2                                                            
         EJECT                                                                  
*=============================================================*                 
* SETFILE - CHECKS IF SPECIAL ACCFILE NEEDED AND SETS VALUES  *                 
*         - ALSO CHECKS IF LIMIT ACCESS                       *                 
*=============================================================*                 
*                                                                               
SETFILE  DS    0H                                                               
         BAS   RE,MACCESS         RE-SET LIMIT ACCESS                           
         OC    QOFF,QOFF          IF NEITHER OFFICE                             
         BNZ   SETF2                                                            
         OC    QCLT,QCLT          NOR CLIENT SPECIFIED                          
         BNZ   SETF2                                                            
         CLI   SVPROF+5,C'Y'      AND LIMIT ACCESS ENABLED                      
         BNE   SETF2                                                            
         OC    SVACCESS(2),SVACCESS  AND MEDIA LIMIT ACCESS DEFINED             
         BZ    SETF2                                                            
         CLI   SVACCESS,C'+'      OTHER THAN MARKET LOCKOUT                     
         BE    SETF2                                                            
         MVC   FVMSGNO,=AL2(FVSECLOK) GIVE SECURITY LOCKOUT MESSAGE             
         B     ROUTSX                                                           
*                                                                               
SETF2    LA    R0,SYSSWMAX        MAX NUMBER OF ENTRIES                         
         LA    RE,ASSWTAB                                                       
         USING SYSSWTAB,RE                                                      
*                                                                               
SETF5    CLI   SYSSWSOV,6         LOOK FOR ACC SYS ENTRY                        
         BE    SETF10                                                           
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,SETF5                                                         
         DC    H'0'                                                             
*                                                                               
SETF10   MVC   SVSE1,SYSSWSYS     SAVE NATIVE SE NUMBER                         
         MVC   COMPANY2,COMPANY   DEFAULT COMPANY CODES ARE EQUAL               
         DROP  RE                                                               
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         BAS   RE,SETPST          SET UP CALL TO ACPOSTER                       
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS (ACPOSTER READS           
*                                 MESS UP FLAG)                                 
         DROP  R3                                                               
         USING ACPRTND,R3                                                       
         MVI   APBYTE,MTPFDACC    LOOK FOR SPECIAL ACC FILE                     
         BAS   RE,GETROW                                                        
         OC    ACPFVAL,ACPFVAL                                                  
         BZ    SETF15                                                           
         CLC   TWAAGY,ACPFVAL     IF SAME AGENCY                                
         BNE   SETF20                                                           
SETF15   MVC   SVSE2,SVSE1        SET EQUAL-SO DON'T SW TOO WRONG FILE          
         B     SETFX                                                            
SETF20   MVC   RHALF,ACPFVAL      GET NEW SE NUMBER                             
         BAS   RE,GETID                                                         
         L     R1,AIOAREA3                                                      
         USING CT5REC,R1                                                        
         LA    R2,CT5DATA                                                       
         USING CTSYSD,R2                                                        
         MVI   RBYTE,6            ACC SYSTEM                                    
         SR    R0,R0                                                            
SETF55   CLI   CTSYSEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTSYSEL,CTSYSELQ   SYSTEM ELEMENT?                               
         BNE   SETF60                                                           
         CLC   CTSYSNUM,RBYTE                                                   
         BE    SETF65                                                           
SETF60   ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     SETF55                                                           
*                                                                               
SETF65   MVC   COMPANY2,CTSYSAGB    OTHER COMPANY CODE (IF APPL)                
         MVC   SVSE2,CTSYSSE        SAVE NEW SE NUMBER                          
         GOTO1 FUDGTAB,APPARM,(R2)  FUDGE GENERALS SWITCH TABLE                 
         BAS   RE,RDCOMPL           READ COMPANY REC & SET VALUES               
*                                                                               
SETFX    MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ROUTSX                                                           
         DROP  R3,R2,R1                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO SET LIMIT ACCESS                                                   
***********************************************************************         
MACCESS  NTR1                                                                   
*                                                                               
         XC    SVACCESS,SVACCESS  CLEAR LIMIT ACCESS                            
         CLI   SVPROF+5,C'Y'      IF LIMIT ACCESS ENABLED                       
         BNE   MACCESSX                                                         
         MVI   RBYTE,2            LOOK FOR APPROPRIATE MEDIA ENTRY              
         CLI   QSYS,C'S'                                                        
         BE    MACCESS5                                                         
         MVI   RBYTE,3                                                          
         CLI   QSYS,C'N'                                                        
         BE    MACCESS5                                                         
         MVI   RBYTE,4                                                          
*                                                                               
MACCESS5 LA    R0,SYSSWMAX        MAX NUMBER OF ENTRIES                         
         LA    RE,ASSWTAB                                                       
         USING SYSSWTAB,RE                                                      
*                                                                               
MACCESS8 CLC   SYSSWSOV,RBYTE     LOOK FOR MEDIA SYSTEM ENTRY                   
         BE    MACCESS9                                                         
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,MACCESS8                                                      
         B     MACCESSX            NO MEDIA SYSTEM - NO LIMIT                   
*                                                                               
MACCESS9 MVC   SVACCESS,SYSSWACS  SAVE LIMIT ACCESS                             
         DROP  RE                                                               
*                                                                               
         CLI   SVACCESS,C'$'      OFFICE LIST/GROUP                             
         BNE   MACCESSX           NO: LEAVE ACCESS ALONE                        
         CLI   SVACCESS+2,C' '    YES: MAKE SURE SINGLE BYTE FORMAT             
         BNH   MACCESSX                                                         
*                                                                               
         XC    RWORK,RWORK        CONVERT TWO CHAR OFFICE LIST TO ONE           
         LA    R4,RWORK                                                         
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'S'        SPOT SYSTEM OR                                
         CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BNE   *+8                                                              
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCMOL2,SVACCESS+1 TWO CHARACTER OFFICE LIST                     
         OI    OFCINDS,OFCIMOLC   CONVERT OFFICE LIST                           
         GOTO1 VOFFICER,RPARM,(C'2',RWORK),(0,ACOM)                             
         JNE   *+2                INVALID OFFICE LIST, SHOULDN'T HAPPEN         
         XC    SVACCESS+1(2),SVACCESS+1  CLEAR THE TWO CHARACTER                
         MVC   SVACCESS+1(1),OFCMOL      REPLACE WITH THE ONE CHARACTER         
         DROP  R4                                                               
*                                                                               
MACCESSX B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* SETPST - SETS UP BASIC BLOCK FOR CALL TO ACPOSTER                             
***********************************************************************         
SETPST   NTR1                                                                   
*                                                                               
         XC    IOKEY,IOKEY        READ DUMMY ACC REC TO SWITCH                  
         MVC   IOKEY(1),COMPANY   TO NATIVE ACC SSYTEM BEFORE CALLING           
         GOTO1 MIOACCL,RPARM,IORD+IOACCFIL+IO3,=C'SE1'                          
         LA    RE,MAXPNUM         CLEAR TABLE OF POSTINGS                       
         L     RF,APSTTBL                                                       
SETP5    XC    0(ACPRTNL,RF),0(RF)                                              
         LA    RF,ACPRTNL(RF)                                                   
         BCT   RE,SETP5                                                         
*                                                                               
         LA    RE,PSTBLK          CLEAR CONTROL BLOCK                           
         LA    RF,ACPOSTL                                                       
         XCEFL                                                                  
*                                                                               
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         USING ACPOSTD,R3                                                       
         MVC   ACPACOM,ACOM       A(COMFACS)                                    
         MVC   ACPSW,VSWITCH      A(SWITCH)                                     
         MVC   ACPPOST,APSTTBL    A(POSTINGS RETURNED)                          
         MVC   ACPCMPC,COMPANY    COMPANY CODE                                  
         MVC   ACPCMPC2,COMPANY2  OTHER COMPANY CODE                            
         MVC   ACPSPROD,SVPROD    PRODUCTION UNIT/LEDGER CODE                   
         MVC   ACPCOST,SVCOST     C'Y' - USE COST,BILL,REV                      
         MVC   ACPMI,SVMI         C'Y' IF MI RECORDS IN USE                     
         MVC   ACPSYS,QSYS        SYSTEM                                        
         MVC   ACPALPH,QALPH      AGENCY ALPHA FOR OTHER MEDIA FILE             
         MVC   ACPMED,QMED        MEDIA CODE                                    
         MVC   ACPSE1,SVSE1       SE NUMBER 1                                   
         MVC   ACPSE2,SVSE2       SE NUMBER 2                                   
         MVC   ACPUTL,SCAUTL      PASS UTL ADDRESS                              
         OC    QCLT,QCLT          CLIENT ENTERED?                               
         BNZ   SETP20                                                           
         MVC   ACPOFC,QOFF        OFFICE                                        
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
         CLI   QOFFIND,C'O'       WAS OFFICE CODE ENTERED?                      
         BE    SETP30                                                           
         MVI   ACPOFC,0           OFFICE                                        
         MVC   ACPOFG,QOFF        OFFICE GROUP                                  
         B     SETP30                                                           
*                                                                               
SETP20   MVC   ACPOFC,SVCOFF      OFFICE                                        
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
SETP30   MVC   ACPCLT,QCLT        CLIENT                                        
         MVC   ACPPRD,QPRD        PRODUCT                                       
         MVI   ACPTYPE2,MPRKREG   REG                                           
         MVI   ACPTYPE,1          PROF RECORDS                                  
         MVI   ACPIND,ACPEXP      ASK FOR AMT & MEMO EXPRESSIONS                
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=====================================*                                         
* GETID - READ ID RECORD FROM CNTRL FL*                                         
*    ENTRY - HALF = ALPHA ID          *                                         
*=====================================*                                         
*                                                                               
GETID    NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R1,IOKEY                                                         
         USING CT5REC,R1                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,RHALF                                                   
         GOTO1 AIO,IORD+IOCTFILE+IO3                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R1                                                               
         SPACE                                                                  
*--------------------------------------------------------------------*          
* FUDGTAB - FUDGES GENERAL'S SYSTEM SWITCH TBL- BY ADD/CHANG AN ENTRY*          
*      NTRY - A (CONTROL ID ELEMENT) ,RBYTE = SYSTEM NUMBER          *          
*--------------------------------------------------------------------*          
*                                                                               
FUDGTAB  NTR1                                                                   
         L     R2,0(R1)           CONTROL ID ELEMENT                            
         USING CTSYSD,R2                                                        
         CLI   RBYTE,6            IF ACC - ADD THE ENTRY                        
         BE    FUDG30                                                           
*                                 FOR MEDIA - FUDGE ENTRY IF THERE              
         LA    RE,ASSWTAB         A(SYSTEM SWITCH TABLE)                        
         USING SYSSWTAB,RE                                                      
         LA    R0,SYSSWMAX        MAX NUMBER OF ENTRIES                         
         LA    RE,ASSWTAB                                                       
FUDG10   CLC   SYSSWSOV,RBYTE     LOOK FOR MEDIA ENTRY                          
         BE    FUDG38                                                           
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,FUDG10                                                        
*                                 IF NO MEDIA ENTRY - ADD IT                    
FUDG30   LA    RE,ASSWTAB         A(SYSTEM SWITCH TABLE)                        
         USING SYSSWTAB,RE                                                      
         LA    R0,SYSSWMAX        MAX NUMBER OF ENTRIES                         
         LA    RE,ASSWTAB                                                       
FUDG37   OC    0(SYSSWLEN,RE),0(RE)                                             
         BZ    FUDG38                                                           
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,FUDG37                                                        
         DC    H'0'               NO ROOM TO FUDGE IT IN                        
*                                                                               
FUDG38   MVC   SYSSWSOV,RBYTE     SYSTEM ENTRY                                  
         MVC   SYSSWACS,CTSYSLMT  LIMIT ACCESS                                  
         MVC   SYSSWAGB,CTSYSAGB  OTHER COMPANY CODE                            
         MVC   SYSSWSYS,CTSYSSE   SE NUMBER                                     
         B     EXIT                                                             
         DROP  R2,RE                                                            
         EJECT                                                                  
*=====================================*                                         
* GETROW - PTS R3 TO CORRECT ROW      *                                         
*          IN POSTINGS RETURNED       *                                         
*=====================================*                                         
*                                                                               
GETROW   NTR1                                                                   
         ZIC   R1,APBYTE          ROW NUMBER                                    
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL                                                       
         MR    RE,R1                                                            
         L     R3,APSTTBL                                                       
         AR    R3,RF                                                            
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*==================================================*                            
* MYIO - READS SPOT/NET/PRINT RECORDS              *                            
* NOTE: ON XIT - THIS ROUTINE DOESN'T GO TO ROUTSX *                            
*       BUT EXITS (IOAREA MUST STAY CHANGED)       *                            
*==================================================*                            
*                                                                               
MYIOL    NTR1                                                                   
         MVI   RBYTE,1            LOCAL ENTRY                                   
         B     MYIO5                                                            
*                                                                               
MYIO     DS    0H                                                               
         MVI   RBYTE,0            GENERAL ENTRY                                 
*                                                                               
MYIO5    DS    0H                                                               
         L     RF,4(R1)           TYPE OF READ                                  
         MVC   RBYTE2,0(RF)                                                     
*                                                                               
         L     R1,0(R1)           IOAREA+IOCMND                                 
*                                                                               
         CLI   RBYTE2,C'D'        DIRECTORY READ                                
         BNE   MYIO20                                                           
         CLI   QSYS,C'S'          SPOT SYS?                                     
         BNE   MYIO10                                                           
         LA    R1,IOACCSPD(R1)    I/O SPTDIR FROM ACC                           
         B     MYIO40                                                           
*                                                                               
MYIO10   CLI   QSYS,C'N'          NET SYS?                                      
         BNE   MYIO15                                                           
         LA    R1,IOACCNTD(R1)    I/O NET SPTDIR FROM ACC                       
         B     MYIO40                                                           
*                                                                               
MYIO15   CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BE    *+6                                                              
         DC    H'0'               NO OTHER SYSTEMS LEFT                         
         LA    R1,IOACCPTD(R1)    I/O PRTDIR FROM ACC                           
         B     MYIO40                                                           
*                                                                               
MYIO20   CLI   RBYTE2,C'F'        FILE READ                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QSYS,C'S'          SPOT SYSTEM?                                  
         BNE   MYIO30                                                           
         LA    R1,IOACCSPF(R1)    IO SPOTFIL FROM ACC                           
         B     MYIO40                                                           
*                                                                               
MYIO30   CLI   QSYS,C'N'          NET SYSTEM?                                   
         BNE   MYIO35                                                           
         LA    R1,IOACCNTF(R1)    IO NET SPTFIL FROM ACC                        
         B     MYIO40                                                           
*                                                                               
MYIO35   CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOACCPTF(R1)    IO PRTFILE FROM ACC                           
         B     MYIO40                                                           
*                                                                               
MYIO40   MVI   MYIOERR,0                                                        
         MVI   IOSWSE,0           CLEAR SE # FROM ACC CALLS                     
         GOTO1 AIO                                                              
         BE    MYIOX                                                            
         MVI   MYIOERR,1          ERROR                                         
         CLC   FVMSGNO,=AL2(FVFIOER)                                            
         BNE   MYIOX                                                            
         LA    R2,APPARM                                                        
         USING GETTXTD,R2                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSYS,X'FF'       GENERAL SYSTEM MESSAGES                       
         MVI   GTMTYP,GTMERR      ERROR MSG                                     
         MVI   GTMSGNO1,X'62'     NUMBER 98                                     
         LA    R1,STOPMSG         TO REPLACE &T                                 
         STCM  R1,7,GTATXT                                                      
         LA    R1,L'STOPMSG                                                     
         STC   R1,GTLTXT                                                        
         GOTO1 VGETTXT,GETTXTD                                                  
         OI    TRAMSGH+6,X'80'                                                  
         DROP  R2                                                               
         L     RD,ACWORKA                                                       
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
MYIOX    CLI   RBYTE,1            LOCAL ENTRY                                   
         BNE   EXIT                                                             
         XIT1                                                                   
         EJECT                                                                  
*==================================================*                            
* MIOACC - READS ACC RECORDS                       *                            
* NOTE: ON XIT - THIS ROUTINE DOESN'T GO TO ROUTSX *                            
*       BUT EXITS (IOAREA MUST STAY CHANGED)       *                            
*==================================================*                            
*                                                                               
MIOACCL  NTR1                                                                   
         MVI   RBYTE,1            LOCAL ENTRY                                   
         B     MIOAC5                                                           
*                                                                               
MIOACC   MVI   RBYTE,0            GENERAL ENTRY                                 
*                                                                               
MIOAC5   L     RF,4(R1)           TYPE OF READ                                  
         MVC   RFULL(3),0(RF)                                                   
         L     R1,0(R1)           IOAREA+IOCMND+IOFILE                          
*                                                                               
         MVI   IOSWSE,0           READ RECORD IN NATIVE SYSTEM                  
         CLC   =C'SE1',RFULL                                                    
         BE    MIOAC30                                                          
         CLC   =C'SE2',RFULL                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SVSE2,0            ANY SECOND SE NUMBER                          
         BE    MIOAC30            TRY TO READ IN NATIVE SYS                     
         CLC   SVSE1,SVSE2        IF 2ND SE = NATIVE                            
         BE    MIOAC30            READ IN NATIVE SYS                            
         MVC   IOSWSE,SVSE2       SET SE NUMBER TO READ                         
*                                                                               
MIOAC30  MVI   MYIOERR,0                                                        
         GOTO1 AIO                                                              
         BE    MIOACX                                                           
         BH    *+6                                                              
         DC    H'0'               HARD IO ERROR                                 
         MVI   MYIOERR,1          SOFT ERROR                                    
*                                                                               
MIOACX   CLI   RBYTE,1            LOCAL ENTRY                                   
         BNE   EXIT                                                             
         XIT1                                                                   
         EJECT                                                                  
*========================================*                                      
* VALIDT  - VALIDATE INV DATE --         *                                      
*           CAN BE M/Y OR M/D/Y          *                                      
*========================================*                                      
*                                                                               
VALIDT   MVI   FVMINL,0            SET FIELD NOT REQUIRED                       
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BH    VALIDTX            ERROR - EXIT                                  
         BE    VALIDT5                                                          
         XC    BIDT,BIDT                                                        
         XC    QIDT,QIDT                                                        
         B     VALIDTX            THEN EXIT                                     
*                                                                               
VALIDT5  GOTO1 VDATVAL,RPARM,(0,FVIFLD),(X'80',RWORK)                           
         OC    RPARM,RPARM                                                      
         BZ    VALIDT10           NOT M/D/Y - TRY M/Y                           
         GOTO1 VDATCON,RPARM,(0,RWORK),(3,RFULL)                                
         MVC   BIDT,RFULL                                                       
         MVC   QIDT,RWORK                                                       
         B     VALIDTX                                                          
*                                                                               
VALIDT10 GOTO1 VDATVAL,RPARM,(2,FVIFLD),(X'80',RWORK)                           
         OC    RPARM,RPARM        TRY -  M/Y                                    
         BZ    VALIDERR                                                         
         MVC   QIDT,RWORK                                                       
         MVC   RWORK+4(2),=C'01'  FOR M/Y - FUDGE                               
         GOTO1 VDATCON,APPARM,(0,RWORK),(3,RFULL)                               
         MVC   BIDT,RFULL                                                       
         MVI   BIDT+2,0           INDICATE NO DAY                               
*                                                                               
VALIDTX  B     ROUTSX                                                           
         SPACE                                                                  
VALIDERR MVC   FVMSGNO,=AL2(FVIDATE)                                            
         B     VALIDTX                                                          
         EJECT                                                                  
*==============================================*                                
* VALRDT  - VALIDATE RUN DATE -- M/D/Y OR M/Y  *                                
*==============================================*                                
*                                                                               
VALRDT   MVI   FVMINL,0            SET FIELD NOT REQUIRED                       
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BH    VALRDTX            ERROR - EXIT                                  
         BE    VALRDT5                                                          
         XC    BRDT,BRDT                                                        
         XC    QRDT,QRDT                                                        
         B     VALRDTX            THEN EXIT                                     
*                                                                               
VALRDT5  GOTO1 VDATVAL,RPARM,(0,FVIFLD),(X'80',RWORK)                           
         OC    RPARM,RPARM                                                      
         BZ    VALRDT10           NOT M/D/Y - TRY M/Y                           
         GOTO1 VDATCON,RPARM,(0,RWORK),(3,RFULL)                                
         MVC   BRDT,RFULL                                                       
         MVC   QRDT,RWORK                                                       
         B     VALRDTX                                                          
*                                                                               
VALRDT10 GOTO1 VDATVAL,RPARM,(2,FVIFLD),(X'80',RWORK)                           
         OC    RPARM,RPARM                                                      
         BZ    VALRDERR            INVALID DATE                                 
         MVC   QRDT,RWORK                                                       
         MVC   RWORK+4(2),=C'01'  FOR M/Y FUDGE DAY                             
         GOTO1 VDATCON,RPARM,(0,RWORK),(3,RFULL)                                
         MVC   BRDT,RFULL                                                       
         MVI   BRDT+2,0           INDICATE NO DAY                               
*                                                                               
VALRDTX  B     ROUTSX                                                           
         SPACE                                                                  
VALRDERR MVC   FVMSGNO,=AL2(FVIDATE)                                            
         B     VALRDTX                                                          
         EJECT                                                                  
*==============================================================*                
* VALFLD  - VALIDS FIELDS ACCORDING TO PARM LIST GIVEN         *                
*    NTRY - P1= A(START FIELD)                                 *                
*           P2= # OF UNPROTECTED FIELDS TO CHECK (SKIPS        *                
*               UNPROTECTED ONES)                              *                
*    XIT  - P2 BYTE 0 - SET TO 00 IF ALL FIELDS BLANK          *                
*                       AND FF IF NOT ALL FIELDS BLANK         *                
*==============================================================*                
*                                                                               
VALFLDL  NTR1                                                                   
         MVI   RBYTE,1            LOCAL ENTRY                                   
         B     VALFLD2                                                          
*                                                                               
VALFLD   MVI   RBYTE,0            GENERAL ENTRY                                 
*                                                                               
VALFLD2  DS    0H                                                               
         LR    R4,R1              SAVE ADDRESS OF PARM LIST                     
         LM    R2,R3,0(R1)                                                      
         SLL   R2,8               R2 =A(FIRST FIELD)                            
         SRL   R2,8                                                             
         SLL   R3,8               R3 = # OF FIELDS                              
         SRL   R3,8                                                             
*                                                                               
VALFLD5  CLI   0(R2),0            IF REACHED END OF SCREEN                      
         BE    VALFLDY            EXIT                                          
         TM    1(R2),X'20'        SKIP PROTECTED FIELDS                         
         BNO   VALFLD10                                                         
         BAS   RE,BUMP                                                          
         B     VALFLD5                                                          
*                                                                               
VALFLD10 CLI   5(R2),0            ANY DATA?                                     
         BNE   VALFLDNO           DATA FOUND                                    
         BAS   RE,BUMP                                                          
         BCT   R3,VALFLD5                                                       
         B     VALFLDY                                                          
*                                                                               
VALFLDY  MVI   0(R4),0            INDICATE ALL FIELDS BLANK                     
         B     VALFLDX                                                          
*                                                                               
VALFLDNO MVI   0(R4),X'FF'        INDICATE NOT ALL FIELDS BLANK                 
*                                                                               
VALFLDX  CLI   RBYTE,1                                                          
         BNE   ROUTSX                                                           
         XIT1                                                                   
*                                                                               
BUMP     DS    0H                 BUMP TO NEXT FIELD                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
*==================================*                                            
* ENTRY POINT FOR OPTION ROUTINES  *                                            
*==================================*                                            
*                                                                               
OBASE    NTR1  BASE=ACBASE1,LABEL=NO                                            
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         L     RE,4(RD)                                                         
         MVC   0(4,RE),=C'+OBA'                                                 
         SRL   RF,24               DETERMINE OPTION NUMBER (1 BASED)            
         SLL   RF,2                                                             
         CH    RF,=AL2(OPTMAX)                                                  
         BNH   *+2(RF)                                                          
         DC    H'0'                OPTION NUMBER OUSIDE RANGE                   
         SPACE 2                                                                
OBRANCH  B     VALTYPE                                                          
         B     VALTYPE                                                          
         B     VALXDAT                                                          
         B     VALVAL                                                           
         B     VALACC                                                           
         B     VALAMT                                                           
         B     VALMEMO                                                          
OPTMAX   EQU   *-OBRANCH                                                        
         EJECT                                                                  
VALTYPE  GOTO1 VSCANNER,ACPARM,FVIHDR,(10,AIOAREA2),C',=/='                     
         SR    R0,R0                                                            
         ICM   R0,1,ACPARM+4                                                    
         BZ    VALTYP50                                                         
         L     R2,AIOAREA2                                                      
         LA    R3,SCWORK                                                        
*                                                                               
VALTYP5  CLI   1(R2),0            NO SECOND HALF                                
         BNE   VALTYP50                                                         
         TM    2(R2),X'40'        MUST BE ALPHA                                 
         BZ    VALTYP50                                                         
         CLI   5(R2),L'INOTYPE                                                  
         BH    VALTYP50                                                         
         LA    RE,TYPETAB         TABLE OF VALID TYPE EXPRESSIONS               
*                                                                               
VALTYP10 CLI   0(RE),X'FF'        END OF TABLE                                  
         BE    VALTYP50                                                         
         CLC   0(L'TYPETAB,RE),12(R2)                                           
         BE    *+12                                                             
         LA    RE,L'TYPETAB(RE)                                                 
         B     VALTYP10                                                         
         MVC   0(L'TYPETAB,R3),0(RE)                                            
         LA    R2,32(R2)          NEXT ENTRY                                    
         LA    R3,L'TYPETAB(R3)                                                 
         BCT   R0,VALTYP5                                                       
         B     VALTYPX                                                          
*                                                                               
VALTYP50 MVC   FVMSGNO,=AL2(FVIBILL)  INVALID BILL TYPE                         
*                                                                               
VALTYPX  XIT1                                                                   
         EJECT                                                                  
*              TABLE OF DIFFERENT TYPE BILLS                                    
*                                                                               
TYPETAB  DS    0CL5                                                             
         DC    CL5'REG  '                                                       
         DC    CL5'UF   '                                                       
         DC    CL5'UFC  '                                                       
         DC    CL5'UFN  '                                                       
         DC    CL5'AOR  '                                                       
         DC    CL5'AORM '                                                       
         DC    CL5'AORU '                                                       
         DC    CL5'AORCU'                                                       
         DC    CL5'AORN '                                                       
         DC    CL5'AORCN'                                                       
         DC    CL5'AORA '                                                       
         DC    CL5'AORC '                                                       
         DC    CL5'AORJ '                                                       
         DC    CL5'RET  '                                                       
         DC    CL5'RETC '                                                       
         DC    CL5'RETP '                                                       
         DC    CL5'RETS '                                                       
         DC    CL5'FIN  '                                                       
         DC    CL5'FINM '                                                       
         DC    CL5'FINR '                                                       
         DC    CL5'ADJ  '                                                       
         DC    CL5'CD   '                                                       
         DC    CL5'CDM  '                                                       
         DC    CL5'CDC  '                                                       
         DC    CL5'COM  '                                                       
         DC    CL5'DIFF '                                                       
         DC    CL5'BPCR '                                                       
         DC    CL5'BPCL '                                                       
         DC    CL5'REGX '                                                       
         DC    CL5'ADJX '                                                       
         DC    CL5'AORXM'                                                       
         DC    CL5'AORXA'                                                       
         DC    CL5'AORXJ'                                                       
         DC    CL5'REGM '                                                       
         DC    CL5'MAN  '                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
VALXDAT  DS    0H                                                               
         XC    APWORK,APWORK                                                    
         GOTO1 VDATVAL,ACPARM,(0,FVIFLD),(X'80',APWORK)                         
         OC    ACPARM,ACPARM                                                    
         BZ    VALXDAT5                                                         
         GOTO1 VDATCON,ACPARM,(0,APWORK),(2,SCWORK)                             
         B     VALXDATX                                                         
*                                                                               
VALXDAT5 MVC   FVMSGNO,=AL2(FVIDATE)  INVALID DATE FORMAT                       
*                                                                               
VALXDATX XIT1                                                                   
         EJECT                                                                  
VALACC   DS    0H                  ACCOUNT, AMOUNT & MEMO ONLY VALID            
VALAMT   DS    0H                  FOR POST TYPE RECORDS                        
VALMEMO  CLI   INREC,RECPROF       IF NUMBER LESS THEN START OF PROF            
         BNL   VALD30                                                           
         LA    R1,REC1             THEN POINT TO POST TABLE                     
         XR    RF,RF               RF=ELEMENT EQUATED VALUE                     
         B     VALD10                                                           
*                                                                               
VALVAL   CLI   INREC,RECPROF      THIS OPTION ONLY VALID FOR PROF'S             
         BE    VALD5                                                            
         CLI   INREC,RECSPROF                                                   
         BE    VALD5                                                            
         CLI   INREC,RECRPROF                                                   
         BNE   VALD30                                                           
VALD5    LA    R1,REC2            PROFILE TABLE                                 
         XR    RF,RF               RF=ELEMENT EQUATED VALUE                     
*                                                                               
VALD10   CLI   0(R1),X'FF'        WHILE NOT END OF TABLE                        
         BE    VALD20                                                           
         LA    RF,1(RF)           BUMP ELEMENT EQUATED NUMBER                   
         ZIC   RE,0(R1)           RE = NUMBER OF ENTRIES IN ROW                 
         LA    R1,1(R1)           BUMP TO DATA FOR ROW                          
         LTR   RE,RE                                                            
         BZ    VALD10                                                           
*                                                                               
VALD15   CLC   0(3,R1),FVIFLD      CHECK DATA OPTION                            
         BE    VALD18                                                           
         LA    R1,3(R1)                                                         
         BCT   RE,VALD15                                                        
         B     VALD10                                                           
*                                                                               
VALD18   MVC   SCWORK(1),FVIFLD                                                 
         STC   RF,SCWORK+1         SET ELEMENT EQUATED NUMBER                   
         B     VALDX                                                            
*                                                                               
VALD20   MVC   FVMSGNO,=AL2(FVFDINV)  INVALID DATA OPTION                       
         B     VALDX                                                            
VALD30   MVC   FVMSGNO,=AL2(FVFKINV)  INVALID OPTION KEYWORD                    
VALDX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* OPTION VALUE TABLE FOR POSTING RECORDS                                        
*                                                                               
REC1     DS    0X                                                               
         DC    X'09',C'P1 C1 N1 A1 11 21 R1 S1 D1 '   ** 01 **                  
         DC    X'08',C'P2 C2 N2 A2 12 22 S4 D2 '      ** 02 **                  
         DC    X'08',C'P3 C3 N3 A3 13 23 S5 D3 '      ** 03 **                  
         DC    X'08',C'P4 C4 N4 A4 14 24 S6 D4 '      ** 04 **                  
         DC    X'09',C'P5 C5 N5 A5 15 25 R5 S8 D5 '   ** 05 **                  
         DC    X'08',C'P6 C6 N6 A6 16 26 S9 D6 '      ** 06 **                  
         DC    X'08',C'P7 C7 N7 A7 17 27 S10D7 '      ** 07 **                  
         DC    X'08',C'P8 C8 N8 A8 18 28 S7 D8 '      ** 08 **                  
         DC    X'03',C'A9 19 29 '                     ** 09 **                  
         DC    X'03',C'A10110210'                     ** 10 **                  
         DC    X'03',C'A11111211'                     ** 11 **                  
         DC    X'03',C'A15115215'                     ** 12 **                  
         DC    X'01',C'S2 '                           ** 13 **                  
         DC    X'01',C'S3 '                           ** 14 **                  
         DC    X'03',C'A10110210'                     ** 15 **                  
         DC    X'03',C'A13113213'                     ** 16 **                  
         DC    X'03',C'A14114214'                     ** 17 **                  
         DC    X'03',C'A16116216'                     ** 18 **                  
         DC    X'00'                                  ** 19 **                  
         DC    X'01',C'T1 '                           ** 20 **                  
         DC    X'01',C'T2 '                           ** 21 **                  
         DC    X'02',C'P9 I17'                        ** 22 **                  
         DC    X'02',C'P10I18'                        ** 23 **                  
         DC    X'02',C'P11I19'                        ** 24 **                  
         DC    X'02',C'P12I20'                        ** 25 **                  
         DC    X'FF'                                                            
         EJECT                                                                  
* OPTION VALUE TABLE FOR PROF RECORDS                                           
*                                                                               
REC2     DS    0X                                                               
         DC    X'01',C'P1 '            ** 01 **                                 
         DC    X'01',C'P3 '            ** 02 **                                 
         DC    X'01',C'P4 '            ** 03 **                                 
         DC    X'01',C'P5 '            ** 04 **                                 
         DC    X'01',C'P6 '            ** 05 **                                 
         DC    X'01',C'P7 '            ** 06 **                                 
         DC    X'01',C'P8 '            ** 07 **                                 
         DC    X'01',C'P9 '            ** 08 **                                 
         DC    X'01',C'P10'            ** 09 **                                 
         DC    X'01',C'P11'            ** 10 **                                 
         DC    X'01',C'P2 '            ** 11 **                                 
         DC    X'01',C'R1 '            ** 12 **                                 
         DC    X'01',C'R4 '            ** 13 **                                 
         DC    X'01',C'R2 '            ** 14 **                                 
         DC    X'01',C'R3 '            ** 15 **                                 
         DC    X'01',C'S1 '            ** 16 **                                 
         DC    X'01',C'S2 '            ** 17 **                                 
         DC    X'01',C'S3 '            ** 18 **                                 
         DC    X'01',C'S4 '            ** 19 **                                 
         DC    X'01',C'S5 '            ** 20 **                                 
         DC    X'01',C'S6 '            ** 21 **                                 
         DC    X'01',C'P12'            ** 22 **                                 
         DC    X'01',C'R5 '            ** 23 **                                 
         DC    X'01',C'S7 '            ** 24 **                                 
         DC    X'01',C'S7 '            ** 25 **  NEVER IMPLEMENTED              
         DC    X'01',C'P13'            ** 26 **                                 
         DC    X'01',C'P14'            ** 27 **                                 
         DC    X'01',C'P15'            ** 28 **                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*--------------*                                                                
* LITERAL POOL *                                                                
*--------------*                                                                
         LTORG                                                                  
*                                                                               
ACC      DC    CL8'ACCOUNT '                                                    
TEMPSTR  DC    CL7'TEMPSTR '                                                    
DMWRITE  DC    CL7'DMWRT   '                                                    
DMREAD   DC    CL7'DMREAD  '                                                    
STOPMSG  DC    CL20'STOP AND CALL DDS - '                                       
         EJECT                                                                  
*===================*                                                           
* OTHER TABLES      *                                                           
*===================*                                                           
*                                                                               
CONADDRS DS    0F                  ** ADDRESS OF ROUTINES ETC. **               
         DC    A(PHASES)                                                        
         DC    A(HOOK)                                                          
         DC    A(OBASE)                                                         
         DC    A(ROUTS)                                                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         SPACE 1                                                                
PHASES   DS    0X                  ** LOADED CORERES PHASES **                  
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QSPDEMUP)                                                    
         DC    AL1(QGETDEM2)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QBOOKVAL)                                                    
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QUNDAY)                                                      
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QXSORT)                                                      
         DC    AL1(QEDITOR)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QSPACNVL)                                                    
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QRANSID)                                                     
         DC    AL1(QMSPACK)                                                     
         DC    AL1(QMSUNPK)                                                     
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QTSAR)                                                       
         DC    AL1(QSPOTBUY)                                                    
         DC    AL1(QSPOTIO)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*====================================================================*          
* - FOR EACH RECORD TYPE A TABLE EXISTS.  THE TABLE IS IN ROW ORDER  *          
* - AND CONSIST OF ONE BYTE ELEMENT TYPE (MBTTYP) FOLLOWED BY LABEL  *          
*====================================================================*          
         SPACE 1                                                                
* --- POST TABLE FOR POST                                                       
         SPACE 1                                                                
RPOSTLBL DC    AL1(01),CL26'** RCVBL **'                                        
         DC    AL1(02),CL26'** INCOME **'                                       
         DC    AL1(03),CL26'** NET **'                                          
         DC    AL1(04),CL26'** CASH DISC **'                                    
         DC    AL1(08),CL26'** OUTPUT GST **'                                   
         DC    AL1(05),CL26'** COSTING **'                                      
         DC    AL1(06),CL26'** BILLING **'                                      
         DC    AL1(07),CL26'** REVENUE **'                                      
         DC    AL1(22),CL26'** INTERNAL INCOME **'                              
         DC    AL1(23),CL26'** INTERNAL COSTING **'                             
         DC    AL1(24),CL26'** INTERNAL BILLING **'                             
         DC    AL1(25),CL26'** INTERNAL REVENUE **'                             
         SPACE 1                                                                
* --- POST TABLE FOR UCPOST/UNPOST/DIFF                                         
         SPACE 1                                                                
POSTLBL  DC    AL1(01),CL26'** RCVBL **'                                        
         DC    AL1(02),CL26'** INCOME **'                                       
         DC    AL1(03),CL26'** NET **'                                          
         DC    AL1(04),CL26'** CASH DISC **'                                    
         DC    AL1(08),CL26'** OUTPUT GST **'                                   
         DC    AL1(05),CL26'** COSTING **'                                      
         DC    AL1(06),CL26'** BILLING **'                                      
         DC    AL1(07),CL26'** REVENUE **'                                      
         SPACE 1                                                                
* --- APOST TABLE                                                               
         SPACE 1                                                                
APSTLBL  DC    AL1(01),CL26'** CLIENT RCVBL **'                                 
         DC    AL1(02),CL26'** MEDIA INCOME **'                                 
         DC    AL1(03),CL26'** NET **'                                          
         DC    AL1(04),CL26'** CASH DISC **'                                    
         DC    AL1(08),CL26'** OUTPUT GST **'                                   
         DC    AL1(05),CL26'** COSTING **'                                      
         DC    AL1(06),CL26'** BILLING **'                                      
         DC    AL1(07),CL26'** REVENUE **'                                      
         DC    AL1(09),CL26'** AOR INCOME **'                                   
         DC    AL1(15),CL26'** AOR BILLINGS **'                                 
         DC    AL1(10),CL26'** AOR REVENUE **'                                  
         DC    AL1(11),CL26'** SELLOFF INC **'                                  
         DC    AL1(16),CL26'** SELLOFF BILLINGS **'                             
         DC    AL1(17),CL26'** SELLOFF REVENUE **'                              
         DC    AL1(12),CL26'** AOR PAY/RCVBL **'                                
         DC    AL1(18),CL26'** INPUT GST **'                                    
         SPACE 2                                                                
* --- APOS2 TABLE                                                               
         SPACE 1                                                                
APS2LBL  DC    AL1(22),CL26'** INTERNAL INCOME **'                              
         DC    AL1(23),CL26'** INTERNAL COSTING **'                             
         DC    AL1(24),CL26'** INTERNAL BILLING **'                             
         DC    AL1(25),CL26'** INTERNAL REVENUE **'                             
         SPACE 1                                                                
* --- RETAIL POST TABLE                                                         
         SPACE 1                                                                
RPSTLBL  DC    AL1(01),CL26'** RCVBL **'                                        
         DC    AL1(05),CL26'** COSTING **'                                      
         SPACE 1                                                                
* --- PST POST TABLE                                                            
         SPACE 1                                                                
TPSTLBL  DC    AL1(20),CL26'** PQ INPUT PST **'                                 
         DC    AL1(21),CL26'** PQ OUTPUT PST **'                                
         DC    AL1(26),CL26'** NB INPUT PST **'                                 
         DC    AL1(27),CL26'** NB OUTPUT PST **'                                
         DC    AL1(28),CL26'** NS INPUT PST **'                                 
         DC    AL1(29),CL26'** NS OUTPUT PST **'                                
         DC    AL1(30),CL26'** NF INPUT PST **'                                 
         DC    AL1(31),CL26'** NF OUTPUT PST **'                                
         DC    AL1(32),CL26'** ON INPUT PST **'                                 
         DC    AL1(33),CL26'** ON OUTPUT PST **'                                
         DC    AL1(34),CL26'** BC INPUT PST **'                                 
         DC    AL1(35),CL26'** BC OUTPUT PST **'                                
         DC    AL1(48),CL26'** PE INPUT PST **'                                 
         DC    AL1(49),CL26'** PE OUTPUT PST **'                                
         SPACE 1                                                                
* --- SPOST( PRINT PRODUCTION/FINANCIAL )  TABLE                                
         SPACE 1                                                                
SPSTLBL  DC    AL1(01),CL26'** CLIENT RCVBL **'                                 
         DC    AL1(13),CL26'** SUSPENSE RCVBL **'                               
         DC    AL1(14),CL26'** REBATE RCVBL **'                                 
         DC    AL1(02),CL26'** INCOME **'                                       
         DC    AL1(03),CL26'** NET **'                                          
         DC    AL1(04),CL26'** CASH DISC **'                                    
         DC    AL1(08),CL26'** OUTPUT GST **'                                   
         DC    AL1(05),CL26'** COSTING **'                                      
         DC    AL1(06),CL26'** BILLING **'                                      
         DC    AL1(07),CL26'** REVENUE **'                                      
         SPACE 1                                                                
* --- XPOST( TRADE BILLING POST)TABLE                                           
         SPACE 1                                                                
XPSTLBL  DC    AL1(01),CL26'** RCVBL **'                                        
         DC    AL1(02),CL26'** INCOME **'                                       
         DC    AL1(03),CL26'** NET **'                                          
         DC    AL1(04),CL26'** CASH DISC **'                                    
         DC    AL1(05),CL26'** COSTING **'                                      
         DC    AL1(06),CL26'** BILLING **'                                      
         DC    AL1(07),CL26'** REVENUE **'                                      
         DC    AL1(36),CL26'** TRADE INCOME **'                                 
         DC    AL1(37),CL26'** TRADE PARTNER **'                                
         DC    AL1(38),CL26'** TRADE BILLING **'                                
         DC    AL1(39),CL26'** TRADE REVENUE **'                                
         SPACE 1                                                                
         DC    AL1(43),CL26'** INTRCMP RCVBL **'                                
         DC    AL1(44),CL26'** INTRCMP PAYBL **'                                
         DC    AL1(40),CL26'** OTHER INCOME **'                                 
         DC    AL1(41),CL26'** OTHER BILLING **'                                
         DC    AL1(42),CL26'** OTHER REVENUE **'                                
         SPACE 1                                                                
* --- XAPOST TABLE (TRADE BILLING WITH AOR POST)                                
         SPACE 1                                                                
XAPSTTBL DC    AL1(01),CL26'** CLIENT RCVBL **'                                 
         DC    AL1(02),CL26'** MEDIA INCOME **'                                 
         DC    AL1(03),CL26'** NET **'                                          
         DC    AL1(04),CL26'** CASH DISC **'                                    
         DC    AL1(05),CL26'** COSTING **'                                      
         DC    AL1(06),CL26'** BILLING **'                                      
         DC    AL1(07),CL26'** REVENUE **'                                      
         DC    AL1(36),CL26'** TRADE INCOME **'                                 
         DC    AL1(37),CL26'** TRADE PARTNER **'                                
         DC    AL1(38),CL26'** TRADE BILLING **'                                
         DC    AL1(39),CL26'** TRADE REVENUE **'                                
         DC    AL1(09),CL26'** AOR INCOME **'                                   
         DC    AL1(15),CL26'** AOR BILLINGS **'                                 
         DC    AL1(10),CL26'** AOR REVENUE **'                                  
         DC    AL1(12),CL26'** AOR PAY/RCVBL **'                                
         SPACE 2                                                                
* --- XAPOS2 TABLE (TRADE BILLING WITH AOR POST EXTENSION) *YKVA*               
         SPACE 1                                                                
XAPS2TBL DC    AL1(43),CL26'** INTRCMP RCVBL **'                                
         DC    AL1(44),CL26'** INTRCMP PAYBL **'                                
         DC    AL1(40),CL26'** OTHER INCOME **'                                 
         DC    AL1(41),CL26'** OTHER BILLING **'                                
         DC    AL1(42),CL26'** OTHER REVENUE **'                                
         SPACE 2                                                                
* --- POST TABLE FOR MPOST (MIDAS TRADE)                                        
         SPACE 1                                                                
MPSTTBL  DC    AL1(01),CL26'** RCVBL **'                                        
         DC    AL1(02),CL26'** INCOME **'                                       
         DC    AL1(03),CL26'** NET **'                                          
         DC    AL1(04),CL26'** CASH DISC **'                                    
         DC    AL1(05),CL26'** COSTING **'                                      
         DC    AL1(06),CL26'** BILLING **'                                      
         DC    AL1(07),CL26'** REVENUE **'                                      
         DC    AL1(40),CL26'** OTHER INCOME **'                                 
         DC    AL1(41),CL26'** OTHER BILLING **'                                
         DC    AL1(42),CL26'** OTHER REVENUE **'                                
         DC    AL1(43),CL26'** INTRCMP RCVBL **'                                
         DC    AL1(44),CL26'** INTRCMP PAYBL **'                                
         DC    AL1(45),CL26'** OPCO INCOME **'                                  
         DC    AL1(46),CL26'** OPCO BILLINGS **'                                
         DC    AL1(47),CL26'** OPCO REVENUE **'                                 
         SPACE 1                                                                
         EJECT                                                                  
* --- PROFILE TABLE                                                             
         SPACE 1                                                                
PROFLBL  DC    AL1(01),CL26'*DDS* ACC FILE'                                     
         DC    AL1(11),CL26'*DDS* MEDIA FILE'                                   
         DC    AL1(02),CL26'*DDS* MAKE POSTING?'                                
         DC    AL1(03),CL26'BILL SOURCE/MEDIA NAME'                             
         DC    AL1(04),CL26'CHECK FOR SJ PRODUCT'                               
         DC    AL1(05),CL26'CHECK FOR PRODUCT LVL 1C'                           
         DC    AL1(06),CL26'SPECIAL NUMBER DELIMITER'                           
         DC    AL1(07),CL26'MOA BACK-UP DATE'                                   
         DC    AL1(08),CL26'MOA FORWARD DATE'                                   
         DC    AL1(09),CL26'MAKE POSTING'                                       
         DC    AL1(10),CL26'INC CC= RPLC CLT LVL 1C'                            
         DC    AL1(22),CL26'PRNT DIV/BCAST PRD SCH'                             
         DC    AL1(26),CL26'PCT OF GROSS (INT)'                                 
         DC    AL1(27),CL26'AOR PCT OF GROSS (INT)'                             
         DC    AL1(28),CL26'OFFICE OVERRIDE (INT)'                              
         SPACE 1                                                                
* --- RETAIL PROFILE TABLE                                                      
         SPACE 1                                                                
RPRFLBL  DC    AL1(12),CL26'UNIT 3 LEDGER'                                      
         DC    AL1(14),CL26'RCVBL ACC ON UNIT 3 PART'                           
         DC    AL1(15),CL26'COST ACC ON UNIT 3 PART'                            
         DC    AL1(13),CL26'CORP BILLG CLT OVERRIDE'                            
         DC    AL1(23),CL26'CORP BILLG PRD OVERRIDE'                            
         DC    AL1(25),CL26'CRP SUMM MKTGRP OVERRIDE'                           
         SPACE 1                                                                
* --- PRINT PROD PROFILE TABLE                                                  
         SPACE 1                                                                
PPRFLBL  DC    AL1(16),CL26'POST RCVBL TO PROD INV(SJ)'                         
         DC    AL1(17),CL26'WORK CODE FOR PROD INV'                             
         DC    AL1(18),CL26'JOB CODE FOR SEP CD (SJ)'                           
         DC    AL1(19),CL26'SUSPENSE SUFFIX'                                    
         DC    AL1(20),CL26'REBATE SUFFIX'                                      
         DC    AL1(21),CL26'POST TO CLT LVL RCVBL'                              
         DC    AL1(24),CL26'POST CD MEMO (SJ)'                                  
         SPACE 1                                                                
* --- TRADE BILLING PROFILE TABLE (XPROF)                                       
         SPACE 1                                                                
XPRFTBL  DC    AL1(29),CL26'PERCENTAGE'                                         
         DC    AL1(30),CL26'BASIS (GROSS,NET)'                                  
         DC    AL1(31),CL26'OFFICE'                                             
         DC    AL1(33),CL26'INTERCOMPANY INC %'                                 
* --- MIDAS TRADE PROFILE TABLE (MPROF)                                         
         SPACE 1                                                                
MPROTBL  DC    AL1(32),CL26'OFFICE'                                             
         DC    AL1(33),CL26'PERCENTAGE'                                         
         EJECT                                                                  
* DDGLOBEQUS                                                                    
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
* ACTRAWRK                                                                      
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFED                                                       
         ORG                                                                    
         SPACE 2                                                                
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFCD                                                       
         ORG                                                                    
         SPACE 2                                                                
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFBD                                                       
         ORG                                                                    
         EJECT                                                                  
*========================*                                                      
* PFK TABLE DSECT        *                                                      
*========================*                                                      
*                                                                               
PFKD     DSECT                                                                  
PFKL     DS    XL1                ONE ENTRY LENGTH                              
PFKREC1  DS    XL1                VALID RECORD                                  
PFKACT1  DS    XL1                VALID ACTION 1                                
PFKACT2  DS    XL1                VALID ACTION 2                                
PFKNAML  DS    XL1                PFK NAME LENGTH                               
PFKNAME  DS    CL20               PFK NAME (MAX LENGTH)                         
PFKLST   DS    XL1                NON-ZERO IF MUST BE IN NESTED LIST            
PFKMNT   DS    XL1                NON-ZERO IF MUST BE IN NESTED MAINT           
         DS    XL2                SPARE                                         
         EJECT                                                                  
*========================*                                                      
* ROUTS S/R LOCAL W/S    *                                                      
*========================*                                                      
*                                                                               
RWRKD    DSECT                                                                  
RIOSAVE  DS    XL(IOAREAX-IOAREA)                                               
RBYTE    DS    XL1                                                              
RBYTE2   DS    XL1                                                              
RFLAG    DS    XL1                                                              
RFLAG2   DS    XL1                                                              
BYTE     DS    XL1                                                              
RENTRY   DS    XL1                                                              
RHALF    DS    H                                                                
RDUB     DS    D                                                                
RDUB2    DS    D                                                                
RFULL    DS    F                                                                
RSPACES  DS    CL42                                                             
RPARM    DS    8F                                                               
RPROF    DS    CL16                                                             
RWORK    DS    XL64                                                             
*                                                                               
TEMPOFF  DS    CL1                                                              
DOTABLE  DS    CL1                INDICATES TABLE TO BE SET                     
*                                                                               
RIO      DS    2000C                                                            
RWRKX    EQU   *                                                                
         EJECT                                                                  
*=========================*                                                     
* OTHER VARIOUS DSECTS    *                                                     
*=========================*                                                     
*                                                                               
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENPRD                                                                      
* SPGENEST                                                                      
* FAGETTXTD                                                                     
* FAFACTS                                                                       
* FATIOB                                                                        
* FASYSLSTD                                                                     
* DDTWABLDD                                                                     
* DEDBLOCK                                                                      
* DDOFFICED                                                                     
* CTGENFILE                                                                     
* PPNEWFILE                                                                     
*                                                                               
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
CLTPHDRD DSECT                     PASSIVE OFFICE/CLIENT HEADER                 
       ++INCLUDE SPGENCLTO                                                      
         ORG CLTPHDRD                                                           
       ++INCLUDE POFFCLTPP                                                      
         ORG                                                                    
PRDHDRD  DSECT                                                                  
*PREFIX=P                                                                       
       ++INCLUDE SPGENPRD                                                       
*PREFIX=                                                                        
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDTWABLDD                                                      
       ++INCLUDE CTGENFILE                                                      
*PREFIX=P  =PPRELEM                                                             
       ++INCLUDE PPNEWFILE                                                      
*PREFIX=                                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055ACTRA00   03/13/19'                                      
         END                                                                    
