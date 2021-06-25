*          DATA SET ACTRA09    AT LEVEL 008 AS OF 02/15/19                      
*PHASE T62209B                                                                  
         TITLE 'TRA09 T62209  BILLING TRANSFER - REPORT OVERLAY'                
T62209   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T62209**,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5            R5 = A(TWA)                                   
         USING SAVAREA,R6         R6 = A(SAVE AREA)                             
         USING WORKD,R7           R7 = A(GLOBAL WORKING STORAGE)                
         L     RC,APALOCAL                                                      
         USING LOCALD,RC          RC = A(LOCAL WORKING STORAGE)                 
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO          RELOCATION FACTOR                             
*                                                                               
         MVI   SPACES2,C' '                                                     
         MVC   SPACES2+1(L'SPACES2-1),SPACES2                                   
*                                                                               
         CLI   APMODE,APMVALQ     VALIDATE REQUEST DETAILS                      
         BE    VALREQ                                                           
         CLI   APMODE,APMREPP     PRINT REPORT                                  
         BE    PRTREP                                                           
*                                                                               
                                                                                
EXIT     CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1                                                                   
*                                                                               
YES      CR    RB,RB                                                            
         B     EXIT                                                             
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALREQ - VALIDATES REQUEST DETAILS                                  *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   DS    0H                                                               
*                                                                               
         CLI   ASONOFF,ASON       ARE WE ONLINE?                                
         BE    *+12               YES                                           
         LA    RF,MYPSTTBL        OFFLINE                                       
         ST    RF,APSTTBL                                                       
*                                                                               
         MVC   INUSER,PERSON      FOR GEN00 TO SHOW REPORT ID                   
         USING REPD,R4                                                          
         L     R4,AREP                                                          
         MVC   REPDESC,=C'PROFILES   '                                          
         DROP  R4                                                               
                                                                                
         USING MPRRECD,R2                                                       
         LA    R2,APRECKEY                                                      
*                                                                               
         LA    R3,TRAWHENH                                                      
         GOTO1 AFVAL,(R3)         ANY WHEN TYPE?                                
         BNE   VRQERR             NO, SO DISPLAY MISSING ERROR                  
                                                                                
         GOTO1 AVALWHEN,TRAWHENH  VALIDATE WHEN                                 
         BE    VRQ10                                                            
                                                                                
         XC    BLOCK,BLOCK                                                      
         GOTO1 VSCANNER,APPARM,TRAWHENH,(2,BLOCK)                               
                                                                                
         USING SCANBLKD,R1                                                      
         LA    R1,BLOCK                                                         
         CLC   =C'SOON',SC1STFLD                                                
         BNE   VRQNOTV                                                          
*                                                                               
         LA    R1,L'SCLINE(R1)                                                  
         ZIC   RF,SC1STLEN                                                      
         CHI   RF,3               IF NAME IS LONGER THEN 3 CHARS, ERROR         
         BH    VRQNOTV                                                          
         CHI   RF,0               IF NAME IS EMPTY THEN ERROR                   
         BE    VRQNOTV                                                          
         MVC   INUSER,SC1STFLD                                                  
         MVI   INWHEN,MIXIOKS     WHEN IS SOON                                  
         DROP  R1                                                               
*                                                                               
VRQ10    GOTO1 AFVAL,TRAOUTH      ANY  OUTPUT TYPE ?                            
         BNE   VRQ20              NO, SO SKIP VALIDATION                        
         GOTO1 AVALOTYP,TRAOUTH   VALIDATE OUTPUT                               
         BNE   VRQX                                                             
*                                                                               
VRQ20    GOTO1 AFVAL,TRADESTH     ANY DESTINATION?                              
         BNE   VRQ30                                                            
         GOTO1 AVALDEST,TRADESTH  DESTINATION                                   
         BNE   VRQX                                                             
*                                                                               
VRQ30    GOTO1 AVALSYS,BTRSYSH    VALIDATE SYSTEM/MEDIA FILE                    
         BE    VRQ35                                                            
         CLC   FVMSGNO,=AL2(FVISPLIT)                                           
         BNE   VRQX                                                             
         CLC   QALPH,TWAAGY                                                     
         BNE   VRQX                                                             
*                                                                               
VRQ35    MVC   MPRKALPH,QALPH     AGY ALPHA FOR SPLIT MED FILE(IF SET)          
         MVC   MPRKSYS,QSYS       SYSTEM                                        
*                                                                               
         LA    R3,BTRMEDH                                                       
         GOTO1 AVALMED,(R3)       VALIDATE MEDIA                                
         BNE   VRQX                                                             
         MVC   MPRKMED,QMED                                                     
         OC    QMED,QMED          IS MEDIA FIELD EMPTY?                         
         BNZ   VRQ40              NO.                                           
         GOTO1 AVALFLD,APPARM,BTROFFH,3                                         
         CLI   APPARM,X'FF'       NO CLIENT/PRODUCT ALLOWED                     
         BE    VRQERR             MISSING INPUT FIELD                           
*                                 VALIDATE OFFICE                               
VRQ40    MVI   OCFLAG,0           MARK NO OFFICE INPUTTED                       
         GOTO1 AVALOFF,BTROFFH                                                  
         BNE   VRQX                                                             
         MVC   MPRKOFC,QOFF                                                     
         OC    QOFF,QOFF          IF NO OFFICE                                  
         BZ    VRQ50              CHECK CLIENT INPUT                            
         MVI   OCFLAG,C'O'        MARK OFFICE INPUTTED                          
*                                                                               
VRQ50    LA    R3,BTRCLTH                                                       
         GOTO1 AVALCLT,(R3)       VALIDATE CLIENT                               
         BNE   VRQX                                                             
         MVC   MPRKCLI,QCLT                                                     
         OC    QCLT,QCLT          IS CLIENT FIELD PRESENT?                      
         BZ    VRQ60              NO                                            
         CLI   OCFLAG,C'O'        YES - CAN'T HAVE OFFICE TOO                   
         BE    VRQNOTV            ERROR                                         
         MVI   OCFLAG,C'C'        SET CLIENT REQUESTED ALONE                    
         B     VRQ70                                                            
*                                                                               
VRQ60    GOTO1 AVALFLD,APPARM,BTRPRDH,1                                         
         CLI   APPARM,X'FF'       CAN'T HAVE PRODUCT                            
         BE    VRQERR             IF PROD -MISSING INPUT FIELD TO CLT           
*                                                                               
VRQ70    GOTO1 AVALPRD,BTRPRDH    VALIDATE PRODUCT                              
         BNE   VRQX                                                             
         MVC   MPRKPRD,QPRD                                                     
*                                                                               
VRQX     B     EXIT                                                             
*                                                                               
VRQNOTV  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VRQX                                                             
VRQERR   STCM  R3,15,APCURSOR                                                   
         MVC   FVMSGNO,=AL2(FVIMISS)                                            
         B     VRQX                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRTREP - PRINT REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   DS    0H                                                               
         XC    IOKEY,IOKEY        READ DUMMY ACC REC TO SWITCH                  
         MVC   IOKEY(1),COMPANY   TO NATIVE ACC SSYTEM BEFORE CALLING           
         GOTO1 AMIOACC,APPARM,IOHI+IOACCFIL+IO3,=C'SE1'                         
         USING CPYELD,R4                                                        
         L     R4,AIOAREA3                                                      
         AH    R4,DATADISP                                                      
PRT10    CLI   0(R4),0            COULD NOT FIND COMPANY CODE                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),CPYELQ                                                     
         BE    PRT20                                                            
         ZIC   R1,CPYLN                                                         
         AR    R4,R1                                                            
         B     PRT10                                                            
*                                                                               
PRT20    MVC   CPY,CPYLOGO        GETTING COMPANY NAME                          
         DROP  R4                                                               
*                                                                               
         USING REPD,R4                                                          
         L     R4,AREP                                                          
                                                                                
         LA    RE,SPECPOOL        SPECPOOL                                      
         ST    RE,REPAPHS                                                       
         MVI   REPWIDTH,REPWREGQ  REGULAR REPORT WIDTH                          
                                                                                
         MVC   REPH2(8),=C'COMPANY:'  DEFAULT HEADINGS                          
         MVC   REPH2+9(L'CPY),CPY                                               
*                                                                               
         LA    R3,REPH3                                                         
         XC    FLAG,FLAG                                                        
         OC    QALPH,QALPH        ANY ALPHA ID?                                 
         BZ    PRT30              NO                                            
         OI    FLAG,ALPHASET      TURN FLAG ON                                  
         CLC   QALPH,TWAAGY       DO THEY MATCH                                 
         BE    PRT30                                                            
         MVC   0(6,R3),=C'ALPHA:'                                               
         MVC   7(L'QALPH,R3),QALPH                                              
         LA    R3,L'REPHS(R3)                                                   
*                                                                               
PRT30    MVC   0(7,R3),=C'SYSTEM:'                                              
         MVC   8(L'QSYS,R3),QSYS                                                
         OI    REPHEADI,REPHSPAC                                                
*                                                                               
         OI    BRKFLAG,SYSLVL     INTIALIZING PAGE BRK FOR SYS LVL              
         OI    FLAG,GETSEQ                                                      
         LA    R1,PROFNAME        SAVING ADDRESS OF BEG. OF TABLE               
         ST    R1,APRONAME                                                      
*                                                                               
PRT40    GOTO1 ASETFILE           CHECK FOR ACCFILE CHANGE OR LIMIT ACC         
         BE    PRT50                                                            
         B     PRTRX                                                            
*                                                                               
*BUILDING KEY                                                                   
*                                                                               
         USING MPRRECD,R2                                                       
PRT50    LA    R2,IOKEY                                                         
                                                                                
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   MOVING IN X'2F'                               
         MVI   MPRKSUB,MPRKSUBQ   MOVING IN X'01'                               
         MVC   MPRKCPY,COMPANY    MOVING IN COMPANY CODE                        
         MVC   MPRKALPH,QALPH     ALPHA AGENCY CODE                             
         MVC   MPRKSYS,QSYS       SYSTEM                                        
         MVC   MPRKMED,QMED       MEDIA                                         
         MVC   MPRKOFC,QOFF       OFFICE                                        
         MVC   MPRKCLI,QCLT       CLIENT                                        
         MVC   MPRKPRD,QPRD       PRODUCT                                       
*                                                                               
*READ HIGH ON RECORD                                                            
*                                                                               
         GOTO1 AMIOACC,APPARM,IOACCDIR+IOHI+IO1,=C'SE1'                         
         MVC   SVKEY2,IOKEY       INITALIZE SVKEY2 TO FIRST KEY FOUND           
         B     PRT70                                                            
*                                                                               
PRT60    GOTO1 AMIOACC,APPARM,IOACCDIR+IOSEQ+IO1,=C'SE1'                        
PRT70    CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IOKEY                                                         
         CLI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         BNE   PRTRX                                                            
         CLI   MPRKSUB,MPRKSUBQ   X'01'                                         
         BNE   PRTRX                                                            
         CLC   MPRKCPY,COMPANY    SAME NATIVE COMPANY                           
         BNE   PRTRX                                                            
         CLC   MPRKSYS,QSYS       SAME SYSTEM                                   
         BNE   PRT60                                                            
*                                                                               
         CLC   QALPH,TWAAGY       DO THEY MATCH                                 
         BNE   PRT75                                                            
         NI    FLAG,X'FF'-ALPHASET  TURN OFF FLAG                               
         OI    FLAG,GETSYS                                                      
         OI    FLAG,RECCHK                                                      
         XC    QALPH,QALPH        CLEARING AGENCY ID                            
         B     PRT50              GET RECORD                                    
*                                                                               
PRT75    TM    FLAG,RECCHK                                                      
         BZ    *+18                                                             
         NI    FLAG,X'FF'-RECCHK                                                
         OC    MPRKALPH,MPRKALPH  ALPHA IN KEY?                                 
         BNZ   PRTRX                                                            
*                                                                               
         TM    FLAG,ALPHASET                                                    
         BZ    *+14                                                             
         CLC   MPRKALPH,QALPH                                                   
         BNE   PRTRX                                                            
         MVC   SVKEY,IOKEY        SAVING KEY OF RECORD FOUND                    
*                                                                               
         OC    QALPH,QALPH        DO WE HAVE ALPHA                              
         BNZ   PRT110             YES, SO DONT PRINT ANY DEFAULTS               
*                                                                               
*MUST PRINT ALL POST TYPES                                                      
*                                                                               
PRT80    L    R1,APRONAME         RESTORING ADDRESS                             
         CLI  0(R1),X'FF'         ARE WE DONE WITH ALL THE POST TYPES           
         BNE  PRT90               NO                                            
         TM   FLAG,GETSYS                                                       
         BO   PRTRX                                                             
         OI   FLAG,GETSEQ                                                       
         TM   BRKFLAG,SYSLVL                                                    
         BZ   PRT110                                                            
         NI   BRKFLAG,X'FF'-SYSLVL  TURN OF BIT                                 
         MVI  REPFHEAD,C'Y'                                                     
         B    PRT110                                                            
*                                                                               
PRT90    CLC  MPRKPRO,0(R1)       DO WE HAVE A RECORD THAT MATCHES              
         BNE  PRT100              NO                                            
         LA   R1,L'PROFNAME(R1)   YES                                           
         ST   R1,APRONAME         UPDATE ADDRESS                                
         OI   FLAG,GETSEQ         READ NEXT RECORD                              
         B    PRT110                                                            
*                                                                               
PRT100   MVC  PROFTYPE,0(R1)      NO SO SET UP TO PRINT DEFUALTS                
         MVC  INREC,1(R1)         FOR DOSPEC                                    
         MVC  REPM2+1(PFNMELEN),2(R1)                                           
         LA   R3,REPM2            R3 IS PARAMETER FOR PUTCOLS                   
         BAS  RE,PUTCOLS          PUTS COLONS IN APPROP. PLACES                 
         CLI  REPLINE,48                                                        
         BL   *+8                                                               
         MVI  REPFHEAD,C'Y'                                                     
*                                                                               
         LA   R1,L'PROFNAME(R1)                                                 
         ST   R1,APRONAME         UPDATE ADDRESS                                
         OI   FLAG,GETDEF         TURN ON DEFAULT FLAG                          
         NI   FLAG,X'FF'-GETSEQ   DON'T READ NEXT RECORD                        
         B    PRT130              PRINT REPORT                                  
*                                                                               
PRT110   BAS   RE,PAGEBRK         CHECK FOR PAGE BREAKS                         
*                                                                               
         MVC   PROFTYPE,MPRKPRO   PROFILE TYPE                                  
         LA    R1,PROFNAME        TABLE OF PROFILE RECORD NAMES                 
PRT120   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PROFTYPE,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,L'PROFNAME(R1)                                                
         B     PRT120                                                           
*                                                                               
         MVC   INREC,1(R1)        FOR DOSPEC                                    
         MVC   REPM2+1(PFNMELEN),2(R1)                                          
         LA    R3,REPM2            R3 IS PARAMETER FOR PUTCOLS                  
         BAS   RE,PUTCOLS          PUTS COLONS IN APPROP. PLACES                
         CLI   REPLINE,48                                                       
         BL    *+8                                                              
         MVI   REPFHEAD,C'Y'      FORCE PAGE BREAK                              
*                                                                               
         USING ACPOSTD,R3                                                       
PRT130   LA    R3,PSTBLK          SET UP USING FOR ACPOSTER                     
*                                                                               
         GOTO1 ADOSPEC            SETS APTABLE & PMAXNUM                        
         BAS   RE,SETBLOCK        BASIC CALL TO ACPOSTER                        
         GOTO1 ACPOSTER,APPARM,(R3)                                             
*                                                                               
*FIX SEQUENCE FOR NEXT SEQUENTIAL READ (DONE BY SETBLOCK)                       
*                                                                               
         MVC   IOKEY,SVKEY                                                      
         GOTO1 AMIOACC,APPARM,IOACCDIR+IORD+IO1,=C'SE1'                         
         CLI   MYIOERR,0          TEST ERROR                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*PROFTYPE PASSED AS PARAMETER                                                   
*                                                                               
         BAS   RE,GETREPTY        GET REPORT TYPE                               
*                                                                               
         LA    R1,PROFTAB                                                       
PRT140   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'               PROFILE TYPE NOT FOUND                        
         CLC   PROFTYPE,0(R1)     COMPARE WITH VALUE IN TABLE                   
         BE    *+12                                                             
         LA    R1,PTABLEN(R1)                                                   
         B     PRT140                                                           
         LA    R1,1(R1)           INCREMENT TABLE                               
         ST    R1,APROFTY         SAVE ADDRESS IN TABLE                         
*                                                                               
*PRINT HEADER INFORMATION                                                       
*                                                                               
         BAS   RE,PRTHDR                                                        
*                                                                               
*PRINT DATA FROM TABLE                                                          
*                                                                               
         BAS   RE,PRTDATA                                                       
*                                                                               
         TM    FLAG,GETSEQ        READ SEQ?                                     
         BZ    PRT80              NO                                            
         MVC   SVKEY2,IOKEY       SAVING OLD KEY                                
         B     PRT60              GET NEXT RECORD                               
*                                                                               
PRTRX    CLC   QALPH,TWAAGY       DO THEY MATCH                                 
         BNE   EXIT               NO, SO EXIT                                   
         NI    FLAG,X'FF'-ALPHASET  TURN OFF FLAG                               
         OI    FLAG,GETSYS                                                      
         OI    FLAG,RECCHK                                                      
         XC    QALPH,QALPH        CLEARING AGENCY ID                            
         B     PRT50              GET RECORD                                    
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* SETBLOCK - SETS UP BLOCK TO ACPOSTER                                *         
***********************************************************************         
         SPACE 1                                                                
SETBLOCK NTR1                                                                   
         XC    IOKEY,IOKEY        READ DUMMY ACC REC TO SWITCH                  
         MVC   IOKEY(1),COMPANY   TO NATIVE ACC SSYTEM BEFORE CALLING           
         GOTO1 AMIOACC,APPARM,IOHI+IOACCFIL+IO3,=C'SE1'                         
         LA    RE,MAXPNUM         CLEAR POSTING TABLE                           
         L     RF,APSTTBL                                                       
SETBLK10 XC    0(ACPRTNL,RF),0(RF)                                              
         LA    RF,ACPRTNL(RF)                                                   
         BCT   RE,SETBLK10                                                      
*                                                                               
         LA    RE,PSTBLK          CLEAR BLOCK                                   
         LA    RF,ACPOSTL                                                       
         XCEFL                                                                  
         USING MPRRECD,R2                                                       
         LA    R2,SVKEY           TAKING INFO FROM KEY OF RECORD                
         USING ACPOSTD,R3                                                       
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         MVC   ACPACOM,ACOM       A(COMFACS)                                    
         MVC   ACPSW,VSWITCH      A(SWITCH)                                     
         MVC   ACPPOST,APSTTBL    A(POSTINGS RETURNED)                          
         MVC   ACPCMPC,COMPANY    NATIVE COMPANY CODE                           
         MVC   ACPCMPC2,COMPANY2  OTHER COMPANY CODE                            
         MVC   ACPSPROD,SVPROD    PRODUCTION UNIT/LEDGER CODE                   
         MVC   ACPCOST,SVCOST     C'Y' - USE COST,BILL,REV                      
         MVI   ACPTYPE,0          POST RECORDS                                  
         MVC   ACPMI,SVMI         C'Y' IF MI RECORDS IN USE                     
         MVC   ACPALPH,MPRKALPH   AGENCY ALPHA FOR SPLIT MEDIA FILES            
         MVC   ACPSYS,QSYS        SYSTEM                                        
         MVC   ACPSE1,SVSE1       NATIVE SE NUMBER                              
         MVC   ACPSE2,SVSE2       OTHER ACC FILE SE NUMBER                      
         MVC   ACPUTL,SCAUTL      PASS UTL ADDRESS                              
*                                                                               
         TM    FLAG,GETDEF        IS DEFAULT FLAG ON                            
         BZ    SETBLK20           NO                                            
         NI    FLAG,X'FF'-GETDEF  YES,TURNING OFF FLAG                          
         B     SETBLK50                                                         
*                                                                               
SETBLK20 MVC   ACPMED,MPRKMED                                                   
         OC    MPRKCLI,MPRKCLI    CLIENT PRESENT                                
         BNZ   SETBLK30                                                         
         MVC   ACPOFC,MPRKOFC     OFFICE                                        
         MVC   ACPOFG,SVOFFG                                                    
*                                                                               
         CLI   MPRKOFC+1,C' '     IS IT OFFICE CODE                             
         BE    SETBLK40           YES                                           
         MVI   ACPOFC,0           OFFICE                                        
         MVC   ACPOFG,MPRKOFC     OFFICE GROUP                                  
         B     SETBLK40                                                         
*                                                                               
SETBLK30 MVC   ACPOFC,SVCOFF      OFFICE                                        
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
                                                                                
SETBLK40 MVC   ACPCLT,MPRKCLI     CLIENT                                        
         MVC   ACPPRD,MPRKPRD     PRODUCT                                       
                                                                                
SETBLK50 MVC   ACPTYPE2,RECTYPE   REG,AOR,RET,PRD (REPLACES A(BILL))            
         MVI   ACPIND,ACPEXP      ASK FOR AMT & MEMO EXPRESSIONS                
*                                                                               
         CLI   RECTYPE,MPRKPST    IF READING TPOST MAINT                        
         BNE   SETBLK60                                                         
         BAS   RE,SETPSTO         SET OUTPUT PST INFO AT CLI/PRD LEVEL          
         BAS   RE,SETPSTI         SET INPUT PST INFO                            
*                                                                               
SETBLK60 MVC   ACPGSTO,SVCGST     GST CODE                                      
         CLI   SVPGST,0                                                         
         BE    SETBLKX                                                          
         MVC   ACPGSTO,SVPGST     OVERRIDE W/PRD GST CODE                       
*                                                                               
SETBLKX  B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SETPSTO - ROUTINE TO SET PST VALUES IN ACPOSTER BLOCK               *         
*           R3 IS PASSED IN AS PARAMTER                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACPOSTD,R3                                                       
SETPSTO  NTR1                                                                   
         MVI   BYTE,C'C'          SET INDICATOR PROCESSING CLIENT               
         LA    R1,SVCPST          R1=A(CLIENT PST CODES)                        
*                                                                               
SETPSTO1 LA    R0,L'SVCPST                                                      
         LA    RE,ACPPSTO         RE=A(PST OUTPUT CODES)                        
         USING PRVTABD,RF                                                       
         LA    RF,PRVTAB          RF=A(PROVINCE TABLE)                          
*                                                                               
SETPSTO2 CLI   PRVTOUT,0          IF ENTRY IS ACTIVE                            
         BE    SETPSTO3                                                         
         MVC   0(2,RE),PRVTCODE   SET PROVINCE CODE                             
         MVC   2(1,RE),PRVTOUT    SET EQUATED ELEMENT CODE FOR OUTPUT           
         CLI   BYTE,C'C'          IF CLIENT LEVEL                               
         BNE   *+8                                                              
         MVI   3(RE),C'S'         SET STANDARD AS DEFAULT                       
         CLI   0(R1),0            IF TAX CODE                                   
         BE    *+10                                                             
         MVC   3(1,RE),0(R1)      SET IT                                        
*                                                                               
SETPSTO3 LA    RE,L'ACPPSTO(RE)                                                 
         LA    RF,L'PRVTAB(RF)    BUMP TO NEXT PROVINCE IN TABLE                
         LA    R1,1(R1)           BUMP TO NEXT CLIENT VALUE                     
         BCT   R0,SETPSTO2                                                      
*                                                                               
         CLI   BYTE,C'C'          IF FINISHED WITH CLIENT VALUES                
         BNE   SETPSTOX                                                         
         MVI   BYTE,C'P'          SET TO GET PRODUCT VALUES                     
         LA    R1,SVPPST          R1=A(PRODUCT PST CODES)                       
         B     SETPSTO1                                                         
*                                                                               
SETPSTOX B     EXIT                                                             
         DROP  R3,RF                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SETPSTI - ROUTINE TO SET INPUT PST INFO                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ACPOSTD,R3                                                       
SETPSTI  NTR1                                                                   
         LA    R1,ACPPSTI                                                       
         USING PRVTABD,RF                                                       
         LA    RF,PRVTAB          RF=A(PROVINCE TABLE)                          
*                                                                               
SETPSTI1 CLI   0(RF),X'FF'                                                      
         BE    SETPSTIX                                                         
         CLI   PRVTIN,0           IF ENTRY IS ACTIVE                            
         BE    SETPSTI2                                                         
         MVC   0(2,R1),PRVTCODE   SET PROVINCE CODE                             
         MVC   2(1,R1),PRVTIN     SET EQUATED ELEMENT INPUT ACCOUNT             
SETPSTI2 LA    RF,L'PRVTAB(RF)                                                  
         LA    R1,L'ACPPSTI(R1)                                                 
         B     SETPSTI1                                                         
*                                                                               
SETPSTIX B     EXIT                                                             
         DROP  R3,RF                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* PAGEBRK- FIGURES OUT IF PAGE BREAK IS NECESSARY AND PRINT           *         
*          CORRESPONDING HEADERS                                      *         
***********************************************************************         
         SPACE 1                                                                
PAGEBRK  NTR1                                                                   
         USING REPD,R4                                                          
         L     R4,AREP                                                          
*                                                                               
         USING MPRRECD,R1                                                       
         LA    R1,SVKEY2                                                        
NEW      USING MPRRECD,R2                                                       
         LA    R2,IOKEY                                                         
         LA    R3,REPH3                                                         
*                                                                               
         CLC   MPRKPRD,NEW.MPRKPRD         DID PRODUCT CHANGE?                  
         BE    PBRK30                      NO,SO NEXT TEST                      
         OC    NEW.MPRKALPH,NEW.MPRKALPH   IS THERE ALPHA AGENCY                
         BZ    PBRK10                      NO.                                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(6,R3),=C'ALPHA:'                                               
         MVC   7(L'MPRKALPH,R3),NEW.MPRKALPH                                    
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
*                                                                               
PBRK10   MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(7,R3),=C'SYSTEM:'                                              
         MVC   8(L'MPRKSYS,R3),NEW.MPRKSYS                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKMED,NEW.MPRKMED                                          
         BZ    PBRK100                                                          
         MVC   0(6,R3),=C'MEDIA:'                                               
         MVC   7(L'MPRKMED,R3),NEW.MPRKMED                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKCLI,NEW.MPRKCLI     IS THERE CLIENT                      
         BZ    PBRK20                      NO CLIENT,CHECK OFFICE               
         MVC   0(7,R3),=C'CLIENT:'                                              
         MVC   8(L'MPRKCLI,R3),NEW.MPRKCLI                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKPRD,NEW.MPRKPRD     IS THERE PRODUCT                     
         BZ    PBRKX1                      NO PRODUCT                           
         MVC   0(8,R3),=C'PRODUCT:'                                             
         MVC   9(L'MPRKPRD,R3),NEW.MPRKPRD                                      
         B     PBRKX1                                                           
*                                                                               
PBRK20   OC    NEW.MPRKOFC,NEW.MPRKOFC     OFFICE?                              
         BZ    PBRK110                     CLEAR LAST LINE,& PAGE BREAK         
         MVC   0(7,R3),=C'OFFICE:'                                              
         GOTO1 ADISOFF,APPARM,NEW.MPRKOFC,8(R3)                                 
         B     PBRK110                                                          
*                                                                               
PBRK30   CLC   MPRKCLI,NEW.MPRKCLI         DID CLIENT CHANGE?                   
         BE    PBRK60                                                           
         OC    NEW.MPRKALPH,NEW.MPRKALPH   IS THERE ALPHA AGENCY                
         BZ    PBRK40                      NO.                                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(6,R3),=C'ALPHA:'                                               
         MVC   7(L'MPRKALPH,R3),NEW.MPRKALPH                                    
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
*                                                                               
PBRK40   MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(7,R3),=C'SYSTEM:'                                              
         MVC   8(L'MPRKSYS,R3),NEW.MPRKSYS                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKMED,NEW.MPRKMED                                          
         BZ    PBRK100                                                          
         MVC   0(6,R3),=C'MEDIA:'                                               
         MVC   7(L'MPRKMED,R3),NEW.MPRKMED                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKCLI,NEW.MPRKCLI     IS THERE CLIENT                      
         BZ    PBRK50                      NO,SO WE HAVE OFFICE                 
         MVC   0(7,R3),=C'CLIENT:'                                              
         MVC   8(L'MPRKCLI,R3),NEW.MPRKCLI                                      
         B     PBRK110                                                          
*                                                                               
PBRK50   OC    NEW.MPRKOFC,NEW.MPRKOFC                                          
         BZ    PBRK110                                                          
         MVC   0(7,R3),=C'OFFICE:'                                              
         GOTO1 ADISOFF,APPARM,NEW.MPRKOFC,8(R3)                                 
         B     PBRK110                                                          
*                                                                               
PBRK60   CLC   MPRKOFC,NEW.MPRKOFC         DID OFFICE CHANGE?                   
         BE    PBRK80                                                           
         OC    NEW.MPRKALPH,NEW.MPRKALPH   IS THERE ALPHA AGENCY                
         BZ    PBRK70                      NO.                                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(6,R3),=C'ALPHA:'                                               
         MVC   7(L'MPRKALPH,R3),NEW.MPRKALPH                                    
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
*                                                                               
PBRK70   MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(7,R3),=C'SYSTEM:'                                              
         MVC   8(L'MPRKSYS,R3),NEW.MPRKSYS                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKMED,NEW.MPRKMED                                          
         BZ    PBRK100                                                          
         MVC   0(6,R3),=C'MEDIA:'                                               
         MVC   7(L'MPRKMED,R3),NEW.MPRKMED                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKOFC,NEW.MPRKOFC                                          
         BZ    PBRK110                     CLEAR LAST LINE AND EXIT             
         MVC   0(7,R3),=C'OFFICE:'                                              
         GOTO1 ADISOFF,APPARM,NEW.MPRKOFC,8(R3)                                 
         B     PBRK110                                                          
*                                                                               
PBRK80   CLC   MPRKMED,NEW.MPRKMED         DID MEDIA CHANGE                     
         BE    PBRK95                      NO                                   
         OC    NEW.MPRKALPH,NEW.MPRKALPH   IS THERE ALPHA AGENCY                
         BZ    PBRK90                      NO.                                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(6,R3),=C'ALPHA:'                                               
         MVC   7(L'MPRKALPH,R3),NEW.MPRKALPH                                    
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
*                                                                               
PBRK90   MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(7,R3),=C'SYSTEM:'                                              
         MVC   8(L'MPRKSYS,R3),NEW.MPRKSYS                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
         OC    NEW.MPRKMED,NEW.MPRKMED                                          
         BZ    PBRK100                                                          
         MVC   0(6,R3),=C'MEDIA:'                                               
         MVC   7(L'MPRKMED,R3),NEW.MPRKMED                                      
         B     PBRK100                                                          
*                                                                               
PBRK95   CLC   MPRKALPH,NEW.MPRKALPH       DID ALPHA CHANGE                     
         BE    PBRKX2                      NO                                   
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(6,R3),=C'ALPHA:'                                               
         MVC   7(L'MPRKALPH,R3),NEW.MPRKALPH                                    
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
*                                                                               
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
         MVC   0(7,R3),=C'SYSTEM:'                                              
         MVC   8(L'MPRKSYS,R3),NEW.MPRKSYS                                      
         LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
*CLEAR REMAINING                                                                
*                                                                               
PBRK100  LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
PBRK110  LA    R3,L'REPHS(R3)              MOVE TO NEXT HEADER                  
         MVC   0(L'SPACES2,R3),SPACES2     CLEAR HEADER                         
*                                                                               
PBRKX1   OI    REPHEADI,REPHSPAC           FORCE PAGE BREAK                     
         MVI   REPFHEAD,C'Y'                                                    
PBRKX2   B     EXIT                                                             
         DROP  R1,R4                                                            
         DROP  NEW                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRTDATA- PRINTS DATA TAKEN FROM BLOCK CREATED BY ACPOSTER           *         
***********************************************************************         
         SPACE 1                                                                
PRTDATA  NTR1                                                                   
         USING REPD,R8                                                          
         L     R8,AREP                                                          
                                                                                
         USING DISD,R2                                                          
         LA    R2,REPP1                                                         
*                                                                               
*PRINT DATA                                                                     
*                                                                               
         ZIC   R0,PMAXNUM                                                       
         L     R4,APTABLE                                                       
PRTD10   CLI   REPLINE,54         ARE WE AT END OF PAGE                         
         BNE   PRTD20             NO.                                           
         MVC   REPP1(L'BROW),BROW CLOSE BOX                                     
         GOTO1 VREPORT,REPBLK                                                   
*                                                                               
PRTD20   MVC   APBYTE,0(R4)                                                     
         USING ACPRTND,R3         GETS R3 FROM GETROW                           
         BAS   RE,GETROW                                                        
*                                                                               
         L     RF,APROFTY         GOING INTO TABLE                              
         MVC   DDIS,0(RF)                                                       
         LA    RF,PFLINELN(RF)    MOVING TO NEXT LINE IN TABLE                  
         ST    RF,APROFTY                                                       
*                                                                               
         XC    DDOC,DDOC                                                        
         TM    ACPSTAT,ACPCR      MOVE IN CREDIT OR DEBIT                       
         BNO   *+10                                                             
         MVC   DDOC(2),=C'CR'                                                   
         TM    ACPSTAT,ACPDEB                                                   
         BNO   *+10                                                             
         MVC   DDOC(2),=C'DR'                                                   
*                                                                               
         MVC   DACC,ACPACC        MOVE ACCOUNT TO SCREEN                        
         MVC   DACCLVL,ACPLVL     MOVE ACCOUNT LEVEL TO SCREEN                  
         MVC   DAMT,ACPAMT        MOVE AMOUNT TO SCREEN                         
         MVC   DMEMO,ACPMEMO      MOVE MEMO TO SCREEN                           
         MVC   DLVL,ACPLVL2       MOVE LVL TO SCREEN                            
         MVC   DLSTACT,ACPID      MOVE INITIALS TO SCREEN                       
         MVC   DLSTACT+3(2),=C'  '                                              
         GOTO1 VDATCON,APPARM,(3,ACPCHDT),(8,DLSTACT+5)                         
         LA   R3,REPP1                                                          
         BAS  RE,PUTCOLS                                                        
*                                                                               
         GOTO1 VREPORT,REPBLK                                                   
         LA    R4,APTABLN(R4)     NEXT RECORD ROW TABLE ENTRY                   
         BCT   R0,PRTD10          LOOP                                          
         MVC   REPP1(L'BROW),BROW CLOSE BOX                                     
*                                                                               
         CLI   REPLINE,54                                                       
         BE    PRTD30                                                           
         GOTO1 VREPORT,REPBLK                                                   
PRTD30   GOTO1 VREPORT,REPBLK     PRINT EMPTY LINE                              
*                                                                               
         B     EXIT                                                             
         DROP  R2,R8                                                            
         EJECT                                                                  
***********************************************************************         
* PRTHDR - PRINTS HEADER INFORMATION BEFORE DISPLAYING PROFILE DATA   *         
***********************************************************************         
         SPACE 1                                                                
PRTHDR   NTR1                                                                   
         USING REPD,R4                                                          
         L     R4,AREP                                                          
                                                                                
         MVC   REPM1(L'BROW),BROW                                               
         MVC   REPM4(L'BROW),BROW                                               
*                                                                               
         USING DISD,R2                                                          
         LA    R2,REPM3                                                         
*                                                                               
*PRINT HEADER INFORMATION                                                       
*                                                                               
         MVC   DDIS,=C'    POSTING       '                                      
         MVC   DDOC,=C'D/C'                                                     
         MVC   DACC,=C'ACCOUNT       '                                          
         MVC   DACCLVL,=C'LVL'                                                  
         MVC   DAMT,=C'AMOUNT  '                                                
         MVC   DMEMO,=C'MEMO    '                                               
         MVC   DLVL,=C'LVL'                                                     
         MVC   DLSTACT,=C'LAST ACTIVITY'                                        
*                                                                               
         OI    REPMIDSI,REPMFRCE  FORCE MIDLINE PRINTING NOW                    
                                                                                
         LA    R3,REPM3                                                         
         BAS   RE,PUTCOLS                                                       
*                                                                               
         GOTO1 VREPORT,REPBLK                                                   
                                                                                
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* PUTCOLS - PUTS COLONS IN THE ROWS TO CREATE BOX EFFECT              *         
*           R3 IS INPUT PARAMETER                                     *         
***********************************************************************         
         SPACE 1                                                                
PUTCOLS  NTR1                                                                   
                                                                                
         MVI    0(R3),C':'                                                      
         MVI    18(R3),C':'                                                     
         MVI    22(R3),C':'                                                     
         MVI    37(R3),C':'                                                     
         MVI    41(R3),C':'                                                     
         MVI    50(R3),C':'                                                     
         MVI    59(R3),C':'                                                     
         MVI    63(R3),C':'                                                     
         MVI    77(R3),C':'                                                     
         B      EXIT                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* GETREPTY - GET CORRESPONDING REPORT TYPE                            *         
***********************************************************************         
         SPACE 1                                                                
GETREPTY NTR1                                                                   
*                                                                               
         LA    R2,RPTTAB                                                        
GETR10   CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'               DID NOT FIND ANYTHING SO DIE                  
         CLC   PROFTYPE,0(R2)                                                   
         BE    *+12                                                             
         LA    R2,L'RPTTAB(R2)                                                  
         B     GETR10                                                           
*                                                                               
         LA    R2,1(R2)                                                         
         MVC   PROFTYPE,0(R2)                                                   
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* GETROW - PTS R3 TO CORRECT ROW IN POSTINGS RETURNED                 *         
***********************************************************************         
         SPACE 1                                                                
GETROW   NTR1                                                                   
         ZIC   R1,APBYTE          ROW NUMBER                                    
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL                                                       
         MR    RE,R1                                                            
         L     R3,APSTTBL                                                       
         AR    R3,RF                                                            
         XIT1  REGS=(R3)                                                        
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
SPECPOOL DS    0H                                                               
         SPEC  H1,51,C'POST RECORD REPORT'                                      
         SPEC  H1,95,REPORT                                                     
         SPEC  H1,114,REQUESTOR                                                 
         SPEC  H2,95,RUN                                                        
         SPEC  H2,123,PAGE                                                      
         DC    XL5'00'                                                          
                                                                                
*                                                                               
PROFNAME DS    0CL10                                                            
         DC    AL1(MPRKREG),AL1(RECPOST),CL8'POST:   '                          
         DC    AL1(MPRKAOR),AL1(RECAPOST),CL8'APOST:  '                         
         DC    AL1(MPRKRTL),AL1(RECRPOST),CL8'RPOST:  '                         
         DC    AL1(MPRKPPB),AL1(RECSPOST),CL8'SPOST:  '                         
         DC    AL1(MPRKUC),AL1(RECUCPST),CL8'UCPOST:  '                         
         DC    AL1(MPRKUN),AL1(RECUNPST),CL8'UNPOST:  '                         
         DC    AL1(MPRKUAC),AL1(RECUACPT),CL8'UACPOST:'                         
         DC    AL1(MPRKUAN),AL1(RECUANPT),CL8'UANPOST:'                         
         DC    AL1(MPRKPST),AL1(RECTPOST),CL8'TPOST:  '                         
         DC    AL1(MPRKDIF),AL1(RECDPOST),CL8'DPOST:  '                         
         DC    AL1(MPRKRLMG),AL1(RECBPOST),CL8'BPOST:  '                        
         DC    X'FF'                                                            
                                                                                
                                                                                
*PROFILE TYPE TABLE USED TO DISPLAY THE CORRESPONDING RULES                     
PROFTAB  DS    0H                                                               
         DC    AL1(RPTYP1)         FOR MPRKPRO = 1                              
         DC    CL18'  1 RCVBL'                                                  
         DC    CL18'  2 INCOME'                                                 
         DC    CL18'  3 NET'                                                    
         DC    CL18'  4 CASH DISC'                                              
         DC    CL18'  5 OUTPUT GST'                                             
         DC    CL18'  6 COSTING'                                                
         DC    CL18'  7 BILLING'                                                
         DC    CL18'  8 REVENUE'                                                
         DC    CL18'  9 INT INCOME'                                             
         DC    CL18' 10 INT COSTING'                                            
         DC    CL18' 11 INT BILLING'                                            
         DC    CL18' 12 INT REVENUE'                                            
         DC    4CL18' '            AREN'T USED                                  
PTABLEN  EQU   *-PROFTAB           LENGTH OF EACH ENTRY                         
         DC    AL1(RPTYP2)         FOR MPRKPRO = 5,6,10                         
         DC    CL18'  1 RCVBL'                                                  
         DC    CL18'  2 INCOME'                                                 
         DC    CL18'  3 NET'                                                    
         DC    CL18'  4 CASH DISC'                                              
         DC    CL18'  5 OUTPUT GST'                                             
         DC    CL18'  6 COSTING'                                                
         DC    CL18'  7 BILLING'                                                
         DC    CL18'  8 REVENUE'                                                
         DC    8CL18' '            AREN'T USED                                  
         DC    AL1(RPTYP3)         FOR MPRKPRO = 2,7,8                          
         DC    CL18'  1 CLIENT RCVBL'                                           
         DC    CL18'  2 MEDIA INCOME'                                           
         DC    CL18'  3 NET'                                                    
         DC    CL18'  4 CASH DISC'                                              
         DC    CL18'  5 OUTPUT GST'                                             
         DC    CL18'  6 COSTING'                                                
         DC    CL18'  7 BILLING'                                                
         DC    CL18'  8 REVENUE'                                                
         DC    CL18'  9 AOR INCOME'                                             
         DC    CL18' 10 AOR BILLING'                                            
         DC    CL18' 11 AOR REVENUE'                                            
         DC    CL18' 12 SELLOFF INC'                                            
         DC    CL18' 13 SELLOFF BILL'                                           
         DC    CL18' 14 SELLOFF REV'                                            
         DC    CL18' 15 AOR PAY/RCBL'                                           
         DC    CL18' 16 INPUT GST'                                              
         DC    AL1(RPTYP4)         FOR MPRKPRO = 3                              
         DC    CL18'  1 RCVBL'                                                  
         DC    CL18'  2 COSTING'                                                
         DC    14CL18' '           AREN'T USED                                  
         DC    AL1(RPTYP5)         FOR MPRKPRO = 9                              
         DC    CL18'  1 PQ INPUT PST'                                           
         DC    CL18'  2 PQ OUTPUT PST'                                          
         DC    CL18'  3 NB INPUT PST'                                           
         DC    CL18'  4 NB OUTPUT PST'                                          
         DC    CL18'  5 NS INPUT PST'                                           
         DC    CL18'  6 NS OUTPUT PST'                                          
         DC    CL18'  7 NF INPUT PST'                                           
         DC    CL18'  8 NF OUTPUT PST'                                          
         DC    8CL18' '            AREN'T USED                                  
         DC    AL1(RPTYP6)         FOR MPRKPRO = 4                              
         DC    CL18'  1 CLIENT RCVBL'                                           
         DC    CL18'  2 SUSP RCVBL'                                             
         DC    CL18'  3 REBATE RCVBL'                                           
         DC    CL18'  4 INCOME'                                                 
         DC    CL18'  5 NET'                                                    
         DC    CL18'  6 CASH DISC'                                              
         DC    CL18'  7 OUTPUT GST'                                             
         DC    CL18'  8 COSTING'                                                
         DC    CL18'  9 BILLING'                                                
         DC    CL18' 10 REVENUE'                                                
         DC    6CL18' '           AREN'T USED                                   
         DC    X'FF'                                                            
                                                                                
RPTTAB   DC    0CL2                                                             
         DC    AL1(MPRKREG),AL1(RPTYP1)                                         
         DC    AL1(MPRKAOR),AL1(RPTYP3)                                         
         DC    AL1(MPRKRTL),AL1(RPTYP4)                                         
         DC    AL1(MPRKPPB),AL1(RPTYP6)                                         
         DC    AL1(MPRKUC),AL1(RPTYP2)                                          
         DC    AL1(MPRKUN),AL1(RPTYP2)                                          
         DC    AL1(MPRKUAC),AL1(RPTYP3)                                         
         DC    AL1(MPRKUAN),AL1(RPTYP3)                                         
         DC    AL1(MPRKPST),AL1(RPTYP5)                                         
         DC    AL1(MPRKDIF),AL1(RPTYP2)                                         
         DC    AL1(MPRKRLMG),AL1(RPTYP4)                                        
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
BROW     DC    CL78'+-----------------+---+--------------+---+--------+C        
               --------+---+-------------+'                                     
*                                                                               
                                                                                
         SPACE 2                                                                
       ++INCLUDE ACTRAPRV                                                       
                                                                                
MYPSTTBL DS    (MAXPNUM)XL(ACPRTNL)  TABLE FOR ACPOSTER                         
         SPACE 2                                                                
* CTGENFILE                                                                     
* ACTRAWRK                                                                      
         SPACE                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
*                                                                               
PFNMELEN EQU   8                  LENGTH OF CHARACTER STR. IN PROFNAME          
PFLINELN EQU   18                 LENGTH OF EACH CHAR STRING IN PROFTAB         
                                                                                
*LEVEL EQUATES                                                                  
SYSLVL   EQU   X'01'              SYSTEM LEVEL                                  
MEDLVL   EQU   X'02'              MEDIA LEVEL                                   
OFFLVL   EQU   X'04'              OFFICE LEVEL                                  
CLTLVL   EQU   X'08'              CLIENT LEVEL                                  
PROLVL   EQU   X'10'              PRODUCT LEVEL                                 
ALLLVL   EQU   SYSLVL+MEDLVL+OFFLVL+CLTLVL+PROLVL                               
                                                                                
*REPORT TYPE EQUATES                                                            
RPTYP1   EQU   1                  FOR MPRKPRO = 1                               
RPTYP2   EQU   2                  FOR MPRKPRO = 5,6,10                          
RPTYP3   EQU   3                  FOR MPRKPRO = 2,7,8                           
RPTYP4   EQU   4                  FOR MPRKPRO = 3,B                             
RPTYP5   EQU   5                  FOR MPRKPRO = 9                               
RPTYP6   EQU   6                  FOR MPRKPRO = 4                               
                                                                                
LOCALD   DSECT                                                                  
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
CPY      DS    CL7                                                              
FLAG     DS    XL1                                                              
GETDEF   EQU   X'80'              GET DEFAULT RECORDS                           
GETSEQ   EQU   X'40'              GET SEQUENTIAL RECORDS                        
GETSYS   EQU   X'20'              GET SYSTEM LEVEL ONLY                         
ALPHASET EQU   X'10'              ALPHA AGENCY ENTERED                          
RECCHK   EQU   X'08'              CHECKS RECORD FOR ALPHA CODE                  
BRKFLAG  DS    XL1                PAGE BREAK FLAG                               
PROFTYPE DS    XL1                REPORT TYPE                                   
APROFTY  DS    A                  ADDRESS OF PROFILE TO PRINT IN TABLE          
APRONAME DS    A                  ADDRESS OF PROFILE NAME IN TABLE              
BLOCK    DS    2CL32                                                            
BYTE     DS    CL1                                                              
SVKEY    DS    CL44                                                             
SVKEY2   DS    CL44                                                             
SPACES2  DS    CL132                                                            
                                                                                
         EJECT                                                                  
*                                                                               
* DISD - DSECT TO COVER DISPLAY SCREEN                                          
*                                                                               
DISD     DSECT                                                                  
DDIS     DS    CL18               DISCRIPTION                                   
         DS    CL1                                                              
DDOC     DS    CL3                DEBIT OR CREDIT                               
         DS    CL1                                                              
DACC     DS    CL14               ACCOUNT                                       
         DS    CL1                                                              
DACCLVL  DS    CL3                ACCOUNT LEVEL                                 
         DS    CL1                                                              
DAMT     DS    CL8                AMOUNT                                        
         DS    CL1                                                              
DMEMO    DS    CL8                MEMO                                          
         DS    CL1                                                              
DLVL     DS    CL3                LEVEL                                         
         DS    CL1                                                              
DLSTACT  DS    CL13               LAST ACTIVITY                                 
DISDL    EQU   *-DDIS             LENGTH OF ONE DISPLAY LINE                    
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAF8D                                                       
       ++INCLUDE DDSCANBLKD                                                     
         SPACE 2                                                                
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACTRA09   02/15/19'                                      
         END                                                                    
