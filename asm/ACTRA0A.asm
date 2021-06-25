*          DATA SET ACTRA0A    AT LEVEL 004 AS OF 11/29/18                      
*PHASE T6220AB                                                                  
         TITLE 'TRA0A T6220A  BILLING TRANSFER - REPORT OVERLAY'                
T6220A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T6220A**,R9,RR=RE                                              
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
EXIT     XIT1                                                                   
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
*                                                                               
         GOTO1 AVALWHEN,TRAWHENH  VALIDATE WHEN                                 
         BE    VRQ10              OK                                            
*                                                                               
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
         BZ    VRQ60              YES                                           
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
         OI    FLAG,GETSEQ        TURN ON SEQUENTIAL FLAG                       
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
         LA    R1,PROFNAME        CHECK WITH TABLE IF NOT THERE                 
PRT80    CLI   0(R1),X'FF'        THEN INCORRECT RECORD, GET NEXT ONE           
         BE    PRT60              GET NEXT RECORD                               
         CLC   MPRKPRO,0(R1)                                                    
         BE    *+12                                                             
         LA   R1,L'PROFNAME(R1)   INCREMENT                                     
         B    PRT80               LOOP                                          
*                                                                               
         CLC   QALPH,TWAAGY       DO THEY MATCH                                 
         BNE   PRT85                                                            
         NI    FLAG,X'FF'-ALPHASET  TURN OFF FLAG                               
         OI    FLAG,GETSYS                                                      
         OI    FLAG,RECCHK                                                      
         XC    QALPH,QALPH        CLEARING AGENCY ID                            
         B     PRT50              GET RECORD                                    
*                                                                               
PRT85    TM    FLAG,RECCHK                                                      
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
         BNZ   PRT120             YES, SO DONT PRINT ANY DEFAULTS               
*                                                                               
*MUST PRINT ALL PROF TYPES                                                      
*                                                                               
PRT90    L    R1,APRONAME         RESTORING ADDRESS                             
         CLI  0(R1),X'FF'         ARE WE DONE WITH ALL THE PROF TYPES           
         BNE  PRT100              NO                                            
         TM   FLAG,GETSYS                                                       
         BO   PRTRX                                                             
         OI   FLAG,GETSEQ                                                       
         TM   BRKFLAG,SYSLVL                                                    
         BZ   PRT120                                                            
         NI   BRKFLAG,X'FF'-SYSLVL  TURN OF BIT                                 
         MVI  REPFHEAD,C'Y'                                                     
         B    PRT120                                                            
*                                                                               
PRT100   CLC  MPRKPRO,0(R1)       DO WE HAVE A RECORD THAT MATCHES              
         BNE  PRT110              NO                                            
         LA   R1,L'PROFNAME(R1)   YES                                           
         ST   R1,APRONAME         UPDATE ADDRESS                                
         OI   FLAG,GETSEQ         READ NEXT RECORD                              
         B    PRT120                                                            
*                                                                               
PRT110   MVC  PROFTYPE,0(R1)      NO SO SET UP TO PRINT DEFUALTS                
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
         B    PRT140              PRINT REPORT                                  
*                                                                               
PRT120   BAS   RE,PAGEBRK         CHECK FOR PAGE BREAKS                         
*                                                                               
         MVC   PROFTYPE,MPRKPRO   PROFILE TYPE                                  
         LA    R1,PROFNAME        TABLE OF PROFILE RECORD NAMES                 
PRT130   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PROFTYPE,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,L'PROFNAME(R1)                                                
         B     PRT130                                                           
*                                                                               
         MVC   INREC,1(R1)        FOR DOSPEC                                    
         MVC   REPM2+1(PFNMELEN),2(R1)                                          
         LA    R3,REPM2            R3 IS PARAMETER FOR PUTCOLS                  
         BAS   RE,PUTCOLS          PUTS COLONS IN APPROP. PLACES                
         CLI   REPLINE,48                                                       
         BL    *+8                                                              
         MVI   REPFHEAD,C'Y'      FORCE PAGE BREAK                              
*                                                                               
PRT140   GOTO1 ADOSPEC            SETS APTABLE & PMAXNUM                        
                                                                                
         BAS   RE,CKBILS          GET BILL SOURCE FROM POST/MAINT RECS          
         BAS   RE,SETDFLT         SET DEFAULT VALUES                            
                                                                                
         USING ACPOSTD,R3                                                       
         LA    R3,PSTBLK          SET UP USING FOR ACPOSTER                     
                                                                                
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
PRT150   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'               PROFILE TYPE NOT FOUND                        
         CLC   PROFTYPE,0(R1)     COMPARE WITH VALUE IN TABLE                   
         BE    *+12                                                             
         LA    R1,PTABLEN(R1)                                                   
         B     PRT150                                                           
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
         BZ    PRT90                                                            
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
* CKBILS - READ POST MAINT FOR LATEST SI ACCOUNT                      *         
***********************************************************************         
         SPACE 1                                                                
CKBILS   NTR1                                                                   
                                                                                
         USING ACPOSTD,R3                                                       
         LA    R3,PSTBLK                                                        
         BAS   RE,CLRPST          CLEAR POST TABLE                              
         BAS   RE,SETBLOCK                                                      
         MVI   ACPTYPE,0          READ PM RECORDS                               
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS(ACPOSTER DID              
*                                 READS TO MESS THIS FLAG UP)                   
         DROP  R3                                                               
         MVC   BILLSCR,SPACES2                                                  
         MVC   BILLSLVL,SPACES2                                                 
         MVC   BILLSACT,SPACES2                                                 
         CLI   SVMI,C'Y'                                                        
         BNE   CKBILS30                                                         
*                                 ON MI RECORDS                                 
         MVI   APBYTE,MBTTINC                                                   
         BAS   RE,GETACC          IF INCOME ACCOUNT EXISTS                      
         BNE   CKBILS10           NO - SEE ABOUT AOR INCOME ACCOUNT             
         CLC   =C'MI',ACCLVL      YES- SEE IF OVERRIDEN                         
         BE    CKBILS20           NO - USE MI DESCRIPTION                       
         BAS   RE,GETNAME         YES -GET SI ACCOUNT NAME                      
         B     CKBILS50                                                         
*                                                                               
CKBILS10 MVI   APBYTE,MBTTARI     SEE IF AOR INCOME ACCOUNT EXISTS              
         BAS   RE,GETACC                                                        
         BNE   CKBILS20           NO - USE MI DESCRIPTION                       
         CLC   =C'AOR',ACCLVL     YES- SEE IF OVERRIDEN                         
         BE    CKBILS20           NO - USE MI DESCRIPTION                       
         BAS   RE,GETNAME         YES -GET SI AOR ACCOUNT NAME                  
         B     CKBILS50                                                         
*                                                                               
CKBILS20 BAS   RE,RDMI            GET MI DESCRIPTION                            
         MVC   ACCLVL,=C'MI '                                                   
         B     CKBILS50                                                         
*                                                                               
CKBILS30 MVI   APBYTE,MBTTINC     IF INCOME ACCOUNT EXISTS                      
         BAS   RE,GETACC                                                        
         BE    CKBILS40           YES - USE IT                                  
         MVI   APBYTE,MBTTARI     IF INCOME ACCOUNT EXISTS                      
         BAS   RE,GETACC          OTHERWISE USE AOR INCOME ACCOUNT              
         BNE   CKBILSX            NO AOR INC EITHER                             
*                                                                               
CKBILS40 BAS   RE,GETNAME                                                       
CKBILS50 MVC   BILLSLVL,ACCLVL                                                  
*                                                                               
CKBILSX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* GETACC - GIVEN ROW NUMBER (APBYTE) SETS ACCOUNT,ACCLVL,& BILSACT    *         
***********************************************************************         
         SPACE 1                                                                
GETACC   NTR1                                                                   
         XC    ACCOUNT,ACCOUNT                                                  
         XC    ACCLVL,ACCLVL                                                    
         LA    R2,MAXPNUM                                                       
*                                                                               
         USING ACPRTND,R3                                                       
         L     R3,APSTTBL                                                       
GETACC10 CLC   APBYTE,0(R3)                                                     
         BNE   GETACC20                                                         
         CLC   ACPACC,SPACES2     ANY ACCOUNT?                                  
         BNH   GETACCN                                                          
         MVC   ACCOUNT,ACPACC     YES SAVE IT AND                               
         MVC   ACCLVL,ACPLVL      IT'S LVL                                      
         CLC   =C'MI',ACCLVL      IF DEFAULT - NO LAST ACTIVITY                 
         BE    GETACCY                                                          
         CLC   =C'DF',ACCLVL                                                    
         BE    GETACCY                                                          
*                                                                               
GETACC20 LA    R3,ACPRTNL(R3)                                                   
         BCT   R2,GETACC10                                                      
GETACCN  LTR   RB,RB                                                            
         B     GETACCX                                                          
GETACCY  CR    RB,RB                                                            
GETACCX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* GETNAME - GIVEN ACCOUNT - READS RECORD FOR ACCOUNT NAME(BILSCR)     *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVC   APWORK,SPACES2                                                   
         LA    R2,IOKEY                                                         
         USING ACTKEY,R2                                                        
         MVC   ACTKEY,SPACES2                                                   
         MVC   ACTKCPY,COMPANY2   OTHER COMPANY CODE (IF APPL)                  
         OC    ACCOUNT,SPACES2                                                  
         MVC   ACTKUNT(14),ACCOUNT                                              
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   GETNMX                                                           
*                                                                               
         SR    R0,R0                                                            
         L     R2,AIOAREA3                                                      
         LA    R1,ACCORFST                                                      
         USING NAMELD,R2                                                        
         AR    R2,R1                                                            
GETNM10  CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),NAMELQ       X'20'                                         
         BE    GETNM20                                                          
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     GETNM10                                                          
*                                                                               
GETNM20  SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),NAMEREC                                                
         MVC   BILLSCR,APWORK     GET FIRST 12                                  
GETNMX   B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* RDMI - READS MI RECORD FOR BILLSCR                                  *         
***********************************************************************         
         SPACE 1                                                                
RDMI     NTR1                                                                   
         MVC   BILLSCR,SPACES2                                                  
         USING MINRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   MINKEY,SPACES2                                                   
         MVI   MINKTYP,MINKTYPQ    X'08'                                        
         MVC   MINKCPY,COMPANY2    OTHER COMPANY CODE(IF APPL)                  
         MVC   MINKMED(1),QSYS     SYSTEM                                       
*                                                                               
         USING MPRRECD,R3                                                       
         LA    R3,SVKEY            GETTING MEDIA FROM KEY                       
         MVC   MINKMED+1(1),MPRKMED                                             
         DROP  R3                                                               
*                                                                               
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   RDMIX                                                            
         DROP  R2                                                               
         L     R2,AIOAREA3                                                      
         USING MDIELD,R2                                                        
         AH    R2,DATADISP                                                      
         SR    R0,R0                                                            
RDMI10   CLI   0(R2),0            END OF RECORD?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),MDIELQ       X'19'                                         
         BE    RDMI20                                                           
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     RDMI10                                                           
*                                                                               
RDMI20   MVC   BILLSCR,MDIDESC    GET MI DESCRIPTION                            
RDMIX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* SETBLOCK - SETS UP BLOCK TO ACPOSTER                                *         
***********************************************************************         
         SPACE 1                                                                
SETBLOCK NTR1                                                                   
*                                                                               
         LA    RE,PSTBLK          CLEAR BLOCK                                   
         LA    RF,ACPOSTL                                                       
         XCEFL                                                                  
*                                                                               
         USING MPRRECD,R2                                                       
         LA    R2,SVKEY           TAKING INFO FROM KEY OF RECORD                
         USING ACPOSTD,R3                                                       
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         MVC   ACPACOM,ACOM       A(COMFACS)                                    
         MVC   ACPSW,VSWITCH      A(SWITCH)                                     
         MVC   ACPPOST,APSTTBL    A(POSTINGS RETURNED)                          
         MVC   ACPCMPC,COMPANY    NATIVE COMPANY CODE                           
         MVC   ACPCMPC2,COMPANY2  OTHER COMPANY CODE                            
         MVC   ACPMI,SVMI         C'Y' IF MI RECORDS IN USE                     
         MVC   ACPALPH,MPRKALPH   AGENCY ALPHA FOR SPLIT MEDIA FILES            
         MVC   ACPSYS,QSYS        SYSTEM                                        
         MVC   ACPSE1,SVSE1       NATIVE SE NUMBER                              
         MVC   ACPSE2,SVSE2       OTHER ACC FILE SE NUMBER                      
         MVC   ACPUTL,SCAUTL      PASS UTL ADDRESS                              
*                                                                               
         TM    FLAG,GETDEF        IS DEFAULT FLAG ON                            
         BZ    SETBLK10           NO                                            
         NI    FLAG,X'FF'-GETDEF  RESETTING FLAG                                
         B     SETBLK40                                                         
*                                                                               
SETBLK10 MVC   ACPMED,MPRKMED                                                   
         OC    MPRKCLI,MPRKCLI    CLIENT PRESENT                                
         BNZ   SETBLK20                                                         
         MVC   ACPOFC,MPRKOFC     OFFICE                                        
         MVC   ACPOFG,SVOFFG                                                    
*                                                                               
         CLI   MPRKOFC+1,C' '     IS IT OFFICE CODE                             
         BE    SETBLK30           YES                                           
         MVI   ACPOFC,0           OFFICE                                        
         MVC   ACPOFG,MPRKOFC     OFFICE GROUP                                  
         B     SETBLK30                                                         
*                                                                               
SETBLK20 MVC   ACPOFC,SVCOFF      OFFICE(CLIENT ENTERED)                        
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
*                                                                               
SETBLK30 MVC   ACPCLT,MPRKCLI     CLIENT                                        
         MVC   ACPPRD,MPRKPRD     PRODUCT                                       
*                                                                               
SETBLK40 MVI   ACPTYPE,1          PROF RECORDS                                  
         MVC   ACPTYPE2,RECTYPE   REG,AOR,RET,PRD (REPLACES A(BILL))            
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SETDFLT - SET DEFAULTS FOR REPORT                                   *         
***********************************************************************         
         SPACE 1                                                                
SETDFLT  NTR1                                                                   
         BAS   RE,CLRPST          CLEAR POST TABLE                              
         L     R4,APTABLE         CURRENT TABLE OF ROW VALUES                   
         USING ACPRTND,R3                                                       
         LA    R1,DFLTBL          PT TO DEFAULT VALUES                          
         CLI   RECTYPE,MPRKREG    PT TO REGULAR PROF DEFAULT VALUES             
         BE    SETD10                                                           
         LA    R1,DFLTBLR         PT TO RETAIL DEFAULT VALUES                   
         CLI   RECTYPE,MPRKRTL    IF RETAIL PROF                                
         BE    SETD10                                                           
         LA    R1,DFLTBLP         PT TO PRINT DEFAULT VALUES                    
*                                                                               
SETD10   CLI   0(R1),X'FF'        END OF BASIC DEFAULTS                         
         BE    SETDX                                                            
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         CLI   0(R4),MTPFBILS     IF WANT BILL SOURCE                           
         BNE   SETD20                                                           
         XC    ACPFVAL,ACPFVAL                                                  
         MVC   ACPFVAL(L'BILLSCR),BILLSCR                                       
         MVC   ACPFLVL,BILLSLVL                                                 
         MVC   ACPFACT,BILLSACT                                                 
         B     SETD50                                                           
*                                                                               
SETD20   CLI   0(R4),MTPFDACC     DDS ACC FILE                                  
         BE    *+12                                                             
         CLI   0(R4),MTPFDMED     DDS MEDIA FILE                                
         BNE   SETD30                                                           
         MVC   ACPFVAL,SPACES2                                                  
         MVC   ACPFVAL(2),TWAAGY  MOVE IN AGENCY ALPHA                          
         MVC   ACPFLVL,=C'DF '                                                  
         B     SETD50                                                           
*                                                                               
SETD30   CLI   0(R4),MTPFIPCT     IF % FOR IOR                                  
         BE    *+12                                                             
         CLI   0(R4),MTPFIPT2                                                   
         BNE   SETD40                                                           
         XC    ACPFVAL,ACPFVAL                                                  
         MVC   ACPFLVL,=C'DF '                                                  
         B     SETD50                                                           
*                                                                               
SETD40   MVC   ACPFVAL,0(R1)      MOVE IN DEFAULT                               
         CLC   ACPFVAL,SPACES2                                                  
         BNH   SETD50                                                           
         MVC   ACPFLVL,=C'DF '                                                  
*                                                                               
SETD50   LA    R4,APTABLN(R4)                                                   
         LA    R1,L'DFLTBL(R1)    NEXT DEFAULT                                  
         B     SETD10                                                           
*                                                                               
SETDX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CLRPST - CLEAR POST TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
CLRPST   NTR1                                                                   
         LA    RE,MAXPNUM                                                       
         L     RF,APSTTBL                                                       
CLRPST10 XC    0(ACPRTNL,RF),0(RF)                                              
         LA    RF,ACPRTNL(RF)                                                   
         BCT   RE,CLRPST10                                                      
         B     EXIT                                                             
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
         CLI   MPRKOFC,C'$'        IS THIS AN OFFICE LIST?                      
         BNE   *+14                NO SO CALL OFFICER TO CONVERT                
         MVC   8(L'MPRKOFC,R3),NEW.MPRKOFC  SINCE OFFICE CAN BE EITHER          
         B     PBRK110                      ONE OR TWO BYTES                    
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
         CLI   MPRKOFC,C'$'        IS THIS AN OFFICE LIST?                      
         BNE   *+14                NO SO CALL OFFICER TO CONVERT                
         MVC   8(L'MPRKOFC,R3),NEW.MPRKOFC  SINCE OFFICE CAN BE EITHER          
         B     PBRK110                      ONE OR TWO BYTES                    
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
         CLI   MPRKOFC,C'$'        IS THIS AN OFFICE LIST?                      
         BNE   *+14                NO SO CALL OFFICER TO CONVERT                
         MVC   8(L'MPRKOFC,R3),NEW.MPRKOFC  SINCE OFFICE CAN BE EITHER          
         B     PBRK110                      ONE OR TWO BYTES                    
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
         USING ACPRTND,R3         GETS R3 FROM GETROW                           
PRTD20   BAS   RE,GETROW                                                        
*                                                                               
         L     RF,APROFTY         GOING INTO TABLE                              
         MVC   DDIS,0(RF)                                                       
         LA    RF,PFLINELN(RF)    MOVING TO NEXT LINE IN TABLE                  
         ST    RF,APROFTY                                                       
*                                                                               
         XC    DVAL,DVAL                                                        
         CLI   0(R4),MTPFIPCT     IF IOR PERCENTAGE                             
         BE    *+12                                                             
         CLI   0(R4),MTPFIPT2     OR AOR PERCENTAGE                             
         BNE   PRTD30                                                           
         ST    R0,TEMP            SAVE COUNT IN TEMP                            
         EDIT  (4,ACPFVAL),(7,DVAL),3,ALIGN=LEFT,ZERO=NOBLANK,         X        
               DUB=APDUB,WRK=APWORK                                             
         L     R0,TEMP            RESTORE R0 FROM TEMP                          
         B     PRTD60                                                           
*                                                                               
PRTD30   CLI   0(R4),MTPFDMED                                                   
         BE    *+12                                                             
         CLI   0(R4),MTPFDACC                                                   
         BNE   PRTD50                                                           
*                                                                               
         OC    ACPFVAL,SPACES2                                                  
         LA    R1,L'ACPFVAL                                                     
         LA    RE,ACPFVAL                                                       
         LA    RF,DVAL                                                          
PRTD40   MVC   0(2,RF),0(RE)                                                    
         LA    RE,2(RE)                                                         
         LA    RF,2(RF)                                                         
         CLC   0(2,RE),SPACES2                                                  
         BE    PRTD60                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         BCTR  R1,0               SUBTRACT 2                                    
         BCT   R1,PRTD40                                                        
         B     PRTD60                                                           
*                                                                               
PRTD50   MVC   DVAL,ACPFVAL       MOVE VALUE TO SCREEN                          
PRTD60   MVC   DLVL,ACPFLVL       MOVE LVL TO SCREEN                            
         MVC   DLSTACT,ACPFACT    MOVE LAST ACTIVITY TO SCREEN                  
         DROP  R3                                                               
*                                                                               
         LA   R3,REPP1                                                          
         BAS  RE,PUTCOLS                                                        
*                                                                               
         GOTO1 VREPORT,REPBLK                                                   
         LA    R4,APTABLN(R4)     NEXT RECORD ROW TABLE ENTRY                   
         BCT   R0,PRTD10          LOOP                                          
                                                                                
         MVC   REPP1(L'BROW),BROW CLOSE BOX                                     
*                                                                               
         CLI   REPLINE,54                                                       
         BE    PRTD70                                                           
         GOTO1 VREPORT,REPBLK                                                   
PRTD70   GOTO1 VREPORT,REPBLK     PRINT EMPTY LINE                              
                                                                                
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
         MVC   DDIS,=CL30'    PROFILE'                                          
         MVC   DVAL,=CL20'VALUE'                                                
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
         MVI    30(R3),C':'                                                     
         MVI    51(R3),C':'                                                     
         MVI    55(R3),C':'                                                     
         MVI    69(R3),C':'                                                     
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
         ZIC   R3,0(R4)           ROW NUMBER                                    
         BCTR  R3,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL         LENGTH OF TABLE                               
         MR    RE,R3              GET TOTAL DISP FROM START                     
         L     R3,APSTTBL                                                       
         AR    R3,RF              R3=CORRECT ROW                                
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*                                                                               
                                                                                
SPECPOOL DS    0H                                                               
         SPEC  H1,51,C'PROFILE RECORD REPORT'                                   
         SPEC  H1,95,REPORT                                                     
         SPEC  H1,114,REQUESTOR                                                 
         SPEC  H2,95,RUN                                                        
         SPEC  H2,123,PAGE                                                      
         DC    XL5'00'                                                          
                                                                                
* DEFAULT FOR PROF MAINT                                                        
*                                                                               
DFLTBL   DS    0CL20              LENGTH OF EACH VALUE                          
         DC    CL20' '            DDS ACC FILE                                  
         DC    CL20' '            DDS MEDIA FILE                                
         DC    C'N',CL19' '       DDS MAKE POSTINGS                             
         DC    CL20' '                                                          
         DC    C'N',CL19' '       CHECK FOR SJ PRODUCT                          
         DC    C'N',CL19' '       CHECK FOR PRODUCT LVL 1C                      
         DC    CL20' '            SPECIAL DELIMITER                             
         DC    CL2'00',CL18' '    MOS BACK-UP DATE                              
         DC    CL2'00',CL18' '    MOS FORWARD DATE                              
         DC    C'N',CL19' '       MAKE POSTINGS                                 
         DC    C'N',CL19' '       CC= IN USE/OR LEVEL                           
         DC    CL20' '            PRODUCT GROUP SCHEME                          
         DC    CL3'.00',CL17' '   PERCENTAGE OF GROSS FOR INTERNAL              
         DC    CL3'.00',CL17' '   AOR PERCENT. OF GROSS FOR INTERNAL            
         DC    CL20' '            OFFICE CODE OVERRIDE FOR INTERNAL             
         DC    X'FF'                                                            
                                                                                
*                                                                               
* DEFAULT FOR RPROF MAINT                                                       
*                                                                               
DFLTBLR  DS    0CL20              LENGTH OF EACH VALUE                          
         DC    CL20' '            UNIT 3 LEDGER                                 
         DC    C'N',CL19' '       RCVBL ACCOUNT IS ON UNIT 3 PART               
         DC    C'N',CL19' '       RCVBL ACCOUNT IS ON UNIT 3 PART               
         DC    CL20' '            OVERRIDE CLT                                  
         DC    CL20' '            OVERRIDE PRD                                  
         DC    CL20' '            OVERRIDE MARKET GROUP (CORP SUMMARY)          
         DC    X'FF'                                                            
*                                                                               
* DEFAULT FOR SPROF MAINT                                                       
*                                                                               
DFLTBLP  DS    0CL20              LENGTH OF EACH VALUE                          
         DC    C'N',CL19' '       POST RCVBL TO PROD INV(SJ)                    
         DC    CL20' '            WORK CODE - PROD INV (SJ)                     
         DC    CL20' '            JOB CODE FOR SEPERATE CD(SJ)                  
         DC    C'OSUS',CL16' '    SUSPENSE SUFFIX                               
         DC    C'OREB',CL16' '    REBATE SUFFIX                                 
         DC    C'N',CL19' '       POST TO CLT LVL RCVBL                         
         DC    C'Y',CL19' '       DON'T POST CD MEMO (SJ)                       
         DC    X'FF'                                                            
                                                                                
PROFNAME DS    0CL8                                                             
         DC    AL1(MPRKREG),AL1(RECPROF),CL6'PROF:'                             
         DC    AL1(MPRKRTL),AL1(RECRPROF),CL6'RPROF:'                           
         DC    AL1(MPRKPPB),AL1(RECSPROF),CL6'SPROF:'                           
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*PROFILE TYPE TABLE USED TO DISPLAY THE CORRESPONDING RULES                     
*                                                                               
PROFTAB  DS    0H                  LENGTH OF EACH ENTRY                         
         DC    AL1(PRTYP1)         FOR MPRKPRO = 1                              
         DC    CL30'  1 *DDS* ACC FILE'                                         
         DC    CL30'  2 *DDS* MEDIA FILE'                                       
         DC    CL30'  3 *DDS* MAKE POSTINGS?'                                   
         DC    CL30'  4 BILL SOURCE/MEDIA NAME'                                 
         DC    CL30'  5 CHECK FOR SJ PRODUCT'                                   
         DC    CL30'  6 CHECK FOR PROD LVL 1C'                                  
         DC    CL30'  7 SPCL NUMBER DELIMETER'                                  
         DC    CL30'  8 MOA BACK-UP DATE'                                       
         DC    CL30'  9 MOA FORWARD DATE'                                       
         DC    CL30' 10 MAKE POSTING?'                                          
         DC    CL30' 11 INC CC= RPLC CLT LVL LC'                                
         DC    CL30' 12 PRNT DIV/BCAST PRD SCH'                                 
         DC    CL30' 13 PCT OF GROSS (INT)'                                     
         DC    CL30' 14 AOR PCT OF GROSS (INT)'                                 
         DC    CL30' 15 OFFICE OVERRIDE (INT)'                                  
PTABLEN  EQU   *-PROFTAB           LENGTH OF EACH ENTRY                         
         DC    AL1(PRTYP2)         FOR MPRKPRO = 3                              
         DC    CL30'  1 UNIT 3 LEDGER'                                          
         DC    CL30'  2 RCVBL ACC ON UNIT 3 PART'                               
         DC    CL30'  3 COST ACC ON UNIT 3 PART'                                
         DC    CL30'  4 CORP BILLING CLT OVERRIDE'                              
         DC    CL30'  5 CORP BILLING PRD OVERRIDE'                              
         DC    CL30'  6 CRP SUMM MKTGRP OVERRIDE'                               
         DC    9CL30' '            AREN'T USED                                  
         DC    AL1(PRTYP3)         FOR MPRKPRO = 4                              
         DC    CL30'  1 POST RCVBL TO PROD (SJ)'                                
         DC    CL30'  2 WORK CODE FOR PROD INV'                                 
         DC    CL30'  3 JOB CODE FOR SEP CD(SJ)'                                
         DC    CL30'  4 SUSPENSE SUFFIX'                                        
         DC    CL30'  5 REBATE SUFFIX'                                          
         DC    CL30'  6 POST TO CLT LVL RCVBL'                                  
         DC    CL30'  7 POST CD MEMO (SJ)'                                      
         DC    8CL30' '            AREN'T USED                                  
         DC    X'FF'                                                            
*                                                                               
RPTTAB   DC    0CL2                                                             
         DC    AL1(MPRKREG),AL1(PRTYP1)                                         
         DC    AL1(MPRKRTL),AL1(PRTYP2)                                         
         DC    AL1(MPRKPPB),AL1(PRTYP3)                                         
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
BROW     DC    CL70'+-----------------------------+--------------------C        
               +---+-------------+'                                             
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
PFNMELEN EQU   6                  LENGTH OF CHARACTER STR. IN PROFNAME          
PFLINELN EQU   30                 LENGTH OF EACH CHAR STRING IN PROFTAB         
                                                                                
*LEVEL EQUATES                                                                  
SYSLVL   EQU   X'01'               SYSTEM LEVEL                                 
MEDLVL   EQU   X'02'               MEDIA LEVEL                                  
OFFLVL   EQU   X'04'               OFFICE LEVEL                                 
CLTLVL   EQU   X'08'               CLIENT LEVEL                                 
PROLVL   EQU   X'10'               PRODUCT LEVEL                                
ALLLVL   EQU   SYSLVL+MEDLVL+OFFLVL+CLTLVL+PROLVL                               
                                                                                
*REPORT TYPE EQUATES                                                            
PRTYP1   EQU   1                   FOR MPRKPRO = 1                              
PRTYP2   EQU   2                   FOR MPRKPRO = 3                              
PRTYP3   EQU   3                   FOR MPRKPRO = 4                              
                                                                                
LOCALD   DSECT                                                                  
TEMP     DS    F                                                                
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
CPY      DS    CL7                                                              
FLAG     DS    XL1                                                              
GETDEF   EQU   X'80'              GET DEFAULT RECORDS                           
GETSEQ   EQU   X'40'              GET SEQUENTIAL RECORDS                        
GETSYS   EQU   X'20'              GET SYSTEM LEVEL ONLY                         
ALPHASET EQU   X'10'              ALPHA AGENCY ENTERED                          
RECCHK   EQU   X'08'              CHECKS RECORD FOR ALPHA                       
BRKFLAG  DS    XL1                PAGE BREAK FLAG                               
PROFTYPE DS    XL1                REPORT TYPE                                   
APROFTY  DS    A                  ADDRESS OF PROFILE TO PRINT IN TABLE          
APRONAME DS    A                  ADDRESS OF PROFILE NAME IN TABLE              
BILLSCR  DS    CL12               BILL SOURCE/MEDIA NAME                        
BILLSLVL DS    CL(L'ACPLVL)       LVL FROM WHERE BILLSCR CAME                   
BILLSACT DS    CL(L'ACPFACT)      INITIALS AND CHANGE DATE                      
ACCLVL   DS    CL3                ACCOUNT LVL                                   
ACCOUNT  DS    CL14               ACCOUNT                                       
BYTE     DS    CL1                                                              
SVKEY    DS    CL44                                                             
SVKEY2   DS    CL44                                                             
BLOCK    DS    2CL32                                                            
SPACES2  DS    CL132                                                            
                                                                                
         EJECT                                                                  
*                                                                               
* DISD - DSECT TO COVER DISPLAY SCREEN                                          
*                                                                               
DISD     DSECT                                                                  
DDIS     DS    CL30               DISCRIPTION                                   
         DS    CL1                                                              
DVAL     DS    CL20               ACCOUNT                                       
         DS    CL1                                                              
DLVL     DS    CL3                ACCOUNT LEVEL                                 
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
**PAN#1  DC    CL21'004ACTRA0A   11/29/18'                                      
         END                                                                    
