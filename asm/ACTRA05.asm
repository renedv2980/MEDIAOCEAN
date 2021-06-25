*          DATA SET ACTRA05    AT LEVEL 052 AS OF 02/15/19                      
*PHASE T62205B                                                                  
         TITLE 'TRA05 T62205  BILLING TRANSFER - PROF MAINT OVERLAY'            
T62205   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T62205**,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5            R5 = A(TWA)                                   
         USING SAVAREA,R6         R6 = A(SAVE AREA)                             
         LA    R8,4095(R7)                                                      
         LA    R8,1(R8)                                                         
         USING WORKD,R7,R8        R7 = A(GLOBAL WORKING STORAGE)                
         L     RC,APALOCAL        RC = A(LOCAL WORKING STORAGE)                 
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO          RELOCATION FACTOR                             
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   APMODE,APMVALK     VALIDATE KEY                                  
         BE    VALKEY                                                           
         CLI   APMODE,APMVALR     VALIDATE RECORD                               
         BE    VALREC                                                           
         CLI   APMODE,APMDISR     DISPLAY RECORD                                
         BE    DISREC                                                           
         CLI   APMODE,APMDISK     DISPLAY KEY (FOR SELECT)                      
         BE    DISKEY                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
*=====================================*                                         
* VALKEY - VALIDATES KEY              *                                         
*=====================================*                                         
*                                                                               
VALKEY   DS    0H                                                               
         TM    TWAFLAG,TWAFTRC    DID WE COME FROM TRACE                        
         BNO   VALK1              NO                                            
         CLI   APPFKEY,PFK03      FIRST TIME IN VALKEY                          
         BE    VALKOK                                                           
         DC    H'0'                                                             
VALK1    LA    R2,APRECKEY                                                      
         USING MPRRECD,R2                                                       
*                                                                               
         GOTO1 ADOSPEC            SET UP SPECIFIC PROFILE INFORMATION           
*                                                                               
         GOTO1 AVALSYS,PROSYSH    VALIDATE SYSTEM                               
         BNE   VALKX                                                            
         MVC   MPRKALPH,QALPH     AGY ALPHA FOR SPLIT FILES(IF SET)             
         MVC   MPRKSYS,QSYS                                                     
*                                                                               
         LA    R3,PROMEDH         VALIDATE MEDIA                                
         GOTO1 AVALMED,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKMED,QMED                                                     
         OC    QMED,QMED          IF NO MEDIA                                   
         BNZ   VALOFF                                                           
         GOTO1 AVALFLD,APPARM,PROOFFH,3                                         
         CLI   APPARM,X'FF'       CAN'T BE ANY OFF/CLT/PRD                      
         BE    MISSERR                                                          
*                                                                               
VALOFF   LA    R3,PROOFFH         VALIDATE OFFICE                               
         MVI   OCFLAG,0           MARK NO OFFICE INPUTTED                       
         GOTO1 AVALOFF,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKOFC,QOFF                                                     
         OC    QOFF,QOFF          IF NO OFFICE                                  
         BZ    VALKCLT            CHECK CLIENT INPUT                            
         MVI   OCFLAG,C'O'        MARK OFFICE INPUTTED                          
*                                                                               
VALKCLT  LA    R3,PROCLTH         VALIDATE CLIENT                               
         GOTO1 AVALCLT,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKCLI,QCLT                                                     
         OC    QCLT,QCLT          IF NO CLIENT REQUESTED                        
         BNZ   VALKCLT5                                                         
         GOTO1 AVALFLD,APPARM,PROPRDH,1                                         
         CLI   APPARM,X'FF'       CAN'T BE ANY PRD                              
         BE    MISSERR                                                          
         B     VALKPRD                                                          
*                                                                               
VALKCLT5 CLI   OCFLAG,C'O'        YES - CAN'T HAVE OFFICE TOO                   
         BE    *+12                                                             
         MVI   OCFLAG,C'C'        SET CLIENT REQUESTED ALONE                    
         B     VALKPRD                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKX                                                            
*                                                                               
VALKPRD  GOTO1 AVALPRD,PROPRDH    VALIDATE PRODUCT                              
         BNE   VALKX                                                            
         MVC   MPRKPRD,QPRD                                                     
*                                                                               
         GOTO1 ASETFILE           SET ACC FILES & LIMIT ACCESS                  
         BE    VALK30                                                           
         LA    R2,PROSYSH                                                       
         ST    R2,APCURSOR                                                      
         B     VALKX                                                            
*                                                                               
VALK30   BAS   RE,CKBILS          GET BILL SOURCE FROM POST/MAINT RECS          
         BAS   RE,SETDFLT         SET DEFAULT VALUES                            
*                                                                               
         XC    IOKEY,IOKEY        READ DUMMY ACC REC TO SWITCH                  
         MVC   IOKEY(1),COMPANY   TO NATIVE ACC SYSTEM BEFORE CALLING           
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE1'                         
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         USING ACPOSTD,R3                                                       
         BAS   RE,SETPST                                                        
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0              CLEAR SAVED ACC SYS(ACPOSTER DID           
*                                    READS TO MESS THIS FLAG UP)                
VALKOK   MVI   NEWREC,C'N'           NOT NEW RECORD                             
         MVI   NEWELE,C'Y'           NEW ELEMENT TYPE                           
         BAS   RE,RDREC              GET CURRENT KEY RECORD INTO AIO1           
         MVC   FVMSGNO,=AL2(FVFOK)   RESET ERROR MSG                            
         LA    R1,PROL1H                                                        
         STCM  R1,15,APCURSOR                                                   
         MVC   PROMSG,SPACES                                                    
         OI    PROMSGH+6,X'80'                                                  
         CLI   NEWELE,C'Y'                                                      
         BNE   VALKX                                                            
         MVC   PROMSG(30),=C'** NEW RECORD WILL BE ADDED **'                    
VALKX    B     EXIT                                                             
         SPACE                                                                  
MISSERR  MVC   FVMSGNO,=AL2(FVIMISS)                                            
         STCM  R3,15,APCURSOR                                                   
         B     VALKX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
*==================================================*                            
* CKBILS - READ POST MAINT FOR LATEST SI ACCOUNT   *                            
*==================================================*                            
*                                                                               
CKBILS   NTR1                                                                   
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         BAS   RE,CLRPST          CLEAR POST TABLE                              
         BAS   RE,SETPST                                                        
         MVI   ACPTYPE,0          READ PM RECORDS                               
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0              CLEAR SAVED ACC SYS(ACPOSTER DID           
*                                    READS TO MESS THIS FLAG UP)                
         DROP  R3                                                               
         MVC   BILLSCR,SPACES                                                   
         MVC   BILLSLVL,SPACES                                                  
         MVC   BILLSACT,SPACES                                                  
         CLI   SVMI,C'Y'                                                        
         BNE   CKBILS40                                                         
*                                 ON MI RECORDS                                 
         MVI   APBYTE,MBTTINC                                                   
         BAS   RE,GETACC          IF INCOME ACCOUNT EXISTS                      
         BNE   CKBILS20           NO - SEE ABOUT AOR INCOME ACCOUNT             
         CLC   =C'MI',ACCLVL      YES- SEE IF OVERRIDEN                         
         BE    CKBILS30             NO - USE MI DESCRIPTION                     
         BAS   RE,GETNAME           YES -GET SI ACCOUNT NAME                    
         B     CKBILS60                                                         
CKBILS20 MVI   APBYTE,MBTTARI     SEE IF AOR INCOME ACCOUNT EXISTS              
         BAS   RE,GETACC                                                        
         BNE   CKBILS30           NO - USE MI DESCRIPTION                       
         CLC   =C'AOR',ACCLVL     YES- SEE IF OVERRIDEN                         
         BE    CKBILS30             NO - USE MI DESCRIPTION                     
         BAS   RE,GETNAME           YES -GET SI AOR ACCOUNT NAME                
         B     CKBILS60                                                         
CKBILS30 BAS   RE,RDMI            GET MI DESCRIPTION                            
         MVC   ACCLVL,=C'MI '                                                   
         B     CKBILS60                                                         
*                                                                               
*                                 NOT ON MI RECORDS                             
CKBILS40 MVI   APBYTE,MBTTINC     IF INCOME ACCOUNT EXISTS                      
         BAS   RE,GETACC                                                        
         BE    CKBILS50           YES - USE IT                                  
         MVI   APBYTE,MBTTARI     IF INCOME ACCOUNT EXISTS                      
         BAS   RE,GETACC          OTHERWISE USE AOR INCOME ACCOUNT              
         BNE   CKBILSX            NO AOR INC EITHER                             
CKBILS50 BAS   RE,GETNAME                                                       
CKBILS60 MVC   BILLSLVL,ACCLVL                                                  
*                                                                               
CKBILSX  B     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* GETACC - GIVEN ROW NUMBER (APBYTE) SETS ACCOUNT,ACCLVL,& BILSACT *            
*==================================================================*            
*                                                                               
GETACC   NTR1                                                                   
         XC    ACCOUNT,ACCOUNT                                                  
         XC    ACCLVL,ACCLVL                                                    
         LA    R2,MAXPNUM                                                       
         L     R3,APSTTBL                                                       
         USING ACPRTND,R3                                                       
GETACC5  CLC   APBYTE,0(R3)                                                     
         BNE   GETACC10                                                         
         CLC   ACPACC,SPACES      ANY ACCOUNT?                                  
         BNH   GETACCN                                                          
         MVC   ACCOUNT,ACPACC     YES SAVE IT AND                               
         MVC   ACCLVL,ACPLVL      IT'S LVL                                      
         CLC   =C'MI',ACCLVL      IF DEFAULT - NO LAST ACTIVITY                 
         BE    GETACCY                                                          
         CLC   =C'DF',ACCLVL                                                    
         BE    GETACCY                                                          
         MVC   BILLSACT(L'ACPID),ACPID                                          
         GOTO1 VDATCON,APPARM,(3,ACPCHDT),(8,BILLSACT+4)                        
         MVC   ACCLVL,=C'PM '     THEN INDICATE POST MAINT OVERRIDE             
         B     GETACCY                                                          
*                                                                               
GETACC10 LA    R3,ACPRTNL(R3)                                                   
         BCT   R2,GETACC5                                                       
GETACCN  LTR   RB,RB                                                            
         B     GETACCX                                                          
GETACCY  CR    RB,RB                                                            
GETACCX  B     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* GETNAME - GIVEN ACCOUNT - READS RECORD FOR ACCOUNT NAME(BILSCR)  *            
*==================================================================*            
*                                                                               
GETNAME  NTR1                                                                   
         MVC   APWORK,SPACES                                                    
         LA    R2,IOKEY                                                         
         USING ACTKEY,R2                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY2   OTHER COMPANY CODE (IF APPL)                  
         OC    ACCOUNT,SPACES                                                   
         MVC   ACTKUNT(14),ACCOUNT                                              
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   GETNMX                                                           
         SR    R0,R0                                                            
         L     R2,AIOAREA3                                                      
         LA    R1,ACCORFST                                                      
         AR    R2,R1                                                            
         USING NAMELD,R2                                                        
GETNM5   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),NAMELQ       X'20'                                         
         BE    GETNM10                                                          
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     GETNM5                                                           
GETNM10  SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),NAMEREC                                                
         MVC   BILLSCR,APWORK     GET FIRST 12                                  
GETNMX   B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*=====================================*                                         
* RDMI - READS MI RECORD FOR BILLSCR  *                                         
*=====================================*                                         
*                                                                               
RDMI     NTR1                                                                   
         MVC   BILLSCR,SPACES                                                   
         LA    R2,IOKEY                                                         
         USING MINRECD,R2                                                       
         MVC   MINKEY,SPACES                                                    
         MVI   MINKTYP,MINKTYPQ    X'08'                                        
         MVC   MINKCPY,COMPANY2    OTHER COMPANY CODE(IF APPL)                  
         MVC   MINKMED(1),QSYS     SYSTEM                                       
         MVC   MINKMED+1(1),QMED   MEDIA                                        
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   RDMIX                                                            
         DROP  R2                                                               
         L     R2,AIOAREA3                                                      
         AH    R2,DATADISP                                                      
         USING MDIELD,R2                                                        
         SR    R0,R0                                                            
RDMI2    CLI   0(R2),0            END OF RECORD?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),MDIELQ       X'19'                                         
         BE    RDMI4                                                            
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     RDMI2                                                            
*                                                                               
RDMI4    MVC   BILLSCR,MDIDESC    GET MI DESCRIPTION                            
RDMIX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*=======================================*                                       
* SETDFLT - SET DEFAULTS FOR SCREEN     *                                       
*=======================================*                                       
*                                                                               
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
         LA    R1,DFLTBLX         PT TO TRADE DEFAULT VALUES                    
         CLI   RECTYPE,MPRKTRAD   IF REGULAR TRADE                              
         BE    SETD10                                                           
         CLI   RECTYPE,MPRKTRAA   OR AOR WITH TRADE                             
         BE    SETD10                                                           
         LA    R1,DFLMPRF        PT TO MPROF DEFAULT VALUES                     
         CLI   RECTYPE,MPRKMTRA                                                 
         BE    SETD10                                                           
         LA    R1,DFLTBLP         PT TO PRINT DEFAULT VALUES                    
*                                                                               
SETD10   CLI   0(R1),X'FF'        END OF BASIC DEFAULTS                         
         BE    SETDX                                                            
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         CLI   0(R4),MTPFBILS     IF WANT BILL SOURCE                           
         BNE   SETD12                                                           
         XC    ACPFVAL,ACPFVAL                                                  
         MVC   ACPFVAL(L'BILLSCR),BILLSCR                                       
         MVC   ACPFLVL,BILLSLVL                                                 
         MVC   ACPFACT,BILLSACT                                                 
         B     SETD18                                                           
*                                                                               
SETD12   CLI   0(R4),MTPFDACC     DDS ACC FILE                                  
         BE    *+12                                                             
         CLI   0(R4),MTPFDMED     DDS MEDIA FILE                                
         BNE   SETD15                                                           
         MVC   ACPFVAL,SPACES                                                   
         MVC   ACPFVAL(2),TWAAGY  MOVE IN AGENCY ALPHA                          
         MVC   ACPFLVL,=C'DF '                                                  
         B     SETD18                                                           
*                                                                               
SETD15   CLI   0(R4),MTPFIPCT     IF % FOR IOR                                  
         BE    SETD15A                                                          
         CLI   0(R4),MTPFIPT2                                                   
         BE    SETD15A                                                          
         CLI   0(R4),MTPFSPCT                                                   
         BE    SETD15A                                                          
         CLI   0(R4),MTPFPCT                                                    
         BNE   SETD17                                                           
SETD15A  XC    ACPFVAL,ACPFVAL                                                  
         MVC   ACPFLVL,=C'DF '                                                  
         B     SETD18                                                           
*                                                                               
SETD17   MVC   ACPFVAL,0(R1)      MOVE IN DEFAULT                               
         CLC   ACPFVAL,SPACES                                                   
         BNH   SETD18                                                           
         MVC   ACPFLVL,=C'DF '                                                  
*                                                                               
SETD18   LA    R4,APTABLN(R4)                                                   
         LA    R1,L'DFLTBL(R1)    NEXT DEFAULT                                  
         B     SETD10                                                           
*                                                                               
SETDX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------*                                           
* CLRPST - CLEAR POST TABLE         *                                           
*-----------------------------------*                                           
*                                                                               
CLRPST   NTR1                                                                   
         LA    RE,MAXPNUM                                                       
         L     RF,APSTTBL                                                       
CLRPST5  XC    0(ACPRTNL,RF),0(RF)                                              
         LA    RF,ACPRTNL(RF)                                                   
         BCT   RE,CLRPST5                                                       
         B     EXIT                                                             
         SPACE                                                                  
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
* DEFAULT FOR PPROF MAINT                                                       
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
*                                                                               
* DEFAULT FOR XPROF MAINT                                                       
*                                                                               
DFLTBLX  DS    0CL20              LENGTH OF EACH VALUE                          
         DC    CL3'.00',CL17' '   PERCENTAGE FOR BASIS                          
         DC    CL1' ',CL19' '     BASIS (GROSS OR NET)                          
         DC    CL20' '            OFFICE CODE OVERRIDE FOR TRADE                
         DC    CL3'.00',CL17' '   INTERCOMPANY PERCENTAGE                       
         DC    X'FF'                                                            
*                                                                               
* DEFAULT FOR MPROF MAINT                                                       
*                                                                               
DFLMPRF  DS    0CL20              LENGTH OF EACH VALUE                          
         DC    CL20' '            OFFICE CODE OVERRIDE FOR INTERNAL             
         DC    CL3'.00',CL17' '   PERCENTAGE                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*=====================================*                                         
* SETPST - SET UP CALL TO ACPOSTER    *                                         
*=====================================*                                         
*                                                                               
SETPST   NTR1                                                                   
         LA    RE,PSTBLK          CLEAR CONTROL BLOCK                           
         LA    RF,ACPOSTL                                                       
         XCEFL                                                                  
*                                                                               
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         USING ACPOSTD,R3                                                       
         MVC   ACPACOM,ACOM       A(COMFACS)                                    
         MVC   ACPSW,VSWITCH      A(SWITCH)                                     
         MVC   ACPPOST,APSTTBL    A(POSTINGS RETURNED)                          
         MVC   ACPCMPC,COMPANY    NATIVE COMPANY CODE                           
         MVC   ACPCMPC2,COMPANY2  OTHER COMPANY CODE                            
         MVC   ACPMI,SVMI         Y= USE MI RECORDS                             
         MVC   ACPALPH,QALPH      AGY ALPHA FOR SPLIT MEDIA FILES               
         MVC   ACPSYS,QSYS        SYSTEM                                        
         MVC   ACPMED,QMED        MEDIA CODE                                    
         MVC   ACPSE1,SVSE1       NATIVE SE NUMBER                              
         MVC   ACPSE2,SVSE2       OTHER SE NUMBER                               
         OC    QCLT,QCLT          WAS CLIENT ENTERED?                           
         BNZ   SETPST20                                                         
         MVC   ACPOFC,QOFF        NO - OFFICE                                   
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
         CLI   QOFFIND,C'O'       WAS IT OFFICE CODE OR OFFICE GROUP            
         BE    SETPST30           OFFICE CODE                                   
         MVI   ACPOFC,0           OFFICE                                        
         MVC   ACPOFG,QOFF        OFFICE GROUP                                  
         B     SETPST30                                                         
*                                                                               
SETPST20 MVC   ACPOFC,SVCOFF      OFFICE (CLIENT ENTERED)                       
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
*                                                                               
SETPST30 MVC   ACPCLT,QCLT        CLIENT                                        
         MVC   ACPPRD,QPRD        PRODUCT                                       
         MVI   ACPTYPE,1          PROF RECORDS                                  
         MVC   ACPTYPE2,RECTYPE   REG,AOR,RET,PRD (REPLACES A(BILL))            
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*==============================================*                                
* RDREC - BUILD LOWEST LEVEL PROFILE RULE      *                                
*         KEY AND GET THE RECORD INTO AIO1     *                                
*==============================================*                                
*                                                                               
RDREC    NTR1                                                                   
         LA    R4,IOKEY           BUILD POST MAINT(RULES)  RECORD               
         USING MPRRECD,R4                                                       
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,QALPH     AGY ALPHA FOR MEDIA SPLIT FILES               
         MVC   MPRKSYS,QSYS       SYSTEM                                        
         MVC   MPRKMED,QMED       MEDIA                                         
         MVC   MPRKOFC,QOFF       OFFICE                                        
         MVC   MPRKCLI,QCLT       CLIENT                                        
         MVC   MPRKPRD,QPRD       PRODUCT                                       
         MVC   MPRKPRO,RECTYPE    REGULAR BILLING                               
         MVC   SVKEY,MPRKEY                                                     
*                                                                               
         CLI   APACTN,ACTDIS                                                    
         BNE   RDREC1                                                           
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO1+IOLOCK,=C'SE1'                  
         B     RDREC2                                                           
*                                                                               
RDREC1   GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO1,=C'SE1'                         
*                                                                               
RDREC2   L     R4,AIOAREA1                                                      
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    IOERR,IOERNF       RECORD NOT FOUND?                             
         BNO   RDREC10                                                          
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         MVI   NEWREC,C'Y'        MARK NEW RECORD                               
         XC    0(256,R4),0(R4)    RESET IO AREA                                 
         MVC   MPRKEY,SVKEY       MOVE IN KEY                                   
         B     RDRECX                                                           
*                                                                               
RDREC10  AH    R4,DATADISP                                                      
RDREC15  CLI   0(R4),0                                                          
         BE    RDRECX                                                           
         CLI   0(R4),MTPELQ       PROF ELEMENT?                                 
         BNE   RDREC18                                                          
         MVI   NEWELE,C'N'        NOT NEW ELEMENT                               
         B     RDRECX                                                           
*                                                                               
RDREC18  SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     RDREC15                                                          
RDRECX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*=================*                                                             
* VALIDATE RECORD *                                                             
*=================*                                                             
*                                                                               
VALREC   TM    TWAFLAG,TWAFTRC    DID WE COME FROM TRACE                        
         BNO   VALR10                                                           
         OI    TWALSCTL,TWALSRTN                                                
         OI    TWALSCTL,TWALSHLD                                                
         MVI   TWALSACT,ACTMAI                                                  
         NI    TWAFLAG,FF-TWAFTRC TURN OFF INDICATOR                            
         B     VALRX                                                            
*                                                                               
VALR10   MVI   RECHANG,C'N'       RECORD HAS NOT BEEN CHANGED                   
         XC    MEDTAB,MEDTAB      CLEAR OUT MEDIA ALPHA AGENCY                  
         XC    ACCTAB,ACCTAB      CLEAR OUT ACC ALPHA AGENCY                    
         XC    IORPCT,IORPCT      CLEAR OUT IOR PERCENTAGES                     
         XC    IORPCTA,IORPCTA                                                  
         XC    TRDPCT,TRDPCT                                                    
         XC    STRDPCT,STRDPCT                                                  
         GOTO1 VDATCON,APPARM,(5,APWORK),(3,LCHDT)                              
         LA    R1,1               FIRST PROFING                                 
         USING ACPRTND,R3                                                       
         LA    R2,PROL1DH         POINT TO FIRST LINE ON SCREEN                 
         USING DISD,R2                                                          
         L     R4,APTABLE         CURRENT RECORD ROW TABLE                      
*                                                                               
VALR20   BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         OC    DVAL,SPACES                                                      
         CLI   0(R4),MTPFIPCT      IF NUMBER                                    
         BE    VALR20A                                                          
         CLI   0(R4),MTPFSPCT                                                   
         BE    VALR20A                                                          
         CLI   0(R4),MTPFIPT2                                                   
         BE    VALR20A                                                          
         CLI   0(R4),MTPFPCT       IF NUMBER                                    
         BNE   VALR30                                                           
VALR20A  BAS   RE,DOCMP           COMPARE DIFFERENTLY                           
         BE    VALR50                                                           
         B     VALR40                                                           
VALR30   OC    ACPFVAL,SPACES                                                   
         CLC   ACPFVAL,DVAL       CHANGE IN VALUE?                              
         BE    VALR50             NO - GO TO NEXT PROFILE LINE                  
*                                                                               
***                                                                             
         CLI   ACPFVAL,C' '                                                     
         BE    VALR40                                                           
         CLI   DVAL,C' '          ARE THEY CLEARING THE FIELD?                  
         BNE   VALR40                                                           
         CLI   TWAOFFC,C'*'       IF NOT DDS TERMINAL                           
         BE    VALR40             DON'T LET CLEAR ACC & MEDIA FILE              
         CLI   0(R4),MTPFDACC     IF ACC FILE                                   
         BE    *+12                                                             
         CLI   0(R4),MTPFDMED     OR MEDIA FILE                                 
         BNE   VALR40                                                           
         MVC   DVAL,ACPFVAL       KEEP OLD VALUE                                
         B     VALR50             NO GO TO NEXT PROFILE LINE                    
***                                                                             
*                                                                               
VALR40   BAS   RE,CHKVAL          YES, VALIDATE                                 
         BNE   VALRCRSR                                                         
         MVC   ELETYPE,0(R4)      CHANGE IN ROW - SAVE ELEMENT TYPE             
         BAS   RE,DOELEM          ADD/CHANGE ELE IN REC (LOWEST LVL)            
*                                                                               
VALR50   LA    R2,DISDL(R2)       PT TO NEXT SCREEN LINE                        
         LA    R1,1(R1)           NEXT PROFING                                  
         LA    R4,APTABLN(R4)                                                   
         ZIC   R0,PMAXNUM         FINISHED ALL PROFILES?                        
         CR    R1,R0                                                            
         BNH   VALR20             NO -CHECK NEXT PROFING ROW FOR CHANGE         
*                                                                               
         CLI   RECHANG,C'Y'       HAS RECORD CHANGED?                           
         BE    VALR55             YES                                           
         MVC   FVADDR,AACTHDR     NO - LEAVE MSG THE SAME                       
         MVC   FVMSGNO,=AL2(FVFCHG1) SAYING TO ENTER RECORD CHANGES             
         MVI   FVOMTYP,C'I'                                                     
         B     VALR70                                                           
*                                                                               
VALR55   MVC   PROMSG,SPACES      CLEAR OUT MSG                                 
         CLI   NEWREC,C'Y'        RECORD HAS CHANGED - IS IT NEW?               
         BE    VALR60             YES - ADD IT                                  
         GOTO1 AMIOACC,APPARM,IOACCFIL+IOWRITE+IO1,=C'SE1'                      
         B     VALR70                                                           
VALR60   GOTO1 AMIOACC,APPARM,IOACCFIL+IOADD+IO1,=C'SE1'                        
         MVC   FVADDR,AACTHDR             AND HAVE MSG REFLECT                  
         MVC   FVMSGNO,=AL2(FVFADD1)      NEW ADD                               
         MVI   FVOMTYP,C'I'                                                     
*                                                                               
VALR70   TM    TWAMODE,TWAMLSM    SELECT FROM LIST                              
         BNO   VALR75                                                           
         OI    TWALSCTL,TWALSRTN  STAY AT MAINT                                 
         OI    TWALSCTL,TWALSHLD  HOLD SCREEN                                   
*                                                                               
VALR75   BAS   RE,DISCHANG        TRANSMIT CHANGED RECORD                       
         CLC   FVMSGNO,=X'0000'                                                 
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         BAS   RE,CHKPFKS         CHECK IF PFKEY WAS HIT                        
VALRX    B     EXIT                                                             
         SPACE 2                                                                
VALRTERR MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LTR   RB,RB                                                            
         B     EXIT                                                             
         SPACE 2                                                                
VALRCRSR LA    R1,DVALH-DISD                                                    
         AR    R1,R2                                                            
         STCM  R1,15,APCURSOR                                                   
         B     EXIT                                                             
         SPACE 2                                                                
*  NEED TO GET CHARACTER FORM SO CAN DO CORRECT COMPARE                         
DOCMP    NTR1                                                                   
         EDIT  (4,ACPFVAL),(5,APWORK2),2,ALIGN=LEFT,ZERO=NOBLANK,      X        
               DUB=APDUB,WRK=APWORK                                             
         OC    APWORK2,SPACES                                                   
         CLC   APWORK2,DVAL        SET CONDITION CODE                           
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------*                                    
* CHKVAL - CHECKS PROFILE VALUES ON INPUT  *                                    
*------------------------------------------*                                    
*                                                                               
CHKVAL   NTR1                                                                   
         CLI   DVALH+5,0          IF INPUT                                      
         BE    VALY                                                             
*                                                                               
         LA    R1,VALTBL          VALIDATE IT                                   
         USING VALTBLD,R1                                                       
VAL5     CLI   0(R1),X'FF'        TEST END OF TABLE                             
         BE    VALY                                                             
         CLC   0(1,R4),0(R1)      MATCH ON EQUATED ROW                          
         BE    *+12                                                             
         LA    R1,L'VALTBL(R1)                                                  
         B     VAL5                                                             
*                                                                               
         CLI   VALMINL,0           IF MINIMUM LENGTH NECESSARY                  
         BE    *+14                                                             
         CLC   DVALH+5(1),VALMINL  AND LESS THAN MINIMUM                        
         BL    VALN                GIVE ERROR                                   
*                                                                               
         CLI   VALMAXL,0           IF MAXIMUM LENGTH NECESSARY                  
         BE    *+14                                                             
         CLC   DVALH+5(1),VALMAXL  AND GREATER THAN MINIMUM                     
         BH    VALN                GIVE ERROR                                   
*                                                                               
         L     RF,VALRTN          IF ROUTINE SPECIFIED                          
         LTR   RF,RF                                                            
         BZ    VALY                                                             
         A     RF,APRELO          ADD RELOCATION FACTOR                         
         BR    RF                 AND BRANCH TO VALIDATION ROUTINE              
         DROP  R1                                                               
*                                                                               
VALN     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALNX                                                            
VALN2    MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALNX                                                            
VALN3    MVC   FVMSGNO,=AL2(FVINOC)                                             
         B     VALNX                                                            
VALN4    MVC   FVMSGNO,=AL2(FVILDGR)                                            
         B     VALNX                                                            
*                                                                               
VALNX    LTR   RB,RB              SET CC,MSG, AND CURSOR                        
         B     VALX                                                             
VALY     CR    RB,RB                                                            
VALX     B     EXIT                                                             
         EJECT                                                                  
CHKVALID DS    0H                 CHECK POWER CODE IS VALID ID                  
         CLI   TWAOFFC,C'*'       IF NOT DDS TERMINAL                           
         BNE   VALN               DON'T LET THEM CHANGE                         
*                                                                               
         LA    R3,ACCTAB          POINT TO ACC ALPHA AGENCY CODE                
         CLI   0(R4),MTPFDACC     IF DDS ACC FILES ROWS                         
         BE    *+8                                                              
         LA    R3,MEDTAB          ELSE, POINT TO MED ALPHA AGENCY CODE          
*                                                                               
         MVC   APHALF,DVAL                                                      
         BAS   RE,CHKID           CHECK IF ID VALID                             
         BNE   VALN               ERROR                                         
         MVC   0(2,R3),APHALF     SAVE IT FOR LATER ADD                         
         B     VALY                                                             
         SPACE 2                                                                
CHKID    NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R1,IOKEY                                                         
         USING CT5REC,R1                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,APHALF    ID                                            
         GOTO1 AIO,IORD+IOCTFILE+IO3                                            
         B     EXIT               CC CODE SET                                   
         DROP  R1                                                               
         EJECT                                                                  
* CHKPST - CHECK DDS POSTINGS INPUT                                             
*                                                                               
CHKPST   DS    0H                                                               
         CLI   TWAOFFC,C'*'       IF NOT DDS TERMINAL                           
         BE    *+14                                                             
         MVC   DVAL,ACPFVAL       JUST IGNORE CHANGES                           
         B     VALY                                                             
         CLI   DVAL,C'Y'                                                        
         BE    VALY                                                             
         CLI   DVAL,C'N'                                                        
         BE    VALY                                                             
         B     VALN                                                             
         SPACE 2                                                                
* CHKLEDGR - VALIDATE UNIT 3 LEDGER                                             
*                                                                               
CHKLEDGR DS    0H                                                               
         OC    QCLT,QCLT          IF CLIENT OR OFFICE SPECIFIED                 
         BNZ   CHKLDG5                                                          
         OC    QOFF,QOFF                                                        
         BZ    VALN3              SET MISSING CLIENT                            
         CLI   QOFFIND,C'O'                                                     
         BNE   VALN3              SET MISSING CLIENT                            
CHKLDG5  MVC   IOKEY,SPACES       READ FOR LEDGER                               
         LA    R1,IOKEY                                                         
         USING LDGKEY,R1                                                        
         MVC   LDGKCPY,COMPANY2   OTHER COMPANY CODE (IF APPL)                  
         MVI   LDGKUNT,C'3'                                                     
         MVC   LDGKLDG,DVAL       SET LEDGER                                    
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BE    VALX                                                             
         B     VALN4              SET INVALID LEDGER                            
         DROP  R1                                                               
         EJECT                                                                  
* CHKDELM - CHECK VALID DELIMITER                                               
*                                                                               
CHKDELM  DS    0H                                                               
         CLI   DVAL,C'/'                                                        
         BE    VALY                                                             
         B     VALN                                                             
         SPACE 2                                                                
* CHKMOS  - CHECK FORWARD AND BACKWARD MOS DATE                                 
*                                                                               
CHKMOS   DS    0H                                                               
         CLC   =C'00',DVAL        IF SIGNIFICANT INPUT                          
         BE    VALY                                                             
         TM    DVALH+4,X'08'      AND VALID NUMERIC                             
         BNO   VALN                                                             
         CLI   DVALH+5,2           MUST BE TWO BYTES LENGTH                     
         BL    VALN                                                             
         CLI   0(R4),MTPFMOSB     IF BACKWARD                                   
         BNE   *+12                                                             
         MVI   APBYTE,MTPFMOSF    CHECK IF MOS FORWARD DATE EXISTS              
         B     *+8                                                              
         MVI   APBYTE,MTPFMOSB    CHECK IF MOS BACKWARD DATE EXISTS             
*                                                                               
         ZIC   R0,PMAXNUM                                                       
         L     RE,APTABLE                                                       
         LA    R2,PROL1DH                                                       
         USING DISD,R2                                                          
CHKMOS5  CLC   APBYTE,0(RE)                                                     
         BE    CHKMOS10                                                         
         LA    RE,APTABLN(RE)                                                   
         LA    R2,DISDL(R2)                                                     
         BCT   R0,CHKMOS5                                                       
         DC    H'0'               MUST BE IN TABLE                              
*                                                                               
CHKMOS10 CLI   DVALH+5,0          CC CODE SET                                   
         BE    VALY               NO INPUT                                      
         CLC   =C'00',DVAL        CC CODE SET                                   
         BE    VALY                                                             
         B     VALN                                                             
         EJECT                                                                  
* CKWKCD - CHECK WORK CODE VALID                                                
*                                                                               
CHKWKCD  DS    0H                                                               
         MVC   IOKEY,SPACES                                                     
         LA    R1,IOKEY                                                         
         USING WCOKEY,R1                                                        
         MVI   WCOKTYP,WCOKTYPQ   X'0A'                                         
         MVC   WCOKCPY,COMPANY2   OTHER COMPANY CODE (IF APPL)                  
         MVC   WCOKUNT(2),SVPROD  UNIT/LEDGER                                   
         MVC   WCOKWRK,DVAL       WORK CODE                                     
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   VALN2                                                            
         B     VALY                                                             
         DROP  R1                                                               
         SPACE 2                                                                
* CHKPGR - CHECK VALID PRODUCT GROUP SCHEME                                     
*                                                                               
CHKPGR   DS    0H                                                               
         LA    R1,PPGRTBL         PT TO PRINT VALID PGR TABLE                   
         CLI   QSYS,C'P'          IF PRINT                                      
         BE    CHKPGR10           GO VALIDATE BY TABLE                          
         CLI   DVAL,C'A'          ELSE, SPOT/NET A-Z VALID                      
         BL    VALN                                                             
         CLI   DVAL,C'Z'                                                        
         BH    VALN                                                             
         B     VALY                                                             
*                                                                               
CHKPGR10 CLI   0(R1),X'FF'                                                      
         BE    VALN               NOT VALID INPUT                               
         CLC   0(1,R1),DVAL       MATCH OF VALID PRODUCT GROUP SCHEME           
         BE    VALY                                                             
         LA    R1,1(R1)                                                         
         B     CHKPGR10                                                         
         EJECT                                                                  
* CHKYORN - CHECK YES OR NO                                                     
*                                                                               
CHKYORN  DS    0H                                                               
         CLI   DVAL,C'Y'                                                        
         BE    VALY                                                             
         CLI   DVAL,C'N'                                                        
         BE    VALY                                                             
         B     VALN                                                             
         SPACE 2                                                                
* CHKBAS - CHECK G(GROSS) OR N(NET)                                             
*                                                                               
CHKBAS   DS    0H                                                               
         CLI   DVAL,C'G'                                                        
         BE    VALY                                                             
         CLI   DVAL,C'N'                                                        
         BE    VALY                                                             
         B     VALN                                                             
         SPACE 2                                                                
* CHKPCT - CHECK % VALID                                                        
*                                                                               
CHKPCT   DS    0H                                                               
         ZIC   R0,DVALH+5                                                       
         GOTO1 VCASHVAL,APPARM,(3,DVAL),(R0)                                    
         CLI   0(R1),X'FF'                                                      
         BE    VALN                                                             
         CLC   4(4,R1),=C'100000'   CAN'T BE MORE THAN 100.000%                 
         BH    VALN                                                             
         CLI   0(R4),MTPFPCT                                                    
         BNE   *+14                                                             
         MVC   TRDPCT,4(R1)                                                     
         B     VALY                                                             
         CLI   0(R4),MTPFSPCT                                                   
         BNE   *+14                                                             
         MVC   STRDPCT,4(R1)                                                    
         B     VALY                                                             
         CLI   0(R4),MTPFIPCT      SAVE PERCENTAGE IN APPROPRIATE FLD           
         BNE   *+14                                                             
         MVC   IORPCT,4(R1)                                                     
         B     *+10                                                             
         MVC   IORPCTA,4(R1)                                                    
         B     VALY                                                             
         EJECT                                                                  
* CHKOFC - CHECK VALID OFFICE CODE                                              
*                                                                               
CHKOFC   DS    0H                                                               
         TM    SVOFF,SVOFFNEW      IF NEW OFFICE CODES                          
         BZ    CHKOFC2                                                          
         CLI   DVALH+5,2           MUST BE 2 CHARACTERS LONG                    
         BNE   VALN                                                             
         LA    R1,IOKEY                                                         
         USING OFFRECD,R1                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY2   OTHER COMPANY CODE (IF APPL)                  
         MVC   OFFKOFF,DVAL       NEW STYLE OFFICE CODE                         
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   VALN                                                             
         SR    R0,R0                                                            
         L     R1,AIOAREA3                                                      
         LA    RE,ACCORFST                                                      
         AR    R1,RE                                                            
CHKOFC1  CLI   0(R1),0             MAKE SURE NO D2 ELEMENT                      
         BNE   CHKOFC5                                                          
         CLI   0(R1),OFLELQ        LOOK FOR OFFICE LIST ELEM                    
         BE    VALN                                                             
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     CHKOFC1                                                          
*                                                                               
CHKOFC2  TM    SVOFF,SVOFFREG      IF REGULAR OFFICE CODES                      
         BZ    VALN                                                             
         CLI   DVALH+5,1           MUST BE ONE CHARACTER ONLY                   
         BNE   VALN                                                             
*                                                                               
CHKOFC5  LA    R1,IOKEY                                                         
         USING ACTKEY,R1                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY2   OTHER COMPANY CODE (IF APPL)                  
         MVI   ACTKUNT,C'2'                                                     
         MVI   ACTKLDG,C'D'                                                     
         MVC   ACTKACT,DVAL                                                     
         OC    ACTKACT,SPACES                                                   
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   VALN                                                             
         B     VALY                                                             
         DROP  R1                                                               
         EJECT                                                                  
*=====================================================*                         
* DOELEM- ADDS NEW ELEMENT OR CHANGES CURRENT ELEMENT *                         
*      NTRY - ELETYPE = PROFILE NUMBER , R2 ->NEW     *                         
*             ON SCREEN                               *                         
*=====================================================*                         
*                                                                               
DOELEM   NTR1                                                                   
         L     R4,AIOAREA1                                                      
         ST    R4,AIOAREA         SET ADDRESS OF RECORD                         
         AH    R4,DATADISP        PT TO FIRST ELEMENT                           
         USING MTPELD,R4                                                        
         SR    R0,R0                                                            
*                                                                               
DOELEM20 CLI   0(R4),0            END OF RECORD?                                
         BE    DOELEM50           YES - NEW ELEMENT MUST BE ADDED               
         CLI   0(R4),MTPELQ       MEDIA TRANSFER PROFILE = X'2E'                
         BNE   DOELEM30           YES -                                         
         CLC   ELETYPE,MTPFNUM    CORRECT PROFILE NUMBER?                       
         BE    DOELEM40           YES                                           
*                                                                               
DOELEM30 ICM   R0,1,1(R4)         NO - GET ELEMENT LENGTH                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0               AND BUMP TO NEXT ELEMENT                     
         B     DOELEM20                                                         
*                                                                               
DOELEM40 DS    0H                 CHANGE ELEMENT (DELETE THEN ADD)              
         MVI   APELCD,MTPELQ      ELEMENT CODE X'2E'                            
         GOTO1 ADELL,APPARM,(X'01',ELETYPE)                                     
         MVI   RECHANG,C'Y'       INDICATE RECORD HAS BEEN CHANGED              
         DROP  R4                                                               
*                                                                               
DOELEM50 XC    APELEM,APELEM                                                    
         LA    R1,APELEM          NEW ELEMENT MUST BE ADDED                     
         USING MTPELD,R1                                                        
         MVI   MTPEL,MTPELQ        X'2E'                                        
         SR    RE,RE                                                            
         ICM   RE,1,DVALH+5       LENGTH OF DVALUE                              
         BZ    DOELEM60           NO NEW DATA TO ADD                            
         MVC   MTPFNUM,ELETYPE    PROFILE NUMBER                                
         MVC   MTPFCHDT,LCHDT     TODAYS DATE                                   
         MVC   MTPFCHID,PERSON    SET PERSON                                    
         ZIC   RE,DVALH+5         LENGTH OF DVALUE                              
*                                                                               
         CLI   ELETYPE,MTPFDACC   ACC FILES                                     
         BNE   DOELEM52                                                         
         LA    RE,L'ACCTAB-1                                                    
         OC    ACCTAB,ACCTAB      IF ACC ALPHA AGENCY DEFINED                   
         BNZ   *+6                                                              
         DC    H'0'               CAN'T ADD ELEMENT WITH NO ID                  
         MVC   MTPFDATA(2),ACCTAB                                               
         B     DOELEM58                                                         
*                                                                               
DOELEM52 CLI   ELETYPE,MTPFDMED   MED FILES                                     
         BNE   DOELEM53                                                         
         LA    RE,L'MEDTAB-1                                                    
         OC    MEDTAB,MEDTAB       IF ID DEFINED                                
         BNZ   *+6                                                              
         DC    H'0'                CAN'T ADD ELEMENT WITH NO ID                 
         MVC   MTPFDATA(2),MEDTAB                                               
         B     DOELEM58                                                         
*                                                                               
DOELEM53 CLI   ELETYPE,MTPFIPCT   IF IOR PERCENTAGE                             
         BNE   DOELEM54                                                         
         LA    RE,L'IORPCT-1                                                    
         MVC   MTPFDATA(L'IORPCT),IORPCT                                        
         B     DOELEM58                                                         
*                                                                               
DOELEM54 CLI   ELETYPE,MTPFPCT    TRADE PERCENTAGE                              
         BNE   DOELE54A                                                         
         LA    RE,L'TRDPCT-1                                                    
         MVC   MTPFDATA(L'TRDPCT),TRDPCT                                        
         B     DOELEM58                                                         
*                                                                               
DOELE54A CLI   ELETYPE,MTPFSPCT   SPLIT TRADE PERCENTAGE                        
         BNE   DOELEM55                                                         
         LA    RE,L'STRDPCT-1                                                   
         MVC   MTPFDATA(L'STRDPCT),STRDPCT                                      
         B     DOELEM58                                                         
*                                                                               
DOELEM55 CLI   ELETYPE,MTPFIPT2   IF IOR PERCENTAGE FOR AOR                     
         BNE   DOELEM56                                                         
         LA    RE,L'IORPCTA-1                                                   
         MVC   MTPFDATA(L'IORPCTA),IORPCTA                                      
         B     DOELEM58                                                         
*                                                                               
DOELEM56 BCTR  RE,0               REGULAR VALUE JUST TAKE IT                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MTPFDATA(0),DVAL   SET VALUE                                     
*                                                                               
DOELEM58 LA    RE,1(RE)           LENGTH OF VALUE ADDED TO ELEM                 
         LA    RF,MTPFDATA-MTPEL                                                
         AR    RE,RF                                                            
         STC   RE,MTPLN           ELEMENT LENGTH                                
         GOTO1 AADDL                                                            
         MVI   RECHANG,C'Y'       INDICATE RECORD HAS BEEN CHANGED              
DOELEM60 MVI   ACPBCHG,C'C'       INDICATE THIS LINE HAS CNANGED                
*                                                                               
DOELEMX  B     EXIT                                                             
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
         SPACE                                                                  
*-----------------------------------------*                                     
* SETTBL - SET TABLE OF VALID TRACE LINES *                                     
*-----------------------------------------*                                     
*                                                                               
SETTBL   NTR1                                                                   
         LA    R3,TRACTBL                                                       
         LA    RF,MAXPNUM                                                       
SETTBL2  XC    0(L'TRACTBL,R3),0(R3)                                            
         BCT   RF,SETTBL2                                                       
*                                                                               
         LA    R3,TRACTBL                                                       
         ZIC   RF,PMAXNUM                                                       
         LA    R2,PROL1DH                                                       
SETTBL5  LR    R1,R2                                                            
         SR    R1,R5              DISPLAY OF PROL1D FROM TOP OF TWA             
         STH   R1,0(R3)                                                         
         LA    RE,PROL2DH-PROL1DH LENGTH OF LINE                                
         AR    R2,RE              PT TO NEXT LINE ON SCREEN                     
         LA    R3,L'TRACTBL(R3)   PT TO NEXT TABLE ENTRY                        
         BCT   RF,SETTBL5                                                       
         B     EXIT                                                             
         SPACE                                                                  
*===================================*                                           
* DISCHANG - DISPLAY CHANGED RECORD *                                           
*            BEFORE EXITING TO USER *                                           
*===================================*                                           
*                                                                               
DISCHANG NTR1                                                                   
         BAS   RE,SETDFLT         SET DEFAULT VALUES                            
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         USING ACPOSTD,R3                                                       
         BAS   RE,SETPST                                                        
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0              CLEAR SAVED ACC SYS(ACPOSTER DID           
*                                    READS TO MESS THIS FLAG UP)                
         BAS   RE,DISPTBL                                                       
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*===================================*                                           
* CHKPFKS  - CHECKS IF PFKEY WAS HIT*                                           
*===================================*                                           
*                                                                               
CHKPFKS  NTR1                                                                   
*                    PFK08 & PFK11 -SWAP W/NO INTENTION OF RETURNING            
         CLI   APPFKEY,PFK08      BACK UP ONE RECORD?                           
         BNE   CHKPFK10                                                         
         ZIC   R1,INREC           CURRENT RECORD NUMBER                         
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,RECSPROF        MAX RECORD NUMBER                             
         STC   R1,APPARM          RECORD TO SWAP TOO                            
         MVI   APPARM+1,ACTMAI    ACTION TO SWAP TOO                            
         MVI   APMODE,APMSWP      SWAP                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CHKPFKX                                                          
*                                                                               
CHKPFK10 CLI   APPFKEY,PFK11      FORWARD ONE RECORD?                           
         BNE   CHKPFK20                                                         
         ZIC   R1,INREC           CURRENT RECORD NUMBER                         
         LA    RE,RECSPROF        MAX RECORD NUMBER                             
         CR    R1,RE                                                            
         BNE   *+8                                                              
         LA    R1,0               START AT ONE AGAIN                            
         LA    R1,1(R1)           BUMP TO NEXT RECORD NUMBER                    
         STC   R1,APPARM          RECORD TO SWAP TOO                            
         MVI   APPARM+1,ACTMAI    ACTION TO SWAP TOO                            
         MVI   APMODE,APMSWP      SWAP                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CHKPFKX                                                          
*                                                                               
CHKPFK20 CLI   APPFKEY,PFK10      TRACE PRESSED?                                
         BNE   CHKPFKX            NO                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   APMODE,APMSWP      YES - INDICATE SWAP AND SAVE SCREEN           
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(2,0),TWAD                          
         BAS   RE,SETTBL          SET TRACE TABLE                               
         LA    R2,TRACTBL                                                       
         CLC   0(L'TRACTBL,R2),ACCURD                                           
         BH    VALRTERR           CAN'T BE BEFORE FIRST PROFILE                 
         LA    R1,1               FIRST PROFILE                                 
         SR    RF,RF                                                            
CHKPFK25 ICM   RF,3,0(R2)                                                       
         LA    RE,PROL2DH-PROL1DH                                               
         AR    RF,RE              RF = DISPLACEMENT TILL END OF CURRENT         
         CLM   RF,3,ACCURD        IF CURSOR BEFORE END OF LINE                  
         BNL   CHKPFK30           TRACE THAT LINE                               
         LA    R2,L'TRACTBL(R2)                                                 
         LA    R1,1(R1)                                                         
         ZIC   R0,PMAXNUM                                                       
         CR    R1,R0                                                            
         BH    VALRTERR           CAN'T BE GREATER THAN MAX PROFS               
         B     CHKPFK25                                                         
*                                                                               
CHKPFK30 STC   R1,PROWNUM         PROFILE NUMBER (PASS TO TRACE PGM)            
         OI    TWAFLAG,TWAFMAI    INDICATE SWAPPED FROM MAINT SCREEN            
         MVC   APPARM(1),INREC    KEEP RECORD THE SAME                          
         MVI   APPARM+1,ACTDIS    ACTION CODE TO SWAP TO                        
*                                                                               
CHKPFKX  B     EXIT                                                             
         EJECT                                                                  
*============================*                                                  
* DISREC - DISPLAYS RECORD   *                                                  
*============================*                                                  
*                                                                               
DISREC   DS    0H                                                               
         BAS   RE,DISPTBL         TABLE GENERATED - NOW DISPLAY IT              
         B     EXIT                                                             
         SPACE                                                                  
*================================================*                              
* DISPTBL - DISPLAY TABLE GENERATED BY ACPOSTER  *                              
*================================================*                              
*                                                                               
DISPTBL  NTR1                                                                   
         ZIC   R1,PMAXNUM         MAX NUMBER OF PROFILES                        
         USING ACPRTND,R3                                                       
         LA    R2,PROL1DH         PT TO FIRST LINE ON SCREEN                    
         USING DISD,R2                                                          
         L     R4,APTABLE                                                       
*                                                                               
DISR30   BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         XC    DVAL,DVAL                                                        
         CLI   0(R4),MTPFIPCT     IF IOR PERCENTAGE                             
         BE    DISR30A                                                          
         CLI   0(R4),MTPFSPCT     OR SPLIT PERCENTAGE                           
         BE    DISR30A                                                          
         CLI   0(R4),MTPFIPT2     OR AOR PERCENTAGE                             
         BE    DISR30A                                                          
         CLI   0(R4),MTPFPCT      TRADE PERCENTAGE                              
         BNE   DISR35                                                           
DISR30A  EDIT  (4,ACPFVAL),(7,DVAL),3,ALIGN=LEFT,ZERO=NOBLANK,         X        
               DUB=APDUB,WRK=APWORK                                             
         B     DISR55                                                           
*                                                                               
DISR35   CLI   0(R4),MTPFDMED                                                   
         BE    *+12                                                             
         CLI   0(R4),MTPFDACC                                                   
         BNE   DISR50                                                           
*                                                                               
         OC    ACPFVAL,SPACES                                                   
         LA    R0,L'ACPFVAL                                                     
         LA    RE,ACPFVAL                                                       
         LA    RF,DVAL                                                          
DISR40   MVC   0(2,RF),0(RE)                                                    
         LA    RE,2(RE)                                                         
         LA    RF,2(RF)                                                         
         CLC   0(2,RE),SPACES                                                   
         BE    DISR55                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         BCTR  R0,0               SUBTRACT 2                                    
         BCT   R0,DISR40                                                        
         B     DISR55                                                           
*                                                                               
DISR50   MVC   DVAL,ACPFVAL       MOVE VALUE TO SCREEN                          
DISR55   OI    DVALH+6,X'80'                                                    
         MVC   DLVL,ACPFLVL       MOVE LVL TO SCREEN                            
         OI    DLVLH+6,X'80'                                                    
         MVC   DLSTACT,ACPFACT    MOVE LAST ACTIVITY TO SCREEN                  
         OI    DLSTACTH+6,X'80'                                                 
*                                                                               
         LA    R4,27(R4)                                                        
         LA    R2,DISDL(R2)       PT TO NEXT SCREEN LINE                        
         BCT   R1,DISR30                                                        
         LA    R1,PROL1H                                                        
         STCM  R1,15,APCURSOR                                                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
GETROW   NTR1                                                                   
         ZIC   R3,0(R4)           ROW NUMBER                                    
         BCTR  R3,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL         LENGTH OF TABLE                               
         MR    RE,R3              GET TOTAL DISP FROM START                     
         L     R3,APSTTBL                                                       
         AR    R3,RF              R3=CORRECT ROW                                
         XIT1 REGS=(R3)                                                         
         EJECT                                                                  
*====================================*                                          
* DISKEY - DISPLAY RECORD SELECTED   *                                          
*====================================*                                          
*                                                                               
DISKEY   LA    R2,APRECKEY                                                      
         USING MPRRECD,R2                                                       
         MVC   PROSYS(L'MPRKSYS),MPRKSYS     DISPLAY SYSTEM                     
         MVC   PROSYS+1(L'MPRKALPH),MPRKALPH AGY ALPHA(MED SPLIT FILE)          
         OI    PROSYSH+6,X'80'                                                  
         MVC   PROMED,MPRKMED     DISPLAY MEDIA                                 
         OI    PROMEDH+6,X'80'                                                  
*                                                                               
         GOTO1 ADISOFF,APPARM,MPRKOFC,PROOFF                                    
         BNE   DISK20                                                           
         OI    PROOFFH+6,X'80'                                                  
DISK20   MVC   PROCLT,MPRKCLI     DISPLAY CLIENT                                
         OI    PROCLTH+6,X'80'                                                  
         MVC   PROPRD,MPRKPRD     DISPLAY PRODUCT                               
         OI    PROPRDH+6,X'80'                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
PPGRTBL  DC    C'NY'              PRINT PRODUCT TABLE                           
         DC    X'FF'                                                            
*                                                                               
VALTBL   DS    0D                                                               
         DC    AL1(MTPFDACC),AL1(0),AL1(2),AL1(0),AL4(CHKVALID)                 
         DC    AL1(MTPFDPST),AL1(1),AL1(1),AL1(0),AL4(CHKPST)                   
         DC    AL1(MTPFBILS),AL1(0),AL1(12),AL1(0),AL4(0)                       
         DC    AL1(MTPFSJ),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                    
         DC    AL1(MTPFSJ1C),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                  
         DC    AL1(MTPFDELM),AL1(1),AL1(1),AL1(0),AL4(CHKDELM)                  
         DC    AL1(MTPFMOSB),AL1(0),AL1(0),AL1(0),AL4(CHKMOS)                   
         DC    AL1(MTPFMOSF),AL1(0),AL1(0),AL1(0),AL4(CHKMOS)                   
         DC    AL1(MTPFPST),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                   
         DC    AL1(MTPFCC),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                    
         DC    AL1(MTPFDMED),AL1(0),AL1(2),AL1(0),AL4(CHKVALID)                 
         DC    AL1(MTPF3LDG),AL1(1),AL1(1),AL1(0),AL4(CHKLEDGR)                 
         DC    AL1(MTPFRCV),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                   
         DC    AL1(MTPFCST),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                   
         DC    AL1(MTPFWKSJ),AL1(2),AL1(2),AL1(0),AL4(CHKWKCD)                  
         DC    AL1(MTPFSUFX),AL1(0),AL1(5),AL1(0),AL4(0)                        
         DC    AL1(MTPFRBFX),AL1(0),AL1(5),AL1(0),AL4(0)                        
         DC    AL1(MTPFRCSJ),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                  
         DC    AL1(MTPFCLRC),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                  
         DC    AL1(MTPFPGR),AL1(1),AL1(1),AL1(0),AL4(CHKPGR)                    
         DC    AL1(MTPFNMEM),AL1(1),AL1(1),AL1(0),AL4(CHKYORN)                  
         DC    AL1(MTPFMGOV),AL1(0),AL1(4),AL1(0),AL4(0)                        
         DC    AL1(MTPFIPCT),AL1(3),AL1(7),AL1(0),AL4(CHKPCT)                   
         DC    AL1(MTPFIPT2),AL1(3),AL1(7),AL1(0),AL4(CHKPCT)                   
         DC    AL1(MTPFIOFC),AL1(1),AL1(2),AL1(0),AL4(CHKOFC)                   
         DC    AL1(MTPFOFF),AL1(1),AL1(2),AL1(0),AL4(CHKOFC)                    
         DC    AL1(MTPFMOFF),AL1(1),AL1(2),AL1(0),AL4(CHKOFC)                   
         DC    AL1(MTPFBAS),AL1(1),AL1(1),AL1(0),AL4(CHKBAS)                    
         DC    AL1(MTPFPCT),AL1(3),AL1(7),AL1(0),AL4(CHKPCT)                    
         DC    AL1(MTPFSPCT),AL1(3),AL1(7),AL1(0),AL4(CHKPCT)                   
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* LITERAL POOL                                                                  
***********************************************************************         
         LTORG                                                                  
*                                                                               
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
*                                                                               
* ACTRAWRK                                                                      
       ++INCLUDE ACTRAWRK                                                       
                                                                                
***********************************************************************         
* LOCAL WORKING STORAGE DSECT                                                   
***********************************************************************         
LOCALD   DSECT                                                                  
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
RECHANG  DS    CL1                                                              
NEWREC   DS    CL1                                                              
NEWELE   DS    CL1                                                              
ELETYPE  DS    XL1                ELEMENT TYPE                                  
LCHDT    DS    XL3                TODAYS BINARY DATE                            
TRACTBL  DS    (MAXPNUM)XL2                                                     
SVKEY    DS    CL44                                                             
BILLSCR  DS    CL12               BILL SOURCE/MEDIA NAME                        
BILLSLVL DS    CL(L'ACPLVL)       LVL FROM WHERE BILLSCR CAME                   
BILLSACT DS    CL(L'ACPFACT)      INITIALS AND CHANGE DATE                      
IORPCT   DS    XL4                IOR PERCENTAGE                                
IORPCTA  DS    XL4                IOR PERCENTAGE FOR AOR BILLS                  
TRDPCT   DS    XL4                TRADE PERCENTAGE                              
STRDPCT  DS    XL4                SPLIT TRADE PERCENTAGE                        
ACCLVL   DS    CL3                ACCOUNT LVL                                   
ACCOUNT  DS    CL14               ACCOUNT                                       
MEDTAB   DS    CL2                OVERRIDE MEDIA ALPHA AGENCY CODE              
ACCTAB   DS    CL2                OVERRIDE ACC ALPHA AGENCY CODE                
APWORK2  DS    CL(L'ACPFVAL)                                                    
         EJECT                                                                  
*VALTBLD - DSECT TO COVER VALTBL                                                
*                                                                               
VALTBLD  DSECT                                                                  
VALROW   DS    XL1                EQUATED ROW                                   
VALMINL  DS    XL1                MINIMUM LENGTH INPUT                          
VALMAXL  DS    XL1                MAXIMUM LENGTH INPUT                          
         DS    XL1                SPARE                                         
VALRTN   DS    XL4                VALIDATION ROUTINE                            
         SPACE 2                                                                
* DISD - DSECT TO COVER DISPLAY SCREEN                                          
*                                                                               
DISD     DSECT                                                                  
DDISH    DS    CL8                FOR HEADER                                    
DDIS     DS    CL26               DESCRIPTION                                   
DVALH    DS    CL8                FOR HEADER                                    
DVAL     DS    CL20               ACCOUNT                                       
DLVLH    DS    CL8                FOR HEADER                                    
DLVL     DS    CL3                ACCOUNT LEVEL                                 
DLSTACTH DS    CL8                FOR HEADER                                    
DLSTACT  DS    CL13               LAST ACTIVITY                                 
DISDL    EQU   *-DDISH            LENGTH OF ONE DISPLAY LINE                    
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFBD                                                       
         SPACE 2                                                                
         ORG                                                                    
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052ACTRA05   02/15/19'                                      
         END                                                                    
