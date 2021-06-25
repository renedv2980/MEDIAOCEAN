*          DATA SET ACREPMT02  AT LEVEL 005 AS OF 11/11/19                      
*PHASE ACMT02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PERVERT                                                                
ACMT02   TITLE 'Missing timesheet report'                                       
                                                                                
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 004 14FEB19 <SPEC-27726> INCLUDE APPROVER AND SUBMITTER EMAIL  *         
*                               ADDRESS ON MT report.                 *         
* VGUP 005 16SEP19 <SPEC-38563> ADDED NEW/OLD OFFICE CHECK FOR STATUS *         
***********************************************************************         
***********************************************************************         
* Request options:-                                                   *         
*                                                                     *         
* QOPT1        Time trigger (default=profile value)                   *         
* QOPT2        Terminated employees (default=N)                       *         
* QOPT3        Show overdue column (default=N)                        *         
* QOPT4        Print delinquent approvals section (default=N)         *         
* QOPT6        Print rejected people (default=N,only=O)               *         
* QOPT7        Download format (download=Y)                           *         
*                                                                     *         
* QCOMMENT     TXXYY (T=P(Period)/D(Daily),XX=Tdays,YY=Adays)         *         
*              to override profile values                             *         
*                                                                     *         
* Request can be made for any level of 1R account or for an employee  *         
* id.  When employee id is provided the application is presented      *         
* with an account for each occurrence of a LOCEL on the person        *         
* record (PROCACC/(PROCTIME)/ACCLAST) - time for these people will    *         
* appear in different sub-departments on the report                   *         
***********************************************************************         
                                                                                
ACMT02   CSECT ,                                                                
         PRINT NOGEN                                                            
         NMOD1 WORKL,**MT02**,CLEAR=Y                                           
                                                                                
         USING WORKD,RC            RC=A(local working storage)                  
         L     R9,0(R1)                                                         
         USING ACWORKD,R9          R9=A(ACWORKD)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
         L     R8,AMONACC                                                       
         USING ACMD,R8             R8=A(ACMASTC)                                
                                                                                
         LLC   RE,MODE             Get calling mode into RE                     
         LLC   RE,MODETAB(RE)      Get jump index into RE                       
         LA    RE,MODEJUMP(RE)     Point to jump instruction                    
         BR    RE                  Go there                                     
                                                                                
MODEJUMP DS    0XL4                ** Mode jump index **                        
                                                                                
EXITJUMP J     EXIT                Unsupported modes                            
FRUNJUMP J     FRUN                First for run                                
PREQJUMP J     PREQ                Process a new request                        
FLDGJUMP J     FLDG                First for (1R) ledger                        
FOFFJUMP J     FOFF                First for office                             
FDEPJUMP J     FDEP                First for department                         
FSUBJUMP J     FSUB                First for sub-department                     
PPERJUMP J     PPER                Process a person account record              
PTIMJUMP J     PTIM                Process a time record                        
LPERJUMP J     LPER                Last for a person account                    
LSUBJUMP J     LSUB                Last for sub-department                      
LREQJUMP J     LREQ                Last for request                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* First for run                                                       *         
***********************************************************************         
                                                                                
FRUN     MVI   FCRESET,YESQ        This module changes read sequence            
         GOTOR DATCON,DMCB,(5,0),(1,TODAY1)                                     
         GOTOR DATCON,DMCB,(5,0),(0,TODAY0)                                     
         GOTOR LOADCR,QGETCAP      Load GETCAP into memory                      
         MVC   GETCAP,DMCB+4       Set its address                              
         GOTOR LOADCR,QTSAR        Load TSAR into memory                        
         MVC   TSAR,DMCB+4         Set its address                              
         GOTOR LOADCR,QEDITOR      Load EDITOR into memory                      
         MVC   EDITOR,DMCB+4       Set its address                              
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Process new request                                                 *         
***********************************************************************         
                                                                                
PREQ     LA    R0,1                                                             
         STCM  R0,3,PAGE           Set page 1                                   
         MVC   HEADHOOK,NULLS      Set no headline hook required                
         MVI   RCSUBPRG,FF         Set invalid subprogram                       
         L     R1,ADBXAREA                                                      
         USING BOXD,R1             Turn box printing off                        
         MVI   BOXYORN,NOQ                                                      
         MVI   BOXOFF,YESQ                                                      
         MVC   REQSTR,NULLS        Initialize request start date                
         MVC   REQEND,NULLS        Initialize request end date                  
         J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* First for (1R) ledger                                               *         
***********************************************************************         
                                                                                
FLDG     GOTOR GETNAM,DMCB,ADCMPNAM,CPYNAME                                     
                                                                                
         CLI   QOPT2,C' '          Apply option defaults                        
         JNE   *+8                                                              
         MVI   QOPT2,NOQ           Exclude terminated employees                 
                                                                                
         CLI   QOPT3,C' '                                                       
         JNE   *+8                                                              
         MVI   QOPT3,NOQ           Don't show overdue                           
                                                                                
         CLI   QOPT4,C' '                                                       
         JNE   *+8                                                              
         MVI   QOPT4,NOQ           Don't print approvals section                
                                                                                
         MVI   RSUBDAYS,0          Initialize request submit days               
         CLC   QCOMMENT+1(2),SPACES                                             
         JE    FLDG0010                                                         
         PACK  DUB,QCOMMENT+1(2)                                                
         CVB   R0,DUB                                                           
         STC   R0,RSUBDAYS         Set request override if present              
         MVC   RSUBTYPE,QCOMMENT   Set submit period/day option                 
                                                                                
FLDG0010 MVI   RAPPDAYS,0          Initialize request approval days             
         CLC   QCOMMENT+3(2),SPACES                                             
         JE    FLDG0020                                                         
         PACK  DUB,QCOMMENT+3(2)                                                
         CVB   R0,DUB                                                           
         STC   R0,RAPPDAYS         Set request override if present              
                                                                                
FLDG0020 CLC   QSTART,SPACES       Test QSTART given                            
         JE    FLDG0030                                                         
         GOTOR DATCON,DMCB,(0,QSTART),(1,REQSTR)                                
                                                                                
FLDG0030 CLC   QEND,SPACES         Test QEND given                              
         JE    FLDG0040                                                         
         GOTOR DATCON,DMCB,(0,QEND),(1,REQEND)                                  
                                                                                
FLDG0040 CLC   REQSTR,NULLS        Test start date provided                     
         JNE   FLDG0050                                                         
         MVC   REQSTR(1),TODAY1    Start at calendar start                      
                                                                                
FLDG0050 CLC   REQEND,NULLS        Test end date provided                       
         JNE   FLDG0060                                                         
         MVC   REQEND,TODAY1       No - today is the default                    
                                                                                
FLDG0060 CLI   QOPT4,NOQ           Test don't want approvals                    
         JE    FLDG0160            Yes                                          
                                                                                
         CLI   QOPT6,ONLYQ         Test want rejects only                       
         JE    FLDG0160            Yes                                          
                                                                                
APPS     USING TSARD,TSARAPPS      Initialize approvals buffer                  
         MVI   APPS.TSACTN,TSAINI                                               
         MVI   APPS.TSRECI,TSRXTN                                               
         MVI   APPS.TSKEYL,AKEYLEN                                              
         LHI   R0,ARECLEN                                                       
         STH   R0,APPS.TSRECL                                                   
         MVC   APPS.TSACOM,ADCOMFAC                                             
         LHI   R0,2*ONEK           Acquire 2MB for TSAR buffer                  
         STCM  R0,3,APPS.TSBUFFL                                                
         LA    R0,ARECORD                                                       
         ST    R0,APPS.TSAREC                                                   
         GOTOR TSAR,APPS.TSARD     Initialize TSAR buffer                       
                                                                                
***********************************************************************         
* Read through delinquent approvals pointers and post delinquent      *         
* approvals to look-up buffer by approver 1R account code             *         
***********************************************************************         
                                                                                
K        USING TAPPASD,IOKEY                                                    
R        USING TIMRECD,IO                                                       
                                                                                
         MVC   K.TAPPAS,NULLS                                                   
         MVI   K.TAPPTYP,TAPPTYPQ  Build key of approver passive                
         MVI   K.TAPPSUB,TAPPSUBQ                                               
         MVC   K.TAPPCPY,QCOMPANY                                               
         MVI   K.TAPPKYST,TAPSAWPQ Set approval status type                     
         MVC   TAPKEY,K.TAPPAS                                                  
         GOTOR DATAMGR,DMCB,DMRDHI,ACMACDIR,K.TAPPAS,K.TAPPAS                   
         JE    FLDG0080                                                         
         DC    H'0'                                                             
                                                                                
FLDG0070 GOTOR DATAMGR,DMCB,DMRSEQ,ACMACDIR,K.TAPPAS,K.TAPPAS                   
         JE    FLDG0080                                                         
         DC    H'0'                                                             
                                                                                
FLDG0080 CLC   K.TAPPAS(TAPPCAT-TAPPAS),TAPKEY                                  
         JNE   FLDG0160                                                         
                                                                                
         TM    K.TAPPSTAT,TIMSPAPP Test part-approved                           
         JNZ   *+12                Yes - include                                
         TM    K.TAPPSTAT,TIMSSUBM Test submitted status                        
         JZ    FLDG0070            No - ignore                                  
                                                                                
         MVC   TAPKEY,K.TAPPAS     Save current TAPPAS key for re-read          
                                                                                
         GOTOR DATAMGR,DMCB,ACMDMGET,ACMACMST,K.TAPPDA,R.TIMRECD,DMWORK         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   ASUBACT,R.TIMKACT   Set 1R account value                         
         MVC   ASUBEDT,R.TIMKPEDT  Set period end date                          
         MVC   ASUBSTA,R.TIMRSTAT  Set time status                              
         MVC   ASUBSUB,NULLS       Set not submitted                            
                                                                                
         LA    R1,R.TIMRFST        Locate submitted date element                
         USING GDAELD,R1                                                        
         SR    R0,R0                                                            
FLDG0100 CLI   GDAEL,0             Test for end of record                       
         JE    FLDG0130                                                         
         CLI   GDAEL,GDAELQ        Test general date element                    
         JNE   FLDG0110                                                         
         CLI   GDATYPE,GDALMSUB    Test line manager submitted date             
         JE    FLDG0120                                                         
FLDG0110 IC    R0,GDALN            Bump to next element on record               
         AR    R1,R0                                                            
         J     FLDG0100                                                         
FLDG0120 MVC   ASUBSUB,GDADATE     Set line manager submitted date              
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Read submitter person record                                        *         
***********************************************************************         
                                                                                
FLDG0130 L     R1,ADLDGHIR                                                      
         LLC   RF,ACLVALS+(L'ACLVALS*3)-ACLELD(R1)                              
         LLC   R0,ACLVALS+(L'ACLVALS*2)-ACLELD(R1)                              
         SR    RF,R0               RF=length of person code in key              
         ICM   RF,8,SPACES         Set pad character                            
         LA    RE,ASUBACT                                                       
         AR    RE,R0               Point to input person code                   
         LA    R0,ASUBCODE         Point to output person code                  
         LA    R1,L'ASUBCODE                                                    
         MVCL  R0,RE               Extract person code                          
                                                                                
         GOTOR GETPER,ASUBCODE     Read submitter person record                 
                                                                                
         CLI   QOPT2,YESQ          Test including terminated employees          
         JE    FLDG0134            Yes - include all employees                  
         CLI   QOPT2,NOQ           Test excluding terminated employees          
         JNE   FLDG0132                                                         
         CLI   PERSTAT,EMPCTRM     Test person is terminated                    
         JE    FLDG0150            Yes - exclude                                
         J     FLDG0134                                                         
                                                                                
FLDG0132 CLI   QOPT2,ONLYQ         Test only terminated employees               
         JNE   FLDG0134                                                         
         CLI   PERSTAT,EMPCTRM     Yes - test person is terminated              
         JNE   FLDG0150            No - exclude                                 
                                                                                
FLDG0134 MVC   ASUBFST,PERFRST     Set submitter first name                     
         MVC   ASUBLST,PERLAST     Set submitter last name                      
                                                                                
***********************************************************************         
* Establish approver and build approval buffer record                 *         
***********************************************************************         
                                                                                
FLDG0140 GOTOR GETAPP,ASUBACT      Get approver PID#                            
         JNE   FLDG0150                                                         
         MVC   APPPID#,WORK        Save the PID number of approver              
         GOTOR GETPID,APPPID#      Resolve approver values                      
         JNE   FLDG0150                                                         
         MVC   AAPPACT,PERACT      Set approver 1R account code                 
                                                                                
         L     R1,ADLDGHIR                                                      
         USING ACLELD,R1           R1=A(ledger hierarchy element)               
         LLC   RF,ACLVALS+(L'ACLVALS*3)                                         
         LLC   RE,ACLVALS+(L'ACLVALS*2)                                         
         DROP  R1                                                               
         SR    RF,RE               RF=length of key value                       
         LA    RE,AAPPACT(RE)      RE=A(start of person code)                   
         LA    R0,AAPPCODE                                                      
         LA    R1,L'AAPPCODE       Set length of destination                    
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               Set space padded person code                 
                                                                                
         L     R1,ADQSTACK                                                      
         USING ACQD,R1                                                          
         CLI   ACQTYP1,ACQPRSN     Test person code in request                  
         JNE   FLDG0142                                                         
         CLC   AAPPCODE,ACQFLT1    Match person code in request                 
         JNE   FLDG0150                                                         
         DROP  R1                                                               
                                                                                
FLDG0142 MVI   APPS.TSACTN,TSAADD  Add record to approval buffer                
         GOTOR TSAR,APPS.TSARD                                                  
         TM    APPS.TSERRS,TSEEOF  Note - duplicates are allowed                
         JZ    FLDG0150            as there may be several pointers for         
         DC    H'0'                the same period                              
                                                                                
K        USING TAPPASD,IOKEY                                                    
FLDG0150 GOTOR DATAMGR,DMCB,DMREAD,ACMACDIR,TAPKEY,K.TAPPASD                    
         JE    FLDG0070            Get next TAP key                             
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Set printing options                                                *         
***********************************************************************         
                                                                                
FLDG0160 CLI   QOPT7,YESQ          Test override download option set            
         JE    FLDG0170                                                         
         L     RF,REMOTEC          Set headline hook if necessary               
         USING REMOTED,RF                                                       
         CLC   REMOTKEY,NULLS      Test output to print queue                   
         JE    FLDG0180            No                                           
         TM    REMOTTYP,REMOTDLQ   Test downloading                             
         JZ    FLDG0180            No                                           
         DROP  RF                                                               
                                                                                
***********************************************************************         
* Initialize for downloading                                          *         
***********************************************************************         
                                                                                
FLDG0170 GOTOR DOWNIT,DOWNINI      Initialize downloading                       
                                                                                
         MVI   DH2ODU,0            No-op/op overdue column headings             
         MVI   DH1ODU,0                                                         
         CLI   QOPT3,NOQ           Test suppressing overdue column              
         JE    FLDG0172            Yes                                          
         MVI   DH2ODU,DH2ODUX-DH2ODU                                            
         MVI   DH1ODU,DH1ODUX-DH1ODU                                            
                                                                                
FLDG0172 GOTOR DLCH,DH1TAB         Download column headings 1                   
         GOTOR DLCH,DH2TAB         Download column headings 2                   
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Initialize for report printing                                      *         
***********************************************************************         
                                                                                
FLDG0180 LARL  RF,HOOK             Point to headline hook routine               
         ST    RF,HEADHOOK                                                      
         MVI   RCSUBPRG,1          Set subprogram for headline printing         
         CLI   QOPT4,ONLYQ         Test awaiting approval only                  
         JNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT3,ONLYQ         Test overdue only                            
         JNE   FLDG0190                                                         
         MVI   RCSUBPRG,3                                                       
         CLC   QOPT4,QOPT3         Test awaiting approval and overdue           
         JNE   FLDG0190                                                         
         MVI   RCSUBPRG,4                                                       
                                                                                
FLDG0190 L     R1,ADBXAREA                                                      
         USING BOXD,R1             Initialize box printing                      
         MVI   BOXYORN,YESQ                                                     
         MVI   BOXOFF,NOQ                                                       
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+07,C'T'                                                  
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+(PLBOXL-PLINE),C'L'                                      
         MVI   BOXCOLS+(PLBOXC1-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC2-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC3-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC4-PLINE),C'C'                                     
         CLI   QOPT3,NOQ           Test want to see overdue column              
         JE    *+8                 No                                           
         MVI   BOXCOLS+(PLBOXC5-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXR-PLINE),C'R'                                      
         OI    BOXDDCTL,BOXDDUL+BOXDDLC                                         
         MVI   RCFLAG1,RCFREQLC+RCFREPLC                                        
         J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* First for office (level 1 account)                                  *         
***********************************************************************         
                                                                                
FOFF     GOTOR GETNAM,DMCB,ADLVANAM,OFFNAME                                     
                                                                                
         LARL  R0,OFFCAL                                                        
         ST    R0,ACALTAB          Point to office calendar for GETCAL          
         L     R1,ADACC            Point to office account record               
         GOTOR GETCAL,ACTKACT-ACTRECD(R1)                                       
         MVC   OCALSTR,CALSTART    Save office level calendar start             
         MVC   OCALEND,CALEND      Save office level calendar end               
                                                                                
         MVC   CALDATES,SPACES     Format calendar dates for printing           
         LA    R3,CALDATES         in the headline hook routine (HOOK)          
         LARL  R2,OFFCAL                                                        
         USING CALTABD,R2          R2=A(calendar table)                         
*                                  Format first entry                           
         LLC   R0,CALTNUM                                                       
         GOTOR EDITNUM                                                          
         MVC   0(2,R3),WORK                                                     
         LA    R3,1(R3)                                                         
         CLI   0(R3),C' '                                                       
         JE    *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'='                                                       
         GOTOR DATCON,DMCB,(1,CALTSTR1),(10,1(R3))                              
         OI    1(R3),C'0'                                                       
         LA    R3,9(R3)                                                         
         MVI   0(R3),C'-'                                                       
         GOTOR DATCON,DMCB,(1,CALTEND1),(10,1(R3))                              
         LA    R3,9(R3)                                                         
                                                                                
         CLI   CALTABD+CALTABLN,CALTEOTQ                                        
         JE    EXIT                                                             
         AHI   R2,CALTABLN         Locate last calendar entry                   
FOFF0020 CLI   CALTABD+CALTABLN,CALTEOTQ                                        
         JE    FOFF0030                                                         
         AHI   R2,CALTABLN         Bump to next calendar entry                  
         J     FOFF0020                                                         
                                                                                
FOFF0030 MVI   0(R3),C','          Format last entry                            
         LA    R3,1(R3)                                                         
         LLC   R0,CALTNUM                                                       
         GOTOR EDITNUM                                                          
         MVC   0(2,R3),WORK                                                     
         LA    R3,1(R3)                                                         
         CLI   0(R3),C' '                                                       
         JE    *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'='                                                       
         GOTOR DATCON,DMCB,(1,CALTSTR1),(10,1(R3))                              
         OI    1(R3),C'0'                                                       
         LA    R3,9(R3)                                                         
         MVI   0(R3),C'-'                                                       
         GOTOR DATCON,DMCB,(1,CALTEND1),(10,1(R3))                              
         OI    1(R3),C'0'                                                       
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* First for department (level 2 account)                              *         
***********************************************************************         
                                                                                
FDEP     GOTOR GETNAM,DMCB,ADLVBNAM,DEPNAME                                     
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* First for sub-department (level 3 account)                          *         
***********************************************************************         
                                                                                
FSUB     GOTOR GETNAM,DMCB,ADLVCNAM,SUBNAME                                     
                                                                                
TIME     USING TSARD,TSARTIME      Initialize sort file                         
         MVI   TIME.TSACTN,TSAINI                                               
         MVI   TIME.TSRECI,TSRXTN                                               
         MVI   TIME.TSKEYL,SKEYLEN                                              
         LHI   R0,SRECLEN                                                       
         STH   R0,TIME.TSRECL                                                   
         MVC   TIME.TSACOM,ADCOMFAC                                             
         LHI   R0,2*ONEK           Acquire 2MB for TSAR buffer                  
         STCM  R0,3,TIME.TSBUFFL                                                
         LA    R0,SRECORD                                                       
         ST    R0,TIME.TSAREC                                                   
         GOTOR TSAR,TIME.TSARD     Initialize TSAR buffer                       
         JE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Process person account record (level 4 account)                     *         
***********************************************************************         
                                                                                
PPER     GOTOR CLRHRS              Clear time data in calendar                  
         MVC   SUBVALS(SUBVALL),NULLS                                           
                                                                                
         L     R1,ADACC            Extract 1R account code                      
         MVC   SUBACT,ACTKACT-ACTRECD(R1)                                       
                                                                                
         L     R2,ADLDGHIR                                                      
         USING ACLELD,R2           R2=A(1R ledger element)                      
                                                                                
         LLC   RF,ACLVALS+(L'ACLVALS*0)                                         
         LA    RE,SUBACT           Point to office in key                       
         LA    R3,0(RF,RE)         Point to department in key                   
         ICM   RF,8,SPACES         Set pad character                            
         LA    R0,OFFICE           Set destination                              
         LA    R1,L'OFFICE         and its length                               
         MVCL  R0,RE               Set office code                              
                                                                                
         LLC   RF,ACLVALS+(L'ACLVALS*1)                                         
         LLC   RE,ACLVALS+(L'ACLVALS*0)                                         
         SR    RF,RE               R1=length of department                      
         LR    RE,R3               Point to department in key                   
         AR    R3,RF               Point to sub-department in key               
         ICM   RF,8,SPACES         Set pad character                            
         LA    R0,DEPT                                                          
         LA    R1,L'DEPT                                                        
         MVCL  R0,RE               Set department code                          
                                                                                
         LLC   RF,ACLVALS+(L'ACLVALS*2)                                         
         LLC   RE,ACLVALS+(L'ACLVALS*1)                                         
         SR    RF,RE               RF=length of department                      
         LR    RE,R3               Point to sub-department in key               
         AR    R3,RF               Point to person code in key                  
         ICM   RF,8,SPACES         Set pad character                            
         LA    R0,SUBDEPT                                                       
         LA    R1,L'SUBDEPT                                                     
         MVCL  R0,RE               Set sub-department code                      
                                                                                
         LLC   RF,ACLVALS+(L'ACLVALS*3)                                         
         LLC   R0,ACLVALS+(L'ACLVALS*2)                                         
         SR    RF,R0               RF=length of department                      
         ICM   RF,8,SPACES         Set pad character                            
         LR    RE,R3               Point to person code                         
         LA    R0,SUBCODE                                                       
         LA    R1,L'SUBCODE                                                     
         MVCL  R0,RE               Set person code                              
                                                                                
K        USING PERKEY,IOKEY        Read submitter person record                 
R        USING PERKEY,IO                                                        
         MVC   K.PERKEY,SPACES                                                  
         MVI   K.PERKTYP,PERKTYPQ                                               
         MVC   K.PERKCPY,QCOMPANY                                               
         MVC   K.PERKCODE,SUBCODE                                               
                                                                                
         MVI   WHYREJ,1            Set can't read person record                 
         GOTOR DATAMGR,DMCB,DMREAD,ACMACDIR,K.PERKEY,K.PERKEY                   
         JNE   PPER0210                                                         
         GOTOR DATAMGR,DMCB,ACMDMGET,ACMACMST,K.PERKDA,R.PERKEY,DMWORK          
         JNE   PPER0210                                                         
                                                                                
         LA    R2,R.PERRFST        R2=A(first element on person record)         
                                                                                
PPER0010 CLI   0(R2),0             Test end of record                           
         JE    PPER0050                                                         
                                                                                
         USING LOCELD,R2                                                        
         CLI   LOCEL,LOCELQ        Test location element                        
         JNE   PPER0020                                                         
         CLC   LOCOFF,OFFICE       Test for same office...                      
         JNE   PPER0040                                                         
         CLC   LOCDEPT,DEPT        ...and department                            
         JNE   PPER0040                                                         
         CLC   LOCSUB,SUBDEPT      ...and sub-department                        
         JNE   PPER0040                                                         
         MVC   SUBSTRDT,LOCSTART   Set start date at location                   
         MVC   SUBENDDT,LOCEND     Set end date at location                     
         GOTOR SETCAL              Set on required weeks                        
         J     PPER0040                                                         
                                                                                
         USING EMPELD,R2                                                        
PPER0020 CLI   EMPEL,EMPELQ        Test employee element                        
         JNE   PPER0030                                                         
         MVC   SUBSTAT,EMPCSTAT    Set person status                            
         J     PPER0040                                                         
                                                                                
         USING PIDELD,R2                                                        
PPER0030 CLI   PIDEL,PIDELQ        Test person PID element                      
         JNE   PPER0040                                                         
         MVC   SUBPID#,PIDNO       Set person PID                               
                                                                                
PPER0040 LLC   R0,1(R2)            Bump to next element                         
         AR    R2,R0                                                            
         J     PPER0010            and process it                               
         DROP  R2                                                               
                                                                                
PPER0050 MVI   WHYREJ,2            No location start date                       
         CLC   SUBSTRDT,NULLS      Test start date for location set             
         JE    PPER0210            no good                                      
                                                                                
         MVI   WHYREJ,3            Person not in period                         
         CLC   SUBENDDT,NULLS      Test end date present                        
         JE    *+14                no                                           
         CLC   SUBENDDT,OCALSTR    Test ended before calendar start             
         JNH   PPER0210                                                         
                                                                                
         MVI   WHYREJ,4            Missing PID                                  
         CLC   SUBPID#,NULLS       Test we have a PID                           
         JE    PPER0210            no good                                      
                                                                                
         MVI   WHYREJ,5            Invalid security PID                         
         GOTOR GETSEC,SUBPID#      Get person security PID                      
         JNE   PPER0210            no good                                      
         MVC   SUBPID,WORK         Set person security PID                      
                                                                                
         MVI   WHYREJ,6            Invalid PID                                  
         GOTOR GETPID,SUBPID#      Get person PID record                        
         JNE   PPER0210            no good                                      
         MVC   SUBFRST,PERFRST     Set person first name                        
         MVC   SUBLAST,PERLAST     Set person last name                         
                                                                                
         MVC   APPCODE,SPACES                                                   
         MVI   APPCODE,C'?'                                                     
         MVC   APPLAST,SPACES                                                   
         MVC   APPLAST(7),=C'Unknown'                                           
         MVC   APPFRST,SPACES                                                   
         MVC   APPPID,SPACES                                                    
         MVI   APPPID,C'?'                                                      
         MVC   APPPID#,NULLS                                                    
         GOTOR GETAPP,SUBACT       Establish approver                           
         JNE   PPER0052            no good                                      
                                                                                
         MVI   WHYREJ,7            Invalid approver PID                         
         MVC   APPPID#,WORK        Set approver PID number                      
         GOTOR GETPID,APPPID#      Get approver name                            
         JNE   PPER0210                                                         
         MVC   APPCODE,PERCODE     Set approver person code                     
         MVC   APPFRST,PERFRST     Set approver first name                      
         MVC   APPLAST,PERLAST     Set approver last name                       
                                                                                
         MVI   WHYREJ,8            Invalid approver security PID                
         GOTOR GETSEC,APPPID#      Get approver security PID                    
         JNE   PPER0210            no good                                      
         MVC   APPPID,WORK         Set approver security PID                    
                                                                                
***********************************************************************         
* Call GETCAP to get cost system options                              *         
***********************************************************************         
                                                                                
PPER0052 MVI   WHYREJ,9            Can't resolve costing options                
         LARL  R2,COBLOCKW         Get cost system options                      
         USING COBLOCKD,R2         R2=A(GETCAP control block)                   
         LA    R0,COBLOCK          Clear the control block                      
         LHI   R1,COBLOCKX-COBLOCK                                              
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   COADM,DATAMGR       Initialize parameter values                  
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,QCOMPANY                                                  
         MVC   COKOFC,OFFICE                                                    
         MVC   COKDPT,DEPT                                                      
         MVC   COKSDT,SUBDEPT                                                   
         MVC   COKPER,SUBCODE                                                   
         GOTOR GETCAP,DMCB,COBLOCKD                                             
         CLI   COSTATUS,0          Test good look-up                            
         JNE   PPER0210            no good                                      
                                                                                
         MVI   WHYREJ,10           Person terminated                            
         CLI   QOPT2,YESQ          Test include terminated employees            
         JE    PPER0062                                                         
                                                                                
         CLI   SUBSTAT,EMPCTRM     Test person is terminated                    
         JNE   PPER0060                                                         
         CLI   QOPT2,NOQ           Test exclude terminated employees            
         JE    PPER0220                                                         
         CLI   QOPT2,ONLYQ         Yes - test include terminated only           
         JE    PPER0062            Yes - keep this one                          
         J     PPER0220            Else discard                                 
                                                                                
PPER0060 MVI   WHYREJ,11           Person isn't terminated                      
         CLI   QOPT2,ONLYQ         Test include terminated only                 
         JE    PPER0220            Yes - exit                                   
                                                                                
PPER0062 MVI   WHYREJ,12           Set skipped                                  
         CLI   QOPT6,ONLYQ         Test want rejects only                       
         JE    PPER0220            Yes - exit                                   
                                                                                
***********************************************************************         
* Set time print trigger                                              *         
***********************************************************************         
                                                                                
PPER0070 MVI   WHYREJ,0            Set person not rejected                      
                                                                                
         MVC   TIMETRIG,COMTS      Default is the profile                       
         CLI   COMTS,0             Test have time trigger                       
         JE    PPER0080            No                                           
         OI    TIMETRIG,X'F0'      Convert to my internal format                
                                                                                
PPER0080 CLI   QOPT1,C' '          Test time trigger in request                 
         JE    PPER0110            No                                           
                                                                                
         LA    R1,STATAB           Convert via display character                
         USING STATABD,R1          Point to status table                        
         LA    R0,STATABN          R0=table count                               
PPER0090 CLC   QOPT1,STATCHAR      Match on printable character                 
         JE    PPER0100            Yes                                          
         AHI   R1,STATABL          Bump to next table entry                     
         JCT   R0,PPER0090                                                      
         DC    H'0'                Invalid request option                       
PPER0100 MVC   TIMETRIG,STATTIM    Set time trigger                             
         DROP  R1                                                               
                                                                                
PPER0110 CLI   TIMETRIG,CALTTNST   Test we have a time trigger                  
         JNL   *+8                                                              
         MVI   TIMETRIG,C'9'       No - set high value                          
                                                                                
***********************************************************************         
* Calculate latest submit date for daily time option                  *         
***********************************************************************         
                                                                                
         SR    R0,R0                                                            
         ICM   R0,1,RSUBDAYS       Get/test request override present            
         JNZ   PPER0130            Yes - use it                                 
                                                                                
         CLC   CONCM,SPACES        Test daily time profile set                  
         JE    PPER0120            No                                           
         ZAP   DUB,CONCM                                                        
         CVB   R0,DUB                                                           
         MVI   RSUBTYPE,RSUBTYPD   Set daily time profile value                 
         LTR   R0,R0                                                            
         JNZ   PPER0130                                                         
                                                                                
***********************************************************************         
* Calculate latest submit date for period time option                 *         
***********************************************************************         
                                                                                
PPER0120 CLC   CONDO,SPACES        Test time profile set                        
         JE    PPER0130            No                                           
         ZAP   DUB,CONDO                                                        
         CVB   R0,DUB                                                           
         MVI   RSUBTYPE,RSUBTYPP   Set period time profile value                
                                                                                
***********************************************************************         
* Calculate latest submit date for for either submit option           *         
***********************************************************************         
                                                                                
PPER0130 MVC   SUBOVER0,SPACES                                                  
         LTR   R0,R0               Test we have delinquent time days            
         JZ    PPER0140            Yes - calculate earliest                     
         STC   R0,RSUBDAYS                                                      
         LCR   R0,R0               Calculate lowest overdue date                
         GOTOR ADDAY,DMCB,TODAY0,SUBOVER0,(R0)                                  
                                                                                
***********************************************************************         
* Calculate latest approval date                                      *         
***********************************************************************         
                                                                                
PPER0140 SR    R0,R0                                                            
         ICM   R0,1,RAPPDAYS       Get/test request override present            
         JNZ   PPER0190            Yes - use it                                 
                                                                                
         CLC   CONCA,SPACES        Test NCA profile set                         
         JE    PPER0150            No                                           
         ZAP   DUB,CONCA                                                        
         CVB   R0,DUB                                                           
         J     PPER0180                                                         
                                                                                
PPER0150 CLC   CONCM,SPACES        Test NCM profile set                         
         JE    PPER0160                                                         
         ZAP   DUB,CONCM                                                        
         J     PPER0180                                                         
                                                                                
PPER0160 CLC   CONDO,SPACES        Test NDO profile set                         
         JE    PPER0170                                                         
         ZAP   DUB,CONDO                                                        
         J     PPER0180                                                         
                                                                                
PPER0170 SR    R0,R0               No profile found                             
         J     PPER0190                                                         
                                                                                
PPER0180 CVB   R0,DUB                                                           
         STC   R0,RAPPDAYS         Set profile value for approval days          
                                                                                
PPER0190 MVC   APPOVER1,NULLS      Set no approval date                         
         LTR   R0,R0               Test delinquent approval days set            
         JZ    PPER0200                                                         
         LCR   R0,R0               Calculate lowest overdue date                
         GOTOR ADDAY,DMCB,TODAY0,WORK,(R0)                                      
         GOTOR DATCON,DMCB,(0,WORK),(1,APPOVER1)                                
                                                                                
***********************************************************************         
* Set time reading options for ACMASTER                               *         
***********************************************************************         
                                                                                
PPER0200 MVI   ACMLMACC,ACCLAST    Set ACCLAST mode required                    
         MVI   FCRDTIME,NOQ        Preset to not read time records              
         CLI   QOPT4,ONLYQ         Test only want approval report               
         JE    *+8                 Yes - don't read time records                
         MVI   FCRDTIME,YESQ       Set to read time records                     
         MVC   ACMTSTR,OCALSTR     Set transaction start date                   
         MVC   ACMTEND,OCALEND     Set transaction end date                     
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Handle rejected people                                              *         
***********************************************************************         
                                                                                
PPER0210 CLI   QOPT6,ONLYQ         Test want only rejected people               
         JE    *+12                                                             
         CLI   QOPT6,YESQ          Test want to see rejected people             
         JNE   PPER0220                                                         
         MVC   P+1(8),=C'Rejected' Print a trace line for rejects               
         MVC   P+12(L'SUBACT),SUBACT                                            
         MVC   P+12+L'SUBACT+1(6),=C'reason'                                    
         LLC   R0,WHYREJ           Edit reason number                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  P+12+L'SUBACT+8(3),DUB                                           
         GOTOR ACREPORT            Print reject trace line                      
                                                                                
PPER0220 MVI   FCRDTIME,NOQ        Don't read time if rejected                  
         MVI   ACMLMACC,ACCLAST    Set ACCLAST mode required                    
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Process a time record                                               *         
***********************************************************************         
                                                                                
PTIM     L     R2,ACMABUF                                                       
         USING TIMRECD,R2          R2=A(time record)                            
         LARL  R3,OFFCAL           Locate calendar entry for time               
         USING CALTABD,R3          R3=A(calendar table entry)                   
PTIM0010 CLI   CALTABD,CALTEOTQ    Test end of table                            
         JNE   *+6                                                              
         DC    H'0'                Bad time record/calendar                     
         CLC   CALTEND1,TIMKPEDT   Match period end date to calendar            
         JE    PTIM0020                                                         
         AHI   R3,CALTABLN         Bump to next table entry                     
         J     PTIM0010                                                         
                                                                                
PTIM0020 CLI   CALTTIM,CALTTNST    Test not started                             
         JNE   PTIM0030                                                         
         MVC   CALTTIM,TIMRSTAT                                                 
         TM    ACMINDS,ACMINEWO    New office?                                  
         JO    *+10                                                             
         MVC   CALTTIM,TIMKSTAT                                                 
         GOTOR SETSTA,CALTTIM      Convert time status to character             
                                                                                
***********************************************************************         
* Process time element - post hours to daily time slots               *         
***********************************************************************         
                                                                                
PTIM0030 L     R2,ADTRANS          Process time elements                        
         USING TIMELD,R2           R2=A(time element)                           
                                                                                
PTIM0040 CLI   TIMEL,0             Test end of record                           
         JE    PTIM0090                                                         
         CLI   TIMEL,TIMELQ        Test time element                            
         JNE   PTIM0080                                                         
         CLI   TIMETYP,TIMETIME    Test time type element                       
         JNE   PTIM0080                                                         
                                                                                
         LLC   RF,TIMLN                                                         
         LA    RF,TIMELD(RF)                                                    
         ST    RF,FULL             Set address of end of element                
                                                                                
         LA    R4,TIMEDAY          Process date/hours list                      
         USING TIMEDAY,R4          R4=A(date/hours list)                        
                                                                                
PTIM0050 CLC   TIMEHRS,NULLS       Test empty hours bucket                      
         JE    PTIM0070                                                         
         AP    CALTTHRS,TIMEHRS    Add to total hours for period                
         SR    R1,R1               Set index to 0 for calendar day 1            
         CLC   TIMETDTE,CALTSTR1   Test for first day of calendar               
         JE    PTIM0060                                                         
                                                                                
W        USING PERVERTD,DMCB       2nd and subseqent days                       
         GOTOR DATCON,DMCB,(1,TIMETDTE),(0,WORK)                                
         GOTOR PERVERT,W.PERVERTD,CALTSTR0,WORK                                 
         LLH   R1,W.PERVDAYS       Days inclusive is index                      
                                                                                
PTIM0060 CLM   R1,1,CALTDAYS       Ensure day index is within range             
         JNH   *+6                                                              
         DC    H'0'                Die if it isn't                              
         MHI   R1,L'CALTDHRS       Multiply index by data width                 
         LA    R1,CALTDHRS(R1)     Point to daily hours bucket                  
         AP    0(L'CALTDHRS,R1),TIMEHRS                                         
                                                                                
PTIM0070 AHI   R4,L'TIMEDAY        Bump to next day/time slot                   
         C     R4,FULL             Test all entries processed                   
         JL    PTIM0050            No                                           
         DROP  R4                                                               
                                                                                
PTIM0080 LLC   R0,TIMLN            Bump to next element on record               
         AR    R2,R0                                                            
         J     PTIM0040            and process it                               
                                                                                
PTIM0090 J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Last for person account record                                      *         
***********************************************************************         
                                                                                
LPER     CLI   WHYREJ,0            Test current person was rejected             
         JNE   EXIT                Yes - exit now                               
                                                                                
         MVC   SPERLAST,SUBLAST    Set submitter last name                      
         MVC   SPERFRST,SUBFRST    Set submitter first name                     
         MVC   SPERACT,SUBACT      Set submitter 1R account                     
         MVC   SSUBCODE,SUBCODE    Set submitter person code                    
         MVC   SSUBPID,SUBPID      Set submitter PID                            
         MVC   SAPPPID,APPPID      Set approver PID                             
         MVC   SAPPCODE,APPCODE    Set approver code                            
         MVC   SAPPLAST,APPLAST    Set approver last name                       
         MVC   SAPPFRST,APPFRST    Set approver first name                      
         MVC   SSEQNUM,NULLS       Initialize sequence number                   
                                                                                
         CLI   FCRDTIME,YESQ       Test reading time records                    
         JNE   LPER0100            No                                           
                                                                                
         CLI   RSUBTYPE,RSUBTYPD   Test daily time profile set                  
         JNE   LPER0010                                                         
         GOTOR GETEDT              Yes - post daily edit hours                  
                                                                                
***********************************************************************         
* Build delinquent time records and post to sort                      *         
***********************************************************************         
                                                                                
LPER0010 MVI   SRECTYPE,SRECTIMQ   These are submitter time records             
                                                                                
         LARL  R2,OFFCAL                                                        
         USING CALTABD,R2          Add record for each bad time period          
LPER0020 CLI   CALTABD,CALTEOTQ    Test end of calendar table                   
         JE    LPER0100                                                         
         CLC   CALTTIM,TIMETRIG    Test time status vs. time trigger            
         JH    LPER0090                                                         
                                                                                
         LLH   R0,SSEQNUM          Bump sequence number                         
         AHI   R0,1                                                             
         STCM  R0,3,SSEQNUM                                                     
         MVC   SCALNUM,CALTNUM     Set calendar period values                   
         MVC   SCALSTR,CALTSTR1                                                 
         MVC   SCALEND,CALTEND1                                                 
         MVC   SCALTIM,CALTTIM                                                  
                                                                                
***********************************************************************         
* Handle overdue time                                                 *         
***********************************************************************         
                                                                                
         MVI   SOVERDUE,C' '       Set null overdue                             
         CLI   QOPT3,NOQ           Test don't want overdue                      
         JE    LPER0080                                                         
         MVI   SOVERDUE,NOQ        Set not overdue                              
         CLI   CALTTIM,CALTTAPP    Fully approved are not overdue               
         JE    LPER0070                                                         
         CLI   RSUBTYPE,RSUBTYPD   Test daily time profile set                  
         JE    LPER0030            No                                           
         CLC   SUBOVER0,CALTEND0   Is this period overdue?                      
         JL    LPER0070            No                                           
         J     LPER0060            Yes                                          
                                                                                
LPER0030 CP    CALTTHRS,PZERO      Do we have all the hours                     
         JE    LPER0070                                                         
                                                                                
         LA    R3,CALTDHRS         Point to daily hours bucket                  
         LLC   R0,CALTDAYS         R0=number of days to inspect                 
         SR    R4,R4               Non-compliant day number                     
LPER0040 CP    0(L'CALTDHRS,R3),PZERO                                           
         JNM   LPER0050                                                         
         GOTOR ADDAY,DMCB,CALTSTR0,WORK,(R4)                                    
         CLC   SUBOVER0,WORK       Test this day is overdue                     
         JH    LPER0060            Yes                                          
LPER0050 AHI   R3,L'CALTDHRS       Bump to next day                             
         JCT   R0,LPER0040         Do for number of days                        
         J     LPER0070                                                         
                                                                                
LPER0060 MVI   SOVERDUE,YESQ       This time sheet is overdue                   
                                                                                
LPER0070 CLI   QOPT3,ONLYQ         Apply overdue only option                    
         JNE   LPER0080                                                         
         CLI   SOVERDUE,YESQ       Test this is overdue                         
         JNE   LPER0090            No - ignore                                  
                                                                                
***********************************************************************         
* Add record to sort buffer and process next calendar entry           *         
***********************************************************************         
                                                                                
LPER0080 MVI   TIME.TSACTN,TSAADD                                               
         GOTOR TSAR,TIME.TSARD     Add time sort record                         
         JE    LPER0090                                                         
         DC    H'0'                                                             
                                                                                
LPER0090 AHI   R2,CALTABLN         Bump to next calendar period                 
         J     LPER0020                                                         
                                                                                
***********************************************************************         
* Build delinquent approval records and post to sort                  *         
***********************************************************************         
                                                                                
LPER0100 CLI   QOPT4,NOQ           Test printing delinquent approvals           
         JE    EXIT                No - exit                                    
                                                                                
         MVC   ARECORD(ARECLEN),NULLS                                           
         MVC   AAPPACT,SUBACT                                                   
         MVI   SRECTYPE,SRECAPPQ   These are approval records                   
         MVC   SSEQNUM,NULLS       Initialize sequence number                   
         MVI   APPS.TSACTN,TSARDH  Read for delinquent approvals                
                                                                                
LPER0110 GOTOR TSAR,APPS.TSARD     Get first/next TSAR sort record              
         MVI   APPS.TSACTN,TSANXT  Set action to read next record               
         TM    APPS.TSERRS,TSEEOF  Test end of file                             
         JNZ   EXIT                Yes                                          
         CLC   AAPPACT,SUBACT      Test for this person account                 
         JNE   EXIT                No                                           
                                                                                
         LARL  R0,PERCAL                                                        
         ST    R0,ACALTAB          Point to person calendar for GETCAL          
         GOTOR GETCAL,ASUBACT      Build submitter calendar                     
                                                                                
         LARL  R2,PERCAL           Point to submitter's calendar                
         USING CALTABD,R2          R2=A(calendar table entry)                   
LPER0120 CLI   CALTABD,CALTEOTQ    Locate calendar period entry                 
         JE    LPER0110                                                         
         CLC   CALTEND1,ASUBEDT    Match on period end date                     
         JE    LPER0130                                                         
         AHI   R2,CALTABLN         Bump to next table entry                     
         J     LPER0120                                                         
                                                                                
LPER0130 MVC   CALTTIM,ASUBSTA     Add delinquent approval record               
         GOTOR SETSTA,CALTTIM      Convert time status to character             
         LLH   R0,SSEQNUM          Bump sequence number                         
         AHI   R0,1                                                             
         STCM  R0,3,SSEQNUM                                                     
         MVC   SAPPLAST,ASUBLST    Approver is submitter in this record         
         MVC   SAPPFRST,ASUBFST                                                 
         MVC   SCALNUM,CALTNUM     Set calendar period values                   
         MVC   SCALSTR,CALTSTR1                                                 
         MVC   SCALEND,CALTEND1                                                 
         MVC   SCALTIM,CALTTIM                                                  
                                                                                
***********************************************************************         
* Handle overdue approvals                                            *         
***********************************************************************         
                                                                                
         MVI   SOVERDUE,NOQ                                                     
         CLC   ASUBSUB,NULLS       Test submitted date is present               
         JE    LPER0140                                                         
         CLC   ASUBSUB,APPOVER1    Test overdue                                 
         JNL   LPER0140                                                         
         MVI   SOVERDUE,YESQ       Set approval is overdue                      
                                                                                
***********************************************************************         
* Put delinquent approvals record to sort buffer and get next record  *         
***********************************************************************         
                                                                                
LPER0140 MVI   TIME.TSACTN,TSAADD                                               
         GOTOR TSAR,TIME.TSARD     Add approval sort record                     
         JE    LPER0110                                                         
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Last for sub-department - read back sort records and print report   *         
***********************************************************************         
                                                                                
LSUB     CLC   HEADHOOK,NULLS      Test printing report                         
         JE    LSUB0010                                                         
         MVI   LINE,FF             Yes - set high line number                   
         MVI   FORCEHED,YESQ       and force headline build                     
                                                                                
LSUB0010 MVC   LPERACT,NULLS       Initialize saved person code                 
         MVC   SKEY(SKEYLEN),NULLS Clear key to get first record                
         MVI   TIME.TSACTN,TSARDH                                               
         J     LSUB0030                                                         
                                                                                
LSUB0020 MVI   TIME.TSACTN,TSANXT  Set to get next TSAR record                  
                                                                                
LSUB0030 GOTOR TSAR,TIME.TSARD     Get first/next TSAR record                   
         TM    TIME.TSERRS,TSEEOF  Test all done                                
         JNZ   LSUB0080            Yes                                          
                                                                                
         CLC   LPERACT,SPERACT     Test change of person                        
         JE    LSUB0050            No                                           
         CLC   LPERACT,NULLS       Test first person                            
         JE    LSUB0040                                                         
         MVI   0(R2),FF            Set end of person buffer                     
         GOTOR PRTBUF              Print report for buffered person             
                                                                                
LSUB0040 MVC   LPERACT,SPERACT     Save current person account code             
         LARL  R2,PERBUF           R2=A(start of person buffer)                 
         MVC   TIMCOUNT,NULLS      Initialize time record count                 
         MVC   APPCOUNT,NULLS      Initialize approval record count             
                                                                                
LSUB0050 LARL  R0,PERBUFX                                                       
         CR    R2,R0               Ensure enough space to store next            
         JL    *+6                                                              
         DC    H'0'                Person buffer exceeded                       
         MVC   0(SRECLEN,R2),SRECORD                                            
         CLI   SRECTYPE,SRECTIMQ   Test time record                             
         JNE   LSUB0060            No                                           
         LH    R0,TIMCOUNT         Count the time records                       
         AHI   R0,1                                                             
         STH   R0,TIMCOUNT                                                      
         J     LSUB0070                                                         
                                                                                
LSUB0060 CLI   SRECTYPE,SRECAPPQ   Test approval record                         
         JE    *+6                                                              
         DC    H'0'                No                                           
         LH    R0,APPCOUNT         Count the approval records                   
         AHI   R0,1                                                             
         STH   R0,APPCOUNT                                                      
                                                                                
LSUB0070 AHI   R2,SRECLEN          Bump to next buffer entry                    
         J     LSUB0020            Get next input record                        
                                                                                
LSUB0080 CLC   LPERACT,NULLS       If no records were read                      
         JE    EXIT                then exit now                                
         MVI   0(R2),FF            Set end of person buffer                     
         GOTOR PRTBUF              Print report for last person                 
         CLC   HEADHOOK,NULLS      Test we are downloading                      
         JE    EXIT                Yes - exit                                   
                                                                                
         LLC   RF,LINE             Draw a box bottom line                       
         L     RE,ADBXAREA                                                      
         LA    R1,BOXROWS-1-BOXD(RF,RE)                                         
         MVI   0(R1),C'B'          Set current line is box bottom               
         GOTOR ACREPORT            Print box bottom line                        
         MVI   0(R1),C' '          Clear bottom line                            
         MVI   SKIPSPEC,C'F'                                                    
         MVI   FORCEFUT,YESQ                                                    
         L     R1,ADBXAREA                                                      
         MVI   BOXOFF-BOXD(R1),YESQ                                             
         GOTOR ACREPORT            Print footlines                              
         MVI   BOXOFF-BOXD(R1),NOQ                                              
         MVI   SKIPSPEC,NOQ                                                     
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Last for request                                                    *         
***********************************************************************         
                                                                                
LREQ     CLC   HEADHOOK,NULLS      Test downloading                             
         JNE   EXIT                                                             
         GOTOR DOWNIT,DOWNEOR      Send end of report signal                    
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Print person buffer in report or download format                    *         
***********************************************************************         
                                                                                
PRTBUF   NTR1  LABEL=*                                                          
         LARL  R2,PERBUF           Point to person buffer                       
         CLI   0(R2),FF            Test anything in the buffer                  
         JE    EXIT                No - exit                                    
                                                                                
         MVC   SSRECORD,SRECORD    Save the current input record                
                                                                                
         LARL  R3,DLCBW                                                         
         USING DLCBD,R3            R3=A(DLCB)                                   
                                                                                
         CLC   HEADHOOK,NULLS      Test downloading                             
         JE    PRTBUF20            Yes                                          
                                                                                
***********************************************************************         
* Regular report printing                                             *         
***********************************************************************         
                                                                                
         CLI   FORCEHED,YESQ       Test forcing a new page                      
         JE    PRTBUF02                                                         
         LLC   RF,LINE             Get current line number                      
         LA    R0,5(RF)                                                         
         CLM   R0,1,MAXLINES       Test near bottom of page                     
         JNH   *+12                                                             
         MVI   FORCEHED,YESQ       Yes - force a new page                       
         J     PRTBUF02                                                         
                                                                                
         L     RE,ADBXAREA                                                      
         LA    R1,BOXROWS-1-BOXD(RF,RE)                                         
         MVI   0(R1),C'M'                                                       
         GOTOR ACREPORT            Print a middle box line                      
         MVI   0(R1),C' '                                                       
                                                                                
PRTBUF02 MVC   SRECORD(SRECLEN),0(R2)                                           
                                                                                
***********************************************************************         
* Print awaiting approval section header                              *         
***********************************************************************         
                                                                                
         CLC   SSEQNUM,HONE        Test first record for person                 
         JNE   PRTBUF08                                                         
         CLI   SRECTYPE,SRECAPPQ   Test first approval line                     
         JNE   PRTBUF04                                                         
         CLI   QOPT4,ONLYQ         Test awaiting approval only                  
         JE    PRTBUF04                                                         
         MVC   P+1(23),=C'** Awaiting approval **'                              
         GOTOR ACREPORT                                                         
                                                                                
***********************************************************************         
* Format person details                                               *         
***********************************************************************         
                                                                                
PRTBUF04 MVC   PLLSTFST(L'SPERLAST),SPERLAST                                    
         LA    R1,PLLSTFST+L'SPERLAST-1                                         
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         MVC   2(L'SPERFRST,R1),SPERFRST                                        
         MVI   PLEMP#-1,C' '                                                    
         MVC   PLEMP#,SSUBCODE                                                  
         MVC   PLCONPID,SSUBPID                                                 
                                                                                
         CLI   SRECTYPE,SRECTIMQ   Test time record                             
         JNE   PRTBUF06                                                         
         LLH   R0,TIMCOUNT                                                      
         GOTOR EDITNUM                                                          
         MVC   PLTOTMIS,WORK                                                    
         J     PRTBUF08                                                         
                                                                                
PRTBUF06 CLI   SRECTYPE,SRECAPPQ   Test approval record                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LLH   R0,APPCOUNT                                                      
         GOTOR EDITNUM                                                          
         MVC   PLTOTMIS,WORK                                                    
                                                                                
***********************************************************************         
* Format time details                                                 *         
***********************************************************************         
                                                                                
PRTBUF08 LLC   R0,SCALNUM                                                       
         GOTOR EDITNUM                                                          
         MVC   PLCALNUM,WORK                                                    
         GOTOR DATCON,DMCB,(1,SCALSTR),(10,PLCALSTR)                            
         MVI   PLPDASH,C'-'                                                     
         GOTOR DATCON,DMCB,(1,SCALEND),(10,PLCALEND)                            
         OI    PLCALEND,C'0'                                                    
                                                                                
         LA    R1,STATAB           Look up time status in table                 
         USING STATABD,R1          R1=A(time status table)                      
         LA    R0,STATABN                                                       
PRTBUF10 CLC   SCALTIM,STATTIM     Match on calendar type                       
         JE    PRTBUF12                                                         
         AHI   R1,L'STATAB                                                      
         JCT   R0,PRTBUF10                                                      
         DC    H'0'                                                             
PRTBUF12 MVC   PLTIMSTA,STATCHAR   Move status to print line                    
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Format line manager/submitter first/last names                      *         
***********************************************************************         
                                                                                
         CLI   SRECTYPE,SRECTIMQ   Test time record                             
         JNE   PRTBUF14                                                         
                                                                                
         MVC   PLLINMGR(L'SAPPLAST),SAPPLAST                                    
         LA    R1,PLLINMGR+L'SAPPLAST-1                                         
         J     PRTBUF16                                                         
                                                                                
PRTBUF14 MVC   PLSUBBER(L'SAPPLAST),SAPPLAST                                    
         LA    R1,PLSUBBER+L'SAPPLAST-1                                         
                                                                                
PRTBUF16 CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,PRTBUF16                                                      
         MVC   2(L'SAPPFRST,R1),SAPPFRST                                        
         MVI   PLBOXC4,C' '        Ensure no overflow                           
                                                                                
         CLI   QOPT3,NOQ           Test suppressing overdue flag                
         JE    *+10                                                             
         MVC   PLOVRDUE,SOVERDUE   Yes - don't show on report                   
                                                                                
         GOTOR ACREPORT            Print report detail line                     
*                                                                               
         AHI   R2,SRECLEN                                                       
         CLI   0(R2),FF            Test at end of buffer                        
         JNE   PRTBUF02            No - process next record                     
*                                                                               
         MVC   SSUBMAIL,SPACES     Init Submitter email address                 
         GOTOR GETEML,SSUBPID      Get email address of submitter               
         JNE   *+10                Found?                                       
         MVC   SSUBMAIL,TMPEMAIL   Yes, save it                                 
*                                                                               
         MVC   SAPPMAIL,SPACES     Init Approver email address                  
         CLI   SAPPPID,C'?'                                                     
         JE    PRTBUF17            Yes, continue                                
         GOTOR GETEML,SAPPPID      no, get email address of approver            
         JNE   PRTBUF17            found?                                       
         MVC   SAPPMAIL,TMPEMAIL   Yes, save it.                                
*                                                                               
PRTBUF17 MVC   PLEMLBL1,=C'Email:' Set "Email" label in Submitter               
         MVC   PLSBEMLR,SSUBMAIL   Print submitter email address                
         MVC   PLEMLBL2,PLEMLBL1   Set "Email" label in Approver                
         MVC   PLAPEMLR,SAPPMAIL   Print Approver email address                 
         GOTOR ACREPORT            Print report detail line                     
*                                                                               
         CLC   SSUBMAIL+L'PLSBEMLR(L'SSUBMAIL-L'PLSBEMLR),SPACES                
         JNH   *+10                More data on submitter email                 
         MVC   PLEMLBL1(L'SSUBMAIL-L'PLSBEMLR),SSUBMAIL+L'PLSBEMLR              
*                                                                               
         CLC   SAPPMAIL+L'PLAPEMLR(L'SAPPMAIL-L'PLAPEMLR),SPACES                
         JNH   *+10                more data on approver email                  
         MVC   PLEMLBL2(28),SAPPMAIL+L'PLAPEMLR on next                         
*                                                                               
         CLC   PLINE1,SPACES       if more data on submitter or                 
         JNH   PRTBUF18            approver email                               
         GOTOR ACREPORT            wrap it up on next line                      
*                                                                               
         CLC   SAPPMAIL+50(13),SPACES still more data on approver eml           
         JNH   PRTBUFX             no, exit                                     
         MVC   PLEMLBL2(13),SAPPMAIL+50  yes, wrap it on next print             
*                                                                               
         GOTOR ACREPORT            Print Line                                   
*                                                                               
PRTBUF18 J     PRTBUFX             Else exit                                    
                                                                                
***********************************************************************         
* Download format                                                     *         
***********************************************************************         
                                                                                
PRTBUF20 MVC   SRECORD(SRECLEN),0(R2)                                           
                                                                                
         MVI   DLCBFLD,C'S'        Record type (S=submitter)                    
         CLI   SRECTYPE,SRECTIMQ                                                
         JE    *+8                                                              
         MVI   DLCBFLD,C'A'                    (A=approver)                     
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'OFFICE),OFFICE                                         
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'DEPT),DEPT                                             
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'SUBDEPT),SUBDEPT                                       
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'SPERLAST),SPERLAST                                     
         LA    R1,DLCBFLD+L'SPERLAST-1                                          
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         MVC   2(L'SPERFRST,R1),SPERFRST                                        
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'SSUBCODE),SSUBCODE                                     
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'SSUBPID),SSUBPID                                       
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         GOTOR GETEML,SSUBPID      Get email address of submitter               
*                                                                               
         MVC   DLCBFLX(L'TMPEMAIL),TMPEMAIL  SUBMITTER EMAIL                    
         MVI   DLCBLEN,L'TMPEMAIL  LENGTH  OF THE FIELD                         
         MVI   DLCBACT,DLCBPUT                                                  
         OI    DLCBFLG1,DLCBFXFL   USING EXTENDED FIELD                         
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR VDLFLD,DLCBD                                                     
         NI    DLCBFLG1,FF-DLCBFXFL   USING EXTENDED FIELD                      
*                                                                               
         MVC   DLCBFLD(L'TIMCOUNT),TIMCOUNT                                     
         CLI   SRECTYPE,SRECTIMQ                                                
         JE    *+10                                                             
         MVC   DLCBFLD(L'APPCOUNT),APPCOUNT                                     
         MVI   DLCBLEN,L'TIMCOUNT                                               
         GOTOR DOWNIT,DOWNBIN                                                   
                                                                                
         MVC   DLCBFLD(L'SCALNUM),SCALNUM                                       
         MVI   DLCBLEN,L'SCALNUM                                                
         GOTOR DOWNIT,DOWNBIN                                                   
                                                                                
         GOTOR DATCON,DMCB,(1,SCALSTR),(10,DLCBFLD)                             
         OI    DLCBFLD,C'0'                                                     
         MVI   DLCBFLD+8,C'-'                                                   
         GOTOR DATCON,DMCB,(1,SCALEND),(10,DLCBFLD+9)                           
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         LA    R1,STATAB           Look up time status in table                 
         USING STATABD,R1                                                       
         LA    R0,STATABN                                                       
PRTBUF22 CLC   SCALTIM,STATTIM     Match on calendar type                       
         JE    PRTBUF24                                                         
         AHI   R1,L'STATAB                                                      
         JCT   R0,PRTBUF22                                                      
         DC    H'0'                                                             
PRTBUF24 MVC   DLCBFLD(L'STATCHAR),STATCHAR                                     
         DROP  R1                                                               
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         CLI   SRECTYPE,SRECTIMQ   Test a time record                           
         JNE   PRTBUF26                                                         
         GOTOR DOWNIT,DOWNTXT      Send a blank column                          
                                                                                
PRTBUF26 MVC   DLCBFLD(L'SAPPLAST),SAPPLAST                                     
         LA    R1,DLCBFLD+L'SAPPLAST-1                                          
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         MVC   2(L'SAPPFRST,R1),SAPPFRST                                        
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         CLI   SRECTYPE,SRECAPPQ   Test approval record                         
         JNE   PRTBUF28                                                         
         GOTOR DOWNIT,DOWNTXT      Yes - send a blank column                    
                                                                                
PRTBUF28 MVC   TMPEMAIL,SPACES                                                  
         CLI   SAPPPID,C'?'                                                     
         JE    PRTBUF29            Yes, continue                                
         GOTOR GETEML,SAPPPID      no, get email address of approver            
*                                                                               
PRTBUF29 MVC   DLCBFLX(L'TMPEMAIL),TMPEMAIL   APPROVER EMAIL ID                 
         MVI   DLCBLEN,L'TMPEMAIL  LENGTH  OF THE FIELD                         
         MVI   DLCBACT,DLCBPUT                                                  
         OI    DLCBFLG1,DLCBFXFL   USING EXTENDED FIELD                         
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR VDLFLD,DLCBD                                                     
         NI    DLCBFLG1,FF-DLCBFXFL   RESET EXTENDED FIELD FLAG                 
                                                                                
         MVC   DLCBFLD(L'OFFNAME),OFFNAME                                       
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'DEPNAME),DEPNAME                                       
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         MVC   DLCBFLD(L'SUBNAME),SUBNAME                                       
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
         CLI   QOPT3,NOQ           Test suppressing overdue flag                
         JE    PRTBUF30                                                         
         MVC   DLCBFLD(L'SOVERDUE),SOVERDUE                                     
         GOTOR DOWNIT,DOWNTXT                                                   
                                                                                
PRTBUF30 GOTOR DOWNIT,DOWNEOL      Put end of line                              
                                                                                
         AHI   R2,SRECLEN          Bump to next person buffer entry             
         CLI   0(R2),FF            Test end of buffer                           
         JNE   PRTBUF20            No - process next input record               
                                                                                
PRTBUFX  MVC   SRECORD(SRECLEN),SSRECORD                                        
         J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Build calendar period table                                         *         
*                                                                     *         
* Ntry:- R1 points to office code or spaces                           *         
***********************************************************************         
                                                                                
GETCAL   NTR1  LABEL=*                                                          
                                                                                
         MVC   GETOFF,0(R1)        Save input office code                       
                                                                                
GETCAL02 L     R3,ACALTAB                                                       
         USING CALTABD,R3          R3=A(Calendar table)                         
         SR    R4,R4               R4=number of entries                         
                                                                                
K        USING CASPAS,IOKEY                                                     
R        USING CASRECD,IO                                                       
                                                                                
         MVC   K.CASPAS,NULLS      Read calendar records                        
         MVI   K.CASPTYP,CASPTYPQ                                               
         MVI   K.CASPSUB,CASPSUBQ                                               
         MVC   K.CASPCPY,QCOMPANY                                               
         MVC   IOKEYSAV,K.CASPAS                                                
         MVC   K.CASPEDTE(1),REQSTR                                             
         GOTOR DATAMGR,DMCB,DMRDHI,ACMACDIR,K.CASPAS,K.CASPAS                   
         JE    GETCAL06                                                         
         DC    H'0'                                                             
                                                                                
GETCAL04 GOTOR DATAMGR,DMCB,DMRSEQ,ACMACDIR,K.CASPAS,K.CASPAS                   
         JE    GETCAL06                                                         
         DC    H'0'                                                             
                                                                                
GETCAL06 CLC   K.CASPAS(CASPEDTE-CASKEY),IOKEYSAV                               
         JNE   GETCAL12                                                         
                                                                                
         CLC   K.CASPSDTE,REQEND   Test starts after request end                
         JH    GETCAL04                                                         
         CLC   K.CASPOFC,GETOFF    Test for correct office                      
         JNE   GETCAL04                                                         
                                                                                
         GOTOR DATAMGR,DMCB,ACMDMGET,ACMACMST,K.CASPDA,IO,DMWORK                
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,R.CASRFST                                                     
         USING TMPEL,R2            R2=A(current element)                        
                                                                                
GETCAL08 CLI   TMPEL,0             Test end of record                           
         JE    GETCAL04                                                         
         CLI   TMPEL,TMPELQ        Test timesheet time period element           
         JNE   GETCAL10            No                                           
                                                                                
         CLC   TMPSTART,REQEND     Test starts after end date                   
         JH    GETCAL10                                                         
         CLC   TMPEND,REQSTR       or ends before start date                    
         JL    GETCAL10                                                         
         MVC   CALTSTR1,TMPSTART   Build a calendar week entry                  
         MVC   CALTEND1,TMPEND                                                  
         MVC   CALTNUM,TMPNUMB                                                  
         GOTOR DATCON,DMCB,(1,CALTSTR1),(0,CALTSTR0)                            
         GOTOR DATCON,DMCB,(1,CALTEND1),(0,CALTEND0)                            
                                                                                
W        USING PERVERTD,WORK       2nd and subseqent days                       
         LA    R0,CALTEND0                                                      
         CLC   CALTEND0,TODAY0     Test future end date                         
         JNH   *+8                                                              
         LA    RF,TODAY0           Yes - set end date as today                  
         GOTOR PERVERT,W.PERVERTD,CALTSTR0,(R0)                                 
         LLH   R1,W.PERVDAYS       R1=number of days inclusive                  
         CHI   R1,CALTMAXD                                                      
         JNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,CALTDAYS         Set number of days in calendar               
                                                                                
         AHI   R3,CALTABLN         Bump table pointer                           
         AHI   R4,1                Bump table count                             
         CHI   R4,CALTMAX          Test table size exceeded                     
         JNH   *+6                                                              
         DC    H'0'                Yes                                          
                                                                                
         MVC   CALEND,TMPEND       Set time end date filter                     
         CHI   R4,1                Test if first date to be added               
         JNE   GETCAL10                                                         
         MVC   CALSTART,TMPSTART   Set time start date filter                   
                                                                                
GETCAL10 LLC   R0,1(R2)            Bump to next element on record               
         AR    R2,R0                                                            
         J     GETCAL08            and go process it                            
         DROP  R2                                                               
                                                                                
GETCAL12 MVI   CALTABD,CALTEOTQ    Set end of calendar                          
         LTR   R4,R4               Test calendar found                          
         JNZ   EXIT                No - exit                                    
                                                                                
         CLC   GETOFF,SPACES       Test have looked for default                 
         JNE   *+6                                                              
         DC    H'0'                Yes - can't resolve calendar                 
         MVC   GETOFF,SPACES       Set to look for default calendar             
         J     GETCAL02            and start from scratch                       
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Set time status to 'not started' for all active weeks in the        *         
* calendar given the start/end date of the location                   *         
***********************************************************************         
                                                                                
SETCAL   LARL  RF,OFFCAL                                                        
         USING CALTABD,RF          RF=A(Calendar table)                         
                                                                                
SETCAL02 CLI   CALTABD,CALTEOTQ    Test end of table                            
         BER   RE                                                               
         CLC   SUBENDDT,NULLS      Test end date for location set               
         JE    SETCAL04                                                         
         CLC   SUBENDDT,CALTSTR1   Test ended before this period                
         JL    SETCAL06                                                         
                                                                                
SETCAL04 CLC   SUBSTRDT,CALTEND1   Test started after this period               
         JH    SETCAL06                                                         
         MVI   CALTTIM,CALTTNST    Set time not started for week                
                                                                                
SETCAL06 AHI   RF,CALTABLN         Bump to next calendar period                 
         J     SETCAL02                                                         
         DROP  RF                                                               
                                                                                
***********************************************************************         
* Extract account name into a local name field                        *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as folows:                     *         
*                                                                     *         
*        P1 points to name element                                    *         
*        P2 points to local name field                                *         
***********************************************************************         
                                                                                
GETNAM   STM   RE,R1,12(RD)        Save work registers                          
         L     RE,0(R1)            RE=A(name element)                           
         LLC   RF,NAMLN-NAMELD(RE)                                              
         SHI   RF,NAMEREC-NAMELD   RF=L'name                                    
         ICM   RF,8,SPACES         Set pad character                            
         LA    RE,NAMEREC-NAMELD(,RE)                                           
         L     R0,4(R1)            R0=A(local name element)                     
         LA    R1,L'NAMEREC        R1=length of local name element              
         MVCL  R0,RE               Move name and pad with spaces                
         LM    RE,R1,12(RD)        Restore work registers                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Get approver for a person                                           *         
*                                                                     *         
* Exit:- WORK contains approver person code and CC set to equal if    *         
*        approver found, else CC will be set to not equal             *         
***********************************************************************         
                                                                                
GETAPP   NTR1  LABEL=*                                                          
                                                                                
         LR    R4,R1               R4=A(1R account code)                        
         L     R2,ADLDGHIR                                                      
         LA    R2,ACLVALS-ACLELD+(L'ACLVALS*3)(R2)                              
         USING ACLVALS,R2                                                       
         LA    R3,4                R3=number of account levels                  
                                                                                
K        USING DPAPASD,IOKEY                                                    
GETAPP02 MVC   K.DPAPAS,NULLS      Build approver look-up key                   
         MVI   K.DPAPTYP,DPAPTYPQ                                               
         MVI   K.DPAPSUB,DPAPSUBQ                                               
         MVC   K.DPAPCPY,QCOMPANY                                               
         MVI   K.DPAPAPPL,DPAPATIM                                              
         ZAP   K.DPAPXVAL,PZERO                                                 
         LA    R0,K.DPAP1RAC       Move 1R account code to key                  
         LA    R1,L'DPAP1RAC                                                    
         LR    RE,R4               Point to 1R account code                     
         LLC   RF,ACLVLEN          RF=L'key at this level                       
         ICM   RF,8,SPACES         Set pad character                            
         MVCL  R0,RE                                                            
         MVC   IOKEYSAV(L'DPAPAS),K.DPAPAS                                      
         GOTOR DATAMGR,DMCB,DMRDHI,ACMACDIR,K.DPAPAS,K.DPAPAS                   
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   K.DPAPAS(DPAPPIDB-DPAPAS),IOKEYSAV                               
         JE    GETAPP04                                                         
         SHI   R2,L'ACLVALS        Back up to previous level                    
         JCT   R3,GETAPP02         Do for number of account levels              
         J     EXITN                                                            
                                                                                
GETAPP04 MVC   WORK(L'DPAPPIDB),K.DPAPPIDB                                      
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get security PID for a person                                       *         
*                                                                     *         
* Ntry:- R1=A(PID number)                                             *         
* Exit:- CC not equal if bad person, else WORK contains printable     *         
*        security PID                                                 *         
***********************************************************************         
                                                                                
GETSEC   NTR1  LABEL=*                                                          
                                                                                
K        USING SA0KEY,IOKEY                                                     
R        USING SA0KEY,IO                                                        
                                                                                
         MVC   K.SA0KEY,NULLS      Read security record and extract PID         
         MVI   K.SA0KTYP,SA0KTYPQ                                               
         L     RF,ADMASTC                                                       
         MVC   K.SA0KAGY,MCAGYSEC-MASTD(RF)                                     
         MVC   K.SA0KNUM,0(R1)                                                  
         GOTOR DATAMGR,DMCB,DMREAD,ACMCTFIL,K.SA0KEY,R.SA0KEY                   
         JNE   EXITN                                                            
                                                                                
         LA    R1,R.SA0DATA        Locate the PID element                       
         SR    R0,R0                                                            
         USING SAPALD,R1                                                        
GETSEC02 CLI   SAPALEL,0           Bad record                                   
         JE    EXITN                                                            
         CLI   SAPALEL,SAPALELQ    Test pointer element                         
         JNE   GETSEC04                                                         
         MVC   WORK(L'SAPALPID),SAPALPID                                        
         J     EXITY                                                            
GETSEC04 IC    R0,SAPALLN          Bump to next element on record               
         AR    R1,R0                                                            
         J     GETSEC02                                                         
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Get Email address of Person                                         *         
*                                                                     *         
* Ntry:- R1=A(Person Id)                                              *         
* Exit:- If SAPEEELQl is available, then print email address else     *         
*        leave it blank.                                              *         
***********************************************************************         
                                                                                
GETEML   NTR1  LABEL=*                                                          
                                                                                
K        USING SAPEKEY,IOKEY       KEY                                          
R        USING SAPEKEY,IO          RECORD                                       
                                                                                
         MVC   TMPEMAIL,SPACES     Init tempemail address                       
         MVC   K.SAPEKEY,NULLS     Init key                                     
         MVI   K.SAPETYP,SAPETYPQ  Set record identifier                        
         MVI   K.SAPESUB,SAPESUBQ  Set record sub indetifier                    
         L     RF,ADMASTC          Get Agency code                              
         MVC   K.SAPEAGY,MCAGYSEC-MASTD(RF)                                     
         MVC   K.SAPEPID,0(R1)     Get Personal Id                              
         GOTOR DATAMGR,DMCB,DMREAD,ACMCTFIL,K.SAPEKEY,R.SAPEKEY                 
         CLI   12(R1),X'00'        record found?                                
         JNE   EXITN               No, skip it                                  
                                                                                
         LA    R1,R.SAPEDATA       Yes, get first element of record             
         USING SAPEED,R1                                                        
GETEML02 CLI   SAPEEEL,0           End of record?                               
         JE    EXITN               exit                                         
         CLI   SAPEEEL,SAPEEELQ    No, Person email id element found?           
         JNE   GETEML04            no, get next element                         
         LLC   RF,SAPEELN          Yes,                                         
         SHI   RF,SAPEELNQ+1                                                    
         EXMVC RF,TMPEMAIL,SAPEEID store email address in temp field            
         J     EXITY               exit                                         
GETEML04 LLC   R0,SAPEELN          Bump to next element on record               
         AR    R1,R0                                                            
         J     GETEML02                                                         
         DROP  R1,K,R                                                           
                                                                                
***********************************************************************         
* Read person record and extract values                               *         
***********************************************************************         
                                                                                
GETPER   NTR1  LABEL=*                                                          
                                                                                
K        USING PERRECD,IOKEY                                                    
R        USING PERRECD,IO                                                       
                                                                                
         MVC   PERVALS(PERVALL),NULLS                                           
         MVC   PERCODE,0(R1)       Set person code                              
         MVC   K.PERKEY,SPACES     Read person record                           
         MVI   K.PERKTYP,PERKTYPQ                                               
         MVC   K.PERKCPY,QCOMPANY                                               
         MVC   K.PERKCODE,PERCODE  Set person code in key                       
                                                                                
         GOTOR DATAMGR,DMCB,DMREAD,ACMACDIR,K.PERKEY,K.PERKEY                   
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DATAMGR,DMCB,ACMDMGET,ACMACMST,K.PERKDA,R.PERKEY,DMWORK          
         JE    EXTPER                                                           
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Read person record via PID passive and extract values               *         
***********************************************************************         
                                                                                
GETPID   NTR1  LABEL=*                                                          
                                                                                
K        USING PIDRECD,IOKEY                                                    
                                                                                
         MVC   PERVALS(PERVALL),NULLS                                           
         MVC   K.PIDKEY,NULLS      Build PID key                                
         MVI   K.PIDKTYP,PIDKTYPQ                                               
         MVI   K.PIDKSUB,PIDKSUBQ                                               
         MVC   K.PIDKCPY,QCOMPANY                                               
         MVC   K.PIDKPID,0(R1)     Set person code                              
         MVI   K.PIDKSTYP,PIDKPERQ                                              
         MVC   IOKEYSAV,K.PIDKEY                                                
                                                                                
         GOTOR DATAMGR,DMCB,DMRDHI,ACMACDIR,K.PIDKEY,K.PIDKEY                   
         JNE   EXITN                                                            
         CLC   K.PIDKEY(PIDKPER-PIDKEY),IOKEYSAV                                
         JNE   EXITN                                                            
                                                                                
         MVC   PERCODE,K.PIDKPER   Set person code of approver                  
         GOTOR DATAMGR,DMCB,ACMDMGET,ACMACMST,K.PIDKDA,R.PERRECD,DMWORK         
         JNE   EXITN                                                            
                                                                                
***********************************************************************         
* Extract values from a person record                                 *         
*                                                                     *         
* Entered from GETPER and GETPID above                                *         
***********************************************************************         
                                                                                
EXTPER   L     R3,ADLDGHIR                                                      
         USING ACLELD,R3           R3=A(ledger hierarchy element)               
         MVC   PERFRST,SPACES                                                   
         MVI   PERFRST,UNKNOWN     Preset first name unknown                    
         MVC   PERLAST,SPACES                                                   
         MVI   PERLAST,UNKNOWN     Preset last name unknown                     
                                                                                
         LA    R2,R.PERRFST        R2=A(first element on person record)         
EXTPER02 CLI   0(R2),0             Test end of record                           
         JE    EXITY               Yes                                          
                                                                                
         USING GPNELD,R2                                                        
         CLI   GPNEL,GPNELQ        Test general name element                    
         JNE   EXTPER06                                                         
         LA    R0,PERFRST                                                       
         LA    R1,L'PERFRST                                                     
         CLI   GPNTYP,GPNTFST      Test first name                              
         JE    EXTPER04                                                         
         LA    R0,PERLAST                                                       
         LA    R1,L'PERLAST                                                     
         CLI   GPNTYP,GPNTLST      Test last name                               
         JNE   EXTPER06                                                         
EXTPER04 LA    RE,GPNNME           Set first/last name                          
         LLC   RF,GPNLN                                                         
         SHI   RF,GPNNME-GPNELD                                                 
         CR    RF,R0                                                            
         JNH   *+6                                                              
         LR    RF,R0                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE                                                            
         J     EXTPER10                                                         
                                                                                
         USING LOCELD,R2                                                        
EXTPER06 CLI   LOCEL,LOCELQ        Test end of record                           
         JNE   EXTPER08                                                         
                                                                                
         CLC   LOCEND,NULLS        Test current location                        
         JNE   EXTPER10            No                                           
         LLC   RF,ACLVALS+(L'ACLVALS*0)                                         
         LA    RE,PERACT           Point to office in key                       
         LA    R0,LOCOFF                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0               Move office                                  
                                                                                
         LLC   RF,ACLVALS+(L'ACLVALS*1)                                         
         LLC   R0,ACLVALS+(L'ACLVALS*0)                                         
         LA    RE,PERACT                                                        
         AR    RE,R0               Point to department in key                   
         SR    RF,R0                                                            
         LA    R0,LOCDEPT                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0               Move department                              
                                                                                
         LLC   RF,ACLVALS+(L'ACLVALS*2)                                         
         LLC   R0,ACLVALS+(L'ACLVALS*1)                                         
         LA    RE,PERACT                                                        
         AR    RE,R0               Point to sub-department in key               
         SR    RF,R0                                                            
         LA    R0,LOCSUB                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0               Move sub-department to key                   
                                                                                
         LLC   RF,ACLVALS+(L'ACLVALS*3)                                         
         LLC   R0,ACLVALS+(L'ACLVALS*2)                                         
         LA    RE,PERACT                                                        
         AR    RE,R0               Point to person in key                       
         SR    RF,R0                                                            
         LA    R0,PERCODE          Add person code to key                       
         LR    R1,RF                                                            
         MVCL  RE,R0               Move person code to key                      
         J     EXTPER10                                                         
                                                                                
         USING EMPELD,R2                                                        
EXTPER08 CLI   EMPEL,EMPELQ        Test employee element                        
         JNE   EXTPER10                                                         
         MVC   PERSTAT,EMPSTAT     Yes - set person status                      
                                                                                
EXTPER10 LLC   R0,1(R2)            Bump to next element                         
         AR    R2,R0                                                            
         J     EXTPER02            and go process                               
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Post daily edit hours to calendar for each working day              *         
***********************************************************************         
                                                                                
GETEDT   NTR1  LABEL=*                                                          
         LARL  R2,OFFCAL                                                        
         USING CALTABD,R2          R2=A(calendar table entry)                   
                                                                                
GETEDT02 CLI   CALTABD,CALTEOTQ    Test end of calendar table                   
         JE    EXIT                                                             
         CLI   CALTTIM,CALTTIGN    Test FGAP/LOA week                           
         JE    GETEDT28                                                         
                                                                                
K        USING EDTRECD,IOKEY                                                    
R        USING EDTRECD,IO                                                       
                                                                                
         MVI   GEDTFLAG,GEDTFOFF+GEDTFDPT+GEDTFSUB+GEDTFPER                     
                                                                                
GETEDT04 MVC   K.EDTKEY,SPACES     Build key of edit hours record               
         MVI   K.EDTKTYP,EDTKTYPQ                                               
         MVI   K.EDTKSUB,EDTKSUBQ                                               
         MVC   K.EDTKCPY,QCOMPANY                                               
                                                                                
         TM    GEDTFLAG,GEDTFOFF   Test move office to key                      
         JZ    GETEDT06                                                         
         MVC   K.EDTKOFC,OFFICE    Move office to key                           
         NI    GEDTFLAG,FF-GEDTFOFF                                             
                                                                                
GETEDT06 TM    GEDTFLAG,GEDTFDPT   Test move department to key                  
         JZ    GETEDT08                                                         
         MVC   K.EDTKDPT,DEPT      Move department to key                       
         NI    GEDTFLAG,FF-GEDTFDPT                                             
         OI    GEDTFLAG,GEDTFOFF   Set to move office to key                    
                                                                                
GETEDT08 TM    GEDTFLAG,GEDTFSUB   Test move sub-department to key              
         JZ    GETEDT10                                                         
         MVC   K.EDTKSBD,SUBDEPT   Move sub-department to key                   
         NI    GEDTFLAG,FF-GEDTFSUB                                             
         OI    GEDTFLAG,GEDTFDPT   Set to move department to key                
                                                                                
GETEDT10 TM    GEDTFLAG,GEDTFPER   Test move person to key                      
         JZ    GETEDT12                                                         
         MVC   K.EDTKPER,SUBCODE   Move person to key                           
         NI    GEDTFLAG,FF-GEDTFPER                                             
         OI    GEDTFLAG,GEDTFSUB   Set to move sub-department to key            
                                                                                
GETEDT12 MVC   K.EDTKSEQ,NULLS     Set sequence number                          
         MVC   K.EDTKYR,CALTSTR1   Set calendar year                            
         MVI   K.EDTKKSTA,EDTKSDAY Set daily time                               
         MVC   IOKEYSAV,K.EDTKEY   Saved key                                    
                                                                                
         GOTOR DATAMGR,DMCB,DMREAD,ACMACDIR,K.EDTKEY,K.EDTKEY                   
         JE    GETEDT14                                                         
         MVC   K.EDTKEY,IOKEYSAV   Restore saved key                            
         CLC   K.EDTKOFC,SPACES    Have we read lowest level                    
         JNE   GETEDT04            No - look at previous level                  
         J     GETEDT28            Yes - process next period                    
                                                                                
GETEDT14 CLC   R.EDTKEY,K.EDTKEY   Test already have the record                 
         JE    GETEDT16                                                         
         GOTOR DATAMGR,DMCB,ACMDMGET,ACMACMST,K.EDTKDA,R.EDTRECD,DMWORK         
         JE    GETEDT16                                                         
         DC    H'0'                                                             
         DROP  K                                                                
                                                                                
***********************************************************************         
* Subtract required hours from each daily time bucket and total       *         
***********************************************************************         
                                                                                
GETEDT16 LA    R3,CALTDHRS         R3=A(hours by day buckets)                   
         GOTOR DATCON,DMCB,(1,CALTSTR1),(0,CURDAY0)                             
         MVC   CURDAY1,CALTSTR1    Set period start date                        
         J     GETEDT20                                                         
                                                                                
GETEDT18 GOTOR ADDAY,DMCB,CURDAY0,WORK,1                                        
         MVC   CURDAY0,WORK        Set current date to insepect                 
         GOTOR DATCON,DMCB,(0,CURDAY0),(1,CURDAY1)                              
         CLC   CURDAY1,TODAY1      Test before today                            
         JNL   GETEDT28            No - we are done                             
         CLC   CURDAY1,CALTEND1    Check past period end date                   
         JH    GETEDT28            Yes - we are done                            
         AHI   R3,L'CALTDHRS       Point to next hour bucket                    
                                                                                
GETEDT20 GOTOR GETDAY,DMCB,CURDAY0,WORK                                         
         LLC   RE,0(R1)            Point to day table entry                     
         BCTR  RE,0                                                             
         MHI   RE,L'DAYTAB                                                      
         LA    RE,DAYTAB(RE)                                                    
         MVC   DAYEDIT,2(RE)       Extract edit hours indicator                 
         LLH   RF,0(RE)            RF=displacement to COBLOCK value             
         LARL  RE,COBLOCKW         for current day                              
         AR    RF,RE                                                            
         CLI   0(RF),YESQ          Test this is a working day                   
         JNE   GETEDT18            No - go on to next day                       
                                                                                
         LA    R4,R.EDTRFST        Look for daily edit element                  
         USING DEDELD,R4           R4=A(daily edit element)                     
GETEDT22 CLI   DEDEL,0             Test end of record                           
         JE    GETEDT18                                                         
         CLI   DEDEL,DEDELQ        Test daily edit element                      
         JNE   GETEDT26                                                         
         CLC   DEDIND,DAYEDIT      Match day indicator                          
         JNE   GETEDT26                                                         
         CLI   DEDLN,DEDLN1Q       Test special exclusion day                   
         JE    GETEDT24                                                         
         CLC   DEDDATE,CURDAY1     Match date                                   
         JNE   GETEDT26                                                         
         TM    DEDSTAT,DEDSPHOL+DEDSSHOL                                        
         JNZ   GETEDT26            Ignore holidays                              
                                                                                
GETEDT24 SP    CALTTHRS,DEDHRS     Post edit hours to total & bucket            
         SP    0(L'CALTDHRS,R3),DEDHRS                                          
         J     GETEDT18                                                         
                                                                                
GETEDT26 LLC   R0,DEDLN            Bump to next element on record               
         AR    R4,R0                                                            
         J     GETEDT22            and process it                               
         DROP  R4                                                               
                                                                                
GETEDT28 AHI   R2,CALTABLN         Bump to next calendar table entry            
         J     GETEDT02            Process next calendar period                 
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Clear time sheet data in calendar table                             *         
***********************************************************************         
                                                                                
CLRHRS   LARL  RF,OFFCAL           Clear hour bucket in calendar table          
         USING CALTABD,RF                                                       
CLRHRS02 CLI   CALTABD,CALTEOTQ    Test end of calendar table                   
         BER   RE                  Yes - exit                                   
         MVI   CALTTIM,CALTTIGN    Set all weeks as FGAP/LOA                    
         ZAP   CALTTHRS,PZERO      Initialize hours buckets                     
         MVC   CALTDHRS(CALTDHRL),CALTTHRS                                      
         AHI   RF,CALTABLN         Bump to next entry                           
         J     CLRHRS02            and process it                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
* Convert time status                                                 *         
*                                                                     *         
* Ntry:- R1 points to TIMKSTAT value, conversion is done in place     *         
***********************************************************************         
                                                                                
SETSTA   MVC   BYTE,0(R1)          Save input value                             
         MVI   0(R1),CALTTREJ      Rejected                                     
         TM    BYTE,TIMSREJE                                                    
         BNZR  RE                                                               
         MVI   0(R1),CALTTSUB      Submitted                                    
         TM    BYTE,TIMSSUBM                                                    
         BNZR  RE                                                               
         MVI   0(R1),CALTTPAP      Part-approved                                
         TM    BYTE,TIMSPAPP                                                    
         BNZR  RE                                                               
         MVI   0(R1),CALTTAPP      Fully approved                               
         TM    BYTE,TIMSFAPP                                                    
         BNZR  RE                                                               
         MVI   0(R1),CALTTINP      In progress (and unknown)                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* Load a core-resident phase into memory                              *         
*                                                                     *         
* Ntry:- R1 contains phase number                                     *         
***********************************************************************         
                                                                                
LOADCR   NTR1  LABEL=*                                                          
         LR    RE,R1                                                            
         SRDL  RE,4                                                             
         SRL   RF,32-4                                                          
         IC    RE,HEXTAB(RE)                                                    
         IC    RF,HEXTAB(RF)                                                    
         L     R1,ADMASTC                                                       
         USING MASTD,R1                                                         
         MVC   MCDUB,=CL8'T00Aef'                                               
         STC   RE,MCDUB+4                                                       
         STC   RF,MCDUB+5                                                       
         L     RF,MCVLOADM                                                      
         GOTOR (RF),DMCB,0                                                      
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Headline hook routine                                               *         
***********************************************************************         
                                                                                
HOOK     MVC   HEAD4+20(L'CPYNAME),CPYNAME                                      
         MVC   HEAD5+16(L'OFFICE),OFFICE                                        
         MVC   HEAD5+20(L'OFFNAME),OFFNAME                                      
         MVC   HEAD6+16(L'DEPT),DEPT                                            
         MVC   HEAD6+20(L'DEPNAME),DEPNAME                                      
         MVC   HEAD7+16(L'SUBDEPT),SUBDEPT                                      
         MVC   HEAD7+20(L'SUBNAME),SUBNAME                                      
         MVC   HEAD7+88(L'CALDATES),CALDATES                                    
                                                                                
         CLI   QOPT3,NOQ           Set heading for overdue if required          
         JE    HOOK0010                                                         
         MVI   MID1+(PLOVRDUE-PLINE),C'O'                                       
         MVI   MID2+(PLOVRDUE-PLINE),C'D'                                       
                                                                                
HOOK0010 LA    R1,STATAB                                                        
         USING STATABD,R1          R1=A(time status table)                      
         LHI   R0,STATABN                                                       
HOOK0020 CLC   STATTIM,TIMETRIG    Display statuses up to time trigger          
         BHR   RE                                                               
         LLH   RF,STATDISP         Get displacement to headline value           
         LA    RF,ACWORKD(RF)      Point to destination and move                
         MVI   0(RF),C'('                                                       
         MVC   1(L'STATCHAR,RF),STATCHAR                                        
         MVI   2(RF),C')'                                                       
         MVC   4(L'STATNAME,RF),STATNAME                                        
         AHI   R1,STATABL          Bump to next table entry                     
         JCT   R0,HOOK0020         Do next                                      
         BR    RE                                                               
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Interface to DLFLD                                                  *         
*                                                                     *         
* Ntry:- R1 contains call number (see list below)                     *         
***********************************************************************         
                                                                                
DOWNINI  EQU   1                   Initialize for new report                    
DOWNTXT  EQU   2                   Put text field                               
DOWNBIN  EQU   3                   Put binary field                             
DOWNEOL  EQU   4                   Put end of line                              
DOWNEOR  EQU   5                   Put end of report                            
                                                                                
DOWNIT   NTR1  LABEL=*                                                          
         LR    R2,R1               R2=calling action                            
         LARL  R1,DLCBW                                                         
         USING DLCBD,R1            R1=A(DLFLD control block)                    
                                                                                
         CHI   R2,DOWNINI          Test initialization                          
         JNE   DOWNIT02                                                         
         MVI   DLCBACT,DLCBINIT    Set to initialize                            
         MVC   DLCBAED,EDITOR      Set address of EDITOR                        
         LA    R0,P                Set address of print line                    
         ST    R0,DLCBAPL                                                       
         LARL  R0,DOWNHOOK         Point to download hook routine               
         ST    R0,DLCBAPR                                                       
         LHI   R0,L'P                                                           
         STH   R0,DLCXMAXL                                                      
         MVI   DLCXDELC,C' '       Delimiter                                    
         MVI   DLCXEOTC,C'"'       Text delimiter                               
         MVI   DLCXEOTA,C''''      Alternate text delimiter                     
         MVI   DLCXEOLC,X'5E'      Semi-colon, end-of-line                      
         MVI   DLCXEORC,C':'       End-of-report                                
         GOTOR VDLFLD                                                           
         MVC   DLCBFLD,SPACES                                                   
         MVI   FORCEHED,YESQ       Force new page                               
         GOTOR ACREPORT                                                         
         J     EXIT                                                             
                                                                                
DOWNIT02 CHI   R2,DOWNTXT          Test download text                           
         JNE   DOWNIT08                                                         
         MVI   DLCBACT,DLCBPUT     Set sending text string                      
         MVI   DLCBTYP,DLCBTXT                                                  
         LA    RE,DLCBFLD+L'DLCBFLD-1                                           
         LA    RF,L'DLCBFLD        Calculate field length                       
DOWNIT04 CLI   0(RE),C' '                                                       
         JH    DOWNIT06                                                         
         BCTR  RE,0                                                             
         JCT   RF,DOWNIT04                                                      
         LHI   RF,1                                                             
DOWNIT06 STC   RF,DLCBLEN          Set field length                             
         J     DOWNIT20                                                         
                                                                                
DOWNIT08 CHI   R2,DOWNBIN          Test download binary data                    
         JNE   DOWNIT10                                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBBIN                                                  
         J     DOWNIT20                                                         
                                                                                
DOWNIT10 CHI   R2,DOWNEOL          Test end of line signal                      
         JNE   DOWNIT12                                                         
         MVI   DLCBACT,DLCBEOL                                                  
         J     DOWNIT20                                                         
                                                                                
DOWNIT12 CHI   R2,DOWNEOR          Test end of report signal                    
         JNE   DOWNIT14                                                         
         MVI   DLCBACT,DLCBEOR                                                  
         J     DOWNIT20                                                         
                                                                                
DOWNIT14 DC    H'0'                Other actions here                           
                                                                                
DOWNIT20 GOTOR VDLFLD              Call DLFLD                                   
         MVC   DLCBFLD,SPACES      Clear data field                             
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Download column headings                                            *         
*                                                                     *         
* Ntry:- R1=A(column heading table (DHnTAB))                          *         
***********************************************************************         
                                                                                
DLCH     NTR1  LABEL=NO                                                         
         LR    R2,R1               R2=A(column heading table)                   
         LARL  R3,DLCBW                                                         
         USING DLCBD,R3            R3=A(DLFLD control block)                    
DLCH0010 CLI   0(R2),0             Test end of colunm heading table             
         JE    DLCH0020            Yes                                          
                                                                                
         LLC   RF,0(R2)            RF=Length of entry                           
         BCTR  RF,0                RF=length of text in table entry             
         LA    R0,DLCBFLD          Point to output field                        
         LR    R1,RF               To length=from length                        
         LA    RE,1(R2)            Point to text in table entry                 
         MVCL  R0,RE               Move the heading data                        
         GOTOR DOWNIT,DOWNTXT      Download heading text                        
         LLC   RF,0(R2)                                                         
         AR    R2,RF               Bump to next column entry                    
         J     DLCH0010            Go back to process                           
                                                                                
DLCH0020 GOTOR DOWNIT,DOWNEOL      Send end of line signal                      
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* DLFLD hook routine to print a line of output                        *         
***********************************************************************         
                                                                                
DOWNHOOK MVI   FORCEHED,NOQ                                                     
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
                                                                                
***********************************************************************         
* Edit a number for printing                                          *         
*                                                                     *         
* Ntry:- R0 contains number                                           *         
* Exit:- WORK contains edited value                                   *         
***********************************************************************         
                                                                                
EDITNUM  CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   WORK(10),SPACES                                                  
         UNPK  WORK(5),DUB                                                      
EDITNUM2 CLI   WORK,C'0'                                                        
         BHR   RE                                                               
         CLI   WORK+1,C' '                                                      
         BER   RE                                                               
         MVC   WORK(5),WORK+1                                                   
         J     EDITNUM2                                                         
                                                                                
***********************************************************************         
* Exits                                                               *         
***********************************************************************         
                                                                                
EXITN    LHI   RE,0                Bad exit                                     
         J     EXITCC                                                           
EXITY    LHI   RE,1                Good exit                                    
EXITCC   CHI   RE,1                Exit with condition code set                 
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* Constants etc.                                                      *         
***********************************************************************         
                                                                                
FF       EQU   X'FF'                                                            
ONEK     EQU   1024                                                             
UNKNOWN  EQU   C'?'                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
ONLYQ    EQU   C'O'                                                             
                                                                                
GLOBALS  DS    0D                  ** Global literals and variables **          
         LTORG ,                                                                
                                                                                
HEXTAB   DC    C'0123456789ABCDEF'                                              
NULLS    DC    256X'00'                                                         
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
HONE     DC    H'1'                                                             
                                                                                
GETCAP   DC    A(0)                A(GETCAP)   (T00AC1)                         
TSAR     DC    A(0)                A(TSAR)     (T00A5D)                         
EDITOR   DC    A(0)                A(EDITOR)   (T00A71)                         
PERVERT  DC    V(PERVERT)          V(PERVERT)                                   
VDLFLD   DC    V(DLFLD)            Download field processor                     
                                                                                
TSARAPPS DC    XL(TSPXTNL)'00'     TSAR block for approval look-ups             
TSARTIME DC    XL(TSPXTNL)'00'     TSAR block for time record sorting           
                                                                                
TODAY1   DC    XL3'00'             Today's date (packed)                        
TODAY0   DC    CL6' '              Today's date (ebcdic YYMMDD)                 
                                                                                
RSUBTYPE DC    C' '                Request override time type                   
RSUBTYPP EQU   C'P'                Period sub-type (CONDO profile)              
RSUBTYPD EQU   C'D'                Daily sub-type (CONCM profile)               
RSUBDAYS DC    X'00'               Request override submit days                 
RAPPDAYS DC    X'00'               Request override approval days               
                                                                                
REQSTR   DC    XL(L'CALSTART)'00'  Request start date                           
REQEND   DC    XL(L'CALEND)'00'    Request end date                             
SUBOVER0 DC    CL6' '              Submit overdue date (ebcdic YYMMDD)          
APPOVER1 DC    XL3'00'             Approval overdue date                        
TIMETRIG DC    C' '                Delinquent time trigger                      
                                                                                
CALSTART DC    XL(L'TMPSTART)'00'  Calendar start date                          
CALEND   DC    XL(L'TMPEND)'00'    Calendar end date                            
CALDATES DC    CL42' '             Printable calendar dates                     
                                                                                
OCALSTR  DC    XL(L'TMPSTART)'00'  Office calendar time start date              
OCALEND  DC    XL(L'TMPEND)'00'    Office calendar time end date                
                                                                                
CPYNAME  DC    CL(L'NAMEREC)' '    Company name                                 
OFFICE   DC    CL(L'LOCOFF)' '     Office code                                  
OFFNAME  DC    CL(L'NAMEREC)' '    Office name                                  
DEPT     DC    CL(L'COKDPT)' '     Department code                              
DEPNAME  DC    CL(L'NAMEREC)' '    Department name                              
SUBDEPT  DC    CL(L'COKSDT)' '     Sub-department code                          
SUBNAME  DC    CL(L'NAMEREC)' '    Sub-department name                          
                                                                                
WHYREJ   DC    X'00'               Why person was rejected                      
                                                                                
SUBVALS  DS    0X                  ** Extracted Person values **                
SUBACT   DC    CL(L'ACTKACT)' '    Person account code                          
SUBCODE  DC    CL(L'COKPER)' '     Person person code                           
SUBFRST  DC    CL(L'PERFRST)' '    Person first name                            
SUBLAST  DC    CL(L'PERLAST)' '    Person last name                             
SUBSTRDT DC    XL(L'LOCSTART)'00'  Location start date                          
SUBENDDT DC    XL(L'LOCEND)'00'    Location end date                            
SUBSTAT  DC    XL(L'EMPCSTAT)'00'  Person status                                
SUBPID#  DC    XL(L'SUBPID)'00'    Person security PID number                   
SUBPID   DC    CL(L'SAPALPID)' '   Person printable PID                         
APPPID#  DC    XL(L'SUBPID)'00'    Approver PID number                          
APPCODE  DC    CL(L'PERCODE)' '    Approver person code                         
APPPID   DC    CL(L'SAPALPID)' '   Approver printable PID                       
APPFRST  DC    CL(L'PERFRST)' '    Approver first name                          
APPLAST  DC    CL(L'PERLAST)' '    Approver last name                           
SUBVALL  EQU   *-SUBVALS                                                        
                                                                                
DAYTAB   DS    0XL3                ** Daily hours days **                       
         DC    AL2(CODMO-COBLOCKD),AL1(DEDIMON)                                 
         DC    AL2(CODTU-COBLOCKD),AL1(DEDITUE)                                 
         DC    AL2(CODWE-COBLOCKD),AL1(DEDIWED)                                 
         DC    AL2(CODTH-COBLOCKD),AL1(DEDITHU)                                 
         DC    AL2(CODFR-COBLOCKD),AL1(DEDIFRI)                                 
         DC    AL2(CODSA-COBLOCKD),AL1(DEDISAT)                                 
         DC    AL2(CODSU-COBLOCKD),AL1(DEDISUN)                                 
                                                                                
MODETAB  DS    0X                  ** Mode translation table **                 
         DC    256AL1(EXITJUMP-MODEJUMP)                                        
         ORG   MODETAB+RUNFRST     First for run                                
         DC    AL1(FRUNJUMP-MODEJUMP)                                           
         ORG   MODETAB+PROCRQST    Process new request                          
         DC    AL1(PREQJUMP-MODEJUMP)                                           
         ORG   MODETAB+LEDGFRST    First for (1R) ledger                        
         DC    AL1(FLDGJUMP-MODEJUMP)                                           
         ORG   MODETAB+PROCLEVA    First for office                             
         DC    AL1(FOFFJUMP-MODEJUMP)                                           
         ORG   MODETAB+PROCLEVB    First for department                         
         DC    AL1(FDEPJUMP-MODEJUMP)                                           
         ORG   MODETAB+PROCLEVC    First for sub-department                     
         DC    AL1(FSUBJUMP-MODEJUMP)                                           
         ORG   MODETAB+PROCACC     Process a person account                     
         DC    AL1(PPERJUMP-MODEJUMP)                                           
         ORG   MODETAB+PROCTIME    Process a time record                        
         DC    AL1(PTIMJUMP-MODEJUMP)                                           
         ORG   MODETAB+ACCLAST     Last for a person account                    
         DC    AL1(LPERJUMP-MODEJUMP)                                           
         ORG   MODETAB+LEVCLAST    Last for sub-department                      
         DC    AL1(LSUBJUMP-MODEJUMP)                                           
         ORG   MODETAB+REQLAST     Last for request                             
         DC    AL1(LREQJUMP-MODEJUMP)                                           
         ORG   ,                                                                
                                                                                
STATAB   DS    0XL(STATABL)        ** Time status table **                      
         DC    AL1(CALTTNST),C'N',CL(L'STATNAME)'Not started'                   
         DC    AL2(HEAD4+88-ACWORKD)                                            
         DC    AL1(CALTTINP),C'I',CL(L'STATNAME)'In progress'                   
         DC    AL2(HEAD4+110-ACWORKD)                                           
         DC    AL1(CALTTSUB),C'S',CL(L'STATNAME)'Submitted'                     
         DC    AL2(HEAD5+88-ACWORKD)                                            
         DC    AL1(CALTTREJ),C'R',CL(L'STATNAME)'Rejected'                      
         DC    AL2(HEAD5+110-ACWORKD)                                           
         DC    AL1(CALTTPAP),C'P',CL(L'STATNAME)'Part approved'                 
         DC    AL2(HEAD6+88-ACWORKD)                                            
         DC    AL1(CALTTAPP),C'A',CL(L'STATNAME)'Approved'                      
         DC    AL2(HEAD6+110-ACWORKD)                                           
STATABN  EQU   (*-STATAB)/L'STATAB                                              
                                                                                
STATABD  DSECT ,                   ** Time status table **                      
STATTIM  DS    X                   Time value (see CALTTIM)                     
STATCHAR DS    C                   Display character                            
STATNAME DS    CL14                Display name                                 
STATDISP DS    AL2                 Displacement to headline value               
STATABL  EQU   *-STATABD           Length of table entry                        
ACMT02   CSECT ,                                                                
                                                                                
DH1TAB   DS    0H                  ** Download column headings 1 **             
DH1001   DS    0X                                                               
         DC    AL1(DH1001X-DH1001)                                              
         DC    C'Record'                                                        
DH1001X  EQU   *                                                                
DH1002   DS    0X                                                               
         DC    AL1(DH1002X-DH1002)                                              
         DC    C'Office'                                                        
DH1002X  EQU   *                                                                
DH1003   DS    0X                                                               
         DC    AL1(DH1003X-DH1003)                                              
         DC    C'Department'                                                    
DH1003X  EQU   *                                                                
DH1004   DS    0X                                                               
         DC    AL1(DH1004X-DH1004)                                              
         DC    C'Sub-department'                                                
DH1004X  EQU   *                                                                
DH1005   DS    0X                                                               
         DC    AL1(DH1005X-DH1005)                                              
         DC    C'Person'                                                        
DH1005X  EQU   *                                                                
DH1006   DS    0X                                                               
         DC    AL1(DH1006X-DH1006)                                              
         DC    C' '                                                             
DH1006X  EQU   *                                                                
DH1007   DS    0X                                                               
         DC    AL1(DH1007X-DH1007)                                              
         DC    C' '                                                             
DH1007X  EQU   *                                                                
DH1017   DS    0X                                                               
         DC    AL1(DH1017X-DH1017)                                              
         DC    C'E-mail'                                                        
DH1017X  EQU   *                                                                
DH1008   DS    0X                                                               
         DC    AL1(DH1008X-DH1008)                                              
         DC    C'Total'                                                         
DH1008X  EQU   *                                                                
DH1009   DS    0X                                                               
         DC    AL1(DH1009X-DH1009)                                              
         DC    C'Period'                                                        
DH1009X  EQU   *                                                                
DH1010   DS    0X                                                               
         DC    AL1(DH1010X-DH1010)                                              
         DC    C' '                                                             
DH1010X  EQU   *                                                                
DH1011   DS    0X                                                               
         DC    AL1(DH1011X-DH1011)                                              
         DC    C'Time'                                                          
DH1011X  EQU   *                                                                
DH1012   DS    0X                                                               
         DC    AL1(DH1012X-DH1012)                                              
         DC    C' '                                                             
DH1012X  EQU   *                                                                
DH1013   DS    0X                                                               
         DC    AL1(DH1013X-DH1013)                                              
         DC    C' '                                                             
DH1013X  EQU   *                                                                
DH1018   DS    0X                                                               
         DC    AL1(DH1018X-DH1018)                                              
         DC    C'Approver'                                                      
DH1018X  EQU   *                                                                
DH1014   DS    0X                                                               
         DC    AL1(DH1014X-DH1014)                                              
         DC    C'Office'                                                        
DH1014X  EQU   *                                                                
DH1015   DS    0X                                                               
         DC    AL1(DH1015X-DH1015)                                              
         DC    C'Department'                                                    
DH1015X  EQU   *                                                                
DH1016   DS    0X                                                               
         DC    AL1(DH1016X-DH1016)                                              
         DC    C'Sub-department'                                                
DH1016X  EQU   *                                                                
DH1ODU   DC    AL1(DH1ODUX-DH1ODU)                                              
         DC    C' '                                                             
DH1ODUX  EQU   *                                                                
DH1X     DC    AL1(0)                                                           
                                                                                
DH2TAB   DS    0X                  ** Download column headings 2 **             
DH2001   DS    0X                                                               
         DC    AL1(DH2001X-DH2001)                                              
         DC    C'Type'                                                          
DH2001X  EQU   *                                                                
DH2002   DS    0X                                                               
         DC    AL1(DH2002X-DH2002)                                              
         DC    C'Code'                                                          
DH2002X  EQU   *                                                                
DH2003   DS    0X                                                               
         DC    AL1(DH2003X-DH2003)                                              
         DC    C'Code'                                                          
DH2003X  EQU   *                                                                
DH2004   DS    0X                                                               
         DC    AL1(DH2004X-DH2004)                                              
         DC    C'Code'                                                          
DH2004X  EQU   *                                                                
DH2005   DS    0X                                                               
         DC    AL1(DH2005X-DH2005)                                              
         DC    C'Name'                                                          
DH2005X  EQU   *                                                                
DH2006   DS    0X                                                               
         DC    AL1(DH2006X-DH2006)                                              
         DC    C'Employee#'                                                     
DH2006X  EQU   *                                                                
DH2007   DS    0X                                                               
         DC    AL1(DH2007X-DH2007)                                              
         DC    C'Person id'                                                     
DH2007X  EQU   *                                                                
DH2017   DS    0X                                                               
         DC    AL1(DH2017X-DH2017)                                              
         DC    C'Address'                                                       
DH2017X  EQU   *                                                                
DH2008   DS    0X                                                               
         DC    AL1(DH2008X-DH2008)                                              
         DC    C'Missing'                                                       
DH2008X  EQU   *                                                                
DH2009   DS    0X                                                               
         DC    AL1(DH2009X-DH2009)                                              
         DC    C'Number'                                                        
DH2009X  EQU   *                                                                
DH2010   DS    0X                                                               
         DC    AL1(DH2010X-DH2010)                                              
         DC    C'Start/end dates'                                               
DH2010X  EQU   *                                                                
DH2011   DS    0X                                                               
         DC    AL1(DH2011X-DH2011)                                              
         DC    C'Status'                                                        
DH2011X  EQU   *                                                                
DH2012   DS    0X                                                               
         DC    AL1(DH2012X-DH2012)                                              
         DC    C'Submitter'                                                     
DH2012X  EQU   *                                                                
DH2013   DS    0X                                                               
         DC    AL1(DH2013X-DH2013)                                              
         DC    C'Line manager'                                                  
DH2013X  EQU   *                                                                
DH2018   DS    0X                                                               
         DC    AL1(DH2018X-DH2018)                                              
         DC    C'E-mail'                                                        
DH2018X  EQU   *                                                                
DH2014   DS    0X                                                               
         DC    AL1(DH2014X-DH2014)                                              
         DC    C'Name'                                                          
DH2014X  EQU   *                                                                
DH2015   DS    0X                                                               
         DC    AL1(DH2015X-DH2015)                                              
         DC    C'Name'                                                          
DH2015X  EQU   *                                                                
DH2016   DS    0X                                                               
         DC    AL1(DH2016X-DH2016)                                              
         DC    C'Name'                                                          
DH2016X  EQU   *                                                                
DH2ODU   DC    AL1(DH2ODUX-DH2ODU)                                              
         DC    C'Overdue'                                                       
DH2ODUX  EQU   *                                                                
DH2X     DC    AL1(0)                                                           
                                                                                
***********************************************************************         
* Delinquent approval (approver) buffer record                        *         
***********************************************************************         
                                                                                
ARECORD  DS    0X                  ** Approvals buffer record **                
                                                                                
AKEY     DS    0X                  ** Approvals key **                          
                                                                                
AAPPACT  DC    CL(L'ACTKACT)' '    Approver 1R account code                     
AAPPCODE DC    CL(L'PERCODE)' '    Approver person code                         
ASUBACT  DC    CL(L'TIMKACT)' '    Submitter 1R account code                    
ASUBCODE DC    CL(L'PERKCODE)' '   Submitter 1R person code                     
ASUBLST  DC    CL(L'SUBLAST)' '    Submitter last name                          
ASUBFST  DC    CL(L'SUBFRST)' '    Submitter first name                         
                                                                                
ASUBEDT  DC    XL(L'TIMKPEDT)'00'  Period end date                              
                                                                                
AKEYLEN  EQU   *-AKEY              Length of record key                         
                                                                                
ASUBSUB  DC    XL(L'GDADATE)'00'   Submitted date                               
ASUBSTA  DC    XL(L'TIMKSTAT)'00'  Time status                                  
                                                                                
ARECLEN  EQU   *-ARECORD           Length of approvals record                   
                                                                                
***********************************************************************         
* Sorted time/approvals records                                       *         
***********************************************************************         
                                                                                
SRECORD  DS    0X                  ** Sort buffer record **                     
                                                                                
SKEY     DS    0X                  ** Sort key **                               
                                                                                
SPERLAST DC    CL(L'SUBLAST)' '    Person last name                             
SPERFRST DC    CL(L'SUBFRST)' '    Person first name                            
SPERACT  DC    CL(L'SUBACT)' '     Person account code                          
                                                                                
SRECTYPE DC    X'00'               ** Record type **                            
SRECTIMQ EQU   0                   Time record                                  
SRECAPPQ EQU   1                   Approval record                              
                                                                                
SSEQNUM  DC    XL2'00'             Sequence number                              
                                                                                
SKEYLEN  EQU   *-SKEY              Length of record key                         
                                                                                
SSUBCODE DC    CL(L'SUBCODE)' '    Person code                                  
SSUBPID  DC    CL(L'SUBPID)' '     Person PID                                   
                                                                                
SAPPCODE DC    CL(L'APPCODE)' '    Approver person (cost) code                  
SAPPPID  DC    CL(L'APPPID)' '     Approver PID                                 
SAPPLAST DC    CL(L'APPLAST)' '    Approver last name                           
SAPPFRST DC    CL(L'APPFRST)' '    Approver first name                          
                                                                                
SCALNUM  DC    XL(L'CALTNUM)'00'   Calendar period number                       
SCALSTR  DC    XL(L'CALTSTR1)'00'  Period start date                            
SCALEND  DC    XL(L'CALTEND1)'00'  Period end date                              
SCALTIM  DC    XL(L'CALTTIM)'00'   Timesheet status                             
SOVERDUE DC    C' '                Timesheet overdue (Y/N/blank)                
SRECLEN  EQU   *-SRECORD           Length of sort record                        
                                                                                
***********************************************************************         
* Calendar tables                                                     *         
***********************************************************************         
                                                                                
         DS    0H                  Office calendar (submitted time)             
OFFCAL   DC    (CALTMAX*CALTABLN)X'00'                                          
                                                                                
         DS    0H                  Person calendar (unapproved time)            
PERCAL   DC    (CALTMAX*CALTABLN)X'00'                                          
                                                                                
CALTABD  DSECT ,                   ** Calendar periods **                       
CALTEOTQ EQU   X'FF'               End of table indicator                       
CALTMAX  EQU   64                  Maximum number of table entries              
CALTNUM  DS    XL(L'TMPNUMB)       Period number                                
CALTSTR1 DS    XL(L'TMPSTART)      Start date                                   
CALTEND1 DS    XL(L'TMPEND)        End date                                     
CALTSTR0 DS    CL6                 Calendar start date (ebcdic YYMMDD)          
CALTEND0 DS    CL6                 Calendar end date   (ebcdic YYMMDD)          
                                                                                
CALTTIM  DS    X                   ** Time sheet status **                      
CALTTNST EQU   C'1'                Not started                                  
CALTTREJ EQU   C'2'                Rejected                                     
CALTTINP EQU   C'3'                In progress                                  
CALTTSUB EQU   C'4'                Submitted                                    
CALTTPAP EQU   C'5'                Part-approved                                
CALTTAPP EQU   C'6'                Fully approved                               
CALTTIGN EQU   X'FF'               FGAP/LOA week (ignored) week                 
                                                                                
CALTMAXD EQU   31                  Maximum days in a calendar period            
CALTDAYS DS    X                   Number of days in period                     
CALTTHRS DS    PL4                 Total hours                                  
CALTDHRS DS    (CALTMAXD)PL4       Hours for each day of period                 
CALTDHRL EQU   *-CALTDHRS                                                       
                                                                                
CALTABLN EQU   *-CALTABD           Length of table entry                        
ACMT02   CSECT ,                                                                
                                                                                
         DS    0F                                                               
DLCBW    DS    XL(DLCBXLX)         DLFLD control block                          
                                                                                
         DS    0F                                                               
COBLOCKW DC    (3*ONEK)X'00'       GETCAP control block                         
                                                                                
         DS    0F                                                               
PERBUF   DC    3000XL(SRECLEN)'00' Person records buffered here                 
PERBUFX  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* Local working storage and included books                            *         
***********************************************************************         
                                                                                
WORKD    DSECT ,                   ** Local working storage **                  
                                                                                
ACALTAB  DS    A                   A(calendar table to build)                   
                                                                                
TIMCOUNT DS    H                   Number of time records read                  
APPCOUNT DS    H                   Number of approval records read              
                                                                                
CURDAY0  DS    CL6                 Today's date in ebcdic format                
CURDAY1  DS    PL3                 Today's date in pwos format                  
                                                                                
GETOFF   DS    CL(L'CASPOFC)       Office code for GETCAL routine               
                                                                                
LPERACT  DS    CL(L'SPERACT)       Saved person code for LSUB routine           
                                                                                
DAYEDIT  DS    X                   Daily edit day value                         
                                                                                
GEDTFLAG DS    X                   ** GETEDT s/r flag byte **                   
GEDTFOFF EQU   X'80'               Move office to key                           
GEDTFDPT EQU   X'40'               Move department to key                       
GEDTFSUB EQU   X'20'               Move sub-department to key                   
GEDTFPER EQU   X'10'               Move person to key                           
                                                                                
PERVALS  DS    0X                  ** GETPID/GETPER record values **            
PERACT   DS    CL(L'ACTKACT)       Person 1R account code                       
PERCODE  DS    CL(L'PIDKPER)       Person code of approver                      
PERFRST  DS    CL20                First name                                   
PERLAST  DS    CL20                Last name                                    
PERSTAT  DS    XL(L'EMPCSTAT)      Person status                                
PERVALL  EQU   *-PERVALS           Length of block                              
                                                                                
SSRECORD DS    XL(SRECLEN)         Saved sort record                            
                                                                                
TAPKEY   DS    XL(L'TAPPAS)        Last TAP key processed                       
                                                                                
IOKEY    DS    XL64                General key area                             
IOKEYSAV DS    XL64                General save key area                        
IO       DS    XL2048              General i/o area                             
                                                                                
TMPEMAIL DS    CL63                Temporary email address field                
SSUBMAIL DS    CL63                Submitter email                              
SAPPMAIL DS    CL63                Approver email                               
WORKL    EQU   *-WORKD             Length of local working storage              
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
COBLOCKD DSECT ,                                                                
       ++INCLUDE ACCAPBLOCK                                                     
       ++INCLUDE ACREPWORKD                                                     
         ORG   P                                                                
PLINE    DS    0CL(L'P)            ** Report print line **                      
PLBOXL   DS    C                   Box left                                     
PLLSTFST DS    CL26                1R person last/first name                    
         DS    C                   Spacer                                       
PLEMP#   DS    CL7                 Person employee number                       
         DS    C                   Spacer                                       
PLCONPID DS    CL8                 Person control PID                           
PLBOXC1  DS    C                   Box column 1                                 
PLTOTMIS DS    CL7                 Total missing/unapproved                     
PLBOXC2  DS    C                   Box column 2                                 
PLCALNUM DS    CL2                 Calendar period number                       
         DS    C                   Spacer                                       
PLCALSTR DS    CL8                 Calendar start date (MM/DD/YY)               
PLPDASH  DS    C                   '-'                                          
PLCALEND DS    CL8                 Calendar end date   (MM/DD/YY)               
         DS    C                   Spacer                                       
PLTIMSTA DS    C                   Time status                                  
PLBOXC3  DS    C                   Box column 3                                 
PLSUBBER DS    CL26                Submitter (for approvals)                    
PLBOXC4  DS    C                   Box column 4                                 
PLLINMGR DS    CL26                Line manager (for submissions)               
PLBOXC5  DS    C                   Box column 5 (optional)                      
PLOVRDUE DS    C                   Overdue (optional)                           
PLBOXR   DS    C                   Box right                                    
*                                                                               
         ORG   P                                                                
PLINE1   DS    0CL(L'P)            ** Report print line for emails*             
         DS    C                   Filler                                       
PLEMLBL1 DS    CL6                 EMAIL Lable 1                                
PLSBEMLR DS    CL37                Submitter email address                      
PLFILL1  DS    CL59                Filler                                       
PLEMLBL2 DS    CL6                 EMAIL Lable 2                                
PLAPEMLR DS    CL22                Approver email address                       
         DS    CL1                 Filler                                       
         ORG   ,                                                                
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACQD                                                           
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDPERVERTD                                                     
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPMT02 11/11/19'                                      
         END   ,                                                                
