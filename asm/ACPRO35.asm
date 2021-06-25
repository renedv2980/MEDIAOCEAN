*          DATA SET ACPRO35    AT LEVEL 024 AS OF 04/11/07                      
*PHASE T60B35A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B35 - JOB ELIST REPORT'                                      
T60B35   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B35**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         ST    R2,RELO                                                          
         SPACE 1                                                                
* VALKEY LOGIC                                                                  
*                                                                               
EL1      CLI   MODE,VALKEY                                                      
         BNE   EL10                                                             
*                                                                               
         LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR LOCAL WORKING STORAGE                  
*                                                                               
         BAS   RE,LOAD             LOAD JOBBER TABLES                           
*                                                                               
EL2      BAS   RE,VALHED           VALIDATE HEADLINES                           
         MVI   ERROR,0                                                          
         B     ELX                                                              
         SPACE 1                                                                
* PRINTREP LOGIC                                                                
*                                                                               
EL10     CLI   MODE,PRINTREP       TEST TO PRINT REPORT                         
         BNE   ELX                                                              
*                                                                               
         LA    R1,BUFF             USE BUFF FOR RECORD AREAS                    
         LA    R0,5                                                             
         LA    RE,ACOMP                                                         
*                                                                               
EL12     ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,1000(R1)                                                      
         BCT   R0,EL12                                                          
*                                                                               
EL14     LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
*                                                                               
         CLI   QMG,0               TEST MEDIA GROUP REQUESTED                   
         BE    EL15                NO                                           
         GOTO1 VBLDMED,DMCB,(1,MEDBUFF)                                         
*                                                                               
EL15     CLI   QOG,0               TEST OFFICE GROUP REQUESTED                  
         BE    EL16                                                             
         GOTO1 VBLDOFF,DMCB,OFFBUFF                                             
*                                                                               
EL16     BAS   RE,RDF                                                           
         MVI   ERROR,0             RETURN OK EXIT                               
*                                                                               
ELX      XMOD1 1                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE FIELDS                                       
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         MVI   OPTION,C'Y'         DISPLAY NAMES FROM OGROUP TO JOB             
         GOTO1 SETHEIR                                                          
         MVI   EXLEN,2                                                          
*                                                                               
* VALIDATE OFFICE GROUP                                                         
*                                                                               
VALHED1  LA    R2,PROOGRH         OFFICE GROUP                                  
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         GOTO1 VALOG                                                            
         MVC   QOG,EFFOFG                                                       
         SPACE 1                                                                
* VALIDATE OFFICE                                                               
*                                                                               
VALHED2  LA    R2,PROOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
         SPACE 1                                                                
* VALIDATE CLIENT                                                               
*                                                                               
VALHED4  LA    R2,PROCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
         ZIC   R1,EXLEN                                                         
         ZIC   R0,LCLI                                                          
         AR    R1,R0                                                            
         STC   R1,EXLEN                                                         
         SPACE 1                                                                
* VALIDATE PRODUCT                                                              
*                                                                               
VALHED6  LA    R2,PROPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VALHED7                                                          
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   PROCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
         ZIC   R1,EXLEN                                                         
         ZIC   R0,LPRO                                                          
         AR    R1,R0                                                            
         STC   R1,EXLEN                                                         
         B     VALHED8                                                          
*                                                                               
VALHED7  TM    WHEN,X'20'          TEST FOR SOON REQUEST                        
         BZ    VALHED8             NO                                           
         MVI   ERROR,SOONERR       FORCE PRODUCT TO BE INPUT                    
         B     ERREND                                                           
         SPACE 1                                                                
* VALIDATE JOB                                                                  
*                                                                               
VALHED8  LA    R2,PROJOBH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED9                                                          
         MVI   ERROR,NEEDPRO                                                    
         CLI   PROPROH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALJOB                                                           
         MVC   QJOB,JOBNUM                                                      
         MVI   ERROR,BOESTERR                                                   
         TM    JOBJSTAT,JOBSMCSE   JOB USES MCS ESTIMATES                       
         BO    ERREND                                                           
*                                                                               
         MVI   ERROR,OLDESERR                                                   
         TM    JOBJSTAT,ACJBNEWQ   INSURE JOB USES NEW ESTIMATES                
         BZ    ERREND                                                           
*                                                                               
         ZIC   R1,EXLEN                                                         
         ZIC   R0,LJOB                                                          
         AR    R1,R0                                                            
         STC   R1,EXLEN                                                         
         B     VALHED10                                                         
*                                                                               
VALHED9  TM    WHEN,X'40'          TEST 'NOW' REQUEST                           
         BZ    VALHED10                                                         
         MVI   ERROR,NOWERR        FORCE JOB TO BE INPUT                        
         B     ERREND                                                           
         SPACE 1                                                                
* VALIDATE MEDIA GROUP                                                          
*                                                                               
VALHED10 MVI   OPTION,0            STOP DISPLAYING RECORD NAMES                 
         LA    R2,PROMGRH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED12                                                         
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PROJOBH+5,0         TEST IF JOB INPUT                            
         BNE   ERREND                                                           
         GOTO1 VALMG                                                            
         MVC   QMG,MGROUP                                                       
         SPACE 1                                                                
* VALIDATE MEDIA                                                                
*                                                                               
VALHED12 LA    R2,PROMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VALHED14                                                         
         MVI   ERROR,NOTMENMG                                                   
         CLI   PROMGRH+5,0         CANNOT HAVE BOTH MGR AND MEDIA               
         BNE   ERREND                                                           
         MVI   ERROR,NOTJBNME                                                   
         CLI   PROJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
         SPACE 1                                                                
* VALIDATE BILLING GROUP                                                        
*                                                                               
VALHED14 LA    R2,PROBGRH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED16                                                         
         GOTO1 ANY                                                              
         MVC   QBGR,WORK           SAVE BILLING GROUP                           
         CLI   WORK,C'*'           TEST FOR NEGATIVE FILTER                     
         BNE   VALHED16                                                         
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),2                                                          
         BL    ERREND                                                           
         MVC   QBGR,WORK+1                                                      
         NI    QBGR,X'FF'-X'40'    SET NEGATIVE BIT ON                          
*                                                                               
* VALIDATE BILLING TYPE                                                         
*                                                                               
VALHED16 LA    R2,PROBTYPH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED18                                                         
*                                                                               
         CLI   5(R2),1                                                          
         BNE   VALHED17                                                         
         CLI   8(R2),C'E'          TEST FOR ALL % OF EST BILLS                  
         BE    *+12                                                             
         CLI   8(R2),C'S'          TEST FOR ALL SPECIAL AMOUNT BILLS            
         BNE   VALHED17            NO-PROCEED WITH EDIT                         
         MVC   QBT,8(R2)                                                        
         B     VALHED18                                                         
*                                                                               
VALHED17 MVI   ERROR,INVALID                                                    
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM             BUILD SKELTON OPTION ELEMENT                 
         USING ACOPD,R6                                                         
         MVI   ACOPEL,ACOPELQ                                                   
         MVI   ACOPNUM,OPNBT       BILLING TYPE OPTION                          
         GOTO1 VVALOPT,DMCB,(R2),(R6)                                           
         BNE   ERREND                                                           
         ZIC   R1,ACOPLEN                                                       
         SH    R1,=Y(ACOPDATA-ACOPD+1)                                          
         EX    R1,*+8                                                           
         B     VALHED18                                                         
         MVC   QBT(0),ACOPDATA     EXTRACT OPTION DATA                          
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
* VALIDATE FILTER 1                                                             
*                                                                               
VALHED18 LA    R2,PROFIL1H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED19                                                         
         GOTO1 VALFIL,PARAS,QFILT,L'QFILT                                       
*                                                                               
* VALIDATE FILTER 2                                                             
*                                                                               
VALHED19 LA    R2,PROFIL2H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED20                                                         
         GOTO1 VALFIL,PARAS,QFILT2,L'QFILT2                                     
*                                                                               
* VALIDATE FILTER 3                                                             
*                                                                               
VALHED20 LA    R2,PROFIL3H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED21                                                         
         GOTO1 VALFIL,PARAS,QFILT3,L'QFILT3                                     
*                                                                               
* VALIDATE FILTER 4                                                             
*                                                                               
VALHED21 LA    R2,PROFIL4H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED2A                                                         
         GOTO1 VALFIL,PARAS,QFILT4,L'QFILT4                                     
*                                                                               
* VALIDATE FILTER 5                                                             
*                                                                               
VALHED2A LA    R2,PROFIL5H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED22                                                         
         GOTO1 VALFIL,PARAS,QFILT5,L'QFILT5                                     
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
         SPACE 1                                                                
* VALIDATE CLOSED JOBS OPTION                                                   
*                                                                               
VALHED22 MVI   QCLOSE,C'N'         DEFAULT IS TO EXCLUDE CLOSED JOBS            
         CLI   CALLER,X'33'        TEST CALLED BY JOB ELIST                     
         BNE   *+8                 NO                                           
         MVI   QCLOSE,C'Y'         YES-GIVE THEM A CLOSED JOB                   
         LA    R2,PROCLOSH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED24                                                         
         CLI   8(R2),C'N'                                                       
         BE    VALHED24                                                         
         CLI   8(R2),C'Y'                                                       
         BNE   INVEND                                                           
         MVI   QCLOSE,C'Y'                                                      
         SPACE 1                                                                
* VALIDATE PREPARER                                                             
*                                                                               
VALHED24 LA    R2,PROPREPH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED26                                                         
         GOTO1 ANY                                                              
         MVC   QPREP,WORK                                                       
         SPACE 1                                                                
* VALIDATE APPROVED BY                                                          
*                                                                               
VALHED26 LA    R2,PROAPBYH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED28                                                         
         GOTO1 ANY                                                              
         MVC   QAPPBY,WORK                                                      
         SPACE 1                                                                
* VALIDATE ESTIMATE TYPE                                                        
*                                                                               
VALHED28 LA    R2,PROETYH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED29                                                         
         MVC   QETYPE,8(R2)                                                     
         CLI   8(R2),ACEVPLN                                                    
         BE    VALHED29                                                         
         CLI   8(R2),ACEVREV                                                    
         BE    VALHED29                                                         
         MVI   ERROR,BADESTYP                                                   
         B     ERREND                                                           
         SPACE 1                                                                
* VALIDATE APPROVED FILTER                                                      
*                                                                               
VALHED29 LA    R2,PROAPFIH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED30                                                         
         MVC   QAPPFILT,8(R2)                                                   
         CLI   8(R2),C'A'          TEST A=APPROVED ONLY                         
         BE    VALHED30                                                         
         CLI   8(R2),C'U'          TEST U=UNAPPROVED ONLY                       
         BNE   INVEND                                                           
         SPACE 1                                                                
* VALIDATE JOB OPEN DATES FILTER                                                
*                                                                               
VALHED30 LA    R2,PROJOSH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED31                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QJOBST)                                       
         SPACE 1                                                                
VALHED31 LA    R2,PROJOEH                                                       
         MVC   QJOBND,=X'FFFFFF'                                                
         CLI   5(R2),0                                                          
         BE    VALHED32                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QJOBND)                                       
         MVI   ERROR,INVEBFRS                                                   
         CLC   QJOBST,QJOBND                                                    
         BH    ERREND                                                           
         SPACE 1                                                                
* VALIDATE ESTIMATE ADD DATES                                                   
*                                                                               
VALHED32 LA    R2,PROEASH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED33                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QEAST)                                        
         SPACE 1                                                                
VALHED33 LA    R2,PROEAEH                                                       
         MVC   QEAND,=X'FFFFFF'                                                 
         CLI   5(R2),0                                                          
         BE    VALHED34                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QEAND)                                        
         MVI   ERROR,INVEBFRS                                                   
         CLC   QEAST,QEAND                                                      
         BH    ERREND                                                           
         SPACE 1                                                                
* VALIDATE ACTIVITY DATES                                                       
*                                                                               
VALHED34 LA    R2,PROACTSH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED35                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QACTST)                                       
         SPACE 1                                                                
VALHED35 LA    R2,PROACTEH                                                      
         MVC   QACTND,=X'FFFFFF'                                                
         CLI   5(R2),0                                                          
         BE    VALHED36                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QACTND)                                       
         MVI   ERROR,INVEBFRS                                                   
         CLC   QACTST,QACTND                                                    
         BH    ERREND                                                           
         SPACE 1                                                                
* VALIDATE PREPARER DATES                                                       
*                                                                               
VALHED36 LA    R2,PROPRSH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED37                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QPREPST)                                      
         SPACE 1                                                                
VALHED37 LA    R2,PROPREH                                                       
         MVC   QPREPND,=X'FFFFFF'                                               
         CLI   5(R2),0                                                          
         BE    VALHED38                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QPREPND)                                      
         MVI   ERROR,INVEBFRS                                                   
         CLC   QPREPST,QPREPND                                                  
         BH    ERREND                                                           
         SPACE 1                                                                
* VALIDATE APPROVED DATES                                                       
*                                                                               
VALHED38 LA    R2,PROAPPSH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED39                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QAPPST)                                       
         SPACE 1                                                                
VALHED39 LA    R2,PROAPPEH                                                      
         MVC   QAPPND,=X'FFFFFF'                                                
         CLI   5(R2),0                                                          
         BE    VALHED40                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QAPPND)                                       
         MVI   ERROR,INVEBFRS                                                   
         CLC   QAPPST,QAPPND                                                    
         BH    ERREND                                                           
         SPACE 1                                                                
VALHED40 DS    0H                                                               
         SPACE 1                                                                
VALHEDX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE FILTER FIELD                                      
* AT ENTRY, R2=A(FLDH), P1=A(OUTPUT), P2=N'OUTPUT POSITIONS                     
*                                                                               
VALFIL   NTR1  ,                                                                
         L     R3,0(R1)            R3=A(OUTPUT)                                 
         L     R5,4(R1)            R5=N'OUTPUT POSITIONS                        
         MVI   ERROR,INVALID                                                    
         LA    R4,8(R2)            R4=A(INPUT)                                  
         ZIC   R6,5(R2)                                                         
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VALFIL2                                                          
         MVC   0(0,R3),SPACES      PRE-CLEAR OUTPUT TO SPACES                   
*                                                                               
VALFIL2  CLI   0(R4),C'*'          TEST FOR NEGATIVE FILTER                     
         BE    VALFIL4             YES                                          
         CLI   0(R4),C' '          TEST ANY FILTER IN POSITION                  
         BE    VALFIL6             YES                                          
         CLI   0(R4),C'.'          TEST FOR NO FILTER VALUE                     
         BE    VALFIL3                                                          
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
*                                                                               
VALFIL3  MVC   0(1,R3),0(R4)       SET VALUE                                    
         B     VALFIL6                                                          
*                                                                               
VALFIL4  LA    R4,1(R4)            NEXT INPUT CHARACTER                         
         SH    R6,=H'1'                                                         
         BZ    ERREND                                                           
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
         MVC   0(1,R3),0(R4)                                                    
         NI    0(R3),X'FF'-X'40'   TURN OFF UPPER CASE BIT                      
*                                                                               
VALFIL6  LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         SH    R6,=H'1'                                                         
         BZ    VALFILX             NO MORE INPUT TO EDIT                        
         BCT   R5,VALFIL2                                                       
         B     ERREND              EXCEEDED FILTER LIMIT                        
*                                                                               
VALFILX  B      XIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC FIELD                                   
*                                                                               
* AT ENTRY, R0=N'BYTES TO CHECK, R1=A(STRING TO CHECK)                          
*                                                                               
TSTAN    ST    RE,SAVERE                                                        
         MVI   ERROR,INVALID                                                    
*                                                                               
TSTAN1   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERREND                                                           
         B     TSTAN2                                                           
         CLI   0(R1),C'A'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'I'                                                       
         BNH   TSTAN2              CHARACTER IS BETWEEN A-I                     
         CLI   0(R1),C'J'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'R'                                                       
         BNH   TSTAN2                                                           
         CLI   0(R1),C'S'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'Z'                                                       
         BH    ERREND                                                           
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   R0,TSTAN1                                                        
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO READ THE FILE                             *                    
************************************************************                    
         SPACE 1                                                                
RDF      NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),CUL     READ COMPANY RECORD                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         GOTO1 SAVREC,ACOMP                                                     
*                                                                               
RDF2     MVC   ACKEYACC(3),CUL     GET THE LEDGER RECORD                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 SAVREC,ALEDG                                                     
*                                                                               
RDF4     OC    QCLI,QCLI           TEST FOR CLIENT REQUEST                      
         BZ    RDF12               NO-GO RIGHT INTO FILE                        
*                                                                               
         MVC   ACKEYACC+3(L'QCLI),QCLI                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BAS   RE,CLIENT                                                        
*                                                                               
RDF6     OC    QPROD,QPROD         TEST PRODUCT REQUESTED                       
         BZ    RDF12               NO-READ NEXT RECORD                          
*                                                                               
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         MVC   0(L'QPROD,R1),QPROD                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BAS   RE,PRODUCT                                                       
*                                                                               
RDF8     OC    QJOB,QJOB           TEST ONE JOB REQUESTED                       
         BZ    RDF12               NO-READ NEXT RECORD                          
*                                                                               
         ZIC   R1,LCLIPRO                                                       
         LA    R1,ACKEYACC+3(R1)                                                
         MVC   0(L'QJOB,R1),QJOB                                                
*                                                                               
RDF10    OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     RDF20                                                            
*                                                                               
RDF12    OI    DMINBTS,X'08'                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
RDF20    ZIC   R1,EXLEN                                                         
         EX    R1,COMPKEY                                                       
         BNE   RDFX                END OF FILE                                  
*                                                                               
RDF22    TM    ACSTATUS,X'80'                                                   
         BO    RDF30                                                            
         ZIC   R5,LCLI                                                          
         LA    R5,ACKEYACC+3(R5)                                                
         CLI   0(R5),C' '          TEST FOR CLIENT RECORD                       
         BH    RDF24               NO                                           
         BAS   RE,CLIENT                                                        
         BE    RDF12               GET NEXT RECORD                              
         MVI   0(R5),X'FF'         FORCE NEXT CLIENT                            
         B     RDF10                                                            
*                                                                               
RDF24    ZIC   R5,LCLIPRO                                                       
         LA    R5,ACKEYACC+3(R5)                                                
         CLI   0(R5),C' '                                                       
         BH    RDF26               ITS A JOB                                    
         BAS   RE,PRODUCT                                                       
         BE    RDF12                                                            
         MVI   0(R5),X'FF'         FORCE NEXT PRODUCT                           
         B     RDF10                                                            
*                                                                               
RDF26    BAS   RE,JOB                                                           
*                                                                               
RDF30    MVI   ACKEYWRK,X'FF'      FORCE NEXT ACCOUNT                           
         B     RDF10                                                            
*                                                                               
RDFX     B     XIT                                                              
         SPACE 2                                                                
COMPKEY  CLC   ACKEYD(0),KEYSAVE                                                
         EJECT                                                                  
****************************************************************                
* HOOK ROUTINES TO PROCESS RECORDS PASSED BY RDF               *                
****************************************************************                
         SPACE 1                                                                
CLIENT   NTR1  ,                                                                
         OC    QBGR,QBGR           TEST FOR BILLING GROUP FILTER                
         BZ    CLIENT2                                                          
*                                                                               
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   NOTOKXIT                                                         
         USING ACPROFD,R6                                                       
         TM    QBGR,X'40'          TEST FOR POSITIVE FILTER                     
         BO    CLIENT1                                                          
*                                                                               
         MVC   DUB(L'QBGR),QBGR                                                 
         OI    DUB,X'40'                                                        
         CLC   ACPRGRUP,DUB                                                     
         BE    NOTOKXIT                                                         
         B     CLIENT2                                                          
*                                                                               
CLIENT1  CLC   QBGR,ACPRGRUP                                                    
         BNE   NOTOKXIT                                                         
         DROP  R6                                                               
*                                                                               
CLIENT2  GOTO1 SAVREC,ACLI                                                      
         GOTO1 SETCLI                                                           
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   CLINAME,WORK                                                     
         B     OKXIT                                                            
         SPACE 1                                                                
PRODUCT  NTR1  ,                                                                
         GOTO1 SAVREC,APROD                                                     
         GOTO1 SETPROD                                                          
         BAS   RE,FILOFF                                                        
         BNE   NOTOKXIT                                                         
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   PRODNAME,WORK                                                    
         MVI   FORCEHED,C'Y'                                                    
         B     OKXIT                                                            
         SPACE 1                                                                
JOB      NTR1  ,                                                                
         GOTO1 SETJOB                                                           
         TM    JOBJSTAT,ACJBNEWQ   TEST FOR NEW ESTIMATE JOB                    
         BZ    JOBX                NO                                           
         TM    JOBSTAT,X'40'       TEST FOR CLOSED JOB                          
         BZ    *+12                                                             
         CLI   QCLOSE,C'N'         TEST TO EXCLUDE CLOSED JOBS                  
         BE    JOBX                                                             
         BAS   RE,FILMED           FILTER ON MEDIA/MEDIA GROUP                  
         BNE   JOBX                                                             
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
JOB2     MVC   BYTE,QFILT          FILTER 1                                     
         GOTO1 APPFIL,EFF1                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT2         FILTER 2                                     
         GOTO1 APPFIL,EFF2                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT3         FILTER 3                                     
         GOTO1 APPFIL,EFF3                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT4         FILTER 4                                     
         GOTO1 APPFIL,EFF4                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT5         FILTER 5                                     
         GOTO1 APPFIL,EFF5                                                      
         BNE   JOBX                                                             
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
JOB4     MVI   ELCODE,ACJBELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACJOBD,R6                                                        
         MVC   JOBOPEN,ACJBOPND    EXTRACT OPEN DATE                            
         OC    ACJBOPND,ACJBOPND   TEST FOR AN OPEN DATE                        
         BNZ   *+10                YES                                          
         MVC   JOBOPEN,ACJBSTRT    NO-USE START DATE                            
         CLC   JOBOPEN,QJOBST      APPLY OPEN DATE FILTERS                      
         BL    JOBX                REJECT IT-BEFORE PERIOD                      
         CLC   JOBOPEN,QJOBND                                                   
         BH    JOBX                                                             
*                                                                               
JOB6     BAS   RE,RDOPT            GET THE OPTIONS                              
         CLI   QBT,0               TEST FOR BILLING TYPE FILTER                 
         BE    JOB8                NO                                           
*                                                                               
         CLC   QBT,GOBILTYP                                                     
         BNE   JOBX                                                             
         OC    QBILAMT,QBILAMT     TEST FOR AMOUNTS ALSO                        
         BZ    JOB8                NONE-SO OK                                   
         CLC   QBILAMT,GOBILAM1                                                 
         BNE   JOBX                                                             
*                                                                               
JOB8     GOTO1 SAVREC,AJOB                                                      
         BAS   RE,PRINT                                                         
         L     RE,AIO                                                           
         MVC   KEY,0(RE)           RE-READ JOB                                  
         GOTO1 READ                                                             
*                                                                               
JOBX     B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE A RECORD IN A BUFFER, AT ENTRY, R1=A(BUFF ADCON)          
*                                                                               
SAVREC   NTR1                                                                   
         L     RE,0(R1)            RE=A(BUFFER)                                 
         L     R4,AIO              R4=A(RECORD TO BE SAVED)                     
         USING ACKEYD,R4                                                        
         LH    R1,ACLENGTH                                                      
         LA    RF,1(R1)                                                         
         LR    R0,R4                                                            
         MVCL  RE,R0               SAVE THE RECORD                              
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON MEDIA GROUP AND MEDIA                                
*                                                                               
* ON EXIT, CC=EQ IF OK, CC=NEQ TO SKIP JOB                                      
*                                                                               
FILMED   NTR1  ,                                                                
         CLI   QMG,0                                                            
         BNE   FILMED2                                                          
         CLI   QMED,0                                                           
         BE    FILMEDY                                                          
*                                                                               
FILMED1  CLC   QMED,JOBNUM                                                      
         BE    FILMEDY                                                          
         B     FILMEDN                                                          
*                                                                               
FILMED2  LA    R6,MEDBUFF                                                       
*                                                                               
FILMED3  CLI   0(R6),0             TEST FOR EOB                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   JOBNUM(1),0(R6)                                                  
         BE    FILMED4                                                          
         LA    R6,2(R6)                                                         
         B     FILMED3                                                          
*                                                                               
FILMED4  CLC   QMG,1(R6)           TEST FOR MATCH ON MG                         
         BE    FILMEDY                                                          
         B     FILMEDN                                                          
*                                                                               
FILMEDY  B     OKXIT                                                            
*                                                                               
FILMEDN  B     NOTOKXIT                                                         
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON THE OFFICE AND OFFICE GROUP                          
*                                                                               
FILOFF   NTR1  ,                                                                
         CLI   QOG,0               TEST FOR OFFICE GROUP FILTER                 
         BNE   FILOFF2             YES                                          
         OC    QOFF,QOFF           TEST FOR OFFICE FILTER                       
         BZ    FILOFFY             NO-TAKE THE RECORD                           
*                                                                               
         CLC   EFFOFFC,QOFF        MATCH ON FILTER                              
         BE    FILOFFY                                                          
         B     FILOFFN                                                          
*                                                                               
FILOFF2  LA    R2,OFFBUFF          USE MY OFFICE BUFFER                         
*                                                                               
FILOFF3  CLI   0(R2),0             TEST FOR EOT                                 
         BE    FILOFFN             NOT THE REQUESTED OFFICE GROUP               
         CLC   EFFOFFC,0(R2)       MATCH ON OFFICE                              
         BE    FILOFF4                                                          
         LA    R2,3(R2)                                                         
         B     FILOFF3                                                          
*                                                                               
FILOFF4  CLC   QOG,2(R2)           MATCH ON OFFICE GROUP                        
         BE    FILOFFY                                                          
         B     FILOFFN                                                          
*                                                                               
FILOFFY  B     OKXIT                                                            
*                                                                               
FILOFFN  B     NOTOKXIT                                                         
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY FILTER, AT ENTRY, R1=A(JOB FILTER VALUE) AND             
* BYTE CONTAINS REQUESTED FILTER VALUE                                          
*                                                                               
APPFIL   CLI   BYTE,0              TEST FOR NO REQUESTED FILTER                 
         BER   RE                  YES-EXIT WITH CC=EQ                          
         CLI   BYTE,C' '           TEST FOR BLANK=TAKE EVERYTHING               
         BE    APPFILY                                                          
         CLI   BYTE,C'.'           TEST FOR NO FILTER ON JOB                    
         BNE   APPFIL2                                                          
         CLI   0(R1),C' '          FILTER POSITION CANNOT BE A VALUE            
         BH    APPFILN                                                          
         B     APPFILY                                                          
*                                                                               
APPFIL2  TM    BYTE,X'40'          TEST FOR POSITIVE FILTER                     
         BZ    APPFIL4                                                          
         CLC   BYTE,0(R1)          TEST FOR MATCH ON FILTER                     
         BE    APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFIL4  OI    BYTE,X'40'          RESTORE UPPER CASE BIT                       
         CLC   BYTE,0(R1)          TEST FOR DIFFERENCE                          
         BNE   APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFILY  CR    RB,RB                                                            
         B     APPFILX                                                          
*                                                                               
APPFILN  LTR   RB,RB                                                            
*                                                                               
APPFILX  BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO READ THE JOB'S OPTIONS                                         
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,CUL                                                     
         MVC   GOACOMP,ACOMP                                                    
         MVC   GOALEDG,ALEDG                                                    
         MVC   GOACLI,ACLI                                                      
         MVC   GOAPRO,APROD                                                     
         MVC   GOAJOB,AJOB                                                      
*                                                                               
         LA    R1,OPTBUFF                                                       
         ST    R1,GOABUFF                                                       
         LA    R1,L'OPTBUFF                                                     
         ST    R1,GOLBUFF                                                       
*                                                                               
         MVC   GOACOVL,COVAIL                                                   
         MVC   GOABINSR,BINSRCH                                                 
*                                                                               
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVI   GOANYWC,C'N'                                                     
         MVC   GOAKEY,AIO                                                       
*                                                                               
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO LOAD IN THE JOBBER TABLES                                      
*                                                                               
LOAD     ST    RE,SAVERE                                                        
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'        LOAD IN JOBBER TABLE PHASE                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         LM    R0,R1,0(RF)                                                      
         AR    R0,RF               FORM ADDRESS OF COLUMN TABLE                 
         STM   R0,R1,ACOLTAB                                                    
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PRINT JOB DATA                                                 
*                                                                               
PRINT    NTR1  ,                                                                
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVC   PRTJOB,JOBNUM                                                    
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   PRTNAME,WORK                                                     
         GOTO1 DATCON,DMCB,(1,JOBOPEN),(8,PRTOPEN)                              
         GOTO1 DATCON,DMCB,(1,JOBCLOSE),(8,PRTCLOSE)                            
         MVC   PRTNAE(4),=C'NAE='                                               
         MVC   PRTNAE+4(1),GONEEDAE NEED APPROVED ESTIMATE VALUE                
*                                                                               
PRINT2   LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         MVC   AIO,AIO2                                                         
         NI    DMINBTS,X'FF'-X'08'                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     PRINT4                                                           
*                                                                               
PRINT3   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PRINT4   CLC   ACEVKEY(ACEVTYPE-ACEVKEY),KEYSAVE  TEST SAME JOB                 
         BNE   PRINT8                                                           
         LA    RF,ACEVKEY+(EVEKWC-EVEKEY)                                       
         OC    0(L'EVEKWC,RF),0(RF) IS IT A TIME EST RECORD?                    
         BNZ   PRINT3              YES, SO READ NEXT RECORD                     
*                                                                               
         BAS   RE,GETEST           EXTRACT ESTIMATE DATA                        
         CLI   QETYPE,0            TEST FOR ESTIMATE TYPE FILTER                
         BE    *+14                                                             
         CLC   QETYPE,ESTTYPE                                                   
         BNE   PRINT3              SKIP THIS ESTIMATE TYPE                      
*                                                                               
         CLI   QAPPFILT,C'A'       TEST FOR APPROVED ONLY                       
         BNE   *+14                                                             
         OC    APPDATE,APPDATE     TEST FOR APPROVED DATE                       
         BZ    PRINT3              NONE-COULD NOT BE APPROVED                   
         CLI   QAPPFILT,C'U'       TEST UNAPPROVED ONLY                         
         BNE   *+14                                                             
         OC    APPDATE,APPDATE     TEST IF APPROVED                             
         BNZ   PRINT3              YES-SKIP                                     
*                                                                               
         OC    QPREP,QPREP                                                      
         BZ    *+14                                                             
         CLC   QPREP,PREPBY                                                     
         BNE   PRINT3              REJECT ESTIMATE                              
*                                                                               
         OC    QAPPBY,QAPPBY       TEST FOR APPROVED BY FILTER                  
         BZ    *+14                NO                                           
         CLC   QAPPBY,APPBY                                                     
         BNE   PRINT3                                                           
*                                                                               
         CLC   ESTADD,QEAST                                                     
         BL    PRINT3                                                           
         CLC   ESTADD,QEAND                                                     
         BH    PRINT3                                                           
*                                                                               
         CLC   ESTLAST,QACTST      APPLY ACTIVITY FILTER                        
         BL    PRINT3                                                           
         CLC   ESTLAST,QACTND                                                   
         BH    PRINT3                                                           
*                                                                               
         CLC   PREPDATE,QPREPST                                                 
         BL    PRINT3                                                           
         CLC   PREPDATE,QPREPND                                                 
         BH    PRINT3                                                           
*                                                                               
         CLC   APPDATE,QAPPST                                                   
         BL    PRINT3                                                           
         CLC   APPDATE,QAPPND                                                   
         BH    PRINT3                                                           
*                                                                               
PRINT5   BAS   RE,BLDCOLS          BUILD JOBBER COLS FOR THIS EST               
         BAS   RE,LOOK             LOOK UP THIS ESTS NET AND GROSS              
         BAS   RE,DISEST           DISPLAY/REPORT THIS ESTIMATE                 
*                                                                               
PRINT6   MVI   ALLOWLIN,3                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)                                                   
         B     PRINT3              READ NEXT RECORD                             
*                                                                               
PRINT8   MVC   AIO,AIO1            RESTORE IO POINTER                           
*                                                                               
PRINTX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EXTRACT VALUES FROM THE ESTIMATE RECORD                        
*                                                                               
GETEST   NTR1  ,                                                                
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
         MVC   ESTTYPE,ACEVTYPE                                                 
         MVC   ESTVERS,ACEVERS                                                  
         ZAP   NETEST,=P'0'                                                     
         ZAP   GROSSEST,=P'0'                                                   
         MVC   ESTNAM1,SPACES                                                   
         MVC   ESTNAM2,SPACES                                                   
         MVC   PREPBY,SPACES                                                    
         XC    PREPDATE,PREPDATE                                                
         MVC   APPBY,SPACES                                                     
         XC    APPDATE,APPDATE                                                  
         LA    R6,ACRECORD                                                      
         SR    R0,R0                                                            
*                                                                               
GETEST2  CLI   0(R6),0                                                          
         BE    GETESTX                                                          
         CLI   0(R6),ACENELQ       TEST FOR ESTIMATE NAME ELEMENT               
         BE    GETEST6             YES                                          
         CLI   0(R6),ACEUELQ                                                    
         BE    GETEST8                                                          
         CLI   0(R6),ACEAELQ                                                    
         BE    GETEST10                                                         
         CLI   0(R6),ACEPELQ       TEST FOR ESTIMATE PREPARER ELEMENT           
         BE    GETEST12                                                         
*                                                                               
GETEST3  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETEST2                                                          
*                                                                               
         USING ACEND,R6                                                         
GETEST6  LA    R3,ESTNAM1                                                       
         CLI   ACENNUM,1                                                        
         BE    *+8                                                              
         LA    R3,ESTNAM2                                                       
         MVC   LISTAR,SPACES                                                    
         ZIC   R1,ACENLEN                                                       
         SH    R1,=Y(ACENAME-ACEND+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),ACENAME                                                
         MVC   0(L'ESTNAM1,R3),LISTAR                                           
         B     GETEST3                                                          
         SPACE 1                                                                
         USING ACEUD,R6                                                         
GETEST8  MVC   ESTADD,ACEUADD                                                   
         MVC   ESTLAST,ACEULAST                                                 
         B     GETEST3                                                          
         SPACE 1                                                                
         USING ACEAD,R6                                                         
GETEST10 MVC   APPBY,ACEAPPBY                                                   
         MVC   APPDATE,ACEADATE                                                 
         B     GETEST3                                                          
         SPACE 1                                                                
         USING ACEPD,R6                                                         
GETEST12 MVC   PREPBY,ACEPREP                                                   
         MVC   PREPDATE,ACEPDATE                                                
         B     GETEST3                                                          
         SPACE 1                                                                
GETESTX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE JOBCOLS COLUMN LIST FOR A SPECIFIC ESTIMATE          
*                                                                               
BLDCOLS  NTR1  ,                                                                
         XC    LISTAR,LISTAR       DUMMY FIELD HEADER                           
         LA    R3,LISTAR+8         WITH FIELD DATA AFTERWARD                    
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
         MVC   0(1,R3),ACEVTYPE    BUILD NET ESTIMATE EXPRESSION                
         LA    R3,1(R3)                                                         
         ZIC   R0,ACEVERS                                                       
         EDIT  (R0),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)            ATTACH GROSS ESTIMATE EXPRESSION             
         MVC   0(1,R3),ACEVTYPE                                                 
         LA    R3,1(R3)                                                         
         ZIC   R0,ACEVERS                                                       
         EDIT  (R0),(3,(R3)),ALIGN=LEFT,TRAIL=C'G'                              
         AR    R3,R0                                                            
         LA    RE,LISTAR+8                                                      
         SR    R3,RE               COMPUTE DATA LENGTH                          
         STC   R3,LISTAR+5                                                      
         LA    R3,8(R3)                                                         
         STC   R3,LISTAR                                                        
*                                                                               
BLDCOLS2 GOTO1 VJOBCOL,DMCB,LISTAR,COLIST,ACOMFACS                              
         CLI   4(R1),0             TEST FOR ERROR                               
         BNE   BLDCOLSX            NO                                           
         DC    H'0'                                                             
*                                                                               
BLDCOLSX B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO LOOK UP THE NET AND GROSS FOR THE JOB                          
*                                                                               
* CALLED BY PRINT WITH ESTIMATE RECORD ADDRESSED BY AIO                         
* ROUTINE USES BLOCK TO BUILD JOBBLOCK AREA                                     
*                                                                               
LOOK     NTR1  ,                                                                
         LA    R6,BLOCK                                                         
         USING JBLOCKD,R6                                                       
         LR    RE,R6                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   JBAJOB,AJOB                                                      
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAIO,AIO           PASS ESTIMATE RECORD TO JOBBER               
*                                                                               
         LA    R1,COLIST                                                        
         ST    R1,JBACOLS                                                       
*                                                                               
         LA    R1,KEY                                                           
         ST    R1,JBAKEY                                                        
*                                                                               
         LA    R1,GOBLOCK                                                       
         ST    R1,JBAGOBLK                                                      
*                                                                               
         MVC   JBGETOPT,GETOPT                                                  
         MVI   JBSELFUN,JBGETEST   PASSING ESTIMATE TO JOBBER                   
         MVC   JBACOLTB(16),ACOLTAB                                             
         GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOK2    L     R5,JBACOLTB                                                      
         USING JBCOLD,R5                                                        
         ZAP   NETEST,JBCOLVAL                                                  
         ZAP   GROSSEST,JBCOLVAL+L'JBCOLVAL                                     
*                                                                               
LOOKX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY ESTIMATE DATA                                          
*                                                                               
DISEST   NTR1  ,                                                                
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
         LA    R2,P                                                             
         USING PRTD,R2                                                          
*                                                                               
         MVC   PRTEST,ESTTYPE                                                   
         ZIC   R0,ESTVERS                                                       
         EDIT  (R0),(3,PRTEST+1),ALIGN=LEFT                                     
*                                                                               
DISEST2  MVC   PRTENAM1,ESTNAM1                                                 
         MVC   PRTENAM2,ESTNAM2                                                 
         MVC   PRTAPPBY,APPBY                                                   
         OC    APPDATE,APPDATE                                                  
         BZ    DISEST4                                                          
         GOTO1 DATCON,DMCB,(1,APPDATE),(8,PRTAPPDT)                             
*                                                                               
DISEST4  MVC   PRTPRBY,PREPBY                                                   
         OC    PREPDATE,PREPDATE                                                
         BZ    DISEST6                                                          
         GOTO1 DATCON,DMCB,(1,PREPDATE),(8,PRTPRDT)                             
*                                                                               
DISEST6  GOTO1 DATCON,DMCB,(1,ESTADD),(8,PRTADD)                                
         GOTO1 (RF),(R1),(1,ESTLAST),(8,PRTLAST)                                
*                                                                               
DISEST8  EDIT  NETEST,(14,PRTNET),2,ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES           
         EDIT  GROSSEST,(14,PRTGRS),2,ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES         
*                                                                               
DISESTX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* HOOK ROUTINE FOR PRINTING                                                     
*                                                                               
HOOK     NTR1  ,                                                                
         MVC   H4+10(L'CLICODE),CLICODE SHOW CLIENT CODE                        
         MVC   H4+18(20),CLINAME   SHOW CLIENT NAME                             
         MVC   H5+10(L'PRODCODE),PRODCODE                                       
         MVC   H5+18(L'PRODNAME),PRODNAME                                       
*                                                                               
HOOK2    ICM   R3,15,ABOX          LOOK FOR BOXES                               
         BZ    HOOKX               NONE                                         
*                                                                               
         USING BOXD,R3                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
*                                                                               
         LA    RE,BOXCOLS                                                       
         MVI   PRTLBOX-PRTD(RE),C'L'                                            
         MVI   PRTBOX1-PRTD(RE),C'C'                                            
         MVI   PRTBOX2-PRTD(RE),C'C'                                            
         MVI   PRTBOX3-PRTD(RE),C'C'                                            
         MVI   PRTBOX4-PRTD(RE),C'C'                                            
         MVI   PRTBOX5-PRTD(RE),C'C'                                            
         MVI   PRTBOX6-PRTD(RE),C'C'                                            
         MVI   PRTBOX7-PRTD(RE),C'C'                                            
         MVI   PRTRBOX-PRTD(RE),C'R'                                            
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
NOTOKXIT LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
INVEND   MVI   ERROR,INVALID       INVALID EXIT                                 
         B     ERREND                                                           
         SPACE 1                                                                
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SPECS FOR ELIST REPORT                                                        
*                                                                               
HEDSPECS DS    0D                                                               
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,47,C'JOB ELIST REPORT'                                        
         SSPEC H2,47,C'----------------'                                        
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,85,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H4,85,REPORT                                                     
         SSPEC H4,98,PAGE                                                       
         SSPEC H5,2,C'PRODUCT'                                                  
*                                                                               
         SSPEC H8,3,C'JOB'                                                      
         SSPEC H8,10,C'------JOB NAME------'                                    
         SSPEC H8,31,C'EST#'                                                    
         SSPEC H8,36,C'ESTIMATE NAME'                                           
         SSPEC H8,51,C'DATE'                                                    
         SSPEC H8,60,C'----PREPARED-----'                                       
         SSPEC H8,78,C'-------ACTIVITY--------'                                 
         SSPEC H8,102,C'----APPROVAL-----'                                      
*                                                                               
         SSPEC H9,10,C'OPEN'                                                    
         SSPEC H9,20,C'CLOSE'                                                   
         SSPEC H9,51,C'ADDED'                                                   
         SSPEC H9,60,C'BY'                                                      
         SSPEC H9,69,C'ON'                                                      
         SSPEC H9,78,C'NET/GROSS'                                               
         SSPEC H9,93,C'LAST CHG'                                                
         SSPEC H9,102,C'BY'                                                     
         SSPEC H9,111,C'ON'                                                     
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
OPTBUFF  DS    XL3000                                                           
         ORG   OPTBUFF                                                          
         DC    (L'OPTBUFF)X'00'                                                 
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACPROWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*DDBIGBOX                                                                       
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
LOCAL    DS    0C                                                               
SAVERE   DS    A                                                                
QOG      DS    CL1                                                              
QOFF     DS    CL2                                                              
QCLI     DS    CL(L'CLICODE)                                                    
QPROD    DS    CL(L'PRODCODE)                                                   
QJOB     DS    CL(L'JOBNUM)                                                     
QMG      DS    CL1                                                              
QMED     DS    CL1                                                              
QBGR     DS    CL3                                                              
QBT      DS    CL1                                                              
QBILAMT  DS    CL12                                                             
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
QFILT    DS    CL1                                                              
QFILT2   DS    CL1                                                              
QFILT3   DS    CL1                                                              
QFILT4   DS    CL1                                                              
QFILT5   DS    CL1                                                              
*--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---            
QCLOSE   DS    CL1                                                              
QETYPE   DS    C                                                                
QAPPFILT DS    CL1                                                              
QPREP    DS    CL(L'ACEPREP)                                                    
QAPPBY   DS    CL(L'ACEAPPBY)                                                   
QJOBST   DS    PL3                                                              
QJOBND   DS    PL3                                                              
QEAST    DS    PL3                                                              
QEAND    DS    PL3                                                              
QACTST   DS    PL3                                                              
QACTND   DS    PL3                                                              
QPREPST  DS    PL3                                                              
QPREPND  DS    PL3                                                              
QAPPST   DS    PL3                                                              
QAPPND   DS    PL3                                                              
*                                                                               
EXLEN    DS    X                                                                
*                                                                               
ACOMP    DS    A                                                                
ALEDG    DS    A                                                                
ACLI     DS    A                                                                
APROD    DS    A                                                                
AJOB     DS    A                                                                
*                                                                               
ACOLTAB  DS    V                                                                
LCOLTAB  DS    V                                                                
AOPVTAB  DS    V                                                                
LOPVTAB  DS    V                                                                
COLIST   DS    XL80                                                             
*                                                                               
CLINAME  DS    CL20                                                             
PRODNAME DS    CL20                                                             
JOBOPEN  DS    PL3                                                              
ESTTYPE  DS    C                                                                
ESTVERS  DS    X                                                                
NETEST   DS    PL6                                                              
GROSSEST DS    PL6                                                              
ESTNAM1  DS    CL14                                                             
ESTNAM2  DS    CL14                                                             
ESTADD   DS    PL3                                                              
ESTLAST  DS    PL3                                                              
APPBY    DS    CL(L'ACEAPPBY)                                                   
APPDATE  DS    PL3                                                              
PREPBY   DS    CL(L'ACEPREP)                                                    
PREPDATE DS    PL3                                                              
*                                                                               
OFFBUFF  DS    XL110                                                            
MEDBUFF  DS    XL100                                                            
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
         EJECT                                                                  
* DSECT TO COVER OPTION CONVERSION REQUEST SCREEN                               
*                                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROD5D                                                       
         SPACE  2                                                               
MOREDATA DS    0D                                                               
         DS    CL((SAVAREA-MOREDATA)-(*-MOREDATA)) SPARE                        
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    C                   SPARE                                        
PRTLBOX  DS    C                   LEFT BOX POSITION                            
PRTJOB   DS    CL6                 JOB NUMBER                                   
PRTBOX1  DS    C                   CENTER BOX POSITION                          
PRTNAME  DS    CL20                JOB NAME                                     
PRTBOX2  DS    C                                                                
PRTEST   DS    CL4                 ESTIMATE TYPE/VERSION                        
PRTBOX3  DS    C                                                                
PRTENAM1 DS    CL14                ESTIMATE NAME 1                              
PRTBOX4  DS    C                                                                
PRTADD   DS    CL8                 ESTIMATE ADD DATE                            
PRTBOX5  DS    C                                                                
PRTPRBY  DS    CL8                 PREPARER                                     
         DS    C                   SPARE                                        
PRTPRDT  DS    CL8                 PREPARED DATE                                
PRTBOX6  DS    C                                                                
PRTNET   DS    CL14                NET ESTIMATE AMOUNT                          
         DS    C                   SPARE                                        
PRTLAST  DS    CL8                 LAST CHANGE DATE                             
PRTBOX7  DS    C                                                                
PRTAPPBY DS    CL8                 APPROVED BY                                  
         DS    C                   SPARE                                        
PRTAPPDT DS    CL8                 APPROVED DATE                                
PRTRBOX  DS    C                   RIGHT BOX POSITION                           
*                                                                               
* SECOND PRINT LINE                                                             
*                                                                               
         ORG   PRTNAME+L'P                                                      
PRTOPEN  DS    CL8                 JOB OPEN DATE                                
         DS    CL2                                                              
PRTCLOSE DS    CL8                 JOB CLOSE DATE                               
         ORG   PRTENAM1+L'P                                                     
PRTENAM2 DS    CL14                ESTIMATE NAME 2                              
         ORG   PRTOPEN+L'P                                                      
PRTNAE   DS    CL5                 NEED APPROVED ESTIMATE VALUE                 
         ORG   PRTNET+L'P                                                       
PRTGRS   DS    CL14                GROSS AMOUNT                                 
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACPRO35   04/11/07'                                      
         END                                                                    
