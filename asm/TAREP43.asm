*          DATA SET TAREP43    AT LEVEL 055 AS OF 05/01/02                      
*PHASE T70343A,*                                                                
*INCLUDE ADSCAN                                                                 
         TITLE 'T70343 - NEW EMPLOYEE DISK REPORT'                              
T70343   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70343,R6                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TUD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE                                                                  
VKEY     NTR1                                                                   
         LR    RE,R7               A(LOCAL WORKING STORAGE)                     
         LA    RF,TULNQ                                                         
         XCEFL ,                                                                
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
*                                                                               
         LA    R2,SNEPERH          VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   TUPER,PVALCPER      SAVE PRINTABLE PERIOD                        
         MVC   TIQPSTR,PVALPSTA                                                 
         MVC   TIQPEND,PVALPEND                                                 
         MVI   TIQDTYPE,TIQDCHK    FILTER ON CHECK DATE                         
         DROP  R3                                                               
*                                                                               
         LA    R2,SNEUNITH         TAX UNIT                                     
         GOTO1 ANY                                                              
         MVC   TIFUNIT,WORK                                                     
         MVI   TIFTUNIT,TIFTWRK    BASED ON WORK                                
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   FLDINV                                                           
*                                                                               
         LA    R2,SNEEMPH          EMPLOYER                                     
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         MVC   TIFEMP,TGEMP                                                     
*                                                                               
VK40     BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS FIELD                                
         SPACE                                                                  
VALOPT   NTR1                                                                   
         LA    R2,SNEOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
*                                                                               
VOPT10   CLC   =C'TAPE',SCDATA1                                                 
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    NEOPTS,NENOTAPE     DON'T GENERATE A TAPE                        
         B     VOPTNEXT                                                         
*                                                                               
VOPT20   CLC   =C'TRACE',SCDATA1                                                
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    NEOPTS,NETRACE      TRACE RECORDS                                
         B     VOPTNEXT                                                         
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO CALL SYSIO AND PRINT REPORT                           
         SPACE                                                                  
PREP     NTR1                                                                   
         MVI   ACTVSW,C'N'         NOTHING ACTIVE                               
         ZAP   COUNT,=P'0'         RECORD COUNTER FOR EMPLOYER                  
         ZAP   TOTCOUNT,=P'0'      RECORD COUNTER FOR TAPE                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   PRNTBL,TPRNTBL      A(PRNTBL)                                    
         DROP  R2                                                               
*                                                                               
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
*                                                                               
         MVI   TIFCUR,C'U'         READ US DOLLARS ONLY                         
         MVI   TIREAD,TLCKYCDQ     READ CHECK RECORDS                           
         OI    TIQFLAGS,TIQFDIR    PASS DIRECTORY HOOKS                         
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         CLI   ACTVSW,C'N'         IF PROCESSED AN INVOICE                      
         BE    PREPX                                                            
         XC    SVEMP,SVEMP         CLEAR SAVED INFO                             
         BAS   RE,GETSRT           GET SORT RECORDS & PUT TO TAPE               
         GOTO1 SORTER,DMCB,=C'END' END SORT                                     
         BAS   RE,CLOSTAPE         CLOSE THE TAPE                               
*                                                                               
         OC    TIFEMP,TIFEMP       IF ALL EMPLOYERS                             
         BNZ   PREPX                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         EDIT  TOTCOUNT,(8,P+1),ALIGN=LEFT                                      
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(17,R1),=CL17'EMPLOYEES ON TAPE'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET HOOK FROM SYSIO AND WRITE SORT RECORDS            
         SPACE                                                                  
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      IF DIRECTORY HOOK                            
         BNE   IOHOOK10                                                         
*                                                                               
         OC    SVTIKEY,SVTIKEY     FOR NEW EMP/SSN                              
         BZ    *+14                                                             
         CLC   SVTIKEY,TIKEY                                                    
         BE    NO                                                               
*                                                                               
         MVC   SVTIKEY,TIKEY       SAVE UP TO DATE                              
         LA    R6,TIKEY            R6=A(SYSIO KEY)                              
         USING TLCKPD,R6                                                        
         CLI   TLCKYW4T,TAW4TYIN   ONLY PROCESS INDIVIDUALS                     
         BNE   NO                                                               
*                                                                               
         LA    R3,SRTREC           R3=A(SORT RECORD)                            
         USING SORTD,R3                                                         
         MVC   SRTEMP,TIEMP        EMPLOYER CODE                                
         MVC   SRTSSN,TISSN        EMPLOYEE SSN                                 
*                                                                               
         GOTO1 MYTRACE,DMCB,=C'SORT IN',SORTD,SRTLNQ                            
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SRTREC                                       
         MVI   ACTVSW,C'Y'         SET SORT SWITCH ACTIVE                       
         B     NO                                                               
*                                                                               
IOHOOK10 CLI   TIMODE,PROCREC      IF RECORD HOOK                               
         BNE   IOHOOKX                                                          
         DC    H'0'                SHOULD NEVER COME HERE                       
IOHOOKX  B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*              ROUTINE GETS SORTER RECORDS/PUTS TO TAPE/PRINTS REPORT           
         SPACE                                                                  
GETSRT   NTR1                                                                   
GETSRT2  LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         LA    R0,NCHUNKS          R0=A(NUMBER OF CHUNKS)                       
*                                                                               
GETSRT5  GOTO1 SORTER,DMCB,=C'GET'                                              
         USING SORTD,R3                                                         
         ICM   R3,15,4(R1)         R3=A(SORT RECORD)                            
         BZ    GETSRTX                                                          
         GOTO1 MYTRACE,DMCB,=C'SORT OUT',SORTD,SRTLNQ                           
*                                                                               
         CP    COUNT,=P'0'         IF TAPE NOT OPENED                           
         BH    *+8                                                              
         BAS   RE,OPENTAPE         OPEN IT                                      
         LA    R5,TAPEREC          R5=A(TAPE RECORD)                            
         USING RECD,R5                                                          
*                                                                               
         OC    SVEMP,SVEMP         IF NEW EMPLOYER                              
         BZ    GETSRT8                                                          
         CLC   SRTEMP,SVEMP                                                     
         BE    GETSRT10                                                         
         CLC   P,SPACES            IF SOMETHING LEFT TO PRINT                   
         BE    *+8                                                              
         BAS   RE,PRNTIT           PRINT IT                                     
         BAS   RE,PRNTOT           PRINT TOTAL FOR EMPLOYER                     
         MVI   FORCEHED,C'Y'       SET NEW PAGE                                 
*                                                                               
GETSRT8  LA    R0,NCHUNKS          RESET NUMBER CHUNKS                          
         LA    R2,P                RESET PRINT LINE                             
         BAS   RE,GETEMP           GET NEW EMPLOYER INFO                        
*                                                                               
GETSRT10 MVC   RECSTID,STATEID     STATE ID NUMBER                              
         MVC   RECEMNAM,EMPNAME    EMPLOYER NAME                                
         MVC   RECEMADD,EMPADDR    EMPLOYER ADDRESS                             
         MVC   RECEMCTY,EMPCITY    EMPLOYER CITY                                
         MVC   RECEMST,EMPSTATE    EMPLOYER STATE                               
         MVC   RECEMZIP,EMPZIP     EMPLOYER ZIP CODE                            
*                                                                               
         BAS   RE,GETW4            GET EMPLOYEE INFO                            
         MVC   RECW4FST,W4FST      EMPLOYEE FIRST INITIAL                       
         MVC   RECW4LST,W4LST      EMPLOYEE LAST NAME                           
         MVC   RECW4SSN,SRTSSN     EMPLOYEE SS NUMBER                           
         MVC   RECBLANK,SPACES     SPACES                                       
         BAS   RE,PUTTAPE          PUT RECORD TO TAPE                           
*                                                                               
         MVC   PRTW4SSN,RECW4SSN   EMPLOYEES SOCIAL SECURITY NUMBER             
         MVC   PRTW4FST,RECW4FST   EMPLOYEES FIRST NAME                         
         MVC   PRTW4MID,W4MID      EMPLOYEES MIDDLE INITIAL                     
         MVC   PRTW4LST,RECW4LST   EMPLOYEES LAST NAME                          
         LA    R2,PRTLNQ(R2)       PT TO NEXT CHUNK                             
         BCT   R0,GETSRT5                                                       
         BAS   RE,PRNTIT           PRINT PRINT THE LINE                         
         B     GETSRT2                                                          
*                                                                               
GETSRTX  CLC   P,SPACES            IF SOMETHING LEFT TO PRINT                   
         BE    *+8                                                              
         BAS   RE,PRNTIT           PRINT IT                                     
         BAS   RE,PRNTOT           PRINT TOTAL FOR EMPLOYER                     
         B     XIT                                                              
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*              ROUTINE TO OPEN TAPE                                             
         SPACE                                                                  
OPENTAPE NTR1                                                                   
         TM    NEOPTS,NENOTAPE    NO TAPE REQUESTED?                            
         BO    OPENTX                                                           
         L     R2,=A(NEDISK)                                                    
         OPEN  ((2),OUTPUT)                                                     
OPENTX   B     XIT                                                              
         SPACE 2                                                                
*              THIS ROUTINE GETS EMPLOYER NAME & ADDRESS                        
         SPACE                                                                  
         USING SORTD,R3            R3=A(SORT RECORD)                            
GETEMP   NTR1                                                                   
         MVC   SVEMP,SRTEMP        SET SAVED EMPLOYER CODE                      
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A0',SRTEMP)                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   EMPNAME,SPACES      EMPLOYER NAME                                
         MVC   EMPNAME(L'TGNAME),TGNAME                                         
*                                                                               
         BAS   RE,GETID            GET STATE ID NUMBER                          
         BAS   RE,GETADD           GET NAME,ADD,CITY,STATE,ZIP                  
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
*              ROUTINE TO SET EMPLOYER STATE ID                                 
         SPACE                                                                  
GETID    NTR1                                                                   
         MVC   STATEID,SPACES      PRE-CLEAR STATE ID                           
         MVI   ELCODE,TATIELQ                                                   
         MVI   WORK,TATITYUN       ELEMENT TYPE                                 
         MVC   WORK+1(3),TIFUNIT   UNIT                                         
         GOTO1 GETL,DMCB,(4,WORK)                                               
         BNE   GETIDX                                                           
*                                                                               
         LA    R3,STATEID          R3=A(SAVED STATE ID NUMBER)                  
         L     R4,TGELEM                                                        
         USING TATID,R4                                                         
         LA    R2,TATIID                                                        
         LA    R1,L'TATIID                                                      
*                                                                               
GETID10  CLI   0(R2),C' '          IGNORE BLANKS                                
         BNH   GETID20                                                          
         CLI   0(R2),C'-'          AND DASHES                                   
         BE    GETID20                                                          
         MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)                                                         
*                                                                               
GETID20  LA    R2,1(R2)            BUMP                                         
         BCT   R1,GETID10          AND LOOP                                     
GETIDX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET EMPLOYER ADDRESS                                  
         SPACE                                                                  
GETADD   NTR1                                                                   
         MVC   BLOCK(120),SPACES   ADDRESS                                      
         MVC   DUB,SPACES          STATE                                        
         MVC   WORK,SPACES         ZIP CODE                                     
*                                                                               
         L     R4,AIO              R4=A(EMPLOYER RECORD)                        
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GAX                                                              
         USING TAADD,R4                                                         
         LA    R3,TAADADD          R3=A(ADDRESS)                                
         ZIC   R2,TAADLNES                                                      
         BCTR  R2,0                                                             
         MH    R2,=H'30'                                                        
*                                                                               
         LA    R1,BLOCK                                                         
         MVC   0(30,R1),0(R3)      STREET ADDRESS                               
         LA    R1,30(R1)                                                        
         ST    R1,TGFULL                                                        
*                                                                               
GA10     LA    R4,30(R3)           NEXT LINE                                    
         GOTO1 =V(ADSCAN),DMCB,((R2),0(R4)),(30,BLOCK+90),DUB,(9,WORK)          
         CLI   0(R1),0             2 ACCEPTABLE RETURN CODES                    
         BE    GA20                                                             
         CLI   0(R1),2                                                          
         BNE   GAX                                                              
*                                                                               
GA20     CLC   BLOCK+90(30),SPACES  DID WE FIND CITY                            
         BH    GA30                                                             
         SH    R2,=H'30'           LESS 1 LINE                                  
         BNP   GA30                                                             
         LA    R3,30(R3)                                                        
         L     R1,TGFULL                                                        
         MVC   0(30,R1),0(R3)      STREET ADDRESS                               
         LA    R1,30(R1)                                                        
         ST    R1,TGFULL                                                        
         B     GA10                                                             
*                                                                               
GA30     GOTO1 SQUASHER,DMCB,BLOCK,90                                           
*                                                                               
GAX      MVC   EMPADDR,BLOCK       STREET ADDRESS                               
         MVC   EMPCITY,BLOCK+90    CITY                                         
         MVC   EMPSTATE,DUB        STATE                                        
         MVC   EMPZIP,WORK         ZIP CODE                                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              THIS ROUTINE GETS W4 NAME                                        
         SPACE                                                                  
         USING SORTD,R3            R3=A(SORT RECORD)                            
GETW4    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SRTSSN)                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO              R4=A(W4 RECORD)                              
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
*                                                                               
         MVC   W4FST,TAW4NAM1      FIRST INITIAL                                
         MVC   W4LST,SPACES        LAST NAME                                    
         MVC   W4LST(L'TAW4NAM2),TAW4NAM2                                       
         CLI   TAW4LEN,TAW4LN2Q     NEW ELEMENT?                                
         BNE   *+10                                                             
         MVC   W4MID,TAW4MIDN      MIDDLE INITIAL                               
         OC    W4MID,SPACES                                                     
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO PUT RECORD TO TAPE                                    
         SPACE                                                                  
         USING RECD,R5             R5=A(TAPE RECORD)                            
PUTTAPE  NTR1                                                                   
         AP    COUNT,=P'1'        ADD TO TAPE COUNTER                           
         GOTO1 MYTRACE,DMCB,=C'TAPE REC',RECD,RECLNQ                            
*                                                                               
         TM    NEOPTS,NENOTAPE    NO TAPE REQUESTED?                            
         BO    PUTTAPEX                                                         
         L     R2,=A(NEDISK)                                                    
         PUT   (R2),(R5)          PUT IT TO TAPE                                
PUTTAPEX B     XIT                                                              
         DROP  R5                                                               
         SPACE 2                                                                
*              THIS ROUTINE PRINTS OUT A LINE OF INFO                           
         SPACE                                                                  
PRNTIT   NTR1                                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    PRINT IT                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLOSE TAPE                                            
         SPACE                                                                  
CLOSTAPE NTR1                                                                   
*                                                                               
         TM    NEOPTS,NENOTAPE     IF TAPE REQUESTED                            
         BO    CLOSTX                                                           
         L     R2,=A(NEDISK)       CLOSE IT                                     
         CLOSE ((2))                                                            
*                                                                               
CLOSTX   B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT TOTAL NUMBER RECORDS ON TAPE                    
         SPACE                                                                  
PRNTOT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         EDIT  COUNT,(8,P+1),ALIGN=LEFT                                         
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(9,R1),=CL9'EMPLOYEES'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    TOTCOUNT,COUNT      ADD TO TOTAL TAPE OCUNTER                    
         ZAP   COUNT,=P'0'         RESET EMPLOYER COUNTER                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TRACE RECORD                                          
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         TM    NEOPTS,NETRACE      IF TRACE ON                                  
         BZ    MYTRACEX                                                         
         L     R2,0(R1)            A(LITERAL)                                   
         L     RF,8(R1)            SET LENGTH OF RECORD                         
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
MYTRACEX B     XIT                                                              
         SPACE 2                                                                
*              HEADLINE HOOK ROUTINE                                            
         SPACE                                                                  
HDHOOK   NTR1                                                                   
         MVC   H3+10(L'TIFUNIT),TIFUNIT   UNIT                                  
         MVC   H3+15(L'TGTANAME),TGTANAME UNIT NAME                             
         MVC   H3+25(L'STATEID),STATEID   STATE ID NUMBER                       
         MVC   H4+10(L'SVEMP),SVEMP       EMPLOYER CODE & NAME                  
         MVC   H4+15(36),EMPNAME                                                
*                                                                               
         MVC   H4+108(L'TUPER),TUPER      PERIOD                                
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(12)'                                  
         SPACE 2                                                                
NEDISK   DCB   DDNAME=NEDISK,DSORG=PS,RECFM=FB,LRECL=175,              X        
               BLKSIZE=1750,MACRF=PM                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
*                                                                               
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H1,53,C'NEW EMPLOYEE REPORT'                                     
         SSPEC H2,53,19X'BF'                                                    
*                                                                               
         SSPEC H3,2,C'UNIT'                                                     
         SSPEC H4,2,C'EMPLOYER'                                                 
         SSPEC H4,100,C'PERIOD'                                                 
         SSPEC H8,2,C'SS NUMBER EMPLOYEE NAME'                                  
         SSPEC H9,2,C'--------- -------------'                                  
         SSPEC H8,35,C'SS NUMBER EMPLOYEE NAME'                                 
         SSPEC H9,35,C'--------- -------------'                                 
         SSPEC H8,68,C'SS NUMBER EMPLOYEE NAME'                                 
         SSPEC H9,68,C'--------- -------------'                                 
         SSPEC H8,101,C'SS NUMBER EMPLOYEE NAME'                                
         SSPEC H9,101,C'--------- -------------'                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TUD      DSECT                                                                  
         DS    0A                                                               
PRNTBL   DS    A                   A(PRNTBL)                                    
*                                                                               
NCHUNKS  EQU   4                   NUMBER OF CHUNKS PER PRINT LINE              
*                                                                               
NEOPTS   DS    XL1                 OPTIONS                                      
NENOTAPE EQU   X'80'               DON'T GENERATE TAPE                          
NETRACE  EQU   X'40'               TRACE RECORDS                                
*                                                                               
ACTVSW   DS    CL1                 ACTIVITY INDICATOR                           
TUPER    DS    CL17                PRINTABLE PERIOD                             
*                                                                               
COUNT    DS    PL6                 RECORD COUNTER FOR EMPLOYER                  
TOTCOUNT DS    PL6                 RECORD COUNTER FOR TAPE                      
SVTIKEY  DS    CL(TLCKYDTE-TLCKPKEY) SAVED KEY                                  
SVEMP    DS    CL3                 SAVED EMPLOYER                               
*                                                                               
STATEID  DS    CL8                 STATE ID NUMBER                              
EMPNAME  DS    CL50                EMPLOYER NAME                                
EMPADDR  DS    CL40                EMPLOYER ADDRESS                             
EMPCITY  DS    CL25                EMPLOYER CITY                                
EMPSTATE DS    CL2                 EMPLOYER STATE                               
EMPZIP   DS    CL5                 EMPLOYER ZIP CODE                            
*                                                                               
W4FST    DS    CL1                 EMPLOYEE'S FIRST INITIAL                     
W4MID    DS    CL1                 EMPLOYEE'S MIDDLE INITIAL                    
W4LST    DS    CL20                EMPLOYEE'S LAST NAME                         
*                                                                               
SRTREC   DS    CL(SRTLNQ)          SORT RECORD AREA                             
*                                                                               
TAPEREC  DS    CL(RECLNQ)          TAPE RECORD AREA                             
*                                                                               
TULNQ    EQU   *-TUD                                                            
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
*                                                                               
SORTD    DSECT                                                                  
SRTEMP   DS    CL3                 EMPLOYER CODE                                
SRTSSN   DS    CL9                 EMPLOYER SSN                                 
SRTLNQ   EQU   *-SORTD                                                          
         SPACE 2                                                                
*              DSECT TO COVER FILE RECORD                                       
*                                                                               
RECD     DSECT                                                                  
RECSTID  DS    CL8                 STATE ID NUMBER                              
RECEMNAM DS    CL50                EMPLOYER NAME                                
RECEMADD DS    CL40                EMPLOYER ADDRESS                             
RECEMCTY DS    CL25                EMPLOYER CITY                                
RECEMST  DS    CL2                 EMPLOYER STATE                               
RECEMZIP DS    CL5                 EMPLOYER ZIP CODE                            
RECW4FST DS    CL1                 EMPLOYEE'S FIRST INITIAL                     
RECW4LST DS    CL20                EMPLOYEE'S LAST NAME                         
RECW4SSN DS    CL9                 EMPLOYEE'S SS#                               
RECBLANK DS    CL15                BLANKS                                       
RECLNQ   EQU   *-RECD                                                           
         SPACE 2                                                                
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                 SPACE                                        
PRTW4SSN DS    CL9                 EMPLOYEE'S SS #                              
         DS    CL1                 SPARE                                        
PRTW4FST DS    CL1                 EMPLOYEE'S FIRST INITIAL                     
PRTW4MID DS    CL1                 EMPLOYEE'S MIDDLE INITIAL                    
         DS    CL1                 SPACE                                        
PRTW4LST DS    CL18                EMPLOYEE'S LAST NAME                         
         DS    CL1                 SPARE                                        
PRTLNQ   EQU   *-PRNTD                                                          
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC3D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDPERVALD                                                                     
* DDTWADCONS                                                                    
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDPERVALD                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055TAREP43   05/01/02'                                      
         END                                                                    
