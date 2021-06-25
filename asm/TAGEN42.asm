*          DATA SET TAGEN42    AT LEVEL 077 AS OF 04/08/14                      
*PHASE T70242A,*                                                                
         TITLE 'T70242 - LIENS LIST'                                            
T70242   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70242                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE                                                                  
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    LINL10                                                           
         MVC   SLNPHED(4),=C'Pid '                                              
*        MVC   SLNPHED(7),=C'Pid Num'                                           
         OI    SLNPHEDH+6,X'80'                                                 
         MVC   SLNSHED(9),=C'Pid Num  '                                         
         OI    SLNSHEDH+6,X'80'                                                 
*                                                                               
LINL10   CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   LINL20                                                           
         BAS   RE,INIT             INITIALIZE INFO FOR SYSIO                    
         B     XIT                                                              
         EJECT                                                                  
LINL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   LINL30                                                           
         MVC   LISTAR,SPACES       CLEAR PREV LINE                              
         LA    R2,LISTAR                                                        
         B     LSTREC                                                           
         SPACE 3                                                                
LINL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'                                                    
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         LA    R2,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LSTREC                                                           
         SPACE 3                                                                
         USING LISTD,R2                                                         
LSTREC   EQU   *                                                                
         LA    R0,HOOK             SET ADDRESS OF HOOK FOR SYSIO                
         ST    R0,TIHOOK                                                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIACOMFC,ACOMFACS                                                
         XC    DUE(8),DUE          CLEAR ACCUMS                                 
         XC    TGSSN,TGSSN         CLEAR FOR XNAME CALL                         
         MVI   NLISTS,16           GET CONTROL BACK AT END OF PAGE              
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15           RESET AT END OF LIST                         
         SPACE                                                                  
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LSTRX                                                            
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE                                    
         MVC   LISNAME(6),=C'TOTALS'                                            
         MVC   TEMPDUE(8),DUE      MOVE AMOUNTS TO AREA FOR PROCAMTS            
         BAS   RE,PROCAMTT                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 (RF),(R1),(R8)      SKIP LINE                                    
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(12,R1),=C'LIEN RECORDS'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LSTRX                                                            
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE                                                                  
LSTRX    MVC   TGSSN,SVTGSSN       RESTORE GLOBAL SSN                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              SETS UP INFO FOR SYSIO                                           
         SPACE                                                                  
INIT     NTR1                                                                   
         MVI   TIREAD,TLLNCDQ      LIST BY LIEN CODE                            
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         LA    R2,SLNSSNH          VALIDATE PERFORMER                           
         TM    4(R2),X'20'         IF NOT PREV VALIDATED                        
         BO    INIT4                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIFSSN,TIFSSN                                                    
         CLI   5(R2),0             AND THERE'S INPUT                            
         BE    INIT3                                                            
*                                                                               
         CLI   SLNSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    INIT1               RECVAL CALL DOES NOT CHECK FOR               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    FLDINV                                                           
         CLI   SLNSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SLNSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   INIT1                                                            
         MVC   SLNSSN,TGSSN                                                     
         MVI   SLNSSNH+5,9                                                      
*                                                                               
INIT1    GOTO1 RECVAL,DMCB,TLW4CDQ,(R2) VALIDATE IT                             
         MVC   TIFSSN,SLNSSN       FILTER ON SS NUMBER                          
         OC    TIFSSN,SPACES                                                    
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    INIT3                                                            
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLNSSN,SPACES                                                    
         MVC   SLNSSN(L'TGPID),TGPID                                            
         MVI   SLNSSNH+5,6                                                      
         OI    SLNSSNH+6,X'80'                                                  
*                                                                               
INIT3    OI    4(R2),X'20'         SET PREV VALIDATED                           
         SPACE                                                                  
INIT4    TM    SLNSTRH+4,X'20'     IF START AT FIELD NOT PREV VALIDATED         
         BO    INIT7                                                            
         MVI   ALLVAL,C'N'                                                      
         XC    TIQSTART,TIQSTART   RESET START AT                               
         ZIC   R3,SLNSTRH+5                                                     
         LTR   R3,R3                                                            
         BZ    INIT6                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),SLNSTR                                               
INIT6    OI    SLNSTRH+4,X'20'     SET PREV VALIDATED                           
         SPACE                                                                  
INIT7    LA    R2,SLNOPTSH         OPTIONS                                      
         TM    4(R2),X'20'         IF NOT PREV. VALIDATED                       
         BO    INIT19                                                           
         MVI   ALLVAL,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    INIT18A                                                          
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
         SPACE                                                                  
INIT10   EQU   *                   VALIDATE OPTIONS                             
         SPACE                                                                  
INIT18   LA    R3,SCANNEXT         GET NEXT                                     
         BCT   R0,INIT10                                                        
INIT18A  OI    4(R2),X'20'         SET PREV VALIDATED                           
         SPACE                                                                  
INIT19   CLI   ALLVAL,C'Y'         IF NOT ALL FIELDS PREV VALIDATED             
         BE    XIT                                                              
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO FIRST TIME IN          
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         MVC   SVTGSSN,TGSSN       SAVE GLOBAL SSN - XNAME CREAMS               
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
HOOK     NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         MVC   LISCODE,TISSN       SOCIAL SECURITY NUMBER                       
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    HK2                                                              
         MVC   LISCODE,SPACES                                                   
         GOTO1 SSNPACK,DMCB,TISSN,LISCODE                                       
*                                                                               
HK2      MVC   LISLNC,TILNC        LIEN REFERENCE NUMBER                        
         SPACE                                                                  
         CLC   TGSSN,TISSN                                                      
         BE    HK5                                                              
         MVC   TGSSN,TISSN                                                      
         GOTO1 XNAME,DMCB,TLW4CDQ,SVW4NME,TIKEY GET NAME FROM W4 RECORD         
HK5      MVC   LISNAME,SVW4NME                                                  
         SPACE                                                                  
         USING TALND,R4                                                         
         L     R4,TIAREC           GET RECORD BEING PROCESSED                   
         MVI   ELCODE,TALNELQ      GET LIEN DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   LISTYPE,TALNTYPE    LIEN TYPE                                    
         MVC   TEMPDUE,TALNDUE     MOVE AMOUNTS TO AREA FOR PROCAMTS            
         MVC   TEMPCOL,TALNCOL                                                  
         BAS   RE,PROCAMTS                                                      
         SPACE                                                                  
         ICM   R3,15,TALNDUE       ADD TO AMOUNT DUE ACCUM                      
         A     R3,DUE                                                           
         ST    R3,DUE                                                           
         ICM   R3,15,TALNCOL       ADD TO AMOUNT COLLECTED ACCUM                
         A     R3,COL                                                           
         ST    R3,COL                                                           
         SPACE                                                                  
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   HK10                                                             
         GOTO1 CATCHIOS            ENSURE IO OVERUSE FOR REPORTS                
         GOTO1 SPOOL,DMCB,(R8)     SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT NUMBER OF LINES OUTPUT                 
         B     XIT                                                              
         SPACE                                                                  
HK10     CLI   LISTNUM,15          IF ALREADY FILLED PAGE                       
         BNE   HK15                                                             
         MVC   TGSSN,SVTGSSN       RESTORE GLOBAL SSN                           
         B     ENDPAGE             GET OUT                                      
         SPACE                                                                  
HK15     GOTO1 LISTMON             ELSE CALL LISTMON                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EDITS AMOUNTS IN TEMPDUE AND TEMPCOL TO AREA AT          
*              R2, AND FINDS AND EDITS THE BALANCE OF THE TWO AMOUNTS           
         SPACE                                                                  
         USING LISTD,R2                                                         
         USING TALND,R4                                                         
PROCAMTT NTR1                                                                   
         B     PROCAMT5            FOR TOTALS PRINT ALL AMOUNTS                 
*                                                                               
PROCAMTS NTR1                      FOR LINE TOTAL                               
         CLC   TALNTYPE,=AL2(TALNTYGT) IF GROSS TRUSTEE                         
         BNE   PROCAMT5                                                         
         EDIT  TEMPCOL,(10,LISCOL),2   ONLY SHOW AMOUNT COLLECTED               
         XC    LISDUE,LISDUE                                                    
         XC    LISBAL,LISBAL                                                    
         B     XIT                                                              
PROCAMT5 EDIT  TEMPDUE,(10,LISDUE),2 AMOUNT DUE                                 
         EDIT  TEMPCOL,(10,LISCOL),2 AMOUNT COLLECTED                           
         L     R1,TEMPDUE                                                       
         S     R1,TEMPCOL                                                       
         EDIT  (R1),(10,LISBAL),2  BALANCE AMOUNT                               
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SLNSELH                                                       
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,36,C'LIENS LIST'                                              
         SSPEC H2,36,C'----------'                                              
         SPACE 1                                                                
         SSPEC H4,1,C'SS NUMBER'                                                
         SSPEC H4,12,C'REF #'                                                   
         SSPEC H4,19,C'TYPE'                                                    
         SSPEC H4,25,C'NAME'                                                    
         SSPEC H4,42,C'AMOUNT DUE'                                              
         SSPEC H4,56,C'COLLECTED'                                               
         SSPEC H4,69,C'BALANCE'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'---------'                                                
         SSPEC H5,12,C'-----'                                                   
         SSPEC H5,19,C'----'                                                    
         SSPEC H5,25,C'----'                                                    
         SSPEC H5,42,C'----------'                                              
         SSPEC H5,56,C'---------'                                               
         SSPEC H5,69,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE                                  
         SPACE                                                                  
LISTD    DSECT                                                                  
LISCODE  DS    CL9                                                              
         DS    CL2                                                              
LISLNC   DS    CL6                                                              
         DS    CL1                                                              
LISTYPE  DS    CL2                                                              
         DS    CL4                                                              
LISNAME  DS    CL16                                                             
         DS    CL1                                                              
LISDUE   DS    CL10                                                             
         DS    CL3                                                              
LISCOL   DS    CL10                                                             
         DS    CL1                                                              
LISBAL   DS    CL10                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR42D                                                       
         SPACE 3                                                                
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
SVW4NME  DS    CL16                SAVED W4 NAME                                
SVTGSSN  DS    CL9                 SAVED GLOBAL SSN                             
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
TEMPDUE  DS    F                   TEMP WORK AREA FOR AMOUNT DUE                
TEMPCOL  DS    F                                      COLLECTED AMOUNT          
DUE      DS    F                   ACCUMS FOR AMOUNTS IN LIST                   
COL      DS    F                                                                
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077TAGEN42   04/08/14'                                      
         END                                                                    
