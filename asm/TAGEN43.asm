*          DATA SET TAGEN43    AT LEVEL 073 AS OF 11/13/14                      
*PHASE T70243E,*                                                                
         TITLE 'T70243 - DUE COMPANY LIST'                                      
T70243   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70243                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTAB                                               
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    DUE10                                                            
         MVC   SDUPHED(7),=C'Pid Num'                                           
         OI    SDUPHEDH+6,X'80'                                                 
         MVC   SDUSHED(9),=C'Pid Num  '                                         
         OI    SDUSHEDH+6,X'80'                                                 
*                                                                               
DUE10    CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
DUE20    CLI   MODE,LISTRECS                                                    
         BNE   DUE30                                                            
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
DUE30    CLI   MODE,PRINTREP                                                    
         BNE   DUEX                                                             
         XC    KEY,KEY             ENSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
DUEX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       LA    R2,SDUSSNH          START AT SPECIFIC SS NUM                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK5                                                              
         NI    SDUSTRH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BE    VK5                                                              
*                                                                               
         CLI   SDUSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    VK3                 RECVAL CALL DOES NOT CHECK FOR               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    INVERR                                                           
         CLI   SDUSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SDUSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK3                                                              
         MVC   SDUSSN,TGSSN                                                     
         MVI   SDUSSNH+5,9                                                      
*                                                                               
VK3      GOTO1 RECVAL,DMCB,TLW4CDQ,(R2)                                         
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK5                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SDUSSN,SPACES                                                    
         MVC   SDUSSN(L'TGPID),TGPID                                            
         MVI   SDUSSNH+5,6                                                      
         OI    SDUSSNH+6,X'80'                                                  
*                                                                               
VK5      OI    4(R2),X'20'                                                      
         LA    R2,SDUSTRH          START AT REFERENCE NUMBER                    
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         NI    SDUEMPH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
*                                                                               
         XC    SVSTART,SVSTART                                                  
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         LA    R4,SVSTART                                                       
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                SUB 1 FOR MOVE                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)       INTO SVSTART                                 
         OC    SVSTART,SPACES                                                   
*                                                                               
VK10     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SDUEMPH          VALIDATE EMPLOYER                            
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK15                                                             
         NI    SDUAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BE    VK15                                                             
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2)                                         
*                                                                               
VK15     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SDUAGYH          VALIDATE AGENCY                              
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK20                                                             
         NI    SDUOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(R2)                                         
         EJECT                                                                  
*                                                                               
VK20     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SDUOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
*                                                                               
         MVI   OPTFLAG,0                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
*                                                                               
VK30     DS    0H                                                               
         CLC   =C'FLAG',SCDATA1                                                 
         BNE   VK50                                                             
         CLI   SCDATA2,C'T'        TAXABLE WAGES?                               
         BNE   *+12                                                             
         OI    OPTFLAG,OPTFTW                                                   
         B     VK50                                                             
         CLI   SCDATA2,C'R'        TAXABLE REIMBURSEMENTS?                      
         BNE   *+12                                                             
         OI    OPTFLAG,OPTFTR                                                   
         B     VK50                                                             
         CLI   SCDATA2,C'N'        NON TAXABLE REIMBURSEMENTS?                  
         BNE   INVERR                                                           
         OI    OPTFLAG,OPTFNR                                                   
*                                                                               
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK20                                                          
*                                                                               
VK60     OI    4(R2),X'20'         VALIDATED                                    
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO                        
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
***      XC    TIFSSN,TIFSSN       CLEAR FILTERS                                
***      XC    TIFEMP,TIFEMP                                                    
***      XC    TIFAGY,TIFAGY                                                    
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF      SCREEN                                    
*                                                                               
         MVC   TIFSSN,SDUSSN       SOCIAL SECURITY NUMBER                       
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    INIT10                                                           
         MVC   TGPID,SDUSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TIFSSN                                        
INIT10   MVC   TIQSTART,SVSTART    START FROM REFERENCE NUMBER                  
         MVC   TIFEMP,SDUEMP       EMPLOYER                                     
         MVC   TIFAGY,SDUAGY       AGENCY                                       
         OC    TIFAGY,TIFAGY                                                    
         BZ    *+10                                                             
         OC    TIFAGY,SPACES                                                    
         OC    TIFEMP,TIFEMP                                                    
         BZ    *+10                                                             
         OC    TIFEMP,SPACES                                                    
         MVI   TIREAD,TLDUCDQ                                                   
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         XC    TOTDUE(8),TOTDUE    CLEAR TOTALS                                 
*                                                                               
LR5      MVC   SVSSN,TGSSN                                                      
         XC    TGSSN,TGSSN                                                      
         MVI   NLISTS,16           GET CONTROL BACK AFTER                       
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15               A FULL PAGE                              
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         LA    R2,P                                                             
         GOTO1 SPOOL,DMCB,(R8)     LEAVE BLANK LINE                             
         BAS   RE,PRTOTALS         PRINT TOTALS                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)     LEAVE BLANK LINE                             
*                                                                               
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(21,R1),=C'DUE COMPANY RECORDS'                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      MVC   TGSSN,SVSSN         RESTORE GLOBAL SSN                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    DS    0H                                                               
         MVC   LISTAR,SPACES       CLEAR PREVIOS LINE                           
         USING LISTD,R2                                                         
*                                                                               
         CLI   OPTFLAG,0                                                        
         BE    PR00A                                                            
*                                                                               
         L     R4,TIAREC                                                        
         USING TADUD,R4                                                         
         MVI   ELCODE,TADUELQ      POINT TO DUE COMP DETAILS ELEMENT            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   OPTFLAG,OPTFTW      TAXABLE WAGES?                               
         BNE   PR00                                                             
         TM    TADUSTAT,TADUSNTR                                                
         BO    PRRX                                                             
         TM    TADUSTA2,TADUSTXR                                                
         BO    PRRX                                                             
PR00     CLI   OPTFLAG,OPTFTR      TAXABLE REIMBURSEMENTS?                      
         BNE   *+12                                                             
         TM    TADUSTA2,TADUSTXR                                                
         BZ    PRRX                                                             
         CLI   OPTFLAG,OPTFNR      NON TAXABLE REIMBURSEMENTS?                  
         BNE   *+12                                                             
         TM    TADUSTAT,TADUSNTR                                                
         BZ    PRRX                                                             
*                                                                               
PR00A    MVC   DUESSN,TISSN        SS NUMBER                                    
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    PR01                                                             
         MVC   DUESSN,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TISSN,DUESSN                                        
*                                                                               
PR01     MVC   DUEREF,TIDUC        REFERENCE NUMBER                             
         CLI   TIDUC,X'FA'         IS THIS A YEAR 2000 DATE?                    
         BL    PR02                NO                                           
         MVC   WORK(4),TIDUC       YES, CONVERT TO DISPLAY                      
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   DUEREF(4),WORK                                                   
         MVC   DUEREF+4(2),TIDUC+4                                              
*                                                                               
PR02     MVC   DUEEMP,TIEMP        EMPLOYER                                     
         CLC   TGSSN,TISSN                                                      
         BE    PR05                IF NAME SAME DONT REREAD                     
         MVC   TGSSN,TISSN                                                      
         GOTO1 XNAME,DMCB,TLW4CDQ,W4NAME,TIKEY                                  
*                                                                               
PR05     MVC   DUENAME,W4NAME      W4 NAME                                      
*                                                                               
         L     R4,TIAREC                                                        
         USING TADUD,R4                                                         
         MVI   ELCODE,TADUELQ      POINT TO DUE COMP DETAILS ELEMENT            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DUEAGY,TADUAGY      AGENCY                                       
         ICM   R3,15,TADUDUE       AMOUNT DUE                                   
         EDIT  (R3),(9,DUEDUE),2,FLOAT=-                                        
         A     R3,TOTDUE           TOTAL TO DATE                                
         STCM  R3,15,TOTDUE                                                     
*                                                                               
         ICM   R5,15,TADUCOL       COLLECTED AMOUNT                             
         EDIT  (R5),(9,DUECOLL),2,FLOAT=-                                       
         A     R5,TOTCOL           TOTAL TO DATE                                
         STCM  R5,15,TOTCOL                                                     
*                                                                               
         ICM   R5,15,TADUCOL       COLLECTED AMOUNT                             
         ICM   R3,15,TADUDUE       AMOUNT DUE                                   
         SR    R3,R5               BALANCE = DUE - COLLECTED                    
         EDIT  (R3),(9,DUEBAL),2,FLOAT=-                                        
*                                                                               
PR20     MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP                                                    
         BNE   PR40                                                             
         GOTO1 CATCHIOS            ENSURE DON'T DO TOO MANY I/O-S               
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PR40     CLI   LISTNUM,15          IF END OF PAGE                               
         BNE   PR50                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SDUSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR50     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
*                                                                               
         USING LISTD,R2                                                         
PRTOTALS NTR1                                                                   
         MVC   DUEEMP(6),=C'TOTALS'                                             
         ICM   R3,15,TOTDUE        TOTAL AMOUNT DUE                             
         EDIT  (R3),(9,DUEDUE),2,FLOAT=-                                        
         ICM   R5,15,TOTCOL        TOTAL AMOUNT COLLECTED                       
         EDIT  (R5),(9,DUECOLL),2,FLOAT=-                                       
         SR    R3,R5               BALANCE = DUE - COLLECTED                    
         EDIT  (R3),(9,DUEBAL),2,FLOAT=-                                        
*                                                                               
PRTOTX   B     XIT                                                              
         SPACE 5                                                                
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
COMPTERR MVI   ERROR,ERCOMPT                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DTRACK  ',CL8'LIST    '                               
PF13     DC    AL1(KEYTYCUR,L'DUESSN),AL2(DUESSN-LISTD)                         
         DC    AL1(KEYTYCUR,L'DUEREF),AL2(DUEREF-LISTD)                         
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,69,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,31,C'DUE COMPANY LIST'                                        
         SSPEC H2,31,C'----------------'                                        
         SPACE 1                                                                
         SSPEC H4,1,C'SS NUMBER'                                                
         SSPEC H4,11,C'REF #'                                                   
         SSPEC H4,19,C'NAME'                                                    
         SSPEC H4,36,C'EMP'                                                     
         SSPEC H4,41,C'AGENCY'                                                  
         SSPEC H4,49,C'AMT DUE'                                                 
         SSPEC H4,58,C'COLLECTED'                                               
         SSPEC H4,69,C'BALANCE'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'---------'                                                
         SSPEC H5,11,C'-----'                                                   
         SSPEC H5,19,C'----'                                                    
         SSPEC H5,36,C'---'                                                     
         SSPEC H5,41,C'------'                                                  
         SSPEC H5,49,C'-------'                                                 
         SSPEC H5,58,C'---------'                                               
         SSPEC H5,69,C'-------'                                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
DUESSN   DS    CL9                                                              
         DS    CL1                                                              
DUEREF   DS    CL6                                                              
         DS    CL2                                                              
DUENAME  DS    CL16                                                             
         DS    CL1                                                              
DUEEMP   DS    CL3                                                              
         DS    CL2                                                              
DUEAGY   DS    CL6                                                              
DUEDUE   DS    CL9                                                              
         DS    CL2                                                              
DUECOLL  DS    CL9                                                              
DUEBAL   DS    CL9                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR43D                                                       
         EJECT                                                                  
*                                                                               
SVSSN    DS    CL9                 SAVED S/S NUMBER                             
SVSTART  DS    CL6                 SAVED START FIELD                            
W4NAME   DS    CL16                W4 NAME REQUESTED                            
COUNTER  DS    PL4                 LINE COUNTER                                 
TOTDUE   DS    F                   TOTAL DUE                                    
TOTCOL   DS    F                   TOTAL COLLECTED                              
OPTFLAG  DS    XL1                                                              
OPTFTR   EQU   X'80'               TAXABLE REIMBURSEMENTS                       
OPTFNR   EQU   X'40'               NON TAXABLE REIMBURSEMENTS                   
OPTFTW   EQU   X'20'               TAXABLE WAGES                                
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
         SPACE 3                                                                
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073TAGEN43   11/13/14'                                      
         END                                                                    
