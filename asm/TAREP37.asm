*          DATA SET TAREP37    AT LEVEL 055 AS OF 12/09/13                      
*PHASE T70337B,*                                                                
*INCLUDE TALIM                                                                  
         TITLE 'T70337 - REFRESH BILLING SUI YEAR-TO-DATE'                      
T70337A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70337A,R6                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         ST    R2,MYABOX                                                        
         L     R8,BOXAWIDE                                                      
         USING WIDED,R8                                                         
         MVC   MYSORTER,SORTER                                                  
         DROP  R1,R2                                                            
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 2                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         GETEL2 R3,DATADISP,ELCODE                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         SPACE 1                                                                
         LA    R2,SYDPDH           VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         USING PERVALD,R3                                                       
         MVC   TIQPSTR,PVALPSTA    SET DATES FOR SYSIO                          
         MVC   TIQPEND,PVALPEND                                                 
         SPACE 1                                                                
         LA    R2,SYDCURH          CURRENCY                                     
         GOTO1 ANY                                                              
         CLI   8(R2),C'U'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'C'                                                       
         BNE   FLDINV                                                           
         MVC   TIFCUR,8(R2)                                                     
         SPACE 1                                                                
         LA    R2,SYDEMPH          EMPLOYER                                     
         MVC   AIO,=A(EMPIO)                                                    
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'20',(R2))                                 
         MVC   AIO,AIO1                                                         
         MVC   TIFEMP,TGEMP                                                     
         SPACE 1                                                                
         LA    R2,SYDSSNH          S/S NUMBER (OPTIONAL)                        
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(R2)                                         
         MVC   TIFSSN,TGSSN                                                     
         SPACE 1                                                                
VK30     LA    R2,SYDOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 1                                                                
VOPTS    NTR1                                                                   
         MVI   TRACEOPT,C'N'                                                    
         XC    OPTS,OPTS                                                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         LA    R4,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(10,(R4)),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    FLDINV                                                           
         USING SCAND,R4            R4=A(SCAN BLOCK)                             
*                                                                               
OPT10    CLC   =C'TRACE',SCDATA1   TRACE OPTION                                 
         BNE   OPT20                                                            
         MVI   TRACEOPT,C'Y'                                                    
         ZAP   TRACOUNT,=P'0'                                                   
         ZAP   TRALIMIT,=P'0'                                                   
         L     R1,SCBIN2           (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
*                                                                               
OPT20    CLC   =C'DISPDA',SCDATA1  DISPLAY D/A                                  
         BNE   OPT25                                                            
         OI    OPTS,OPTDA                                                       
         B     OPTEND                                                           
*                                                                               
OPT25    CLC   =C'TAPE',SCDATA1    WRITE TO TAPE                                
         BNE   OPT27                                                            
         OI    OPTS,OPTTAPE                                                     
         B     OPTEND                                                           
*                                                                               
OPT27    CLC   =C'ASOF',SCDATA1    REGENERATE AS START OF PERIOD                
         BNE   OPT30                                                            
         CLI   SCLEN2,0                                                         
         BE    FLDINV                                                           
         LA    R2,SCDATA2                                                       
         GOTO1 DTVAL,DMCB,(X'40',ASOFDATE)                                      
         BNE   FLDINV                                                           
         OI    OPTS,OPTASOF                                                     
         B     OPTEND                                                           
*                                                                               
OPT30    CLC   =C'TAXABLE',SCDATA1  DON'T SHOW TAXABLE LINE                     
         BNE   OPT40                                                            
         OI    LINES,TAXABLE                                                    
         B     OPTEND                                                           
*                                                                               
OPT40    CLC   =C'TAXES',SCDATA1    DON'T SHOW TAXES LINE                       
         BNE   OPT50                                                            
         OI    LINES,TAXES                                                      
         B     OPTEND                                                           
*                                                                               
OPT50    CLC   =C'WGBASE',SCDATA1   DON'T SHOW WAGE BASE LINE                   
         BNE   OPT60                                                            
         OI    LINES,WGBASE                                                     
         B     OPTEND                                                           
*                                                                               
OPT60    CLC   =C'ONLYCHG',SCDATA1  ONLY PRINT CHANGED RECORDS                  
         BNE   OPT70                                                            
         OI    OPTS,OPTCHG                                                      
         B     OPTEND                                                           
*                                                                               
OPT70    CLC   =C'RATES',SCDATA1    PRINT RATES INSTEAD OF WAGE BASE            
         BNE   OPT80                                                            
         OI    OPTS,OPTRATES                                                    
         B     OPTEND                                                           
*                                                                               
OPT80    CLC   =C'CHECK',SCDATA1    USE PAYROLL RATES (ALLTAX)                  
         BNE   OPT90                                                            
         OI    OPTS,OPTCHECK                                                    
         B     OPTEND                                                           
*                                                                               
OPT90    B     FLDINV                                                           
*                                                                               
OPTEND   LA    R4,SCANNEXT                                                      
         BCT   R0,OPT10                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS REPORT                                                   
         SPACE 1                                                                
PREP     NTR1                                                                   
         MVI   STATUS,0            INITIALIZE                                   
         ZAP   INVCOUNT,=P'0'                                                   
         ZAP   CHKCOUNT,=P'0'                                                   
         ZAP   CHACOUNT,=P'0'                                                   
         SPACE 1                                                                
         L     R2,ASPOOLD                                                       
         USING SPOOLD,R2                                                        
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         DROP  R2                                                               
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         OC    TIFSSN,TIFSSN       IF TESTING FOR AN SSN                        
         BZ    *+8                                                              
         MVI   TIFPAIN,X'FF'       EXCLUDE ADJUSTMENTS                          
*                                                                               
         MVI   TIQDTYPE,TIQDBILL   SET FILTERING ON BILL DATE                   
         MVI   TIFINSTY,0                                                       
         MVI   TIFINS2Y,0                                                       
         OI    TIFCDSTN,TACDSLIN+TACDSTRS IGNORE LIENS & W4 TRUSTEES            
         MVI   TIREAD,TLCKCDQ      SET READ SEQUENCE                            
         MVI   TISUBRD,0                                                        
         OI    TIQFLAGS,TIQFUPDR   READ FOR UPDATE                              
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO HANDLE I/O                
*                                                                               
         BAS   RE,SETCHK                                                        
         BAS   RE,GETSORT          GET SORT RECORDS AND PROCESS                 
*                                                                               
         MVC   SYSFIL,=CL8'TALFIL' RESET BACK TO TAL FILES                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
*                                                                               
*NO-OP   LA    R1,CODHOOK          SET UP HOOK FOR COD INVOICES                 
*        ST    R1,TIHOOK                                                        
*        MVI   TIFCDSTN,0                                                       
*        MVI   TIQDTYPE,TIQDCOD    SET FILTERING ON COD DATE                    
*        MVI   TIREAD,TLINBCDQ     SET READ SEQUENCE                            
*        MVI   TISUBRD,0                                                        
*        OI    TIQFLAGS,TIQFUPDR   READ FOR UPDATE                              
*        MVC   TIQPSTR,=X'911007'  SET DATES FOR SYSIO                          
*        MVC   TIQPEND,=X'911016'                                               
*                                                                               
*NO-OP   GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO HANDLE I/O                
*                                                                               
         L     R4,MYABOX             CLOSE BOX                                  
         LTR   R4,R4                                                            
         BZ    PREP10                                                           
         USING BOXD,R4                                                          
         MVC   BOXROWS,XSPACES                                                  
         L     R2,ASPOOLD                                                       
         USING SPOOLD,R2                                                        
         ZIC   R1,LINE                                                          
         DROP  R2                                                               
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         MVC   XP,XSPACES                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
PREP10   EDIT  INVCOUNT,(9,XP+1),ZERO=NOBLANK                                   
         MVC   XP+11(22),=C'INVOICE RECORD CHANGES'                             
         EDIT  CHKCOUNT,(9,XP2+1),ZERO=NOBLANK                                  
         MVC   XP2+11(13),=C'CHECK RECORDS'                                     
         EDIT  CHACOUNT,(9,XP3+1),ZERO=NOBLANK                                  
         MVC   XP3+11(20),=C'CHECK RECORD CHANGES'                              
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES I/O HOOKS FROM SYSIO                             
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCINV      IF GETTING AN INVOICE                        
         BNE   IOHOOK10                                                         
         MVI   SORTCOD,C'N'                                                     
         L     R4,TIAREC                                                        
         USING TAIND,R4                                                         
         MVI   ELCODE,TAINELQ      GET INVOICE DETAILS                          
         BAS   RE,GETEL            IF THIS WAS A COD                            
         BNE   IOHOOK02                                                         
         TM    TAINSTA2,TAINSHLP   THAT WAS PRINTED                             
         BNO   IOHOOK01                                                         
         MVI   SORTCOD,C'Y'        SET BYTE IN SORT RECORD                      
*                                                                               
IOHOOK01 OC    TIBIDATE,TIBIDATE   IF THERE IS NO BILL DATE                     
         BNZ   IOHOOK05                                                         
*                                                                               
IOHOOK02 MVI   TIMODE,PROCNOCK     DON'T PASS CHECKS                            
         B     IOHX                                                             
*                                                                               
IOHOOK05 MVC   SORTDTE,TIBIDATE    ELSE USE BILL DATE                           
         MVC   TEMPDATE,TIBIDATE                                                
*        BAS   RE,FIXHAND          FIX HANDLING AMOUNT IF NECCESSARY            
         B     IOHX                                                             
*                                                                               
IOHOOK10 CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   IOHX                                                             
         USING TLCKD,R4                                                         
         L     R4,TIAREC                                                        
         MVC   SORTSEQ,TLCKSORT+4  MAKE SORT UNIQUE IF NO TIME                  
         CLI   TLCKCD,TLCKCDQ      INSURE WE'RE PROCESSING CHECK REC.           
         BNE   IOHX                                                             
         OC    TIFSSN,TIFSSN       IF READING EMPLOYEE'S CHECKS                 
         BZ    IOHOOK20                                                         
         MVC   SORTDTE,TICKDATE    USE CHECK DATE FOR SORT DATE                 
*                                                                               
IOHOOK20 MVC   TGSSN,TISSN                                                      
         MVC   SORTSSN,TISSN       SOCIAL SECURITY NUMBER                       
         MVC   SORTDA,TIDSKADD     D/A                                          
         MVC   SORTINV,TLCKINV     INVOICE                                      
*                                                                               
         MVC   SORTAGY,TIAGY       SET AGENCY                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TATIELQ      IF THERE'S A TAX ID ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TATID,R4                                                         
         MVC   SORTSSN,TATIID      USE CORP ID                                  
*                                                                               
         XC    SORTTIM,SORTTIM                                                  
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABYELQ      IF THERE'S A BILLING YTD ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TABYD,R4                                                         
         MVC   SORTTIM,TABYTIME    GET BILLING TIME                             
*                                                                               
         L     R4,TIAREC                                                        
         XC    SORTCHK,SORTCHK                                                  
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACDD,R4                                                         
         MVC   SORTCHK,TACDCHK     CHECK NUMBER                                 
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   SORTCLI,TAPDCLI     SET CLIENT                                   
         MVI   SORTCUR,C'U'        CURRENCY                                     
         TM    TAPDSTAT,TAPDSCAN                                                
         BZ    *+8                                                              
         MVI   SORTCUR,C'C'                                                     
         MVC   SORTEMP,TAPDEMP     EMPLOYER                                     
*                                                                               
         AP    CHKCOUNT,=P'1'                                                   
         BAS   RE,PUTSORT                                                       
*                                                                               
IOHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES I/O HOOKS FROM SYSIO (COD INVOICES               
         SPACE 1                                                                
*ODHOOK  NTR1                                                                   
*NO-OP   CLI   TIMODE,PROCREC                                                   
*        BNE   CODX                                                             
*        MVC   TEMPDATE,TIDATE                                                  
*        BAS   RE,FIXHAND                                                       
*NO-OP                                                                          
*ODX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        IF BILLING TYPE 1 OR 2 & BILLED BETWEEN 10/7-10/16                     
*        MOVE IND HANDLING TO CORP HANDLING                                     
*                                                                               
FIXHAND  NTR1                                                                   
         CLC   TEMPDATE,=X'911007'    IF BILLED BETWEEN 10/7 & 10/15            
         BL    FHX                                                              
         CLC   TEMPDATE,=X'911016'                                              
         BH    FHX                                                              
         USING TABDD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABDELQ      GET BILLING DETAILS                          
         BAS   RE,GETEL                                                         
         BNE   FHX                                                              
         CLI   TABDTYPE,TABRTY1    & IF IT'S BILLING TYPE 1                     
         BE    FH10                                                             
         CLI   TABDTYPE,TABRTY2      OR BILLING TYPE 2                          
         BE    FH10                                                             
         CLI   TABDTYPE,TABRTY20     OR BILLING TYPE 20                         
         BNE   FHX                                                              
*                                                                               
FH10     OC    TABDHND,TABDHND     IF THERE IS NO HANDLING AMOUNT               
         BZ    FHX                 EXIT                                         
         MVC   TABDHNDC,TABDHND    MOVE IND HANDLING TO CORP HAND               
         XC    TABDHND,TABDHND     AND CLEAR IND HANDLING                       
         AP    INVCOUNT,=P'1'                                                   
         CLI   TWAWRITE,C'N'       IF WRITE = NO THEN DON'T                     
         BE    *+8                                                              
         MVI   TIMODE,PROCPTRC     ASK SYSIO TO PUT BACK RECORD                 
         GOTO1 MYTRAC2,DMCB,TIAREC,0,=C'INV PUTREC'                             
*                                                                               
FHX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS PUTS TO SORTER                                  
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         TM    STATUS,SORTING                                                   
         BO    PUTSORT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         OI    STATUS,SORTING                                                   
*                                                                               
PUTSORT2 GOTO1 MYSORTER,DMCB,=C'PUT',SORTREC                                    
*NO-OP   GOTO1 MYTRAC2,DMCB,SORTREC,L'SORTREC,=C'SORT PUT'                      
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS SORT RECORDS                                             
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         TM    STATUS,SORTING      DON'T BOTHER IF DON'T HAVE ANYTHING          
         BZ    XIT                                                              
         XC    LASTREC,LASTREC                                                  
         LA    R2,XP               R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         TM    OPTS,OPTTAPE        IF WRITING TO TAPE                           
         BZ    GETS10                                                           
         OPEN  (RCVTAPE,(OUTPUT))  OPEN IT NOW                                  
*                                                                               
GETS10   GOTO1 MYSORTER,DMCB,=C'GET' GET A RECORD FROM SORTER                   
         ICM   RF,15,4(R1)                                                      
         BZ    GETSX                                                            
         MVC   SORTREC,0(RF)       MOVE RECORD TO LOCAL STORAGE                 
*                                                                               
*NO-OP   GOTO1 MYTRAC2,DMCB,SORTREC,L'SORTREC,=C'SORT GET'                      
         CLC   SORTREC(SORTNEW),LASTREC   TEST REACHED CONTROL BREAK            
         BE    GETS30                                                           
*                                                                               
         CLC   SORTSSN,LASTREC+SORTSSN-SORTREC IF SSN CHANGED                   
         BE    GETS30                                                           
         BAS   RE,NEWSSN           HANDLE NEW S/S NUMBER                        
*                                                                               
         CLC   PRNTSSN,SORTSSN                                                  
         BE    GETS30                                                           
         MVC   PRNTSSN,SORTSSN             MOVE NEW SSN TO PRINT LINE           
*                                                                               
GETS30   CLC   SORTSSN(SORTNDTE),LASTREC+SORTSSN-SORTREC                        
         BE    GETS31                                                           
         GOTO1 DATCON,DMCB,(1,SORTDTE),(5,PRNTDTE) PRINT NEW BILL DATE          
*                                                                               
GETS31   BAS   RE,CHECK            GET CHECK RECORD                             
         NI    STATUS,X'FF'-CHANGED  INITIALIZE CHANGED STATUS                  
         BAS   RE,PROCESS          PROCESS CHECK                                
*                                                                               
         TM    STATUS,CHANGED      IF CHECK RECORD CHANGED                      
         BZ    GETS33                                                           
         MVI   PRNTCHG,C'*'        THIS RECORD CHANGED                          
         AP    CHACOUNT,=P'1'                                                   
**********************************************************                      
*                                                                               
         TM    OPTS,OPTTAPE        TEST WRITE TO TAPE                           
         BO    GETS32                                                           
         XC    KEY,KEY                                                          
         LA    R3,KEY              BUILD KEY                                    
         MVC   AIO,AIO2                                                         
         USING TLDRD,R3                                                         
         MVC   TLDRDA,SORTDA       SET D/A                                      
         GOTO1 GETREC              GET THE RECORD                               
         GOTO1 MYTRACE,DMCB,AIO,0,=C'GETREC'                                    
         MVC   AIO,AIO1            PREVENT PUTREC/GETREC SYNDROME               
*                                                                               
         GOTO1 PUTREC              WRITE IT BACK                                
         GOTO1 MYTRACE,DMCB,AIO,0,=C'PUTREC'                                    
         B     GETS33                                                           
**********************************************************                      
*                                                                               
GETS32   BAS   RE,PUTCPY           WRITE COPY TO TAPE                           
         GOTO1 MYTRACE,DMCB,=A(CHKIO),0,=C'COPY TO TAPE'                        
*                                                                               
         BAS   RE,PUTCHG           WRITE CHANGE TO TAPE                         
         GOTO1 MYTRACE,DMCB,AIO,0,=C'CHANGE TO TAPE'                            
**********************************************************                      
*                                                                               
GETS33   TM    OPTS,OPTCHG         ONLY PRINT                                   
         BNO   GETS35                                                           
         TM    STATUS,CHANGED      IF CHECK RECORD CHANGED                      
         BZ    GETS40                                                           
*                                                                               
GETS35   BAS   RE,PRREP                                                         
*                                                                               
GETS40   MVC   LASTREC,SORTREC     SAVE THIS RECORD                             
         B     GETS10              GET ANOTHER RECORD FROM SORTER               
*                                                                               
GETSX    GOTO1 MYSORTER,DMCB,=C'END'                                            
*                                                                               
         TM    OPTS,OPTTAPE        IF WRITING TO TAPE                           
         BZ    XIT                                                              
         CLOSE (RCVTAPE)           CLOSE IT NOW                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        HANDLE FIRST FOR S/S NUMBER                                            
*                                                                               
NEWSSN   NTR1                                                                   
         BAS   RE,GETYTD           GET YTD                                      
         XC    SORSTATE,SORSTATE   CLEAR SOR                                    
*                                                                               
         MVC   AIO,AIO2                                                         
         BAS   RE,SETTAL           SET TALENT FILES                             
*                                                                               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A4',SORTSSN)                              
         BNE   NSSNX                                                            
*                                                                               
         L     R4,AIO              R4 = A(FIRST W4 WITHHOLDING ELEMENT)         
         MVI   ELCODE,TAWHELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
NSSN10   BAS   RE,NEXTEL                                                        
         BNE   NSSNX                                                            
*                                                                               
         USING TAWHD,R4                                                         
         OC    TAWHEMP,TAWHEMP     IF NO EMPLOYER DEFINED                       
         BNZ   *+10                                                             
         MVC   TAWHEMP,TGTPEMP     SET TO DEFAULT EMPLOYER                      
*                                                                               
         CLC   TAWHEMP,TGTPEMP     ONLY USE TALENT PARTNERS FOR NOW             
         BNE   NSSN10              ** NOT SUPPORTING MULT. RULES YET **         
         CLC   TAWHUNIT,=C'FD '    SKIP IF THIS IS FEDERAL                      
         BE    NSSN10                                                           
         CLI   TAWHUNIT+2,C' '     FIND STATE OF RESIDENCE                      
         BNE   NSSN10                                                           
         MVC   SORSTATE,TAWHUNIT   SAVE STATE OF RESIDENCE                      
*                                                                               
NSSNX    BAS   RE,SETCHK           SET CHECK FILES                              
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT OUT INFO                                                         
*                                                                               
PRREP    NTR1                                                                   
         LA    R2,XP                R2=A(PRINT LINE)                            
         USING PRNTD,R2                                                         
         MVC   PRNTCK,SORTCHK                         CHECK NUMBER              
         MVC   PRNTST(2),TAXSTATE                     STATE                     
         MVC   PRNTAGY,SORTAGY                        AGENCY                    
         MVC   PRNTW4,W4TYPE                          W4 TYPE                   
         GOTO1 TINVCON,DMCB,SORTINV,PRNTINV,DATCON    INVOICE NUMBER            
*                                                                               
         LA    R3,BLOCK                                                         
         USING TMD,R3              R3=A(TALIM BLOCK)                            
         L     R4,TMTXEARN                                                      
         CLI   W4TYPE,TAW4TYCO      IF THIS IS CORPORATION                      
         BE    PRR05                                                            
         CLI   W4TYPE,TAW4TYTR     OR TRUSTEE                                   
         BE    PRR05                                                            
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BNE   PRR10                                                            
*                                                                               
PRR05    L     R4,TMNTEARN                                                      
*                                                                               
PRR10    LA    RF,PRNTCKER                EARNINGS                              
         BAS   RE,EDIT10                                                        
         BAS   RE,PRLINE1           PRINT 1ST LINE                              
*                                                                               
         TM    LINES,TAXABLE        DON'T PRINT TAXABLE LINE                    
         BO    *+8                                                              
         BAS   RE,PRTAXBLE          PRINT TAXABLE LINE                          
*                                                                               
         TM    LINES,TAXES          DON'T PRINT TAXES LINE                      
         BO    *+8                                                              
         BAS   RE,PRTAXES           PRINT TAXES                                 
*                                                                               
         TM    LINES,WGBASE         DON'T PRINT WAGE BASE LINE                  
         BO    *+8                                                              
         BAS   RE,PRWGBS            PRINT WAGE BASES                            
*                                                                               
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         BAS   RE,PRNTIT                                                        
         DROP  R1                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT 1ST LINE OF REPORT                                               
*                                                                               
PRLINE1  NTR1                                                                   
         LA    R1,TMYTDN           N'YTD FIELDS                                 
         LA    R5,TMNYTD           A(YTD AMOUNTS)                               
         LA    RF,PRNTYTD          PRINT FIELD                                  
*                                                                               
P1L10    L     R4,0(R5)                                                         
         BAS   RE,EDIT10                                                        
         LA    R5,4(R5)            BUMP TO NEXT FIELD                           
         LA    RF,11(RF)                                                        
         BCT   R1,P1L10                                                         
*                                                                               
         XR    R4,R4                                                            
         CLI   W4TYPE,TAW4TYCO            IF THIS IS CORPORATION                
         BE    P1L20                                                            
         CLI   W4TYPE,TAW4TYTR            OR TRUSTEE                            
         BE    P1L20                                                            
         CLI   W4TYPE,TAW4TYFO            OR FOREIGNER                          
         BE    P1L20                      NO OMED AMOUNT                        
         L     R4,TMNYEARN                                                      
*                                                                               
P1L20    LA    RF,PRNTOMED                ELSE OVER MEDICARE = NEW YTD          
         BAS   RE,EDIT10                                                        
*                                                                               
         L     R4,TMNYNTAX                                                      
         LA    RF,PRNTCORP                NON TAXABLE YTD                       
         BAS   RE,EDIT10                                                        
*                                                                               
         TM    OPTS,OPTDA                                                       
         BNO   P1LX                                                             
         GOTO1 HEXOUT,DMCB,SORTDA,PRNTDA+1,4,0                                  
*                                                                               
P1LX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT TAXABLE LINE OF REPORT                                           
*                                                                               
PRTAXBLE NTR1                                                                   
         LA    R2,XP2               TAXABLE AMOUNT AT GIVEN LEVEL               
         MVC   PRNTCHG,TMLEVEL      LEVEL TAXED AT                              
         MVC   PRNTCK(7),=C'TAXABLE'                                            
         LA    R1,TMYTDN           N'YTD FIELDS                                 
         LA    R5,TMTTOTAL         A(YTD AMOUNTS)                               
         LA    RF,PRNTYTD          PRINT FIELD                                  
*                                                                               
PRTB10   L     R4,0(R5)                                                         
         BAS   RE,EDIT10                                                        
         LA    R5,4(R5)            BUMP TO NEXT FIELD                           
         LA    RF,11(RF)                                                        
         BCT   R1,PRTB10                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT TAXES LINE OF REPORT                                             
*                                                                               
PRTAXES  NTR1                                                                   
         LA    R2,XP3               TAXES - TAXABLE * RATE                      
         MVC   PRNTCK(5),=C'TAXES'                                              
         L     R4,TMXTOTAL                                                      
         LA    RF,PRNTYTD           TOTAL TAXES                                 
         BAS   RE,EDIT10                                                        
*                                                                               
         LA    R1,5                N'YTD FIELDS                                 
         LA    R5,TMXFUI           A(YTD AMOUNTS)                               
         LA    RF,PRNTFUI          PRINT FIELD                                  
*                                                                               
PRTX10   L     R4,0(R5)                                                         
         BAS   RE,EDIT10                                                        
         LA    R5,4(R5)            BUMP TO NEXT FIELD                           
         LA    RF,11(RF)                                                        
         BCT   R1,PRTX10                                                        
*                                                                               
         OC    TMXFICR,TMXFICR                                                  
         BZ    PRTX20                                                           
         L     R4,TMXFICR                                                       
         LA    RF,PRNTOMED          FICA CREDITS                                
         BAS   RE,EDIT10Z                                                       
*                                                                               
PRTX20   TM    TMSTAT,TMSBIL                                                    
         BO    PRTX30                                                           
         L     R4,TMOSDI                                                        
         LA    RF,PRNTOMED          SDI FOR PAYROLL                             
         BAS   RE,EDIT10Z                                                       
*                                                                               
PRTX30   L     R4,TMOCORP                                                       
         LA    RF,PRNTCORP          CORP                                        
         BAS   RE,EDIT10                                                        
*                                                                               
         L     R4,TMOHAND                                                       
         LA    RF,PRNTHAND          HANDLING                                    
         BAS   RE,EDIT10                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT WAGE BASES                                                       
*                                                                               
PRWGBS   NTR1                                                                   
         LA    R2,XP4               WAGE BASES                                  
         MVC   PRNTCK(8),=C'WGE BASE'                                           
         LA    R1,4                N'YTD FIELDS                                 
         LA    R5,TMWGBS           A(WAGE BASES)                                
         LA    RF,PRNTFUI          PRINT FIELD                                  
*                                                                               
         TM    OPTS,OPTRATES       PRINT OUT RATES INSTEAD                      
         BNO   PRWB10                                                           
         MVC   PRNTCK(8),=C'RATES   '                                           
         LA    R5,TMBRATES         A(BILLING RATES)                             
         TM    OPTS,OPTCHECK       IF REQUESTING PAYROLL RATES                  
         BNO   PRWB10                                                           
         LA    R5,TMWGRT           A(ALLTAX RATES)                              
*                                                                               
PRWB10   L     R4,0(R5)                                                         
         BAS   RE,EDIT10                                                        
         LA    R5,4(R5)            BUMP TO NEXT FIELD                           
         LA    RF,11(RF)                                                        
         BCT   R1,PRWB10                                                        
*                                                                               
         L     R4,TMBSDI                                                        
         TM    OPTS,OPTRATES       PRINT OUT RATES INSTEAD                      
         BNO   PRWB20                                                           
         L     R4,TMBROMED         A(BILLING OVER MEDICARE RATE)                
         TM    TMSTAT,TMSBIL                                                    
         BO    PRWB20                                                           
         L     R4,TMRSDI           A(ALLTAX SDI RATE)                           
*                                                                               
PRWB20   LA    RF,PRNTOMED                                                      
         BAS   RE,EDIT10Z                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*              ROUTINE TO GET CHECK RECORD BASED ON SORT RECORD                 
         SPACE 1                                                                
CHECK    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY              BUILD KEY                                    
         USING TLDRD,R3                                                         
         MVC   TLDRDA,SORTDA       SET D/A                                      
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R0,=A(CHKIO)        COPY IT TO CHKIO                             
         LA    R1,4000                                                          
         LR    RF,R1                                                            
         L     RE,AIO                                                           
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CHECK RECORD                                  
         SPACE 1                                                                
PROCESS  NTR1                                                                   
         XC    SSNDATA(SSNLNQ),SSNDATA      CLEAR INDIVIDUAL'S DATA             
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FOUND IT EARLIER                             
         USING TAPDD,R4                                                         
         MVC   W4TYPE,TAPDW4TY     W4 TYPE                                      
         L     R1,TAPDPAYI         PAY AMOUNT INDIVIDUAL                        
         A     R1,TAPDPAYC         + PAY AMT CORP (TEST-FORCE W4 TYPE)          
         CLI   W4TYPE,TAW4TYCO     IF THIS IS CORPORATION                       
         BE    PROC30                                                           
         CLI   W4TYPE,TAW4TYTR     OR TRUSTEE                                   
         BE    PROC30                                                           
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BE    PROC30                                                           
         CLI   SORTCUR,C'C'        OR IF THIS IS A CANADIAN PAYMENT             
         BE    PROC30                                                           
*                                                                               
PROC20   ST    R1,EARNINGS                                                      
         MVC   NONEARN,TAPDREXP    REIMBURSED EXPENSES                          
         B     PROC40                                                           
*                                                                               
PROC30   A     R1,TAPDREXP         ADD IN REIMBURSED EXPENSES                   
         ST    R1,NONEARN                                                       
*                                                                               
PROC40   MVC   PNH,TAPDPNH                                                      
         MVC   MDED,TAPDMDED                                                    
         MVC   DUES,TAPDDUES                                                    
*                                                                               
         USING TACDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   PROC45                                                           
         OC    TACDDTE,TACDDTE     IF THIS CHECK WAS WRITTEN                    
         BZ    PROC45                                                           
         L     R4,AIO                                                           
         USING TACWD,R4                                                         
         MVI   ELCODE,TACWELQ      THEN GET CHECK WITHHOLDING ELEMENT           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PROC42   BAS   RE,NEXTEL                                                        
         BNE   PROC45                                                           
         TM    TACWSTAT,TACWSTAX   THAT HAS TAXABLE STATE                       
         BNO   PROC42                                                           
         CLC   =C'FD ',TACWUNIT    AND IS NOT FEDERAL                           
         BE    PROC42                                                           
         CLI   TACWUNIT+2,C' '     AND IT'S NOT A CITY                          
         BNE   PROC42                                                           
         MVC   TAXSTATE,TACWUNIT   & SET THAT UNIT                              
         B     PROC47                                                           
*                                                                               
PROC45   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                 TO GET TAX UNIT                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
         MVC   TAXSTATE,TACAUNIT                                                
         BAS   RE,SETTAXBL         SET TAXABLE STATE                            
*                                                                               
PROC47   XC    TAXAMTS(TAXLNQ),TAXAMTS                                          
         CLI   SORTCOD,C'Y'        IF THIS WAS A COD INVOICE                    
         BNE   PROC50                                                           
         MVI   ELCODE,TAYEELQ      GET YTD ELEMENT                              
         MVI   WORK,TAYETBIL                                                    
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   PROC50                                                           
         USING TAYED,R4                                                         
         L     R4,TGELEM                                                        
         MVC   TAXAMTS(TAXLNQ),TAYETXBL  AND SET AMTS THAT WERE TAXED           
*                                                                               
PROC50   BAS   RE,SUICALC          CALCULATE SUI (TAXABLE) EARNINGS             
*                                                                               
         LA    R2,BLOCK                                                         
         USING TMD,R2              R2=A(TALIM BLOCK)                            
         L     R5,ATHISYTD                                                      
         USING TYTDTABD,R5                                                      
         MVC   TYTDAMTS(TMYTDN*4),TMNYTD  UPDATE YTD TABLE                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO CALCULATE BILLING SUI                                       
*                                                                               
         SPACE 1                                                                
SUICALC  NTR1                                                                   
*NO-OP** BAS   RE,GETBTYP          GET BILLLING TYPE                            
         XC    BLOCK(256),BLOCK    INITIALIZE TALIM BLOCK                       
         LA    R2,BLOCK                                                         
         USING TMD,R2              R2=A(TALIM BLOCK)                            
         ST    RC,TMRC                                                          
         GOTO1 DATCON,DMCB,(1,TIQPEND),(0,TMEFDTE0)                             
*                                                                               
         MVC   TMEMP,SORTEMP       BUILD INPUT BLOCK FOR TALIMIT                
         MVC   TMCURR,SORTCUR                                                   
         MVI   TMBILTYP,TABRTY99   SET BILLING TYPE                             
         MVC   TMSSN,SORTSSN                                                    
         MVC   TMUNIT,TAXSTATE     TAXABLE STATE                                
         MVC   TMTXEARN,EARNINGS   PASS EARNINGS THIS CHECK                     
         MVC   TMNTEARN,NONEARN                                                 
         MVC   TMPNH,PNH                                                        
         L     R1,MDED                                                          
         A     R1,DUES                                                          
         ST    R1,TMMDED                                                        
         MVC   TMW4TYPE,W4TYPE                                                  
         TM    OPTS,OPTCHECK       IF REQUESTING PAYROLL RATES                  
         BNO   SUI02                                                            
         OI    TMSTAT,TMSCHK       SET CHECK STATUS IN TALIM                    
         B     SUI07                                                            
*                                                                               
SUI02    OI    TMSTAT,TMSBIL       ELSE SET FOR BILLING (DEFAULT)               
         LA    R3,TGBSRC                                                        
         LA    R1,TMBRFUI                                                       
         LA    R4,8                                                             
*                                                                               
SUI05    LH    R5,0(R3)            GET 1ST RATE                                 
         ST    R5,0(R1)                                                         
         LA    R3,2(R3)            BUMP TO NEXT AMOUNTS                         
         LA    R1,4(R1)                                                         
         BCT   R4,SUI05                                                         
*                                                                               
SUI07    L     R5,=A(TYTDTAB)                                                   
         USING TYTDTABD,R5                                                      
         MVC   TMYTD(TMYTDN*4),TYTDAMTS SET EXISTING YTD                        
*                                                                               
         CLI   SORTCOD,C'Y'        IF THIS WAS A COD INVOICE                    
         BNE   SUI08                                                            
         MVC   TMTAXBLE(TMTNAMT*4),TAXAMTS     USE AMOUNTS TAXED                
         OI    TMSTAT,TMSPAMT                  & PASS AMOUNTS TO TALIM          
*                                                                               
SUI08    CLI   TRACEOPT,C'Y'                                                    
         BNE   SUI10                                                            
         MVI   TMTRACE,C'Y'                                                     
*                                                                               
*NO-OP   GOTO1 MYTRAC2,DMCB,TMD,256,=C'TALIM BLOCK'                             
SUI10    GOTO1 =V(TALIM),DMCB,TMD      GO TO TALIMIT                            
*                                                                               
         BAS   RE,UPDTABY          UPDATE TABY ELEMENT                          
         BAS   RE,UPDTAYE          UPDATE TAYE ELEMENT                          
*                                                                               
         GOTO1 MYTRACE,DMCB,TMD,256,=C'TALIM BLOCK'                             
         B     XIT                                                              
         EJECT                                                                  
****                                                                            
****     GET THE BILLING TYPE                                                   
****                                                                            
****TYP  NTR1                                                                   
****     BAS   RE,SETTAL           SET TALENT FILES                             
****     MVC   AIO,AIO2                                                         
****     GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A4',SORTAGY)                              
****     BE    *+6                                                              
****     DC    H'0'                                                             
****                                                                            
****     L     R4,AIO                                                           
****     MVI   ELCODE,TABRELQ      GET BILLING RATES ELEMENT                    
****     USING TABRD,R4                                                         
****     BAS   RE,GETEL                                                         
****     BE    *+6                                                              
****     DC    H'0'                                                             
****     MVC   TGBTYPE,TABRTYPE    GET BILLING TYPE                             
****     MVC   TGBSRC,TABRRATE                 RATES                            
****     TM    TABRSTAT,TABRSSRC                                                
****     BNO   BTYP10                                                           
****     GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
****                                                                            
****10   CLC   SORTCLI,XSPACES                                                  
****     BNH   BTYP40                                                           
****     GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A4',SORTCLI)                              
****     BE    *+6                                                              
****     DC    H'0'                                                             
****                                                                            
****     L     R4,AIO                                                           
****     MVI   ELCODE,TABRELQ      GET BILLING RATES ELEMENT                    
****     USING TABRD,R4                                                         
****     BAS   RE,GETEL                                                         
****     BNE   BTYP40                                                           
****     OC    TABRTYPE,TABRTYPE                                                
****     BZ    BTYP40                                                           
****     MVC   TGBTYPE,TABRTYPE    GET BILLING TYPE                             
****                                                                            
****     OC    TABRRATE,TABRRATE                                                
****     BZ    BTYP30                                                           
****     MVC   TGBSRC,TABRRATE                 RATES                            
****                                                                            
****30   TM    TABRSTAT,TABRSSRC                                                
****     BNO   BTYP40                                                           
****     GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
****                                                                            
****40   BAS   RE,SETCHK           SET CHECK FILES                              
****     MVC   AIO,AIO1                                                         
****     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        UPDATE/ADD BILLING YTD ELEMENT                                         
*              R2 - TALIM BLOCK                                                 
*                                                                               
         USING TMD,R2                                                           
UPDTABY  NTR1                                                                   
*                                                                               
         USING TABYD,R4                                                         
UBY10    L     R4,AIO              ELSE FILTERING BILL DATE                     
         MVI   ELCODE,TABYELQ      GET BILLING YTD ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   UBY20                                                            
         CLC   TABYDATE,SORTDTE    IF TABYDATE ISN'T = BILL DATE                
         BE    UBYX                                                             
         MVC   TABYDATE,SORTDTE    SET IT TO BILL DATE                          
         B     UBY30                                                            
********************************************************************            
         CLI   SORTCOD,C'Y'        IF THIS WAS A COD INVOICE                    
         BE    UBYX                LEVEL WAS ALREADY SET                        
         CLC   TABYLVL,TMLEVEL     IF LEVEL CHANGED                             
         BE    UBYX                                                             
********************************************************************            
         B     UBYX                !!!! DON'T BOTHER WITH LEVEL !!!!            
********************************************************************            
         MVC   TABYLVL,TMLEVEL     SET TO CORRECT LEVEL                         
         B     UBY30               AND SET RECORD CHANGED                       
*                                                                               
UBY20    XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVI   TABYEL,TABYELQ                                                   
         MVI   TABYLEN,TABYLNQ                                                  
         MVC   TABYDATE,SORTDTE    BILL DATE                                    
         MVC   TABYLVL,TMLEVEL          LEVEL                                   
         GOTO1 ADDELEM                                                          
*                                                                               
UBY30    OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UBYX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        UPDATE/ADD YTD EARNINGS ELEMENT                                        
*                                                                               
UPDTAYE  NTR1                                                                   
*                                                                               
         USING TAYED,R4                                                         
UYE05    L     R4,AIO              ELSE FILTERING BILL DATE                     
         MVI   ELCODE,TAYEELQ      BILL YTD EARNINGS ELEMENT                    
         MVI   BYTE,TAYETBIL                                                    
         GOTO1 GETL,DMCB,(1,BYTE)  GET OLD ELEMENT                              
         BNE   UYE10                                                            
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         ZIC   RE,TAYELEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R4)                                                 
         MVI   ELEMENT,TACVELQ                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
         L     R4,AIO              ELSE FILTERING BILL DATE                     
         MVI   ELCODE,TAYEELQ      BILL YTD EARNINGS ELEMENT                    
         MVI   BYTE,TAYETBIL                                                    
         GOTO1 GETL,DMCB,(1,BYTE)  GET OLD ELEMENT                              
         BNE   UYE10                                                            
         L     R4,TGELEM                                                        
         CLI   TAYELEN,TAYELNQ     IF LENGTH OF ELEMENT CHANGED                 
         BE    UYE20                                                            
         GOTO1 DELL,DMCB,(1,BYTE)  DELETE OLD ELEMENT & ADD NEW ONE             
*                                                                               
UYE10    XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVI   TAYEEL,TAYEELQ                                                   
         MVI   TAYELEN,TAYELNQ                                                  
         MVI   TAYETYPE,TAYETBIL   SET FROM BILLING                             
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
         GOTO1 ADDELEM                                                          
         B     UYE05                                                            
*                                                                               
UYE20    CLC   TAYEEARN,TMNYEARN   IF YTD EARNINGS CHANGED,                     
         BE    UYE30                                                            
         MVC   TAYEEARN,TMNYEARN   SAVE YTD EARNINGS                            
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE30    CLC   TAYEFUI,TMNYFUI     IF FUI YTD CHANGED,                          
         BE    UYE40                                                            
         MVC   TAYEFUI,TMNYFUI     SAVE YTD FUI EARNINGS                        
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE40    CLC   TAYESUI,TMNYSUI     IF SUI YTD CHANGED,                          
         BE    UYE50                                                            
         MVC   TAYESUI,TMNYSUI     SAVE YTD SUI EARNINGS                        
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE50    CLC   TAYEFICA,TMNYFICA   IF FICA YTD CHANGED,                         
         BE    UYE60                                                            
         MVC   TAYEFICA,TMNYFICA   SAVE YTD FICA EARNINGS                       
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE60    CLC   TAYEMED,TMNYMED     IF MEDICARE YTD CHANGED,                     
         BE    UYE65                                                            
         MVC   TAYEMED,TMNYMED     SAVE YTD MED EARNINGS                        
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE65    CLC   TAYENTAX,TMNYNTAX   IF YTD NON TAXABLE EARNINGS CHANGED,         
         BE    UYE70                                                            
         MVC   TAYENTAX,TMNYNTAX   SAVE NEW NON TAXABLE EARNINGS                
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE70    CLC   TAYETERN,TMTTOTAL   IF TAXABLE AMOUNT CHANGED                    
         BE    UYE80                                                            
         MVC   TAYETERN,TMTTOTAL                                                
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE80    CLC   TAYETFUI,TMTFUI     IF FUI TAXABLE CHANGED,                      
         BE    UYE90                                                            
         MVC   TAYETFUI,TMTFUI                                                  
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE90    CLC   TAYETSUI,TMTSUI     IF SUI TAXABLE CHANGED,                      
         BE    UYE100                                                           
         MVC   TAYETSUI,TMTSUI                                                  
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE100   CLC   TAYETFIC,TMTFICA    IF FICA TAXABLE CHANGED,                     
         BE    UYE110                                                           
         MVC   TAYETFIC,TMTFICA                                                 
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE110   CLC   TAYETMED,TMTMED     IF MEDICARE TAXABLE CHANGED,                 
         BE    UYE120                                                           
         MVC   TAYETMED,TMTMED                                                  
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYE120   CLC   TAYETOMD,TMTOMED    IF OVER MEDICARE TAXABLE CHANGED,            
         BE    UYEX                                                             
         MVC   TAYETOMD,TMTOMED                                                 
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
*                                                                               
UYEX     B     XIT                                                              
         EJECT                                                                  
*              GET YTD AT FIRST FOR SSN                                         
*                                                                               
GETYTD   NTR1                                                                   
         L     R5,=A(TYTDTAB)                                                   
         ST    R5,ATHISYTD                                                      
         XC    0(TYTDLEN,R5),0(R5) CLEAR LOCAL YTD TABLE ENTRY                  
         USING TYTDTABD,R5                                                      
*                                                                               
         TM    OPTS,OPTASOF        TEST REQUEST AS OF A DATE                    
         BZ    GYTDX                                                            
*                                                                               
         XC    YTDBLK,YTDBLK       CLEAR TAYTD PARAMETER BLOCK                  
         LA    R3,YTDBLK                                                        
         USING TYD,R3                                                           
*                                                                               
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         LA    R4,TAYTDTAB                                                      
         ST    R4,TYATAB                                                        
         MVC   TYPEND,ASOFDATE     PASS AS/OF DATE                              
         MVC   TYEMP,SORTEMP       EMPLOYER                                     
         MVC   TYSSN,SORTSSN       SOCIAL SECURITY NUMBER                       
         OI    TYSTAT,TYSTBILL     REQUEST BILLING YTD                          
         MVC   TYCUR,SORTCUR       SET CURRENCY                                 
*                                                                               
         GOTO1 MYTRACE,DMCB,YTDBLK,L'YTDBLK,=C'YTD BLOCK'                       
*                                                                               
         CLI   TRACEOPT,C'Y'                                                    
         BNO   *+8                                                              
         MVI   TYTRACE,C'Y'                                                     
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R3)  BUILD YTD TABLE                  
*                                                                               
         TM    TYRETURN,TYFNDPTR   IF A POINTER WAS FOUND                       
         BZ    GYTDX                                                            
         MVC   TYTDAMTS(TYBYLNQ),TYBYTD SET YTD AMOUNTS                         
*                                                                               
         GOTO1 MYTRACE,DMCB,TYBYTD,TYBYLNQ,=C'RETURNED FROM YTD'                
*                                                                               
GYTDX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE SETS DEFAULT TAXABLE STATE IF NECESSARY                  
*                                                                               
SETTAXBL NTR1                                                                   
         CLI   W4TYPE,TAW4TYCO     GET OUT IF THIS IS CORPORATION               
         BE    SETTXX                                                           
         CLI   W4TYPE,TAW4TYTR     OR TRUSTEE                                   
         BE    SETTXX                                                           
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BE    SETTXX                                                           
*                                                                               
         MVC   AIO,=A(EMPIO)                                                    
*                                                                               
         MVC   TMPFULL,TAXSTATE                                                 
         CLI   TMPFULL+2,C' '      IF THIS IS A CITY                            
         BNH   SETT05                                                           
         GOTO1 TAXVAL,DMCB,(3,TMPFULL)                                          
         MVC   TMPFULL,TGTASTCY    FIND THE STATE IT BELONGS TO                 
         MVC   TAXSTATE,TGTASTCY                                                
*                                                                               
SETT05   BAS   RE,GETSTAT          LOOK UP STATE OF WORK ON EMP RECORD          
         BE    SETTXX              IF FOUND - DONE                              
*                                                                               
         MVC   TMPFULL,SORSTATE    STATE OF RESIDENCE                           
         BAS   RE,GETSTAT          LOOK UP STATE OF WORK ON EMP RECORD          
         BNE   SETT10              IF FOUND                                     
         MVC   TAXSTATE,SORSTATE   SET TAXABLE STATE IS STATE OF RES.           
         B     SETTXX                                                           
*                                                                               
SETT10   L     R4,=A(EMPIO)                                                     
         MVI   ELCODE,TAEDELQ                                                   
         USING TAEDD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   SETTXX                                                           
         MVC   TAXSTATE,TAEDDST    IF NOT USE DEFAULT STATE                     
         MVI   TAXSTATE+2,C' '                                                  
*                                                                               
SETTXX   MVC   AIO,AIO1                                                         
         BAS   RE,SETCHK           SET CHECK FILES                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        FIND STATE IN EMPLOYER RECORD                                          
*                      TFULL - STATE                                            
*                                                                               
GETSTAT  NTR1                                                                   
         MVC   AIO,=A(EMPIO)       SEARCH EMPLOYER RECORD FOR TAX ID            
         MVI   ELCODE,TATIELQ      ELEMENT FOR STATE OF WORK                    
         MVI   FULL,TATITYUN                                                    
         MVC   FULL+1(3),TMPFULL                                                
         GOTO1 GETL,DMCB,(4,FULL)  FIRST TRY TO FIND UNEMPLOYMENT ID            
         BNE   GS10                                                             
         MVC   AIO,AIO1                                                         
         B     YES                                                              
*                                                                               
GS10     MVI   FULL,TATITYTX       THEN TRY TO FIND TAX ID                      
         GOTO1 GETL,DMCB,(4,FULL)                                               
         MVC   AIO,AIO1                                                         
         BE    YES                 SET CC                                       
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO WRITE OLD CHECK RECORD TO TAPE                        
         SPACE 1                                                                
PUTCPY   NTR1                                                                   
         L     R4,=A(CHKIO)        R4=A(CHK REC)                                
         USING TLCKD,R4                                                         
         LR    R3,R4                                                            
         SH    R3,=H'28'           R3=A(RCV HDR)                                
         LH    R1,TLCKLEN          R1=REC LEN                                   
         LA    R1,28(R1)           ADD L'RCV HDR                                
         STH   R1,0(R3)            SAVE AT BEG OF RCV HDR                       
         MVC   4(2,R3),=X'7601'    SET CHKFIL1/COPY                             
         PUT   RCVTAPE,(R3)        WRITE RECORD TO TAPE                         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO WRITE NEW CHECK RECORD TO TAPE                        
         SPACE 1                                                                
PUTCHG   NTR1                                                                   
         L     R0,=A(CHKIO)        COPY IT TO CHKIO                             
         LA    R1,4000                                                          
         LR    RF,R1                                                            
         L     RE,AIO                                                           
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,=A(CHKIO)        R4=A(CHK REC)                                
         USING TLCKD,R4                                                         
         LR    R3,R4                                                            
         SH    R3,=H'28'           R3=A(RCV HDR)                                
         LH    R1,TLCKLEN          R1=REC LEN                                   
         LA    R1,28(R1)           ADD L'RCV HDR                                
         STH   R1,0(R3)            SAVE AT BEG OF RCV HDR                       
         MVC   4(2,R3),=X'7602'    SET CHKFIL1/CHANGE                           
         PUT   RCVTAPE,(R3)        WRITE RECORD TO TAPE                         
         B     XIT                                                              
         EJECT                                                                  
*              ODD ROUTINES                                                     
SETCHK   NTR1                                                                   
         MVC   SYSFIL,=CL8'CHKFIL' SET CHECK FILES                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         B     XIT                                                              
*                                                                               
SETTAL   NTR1                                                                   
         MVC   SYSFIL,=CL8'TALFIL' SET TALENT FILES                             
         MVC   SYSDIR,=CL8'TALDIR'                                              
         B     XIT                                                              
*                                                                               
EDIT10   NTR1                                                                   
         EDIT  (R4),(10,(RF)),2,FLOAT=-                                         
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
EDIT10Z  NTR1                                                                   
         EDIT  (R4),(10,(RF)),2,FLOAT=-,ZERO=BLANK                              
         B     XIT                                                              
         SPACE 2                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     XIT                                                              
         SPACE 1                                                                
MYTRACE  NTR1  ,                   TRACE A RECORD                               
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         CP    TRALIMIT,=P'0'                                                   
         BE    MYT10                                                            
         CP    TRACOUNT,TRALIMIT                                                
         BH    XIT                                                              
*                                                                               
MYT10    AP    TRACOUNT,=P'1'                                                   
         TM    OPTS,OPTCHG         ONLY PRINT                                   
         BNO   MYT20                                                            
         TM    STATUS,CHANGED      IF CHECK RECORD CHANGED                      
         BZ    XIT                                                              
*                                                                               
MYT20    LM    R2,R4,0(R1)                                                      
         ZIC   R5,8(R1)                                                         
         GOTO1 TRACE,DMCB,(R2),(R3),(R4),(R5)                                   
         B     XIT                                                              
         SPACE 1                                                                
MYTRAC2  NTR1  ,                   TRACE A RECORD                               
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         LM    R2,R4,0(R1)                                                      
         ZIC   R5,8(R1)                                                         
         GOTO1 TRACE,DMCB,(R2),(R3),(R4),(R5)                                   
         B     XIT                                                              
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERRXIT                                                           
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   XHEAD3+11(1),SYDCUR  DISPLAY CURRENCY                            
         MVC   XHEAD4+11(3),SYDEMP          EMPLOYER                            
         MVC   XHEAD4+139(17),SYDPD         PERIOD                              
         MVC   XP1+1(9),SORTSSN                                                 
         SPACE 1                                                                
         L     R4,MYABOX                                                        
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVC   BOXCOLS,XSPACES                                                  
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BC11,C'C'                                                        
         MVI   BC12,C'C'                                                        
         MVI   BC13,C'C'                                                        
         MVI   BC14,C'C'                                                        
         MVI   BC15,C'C'                                                        
         MVI   BC16,C'C'                                                        
         MVI   BC17,C'C'                                                        
         MVI   BR,C'R'                                                          
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXYORN,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
SORTCARD DC    CL80'SORT FIELDS=(1,34,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=53'                                    
         SPACE 1                                                                
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=VB,LRECL=2048,            X        
               MACRF=PM,BLKSIZE=8200,BUFNO=2                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              OTHER AREAS                                                      
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         WSPEC H1,2,RUN                                                         
         WSPEC H1,72,C'REFRESH BILLING YTD'                                     
         WSPEC H2,72,19X'BF'                                                    
         WSPEC H1,132,REPORT                                                    
         WSPEC H1,148,PAGE                                                      
         WSPEC H2,132,REQUESTOR                                                 
         WSPEC H3,2,C'CURRENCY'                                                 
         WSPEC H4,2,C'EMPLOYER'                                                 
         WSPEC H4,132,C'PERIOD'                                                 
         SPACE 1                                                                
         WSPEC H7,2,C'S/S NUMB  W C   DATE   AGENCY  INV#   CHECK # '           
         WSPEC H7,48,C'ST  EARNINGS   YTD EARN     FUI'                         
         WSPEC H7,87,C'SUI        FICA     MEDICARE   OMED/FICR'                
         WSPEC H7,129,C'HANDLING     CORP      DA/CAN'                          
         DC    X'00'                                                            
         SPACE 3                                                                
*                                                                               
*        TABLE OF YTD OF A PERFORMER                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'TAYTDTAB'                                                    
TAYTDTAB DS    CL(55*YTDLNQ)                                                    
         SPACE 1                                                                
*                                                                               
*        TABLE OF YTD OF ALL PERFORMERS                                         
*                                                                               
         DS    0D                                                               
         DC    CL8'*TYTDTAB'                                                    
TYTDTAB  DS    (2*TYTDLEN)X'00'                                                 
TYTDTABX DS    0C                                                               
*                                                                               
*        SAVED CHECK RECORD                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'**CHECK*'                                                    
         DC    28X'00'             RECOVERY HEADER                              
CHKIO    DS    4000X'00'                                                        
*                                                                               
*        SAVED EMPLOYER RECORD                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**EMP***'                                                    
EMPIO    DS    4000X'00'                                                        
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 1                                                                
MYD      DSECT                                                                  
ATHISYTD DS    A                   A(YTD ENTRY IN TABLE)                        
MYABOX   DS    A                   A(BOXES)                                     
MYSORTER DS    A                   A(SORTER)                                    
*                                                                               
SORTREC  DS    0CL53               * SORT RECORD                                
SORTSSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
SORTCUR  DS    CL1                 CURRENCY                                     
SORTEMP  DS    CL3                 EMPLOYER                                     
SORTNEW  EQU   *-SORTREC           * CONTROL BREAK                              
SORTDTE  DS    PL3                 BILL DATE                                    
SORTNDTE EQU   *-SORTREC                                                        
SORTTIM  DS    XL4                 BILL TIME                                    
SORTAGY  DS    CL6                 AGENCY                                       
SORTINV  DS    CL6                 INVOICE                                      
SORTSEQ  DS    XL2                 MAKE SORT UNIQUE IF NO TIME                  
SORTNKEY EQU   *-SORTREC                                                        
*                                  * END OF SORT KEY                            
SORTCOD  DS    CL1                 WAS A COD                                    
SORTCLI  DS    CL6                 CLIENT                                       
SORTCHK  DS    CL8                 CHECK NUMBER                                 
SORTDA   DS    CL4                 D/A                                          
*                                  * END OF SORT RECORD                         
         SPACE 1                                                                
LASTREC  DS    CL(L'SORTREC)       SAVED PREVIOUS SORT RECORD                   
         SPACE 1                                                                
STATUS   DS    XL1                 LOCAL STATUS BYTE                            
SORTING  EQU   X'80'               SORT IS ACTIVE                               
CHANGED  EQU   X'40'               CHECK RECORD CHANGED                         
         SPACE 1                                                                
OPTS     DS    XL1                 OPTIONS                                      
OPTDA    EQU   X'80'               DISPLAY D/A                                  
OPTCHG   EQU   X'40'               ONLY PRINT CHANGED RECORDS                   
OPTRATES EQU   X'20'               PRINT OUT RATES INSTEAD OF WAGE BASE         
OPTCHECK EQU   X'10'               USE PAYROLL RATES                            
OPTTAPE  EQU   X'08'               WRITE TO TAPE                                
OPTASOF  EQU   X'04'               REGENERATE AS OF A DATE                      
         SPACE 1                                                                
ASOFDATE DS    XL3                 AS OF DATE WHEN OPTION REQUESTED             
LINES    DS    XL1                 WHAT LINES NOT TO DISPLAY                    
TAXABLE  EQU   X'80'               TAXABLE                                      
TAXES    EQU   X'40'               TAXES                                        
WGBASE   EQU   X'20'               WAGE BASE                                    
         SPACE 1                                                                
*                                                                               
TMPFULL  DS    F                   TEMPORARY STORAGE                            
SORSTATE DS    CL3                 STATE OF RESIDENCE                           
*                                                                               
SSNDATA  DS    0C                  VALUES FOR INDIVIDUAL CHECKS                 
W4TYPE   DS    CL1                 W4 TYPE                                      
TAXSTATE DS    CL3                 TAXABLE STATE                                
EARNINGS DS    F                   EARNINGS                                     
NONEARN  DS    F                   NON TAXABLE EARNINGS                         
PNH      DS    F                   P & H                                        
MDED     DS    F                   MISC DEDUCTIONS                              
DUES     DS    F                   UNION DUES (ANOTHER DEDUCTION)               
SSNLNQ   EQU   *-SSNDATA                                                        
         SPACE 1                                                                
TAXAMTS  DS    0F                                                               
TAXTOTAL DS    F                                                                
TAXFUI   DS    F                                                                
TAXSUI   DS    F                                                                
TAXFICA  DS    F                                                                
TAXMED   DS    F                                                                
TAXOMED  DS    F                                                                
TAXLNQ   EQU   *-TAXAMTS                                                        
         SPACE 1                                                                
INVCOUNT DS    PL6                 INVOICE RECORD COUNT                         
CHKCOUNT DS    PL6                 CHECK RECORD COUNT                           
CHACOUNT DS    PL6                 CHANGE COUNT                                 
TRALIMIT DS    PL6                 MAX N'TRACE RECORDS                          
TRACOUNT DS    PL6                 N'RECORDS TRACED SO FAR                      
*                                                                               
TEMPDATE DS    XL3                 TEMP DATE (PWOS) FIELD                       
*                                                                               
YTDBLK   DS    CL(TYLNQ)           TAYTD PARM LIST                              
         EJECT                                                                  
*              DSECT TO COVER TYTDTAB                                           
         SPACE 1                                                                
TYTDTABD DSECT                                                                  
TYTDAMTS DS    0F                  CURRENT (RUN) Y-T-D AMOUNTS                  
TYTDEARN DS    F                                       EARNINGS                 
TYTDFUI  DS    F                                       FUI TAXABLE              
TYTDSUI  DS    F                                       SUI TAXABLE              
TYTDFICA DS    F                                       FICA TAXABLE             
TYTDMED  DS    F                                       MEDICARE TAXABLE         
TYTDNTAX DS    F                                       NON TAXABLE              
TYTDLEN  EQU   *-TYTDTABD                                                       
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
       ++INCLUDE TAYTDD                                                         
         EJECT                                                                  
*              DSECT TO COVER PRINT PRNTE                                       
         SPACE 1                                                                
PRNTD    DSECT                                                                  
BL       DS    CL1                                                              
PRNTSSN  DS    CL9                                                              
BC1      DS    CL1                                                              
PRNTW4   DS    CL1                                                              
BC2      DS    CL1                                                              
PRNTCHG  DS    CL1                                                              
BC3      DS    CL1                                                              
PRNTDTE  DS    CL8                                                              
BC4      DS    CL1                                                              
PRNTAGY  DS    CL6                                                              
BC5      DS    CL1                                                              
PRNTINV  DS    CL6                                                              
BC6      DS    CL1                                                              
PRNTCK   DS    CL8                                                              
BC7      DS    CL1                                                              
PRNTST   DS    CL2                                                              
BC8      DS    CL1                                                              
PRNTCKER DS    CL10                                                             
BC9      DS    CL1                                                              
PRNTYTD  DS    CL10                                                             
BC10     DS    CL1                                                              
PRNTFUI  DS    CL10                                                             
BC11     DS    CL1                                                              
PRNTSUI  DS    CL10                                                             
BC12     DS    CL1                                                              
PRNTFICA DS    CL10                                                             
BC13     DS    CL1                                                              
PRNTMED  DS    CL10                                                             
BC14     DS    CL1                                                              
PRNTOMED DS    CL10                                                             
BC15     DS    CL1                                                              
PRNTHAND DS    CL10                                                             
BC16     DS    CL1                                                              
PRNTCORP DS    CL10                                                             
BC17     DS    CL1                                                              
PRNTDA   DS    CL10                                                             
BR       DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD7D                                                       
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* DDSPOOLD                                                                      
* DDWIDED                                                                       
* DDSPLWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDBIGBOX                                                                      
* DDPERVALD                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055TAREP37   12/09/13'                                      
         END                                                                    
