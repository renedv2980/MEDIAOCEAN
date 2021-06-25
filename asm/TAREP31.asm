*          DATA SET TAREP31    AT LEVEL 043 AS OF 08/11/14                      
*PHASE T70331E,*                                                                
*INCLUDE TALIM                                                                  
         TITLE 'T70331 - REFRESH PAYROLL YEAR-TO-DATE'                          
********************************************************************            
*   THIS PROGRAM IS NOW DEFUNCT.  NOT NECESSARY TO USE THIS BECAUSE             
*   WE NOW HAVE ALL THE PG BUSINESS AND ARE NOT RECEIVING DATA TO UPLD          
*   FROM THEM.  ALSO, W2BUILD HAS BEEN MODIFIED TO HANDLE CREATING W2           
*   RECORDS FOR PG AND PP AND THIS PROGRAM SHOULD NOT BE USED BECAUSE           
*   IT COULD NOT HANDLE GENERATING THE CORRECT CITY EARNINGS IF THEY            
*   DIFFERED FROM THE STATE EARNINGS.                                           
********************************************************************            
T70331   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70331,R6,R5                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
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
         MVI   TIQDTYPE,TIQDCHK    SET FILTERING ON CHECK DATE                  
         GOTO1 DATCON,DMCB,(0,PVALEEND),(20,WORK)                               
         MVC   YEAR,WORK           SET CCYY                                     
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
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SYDEMPNH                        
         MVC   TIFEMP,TGEMP                                                     
         SPACE 1                                                                
         LA    R2,SYDSSNH          S/S NUMBER (OPTIONAL)                        
         XC    SYDSSNN,SYDSSNN                                                  
         OI    SYDSSNNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SYDSSNNH                        
         MVC   TIFSSN,TGSSN                                                     
         SPACE 1                                                                
VK30     LA    R2,SYDAGYH          AGENCY (OPTIONAL)                            
         XC    SYDAGYN,SYDAGYN     ** USE FOR CUSTOM CHANGES ONLY!! **          
         OI    SYDAGYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SYDAGYNH                        
         MVC   TIFAGY,TGAGY                                                     
         SPACE 1                                                                
VK40     LA    R2,SYDOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 1                                                                
VOPTS    NTR1                                                                   
         ZAP   RECLIMIT,=P'0'                                                   
         MVI   TRACEOPT,C'N'                                                    
         MVI   W2GENOPT,C'N'                                                    
         MVI   SPECIAL,C'N'                                                     
         MVI   MARKFILE,C'N'                                                    
         XC    ASOFDATE,ASOFDATE                                                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         LA    R4,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(10,(R4)),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    FLDINV                                                           
         USING SCAND,R4            R4=A(SCAN BLOCK)                             
         SPACE 1                                                                
OPT2     CLC   =C'W2GEN',SCDATA1   GENERATE W2 RECORDS                          
         BNE   OPT3                                                             
         MVC   W2GENOPT,SCDATA2                                                 
         CLI   W2GENOPT,C' '                                                    
         BNE   *+8                                                              
         MVI   W2GENOPT,C'Y'       IF NO RHS THEN ASSUME YES                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT3     CLC   =C'SPECIAL',SCDATA1 SPECIAL CUSTOM CHANGES ONLY                  
         BNE   OPT4                                                             
         MVC   SPECIAL,SCDATA2                                                  
         CLI   SPECIAL,C' '                                                     
         BNE   *+8                                                              
         MVI   SPECIAL,C'Y'        IF NO RHS THEN ASSUME YES                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   =C'TRACE',SCDATA1   TRACE OPTION                                 
         BNE   OPT6                                                             
         MVI   TRACEOPT,C'Y'                                                    
         ZAP   TRACOUNT,=P'0'                                                   
         ZAP   TRALIMIT,=P'10'                                                  
         L     R1,SCBIN2           (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   =C'LIMIT',SCDATA1   LIMIT OPTION                                 
         BNE   OPT8                                                             
         L     R1,SCBIN2           (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   =C'MARKFILE',SCDATA1  IF WRITING TO FILE                         
         BNE   OPT10                                                            
         MVI   MARKFILE,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   =C'ASOF',SCDATA1    REGENERATE AS START OF PERIOD                
         BNE   OPT20                                                            
         CLI   SCLEN2,0                                                         
         BE    FLDINV                                                           
         LA    R2,SCDATA2                                                       
         GOTO1 DTVAL,DMCB,(X'40',ASOFDATE)                                      
         BNE   FLDINV                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    B     FLDINV                                                           
         SPACE 1                                                                
OPTEND   LA    R4,SCANNEXT                                                      
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS REPORT                                                   
         SPACE 1                                                                
PREP     NTR1                                                                   
         MVI   STATUS,0            INITIALIZE                                   
         MVI   STATUS2,0                                                        
         ZAP   CHKCOUNT,=P'0'                                                   
         ZAP   CHACOUNT,=P'0'                                                   
         ZAP   TAPECNT,=P'0'                                                    
         SPACE 1                                                                
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLCKCDQ      SET UP TO READ CHECKS                        
         OI    TIQFLAG2,TIQFPGRY   NEED TO PASS GREY RECORDS TOO                
         OI    TIFCDSTN,TACDSLIN+TACDSTRS  IGNORE LIENS & W4 TRUSTEES           
         OI    TIFPDPN,TAPDPCRD+TAPDPBNP  IGNORE CREDIT/BNP PAYMENTS            
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO HANDLE I/O                
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET CHECK FILES                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         SPACE 1                                                                
         BAS   RE,GETSORT          GET SORT RECORDS AND PROCESS                 
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'TALFIL' RESET BACK TO TAL FILES                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
         SPACE 1                                                                
         EDIT  CHKCOUNT,(9,P+1),ZERO=NOBLANK                                    
         MVC   P+11(13),=C'CHECK RECORDS'                                       
         EDIT  CHACOUNT,(9,P2+1),ZERO=NOBLANK                                   
         MVC   P2+11(7),=C'CHANGES'                                             
         CLI   W2GENOPT,C'Y'                                                    
         BNE   PRX                                                              
         EDIT  TAPECNT,(9,P3+1),ZERO=NOBLANK                                    
         MVC   P3+11(12),=C'TAPE RECORDS'                                       
PRX      BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES I/O HOOKS FROM SYSIO                             
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   IOHX                                                             
         L     R4,TIAREC                                                        
         USING TLCKD,R4                                                         
         CLI   TLCKCD,TLCKCDQ      INSURE WE'RE PROCESSING CHECK REC.           
         BNE   IOHX                                                             
         AP    CHKCOUNT,=P'1'                                                   
         MVC   SORTAGY,TLCKAGY     SET AGENCY                                   
         MVC   SORTSSN,TLCKSSN     SOCIAL SECURITY NUMBER                       
         SPACE 1                                                                
         MVI   ELCODE,TAOIELQ      ADJUSTMENTS NEED ORIGINAL AGENCY             
         BAS   RE,GETEL                                                         
         BNE   IOHOOK2                                                          
         USING TAOID,R4                                                         
         OC    TAOIAGY,TAOIAGY                                                  
         BZ    IOHOOK2                                                          
         MVC   SORTAGY,TAOIAGY                                                  
         SPACE 1                                                                
IOHOOK2  L     R4,TIAREC                                                        
         MVI   ELCODE,TATIELQ      IF THERE'S A TAX ID ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TATID,R4                                                         
         MVC   SORTSSN,TATIID      USE CORP ID                                  
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   IOHX                                                             
         USING TAPDD,R4                                                         
         CLI   W2GENOPT,C'Y'       IF GENERATING W2 RECORDS                     
         BNE   IOHOOK4                                                          
         CLI   TAPDW4TY,TAW4TYCO   THEN IGNORE CORP                             
         BE    IOHX                                                             
         CLI   TAPDW4TY,TAW4TYCA   AND CANADIAN                                 
         BE    IOHX                                                             
         CLI   TAPDW4TY,TAW4TYTR   AND TRUSTEE CHECKS                           
         BE    IOHX                                                             
IOHOOK4  MVI   SORTCUR,C'U'        CURRENCY                                     
         TM    TAPDSTAT,TAPDSCAN                                                
         BZ    *+8                                                              
         MVI   SORTCUR,C'C'                                                     
         MVC   SORTEMP,TAPDEMP     EMPLOYER                                     
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   IOHX                                                             
         USING TACDD,R4                                                         
         MVC   SORTDTE,TACDDTE     CHECK DATE                                   
         MVC   SORTSEQ,TACDSEQ     SEQUENCE NUMBER                              
         SPACE 1                                                                
         BAS   RE,PUTSORT                                                       
         SPACE 1                                                                
IOHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS PUTS TO SORTER                                  
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         TM    STATUS,SORTING                                                   
         BO    PUTSORT2                                                         
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    STATUS,SORTING                                                   
         SPACE 1                                                                
PUTSORT2 GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS SORT RECORDS                                             
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         TM    STATUS,SORTING      DON'T BOTHER IF DON'T HAVE ANYTHING          
         BZ    XIT                                                              
         XC    LASTREC,LASTREC                                                  
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         SPACE 1                                                                
GETS10   MVI   SORTREC,X'FF'       SET EOF MARKER IN LOCAL SORT RECORD          
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)         IF SORTER RETURNED A RECORD                  
         BZ    *+10                                                             
         MVC   SORTREC,0(RF)       MOVE RECORD TO LOCAL STORAGE                 
         SPACE 1                                                                
******** GOTO1 MYTRACE,DMCB,SORTREC,L'SORTREC,=C'SORT GET'                      
         SPACE 1                                                                
         CLC   SORTREC(SORTNEW),LASTREC   TEST REACHED CONTROL BREAK            
         BE    GETS30                                                           
         OC    LASTREC,LASTREC     SKIP IF FIRST TIME IN                        
         BZ    GETS25                                                           
         CLI   W2GENOPT,C'Y'       IF GENERATING W2 RECORDS                     
         BE    *+12                                                             
         TM    STATUS,YTDCHG       OR IF YTD CHANGED                            
         BZ    GETS20                                                           
         GOTO1 YTDTRACE,DMCB,=C'YTD',(C'B',CYBUFF)  TRACE BUFFER                
         SPACE 1                                                                
GETS20   CLI   W2GENOPT,C'Y'       IF GENERATING W2 RECORDS                     
         BNE   *+8                                                              
         BAS   RE,W2GEN            GENERATE W2 RECORD                           
         SPACE 1                                                                
GETS25   CLI   SORTREC,X'FF'       TEST GOT A RECORD FROM SORTER                
         BE    GETSX                                                            
         BAS   RE,INITYTD          INITIALIZE FOR NEW YTD CALC                  
         SPACE 1                                                                
         CLC   SORTSSN,LASTREC+SORTSSN-SORTREC IF SSN CHANGED                   
         BE    *+10                                                             
         MVC   PRNTSSN,SORTSSN     MOVE NEW SSN TO PRINT LINE                   
         SPACE 1                                                                
GETS30   CLC   SORTSSN(SORTSEQ-SORTREC),LASTREC+SORTSSN-SORTREC IF CHGD         
         BE    GETS34                                                           
         GOTO1 DATCON,DMCB,(1,SORTDTE),(4,PRNTDTE) PRINT NEW CHECK DATE         
         SPACE 1                                                                
GETS34   BAS   RE,CHECK            GET CHECK RECORD                             
         SPACE 1                                                                
         NI    STATUS,X'FF'-CHANGED-PRINTIT INITIALIZE CHANGED STATUS           
         NI    STATUS2,X'FF'-CHANGEDS                                           
         SPACE 1                                                                
         BAS   RE,PROCESS          PROCESS CHECK AGAINST BUFFER                 
         SPACE 1                                                                
         CLI   W2GENOPT,C'Y'       SKIP IF GENERATING W2 RECORDS                
         BE    GETS40                                                           
         CLI   SPECIAL,C'Y'        IF PROCESSING SPECIAL CUSTOM CHGS            
         BNE   *+16                                                             
         TM    STATUS2,CHANGEDS    AND SPECIAL CHANGE MADE                      
         BZ    GETS40                                                           
         B     *+12                                                             
         TM    STATUS,CHANGED      OR IF CHECK RECORD CHANGED                   
         BZ    GETS40                                                           
         GOTO1 PUTREC              WRITE IT BACK                                
         GOTO1 MYTRACE,DMCB,AIO2,0,=C'GETREC'                                   
         GOTO1 MYTRACE,DMCB,AIO,0,=C'PUTREC'                                    
         AP    CHACOUNT,=P'1'      ADD TO COUNT                                 
         OI    STATUS,YTDCHG       SET YTD CHANGED                              
         SPACE 1                                                                
         OC    TIFSSN,TIFSSN       IF S/S NUMBER FILTER DEFINED                 
         BNZ   *+12                                                             
         TM    STATUS,PRINTIT      OR IF PRINT BIT SET                          
         BZ    GETS40                                                           
         MVC   PRNTCHK,CHECKNO                   PRINT CHECK NUMBER             
         EDIT  (4,EARNINGS),(11,PRNTCKER),2,MINUS=YES  EARNINGS                 
         GOTO1 YTDTRACE,DMCB,=C'OLD CK',AIO2  TRACE ORIGINAL RECORD             
         GOTO1 YTDTRACE,DMCB,=C'NEW CK',AIO   TRACE CHANGED RECORD              
* NO-OP  GOTO1 YTDTRACE,DMCB,=C'YTDBUF',(C'B',CYBUFF) TEMP TRACE BUFFER         
         SPACE 1                                                                
GETS40   MVC   LASTREC,SORTREC     SAVE THIS RECORD                             
         B     GETS10              GET ANOTHER RECORD FROM SORTER               
         SPACE 1                                                                
GETSX    GOTO1 SORTER,DMCB,=C'END'                                              
         SPACE 1                                                                
         TM    STATUS,TAPEOPEN     IF TAPE OPENED                               
         BZ    XIT                                                              
         CLOSE W2TAPE              CLOSE IT                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INITIALIZES YTD FIELDS FOR NEW SSN                       
         SPACE 1                                                                
INITYTD  NTR1                                                                   
         L     RF,=AL4(NCYBUFF*TACYLNQ)   CLEAR BUFFER                          
         XCEFL CYBUFF                                                           
         NI    STATUS,X'FF'-YTDCHG        AND YTD CHANGED STATUS                
         XC    MYCKYEEL,MYCKYEEL          AND SAVED TAYE ELEMENT                
         XC    NEWAMTS,NEWAMTS            AND NEW YTD AMTS FOR PRINTING         
         SPACE 1                                                                
         OC    ASOFDATE,ASOFDATE   TEST REQUEST AS OF A DATE                    
         BZ    IYTDX               NO, SO GET OUT                               
         SPACE 1                                                                
         LA    R2,BLOCK            R2=A(YTD PARAMETER BLOCK)                    
         USING TYD,R2                                                           
         XC    0(TYLNQ,R2),0(R2)   CLEAR TAYTD PARAMETER BLOCK                  
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         MVC   TYCUR,SORTCUR       SET CURRENCY                                 
         MVC   TYEMP,SORTEMP       EMPLOYER                                     
         MVC   TYSSN,SORTSSN       SOCIAL SECURITY NUMBER                       
         MVC   TYPEND,ASOFDATE     PASS AS/OF DATE                              
         MVC   TYTRACE,TRACEOPT    TRACE OPTION                                 
         LA    R4,TYBUFF           PASS A(YTD TABLE FOR TAYTD)                  
         ST    R4,TYATAB                                                        
         SPACE 1                                                                
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R2)  GET YTD DATA                     
         SPACE 1                                                                
         OC    TIFSSN,TIFSSN       IF S/S NUMBER FILTER DEFINED                 
         BZ    IYTD20                                                           
         GOTO1 MYTRACE,DMCB,(R2),TYLNQ,=C'TAYTD BLOCK'  OK TO TRACE             
         SPACE 1                                                                
IYTD20   LA    R4,MYCKYEEL               R4=A(YTD EARNINGS ELEMENT)             
         USING TAYED,R4                                                         
         MVC   TAYENYTD(TYCYLNQ),TYCYTD  SET YTD AMOUNTS THERE                  
         SPACE 1                                                                
         LA    R4,TYBUFF           R4=A(YTD TABLE FROM TAYTD)                   
         USING YTDD,R4                                                          
         LA    R3,CYBUFF           R3=A(LOCAL YTD ELEMENT BUFFER)               
         USING TACYD,R3                                                         
IYTD30   CLI   0(R4),0             TEST END OF TAYTD TABLE                      
         BE    IYTDX                                                            
         CLC   YTDUNIT,=C'FD '     IF THIS IS FEDERAL ELEMENT                   
         BNE   IYTD34                                                           
         MVC   NEWREXP,YTDREXP     SET REIMB. EXPENSES                          
         XC    YTDREXP,YTDREXP     CLEAR THEM IN TAYTD TABLE                    
         OC    YTDAMTS(YTDAMTLQ),YTDAMTS  IF NO AMOUNTS IN TABLE                
         BZ    IYTD50                     THEN SKIP THIS ENTRY                  
         MVC   TACYFICA,YTDFICA    ELSE SET FICA                                
         B     IYTD40              AND CONTINUE                                 
         SPACE 1                                                                
IYTD34   CLI   YTDUNIT+2,C' '      IF THIS IS A STATE                           
         BNE   *+16                                                             
         MVC   TACYSUI,YTDSUI      SET SUI                                      
         MVC   TACYSDI,YTDSDI          SDI                                      
         SPACE 1                                                                
IYTD40   MVI   TACYEL,TACYELQ      ADD NEW ELEMENT TO BUFFER                    
         MVI   TACYLEN,TACYLNQ                                                  
         MVC   TACYUNIT,YTDUNIT    TAX UNIT                                     
         MVC   TACYEARN,YTDEARN    EARNINGS                                     
         MVC   TACYTAX,YTDTAX      TAX                                          
         SPACE 1                                                                
         LA    R3,TACYLNQ(R3)      BUMP TO NEXT ENTRY IN MY BUFFER              
IYTD50   LA    R4,YTDNEXT          BUMP TO NEXT ENTRY IN TAYTD TABLE            
         B     IYTD30              LOOP                                         
         SPACE 1                                                                
IYTDX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET CHECK RECORD BASED ON SORT RECORD                 
         SPACE 1                                                                
CHECK    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY              BUILD KEY                                    
         USING TLCKPD,R3                                                        
         MVI   TLCKPCD,TLCKECDQ    READ VIA EMPLOYEE'S CHECKS POINTER           
         MVC   TLCKECUR,SORTCUR    CURRENCY                                     
         MVC   TLCKEEMP,SORTEMP    EMPLOYER                                     
         MVC   TLCKESSN,SORTSSN    SOCIAL SECURITY NUMBER                       
         MVC   TLCKEDTE,SORTDTE    CHECK DATE                                   
         XC    TLCKEDTE,HEXFFS                                                  
         MVC   TLCKESEQ,SORTSEQ    SEQUENCE NUMBER                              
         XC    TLCKESEQ,HEXFFS                                                  
         MVC   TLCKEAGY,SORTAGY    AGENCY                                       
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         CLC   TLCKPKEY(TLCKEAGT-TLCKPD),KEYSAVE  DON'T CHECK AGENT             
         BE    *+6                                                              
         DC    H'0'                RECORD VANISHED                              
         SPACE 1                                                                
         GOTO1 GETREC              GET THE RECORD                               
******** GOTO1 MYTRACE,DMCB,AIO,0,=C'GETREC'                                    
         SPACE 1                                                                
         MVC   MYDSKADD,DMDSKADD   SAVE D/A                                     
         SPACE 1                                                                
         L     R0,AIO2             COPY IT TO I/O 2 FOR TRACE                   
         LA    R1,4000                                                          
         LR    RF,R1                                                            
         L     RE,AIO                                                           
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CHECK RECORD                                  
         SPACE 1                                                                
PROCESS  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FOUND IT EARLIER                             
         USING TAPDD,R4                                                         
         MVC   W4TYPE,TAPDW4TY     W4 TYPE                                      
         MVC   REIMBEXP,TAPDREXP   REIMBURSED EXPENSES                          
         MVC   PAYADJS,TAPDADJS    ADJUSTMENT INDICATOR                         
         SPACE 1                                                                
         XC    OLDAMTS,OLDAMTS     INITIALIZE SAVED AMTS FOR THIS CHK           
         MVI   ELCODE,TAYEELQ      GET EXISTING TAYE EL.                        
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         BNE   PROC20                                                           
         L     R4,TGELEM                                                        
         USING TAYED,R4                                                         
         MVC   OLDEARN,TAYEEARN    SAVE OLD YTD TAXABLE EARNINGS                
         MVC   OLDNTAX,TAYENTAX                 NON-TAXABLE EARNINGS            
         SPACE 1                                                                
PROC20   BAS   RE,MERGECW          MERGE ANY DUPLICATE TACW ELEMENTS            
         SPACE 1                                                                
         BAS   RE,BADTAXBL         DETECT BAD TAXABLE STATES                    
         SPACE 1                                                                
         MVI   BYTE,1              SET FIRST PASS                               
         BAS   RE,SETTAXBL         SET TAXABLE STATE                            
         SPACE 1                                                                
         OC    TAXSTATE,TAXSTATE   IF TAXABLE STATE WASN'T FOUND                
         BNZ   *+12                                                             
         MVI   BYTE,2              SET SECOND PASS                              
         BAS   RE,SETTAXBL         AND TRY AGAIN                                
         SPACE 1                                                                
         BAS   RE,DEFAULT          ASSIGN DEFAULT TAXABLE STATE IF NEC.         
         SPACE 1                                                                
         BAS   RE,TAXABLE          SET FEDERAL/CITY TAXABLE BITS IF NEC         
         SPACE 1                                                                
         BAS   RE,CYPROC           PROCESS CHECK YTD ELEMENTS                   
         SPACE 1                                                                
         BAS   RE,CDPROC           PROCESS CHECK DETAILS ELEMENT                
         SPACE 1                                                                
         BAS   RE,ODPROC           PROCESS OTHER DEDUCTION ELEMENTS             
         SPACE 1                                                                
         BAS   RE,CWPROC           PROCESS CHECK WITHHOLDING ELEMENTS           
*                                  AGAINST CHECK YTD ELEMENTS                   
         SPACE 1                                                                
         TM    PAYADJS,TAPDADST    IF THIS IS SSN/TRANSFER                      
         BZ    PROC90                                                           
* NO-OP  BAS   RE,GETBYTD          GET BILLING YTD DATA                         
* NO-OP  MVI   MYTMSTAT,TMSBIL+TMSNOBTY  TALIM TO CALC BILLING DATA             
* NO-OP  MVI   MYYETYPE,TAYETBIL   SET BILL TYPE FOR TAYEEL                     
* NO-OP  LA    R4,MYBIYEEL         R4=A(LOCAL YTD ELEMENT)                      
* NO-OP  BAS   RE,YEPROC           PROCESS YTD ELEMENT                          
         SPACE 1                                                                
PROC90   MVI   MYTMSTAT,TMSCHK     SET TALIM TO CALC PAYROLL DATA               
         MVI   MYYETYPE,TAYETCHK   SET CHECK TYPE FOR TAYEEL                    
         LA    R4,MYCKYEEL         R4=A(LOCAL YTD ELEMENT)                      
         BAS   RE,YEPROC           PROCESS YTD ELEMENT                          
         SPACE 1                                                                
PROCX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE MERGES DUPLICATE CHECK WITHHOLDING ELEMENTS              
         SPACE 1                                                                
MERGECW  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ      LOOP THROUGH CHECK WITHHOLDING ELS.          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
MER10    BAS   RE,NEXTEL                                                        
         BNE   MER40                                                            
         USING TACWD,R4            R4=A(CHECK WITHHOLDING EL.)                  
         SPACE 1                                                                
         OC    TACWUNIT,TACWUNIT   IF ELEMENT HAS NO UNIT                       
         BNZ   *+12                                                             
         MVI   TACWEL,X'FF'        SET TO DELETE IT                             
         B     MER10               AND DON'T BOTHER MERGING                     
         SPACE 1                                                                
         CLI   SORTCUR,C'C'        IF CAN$ CHECK                                
         BE    MER12                                                            
         CLI   W4TYPE,TAW4TYCO     OR IF THIS IS CORPORATION                    
         BE    MER10A                                                           
         CLI   W4TYPE,TAW4TYCA     OR CANADIAN                                  
         BE    MER10A                                                           
         CLI   W4TYPE,TAW4TYTR     OR TRUSTEE                                   
         BNE   MER11                                                            
MER10A   CLC   TACWUNIT,=C'CA '    AND UNIT IS NOT CA                           
         BE    MER15                                                            
         B     MER12                                                            
MER11    CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BNE   MER15                                                            
MER12    TM    TACWSTAT,TACWSTAX   AND UNIT MARKED TAXABLE                      
         BZ    MER15                                                            
         NI    TACWSTAT,ALL-TACWSTAX  TURN OFF TAXABLE BIT                      
         OI    STATUS,CHANGED         SET RECORD CHANGED STATUS                 
         SPACE 1                                                                
         OC    TACWWHLD,TACWWHLD   IF NOTHING WITHHELD                          
         BNZ   MER15                                                            
         MVI   TACWEL,X'FF'        SET TO DELETE ELEMENT                        
         B     MER10               AND DON'T BOTHER MERGING                     
         SPACE 1                                                                
MER15    LR    R3,R4                                                            
MER20    BAS   RE,NEXTEL2          LOOK FOR ANOTHER WITHHOLDING EL.             
         BNE   MER10                                                            
         CLC   TACWUNIT,TACWUNIT-TACWD(R3)  WITH SAME TAX UNIT                  
         BNE   *-14                                                             
         MVI   0(R3),X'FF'         FOUND A DUPLICATE - SET TO DELETE            
         SPACE 1                                                                
         LA    R1,TACWWHLD         SET TO ADD UP AMOUNTS                        
         LA    RF,TACWWHLD-TACWD(R3)                                            
         LA    R0,L'TACWWHLD/4                                                  
MER30    L     RE,0(R1)            ADD THEM ALL TO FIRST ELEMENT                
         A     RE,0(RF)                                                         
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,MER30                                                         
         B     MER20               LOOK FOR ADDITIONAL DUPLICATES               
         SPACE 1                                                                
MER40    MVI   ELCODE,X'FF'        IF THERE ARE ANY DUMMY ELEMENTS              
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   MERX                                                             
         OI    STATUS,CHANGED      SET RECORD CHANGED STATUS                    
         GOTO1 REMELEM             AND REMOVE THEM                              
MERX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS FOR BAD TAXABLE STATES AND REMOVES                 
         SPACE 1                                                                
BADTAXBL NTR1                                                                   
         NI    STATUS,X'FF'-HADTAXBL  INITIALIZE HAD TAXABLE STATUS             
         SPACE 1                                                                
         CLI   SORTCUR,C'C'        GET OUT IF CAN$ CHECK                        
         BE    BADTXX                                                           
*NO-OP   CLI   W4TYPE,TAW4TYCO     GET OUT IF THIS IS CORPORATION               
*4/97    BE    BADTXX                                                           
*        CLI   W4TYPE,TAW4TYCA     GET OUT IF THIS IS CANADIAN                  
*        BE    BADTXX                                                           
*        CLI   W4TYPE,TAW4TYTR     GET OUT IF THIS IS TRUSTEE                   
*        BE    BADTXX                                                           
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BE    BADTXX                                                           
         TM    PAYADJS,TAPDADST    GET OUT IF SSN/TRANSFER                      
         BO    BADTXX                                                           
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ      CHECK WITHHOLDING ELEMENTS                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
BADTX2   BAS   RE,NEXTEL                                                        
         BNE   BADTXX                                                           
         USING TACWD,R4                                                         
         SPACE 1                                                                
         OC    TACWUNIT,TACWUNIT   IGNORE DUMMIES                               
         BZ    BADTX2                                                           
         CLC   TACWUNIT,=C'FD '    AND FEDERAL                                  
         BE    BADTX2                                                           
         CLI   TACWUNIT+2,C' '     AND CITIES                                   
         BNE   BADTX2                                                           
         CLC   TACWUNIT,=C'OT '    OTHER SHOULD NEVER BE TAXABLE STATE          
         BE    BADTX8                                                           
         CLC   SORTEMP,=C'PG '     OR IF EMPLOYER IS PG                         
         BNE   *+12                                                             
         LA    R1,PGSTATES         ANY STATE IT'S NOT AN EMPLOYER IN            
         B     BADTX4                                                           
         CLC   SORTEMP,=C'PP '     OR IF EMPLOYER IS PP                         
         BNE   *+12                                                             
         LA    R1,PPSTATES         ANY STATE IT'S NOT AN EMPLOYER IN            
         B     BADTX4                                                           
         CLC   SORTEMP,=C'DM '     OR IF EMPLOYER IS DM                         
         BNE   BADTX2                                                           
         LA    R1,DMSTATES         ANY STATE IT'S NOT AN EMPLOYER IN            
BADTX4   CLC   TACWUNIT,0(R1)                                                   
         BE    BADTX2                                                           
         LA    R1,3(R1)            TRY NEXT STATE                               
         CLI   0(R1),C' '          NO MORE                                      
         BNE   BADTX4                                                           
         SPACE 1                                                                
BADTX8   TM    TACWSTAT,TACWSTAX   IF STATE MARKED TAXABLE                      
         BZ    BADTX2                                                           
         NI    TACWSTAT,X'FF'-TACWSTAX          TURN IT OFF                     
         OI    STATUS,CHANGED+PRINTIT+HADTAXBL  SET HAD A TAXABLE STATE         
         SPACE 1                                                                
BADTXX   B     XIT                                                              
         SPACE 1                                                                
PGSTATES DC    C'CA FL IL NJ NY OH   '                                          
PPSTATES DC    C'CA IL NY TX   '                                                
DMSTATES DC    C'CA FL CT IL MI MO NY TX PA NV   '                              
         EJECT                                                                  
*              ROUTINE SETS TAXABLE STATE                                       
         SPACE 1                                                                
*                                  BYTE = PASS NUMBER (1 OR 2)                  
SETTAXBL NTR1                                                                   
         XC    TAXSTATE,TAXSTATE   CLEAR TAXABLE STATE                          
         SPACE 1                                                                
         CLI   SORTCUR,C'C'        GET OUT IF CAN$ CHECK                        
         BE    STXX                                                             
*NO-OP   CLI   W4TYPE,TAW4TYCO     GET OUT IF THIS IS CORPORATION               
*4/97    BE    STXX                                                             
*        CLI   W4TYPE,TAW4TYCA     GET OUT IF THIS IS CANADIAN                  
*        BE    STXX                                                             
*        CLI   W4TYPE,TAW4TYTR     GET OUT IF THIS IS TRUSTEE                   
*        BE    STXX                                                             
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BE    STXX                                                             
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ      LOOP THROUGH CHECK WITHHOLDING ELS.          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
STX10    BAS   RE,NEXTEL                                                        
         BNE   STXX                                                             
         USING TACWD,R4            R4=A(CHECK WITHHOLDING EL.)                  
         SPACE 1                                                                
         OC    TACWUNIT,TACWUNIT   IGNORE ELEMENTS WITH NO UNIT                 
         BZ    STX10                                                            
         CLC   TACWUNIT,=C'FD '    IGNORE FEDERAL                               
         BE    STX10                                                            
         CLI   TACWUNIT+2,C' '     AND CITIES                                   
         BNE   STX10                                                            
         SPACE 1                                                                
         TM    TACWSTAT,TACWSTAX   IF TAXABLE BIT IS SET                        
         BZ    STX30                                                            
         CLI   SPECIAL,C'Y'        AND LOOKING FOR SPECIAL CHANGE               
         BNE   STX20                                                            
         CLC   TACWUNIT,=C'OH '    AND TAXABLE STATE IS OHIO                    
         BNE   STX20                                                            
         OC    TACWWHLD,TACWWHLD   AND THERE WAS NO WITHHOLDING                 
         BNZ   STX20                                                            
         OI    TACWSTAT,TACWSDEF   THEN MARK IT AS DEFAULT STATE                
         OI    STATUS2,CHANGEDS    SET CHANGED STATUS                           
         OI    STATUS,PRINTIT      AND SET TO PRINT THIS ENTRY                  
STX20    B     STX40               GO SAVE TAXABLE STATE                        
         SPACE 1                                                                
STX30    CLI   BYTE,2              ELSE IF THIS IS 2ND PASS                     
         BNE   STX10                                                            
         TM    STATUS,HADTAXBL     AND IF DIDN'T HAVE BAD TAXABLE ST.           
         BO    STX10                                                            
         OC    TACWWHLD,TACWWHLD   AND IF SOMETHING WITHHELD IN THIS ST         
         BZ    STX10                                                            
         OI    TACWSTAT,TACWSTAX   THEN THIS STATE MUST BE TAXABLE              
         OI    STATUS,CHANGED+PRINTIT  SET SOMETHING CHANGED                    
         SPACE 1                                                                
STX40    MVC   TAXSTATE,TACWUNIT   SAVE TAXABLE STATE                           
         SPACE 1                                                                
STXX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS DEFAULT TAXABLE STATE IF NECESSARY                  
         SPACE 1                                                                
DEFAULT  NTR1                                                                   
         CLI   SORTCUR,C'C'        GET OUT IF CAN$ CHECK                        
         BE    DEFX                                                             
*NO-OP   CLI   W4TYPE,TAW4TYCO     GET OUT IF THIS IS CORPORATION               
*4/97    BE    DEFX                                                             
*        CLI   W4TYPE,TAW4TYCA     GET OUT IF THIS IS CANADIAN                  
*        BE    DEFX                                                             
*        CLI   W4TYPE,TAW4TYTR     GET OUT IF THIS IS TRUSTEE                   
*        BE    DEFX                                                             
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BE    DEFX                                                             
         TM    PAYADJS,TAPDADST    GET OUT IF SSN/TRANSFER                      
         BO    DEFX                                                             
         SPACE 1                                                                
         OC    TAXSTATE,TAXSTATE   IF NO TAXABLE STATE DEFINED                  
         BNZ   DEFX                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ      AND THERE ARE WITHHOLDING ELS.               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DEF10    BAS   RE,NEXTEL                                                        
         BNE   DEFX                                                             
         USING TACWD,R4                                                         
         OC    TACWUNIT,TACWUNIT   (IGNORE DUMMIES)                             
         BZ    DEF10                                                            
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     THEN BUILD ELEMENT FOR DEFAULT STATE         
         LA    R4,ELEMENT                                                       
         MVI   TACWEL,TACWELQ                                                   
         MVI   TACWLEN,TACWLNQ                                                  
         OI    TACWSTAT,TACWSTAX+TACWSDEF   SET TAXABLE/DEFAULT STATE           
         SPACE 1                                                                
         MVC   TACWUNIT,=C'OH '    SET OHIO                                     
         CLC   SORTEMP,=C'PG '     FOR EMPLOYER PG                              
         BE    DEF20                                                            
         MVC   TACWUNIT,=C'NY '    SET NEW YORK                                 
         CLC   SORTEMP,=C'DM '     FOR EMPLOYER DM                              
         BE    DEF20                                                            
         MVC   TACWUNIT,=C'IL '    ELSE SET ILLINOIS                            
         SPACE 1                                                                
DEF20    MVC   TAXSTATE,TACWUNIT   SAVE TAXABLE STATE                           
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         OI    STATUS,CHANGED+PRINTIT  SET RECORD CHANGED STATUS                
         SPACE 1                                                                
DEFX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS TAXABLE BIT WHERE NECESSARY                         
         SPACE 1                                                                
TAXABLE  NTR1                                                                   
         NI    STATUS,X'FF'-FEDTAXBL  INITIALIZE FED. TAXABLE STATUS            
         SPACE 1                                                                
         CLI   SORTCUR,C'C'        GET OUT IF CAN$ CHECK                        
         BE    TAXX                                                             
*NO-OP   CLI   W4TYPE,TAW4TYCO     GET OUT IF THIS IS CORPORATION               
*4/97    BE    TAXX                                                             
*        CLI   W4TYPE,TAW4TYCA     GET OUT IF THIS IS CANADIAN                  
*        BE    TAXX                                                             
*        CLI   W4TYPE,TAW4TYTR     GET OUT IF THIS IS TRUSTEE                   
*        BE    TAXX                                                             
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BE    TAXX                                                             
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ      LOOP THROUGH CHECK WITHHOLDING ELS.          
         BAS   RE,GETEL                                                         
         BNE   TAXX                                                             
         SPACE 1                                                                
         USING TACWD,R4            R4=A(CHECK WITHHOLDING EL.)                  
TAX10    OC    TACWUNIT,TACWUNIT   IGNORE ELEMENTS WITH NO UNIT                 
         BZ    TAX40                                                            
         TM    TACWSTAT,TACWSTAX   AND IGNORE ELEMENTS ALREADY TAXABLE          
         BO    TAX40                                                            
         TM    PAYADJS,TAPDADST    NEVER CHANGE SSN/TRANSFERS                   
         BO    TAX40                                                            
         SPACE 1                                                                
         CLC   TACWUNIT,=C'FD '    IF THIS IS FEDERAL                           
         BNE   TAX20                                                            
         TM    TACWSTAT,TACWSWRK   AND IF WORKED BIT IS ON                      
         BO    TAX30               TAXABLE BIT SHOULD ALSO BE SET               
         CLC   TAXSTATE,=C'CN '    OR IF TAXABLE STATE IS NOT CANADA            
         BE    *+14                                                             
         CLC   TAXSTATE,=C'OT '    AND NOT OTHER                                
         BNE   TAX30               TAXABLE BIT SHOULD ALSO BE SET               
         B     TAX40                                                            
         SPACE 1                                                                
TAX20    CLI   TACWUNIT+2,C' '     ELSE IF THIS IS A CITY                       
         BE    TAX40                                                            
         TM    TACWSTAT,TACWSWRK   AND IF WORKED BIT IS ON                      
         BZ    TAX40                                                            
         GOTO1 TAXVAL,DMCB,(3,TACWUNIT)  GET PARENT STATE                       
         BNE   TAX40                                                            
         CLC   TGTASTCY,TAXSTATE   IF PARENT STATE IS TAXABLE STATE             
         BNE   TAX40                                                            
         SPACE 1                                                                
TAX30    OI    TACWSTAT,TACWSTAX   NEED TO SET TAXABLE BIT                      
         OI    STATUS,CHANGED+PRINTIT  SET RECORD CHANGED STATUS                
         SPACE 1                                                                
TAX40    CLC   TACWUNIT,=C'FD '    IF THIS IS FEDERAL                           
         BNE   *+16                                                             
         TM    TACWSTAT,TACWSTAX   AND IT'S MARKED TAXABLE                      
         BZ    *+8                                                              
         OI    STATUS,FEDTAXBL     SET LOCAL STATUS BIT                         
         SPACE 1                                                                
         CLI   W2GENOPT,C'Y'       IF GENERATING W2 RECORDS                     
         BNE   TAX50                                                            
         TM    TACWSTAT,TACWSTAX+TACWSDEF  AND STATE TAXABLE BY DEFAULT         
         BNO   TAX50                                                            
         NI    TACWSTAT,X'FF'-TACWSTAX     THEN TURN OFF TAXABLE BIT            
         XC    TAXSTATE,TAXSTATE           & CLEAR LOCAL TAXABLE STATE          
         SPACE 1                                                                
TAX50    BAS   RE,NEXTEL           LOOK FOR ANOTHER WITHHOLDING EL.             
         BE    TAX10                                                            
         SPACE 1                                                                
TAXX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES CHECK YTD ELEMENTS                             
         SPACE 1                                                                
CYPROC   NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,TACYELQ      LOOP THROUGH CHECK YTD ELS.                  
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
CYP10    BAS   RE,NEXTEL2                                                       
         BNE   CYP30                                                            
         USING TACYD,R3            R3=A(CHECK YTD EL.)                          
         SPACE 1                                                                
         OC    TACYUNIT,TACYUNIT   IF ELEMENT HAS NO UNIT                       
         BNZ   *+16                                                             
         MVI   TACYEL,X'FF'        SET TO DELETE IT                             
         OI    STATUS,CHANGED      SET RECORD CHANGED STATUS                    
         B     CYP10               LOOK FOR MORE                                
         SPACE 1                                                                
         LR    R4,R3                                                            
CYP20    BAS   RE,NEXTEL           LOOK FOR ANOTHER YTD EL.                     
         BNE   CYP25                                                            
         CLC   TACYUNIT,TACYUNIT-TACYD(R4)  WITH SAME TAX UNIT                  
         BNE   *+12                                                             
         MVI   0(R4),X'FF'         FOUND A DUPLICATE - SET TO DELETE            
         OI    STATUS,CHANGED+PRINTIT  SET RECORD CHANGED STATUS                
         B     CYP20               LOOK FOR MORE                                
         SPACE 1                                                                
CYP25    MVI   ELCODE,TACWELQ      LOOK FOR CORRES. WITHHOLDING EL.             
         GOTO1 GETL,DMCB,(3,TACYUNIT)                                           
         BE    CYP27               FOUND, SO OK                                 
         SPACE 1                                                                
         CLC   TACYUNIT,=C'FD '    NOT FOUND - IF UNIT IS NOT FEDERAL           
         BNE   *+14                            THEN DELETE IT                   
         OC    TACYAMTS,TACYAMTS   ELSE IF NOTHING IN EL THEN DELETE            
         BNZ   CYP27                                                            
         SPACE 1                                                                
         MVI   TACYEL,X'FF'        IF NOT FOUND THEN DELETE YTD EL.             
         OI    STATUS,CHANGED+PRINTIT  SET RECORD CHANGED STATUS                
         SPACE 1                                                                
CYP27    MVI   ELCODE,TACYELQ      RESET ELCODE                                 
         B     CYP10               CONTINUE LOOP                                
         SPACE 1                                                                
CYP30    L     R3,AIO              LOOP THROUGH CHECK YTD ELS. AGAIN            
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
CYP40    BAS   RE,NEXTEL2                                                       
         BNE   CYPX                                                             
         USING TACYD,R3            R3=A(CHECK YTD EL.)                          
         OC    TACYUNIT,TACYUNIT   IGNORE ELEMENTS WITH NO UNIT                 
         BZ    CYP40                                                            
         SPACE 1                                                                
         CLI   TACYLEN,TACYLNQ     IF ELEMENT IS OLD LENGTH                     
         BNL   CYP40                                                            
         XC    ELEMENT,ELEMENT     MOVE ELEMENT TO TEMP AREA                    
         ZIC   R1,TACYLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),TACYEL                                                
         MVI   ELEMENT+1,TACYLNQ   SET NEW LENGTH IN ELEMENT                    
         SPACE 1                                                                
         MVI   TACYEL,X'FF'        SET TO DELETE OLD ELEMENT                    
         OI    STATUS,CHANGED+PRINTIT  SET RECORD CHANGED STATUS                
         GOTO1 ADDELEM             ADD ELEMENT WITH CORRECT LENGTH              
         B     CYP30               START AT BEGINNING AND LOOK AGAIN            
         SPACE 1                                                                
CYPX     MVI   ELCODE,X'FF'        SET TO DELETE BAD ELEMENTS                   
         GOTO1 REMELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES CHECK DETAILS ELEMENT                          
         SPACE 1                                                                
CDPROC   NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FOUND IT EARLIER                             
         USING TACDD,R4                                                         
         MVC   CHECKNO,TACDCHK     SAVE CHECK NUMBER                            
         MVC   EARNINGS,TACDEARN        EARNINGS                                
         MVC   RUNDATE,TACDRUN          RUN DATE                                
         SPACE 1                                                                
         CLI   SORTCUR,C'C'        IF THIS IS CAN$ CHECK                        
         BNE   CDP10                                                            
         L     R1,TACDEARN         FORCE ALL EARNINGS TO BE NON-TAXABLE         
         LTR   R1,R1               IF THERE WERE TAXABLE EARNINGS               
         BZ    CDP20                                                            
         A     R1,TACDNTAX         ADD TO NON-TAXABLE EARNINGS                  
         ST    R1,TACDNTAX                                                      
         XC    TACDEARN,TACDEARN   NOW CLEAR NON-TAXABLE EARNINGS               
         OI    STATUS,CHANGED      AND SET RECORD CHANGED STATUS                
         B     CDP20                                                            
         SPACE 1                                                                
CDP10    CLI   W4TYPE,TAW4TYCO     IF THIS IS CORPORATION                       
         BE    CDP20                                                            
         CLI   W4TYPE,TAW4TYCA     OR CANADIAN                                  
         BE    CDP20                                                            
         CLI   W4TYPE,TAW4TYTR     OR TRUSTEE                                   
         BE    CDP20                                                            
         CLI   W4TYPE,TAW4TYFO     OR FOREIGNER                                 
         BNE   CDP30                                                            
         SPACE 1                                                                
CDP20    L     R1,TACDNTAX         NON-TAXABLE EARNINGS                         
         S     R1,REIMBEXP         LESS REIMBURSE EXPENSES                      
         ST    R1,EARNINGS         IS EARNINGS                                  
         SPACE 1                                                                
CDP30    MVC   OLDREXP,TACDYREX    SAVE OLD YTD REIMBURSED EXPENSES             
         L     R1,NEWREXP          PREVIOUS REIMBURSED EXPENSES                 
         CLI   W4TYPE,TAW4TYIN     IF THIS IS INDIVIDUAL                        
         BE    *+12                                                             
         CLI   W4TYPE,TAW4TYES     OR ESTATE                                    
         BNE   *+12                                                             
         TM    STATUS,FEDTAXBL     FEDERAL MUST BE TAXABLE TO ADD REIMB         
         BZ    *+12                                                             
         A     R1,REIMBEXP         PLUS CURRENT REIMBURSED EXPENSES             
         ST    R1,NEWREXP          IS NEW YTD REIMBURSED EXPENSES               
         C     R1,TACDYREX         IF IT DOESN'T MATCH WHAT'S IN EL.            
         BE    *+12                                                             
         ST    R1,TACDYREX         SET CORRECTED AMOUNT                         
         OI    STATUS,CHANGED      AND SET RECORD CHANGED STATUS                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES OTHER DEDUCTION ELEMENTS                       
         SPACE 1                                                                
ODPROC   NTR1                                                                   
         XC    FORTAX,FORTAX       CLEAR FOREIGN FEDERAL TAX                    
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAODELQ      LOOK FOR ANY OTHER DEDUCTION ELS.            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ODP10    BAS   RE,NEXTEL           LOOK FOR ANOTHER WITHHOLDING EL.             
         BNE   ODPX                                                             
         SPACE 1                                                                
         USING TAODD,R4                                                         
         OC    TAODAMT,TAODAMT     IF NO AMOUNT SET                             
         BNZ   *+16                                                             
         MVI   TAODEL,X'FF'        SET TO DELETE                                
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
         B     ODP10                                                            
         SPACE 1                                                                
         CLI   TAODTYPE,TAODTYPF   IF THIS IS FEDERAL TAX TYPE                  
         BNE   *+10                                                             
         MVC   FORTAX,TAODAMT      SAVE TAX AMOUNT                              
         SPACE 1                                                                
         B     ODP10                                                            
         SPACE 1                                                                
ODPX     MVI   ELCODE,X'FF'        SET TO DELETE BAD ELEMENTS                   
         GOTO1 REMELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES CHECK WITHHOLDING ELEMENTS                     
         SPACE 1                                                                
CWPROC   NTR1                                                                   
         NI    STATUS,ALL-PROCFDCW TURN OFF PROCESSED FD FLAG                   
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ      LOOP THROUGH CHECK WITHHOLDING ELS.          
         BAS   RE,GETEL                                                         
         BE    CWP14                                                            
CWP5     LA    R4,DMYCWEL          IF NONE, POINT TO DUMMY AND PROCESS          
         B     CWP14                                                            
         SPACE 1                                                                
CWP10    BAS   RE,NEXTEL           LOOK FOR ANOTHER WITHHOLDING EL.             
         BE    CWP14                                                            
         TM    STATUS,PROCFDCW     NO MORE - IF DIDN'T PROCESS FD EL.           
         BZ    CWP5                GO DO IT NOW (CORPS W/ CN TAX WTHLD)         
         B     XIT                 ELSE ALL DONE                                
         SPACE 1                                                                
         USING TACWD,R4            R4=A(CHECK WITHHOLDING EL.)                  
CWP14    OC    TACWUNIT,TACWUNIT   IGNORE ELEMENTS WITH NO UNIT                 
         BZ    CWP10                                                            
         CLC   TACWUNIT,=C'FD '    IF THIS IS FEDERAL WITHHOLDING EL.           
         BNE   *+8                                                              
         OI    STATUS,PROCFDCW     TURN ON PROCESSED FD FLAG                    
         SPACE 1                                                                
         LA    R3,CYBUFF           LOOK FOR THIS UNIT IN BUFFER                 
         USING TACYD,R3            R3=A(BUFFER)                                 
         LA    R0,NCYBUFF          R0=MAX N'ENTRIES IN BUFFER                   
         SPACE 1                                                                
CWP20    CLI   TACYEL,0            NO MORE ELEMENTS IN BUFFER                   
         BE    CWP30                                                            
         CLC   TACWUNIT,TACYUNIT   MATCH ON UNIT CODE                           
         BE    CWP40                                                            
         LA    R3,TACYLNQ(R3)                                                   
         BCT   R0,CWP20                                                         
         DC    H'0'                RAN OUT OF ROOM IN TABLE!!                   
         SPACE 1                                                                
CWP30    OC    FORTAX,FORTAX       UNLESS THERE'S FOREIGN FEDERAL TAX           
         BNZ   *+12                                                             
         C     R4,=A(DMYCWEL)      IF POINTING TO DUMMY ELEMENT                 
         BE    CWP80               THEN DON'T BOTHER                            
         MVI   TACYEL,TACYELQ      ELSE ADD NEW ELEMENT TO BUFFER               
         MVI   TACYLEN,TACYLNQ                                                  
         MVC   TACYUNIT,TACWUNIT   UNIT CODE                                    
         SPACE 1                                                                
CWP40    MVI   ELCODE,TACYELQ      GET EXISTING YTD EL. IN RECORD               
         GOTO1 GETL,DMCB,(3,TACWUNIT)  FOR THIS UNIT                            
         L     R2,TGELEM           R2=A(TACY EL. IN CHECK RECORD)               
         BE    CWP50                                                            
         OC    FORTAX,FORTAX       UNLESS THERE'S FOREIGN FEDERAL TAX           
         BNZ   *+12                                                             
         C     R4,=A(DMYCWEL)      IF POINTING TO DUMMY ELEMENT                 
         BE    CWP80               THEN DON'T BOTHER                            
         MVC   ELEMENT(TACYLNQ),TACYEL  ADD TACY EL. NOW                        
         GOTO1 ADDELEM                                                          
         OI    STATUS,CHANGED+PRINTIT   SET CHANGED STATUS                      
         B     CWP40               NOW GO GET IT                                
         SPACE 1                                                                
CWP50    L     R1,TACYEARN         PREVIOUS YTD EARNINGS                        
         TM    TACWSTAT,TACWSTAX   IF THIS IS TAXABLE UNIT                      
         BZ    *+12                                                             
         A     R1,EARNINGS         ADD CURRENT EARNINGS                         
         ST    R1,TACYEARN         NEW YTD EARNINGS                             
         C     R1,TACYEARN-TACYD(R2) IF IT DOESN'T MATCH WHAT'S IN EL.          
         BE    *+12                                                             
         ST    R1,TACYEARN-TACYD(R2) SET CORRECTED AMOUNT                       
         OI    STATUS,CHANGED+PRINTIT  AND SET RECORD CHANGED STATUS            
         SPACE 1                                                                
         L     R1,TACYTAX          PREVIOUS YTD TAXES                           
         A     R1,TACWTAX          PLUS CURRENT TAXES                           
         CLC   TACWUNIT,=C'FD '    IF THIS IS FEDERAL                           
         BNE   *+8                                                              
         A     R1,FORTAX           ADD FOREIGN FEDERAL TAX                      
         ST    R1,TACYTAX          IS NEW YTD TAXES                             
         C     R1,TACYTAX-TACYD(R2) IF IT DOESN'T MATCH WHAT'S IN EL.           
         BE    *+12                                                             
         ST    R1,TACYTAX-TACYD(R2) SET CORRECTED AMOUNT                        
         OI    STATUS,CHANGED+PRINTIT  AND SET RECORD CHANGED STATUS            
         SPACE 1                                                                
         CLC   TACWUNIT,=C'FD '    IF THIS IS FEDERAL                           
         BNE   CWP70                                                            
         L     R1,TACYFICA         PREVIOUS YTD FICA                            
         A     R1,TACWFICA         PLUS CURRENT FICA                            
         ST    R1,TACYFICA         IS NEW YTD FICA                              
         C     R1,TACYFICA-TACYD(R2) IF IT DOESN'T MATCH WHAT'S IN EL.          
         BE    *+12                                                             
         ST    R1,TACYFICA-TACYD(R2) SET CORRECTED AMOUNT                       
         OI    STATUS,CHANGED+PRINTIT  AND SET RECORD CHANGED STATUS            
         B     CWP80                                                            
         SPACE 1                                                                
CWP70    CLI   TACWUNIT+2,C' '     IF THIS IS STATE                             
         BNE   CWP80                                                            
         L     R1,TACYSDI          PREVIOUS YTD SDI                             
         A     R1,TACWSDI          PLUS CURRENT SDI                             
         ST    R1,TACYSDI          IS NEW YTD SDI                               
         C     R1,TACYSDI-TACYD(R2) IF IT DOESN'T MATCH WHAT'S IN EL.           
         BE    *+12                                                             
         ST    R1,TACYSDI-TACYD(R2) SET CORRECTED AMOUNT                        
         OI    STATUS,CHANGED+PRINTIT  AND SET RECORD CHANGED STATUS            
         SPACE 1                                                                
         L     R1,TACYSUI          PREVIOUS YTD SUI                             
         A     R1,TACWSUI          PLUS CURRENT SUI                             
         ST    R1,TACYSUI          IS NEW YTD SUI                               
         C     R1,TACYSUI-TACYD(R2) IF IT DOESN'T MATCH WHAT'S IN EL.           
         BE    *+12                                                             
         ST    R1,TACYSUI-TACYD(R2) SET CORRECTED AMOUNT                        
         OI    STATUS,CHANGED+PRINTIT  AND SET RECORD CHANGED STATUS            
         SPACE 1                                                                
CWP80    MVI   ELCODE,TACWELQ      RESET ELCODE                                 
         B     CWP10               LOOK FOR ANOTHER WITHHOLDING EL.             
         EJECT                                                                  
*              ROUTINE GETS BILLING YTD FOR SSN TRANSFERS                       
         SPACE 1                                                                
GETBYTD  NTR1                                                                   
         LA    R2,BLOCK            R2=A(YTD PARAMETER BLOCK)                    
         USING TYD,R2                                                           
         XC    0(TYLNQ,R2),0(R2)   CLEAR TAYTD PARAMETER BLOCK                  
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         MVC   TYPEND,RUNDATE      SET CHECK RUN DATE                           
         MVC   TYEMP,SORTEMP       EMPLOYER                                     
         MVC   TYSSN,SORTSSN       SOCIAL SECURITY NUMBER                       
         OI    TYSTAT,TYSTBILL     REQUEST BILLING YTD                          
         MVC   TYCUR,SORTCUR       SET CURRENCY                                 
         MVC   TYTRACE,TRACEOPT    TRACE OPTION                                 
         SPACE 1                                                                
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,(R2)  GET YTD AMOUNTS                  
         SPACE 1                                                                
         LA    R4,MYBIYEEL         BUILD BILLING YTD ELEMENT                    
         USING TAYED,R4                                                         
         XC    TAYEEL(TAYELNQ),TAYEEL                                           
         MVI   TAYEEL,TAYEELQ                                                   
         MVI   TAYELEN,TAYELNQ                                                  
         MVI   TAYETYPE,TAYETBIL            BILLING TYPE                        
         MVC   TAYENYTD(TMYTDN*4),TYBYTD    YTD AMOUNTS FROM TAYTD              
         SPACE 1                                                                
         MVC   KEY+TLDRDA-TLDRD(4),MYDSKADD  SET TO RE-READ MY CHECK            
         MVC   AIO,AIO2            READ DIRECTLY INTO OLD I/O AREA              
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            RESTORE I/O AREA OF UPDATED CHECK            
         SPACE 1                                                                
         LA    R4,ELEMENT          BUILD DUMMY TABY EL TO GET TLCKB PTR         
         XC    ELEMENT,ELEMENT                                                  
         USING TABYD,R4                                                         
         MVI   TABYEL,TABYELQ      ELEMENT CODE                                 
         MVI   TABYLEN,TABYLNQ     ELEMENT LENGTH                               
         MVC   TABYDATE,RUNDATE    RUN DATE                                     
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TABYELQ      LOOK FOR EXISTING TABY ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   GETB40                                                           
         CLC   TABYEL(TABYLNQ),ELEMENT  IF FOUND AND DIDN'T CHANGE              
         BE    GETBX                    THEN DONE                               
         GOTO1 DELL,DMCB,0         ELSE DELETE EL (W/O CREAMING ELEM)           
         SPACE 1                                                                
GETB40   GOTO1 ADDELEM             ADD NEW ELEMENT                              
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
         SPACE 1                                                                
GETBX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES NEW YTD ELEMENTS                              
         SPACE 1                                                                
         USING TAYED,R4            R4=A(LOCAL YTD ELEMENT)                      
YEPROC   NTR1                                                                   
         XCEF  TMD,'TMLNQ'         BUILD PARAMETER BLOCK FOR TALIM              
         MVC   TMEMP,SORTEMP       EMPLOYER                                     
         MVC   TMCURR,SORTCUR      CURRENCY                                     
         MVC   TMSSN,SORTSSN       S/S NUMBER                                   
         MVC   TMW4TYPE,W4TYPE     W4 TYPE                                      
         MVC   TMUNIT,TAXSTATE     TAXABLE STATE                                
         MVC   TMSTAT,MYTMSTAT     STATUS - CHECKS/BILLING                      
         ST    RC,TMRC             A(GEND)                                      
         GOTO1 DATCON,DMCB,(1,TIQPEND),(0,TMEFDTE0)  EFFECTIVE DATE             
         SPACE 1                                                                
         L     RE,EARNINGS         RE=TAXABLE EARNINGS                          
         L     RF,REIMBEXP         RF=REIMBURSED EXPENSES (NON-TAXABLE)         
         SPACE 1                                                                
         CLI   SORTCUR,C'C'        IF CAN$ CHECK                                
         BE    YEP10                                                            
         CLI   W4TYPE,TAW4TYCO     OR A CORP                                    
         BE    YEP10                                                            
         CLI   W4TYPE,TAW4TYCA     OR A CANADIAN                                
         BE    YEP10                                                            
         CLI   W4TYPE,TAW4TYTR     OR A TRUSTEE                                 
         BE    YEP10                                                            
         CLI   W4TYPE,TAW4TYFO     OR A FOREIGNER                               
         BNE   *+8                                                              
YEP10    AR    RF,RE               TREAT EARNINGS AS NON-TAXABLE                
         XR    RE,RE               AND CLEAR TAXABLE                            
         SPACE 1                                                                
         TM    PAYADJS,TAPDADST    IF THIS IS SSN/TRANSFER                      
         BZ    YEP15                                                            
         CLI   W4TYPE,TAW4TYIN     AND NOT AN INDIVIDUAL                        
         BE    *+12                                                             
         CLI   W4TYPE,TAW4TYES     OR AN ESTATE                                 
         BNE   YEP15                                                            
         TM    STATUS,FEDTAXBL     THEN ONLY PASS EARNINGS IF FED TAXBL         
         BZ    YEP17                                                            
         SPACE 1                                                                
YEP15    ST    RE,TMTXEARN         PASS TAXABLE EARNINGS                        
         ST    RF,TMNTEARN         AND NON-TAXABLE EARNINGS                     
         SPACE 1                                                                
YEP17    MVC   TMYTD(TMYTDN*4),TAYENYTD  PASS PREVIOUS YTD AMOUNTS              
         SPACE 1                                                                
         GOTO1 =V(TALIM),DMCB,TMD    OFF TO TALIM                               
         SPACE 1                                                                
         OC    TIFSSN,TIFSSN       IF S/S NUMBER FILTER DEFINED                 
         BZ    YEP20                                                            
         GOTO1 MYTRACE,DMCB,TMD,TMLNQ,=C'TALIM BLOCK'  OK TO TRACE THIS         
         SPACE 1                                                                
YEP20    XC    TAYEEL(TAYELNQ),TAYEEL       BUILD NEW YTD ELEMENT               
         MVI   TAYEEL,TAYEELQ                                                   
         MVI   TAYELEN,TAYELNQ                                                  
         MVC   TAYETYPE,MYYETYPE            CHECK/BILLING TYPE                  
         MVC   TAYENYTD(TMYTDN*4),TMNYTD    NEW YTD AMOUNTS                     
         MVC   TAYETXBL(TMTNAMT*4),TMTAXBLE TAXABLE AMOUNTS THIS CHECK          
         SPACE 1                                                                
         TM    TMSTAT,TMSCHK       IF CALCULATING PAYROLL DATA                  
         BZ    *+16                                                             
         MVC   NEWEARN,TAYEEARN    SAVE NEW YTD TAXABLE EARNINGS                
         MVC   NEWNTAX,TAYENTAX                 NON-TAXABLE EARNINGS            
         SPACE 1                                                                
         MVI   ELCODE,TAYEELQ      LOOK FOR EXISTING YTD ELEMENT                
         GOTO1 GETL,DMCB,(1,MYYETYPE)                                           
         BNE   YEP30                                                            
         L     R1,TGELEM           R4=A(YTD ELEMENT ALREADY ON RECORD)          
         CLC   TAYEEL(TAYELNQ),0(R1)  IF ELEMENT DIDN'T CHANGE                  
         BE    YEPX                   THEN GET OUT                              
         GOTO1 DELL,DMCB,(1,MYYETYPE) ELSE DELETE THIS ELEMENT                  
         SPACE 1                                                                
YEP30    MVC   ELEMENT,TAYEEL      SET TO ADD NEW YTD ELEMENT                   
         GOTO1 ADDELEM                                                          
         OI    STATUS,CHANGED      SET RECORD CHANGED                           
YEPX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT RECORD TRACES                                   
         SPACE 1                                                                
*                                  P1=A(LITERAL)  BYTE 0=L'LITERAL              
*                                  P2=A(I/O AREA) OR A(BUFFER)                  
         USING PRNTD,R2            R2=A(PRINT LINE)                             
YTDTRACE NTR1                                                                   
         CLI   W2GENOPT,C'Y'       IF GENERATING W2S                            
         BNE   YTDT5                                                            
         CP    RECLIMIT,=P'0'      IF LIMITING NUMBER OF RECORDS                
         BE    YTDT5                                                            
         CP    TAPECNT,RECLIMIT    CHECK NOT HIGHER THAN MAX                    
         BH    XIT                                                              
*                                                                               
YTDT5    L     RF,0(R1)            RF=A(LITERAL)                                
         L     R3,4(R1)                                                         
         MVC   TGBYTE,4(R1)                                                     
         ZIC   RE,0(R1)            MOVE LITERAL TO STATUS AREA                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRNTSTAT(0),0(RF)                                                
         SPACE 1                                                                
         C     R3,AIO2             IF POINTING TO SAVED I/O AREA                
         BNE   YTDT10                                                           
         EDIT  (4,OLDEARN),(11,PRNTYERN),2,MINUS=YES  PRINT OLD YTD             
         EDIT  (4,OLDNTAX),(11,PRNTYNTX),2,MINUS=YES                            
         EDIT  (4,OLDREXP),(10,PRNTYREX),2,MINUS=YES                            
         B     YTDT20                                                           
         SPACE 1                                                                
YTDT10   EDIT  (4,NEWEARN),(11,PRNTYERN),2,MINUS=YES  ELSE PRINT NEW            
         EDIT  (4,NEWNTAX),(11,PRNTYNTX),2,MINUS=YES                            
         EDIT  (4,NEWREXP),(10,PRNTYREX),2,MINUS=YES                            
         SPACE 1                                                                
YTDT20   CLI   TGBYTE,C'B'         IF NOT PRINTING BUFFER                       
         BE    *+8                                                              
         AH    R3,DATADISP         BUMP TO FIRST ELEMENT                        
         SPACE 1                                                                
         USING TACYD,R3                                                         
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,FIRSTEL2         DISPLAY ALL ELEMENTS                         
         BNE   YTDTX                                                            
YTDT30   OC    TACYUNIT,TACYUNIT   IGNORE ELEMENTS WITH NO UNIT                 
         BZ    YTDT40                                                           
         MVC   PRNTUNIT,TACYUNIT                                                
         EDIT  (4,TACYEARN),(11,PRNTEARN),2,MINUS=YES                           
         EDIT  (4,TACYTAX),(11,PRNTTAX),2,MINUS=YES                             
         EDIT  (4,TACYSDI),(11,PRNTSDI),2,MINUS=YES                             
         EDIT  (4,TACYSUI),(11,PRNTSUI),2,MINUS=YES                             
         BAS   RE,PRNTIT                                                        
YTDT40   BAS   RE,NEXTEL2                                                       
         BE    YTDT30                                                           
         SPACE 1                                                                
YTDTX    CLC   P,SPACES            IF ANYTHING LEFT TO PRINT                    
         BE    *+8                                                              
         BAS   RE,PRNTIT           PRINT IT NOW                                 
         B     XIT                                                              
         EJECT                                                                  
*              ADD A W2 RECORD BASED ON LOCAL BUFFERS                           
         SPACE 1                                                                
W2GEN    NTR1                                                                   
         TM    STATUS,TAPEOPEN     IF TAPE NOT OPENED YET                       
         BO    W2G10                                                            
         OPEN  (W2TAPE,OUTPUT)     OPEN IT                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                COULDN'T OPEN TAPE                           
         OI    STATUS,TAPEOPEN                                                  
         SPACE 1                                                                
W2G10    L     R2,AIO              R2 = A(NEW W2 RECORD)                        
         USING TLW2D,R2                                                         
         LR    RE,R2               PRE-CLEAR THE RECORD                         
         L     RF,SIZEIO                                                        
         XCEFL                                                                  
         MVI   TLW2CD,TLW2CDQ      FILL IN KEY                                  
         MVC   TLW2YEAR,YEAR       YEAR                                         
         MVC   TLW2CUR,LASTREC+SORTCUR-SORTREC  CURRENCY                        
         MVC   TLW2EMP,LASTREC+SORTEMP-SORTREC  EMPLOYER                        
         MVC   TLW2SSN,LASTREC+SORTSSN-SORTREC  S/S NUMBER                      
         MVC   TLW2CDTE,TGTODAY1   INITIAL CHANGE DATE IS TODAY                 
         XC    TLW2CDTE,HEXFFS     COMPLEMENTED                                 
         MVC   TLW2LEN,DATADISP    SET INITIAL RECORD LENGTH                    
         SPACE 1                                                                
         LA    R3,CYBUFF           R3 = A(BUFFER OF CHECK YTD ELS)              
         USING TACYD,R3                                                         
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,FIRSTEL2         IF NO CHECK YTD ELS IN BUFFER                
         BNE   W2GX                THEN DON'T CREATE W2 RECORD                  
         SPACE 1                                                                
W2G30    OC    TACYUNIT,TACYUNIT   IGNORE ELEMENTS WITH NO UNIT                 
         BNZ   *+6                                                              
         DC    H'0'                IF DIES, CHANGE ABOVE TO 'BE  W2G50'         
         CLC   TACYUNIT(2),=C'FD'  IF THIS IS NOT FEDERAL                       
         BE    *+14                                                             
         OC    TACYAMTS,TACYAMTS   IGNORE IF AMOUNTS ARE ZERO                   
         BZ    W2G50                                                            
         LA    R4,ELEMENT                                                       
         USING TAW2D,R4            R4 = A(W2 DETAILS ELEMENT)                   
         XC    ELEMENT(TAW2LNQ),ELEMENT                                         
         MVI   TAW2EL,TAW2ELQ      BUILD W2 EL CORRES. TO CHECK YTD EL          
         MVI   TAW2LEN,TAW2LNQ                                                  
         MVC   TAW2UNIT,TACYUNIT   UNIT                                         
         MVC   TAW2EARN,TACYEARN   EARNINGS                                     
         MVC   TAW2TAX,TACYTAX     TAXES                                        
         CLC   TAW2UNIT(2),=C'FD'  IF THIS IS FEDERAL                           
         BNE   W2G40                                                            
         MVC   TAW2FICA,TACYFICA   FICA                                         
         MVC   TAW2REXP,NEWREXP    REIMBURSED EXPENSES                          
         B     *+16                                                             
W2G40    MVC   TAW2SDI,TACYSDI     ELSE SDI                                     
         MVC   TAW2SUI,TACYSUI          SUI                                     
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD W2 DETAILS ELEMENT                       
         SPACE 1                                                                
W2G50    BAS   RE,NEXTEL2          GET NEXT CHECK YTD EL. FROM BUFFER           
         BE    W2G30                                                            
         SPACE 1                                                                
         LA    R4,ELEMENT                                                       
         USING TAWSD,R4            R4 = A(W2 SUBSIDIARY ELEMENT)                
         XC    ELEMENT(TAWSLNQ),ELEMENT                                         
         MVI   TAWSEL,TAWSELQ      BUILD NEW ELEMENT                            
         MVI   TAWSLEN,TAWSLNQ                                                  
         MVC   TAWSDPRT,TGTODAY1   SET PRINTED TODAY                            
         GOTO1 ADDELEM             ADD IT                                       
         SPACE 1                                                                
         MVC   ELEMENT,MYCKYEEL    ADD YTD EARNINGS ELEMENT                     
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
         SPACE 1                                                                
         CP    RECLIMIT,=P'0'      IF LIMITING NUMBER OF RECORDS                
         BE    W2G60                                                            
         CP    TAPECNT,RECLIMIT    CHECK NOT HIGHER THAN MAX                    
         BH    W2GX                                                             
         SPACE 1                                                                
W2G60    GOTO1 MYTRACE,DMCB,AIO,0,=C'W2 RECORD'                                 
         SPACE 1                                                                
         CLI   MARKFILE,C'Y'       IF WRITING TO THE FILE                       
         BNE   W2G70                                                            
         GOTO1 ADDREC              WRITE DIRECTLY TO FILE                       
         B     W2G80                                                            
         SPACE 1                                                                
W2G70    LH    R1,TLW2LEN          R1 = L'RECORD                                
         LA    R1,4(R1)                                                         
         SH    R2,=H'4'            BACK UP AND WRITE VAR. RECORD LEN.           
         XC    0(4,R2),0(R2)                                                    
         STH   R1,0(R2)                                                         
         PUT   W2TAPE,(R2)         WRITE THE RECORD TO TAPE                     
         SPACE 1                                                                
W2G80    AP    TAPECNT,=P'1'       ADD TO COUNT                                 
         SPACE 1                                                                
W2GX     B     XIT                                                              
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
MYTRACE  NTR1  ,                   TRACE A RECORD                               
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         CP    TRACOUNT,TRALIMIT                                                
         BH    XIT                                                              
         AP    TRACOUNT,=P'1'                                                   
         LM    R2,R4,0(R1)                                                      
         ZIC   RF,8(R1)                                                         
         GOTO1 TRACE,DMCB,(R2),(R3),(R4),(RF)                                   
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
         MVC   HEAD3+11(1),SYDCUR  DISPLAY CURRENCY                             
         MVC   HEAD4+11(3),SYDEMP          EMPLOYER                             
         MVC   HEAD4+15(36),SYDEMPN        EMPLOYER NAME                        
         MVC   HEAD4+106(17),SYDPD         PERIOD                               
         SPACE 1                                                                
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
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
         MVI   BR,C'R'                                                          
         MVC   BOXROWS,SPACES                                                   
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
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=26'                                    
DMYCWEL  DC    AL1(TACWELQ,TACWLNQ)  DUMMY FEDERAL WITHHOLDING EL.              
         DC    C'FD '                                                           
         DC    (TACWLNQ-(*-DMYCWEL))X'00'                                       
         DC    X'00'               SET EOR                                      
W2TAPE   DCB   DDNAME=W2TAPE,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              OTHER AREAS                                                      
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,52,C'REFRESH PAYROLL YTD'                                     
         SSPEC H2,52,19X'BF'                                                    
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,115,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SSPEC H3,2,C'CURRENCY'                                                 
         SSPEC H4,2,C'EMPLOYER'                                                 
         SSPEC H4,99,C'PERIOD'                                                  
         SPACE 1                                                                
         SSPEC H7,2,C'S/S NUMB  DATE   CHECK   CHECK-EARN  YTD-TAXABLE'         
         SSPEC H7,51,C'YTD-NONTXBL CD-YTDREXP UNT'                              
         SSPEC H7,80,C'EARNINGS'                                                
         SSPEC H7,94,C'TAX '                                                    
         SSPEC H7,104,C'SDI/FICA'                                               
         SSPEC H7,115,C'   SUI     STATUS'                                      
         DC    X'00'                                                            
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*CYBUFF*'         CHECK YTD ELEMENT BUFFER                     
CYBUFF   DC    (NCYBUFF*TACYLNQ)X'00'                                           
         DC    X'00'                                                            
NCYBUFF  EQU   60                  MAX N'ELEMENTS ALLOWED IN BUFFER             
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*TYBUFF*'         YTD BUFFER FOR TAYTD CALLS                   
TYBUFF   DC    (NCYBUFF*YTDLNQ)X'00'                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 1                                                                
MYD      DSECT                                                                  
SORTREC  DS    0CL26               * SORT RECORD                                
SORTSSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
SORTCUR  DS    CL1                 CURRENCY                                     
SORTEMP  DS    CL3                 EMPLOYER                                     
SORTNEW  EQU   *-SORTREC           * CONTROL BREAK                              
SORTDTE  DS    PL3                 CHECK DATE                                   
SORTSEQ  DS    XL4                 SEQUENCE NUMBER                              
*                                  * END OF SORT KEY                            
SORTAGY  DS    CL6                 AGENCY                                       
*                                  * END OF SORT RECORD                         
         SPACE 1                                                                
LASTREC  DS    CL(L'SORTREC)       SAVED PREVIOUS SORT RECORD                   
         SPACE 1                                                                
STATUS   DS    XL1                 LOCAL STATUS BYTE                            
SORTING  EQU   X'80'               SORT IS ACTIVE                               
CHANGED  EQU   X'40'               CHECK RECORD CHANGED                         
YTDCHG   EQU   X'20'               YTD CHANGED                                  
HADTAXBL EQU   X'10'               HAD A TAXABLE STATE (WAS BAD)                
FEDTAXBL EQU   X'08'               FEDERAL IS MARKED TAXABLE                    
PRINTIT  EQU   X'04'               PRINT THIS ITEM                              
PROCFDCW EQU   X'02'               PROCESSED FEDERAL WITHHOLDING EL.            
TAPEOPEN EQU   X'01'               TAPE IS OPENED                               
         SPACE 1                                                                
STATUS2  DS    XL1                 LOCAL STATUS BYTE 2                          
CHANGEDS EQU   X'80'               SPECIAL CUSTOM CHANGE MADE                   
         SPACE 1                                                                
*                                  * VALUES FOR INDIVIDUAL CHECKS *             
MYDSKADD DS    F                   D/A OF RECORD                                
CHECKNO  DS    CL8                 CHECK NUMBER                                 
W4TYPE   DS    CL1                 W4 TYPE                                      
PAYADJS  DS    XL1                 ADJUSTMENT INDICATORS                        
TAXSTATE DS    CL3                 TAXABLE STATE                                
EARNINGS DS    F                   EARNINGS                                     
REIMBEXP DS    F                   REIMBURSED EXPENSES                          
FORTAX   DS    F                   FOREIGN FEDERAL TAX                          
RUNDATE  DS    PL3                 CHECK RUN DATE                               
         SPACE 1                                                                
         DS    0F                                                               
NEWAMTS  DS    0CL12                                                            
NEWEARN  DS    F                   YTD TAXABLE EARNINGS                         
NEWNTAX  DS    F                   YTD NON-TAXABLE EARNINGS                     
NEWREXP  DS    F                   YTD REIMBURSED EXPENSES                      
         SPACE 1                                                                
OLDAMTS  DS    0CL12                                                            
OLDEARN  DS    F                   YTD TAXABLE EARNINGS                         
OLDNTAX  DS    F                   YTD NON-TAXABLE EARNINGS                     
OLDREXP  DS    F                   YTD REIMBURSED EXPENSES                      
         SPACE 1                                                                
CHKCOUNT DS    PL6                 CHECK RECORD COUNT                           
CHACOUNT DS    PL6                 CHANGE COUNT                                 
RECLIMIT DS    PL6                 RECORD LIMIT                                 
TRALIMIT DS    PL6                 MAX N'TRACE RECORDS                          
TRACOUNT DS    PL6                 N'RECORDS TRACED SO FAR                      
W2GENOPT DS    CL1                 GENERATE W2 RECORDS                          
SPECIAL  DS    CL1                 SPECIAL CUSTOM CHANGES ONLY                  
ASOFDATE DS    XL3                 REBUILD YTD AS OF THIS DATE                  
YEAR     DS    CL4                 YEAR FOR W2 RECORD                           
TAPECNT  DS    PL6                 TAPE RECORD COUNT                            
MARKFILE DS    CL1                 WRITE DIRECTLY TO FILE                       
         SPACE 1                                                                
MYCKYEEL DS    CL(TAYELNQ)         SAVED PAYROLL YTD ELEMENT                    
MYBIYEEL DS    CL(TAYELNQ)         SAVED BILLING YTD ELEMENT                    
         SPACE 1                                                                
MYTMSTAT DS    XL1                 TYPE FOR TALIM CALL                          
MYYETYPE DS    XL1                 TYPE FOR TAYEEL CREATED FROM TALIM           
         EJECT                                                                  
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
       ++INCLUDE TAYTDD                                                         
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
BL       DS    CL1                                                              
PRNTSSN  DS    CL9                                                              
BC1      DS    CL1                                                              
PRNTDTE  DS    CL5                                                              
BC2      DS    CL1                                                              
PRNTCHK  DS    CL8                                                              
BC3      DS    CL1                                                              
PRNTCKER DS    CL11                                                             
BC4      DS    CL1                                                              
PRNTYERN DS    CL11                                                             
BC5      DS    CL1                                                              
PRNTYNTX DS    CL11                                                             
BC6      DS    CL1                                                              
PRNTYREX DS    CL10                                                             
BC7      DS    CL1                                                              
PRNTUNIT DS    CL3                                                              
BC8      DS    CL1                                                              
PRNTEARN DS    CL11                                                             
BC9      DS    CL1                                                              
PRNTTAX  DS    CL11                                                             
BC10     DS    CL1                                                              
PRNTSDI  DS    CL11                                                             
BC11     DS    CL1                                                              
PRNTSUI  DS    CL11                                                             
BC12     DS    CL1                                                              
PRNTSTAT DS    CL6                                                              
BR       DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD1D                                                       
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* DDSPOOLD                                                                      
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
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043TAREP31   08/11/14'                                      
         END                                                                    
