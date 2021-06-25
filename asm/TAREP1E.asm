*          DATA SET TAREP1E    AT LEVEL 053 AS OF 03/08/16                      
*PHASE T7031EC,*                                                                
         TITLE 'T7031E - UNEMPLOYMENT INTERFACE FOR EQUIFAX'                    
T7031E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7031E,R6                                                      
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
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
*        XC    TUD(TULNQ),TUD      CLEAR LOCAL W/S                              
                                                                                
         LA    R2,SUIPDH           R2=A(FIELD)                                  
         BRAS  RE,VSFTDAT          VALIDATE USING SOFDAT                        
         BE    VKEY50              OK, CONTNUE                                  
         LA    R3,BLOCK            DO IT THE OLD WAY                            
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TUPERIOD,PVALCPER   SAVE DISPLAYABLE PERIOD IN W/S               
         MVC   TIQPSTR,PVALPSTA    SET PWOS DATES FOR SYSIO                     
         MVC   TIQPEND,PVALPEND                                                 
                                                                                
VKEY50   LA    R2,SUIEMPH          EMPLOYER                                     
         GOTO1 RECVAL,DMCB,(X'40',TLEMCDQ),(X'08',(R2)),SUIEMPNH                
         CLI   8(R2),C'@'          TEST FOR LEADING 'AT' SIGN                   
         BE    *+14                                                             
         MVC   TIFEMP,TGEMP                                                     
         B     *+14                                                             
         MVC   TIFEMP,TGLST        SET EMP FILTER IS A FLIST                    
         NI    TIFEMP,X'7F'                                                     
         SPACE 1                                                                
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         LA    R2,SUIOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         SPACE 1                                                                
VOPT4    DS    0H                                                               
* NO-OP  MVC   ERRDISP,SCDISP1     SET DISP. INTO FLD OF LHS FOR ERRORS         
         SPACE 1                                                                
         CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VOPT5                                                            
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUTRACE      SET TRACE ON                                 
         B     VOPT80                                                           
         SPACE 1                                                                
VOPT5    CLC   =C'TAPE',SCDATA1   TAPE                                          
         BNE   VOPT6                                                            
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUNOTAPE     SET NO TAPE                                  
         B     VOPT80                                                           
         SPACE 1                                                                
VOPT6    CLC   =C'REUSE',SCDATA1  REUSE                                         
         BNE   VOPT7                                                            
         CLC   =C'CHECKDATE',SCDATA2                                            
         BNE   FLDINV                                                           
         OI    TUOPTS,TURECKDT     USE CHECK DATE FOR REUSE PAYMENTS            
         B     VOPT80                                                           
         SPACE 1                                                                
VOPT7    CLC   =C'BENE',SCDATA1    BENEFITS                                     
         BNE   VOPT70                                                           
         OI    TUOPTS,TUBENFIT     SET FOR BENEFITS REPORTING                   
         B     VOPT80                                                           
         SPACE 1                                                                
VOPT70   DS    0H                                                               
         B     FLDINV                                                           
         SPACE 1                                                                
VOPT80   LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT4            AND CONTINUE                                 
         SPACE 1                                                                
VOPTX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
                                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLCKCDQ      READ CHECK RECORDS                           
         MVI   TIQDTYPE,TIQDCHK    SET FILTERING ON CHECK DATE                  
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
                                                                                
         TM    TUSTAT,TUSORTNG     DON'T BOTHER IF SORT NOT ACTIVE              
         BZ    PRX                                                              
                                                                                
         TM    TUOPTS,TUBENFIT     BENEFITS REPORTING?                          
         BO    PR300                                                            
         OPEN  (UITAPE,OUTPUT)     OPEN TAPE                                    
                                                                                
         BRAS  RE,RC000            HEADER RECORD                                
                                                                                
         BAS   RE,GETSORT          GET SORT RECORDS/WRITE TO TAPE               
                                                                                
         BRAS  RE,RC999            FOOTER RECORD                                
                                                                                
         CLOSE (UITAPE)            CLOSE TAPE                                   
         B     PRX                                                              
*---------------------------------------------------------------------          
PR300    DS    0H                                                               
         OPEN  (EITAPE,OUTPUT)     OPEN TAPES                                   
         OPEN  (PDTAPE,OUTPUT)                                                  
                                                                                
         BRAS  RE,EMPINFHD                                                      
         BRAS  RE,PAYDETHD                                                      
                                                                                
         BAS   RE,GETSORT                                                       
                                                                                
         CLOSE (PDTAPE)            CLOSE TAPES                                  
         CLOSE (EITAPE)                                                         
PRX      B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO AND WRITE TO SORT                     
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         USING TLCKD,R4                                                         
         L     R4,TIAREC           R4=A(RECORD)                                 
         ST    R4,AIO                                                           
         CLI   TLCKCD,TLCKCDQ      BRANCH IF CHECK RECORD                       
         BE    IOH5                                                             
         BAS   RE,SETPRINT         CHECK FOR PRINT PAYROLL AND SET INFO         
         B     IOHX                                                             
         SPACE                                                                  
IOH5     MVC   BYTE,TLCKINV+5      ALSO IGNORE CONVERTED RECORDS                
         NI    BYTE,X'7F'          TURN OFF CANCEL BIT IN INV NO. STAT.         
         CLI   BYTE,0              NEW INVOICES HAVE ZERO HERE                  
         BNE   IOHX                                                             
         GOTO1 MYTRACE,DMCB,=C'CHECK'                                           
         MVC   TGAGY,TLCKAGY       SET GLOBAL AGENCY                            
*&&DO                                                                           
         MVC   AIO,AIO1            SET AIO SO WON'T CREAM SYSIO'S REC           
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TISSN)      READ/SET GLOBAL           
         MVC   AIO,TIAREC          RESET AIO TO TIAREC                          
                                                                                
         BRAS  RE,SKIPCANC        SKIP CANADIAN W4 COUNTRY                      
         BNE   IOHX                                                             
*&&                                                                             
         LA    R2,TUSRTREC         R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
         MVC   SORTSSN,TISSN       S/S NUMBER FROM SYSIO                        
                                                                                
         MVC   SORTCAT,SPACES                                                   
         MVC   SORTCAT(3),TLCKCAT  CAST CATEGORY                                
         BRAS  RE,JOBTITLE         GET CAST CATEGORY NAME                       
                                                                                
         MVC   SORTAGY,TLCKAGY                                                  
         TM    TUOPTS,TUBENFIT     BENEFITS REPORTING?                          
         BZ    *+10                                                             
         MVC   SORTAGY2,TLCKAGY    NEEDS HIGHER SORT ORDER                      
                                                                                
         MVC   SORTINV,TLCKINV                                                  
         MVC   SVINV,TLCKINV                                                    
         MVC   SVCKSORT,TLCKSORT                                                
                                                                                
         CLI   TUPRINT,C'Y'        IF PRINT PAYROLL                             
         BNE   *+10                                                             
         MVC   SORTFRST,TUSHOOT    SET SHOOT DATE                               
                                                                                
         BRAS  RE,XCWEL                              CHECK WTH DETAILS          
         BNE   IOHX                                                             
         BAS   RE,XPDEL            EXTRACT DATA FROM PAY DETAILS EL.            
         BNE   IOHX                                                             
         BAS   RE,XCDEL                              CHECK DETAILS EL.          
         BNE   IOHX                                                             
         BAS   RE,XCYEL                              CHECK YTD DETAILS          
         BRAS  RE,XYEEL                              CHECK YTD ERN DETL         
         BAS   RE,XLWEL                              CHECK LIEN DETAILS         
         BAS   RE,XSOEL                              SOAP DETAILS EL.           
         BAS   RE,XCAEL                              CAST DETAILS EL.           
         BNE   IOHX                                                             
         CLC   =C'P+',SORTEMP                                                   
         BE    *+12                                                             
         BRAS  RE,SOTINFO          SET TAXABLE STATE INFO                       
         BNE   IOHX                                                             
                                                                                
         TM    TUSTAT,TUSORTNG     IS SORT ACTIVE YET                           
         BO    IOH8                                                             
         LA    R1,SORTLNQ          SET L'SORT RECORD IN RECCARD                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD  INITIALIZE                         
         OI    TUSTAT,TUSORTNG               SORT IS ACTIVE                     
                                                                                
IOH8     CLC   =C'P+',SORTEMP      P+ NEEDS MORE DATA                           
         BNE   IOH9                                                             
         L     R4,TIAREC           R4=A(RECORD)                                 
         BRAS  RE,GETTASK                                                       
                                                                                
         MVI   ELCODE,TATUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   IOH9                                                             
         BAS   RE,PROCPP           PAYROLL PLUS?                                
         B     IOHX                                                             
                                                                                
IOH9     GOTO1 SORTER,DMCB,=C'PUT',(R2)     WRITE OUT SORT RECORD               
         SPACE 1                                                                
IOHX     MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*        PAYROLL PLUS                                                           
*                                                                               
PROCPP   NTR1                                                                   
         XC    SORTFEDT(SORTMEDT-SORTFEDT),SORTFEDT                             
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TATUELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PPP10    BAS   RE,NEXTEL                                                        
         BNE   PROCPPX                                                          
*                                                                               
         USING TATUD,R4                                                         
         CLC   =C'FD',TATUUNIT     SKIP FEDERAL UNIT                            
         BE    PPP10                                                            
         CLI   TATUUNIT+2,C' '                                                  
         BNE   PPP10               SKIP CITIES                                  
*                                                                               
         LA    R2,TUSRTREC         R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
         MVC   SORTSOT,TATUUNIT                                                 
         ICM   RF,15,TATUWAAD                                                   
         ICM   RE,15,TATUTNAD                                                   
         AR    RF,RE                                                            
*                                                                               
PPP20    STCM  RF,15,SORTEARN                                                   
*                                                                               
         MVC   TIUNIT,TATUUNIT                                                  
         BRAS  RE,STLOCWTH         GET STATE & LOCAL WITHHOLDINGS               
         MVI   ELCODE,TATUELQ      RESTORE ELCODE                               
                                                                                
*        BRAS  RE,SOTINFO                                                       
*        BNE   PPP10                                                            
*                                                                               
*        GOTO1 SORTER,DMCB,=C'PUT',(R2)     WRITE OUT SORT RECORD               
         B     PPP10                                                            
*                                                                               
PROCPPX  B     XIT                                                              
*                                                                               
SAVESRT  DS    CL(SORTLNQ)         SORT RECORD AREA                             
*                                                                               
*            CHECK FOR PRINT PAYROLL AND SET SOME PRINT INFO                    
         SPACE 1                                                                
SETPRINT NTR1                                                                   
         MVI   TUPRINT,C'N'        INIT NOT PRINT PAYROLL                       
         XC    TUSHOOT,TUSHOOT     CLEAR SHOOT DATE                             
         SPACE                                                                  
         USING TACOD,R4            R4=A(COMM'L DETAILS EL.)                     
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   SETP10                                                           
         CLI   TACOMED,TACOMEDP    CHECK IF MEDIA IS PRINT                      
         BNE   XIT                                                              
         B     SETP15                                                           
         SPACE                                                                  
SETP10   L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         TM    TAPDADJS,TAPDADTR+TAPDADTT+TAPDADST IF TAX ADJUSTMENT            
         BZ    XIT                                                              
         CLC   TAPDEMP,=C'PP '     EMPLOYER MUST BE PP FOR PRINT                
         BNE   XIT                                                              
         SPACE                                                                  
SETP15   MVI   TUPRINT,C'Y'        SET IS PRINT PAYROLL                         
         MVI   ELCODE,TACSELQ      GET COMMERCIAL DETAILS ELEMENT               
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPS))                                     
         BNE   XIT                                                              
         USING TACSD,R4            R4=A(COMM'L DETAILS EL.)                     
         L     R4,TGELEM                                                        
         MVC   TUSHOOT,TACSDATE    SAVE SHOOT DATE                              
         B     XIT                                                              
         EJECT                                                                  
*              EXTRACT DATA FROM PAYMENT DETAILS ELEMENT                        
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XPDEL    NTR1                                                                   
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
                                                                                
         TM    TAPDADJS,TAPDADTR+TAPDADTT+TAPDADST IF TAX ADJUSTMENT            
         BNZ   NO                  DON'T WANT THOSE                             
                                                                                
         CLI   TAPDW4TY,TAW4TYIN   WANT INDIVIDUALS AND ESTATES ONLY            
         BE    XPDEL3                                                           
         CLI   TAPDW4TY,TAW4TYES                                                
         BNE   NO                                                               
                                                                                
XPDEL3   MVC   SORTW4TY,TAPDW4TY   W4 TYPE                                      
         MVC   SORTCYCS,TAPDCYCS   CYCLE DATES                                  
         MVC   SORTCYCE,TAPDCYCE                                                
         MVC   SORTEMP,TAPDEMP     EMPLOYER                                     
         MVC   SORTSPNH,TAPDSPNH   SUBJ TO P&H                                  
         MVC   SORTMDED,TAPDMDED   MISC DEDUCTIONS                              
         MVC   SVINTCNM,TAPDCOM                                                 
                                                                                
         CLC   =C'P+',SORTEMP      IF EMPLOYER IS P+,                           
         BNE   *+10                                                             
         MVC   SORTFRST,TAPDCYCS   USE CYCLE START DATE AS FIRST SERV.          
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE  LOOK UP USE                        
         SPACE 1                                                                
         MVI   SORTSES,C'N'        ASSUME REUSE                                 
         TM    TGUSSTAT,SESSION    TEST FOR SESSION TYPE                        
         BZ    *+8                                                              
         MVI   SORTSES,C'Y'                                                     
                                                                                
         TM    TUOPTS,TUBENFIT     BENEFITS REPORTING?                          
         BZ    XPDEL4              NO, TAKE REUSE TOO                           
         TM    TGUSSTAT,SESSION    ONLY WANT SESSIONS                           
         BZ    NO                                                               
                                                                                
XPDEL4   LA    R1,EMPCORP                                                       
XPDEL5   CLI   0(R1),X'FF'         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TAPDEMP,0(R1)       MATCH ON EMPLOYER                            
         BE    *+12                                                             
         LA    R1,L'EMPCORP(R1)    BUMP TO NEXT ENTRY AND KEEP LOOKING          
         B     XPDEL5                                                           
         SPACE 1                                                                
         MVC   SORTCRP,3(R1)       SET CORP CODE                                
         MVC   SORTW4TY,TAPDW4TY   W4 TYPE                                      
         B     YES                                                              
         EJECT                                                                  
*              EXTRACT DATA FROM SOAP DETAILS ELEMENT IF SOAP PAYMENT           
*              R4=A(CHECK RECORD)                                               
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
XSOEL    NTR1                                                                   
         TM    TGUSSTA3,SOAPUSE    ONLY IF SOAP USE TYPE                        
         BZ    XIT                                                              
         MVC   SORTFRST,SORTCKDT   INIT SORTFRST WITH CHECK DATE                
         MVI   ELCODE,TASOELQ      GET SOAP DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TASOD,R4            R4=A(ELEMENT)                                
         CLI   TASONUM,0           MUST HAVE AT LEAST 1 SUB-EL                  
         BE    XIT                                                              
         LH    R1,TASOEPI          GET FIRST EPISODE NUM                        
         CVD   R1,DUB                                                           
         UNPK  TGEPI,DUB+5(3)      SET TGEPI FOR RECVAL                         
         OI    TGEPI+4,X'F0'                                                    
         MVC   AIO,AIO1            SET AIO SO WON'T CREAM SYSIO REC             
         GOTO1 RECVAL,DMCB,TLEPCDQ,(X'20',0)  GET EPISODE RECORD                
         MVC   AIO,TIAREC          RESET AIO TO TIAREC                          
         BNE   XIT                                                              
         USING TAEID,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAEIELQ      GET EPISODE INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   SORTFRST,TAEIWDT    SAVE WORKDATE                                
         B     XIT                                                              
         EJECT                                                                  
*              EXTRACT DATA FROM CHECK DETAILS ELEMENT                          
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCDEL    NTR1                                                                   
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4            R4=A(CHECK DETAILS EL.)                      
         SPACE 1                                                                
         OC    TACDEARN,TACDEARN   REJECT IF NO EARNINGS                        
         BZ    NO                                                               
         MVC   SORTCHK,TACDCHK     CHECK NUMBER                                 
         MVC   SORTCKDT,TACDDTE    CHECK DATE                                   
         MVC   SORTRUND,TACDRUN    CHECK RUN DATE                               
                                                                                
         MVC   SORTEARN,TACDEARN   GROSS EARNINGS                               
         MVC   SORTNET,TACDNET                                                  
         B     YES                                                              
         EJECT                                                                  
*              EXTRACT DATA FROM CHECK YTD DETAILS ELEMENT                      
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCYEL    NTR1                                                                   
         MVI   ELCODE,TACYELQ      GET CHECK YTD DETAILS ELEMENT                
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TACYD,R4            R4=A(CHECK DETAILS EL.)                      
                                                                                
         MVC   SORTYTDG,TACYEARN                                                
         B     YES                                                              
         EJECT                                                                  
*              EXTRACT DATA FROM CHECK LIEN DETAILS ELEMENT                     
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XLWEL    NTR1                                                                   
         XC    SORTLIEN,SORTLIEN                                                
                                                                                
         USING TALWD,R4            R4=A(CHECK DETAILS EL.)                      
         MVI   ELCODE,TALWELQ      GET CHECK LIEN DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
         MVC   SORTLIEN,TALWREC                                                 
         B     YES                                                              
         EJECT                                                                  
*              EXTRACT DATA FROM CAST DETAILS ELEMENT                           
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCAEL    NTR1                                                                   
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
                                                                                
         MVC   SORTUN,TACAUN       UNION                                        
         MVC   SORTCAUT,TACAUNIT   TAX UNIT                                     
         TM    TGUSSTA3,SOAPUSE    IF NOT SOAP USE TYPE                         
         BO    YES                                                              
         CLI   TUPRINT,C'Y'        AND NOT PRINT PAYROLL                        
         BE    YES                                                              
         TM    TACASTA3,TACASPPL   AND NOT P+                                   
         BO    YES                                                              
         MVC   SORTFRST,TACAFRST   SET FIRST SERVICES DATE                      
         B     YES                                                              
         SPACE 3                                                                
*              GET RECORDS FROM SORT / WRITE TAPE RECORDS                       
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         ZAP   TUCNT,=P'0'         CLEAR RECORD COUNTS                          
         ZAP   TUALLCNT,=P'0'                                                   
         MVI   TGEMP,X'FF'         SET FIRST TIME FOR EMPLOYER                  
         MVI   TGAGY,X'FF'                            AGENCY                    
         MVI   TGTAGATE,X'FF'                         STATE                     
         MVI   TGSSN,X'FF'                            S/S NUMBER                
         SPACE 1                                                                
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRNTD,R3                                                         
         LA    R2,TUSRTREC         R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
GETS2    GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)                                                      
         BNZ   GETS2A              END OF SORT                                  
         TM    TUSTAT,TUSKIPW4                                                  
         BO    GETSX                                                            
         LA    R2,TTSRTREC         OUTPUT THE LAST ONE                          
         TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
*        BO    GETS200                                                          
         BO    GETSX                                                            
         BRAS  RE,RC220                                                         
         B     GETSX               END OF SORT                                  
                                                                                
GETS200  BRAS  RE,PAYDET                                                        
         B     GETSX                                                            
                                                                                
GETS2A   MVC   TUSRTREC,0(RF)      MOVE SORT RECORD TO LOCAL AREA               
         CLI   TGEMP,X'FF'         FIRST TIME IN?                               
         BNE   GETS2C              NO, DON'T COPY                               
         MVC   TTSRTREC,0(RF)      MOVE SORT RECORD FOR TOTALLING               
         BAS   RE,NEWCRP                                                        
                                                                                
GETS2C   TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
         BO    GETS2M                                                           
         TM    TUSTAT,TUSKIPW4                                                  
         BZ    GETS2H                                                           
         CLC   TTSRTREC(SORTCKDT-SORTD),TUSRTREC  SAME SIN?                     
         BNE   GETS2H                                                           
         MVC   TTSRTREC,TUSRTREC                                                
         B     GETS2                                                            
                                                                                
GETS2H   CLC   TTSRTREC(SORTCHK-SORTD),TUSRTREC   SAME SIN & CHK DATE           
         BNE   GETS2I                                                           
         CLC   =C'P+',TUSRTREC+SORTEMP-SORTD                                    
         BNE   GETS2Q                             SAME TAXABLE STATE?           
         CLC   TTSRTREC+SORTSOT-SORTD(3),TUSRTREC+SORTSOT-SORTD                 
         BE    GETS2Q                                                           
GETS2I   TM    TUSTAT,TUSKIPW4                                                  
         BO    GETS2M                                                           
         LA    R2,TTSRTREC                                                      
GETS2K   TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
         BO    GETS2M                                                           
         BRAS  RE,RC220                                                         
                                                                                
GETS2M   MVC   TTSRTREC,TUSRTREC                                                
         LA    R2,TUSRTREC         R2=A(SORT RECORD)                            
                                                                                
GETS2Q   CLC   TGEMP,SORTEMP       IF EMPLOYER CORP CODE CHANGED                
         BE    GETS3                                                            
         BAS   RE,NEWCRP           PROCESS NEW EMPLOYER CORP CODE               
                                                                                
GETS3    DS    0H                                                               
         CLI   TGAGY,X'FF'         UNLESS FIRST TIME IN                         
         BE    GETS4                                                            
         BRAS  RE,GETSTAT7         GET AGENY'S STATUS 7                         
                                                                                
GETS4    CLC   TGSSN,SORTSSN       TEST S/S NUMBER CHANGED                      
         BNE   GETS6                                                            
         TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
         BZ    GETS4B                                                           
         CLC   SVAGY,SORTAGY       DIFF AGY, NEED ANOTHER RECORD                
         BNE   GETS8                                                            
GETS4B   TM    TUSTAT,TUSKIPW4                                                  
         BO    GETS2               SKIPPING W4 IN TGSSN?                        
         B     GETS9                                                            
                                                                                
GETS6    MVI   SVSSNULS,0                                                       
GETS8    SR    RE,RE               BUMP UP UNIQUE LINE SEQ NUM                  
         IC    RE,SVSSNULS                                                      
         AHI   RE,1                                                             
         STC   RE,SVSSNULS                                                      
                                                                                
         NI    TUSTAT,X'FF'-TUSKIPW4                                            
         BAS   RE,NEWSSN                                                        
         BE    GETS9                                                            
         TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
         BO    GETS9                                                            
         OI    TUSTAT,TUSKIPW4                                                  
         B     GETS2                                                            
                                                                                
GETS9    TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
         BZ    GETS9X                                                           
         BRAS  RE,PAYDET                                                        
GETS9X   BAS   RE,DISPLAY          DISPLAY IT                                   
         TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
         BO    GETS2                                                            
         BAS   RE,ADDUP            ADD TO HIGH LEVEL TOTALS                     
         B     GETS2               GET NEXT RECORD FROM SORT                    
                                                                                
GETSX    BAS   RE,CRPTOTAL                           CORP TOTAL                 
         BAS   RE,TOTAL                   PRINT OVERALL TOTALS                  
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
         XI    TUSTAT,TUSORTNG     SORT NO LONGER ACTIVE                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS NEW EMPLOYER CORP CODE                        
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
NEWCRP   NTR1                                                                   
         MVC   IRSACCT,SPACES                                                   
         MVC   AIO,AIO1            SET AIO SO WON'T CREAM SYSIO'S REC           
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A0',SORTEMP)  READ EMPLOYER REC           
         BNE   NEWCRP50                                                         
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,(4,=CL4'UFD ')                                         
         BNE   NEWCRP50                                                         
         USING TATID,R4                                                         
         L     R4,TGELEM                                                        
*                                                                               
         TM    TUOPTS,TUBENFIT     SET FOR BENEFITS REPORTING?                  
         BO    NEWCRP40                                                         
                                                                                
         LA    R1,L'TATIID                MAKE IT RIGHT JUSTIFIED               
         LA    RE,IRSACCT+L'IRSACCT-1                                           
         LA    RF,TATIID+L'TATIID-1                                             
NEWCRP30 CLI   0(RF),C' '                                                       
         BNH   NEWCRP35                                                         
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,-1                                                            
NEWCRP35 AHI   RF,-1                                                            
         BCT   R1,NEWCRP30                                                      
         B     NEWCRP50                                                         
                                                                                
NEWCRP40 MVC   IRSACCT,TATIID      LEFT JUSTIFIED FOR BENEFITS REPORT           
                                                                                
NEWCRP50 MVC   AIO,TIAREC          RESET AIO TO TIAREC                          
                                                                                
         MVC   TAPECRP,SORTCRP     EMPLOYER CORP CODE TO TAPE                   
         MVC   TGEMP,SORTCRP                          TO GLOBALS                
                                                                                
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PROCESS NEW STATE                                     
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
NEWSTA   NTR1                                                                   
         MVC   TAPEGATE,SORTGATE   GATES/MCDONALD STATE NO. TO TAPE             
         SPACE 1                                                                
         GOTO1 TAXVAL,DMCB,(3,SORTSOT)  LOOK UP IN TABLE - SET GLOBALS          
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PROCESS NEW S/S NUMBER                                
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING PRNTD,R3            R3=A(PRINT AREA)                             
NEWSSN   NTR1                                                                   
         MVC   AIO,AIO1            SET AIO SO WON'T CREAM SYSIO'S REC           
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SORTSSN)  READ/SET GLOBAL             
         MVC   AIO,TIAREC          RESET AIO TO TIAREC                          
         JNE   NO                                                               
                                                                                
NEWSSN5  BRAS  RE,SKIPCANC         SKIP CANADIAN W4 COUNTRY                     
         JNE   NO                                                               
         GOTO1 MYTRACE,DMCB,=C'W4'                                              
                                                                                
         TM    TUOPTS,TUBENFIT     BENEFITS HAVE DIFF OUTPUT                    
         BO    NEWSSN10                                                         
         BRAS  RE,RC202                                                         
         BRAS  RE,RC210                                                         
                                                                                
         BRAS  RE,CLRREC                                                        
         MVC   PRNTSSN,SORTSSN     SET TO PRINT S/S NUMBER                      
         MVC   TAPESSN,SORTSSN     ALSO MOVE TO TAPE AREA                       
                                                                                
         MVC   PRNTLAST,SPACES     PRE-FILL WITH NOT-FOUND LITERAL              
         MVC   PRNTLAST(L'LTNOW4),LTNOW4                                        
                                                                                
         MVC   PRNTLAST,W4NAM2   MOVE LAST NAME                                 
         MVC   PRNTFRST,W4NAM1   AND FIRST NAME TO PRINT LINE                   
         MVC   PRNTMID,W4MIDN    AND MIDDLE INITIAL                             
                                                                                
         J     YES                                                              
*----------------------------------------------------------------------         
NEWSSN10 BRAS  RE,EMPINF                                                        
                                                                                
         MVC   PRNTSSN,SORTSSN     SET TO PRINT S/S NUMBER                      
                                                                                
         MVC   PRNTLAST,SPACES     PRE-FILL WITH NOT-FOUND LITERAL              
         MVC   PRNTLAST(L'LTNOW4),LTNOW4                                        
                                                                                
         MVC   PRNTLAST,W4NAM2   MOVE LAST NAME                                 
         MVC   PRNTFRST,W4NAM1   AND FIRST NAME TO PRINT LINE                   
         MVC   PRNTMID,W4MIDN    AND MIDDLE INITIAL                             
                                                                                
         J     YES                                                              
         EJECT                                                                  
*              PRINT SORT RECORD DETAILS                                        
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING PRNTD,R3            R3=A(PRINT AREA)                             
DISPLAY  NTR1                                                                   
         MVC   PRNTCHK,SORTCHK                     CHECK NUMBER                 
         MVC   PRNTW4TY,SORTW4TY                   W4 TYPE                      
         GOTO1 DATCON,DMCB,(1,SORTCKDT),(8,PRNTCHKD)   CHECK DATE               
         MVC   PRNTUNIT,SORTSOT                    TAX UNIT                     
         MVC   PRNTTYPE,=C'SES'                    PAYMENT TYPE                 
         CLI   SORTSES,C'N'                                                     
         BNE   *+10                                                             
         MVC   PRNTTYPE,=C'RES'                    PAYMENT TYPE                 
         SPACE 1                                                                
         CLI   SORTSES,C'Y'                  IF THIS IS A SESSION               
         BNE   DISP2                                                            
         SPACE 1                                                                
DISP2    EDIT  SORTEARN,(12,PRNTEARN),2,MINUS=YES  GROSS EARNINGS               
         SPACE 1                                                                
         BAS   RE,PRNTIT           PRINT THE LINE                               
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO ADD ACCUMS TO HIGHER LEVELS                           
         SPACE 1                                                                
         USING SORTD,R2            R2=A(SORT RECORD)                            
ADDUP    NTR1                                                                   
         CLC   TTSRTREC(SORTGATE-SORTD),TUSRTREC     SAME SS#, CHK DT,          
         JE    ADDUP500                              AND CHECK NUMBER?          
         LA    R3,13               13 AMOUNTS                                   
         LA    RF,TTSRTREC+(SORTEARN-SORTD)                                     
         LA    RE,SORTEARN                                                      
ADDUP100 ICM   R1,15,0(RF)                                                      
         ICM   R0,15,0(RE)                                                      
         AR    R1,R0                                                            
         STCM  R1,15,0(RF)                                                      
         AHI   RF,4                                                             
         AHI   RE,4                                                             
         BCT   R3,ADDUP100                                                      
                                                                                
ADDUP500 ICM   R1,15,SORTEARN                                                   
                                                                                
         LA    R0,3                NUMBER OF TOTALS TO ACCUMULATE               
         LA    RE,TUTOTALS         START OF TOTALS                              
                                                                                
ADDUP550 L     RF,0(RE)                                                         
         AR    RF,R1               ADD SORTEARN TO EACH TOTAL                   
         ST    RF,0(RE)                                                         
         LA    RE,L'TUTOTALS(RE)                                                
         BCT   R0,ADDUP550                                                      
                                                                                
         AP    TUCNT,=P'1'         ADD 1 TO COUNT                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT STATE TOTALS                                    
         SPACE 1                                                                
         USING PRNTD,R3            R3=A(PRINT AREA)                             
STATOTAL NTR1                                                                   
         BAS   RE,PRNTIT                           SKIP A LINE                  
         MVC   PRNTLAST(L'LTSTATOT),LTSTATOT       STATE TOTALS LITERAL         
         EDIT  TUSTATOT,(12,PRNTEARN),2,MINUS=YES  TOTAL EARNINGS               
         BAS   RE,PRNTIT                           PRINT THE LINE               
         XC    TUSTATOT,TUSTATOT                   CLEAR TOTAL                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO EMPLOYER CORP. TOTALS                                 
         SPACE 1                                                                
CRPTOTAL NTR1                                                                   
         BAS   RE,PRNTIT                           SKIP A LINE                  
         MVC   PRNTLAST(L'LTCRPTOT),LTCRPTOT       EMP CORP TOTALS              
         EDIT  TUCNT,(8,PRNTCHK),COMMAS=YES        PRINT N'RECORDS              
         EDIT  TUCRPTOT,(12,PRNTEARN),2,MINUS=YES  TOTAL EARNINGS               
         BAS   RE,PRNTIT                           PRINT THE LINE               
*                                                                               
         AP    TUALLCNT,TUCNT                      ADD TO OVERALL COUNT         
         ZAP   TUCNT,=P'0'                         CLEAR COUNT                  
         XC    TUCRPTOT,TUCRPTOT                   & TOTAL                      
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PRINT OVERALL TOTALS                                  
         SPACE 1                                                                
TOTAL    NTR1                                                                   
         BAS   RE,PRNTIT                           SKIP A LINE                  
         MVC   PRNTLAST(L'LTALLTOT),LTALLTOT       OVERALL TOTALS LIT           
         EDIT  TUALLCNT,(8,PRNTCHK),COMMAS=YES     PRINT N'RECORDS              
         EDIT  TUALLTOT,(12,PRNTEARN),2,MINUS=YES  TOTAL EARNINGS               
         BAS   RE,PRNTIT                           PRINT THE LINE               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         TM    TUOPTS,TUTRACE      TEST TRACE ENABLED                           
         BZ    XIT                                                              
         L     R2,0(R1)            A(LITERAL)                                   
         ZIC   R3,0(R1)            L'LITERAL                                    
         GOTO1 TRACE,DMCB,AIO,0,(R2),(R3)                                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
*        MVC   H4+8(3),TAPECRP     CORPORATION CODE                             
*        MVC   H5+8(2),TGTAGATE    STATE NUMBER                                 
*        MVC   H5+11(8),TGTANAME   STATE NAME                                   
         MVC   H4+57(17),TUPERIOD  REQUESTED PERIOD                             
                                                                                
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+5,C'T'      TOP                                          
         MVI   BOXROWS+7,C'M'      MIDDLE                                       
         MVI   BOXROWS+60,C'B'     BOTTOM                                       
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    R3,BOXCOLS                                                       
         USING PRNTD,R3            USE PRINT LINE DSECT                         
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
         MVI   BR,C'R'                                                          
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
         SPACE 1                                                                
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,28,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=000'                                   
         SPACE 1                                                                
UITAPE   DCB   DDNAME=UITAPE,DSORG=PS,RECFM=VB,MACRF=PM,               X        
               LRECL=4940,BUFNO=2,BLKSIZE=32760                                 
EITAPE   DCB   DDNAME=EITAPE,DSORG=PS,RECFM=VB,MACRF=PM,               X        
               LRECL=4940,BUFNO=2,BLKSIZE=32760                                 
PDTAPE   DCB   DDNAME=PDTAPE,DSORG=PS,RECFM=VB,MACRF=PM,               X        
               LRECL=4940,BUFNO=2,BLKSIZE=32760                                 
         SPACE 1                                                                
LTNOW4   DC    C'* Not Found *'                                                 
LTSTATOT DC    C'State Totals'                                                  
LTCRPTOT DC    C'Corp. Totals'                                                  
LTALLTOT DC    C'Overall Totals'                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EMPCORP  DS    0CL6                TABLE OF EMPLOYERS AND CORP CODE             
         DC    C'TP ',C'LX4'                                                    
         DC    C'PP ',C'LX5'                                                    
*        DC    C'PG ',C'CVP'                                                    
         DC    C'P+ ',C'LX6'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H4,100,AGYNAME                                                   
         SSPEC H5,100,AGYADD                                                    
*                                                                               
         SSPEC H1,46,C'Unemployment Interface for Equifax'                      
         SSPEC H2,46,42X'BF'                                                    
*                                                                               
         SSPEC H7,02,C'S/S Numb  Last Name        First Name'                   
         SSPEC H7,46,C'MI T Chk Date  Check#  Type Unt       Gross'             
         DC    X'00'                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EF000 RECORD (HEADER)                                            
*---------------------------------------------------------------------          
RC000    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRREC                                                        
                                                                                
         USING EFRECD,R5                                                        
         LAY   R5,TAPEREC                                                       
         MVC   EFRTYP,RC000TYP                                                  
         MVC   EF000VER,=C'02.00'         VERSION                               
         MVC   EF000ID,=C'17300'          SOURCE ID                             
         MVC   EF000TYP,=CL20'EMPLOYER'   SOURCE TYPE                           
         MVI   EF000CDE,C'I'              CODE = INITIAL                        
                                                                                
         LAY   R5,UITAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                                                
         J     XIT                                                              
*                                                                               
RC000TYP DC    CL15'000HEADER'                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EF202 RECORD (EMPLOYEE GENERAL INFORMATION)                      
*---------------------------------------------------------------------          
RC202    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRREC                                                        
                                                                                
         BRAS  RE,MRCNTCK              MOST RECENT CHECK - NEW HIRE?            
                                                                                
         USING EFRECD,R5                                                        
         LAY   R5,TAPEREC                                                       
         MVC   EFRTYP,RC202TYP                                                  
         MVC   EF202ECC,=C'17300'                                               
         MVC   EF202SSN,SORTSSN                                                 
         GOTO1 SSNPACK,DMCB,SORTSSN,EF202EID                                    
         MVC   EF202UID(9),SORTSSN                                              
         MVC   EF202UCX,=C'TG97'                                                
         GOTO1 DATCON,DMCB,(1,SORTCKDT),(20,EF202PDT)                           
                                                                                
         BRAS  RE,GW4NAME                                                       
                                                                                
*&&DO                                                                           
         USING TAW4D,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   TAW4TYPE,TAW4TYCO   NOW CORPORATION                              
         BE    RC202A10                                                         
         CLI   TAW4TYPE,TAW4TYES   OR ESTATE?                                   
         BNE   RC202A20                                                         
                                                                                
RC202A10 BRAS  RE,PESTCRP          PARSE OUT NAME                               
         J     RC202A30                                                         
                                                                                
RC202A20 MVC   W4NAM2,TAW4NAM2                                                  
         MVC   W4NAM1,TAW4NAM1                                                  
         MVC   W4MIDN,SPACES                                                    
         CLI   TAW4LEN,TAW4LN2Q    NEW ELEMENT?                                 
         JNE   RC202A30                                                         
         MVC   W4MIDN,TAW4MIDN                                                  
         OC    W4MIDN,SPACES                                                    
*&&                                                                             
RC202A30 MVC   EF202FN(L'TAW4NAM1),W4NAM1                                       
         MVC   EF202MN(L'TAW4MIDN),W4MIDN                                       
         MVC   EF202LN(L'TAW4NAM2),W4NAM2                                       
         MVC   EF202PIN(4),SORTSSN+5                                            
         MVI   EF202LGN,C'Y'                                                    
         MVC   EF202DIV(3),SORTCRP                                              
         MVC   EF202JOB(L'SORTJOB),SORTJOB                                      
         MVI   EF202ESC,C'A'                                                    
         CLI   SORTW4TY,TAW4TYES                                                
         JNE   RC202A35                                                         
         MVI   EF202ESC,C'Q'                                                    
         BRAS  RE,FINDDOD             FIND MOST RECENT NON-ADJ CHECK            
         GOTO1 DATCON,DMCB,(1,SVDOD),(20,EF202TRM)       TERM DATE              
                                                                                
RC202A35 GOTO1 DATCON,DMCB,(1,SVMRCTHD),(20,EF202MHD)    HIRE DATE              
         GOTO1 DATCON,DMCB,(1,SVORGNHD),(20,EF202OHD)    ORIG HIRE DATE         
                                                                                
         MVC   EF202FRQ,=C'01'                  TP OR PP, ANNUAL                
         CLC   SORTEMP,=C'P+ '                  P+                              
         JNE   RC202A40                                                         
         MVC   EF202FRQ,=C'05'                  SEMI-MONTHLY                    
         TM    AGYSTAT7,TAAYSTSM                IS IT?                          
         BO    RC202A40                                                         
         MVC   EF202FRQ,=C'07'                  NO, WEEKLY THEN                 
                                                                                
RC202A40 MVI   EF202AI1,C'U'                    UPDATE                          
         MVC   EF202AT1(4),=C'home'             HOME ADDRESS                    
*                                                                               
         BRAS  RE,GW4ADDDB                                                      
                                                                                
         MVC   EF202AD1(L'TAA2ADD1),W4ADDR1                                     
         OC    EF202AD1,SPACES                                                  
         MVC   EF202AD2(L'TAA2ADD2),W4ADDR2                                     
         OC    EF202AD2,SPACES                                                  
         MVC   EF202AD3(L'TAA2ADD3),W4ADDR3                                     
         OC    EF202AD3,SPACES                                                  
         MVC   EF202CIT(L'TAA2CITY),W4CITY                                      
                                                                                
         MVC   EF202ST,W4STAT                                                   
         CLC   EF202ST,SPACES                                                   
         JH    *+10                                                             
         MVC   EF202ST,=C'OT '                                                  
                                                                                
         MVC   EF202ZIP(L'TAA2ZIP),W4ZIP                                        
         MVC   EF202CNT(L'TAA2CTRY),W4CTRY                                      
                                                                                
         OC    W4DOB,W4DOB                                                      
         JZ    RC202A80                                                         
         GOTO1 DATCON,DMCB,(1,W4DOB),(20,EF202DOB)                              
                                                                                
RC202A80 LAY   R5,UITAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                                                
         J     XIT                                                              
*                                                                               
         SPACE 3                                                                
RC202TYP DC    CL15'202EMPLPIM'                                                 
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              MOST RECENT CHECK                                                
*---------------------------------------------------------------------          
MRCNTCK  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVMRCTHD,SORTCKDT       FIRST CHECK IN REQUEST PERIOD            
         MVC   SVORGNHD,SORTCKDT                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R3                                                        
         MVC   AIO,AIO2                GET YTD GROSS FOR SSN                    
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SORTSSN                                                 
         MVI   TLCKECUR,C'U'           US$                                      
         MVC   TLCKEEMP,SORTEMP                                                 
         MVC   TLCKEDTE,SORTCKDT                                                
         XC    TLCKEDTE,=XL3'FFFFFF'   COMPLEMENT                               
         GOTO1 HIGH                                                             
                                                                                
MRCK100  MVC   WORK+8(3),TLCKEDTE                                               
         XC    WORK+8(3),=XL3'FFFFFF'   COMPLEMENT                              
                                                                                
         GOTO1 DATCON,DUB,(1,WORK+8),(0,WORK)                                   
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+8,-90      90 DAYS AGO               
         GOTO1 DATCON,DUB,(0,WORK+8),(1,PRI90DYS)     SAVE IN PACKED            
                                                                                
MRCK200  GOTO1 SEQ                                                              
         CLC   KEY(TLCKEDTE-TLCKPD),KEYSAVE     SAME SSN - EMP?                 
         JNE   MRCK900                                                          
         MVC   WORK(3),TLCKEDTE                                                 
         XC    WORK(3),=XL3'FFFFFF'   COMPLEMENT                                
         CLC   WORK(3),PRI90DYS                                                 
         JNH   MRCK900                                                          
                                                                                
         CLC   TLCKEAGY,=C'999999'    ADJUSTMENTS?                              
         BNE   MRCK300                NO, DON'T CHECK FOR TAX ADJ               
                                                                                
         GOTO1 GETREC                 YES, CHECK TAX ADJ                        
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         TM    TAPDADJS,TAPDADTR+TAPDADTT+TAPDADST   IF TAX ADJUSTMENT          
         JNZ   MRCK200                               SKIP CHECK                 
                                                                                
MRCK300  MVC   SVMRCTHD,WORK                                                    
         MVC   SVORGNHD,WORK                                                    
         J     MRCK100                                                          
                                                                                
MRCK900  MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              SUBSTITUTE COUNTRY CODE WITH ISO STANDARD                        
*---------------------------------------------------------------------          
ISOCTRY  NTR1  BASE=*,LABEL=*                                                   
         USING CTRYTABD,R4                                                      
         MVC   W4CTRYN,SPACES                                                   
         CLC   W4CTRY,SPACES                                                    
         JE    XIT                                                              
                                                                                
         L     R4,TGACTRYS                                                      
ISOCTY10 CLI   0(R4),X'FF'         IF CAN'T FIND IT, USE WHAT'S IN W4           
         JE    XIT                                                              
         CLC   CTRYCODE,W4CTRY     FIND COUNTRY CODE IN COUNTRY TABLE           
         JE    ISOCTY30                                                         
         ZIC   R0,CTRYLEN                                                       
         AR    R4,R0                                                            
         J     ISOCTY10                                                         
                                                                                
ISOCTY30 MVC   W4CTRYN,CTRYISO                                                  
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              FIND DATE OF DEATH FOR ESTATE W4                                 
*---------------------------------------------------------------------          
FINDDOD  NTR1  BASE=*,LABEL=*                                                   
         XC    SVDOD,SVDOD                                                      
         MVC   DODDATE,SPACES                                                   
                                                                                
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPG))                                     
         JNE   FDOD60                                                           
                                                                                
         USING TACMD,R4                                                         
         L     R4,TGELEM                                                        
         LA    R3,TACMCOMM                                                      
         ZIC   R1,TACMLEN                                                       
         AHI   R1,-3                   LENGTH OF COMMENT                        
                                                                                
FDOD10   CLC   0(3,R3),=C'DOD'         FIRST FIND "DOD"                         
         JE    FDOD20                                                           
         CLC   0(13,R3),=C'DATE OF DEATH'                                       
         JE    FDOD23                                                           
         AHI   R3,1                                                             
         BCT   R1,FDOD10                                                        
         J     FDOD60                  CAN'T FIND DOD, OTHER METHOD             
                                                                                
FDOD20   AHI   R3,3                    POINT TO CHAR AFTER DOD                  
         AHI   R1,-3                                                            
         J     FDOD25                                                           
FDOD23   AHI   R3,13                                                            
         AHI   R1,-13                                                           
FDOD25   CLI   0(R3),C'?'              UNKNOWN?                                 
         JE    FDOD60                                                           
         CLI   0(R3),C'0'              START NUMBER?                            
         JNL   FDOD30                                                           
         AHI   R3,1                                                             
         BCT   R1,FDOD25                                                        
         J     FDOD60                                                           
                                                                                
FDOD30   LA    R4,DODDATE                                                       
FDOD35   CLI   0(R3),C'0'              ONLY COPY NUMBERS                        
         JNL   FDOD38                                                           
         CLI   0(R3),C'/'              AND DATE CHARS                           
         JE    FDOD38                                                           
         J     FDOD40                  NO MORE, VALIDATE DATE                   
                                                                                
FDOD38   MVC   0(1,R4),0(R3)                                                    
         AHI   R3,1                                                             
         AHI   R4,1                                                             
         BCT   R1,FDOD35                                                        
                                                                                
FDOD40   LA    R3,DODDATE                                                       
         SR    R4,R3                   LENGTH OF DATE                           
         STC   R4,DODDATEH+5                                                    
                                                                                
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         LA    R2,DODDATEH                                                      
         MVC   BYTE,5(R2)          SET L'INPUT FIELD                            
         GOTO1 PERVAL,DMCB,(BYTE,8(R2)),('PVINSGLS',(R3))                       
         CLI   4(R1),PVRCONE       ONLY ONE DATE INPUT                          
         BE    FDOD49              YES, OK                                      
         CLI   4(R1),PVRCOK        ANY ERRORS?                                  
         BNE   FDOD60              YES, USE OTHER METHOD                        
FDOD49   MVC   SVDOD,PVALPSTA                                                   
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
*-----------                                                                    
FDOD60   MVC   SYSDIR,=CL8'CHKDIR'     FIND LAST NON-ADJ CHECK                  
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   AIO,AIO2                                                         
         MVC   SVDOD,SORTCKDT          USE FIRST CHECK DATE AS DOD              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SORTSSN                                                 
         MVI   TLCKECUR,C'U'           US$                                      
         MVC   TLCKEEMP,SORTEMP                                                 
         MVC   TLCKEDTE,SORTCKDT                                                
         XC    TLCKEDTE,=X'FFFFFFFFFFFF'                                        
                                                                                
         GOTO1 HIGH                                                             
                                                                                
FDOD70   GOTO1 SEQ                                                              
FDOD80   CLC   KEY(TLCKEDTE-TLCKPD),KEYSAVE     SAME SSN - EMP?                 
         JNE   FDOD90                                                           
         CLI   TLCKEDTE,0             CHECK NOT WRITTEN YET                     
         JE    FDOD70                                                           
         CLC   TLCKEAGY,=C'999999'    ADJUSTMENTS?                              
         BE    FDOD70                 YES, SKIP IT                              
         MVC   SVDOD,TLCKEDTE                                                   
         XC    SVDOD,=X'FFFFFFFFFFFF' MOST RECENT NON-ADJ CHECK DATE            
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         CLI   TAPDW4TY,C'E'       KEEP LOOKING FOR NON-ESTATE CHECK            
         BE    FDOD70                AND USE THAT AS DOD                        
                                                                                
FDOD90   MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              GET NAME FROM W4 RECORD                                          
*---------------------------------------------------------------------          
         USING TAW4D,R4                                                         
GW4NAME  NTR1  BASE=*,LABEL=*                                                   
         USING TAW4D,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   W4SEX,TAW4SEX                                                    
                                                                                
         CLI   TAW4TYPE,TAW4TYCO   NOW CORPORATION                              
         BE    GW4NM10                                                          
         CLI   TAW4TYPE,TAW4TYES   OR ESTATE?                                   
         BNE   GW4NM20                                                          
                                                                                
GW4NM10  BAS   RE,PESTCRP          PARSE OUT NAME                               
         J     GW4NMXIT                                                         
                                                                                
GW4NM20  MVC   W4NAM2,TAW4NAM2                                                  
         MVC   W4NAM1,TAW4NAM1                                                  
         MVC   W4MIDN,SPACES                                                    
         CLI   TAW4LEN,TAW4LN2Q    NEW ELEMENT?                                 
         JNE   GW4NMXIT                                                         
         MVC   W4MIDN,TAW4MIDN                                                  
         OC    W4MIDN,SPACES                                                    
                                                                                
GW4NMXIT J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PARSE NAME FOR ESTATES AND CORPS                                 
*---------------------------------------------------------------------          
         USING TAW4D,R4                                                         
PESTCRP  NTR1                                                                   
         MVC   W4NAM2,SPACES       PARSE OUT FIRST AND LAST NAMES               
         MVC   W4NAM1,SPACES                                                    
         MVC   W4MIDN,SPACES                                                    
         MVI   BYTE,0              FIRST NAME PASS                              
         LHI   R1,32                                                            
         LA    RF,TAW4NAM2                                                      
         LA    R3,W4NAM1                                                        
PEC300   CLI   0(RF),C' '                                                       
         JE    PEC600                                                           
         CLI   BYTE,1              MID INITIAL PASS?                            
         JNE   PEC500                                                           
         CLI   1(RF),C' '          AND AN INITIAL?                              
         JE    PEC500              YES, CONTINUE                                
PEC400   MVI   BYTE,2              NO, MAKE IT LAST NAME PASS                   
         LA    R3,W4NAM2                                                        
PEC500   MVC   0(1,R3),0(RF)                                                    
         J     PEC800                                                           
                                                                                
PEC600   CLI   BYTE,2              GOT LAST NAME ALREADY?                       
         JE    XIT                 YES, DONE AND LEAVE                          
         CLI   BYTE,1              GOT MID INITIAL ALREADY?                     
         BE    PEC700              YES, LAST NAME PASS                          
         MVI   BYTE,1              NO, SET TO GET MID INITIAL                   
         LA    R3,W4MIDN                                                        
         J     PEC900                                                           
                                                                                
PEC700   MVI   BYTE,2                                                           
         LA    R3,W4NAM2                                                        
         J     PEC900                                                           
                                                                                
PEC800   AHI   R3,1                                                             
PEC900   AHI   RF,1                                                             
         JCT   R1,PEC300                                                        
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              GET ADDR AND DOB FROM W4                                         
*---------------------------------------------------------------------          
         USING TAW4D,R4                                                         
GW4ADDDB NTR1  BASE=*,LABEL=*                                                   
         USING TAA2D,R4                         ADDRESS                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   W4ADDR1,TAA2ADD1                                                 
         MVC   W4ADDR2,TAA2ADD2                                                 
         MVC   W4ADDR3,TAA2ADD3                                                 
         MVC   W4CITY,TAA2CITY                                                  
         MVC   W4STAT,TAA2ST                                                    
         MVC   W4ZIP,TAA2ZIP                                                    
         MVC   W4CTRY,SPACES                                                    
         CLI   TAA2LEN,TAA2LNQ                                                  
         JL    GW4ADB10                                                         
         MVC   W4CTRY,TAA2CTRY                                                  
         BRAS  RE,ISOCTRY                                                       
*                                                                               
         USING TAWXD,R4                                                         
GW4ADB10 XC    W4DOB,W4DOB                                                      
         L     R4,AIO1                                                          
         MVI   ELCODE,TAWXELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         MVC   W4DOB,TAWXDOB                                                    
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EMPLOYEE INFO HEADINGS                                           
*---------------------------------------------------------------------          
EMPINFHD NTR1  BASE=*,LABEL=*                                                   
         LAY   R5,TAPEREC                                                       
         ST    R5,TAPEPTR                                                       
                                                                                
         BRAS  RE,CLRREC               CLEAR TAPE RECORD                        
         BRAS  RE,MRCNTCK              MOST RECENT CHECK - NEW HIRE?            
                                                                                
         GOTOR ENCTEXTF,DMCB,(06,=C'COCODE')                                    
         GOTOR ENCTEXTF,DMCB,(10,=C'DataSource')                                
         GOTOR ENCTEXTF,DMCB,(04,=C'FEIN')                                      
         GOTOR ENCTEXTF,DMCB,(03,=C'SSN')                                       
         GOTOR ENCTEXTF,DMCB,(10,=C'EmployeeID')                                
         GOTOR ENCTEXTF,DMCB,(12,=C'AssignmentID')                              
         GOTOR ENCTEXTF,DMCB,(11,=C'BillingCode')                               
         GOTOR ENCTEXTF,DMCB,(19,=C'IsPrimaryAssignment')                       
         GOTOR ENCTEXTF,DMCB,(13,=C'CurrentStatus')                             
         GOTOR ENCTEXTF,DMCB,(16,=C'OriginalHireDate')                          
         GOTOR ENCTEXTF,DMCB,(18,=C'MostRecentHireDate')                        
         GOTOR ENCTEXTF,DMCB,(19,=C'MostRecentStartDate')                       
         GOTOR ENCTEXTF,DMCB,(15,=C'TerminationDate')                           
         GOTOR ENCTEXTF,DMCB,(09,=C'FirstName')                                 
         GOTOR ENCTEXTF,DMCB,(08,=C'LastName')                                  
         GOTOR ENCTEXTF,DMCB,(10,=C'MiddleName')                                
         GOTOR ENCTEXTF,DMCB,(07,=C'PayRate')                                   
         GOTOR ENCTEXTF,DMCB,(07,=C'PayType')                                   
         GOTOR ENCTEXTF,DMCB,(17,=C'PayCycleFrequency')                         
         GOTOR ENCTEXTF,DMCB,(16,=C'HomeAddressLine1')                          
         GOTOR ENCTEXTF,DMCB,(16,=C'HomeAddressLine2')                          
         GOTOR ENCTEXTF,DMCB,(15,=C'HomeAddressCity')                           
         GOTOR ENCTEXTF,DMCB,(16,=C'HomeAddressState')                          
         GOTOR ENCTEXTF,DMCB,(14,=C'HomeAddressZIP')                            
         GOTOR ENCTEXTF,DMCB,(18,=C'HomeAddressCountry')                        
         GOTOR ENCTEXTF,DMCB,(17,=C'NotificationEmail')                         
         GOTOR ENCTEXTF,DMCB,(11,=C'DateOfBirth')                               
         GOTOR ENCTEXTF,DMCB,(06,=C'Gender')                                    
         GOTOR ENCTEXTF,DMCB,(16,=C'UnionAffiliation')                          
         GOTOR ENCTEXTF,DMCB,(17,=C'EmployeeClassCode')                         
         GOTOR ENCTEXTF,DMCB,(18,=C'EmployeeClassLabel')                        
         GOTOR ENCTEXTF,DMCB,(26,=C'ACAEmployeeDesignationCode')                
         GOTOR ENCTEXTF,DMCB,(23,=C'ACAEmployeeCategoryCode')                   
         GOTOR ENCTEXTF,DMCB,(28,=C'ACAPayTypeClassificationCode')              
         GOTOR ENCTEXTF,DMCB,(08,=C'JobTitle')                                  
         GOTOR ENCTEXTF,DMCB,(12,=C'JobClassCode')                              
         GOTOR ENCTEXTF,DMCB,(13,=C'JobClassLabel')                             
         GOTOR ENCTEXTF,DMCB,(12,=C'WorkLocation')                              
         GOTOR ENCTEXTF,DMCB,(06,=C'Region')                                    
         GOTOR ENCTEXTF,DMCB,(08,=C'Division')                                  
         GOTOR ENCTEXTF,DMCB,(10,=C'Department')                                
         GOTOR ENCTEXTF,DMCB,(16,=C'WorkAddressLine1')                          
         GOTOR ENCTEXTF,DMCB,(16,=C'WorkAddressLine2')                          
         GOTOR ENCTEXTF,DMCB,(15,=C'WorkAddressCity')                           
         GOTOR ENCTEXTF,DMCB,(16,=C'WorkAddressState')                          
         GOTOR ENCTEXTF,DMCB,(14,=C'WorkAddressZIP')                            
         GOTOR ENCTEXTF,DMCB,(18,=C'WorkAddressCountry')                        
         GOTOR ENCTEXTF,DMCB,(14,=C'ACASecurityKey')                            
         GOTOR ENCTEXTF,DMCB,(16,=C'WorkNumberUserID')                          
         GOTOR ENCTEXTF,DMCB,(16,=C'AdjustedHireDate')                          
         GOTOR ENCTEXTF,DMCB,(14,=C'YearsOfService')                            
         GOTOR ENCTEXTF,DMCB,(15,=C'MonthsOfService')                           
         GOTOR ENCTEXTF,DMCB,(18,=C'WorkNumberDivision')                        
         GOTOR ENCTEXTF,DMCB,(20,=C'WorkNumberDefaultPIN')                      
                                                                                
         L     RE,TAPEPTR                                                       
         AHI   RE,-1                                                            
         MVI   0(RE),C' '          GET RID OF LAST COMMA                        
                                                                                
         LAY   R5,EITAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                                                
                                                                                
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EMPLOYEE INFO HEADINGS                                           
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
EMPINF   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LAY   R5,TAPEREC                                                       
         ST    R5,TAPEPTR                                                       
                                                                                
         BRAS  RE,CLRREC               CLEAR TAPE RECORD                        
         BRAS  RE,MRCNTCK              MOST RECENT CHECK - NEW HIRE?            
                                                                                
         GOTOR ENCTEXTF,DMCB,(5,=C'17300')                                      
         GOTOR ENCTEXTF,DMCB,(3,=C'PCT')                                        
         GOTOR ENCTEXTF,DMCB,(L'IRSACCT,IRSACCT)     FEIN                       
         GOTOR ENCTEXTF,DMCB,(L'SORTSSN,SORTSSN)     SS#                        
         GOTO1 SSNPACK,DMCB,SORTSSN,WORK                                        
         GOTOR ENCTEXTF,DMCB,(L'TGPID,WORK)          PID                        
         GOTOR ENCTEXTF,DMCB,(L'SORTAGY,SORTAGY)     AGENCY                     
         MVC   BLOCK(6),SORTAGY                                                 
         MVC   BLOCK+6(6),WORK                                                  
         EDIT  SVSSNULS,(3,BLOCK+12),0,FILL=0                                   
         GOTOR ENCTEXTF,DMCB,(15,BLOCK)              BILLING CODE               
         GOTOR ENCNUMF,DMCB,(1,=C'1')                                           
                                                                                
         GOTOR ENCTEXTF,DMCB,(1,=C'A')     CURRENT STATUS                       
         GOTOR ENCDATEF,DMCB,SVORGNHD      ORIGINAL HIRE DATE                   
         GOTOR ENCDATEF,DMCB,SVMRCTHD      MOST RECENT HIRE DATE                
         GOTOR ENCDATEF,DMCB,SORTCKDT      SESSION DATE                         
         GOTOR ENCDATEF,DMCB,SVDOD         TERMINATION DATE                     
         BRAS  RE,GW4NAME                                                       
         BRAS  RE,GW4ADDDB                                                      
         GOTOR ENCTEXTF,DMCB,(L'W4NAM1,W4NAM1)    FIRST NAME                    
         GOTOR ENCTEXTF,DMCB,(L'W4NAM2,W4NAM2)    LAST NAME                     
         GOTOR ENCTEXTF,DMCB,(L'W4MIDN,W4MIDN)    MIDDLE NAME                   
         GOTOR ENCNUMF,DMCB,(4,=C'0.00')          PAY RATE                      
         GOTOR ENCTEXTF,DMCB,(2,=C'SH')           PAY TYPE                      
                                                                                
         GOTOR ENCTEXTF,DMCB,(2,=C'MY')           PAY FREQUENCY                 
         GOTOR ENCTEXTF,DMCB,(L'W4ADDR1,W4ADDR1)  ADDRESS 1                     
         GOTOR ENCTEXTF,DMCB,(L'W4ADDR2,W4ADDR2)  ADDRESS 2                     
         GOTOR ENCTEXTF,DMCB,(L'W4CITY,W4CITY)    CITY                          
         GOTOR ENCTEXTF,DMCB,(L'W4STAT,W4STAT)    STATE                         
         GOTOR ENCTEXTF,DMCB,(L'W4ZIP,W4ZIP)      ZIP                           
         GOTOR ENCTEXTF,DMCB,(L'W4CTRY,W4CTRY)    COUNTRY                       
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCDATEF,DMCB,W4DOB                DATE OF BIRTH                 
         GOTOR ENCTEXTF,DMCB,(L'W4SEX,W4SEX)      GENDER                        
         GOTOR ENCTEXTF,DMCB,(1,=C' ')            NON-UNION                     
         GOTOR ENCTEXTF,DMCB,(1,=C'V')            EMPLOYEE CLASS CODE           
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C'V')            ACA DESIG = VARIABLE          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')            ACA CATEGORY CODE             
         GOTOR ENCTEXTF,DMCB,(1,=C'H')            ACA CLASSIF CODE              
                                                                                
         GOTOR ENCTEXTF,DMCB,(L'SORTJOB,SORTJOB)  JOB TITLE                     
         GOTOR ENCTEXTF,DMCB,(L'SORTCAT,SORTCAT)  CAST CATEGORY                 
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(L'SORTCAUT,SORTCAUT)   WORK LOCATION              
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
                                                                                
         L     RE,TAPEPTR                                                       
         AHI   RE,-1                                                            
         MVI   0(RE),C' '          GET RID OF LAST COMMA                        
                                                                                
         LAY   R5,EITAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                                                
                                                                                
         MVC   SVAGY,SORTAGY       FOR EMPLOYEES WORKING AT DIFF AGY            
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EF210 RECORD (EMPLOYEE PAY INFORMATION)                          
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
RC210    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         XC    YTDGRS,YTDGRS                                                    
         MVC   ENDCHKDT,=XL3'FFFFFF'                                            
                                                                                
*        XC    PRYTDGRS,PRYTDGRS                                                
*        XC    PRDGRS,PRDGRS                                                    
                                                                                
         LA    R4,EMPLST                                                        
RC210A10 CLI   0(R4),X'FF'             EOT                                      
         JE    RC210A80                                                         
                                                                                
         USING TLCKPD,R3                                                        
         MVC   AIO,AIO1                GET YTD GROSS FOR SSN                    
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SORTSSN                                                 
         MVI   TLCKECUR,C'U'           US$                                      
         MVC   TLCKEEMP,0(R4)                                                   
         MVC   TLCKEDTE,TIQPEND                                                 
         XC    TLCKEDTE,=XL3'FFFFFF'   COMPLEMENT                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKEDTE-TLCKPD),KEYSAVE     SAME SSN - EMP?                 
         JNE   RC210A50                                                         
         CLC   TLCKEDTE(1),KEYSAVE+TLCKEDTE-TLCKPD     SAME YEAR                
         JNE   RC210A50                                                         
         CLC   TLCKEDTE,ENDCHKDT                                                
         BNL   RC210A30                                                         
         MVC   ENDCHKDT,TLCKEDTE       LAST CHK DATE IN REQ PERIOD              
         XC    ENDCHKDT,=XL3'FFFFFF'   COMPLEMENT                               
                                                                                
RC210A30 GOTO1 GETREC                                                           
         BRAS  RE,GETYEERN                                                      
         L     RF,DMCB                                                          
         A     RF,YTDGRS                                                        
         ST    RF,YTDGRS                                                        
                                                                                
RC210A50 AHI   R4,3                                                             
         J     RC210A10                                                         
                                                                                
*=====================================================================          
*&&DO                                                                           
         MVC   TLCKEDTE,TIQPSTR        GET PRIOR YTD GROSS B4 RQST              
         XC    TLCKEDTE,=XL3'FFFFFF'   COMPLEMENT                               
         ICM   RF,7,TLCKEDTE                                                    
         AHI   RF,1                                                             
         STCM  RF,7,TLCKEDTE                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKEDTE-TLCKPD+1),KEYSAVE     SAME SSN - CK YEAR?           
         JNE   RC210A70                                                         
                                                                                
         GOTO1 GETREC                                                           
         BRAS  RE,GETYEERN                                                      
         L     RF,DMCB                                                          
         ST    RF,PRYTDGRS                                                      
                                                                                
RC210A70 L     RF,YTDGRS               AND CALC PERIOD GROSS                    
         L     RE,PRYTDGRS                                                      
         SR    RF,RE                                                            
         ST    RF,PRDGRS                                                        
*&&                                                                             
*=====================================================================          
RC210A80 MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
*--------------------------------------------------------------------           
         USING EFRECD,R5                                                        
         BRAS  RE,CLRREC                                                        
         LAY   R5,TAPEREC                                                       
         MVC   EFRTYP,RC210TYP                                                  
                                                                                
         MVC   EF210ECC,=C'17300'               CORP CODE                       
         MVC   EF210SSN,SORTSSN                 SSN                             
         MVC   EF210FEI,IRSACCT                 FEIN                            
         MVI   EF210PMR,C'N'                    PAYROLL MASTER RECORD           
         GOTO1 DATCON,DMCB,(1,ENDCHKDT),(20,EF210CKD)  LAST IN REQ PERD         
         MVC   EF210PYR,=CL13'0.0000'           PAYRATE                         
         MVC   EF210PCD,=C'01'                  TP OR PP, ANNUAL                
         CLC   SORTEMP,=C'P+ '                  P+                              
         JNE   RC210A90                                                         
         MVC   EF210PCD,=C'05'                  SEMI-MONTHLY                    
         TM    AGYSTAT7,TAAYSTSM                IS IT?                          
         BO    RC210A90                                                         
         MVC   EF210PCD,=C'07'                  NO, WEEKLY THEN                 
                                                                                
RC210A90 MVC   EF210YGD,EF210CKD                LAST IN REQ PERD                
*        EDIT  PRDGRS,(11,EF210GRS),2,FLOAT=-,ZERO=NOBLANK                      
*                                                     RQST PERIOD GROSS         
         EDIT  YTDGRS,(13,EF210YTG),2,FLOAT=-,ZERO=NOBLANK                      
*                                                     YTD GROSS                 
         MVC   EF210UCX,=C'TG97'                UCX ACCOUNT                     
                                                                                
         LAY   R5,UITAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                WRITE THE RECORD                
         J     XIT                                                              
*---------------------------------------------------------------------          
GETYEERN NTR1                                                                   
         MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         BNE   GETYEENO                                                         
                                                                                
         USING TAYED,R4                                                         
         L     R4,TGELEM                                                        
         MVC   DMCB(4),TAYEEARN                                                 
         J     XIT                                                              
                                                                                
GETYEENO XC    DMCB,DMCB                                                        
         J     XIT                                                              
                                                                                
RC210TYP DC    CL15'210EMPLPAYINFO'                                             
                                                                                
EMPLST   DC    C'TP '                                                           
         DC    C'PP '                                                           
         DC    C'P+ '                                                           
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EF220 RECORD (EMPLOYEE PAY INFORMATION)                          
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
RC220    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRREC                                                        
         BAS   RE,NEWSUI                                                        
                                                                                
         USING EFRECD,R5                                                        
         LAY   R5,TAPEREC                                                       
         MVC   EFRTYP,RC220TYP                                                  
         MVC   EF220ECC,=C'17300'               CORP CODE                       
         MVC   EF220SSN,SORTSSN                 SSN                             
         MVC   EF220DIV(3),SORTCRP              DIVISION / LOCATION             
         MVC   EF220FRQ,=C'01'                  TP OR PP, ANNUAL                
         CLC   SORTEMP,=C'P+ '                  P+                              
         JNE   RC220A50                                                         
         MVC   EF220FRQ,=C'05'                  SEMI-MONTHLY                    
         TM    AGYSTAT7,TAAYSTSM                IS IT?                          
         BO    RC220A50                                                         
         MVC   EF220FRQ,=C'07'                  NO, WEEKLY THEN                 
                                                                                
RC220A50 MVC   EF220FEI,IRSACCT                 FEIN                            
         MVC   EF220SAC,SUIACCT                 SUI ACCOUNT                     
         MVC   EF220WST,SORTSOT                 WORK STATE                      
         MVC   EF220WLC,SORTSOT                 WORK STATE                      
                                                                                
         EDIT  SORTSPNH,(11,EF220PEN),2,FLOAT=- SUBJ P&H                        
         EDIT  SORTFEDT,(11,EF220FDT),2,FLOAT=- FEDERAL TAX                     
         EDIT  SORTSTAT,(11,EF220STT),2,FLOAT=- STATE TAX                       
         EDIT  SORTLOCT,(11,EF220LCT),2,FLOAT=- LOCAL TAX                       
         EDIT  SORTFICT,(11,EF220FCT),2,FLOAT=- FICA TAX                        
         EDIT  SORTMEDT,(11,EF220MDT),2,FLOAT=- MEDICARE TAX                    
         EDIT  SORTLIEN,(11,EF220LEN),2,FLOAT=- LIEN WITHHELD                   
         EDIT  SORTMDED,(11,EF220OTH),2,FLOAT=- MISC DEDUCTION                  
         GOTO1 DATCON,DMCB,(1,SORTRUND),(20,EF220END)    RUN DATE               
         GOTO1 DATCON,DMCB,(1,SORTCKDT),(20,EF220PAY)                           
         EDIT  SORTSUIW,(11,EF220SUI),2,FLOAT=- SUI WAGES                       
         MVI   EF220PTA,C'A'                    PAY TYPE A                      
         EDIT  SORTEARN,(11,EF220AGR),2,FLOAT=- GROSS                           
         EDIT  SORTNET,(11,EF220ANT),2,FLOAT=-  NET                             
         MVC   EF220UCX,=C'TG97'                UCX ACCOUNT                     
         MVI   EF220PTR,C'R'                    PAY TYPE R                      
         MVC   EF220RGR,EF220AGR                                                
                                                                                
         LAY   R5,UITAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                WRITE THE RECORD                
         J     XIT                                                              
         EJECT                                                                  
NEWSUI   NTR1                                                                   
                                                                                
         MVC   SUIACCT,SPACES                                                   
                                                                                
         MVC   AIO,AIO1            SET AIO SO WON'T CREAM SYSIO'S REC           
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A0',SORTEMP)  READ EMPLOYER REC           
         JNE   XIT                                                              
         MVI   ELCODE,TATIELQ                                                   
                                                                                
         MVI   WORK,C'U'                                                        
         MVC   WORK+1(3),SORTSOT                                                
         GOTO1 GETL,DMCB,(4,WORK)   STATE SUC ACCT                              
         JNE   XIT                                                              
         USING TATID,R4                                                         
         L     R4,TGELEM                                                        
*                                                                               
         LA    R1,L'TATIID                MAKE IT RIGHT JUSTIFIED               
         LA    RE,SUIACCT+L'SUIACCT-1                                           
         LA    RF,TATIID+L'TATIID-1                                             
NEWSUI10 CLI   0(RF),C' '                                                       
         BNH   NEWSUI20                                                         
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,-1                                                            
NEWSUI20 AHI   RF,-1                                                            
         BCT   R1,NEWSUI10                                                      
                                                                                
NEWSUI50 MVC   AIO,TIAREC          RESET AIO TO TIAREC                          
         J     XIT                                                              
*                                                                               
*                                                                               
RC220TYP DC    CL15'220EMPLPAYDTL'                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PAY DETAILS HEADINGS                                             
*---------------------------------------------------------------------          
PAYDETHD NTR1  BASE=*,LABEL=*                                                   
         LAY   R5,TAPEREC                                                       
         ST    R5,TAPEPTR                                                       
                                                                                
         BRAS  RE,CLRREC               CLEAR TAPE RECORD                        
         BRAS  RE,MRCNTCK              MOST RECENT CHECK - NEW HIRE?            
                                                                                
         GOTOR ENCTEXTF,DMCB,(06,=C'COCODE')                                    
         GOTOR ENCTEXTF,DMCB,(10,=C'DataSource')                                
         GOTOR ENCTEXTF,DMCB,(15,=C'PayrollSystemID')                           
         GOTOR ENCTEXTF,DMCB,(07,=C'PayDate')                                   
         GOTOR ENCTEXTF,DMCB,(18,=C'PayPeriodStartDate')                        
         GOTOR ENCTEXTF,DMCB,(16,=C'PayPeriodEndDate')                          
         GOTOR ENCTEXTF,DMCB,(04,=C'FEIN')                                      
         GOTOR ENCTEXTF,DMCB,(03,=C'SSN')                                       
         GOTOR ENCTEXTF,DMCB,(10,=C'EmployeeID')                                
         GOTOR ENCTEXTF,DMCB,(12,=C'AssignmentID')                              
         GOTOR ENCTEXTF,DMCB,(11,=C'BillingCode')                               
         GOTOR ENCTEXTF,DMCB,(16,=C'CompensationType')                          
         GOTOR ENCTEXTF,DMCB,(13,=C'AdjustmentKey')                             
         GOTOR ENCTEXTF,DMCB,(07,=C'PayRate')                                   
         GOTOR ENCTEXTF,DMCB,(07,=C'PayType')                                   
         GOTOR ENCTEXTF,DMCB,(17,=C'PayCycleFrequency')                         
         GOTOR ENCTEXTF,DMCB,(11,=C'HoursWorked')                               
         GOTOR ENCTEXTF,DMCB,(08,=C'GrossPay')                                  
         GOTOR ENCTEXTF,DMCB,(16,=C'W2Box1Deductions')                          
         GOTOR ENCTEXTF,DMCB,(14,=C'ACASecurityKey')                            
         GOTOR ENCTEXTF,DMCB,(06,=C'NetPay')                                    
         GOTOR ENCTEXTF,DMCB,(18,=C'WorkNumberDivision')                        
                                                                                
         L     RE,TAPEPTR                                                       
         AHI   RE,-1                                                            
         MVI   0(RE),C' '          GET RID OF LAST COMMA                        
                                                                                
         LAY   R5,PDTAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                                                
                                                                                
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PAYDET (EMPLOYEE PAYROLL DETAIL RECORD)                          
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
PAYDET   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LAY   R5,TAPEREC                                                       
         ST    R5,TAPEPTR                                                       
                                                                                
         BRAS  RE,CLRREC               CLEAR TAPE RECORD                        
                                                                                
         GOTOR ENCTEXTF,DMCB,(5,=C'17300')                                      
         GOTOR ENCTEXTF,DMCB,(3,=C'PCT')                                        
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCDATEF,DMCB,SORTCKDT                  CHECK DATE               
         OC    SORTCYCS(6),SORTCYCS                                             
         BZ    PAYDET30                                                         
         GOTOR ENCDATEF,DMCB,SORTCYCS                  CYCLE START              
         OC    SORTCYCE,SORTCYCE                       NO CYCLE END?            
         BZ    PAYDET20                                USE CYCLE START          
         GOTOR ENCDATEF,DMCB,SORTCYCE                  CYCLE END                
         B     PAYDET50                                                         
PAYDET20 GOTOR ENCDATEF,DMCB,SORTCYCS                                           
         B     PAYDET50                                                         
PAYDET30 GOTOR ENCDATEF,DMCB,SORTCKDT                                           
         GOTOR ENCDATEF,DMCB,SORTCKDT                                           
PAYDET50 GOTOR ENCTEXTF,DMCB,(L'IRSACCT,IRSACCT)       FEIN                     
         GOTOR ENCTEXTF,DMCB,(L'SORTSSN,SORTSSN)       SS#                      
                                                                                
         GOTO1 SSNPACK,DMCB,SORTSSN,WORK                                        
         GOTOR ENCTEXTF,DMCB,(L'TGPID,WORK)            PID                      
         GOTOR ENCTEXTF,DMCB,(L'SORTAGY,SORTAGY)       AGENCY                   
         MVC   BLOCK(6),SORTAGY                                                 
         MVC   BLOCK+6(6),WORK                                                  
         EDIT  SVSSNULS,(3,BLOCK+12),0,FILL=0                                   
         GOTOR ENCTEXTF,DMCB,(15,BLOCK)                BILLING CODE             
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
                                                                                
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         GOTOR ENCNUMF,DMCB,(4,=C'0.00')               PAY RATE                 
         GOTOR ENCTEXTF,DMCB,(2,=C'SH')                PAY TYPE                 
                                                                                
         GOTOR ENCTEXTF,DMCB,(2,=C'MY')                PAY FREQ                 
         GOTOR ENCNUMF,DMCB,(1,=C'8')                  HOURS WORKED             
         EDIT  SORTEARN,(13,WORK),2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=-             
         GOTOR ENCNUMF,DMCB,(13,WORK)                  GROSS PAY                
                                                                                
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                 W2 BOX 1                 
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
         EDIT  SORTNET,(13,WORK),2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=-              
         GOTOR ENCNUMF,DMCB,(13,WORK)                  NET WAGE                 
         GOTOR ENCTEXTF,DMCB,(1,=C' ')                                          
                                                                                
         L     RE,TAPEPTR                                                       
         AHI   RE,-1                                                            
         MVI   0(RE),C' '          GET RID OF LAST COMMA                        
                                                                                
         LAY   R5,PDTAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                                                
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EF999 RECORDS (FOOTER)                                           
*---------------------------------------------------------------------          
RC999    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRREC                                                        
                                                                                
         USING EFRECD,R5                                                        
         LAY   R5,TAPEREC                                                       
         MVC   EFRTYP,EF999                                                     
                                                                                
         LAY   R5,UITAPE                                                        
         GOTOR PUTTAPE,DMCB,(R5)                                                
         J     XIT                                                              
*                                                                               
EF999    DC    CL15'999FOOTER'                                                  
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PUT RECORD TO TAPE                                               
*        P1 = A(FILE DCB)                                                       
*---------------------------------------------------------------------          
PUTTAPE  NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)            SAVE ADDRESS OF TAPE DCB                     
         LAY   R0,TAPEREC                                                       
                                                                                
         LHI   R1,L'TAPEREC                                                     
         LAY   RF,TAPEEND-1                                                     
PTAPE10  CLI   0(RF),C' '                                                       
         BH    PTAPE50                                                          
         AHI   RF,-1                                                            
         BCT   R1,PTAPE10                                                       
                                                                                
PTAPE50  AHI   R1,4                                                             
         USING OWTAPED,RF                                                       
         LAY   RF,TAPERLN                                                       
         STH   R1,OWRECLN                                                       
                                                                                
         LR    R1,R5               RESTORE ADDRESS OF TAPE DCB                  
         LAY   R0,TAPERLN                                                       
                                                                                
         TM    TUOPTS,TUNOTAPE     DO NOT OUTPUT TO TAPE?                       
         JO    XIT                 YES, LEAVE                                   
         PUT   (1),(0)             PUT THE RECORD TO TAPE                       
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO GET AGENCY'S STATUS 7                                 
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
GETSTAT7 NTR1  BASE=*,LABEL=*                                                   
         MVI   AGYSTAT7,0                                                       
                                                                                
         CLC   SORTEMP,=C'P+ '                                                  
         JNE   XIT                                                              
         MVC   AIO,AIO1            SET AIO SO WON'T CREAM SYSIO'S REC           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',SORTAGY)  READ EMPLOYER REC           
         MVC   AIO,TIAREC          SET AIO SO WON'T CREAM SYSIO'S REC           
         JNE   XIT                                                              
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   AGYSTAT7,TAAYSTA7                                                
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              VALIDATE USING SOFT DATES                                        
*              R2 = FIELD HEADER                                                
*=====================================================================          
         USING SOFDATD,R1                                                       
VSFTDAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SDBLOCK                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R2,SOFAINP          A(INPUT)                                     
         LA    R3,OUTDATE                                                       
         ST    R3,SOFAOUT          A(OUTPUT)                                    
         MVC   SOFACOM,ACOMFACS    A(COMFACS)                                   
         MVI   SOFITYPE,SOFITYMD   VALIDATE FOR YEAR, MONTH, DAY                
         MVI   SOFOTYPE,SOFOTSD2   12 BYTE EBCIDIC (YYMMDDYYMMDD)               
         MVI   SOFIINDS,SOFIISFT   VALIDATE ONLY SOFT DATES                     
*                                                                               
         MVC   SOFTODAY,TGTODAY0   TODAY'S DATE                                 
         MVC   SOFCTRY,CTRY        COUNTRY CODE                                 
         MVC   SOFLANG,LANG        LANGUAGE CODE                                
         MVI   SOFSYSN,7           TALENT SYSTEM                                
         GOTO1 SOFTDATE,SOFDATD                                                 
         JZ    VSFTDAT3                                                         
* CHECK FOR YEAR, MONTH VALIDATION                                              
         MVI   SOFITYPE,SOFITYM    VALIDATE FOR YEAR, MONTH THEN                
         GOTO1 SOFTDATE,SOFDATD                                                 
         JZ    VSFTDAT3                                                         
         MVC   ERROR,SOFERROR                                                   
         J     NO                                                               
*                                                                               
VSFTDAT3 CLI   OFFLINE,C'Y'                                                     
         JE    VSFTDAT5                                                         
         CLI   CONOUTH+4,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         JE    VSFTDAT5                                                         
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         JE    YES                 DON'T RESOLVE DATES                          
*                                                                               
VSFTDAT5 OI    SOFIINDS,SOFIIRES   RESOLVE THE DATES TO ACTUAL                  
         GOTO1 SOFTDATE,SOFDATD                                                 
         GOTO1 DATCON,DMCB,(0,OUTDATE),(1,TIQPSTR)                              
         GOTO1 DATCON,DMCB,(0,OUTDATE+6),(1,TIQPEND)                            
         J     YES                                                              
*                                                                               
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        PUT TEXT FIELD INSIDE QUOTES                                           
*          P1 = B0 = LENGTH OF FIELD                                            
*          P1 = B1-B3 = ADDRESS OF FIELD                                        
*---------------------------------------------------------------------          
ENCTEXTF NTR1  BASE=*,LABEL=*                                                   
         XR    R3,R3                                                            
         IC    R3,0(R1)            LENGTH OF FIELD                              
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)          ADDRESS OF FIELD                             
                                                                                
         L     RF,TAPEPTR                                                       
         AHI   R3,-1               DECR FOR EX MOVE                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SPACES      ANY DATA TO SHOW?                            
         BNH   ENCTF50             NO, JUST PUT COMMA AND CONTINUE              
                                                                                
         MVI   0(RF),C'"'                                                       
         AHI   RF,1                                                             
                                                                                
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE IN FIELD TO TAPE AREA                   
                                                                                
         LR    R4,RF               CHECK FOR LAST NON SPACE CHAR                
         AR    RF,R3                                                            
ENCTF10  CLI   0(RF),C' '                                                       
         BH    ENCTF20                                                          
         AHI   RF,-1                                                            
         B     ENCTF10                                                          
                                                                                
ENCTF20  MVC   1(2,RF),=C'",'      ENCLOSE IT WITH END QUOTE AND COMMA          
         AHI   RF,3                                                             
         ST    RF,TAPEPTR                                                       
         J     XIT                                                              
                                                                                
ENCTF50  MVI   0(RF),C','          BLANK, JUST PUT COMMA                        
         AHI   RF,1                                                             
         ST    RF,TAPEPTR                                                       
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        PUT NUMERIC FIELD OUT                                                  
*          P1 = ADDRESS OF FIELD                                                
*---------------------------------------------------------------------          
ENCNUMF  NTR1  BASE=*,LABEL=*                                                   
         XR    R3,R3                                                            
         IC    R3,0(R1)            LENGTH OF FIELD                              
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)          ADDRESS OF FIELD                             
                                                                                
         L     RF,TAPEPTR                                                       
         AHI   R3,-1               DECR FOR EX MOVE                             
                                                                                
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE IN FIELD TO TAPE AREA                   
                                                                                
         LR    R4,RF               CHECK FOR LAST NON SPACE CHAR                
         AR    RF,R3                                                            
ENCNF10  CLI   0(RF),C' '                                                       
         BH    ENCNF20                                                          
         AHI   RF,-1                                                            
         B     ENCNF10                                                          
                                                                                
ENCNF20  MVC   1(2,RF),=C','      ENCLOSE IT WITH COMMA                         
         AHI   RF,2                                                             
         ST    RF,TAPEPTR                                                       
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        PUT DATE FIELD OUT                                                     
*          P1 = ADDRESS OF FIELD                                                
*---------------------------------------------------------------------          
ENCDATEF NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            ADDRESS OF FIELD                             
         MVC   WORK,SPACES                                                      
         OC    0(3,R2),0(R2)                                                    
         JZ    ENCDF10                                                          
                                                                                
         GOTO1 DATCON,DMCB,(1,0(R2)),(23,WORK)                                  
         MVC   WORK+10(4),WORK                                                  
         MVC   WORK(5),WORK+5                                                   
         MVI   WORK+2,C'/'                                                      
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(4),WORK+10                                                
                                                                                
ENCDF10  GOTOR ENCTEXTF,DMCB,(10,WORK)                                          
                                                                                
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        SKIP CANADIAN COUNTRY FROM W4                                          
*---------------------------------------------------------------------          
         USING TAA2D,R4                                                         
SKIPCANC NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1                                                          
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   TAA2LEN,TAA2LNQ     HAS COUNTRY CODE?                            
         JL    YES                 NO, ASSUME UNITED STATES                     
         CLC   TAA2CTRY,=C'CA'                                                  
         JE    NO                                                               
         J     YES                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        RETRIEVE CATEGORY NAME (JOB TITLE)                                     
*            SAVE IT IN SORTJOB                                                 
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING CATTABD,RF                                                       
JOBTITLE NTR1  BASE=*,LABEL=*                                                   
         MVC   SORTJOB,SPACES                                                   
         L     RF,TGACATS                                                       
JOBTTL3  CLI   0(RF),X'FF'         TEST END OF TABLE                            
         JE    XIT                                                              
         CLC   CATCDE,SORTCAT      FIND CATEGORY                                
         JE    JOBTTL5                                                          
         ZIC   R1,CATLEN                                                        
         AR    RF,R1                                                            
         J     JOBTTL3                                                          
JOBTTL5  ZIC   RE,CATLEN           SET CATEGORY NAME                            
         SH    RE,=Y(CATNAME-CATTABD+1)                                         
         CHI   RE,L'SORTJOB-1                                                   
         JNH   *+8                                                              
         LHI   RE,L'SORTJOB-1                                                   
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   SORTJOB(0),CATNAME    SAVE IT IN BLOCK                           
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EXTRACT TASK CODE FOR PERFORMER                                  
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLTMD,R3            R2=A(TIMESHEET RECORD)                       
GETTASK  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLC   TGAGY,=CL6'999999'  ADJUSTMENTS                                  
         JNE   GTASK500                                                         
                                                                                
         USING TAOID,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAOIELQ      WILL NEED OLD INVOICE                        
         BRAS  RE,GETEL                                                         
         JNE   GTASK500                                                         
         MVC   SVINV,TAOIINV                                                    
                                                                                
GTASK500 MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSPPL    P+ TIMESHEET ONLY                            
         MVC   TLTMCOM,SVINTCNM                                                 
         MVC   TLTMINV,SVINV                                                    
         XC    TLTMINV,=XL6'FFFFFFFFFFFF'                                       
         MVC   TLTMSSN,SORTSSN                                                  
         MVC   TLTMSORT,SVCKSORT                                                
         MVI   TLTMSORT,0                                                       
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(TLTMCAT-TLTMD),KEYSAVE    MAKE SURE SAME TIMESHEET           
         JNE   GTASKXIT                                                         
         GOTO1 GETREC                                                           
                                                                                
         USING TATDD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   GTASKXIT                                                         
*        MVC   SORTJOB(6),TATDTASK                                              
                                                                                
         USING TLTKD,R3                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVI   TLTKCD,TLTKCDQ                                                   
         MVI   TLTKSCD,TLTKSCDQ                                                 
         MVC   TLTKTASK,TATDTASK                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(TLTKLEN-TLTKD),KEYSAVE    MAKE SURE SAME TASK                
         JNE   GTASKXIT                                                         
         GOTO1 GETREC                                                           
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   GTASKXIT                                                         
         SR    R1,R1                                                            
         IC    R1,TANALEN                                                       
         SH    R1,=Y(TANANAME-TANAD+1)                                          
         CHI   R1,L'SORTJOB-1                                                   
         JNH   *+8                                                              
         LHI   R1,L'SORTJOB-1                                                   
         EX    R1,*+8                                                           
         J     *+10                                                             
                                                                                
         MVC   SORTJOB(0),TANANAME                                              
                                                                                
GTASKXIT MVC   AIO,TIAREC          RESTORE AIO AND READ SEQ                     
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         J     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EXTRACT DATA FROM YTD EARNINGS ELEMENT                           
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XYEEL    NTR1  BASE=*,LABEL=*                                                   
         XC    SORTSUIW,SORTSUIW                                                
                                                                                
         MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         JNE   XIT                                                              
                                                                                
         USING TAYED,R4                                                         
         L     R4,TGELEM                                                        
         MVC   SORTSUIW,TAYETSUI      TAXABLE SUI                               
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              EXTRACT DATA FROM CHECK WTH DETAILS ELEMENT                      
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
         USING TLCKD,R4            R4=A(RECORD)                                 
XCWEL    NTR1  BASE=*,LABEL=*                                                   
         XC    SORTFEDT(SORTMEDT-SORTFEDT),SORTFEDT                             
                                                                                
         USING TACWD,R4            R4=A(CHECK DETAILS EL.)                      
         MVI   ELCODE,TACWELQ      GET CHECK WTH DETAILS ELEMENT                
         GOTO1 GETL,DMCB,(3,=C'CN ')                                            
         JNE   XCWEL100                                                         
         L     R4,TGELEM                                                        
         TM    TACWSTAT,TACWSRES   NO CANADIAN RESIDENTS                        
         JO    NO                                                               
         TM    TACWSTAT,TACWSWRK   OR WORK IN CANADA                            
         JO    NO                                                               
                                                                                
XCWEL100 MVI   ELCODE,TACWELQ      GET CHECK WTH DETAILS ELEMENT                
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         JNE   XCWEL300                                                         
         L     R4,TGELEM                                                        
                                                                                
         MVC   SORTFEDT,TACWTAX                                                 
         MVC   SORTFICT,TACWFICA                                                
         MVC   SORTMEDT,TACWMED                                                 
                                                                                
XCWEL300 L     R4,TIAREC                                                        
         BRAS  RE,GETEL                                                         
         J     XCWEL600                                                         
XCWEL500 BRAS  RE,NEXTEL                                                        
XCWEL600 JNE   XCWEL900                                                         
         CLC   =C'FD ',TACWUNIT    SKIP FEDERAL, DONE ABOVE                     
         JE    XCWEL500                                                         
         CLI   TACWUNIT+2,C' '     LOCAL UNIT?                                  
         JH    XCWEL800                                                         
         ICM   RF,15,SORTSTAT      ACCUMULATE STATE TAXES                       
         A     RF,TACWTAX                                                       
         STCM  RF,15,SORTSTAT                                                   
         J     XCWEL500                                                         
                                                                                
XCWEL800 ICM   RF,15,SORTLOCT      ACCUMULATE LOCAL TAXES                       
         A     RF,TACWTAX                                                       
         STCM  RF,15,SORTLOCT                                                   
         J     XCWEL500                                                         
                                                                                
XCWEL900 J     YES                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              SET TAXABLE STATE INFO                                           
*---------------------------------------------------------------------          
         USING SORTD,R2            R2=A(SORT RECORD)                            
SOTINFO  NTR1  BASE=*,LABEL=*                                                   
         OC    TIUNIT,TIUNIT       TAXABLE STATE SET BY SYSIO                   
         JZ    NO                                                               
         GOTO1 TAXVAL,DMCB,(3,TIUNIT)  LOOK IT UP TO SET GLOBAL INFO            
         JNE   NO                                                               
         CLC   TGTACODE,=CL3'CN '  REJECT CANADA                                
         JE    NO                                                               
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TGTACODE)                                     
         JE    NO                                                               
         CLC   TGTACODE,=CL3'OT '  AND OTHER                                    
         JE    NO                                                               
         MVC   SORTGATE,TGTAGATE   SAVE GATES/MCDONALD STATE NUMBER             
         MVC   SORTSOT,TGTASTCY    AND OUR STATE CODE                           
         J     YES                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              GET STATE AND LOCAL WITHHOLDINGS FOR P+                          
*---------------------------------------------------------------------          
STLOCWTH NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   PRCSDST,C'N'        DIDN'T PROCESS STATE YET                     
         BRAS  RE,SOTINFO                                                       
                                                                                
         USING TACWD,R4                                                         
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TACWELQ      FIND THE STATE WITHHOLD FIRST                
         GOTO1 GETL,DMCB,(3,TIUNIT)                                             
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         MVC   SORTSTAT,TACWTAX                                                 
         CLI   TACWLEN,TACWLN3Q                                                 
         JL    STLW100                                                          
         MVC   SORTFEDT,TACWPFTX                                                
         MVC   SORTFICT,TACWPFFI                                                
*        GOTO1 SORTER,DMCB,=C'PUT',(R2)     WRITE OUT SORT RECORD               
*                                                                               
         USING TALUNITD,R3                                                      
STLW100  DS    0H                  SEE IF UNIT HAS LOCALS                       
         L     R3,TGAUNITS         R3 = A(TAX UNIT TABLE)                       
STLW110  CLI   0(R3),X'FF'         IF END OF TABLE THEN RETURN NEQ              
         JE    STLW999                                                          
         CLC   TALUCODE,TIUNIT     FIND THE STATE FIRST                         
         JE    STLW200                                                          
         LA    R3,TALUNEXT         BUMP TO NEXT ONE                             
         J     STLW110                                                          
                                                                                
STLW200  MVC   PRCSTATE,TALUNUM    SAVE UNIT NUMBER TO MATCH ON                 
         LA    R3,TALUNEXT         SEE IF NEXT ENTRY MATCHES                    
STLW300  CLC   PRCSTATE,TALUNUM                                                 
         BNE   STLW999                                                          
                                                                                
         GOTO1 GETL,DMCB,(3,TALUCODE)  FIND LOCAL WITHHOLDING ELEM              
         JNE   STLW800                                                          
         L     R4,TGELEM                                                        
                                                                                
         ICM   RF,15,SORTLOCT      ACCUMULATE LOCAL TAXES                       
         A     RF,TACWTAX                                                       
         STCM  RF,15,SORTLOCT                                                   
                                                                                
         GOTO1 SORTER,DMCB,=C'PUT',(R2)     WRITE OUT SORT RECORD               
         MVI   PRCSDST,C'Y'        PROCESSED STATE WITH LOCAL                   
         LA    R3,TALUNEXT                                                      
         J     STLW300                                                          
                                                                                
STLW800  DS    0H                                                               
*---------------------------------                                              
STLW999  CLI   PRCSDST,C'N'        PROCESSED STATE ALREADY?                     
         JNE   XIT                 YES, LEAVE                                   
         GOTO1 SORTER,DMCB,=C'PUT',(R2)     WRITE OUT SORT RECORD               
         J     XIT                                                              
                                                                                
PRCSDST  DS    CL1                                                              
PRCSTATE DS    CL2                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              CLEAR TAPEREC WITH SPACES                                        
*---------------------------------------------------------------------          
CLRREC   NTR1  BASE=*,LABEL=*                                                   
         LAY   R0,TAPEREC                                                       
         LHI   R1,L'TAPEREC                                                     
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               CLEAR RECORD TO ALL SPACES                   
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        EQUIFAX DSECT                                                          
*        CURRENTLY FOR TENNESSEE                                                
*----------------------------------------------------------------------         
EFRECD   DSECT                                                                  
EFRTYP   DS    CL15                RECORD TYPE                                  
EF000FIL DS    CL91                FILLER                                       
EF000VER DS    CL5                 VERSION                                      
EF000ID  DS    CL5                 SOURCE ID                                    
EF000TYP DS    CL20                SOURCE TYPE                                  
EF000CDE DS    CL1                 CODE (I-INITIAL / R-RESUBMITTED)             
         DS    CL6                 FILLER                                       
         EJECT                                                                  
*----------------------------------------------------------------------         
         ORG   EF000FIL                                                         
         DS    CL11                EMPLOYER COMPANY CDOE BLANKS                 
EF202ECC DS    CL5                 EMPLOYER COMPANY CDOE                        
EF202SSN DS    CL9                 SSN                                          
         DS    CL2                                                              
EF202EID DS    CL64                EMPLOYEE ID (PID)                            
EF202UID DS    CL64                USER ID                                      
EF202UCX DS    CL4                 TG97                                         
EF202PDT DS    CL8                 PAYROLL AS OF DATE                           
EF202FN  DS    CL64                FIRST NAME                                   
EF202MN  DS    CL64                MIDDLE NAME                                  
EF202LN  DS    CL64                LAST NAME                                    
         DS    CL17                                                             
EF202PIN DS    CL8                 EMPLOYEE PIN - LAST 4 OF SSN                 
EF202LGN DS    CL1                 DIRECT LOGIN TO TALX - Y                     
EF202DIV DS    CL12                DIVISION LX4, LX5, LX6 (TP, PP, P+)          
EF202JOB DS    CL64                JOB TITLE                                    
EF202ESC DS    CL1                 EMPLOYEE STATUS CODE                         
EF202MHD DS    CL8                 MOST RECENT HIRE DATE                        
         DS    CL1                                                              
EF202OHD DS    CL8                 ORIGINAL HIRE DATE                           
EF202TRM DS    CL8                 TERMINATION DATE                             
EF202RTM DS    CL2                 REASON FOR TERM - 03 LACK OF WORK            
         DS    CL83                                                             
EF202FRQ DS    CL2                 PAY FREQUENCY                                
         DS    CL2                                                              
EF202AI1 DS    CL1                 ADDRESS 1 INDICATOR                          
EF202AT1 DS    CL20                ADDRESS 1 TYPE                               
EF202AD1 DS    CL60                ADDRESS LINE 1                               
EF202AD2 DS    CL60                ADDRESS LINE 2                               
EF202AD3 DS    CL60                ADDRESS LINE 3                               
EF202CIT DS    CL60                CITY                                         
EF202ST  DS    CL2                 STATE                                        
EF202ZIP DS    CL16                ZIP                                          
EF202CNT DS    CL2                 COUNTRY                                      
         DS    CL84                                                             
         DS    365C                ADDRESS 2                                    
         DS    365C                ADDRESS 3                                    
         DS    365C                ADDRESS 4                                    
EF202PHP DS    CL1                 HOME PHONE PROVIDED                          
EF202PHC DS    CL3                 HOME PHONE COUNTRY                           
EF202PHN DS    CL16                HOME PHONE                                   
         DS    1662C                                                            
EF202DOB DS    CL8                 DATE OF BIRTH                                
         DS    1128C                                                            
EF202TCI DS    CL5                 TCI CLIENT ID                                
         DS    CL71                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
         ORG   EF000FIL                                                         
         DS    CL11                EMPLOYER COMPANY CDOE BLANKS                 
EF210ECC DS    CL5                 EMPLOYER COMPANY CDOE                        
EF210SSN DS    CL9                 SSN                                          
         DS    CL66                                                             
EF210FEI DS    CL15                FEIN                                         
         DS    CL12                                                             
EF210PMR DS    CL1                 PAYROLL MASTER RECORD - N                    
EF210CKD DS    CL8                 CHECK DATE                                   
         DS    CL3                                                              
EF210SES DS    CL1                 SESSION?                                     
         DS    CL246                                                            
EF210PYR DS    CL13                PAY RATE - 0.00                              
EF210PCD DS    CL2                 PAY RATE CODE (01=TP,PP/ P+ DEPENDS)         
         DS    CL10                                                             
EF210YGD DS    CL8                 YEAR OF GROSS SALARY - CHECK DATE            
EF210GRS DS    CL11                GROSS PAY                                    
         DS    CL44                                                             
EF210YTG DS    CL13                YTD GROSS                                    
         DS    CL152                                                            
EF210UCX DS    CL4                 UCX ACCOUNT                                  
         DS    CL6                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
         ORG   EF000FIL                                                         
         DS    CL11                EMPLOYER COMPANY CDOE BLANKS                 
EF220ECC DS    CL5                 EMPLOYER COMPANY CDOE                        
EF220SSN DS    CL9                 SSN                                          
         DS    CL66                                                             
EF220DIV DS    CL12                DIVISION / LOCATION                          
EF220FRQ DS    CL2                 PAY FREQUENCY                                
EF220FEI DS    CL15                FEIN                                         
EF220SAC DS    CL15                SUI ACCOUNT                                  
EF220WST DS    CL2                 WORK STATE                                   
EF220WLC DS    CL2                 WORK STATE                                   
         DS    CL18                                                             
EF220CNT DS    CL2                 COUNTRY CODE                                 
         DS    CL11                                                             
EF220PEN DS    CL11                WAGES SUBJ TO P&H                            
         DS    CL22                                                             
EF220FDT DS    CL11                FEDERAL TAX                                  
EF220STT DS    CL11                STATE TAX                                    
EF220LCT DS    CL11                LOCAL TAX                                    
EF220FCT DS    CL11                FICA TAX                                     
EF220MDT DS    CL11                MEDICARE TAX                                 
EF220LEN DS    CL11                LIEN WITHHELD                                
EF220OTH DS    CL11                MISC DEDUCTION                               
EF220END DS    CL8                 PAY PERIOD END - RUN DATE                    
EF220BEG DS    CL8                 PAY PERIOD BEGIN - FFC, PAY PRD              
EF220PAY DS    CL8                 PAY DATE - CHECK DATE                        
EF220SUI DS    CL11                SUI WAGES                                    
EF220PTA DS    CL1                 PAY TYPE A - A                               
EF220AGR DS    CL11                GROSS PAY                                    
EF220ANT DS    CL11                NET PAY                                      
         DS    CL8                                                              
EF220UCX DS    CL4                 UCX ACCOUNT NUMBER                           
EF220PTR DS    CL1                 PAY TYPE R - R                               
EF220RGR DS    CL11                GROSS PAY                                    
         DS    CL19                                                             
EF220PTO DS    CL1                 PAY TYPE O - O                               
EF220OGR DS    CL11                GROSS PAY                                    
         DS    CL19                                                             
EF220PTV DS    CL1                 PAY TYPE V - V                               
         DS    CL30                                                             
EF220PTS DS    CL1                 PAY TYPE S - S                               
         DS    CL30                                                             
EF220PTH DS    CL1                 PAY TYPE H - H                               
         DS    CL30                                                             
EF220PTB DS    CL1                 PAY TYPE B - B                               
EF220BGR DS    CL11                GROSS PAY                                    
         DS    CL19                                                             
EF220PTE DS    CL1                 PAY TYPE E - E                               
         DS    CL30                                                             
EF220PTP DS    CL1                 PAY TYPE P - P                               
         DS    CL30                                                             
EF220PTW DS    CL1                 PAY TYPE W - W                               
         DS    CL30                                                             
EF220PTK DS    CL1                 PAY TYPE K - K                               
         DS    CL30                                                             
EF220PTC DS    CL1                 PAY TYPE C - C                               
EF220CGR DS    CL11                GROSS PAY                                    
         DS    CL19                                                             
EF220PTT DS    CL1                 PAY TYPE T - T                               
         DS    CL30                                                             
EF220PTM DS    CL1                 PAY TYPE M - M                               
         DS    CL30                                                             
         DS    CL4                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
         ORG   EF000FIL                                                         
         DS    CL100                                                            
EF990GAD DS    CL9                 EMPLOYER GRPADD RECORDS                      
EF990GMT DS    CL9                 EMPLOYER GRPMGMT RECORDS                     
EF990NTM DS    CL9                 EMPLOYER NOTE TEMPLATE RECORDS               
EF990EGI DS    CL9                 EMPLOYEE GENERAL INFORMATION RECORDS         
EF990EPI DS    CL9                 EMPLOYEE PAY INFORMATION RECORDS             
EF990EPD DS    CL9                 EMPLOYEE PAY DETAIL RECORDS                  
EF990EBI DS    CL9                 EMPLOYEE BENEFITS INFORMATION RECORD         
EF990EBD DS    CL9                 EMPLOYEE BENEFIT DEPENDENT RECORDS           
EF990EDR DS    CL9                 EMPLOYEE DISABILITY RECORDS                  
EF990ENR DS    CL9                 EMPLOYEE NOTE RECORDS                        
EF990GDL DS    CL9                 EMPLOYEE GRPDEL RECORDS                      
EF990EGA DS    CL9                 EMPLOYEE GRPADD RECORDS                      
EF990EGM DS    CL9                 EMPLOYEE GRPMGMT RECORDS                     
EF990PRC DS    CL9                 EMPLOYEE PAYSTUB RECORDS                     
EF990PMS DS    CL9                 EMPLOYEE PAYSTUB MESSAGE RECORDS             
EF990PER DS    CL9                 EMPLOYEE PAYSTUB EARNINGS DETAIL REC         
EF990PPR DS    CL9                 EMPLOYEE PAYSTUB PRETAX DETAIL REC           
EF990PTX DS    CL9                 EMPLOYEE PAYSTUB TAXES DETAIL REC            
EF990PPO DS    CL9                 EMPLOYEE PAYSTUB POSTTAX DETAIL REC          
EF990POB DS    CL9                 EMPLOYEE PAYSTUB OTHERBENS DETAIL RE         
EF990PDS DS    CL9                 EMPLOYEE PAYSTUB DISTRIBUTIONS DETAI         
EF990TWS DS    CL9                 EMPLOYEE TAXABLE WAGE SUMMARY REC            
EF990PDR DS    CL9                 EMPLOYEE PAYSTUB DIRDEP RECORDS              
EF990EW4 DS    CL9                 EMPLOYEE W4 RECORDS                          
EF990EGD DS    CL9                 EMPLOYER GRPDEL RECORDS                      
EF990PIM DS    CL9                 EMPLOYEE PIM RECORDS                         
EF990WTC DS    CL9                 EMPLOYER WOTC LOCATION RECORDS               
EF990TIC DS    CL9                 EMPLOYER TICS WORK SITE RECORDS              
EF990TCR DS    CL9                 EMPLOYEE TAX CREDT RECORDS                   
EF990TCP DS    CL9                 EMPLOYEE TAX CREDT PAY CHECK RECORDS         
EF990IW4 DS    CL9                 EMPLOYEE INTERACTIVE W4 RECORDS              
EF990LOC DS    CL9                 I-9 LOCATION RECORDS                         
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TUD      DSECT                                                                  
TUPERIOD DS    CL17                DISPLAYABLE REQUEST PERIOD                   
*                                                                               
TUOPTS   DS    XL1                 OPTIONS                                      
TUTRACE  EQU   X'80'               TRACE ACTIVE                                 
TURECKDT EQU   X'40'               USE CHECK DATE FOR REUSE PAYMENTS            
TUNOTAPE EQU   X'20'               DO NOT OUTPUT TO TAPE                        
TUBENFIT EQU   X'10'               BENEFITS REPORTING                           
*                                                                               
TUPRINT  DS    C                   Y=PRINT PAYROLL                              
TUSHOOT  DS    XL3                 SHOOT DATE FOR PRINT                         
*                                                                               
TUSTAT   DS    XL1                 STATUS                                       
TUSORTNG EQU   X'80'               SORT IS ACTIVE                               
TUSKIPW4 EQU   X'40'               SKIP TGSSN                                   
*                                                                               
TUSRTREC DS    CL(SORTLNQ)         SORT RECORD AREA                             
TTSRTREC DS    CL(SORTLNQ)         SORT RECORD AREA TOTALLING                   
TUCNT    DS    PL4                 COUNT                                        
TUALLCNT DS    PL4                 OVERALL TOTAL COUNT                          
*                                                                               
TUTOTALS DS    0F                                                               
TUSTATOT DS    F                   STATE TOTAL                                  
TUCRPTOT DS    F                   EMPLOYER CORP CODE TOTAL                     
TUALLTOT DS    F                   OVERALL TOTAL                                
*                                                                               
SVEMPCC  DS    CL3                 EMPLOYER COMPANY CODE                        
W4NAM1   DS    CL(L'TAW4NAM1)                                                   
W4NAM2   DS    CL(L'TAW4NAM2)                                                   
W4MIDN   DS    CL(L'TAW4MIDN)                                                   
W4DOB    DS    XL3                                                              
W4SEX    DS    XL1                                                              
W4ADDR1  DS    CL(L'TAA2ADD1)                                                   
W4ADDR2  DS    CL(L'TAA2ADD2)                                                   
W4ADDR3  DS    CL(L'TAA2ADD3)                                                   
W4CITY   DS    CL(L'TAA2CITY)                                                   
W4STAT   DS    CL(L'TAA2ST)                                                     
W4ZIP    DS    CL(L'TAA2ZIP)                                                    
W4CTRY   DS    CL(L'TAA2CTRY)                                                   
W4CTRYN  DS    CL(L'WORK)                                                       
IRSACCT  DS    CL15                                                             
SUIACCT  DS    CL15                STATE SUI ACCOUNT                            
AGYSTAT7 DS    XL1                 CURRENT AGENCY'S STATUS 7                    
SVSSNULS DS    XL1                 UNIQUE LINE SEQ #                            
SVINTCNM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
SVAGY    DS    XL6                 AGENCY                                       
SVINV    DS    XL6                 INVOICE                                      
SVCKSORT DS    XL6                 CHECK SORT                                   
SVMRCTHD DS    XL3                 HIRE DATE                                    
SVORGNHD DS    XL3                 ORIGINAL HIRE DATE                           
SVDOD    DS    XL3                 DATE OF DEATH                                
ENDCHKDT DS    XL3                 LAST CHECK DATE IN REQUEST PERIOD            
PRI90DYS DS    XL3                 PRIOR 90 DAYS DATE                           
YTDGRS   DS    F                   YTD GROSS                                    
PRYTDGRS DS    F                   PRIOR YTD GROSS BEFORE REQUEST START         
PRDGRS   DS    F                   PERIOD GROSS                                 
SDBLOCK  DS    CL(SOFXTNL)         SOFTDATE BLOCK                               
OUTDATE  DS    CL12                                                             
                                                                                
DODDATEH DS    XL8                 DUMMY FIELD HEADER FOR DODDATE               
DODDATE  DS    CL20                                                             
*                                                                               
TAPEPTR  DS    A                   CURR POS IN TAPEREC (BENE)                   
*                                                                               
         DS    0D                                                               
         DS    F                                                                
TAPERLN  DS    F                                                                
TAPEREC  DS    CL4940                                                           
TAPEEND  EQU   *                                                                
         ORG   TAPEREC                                                          
TAPEGATE DS    CL2                 GATES/MCDONALD STATE NUMBER                  
TAPECRP  DS    CL3                 EMPLOYER CORPORATION CODE                    
TAPELOC  DC    CL3' '              LOCATION CODE                                
         DS    CL3                 SES/RES                                      
TAPESSN  DS    CL9                 S/S NUMBER                                   
TAPELAST DS    CL14                LAST NAME                                    
TAPEFRST DS    CL1                 FIRST INITIAL                                
         DC    CL6' '              TERMINATION DATE                             
TAPEDTE  DS    CL6                 END DATE OF REQUEST (MMDDYY)                 
         DC    C'0'                PAYROLL TYPE  (0=REGULAR)                    
         DC    C'1'                FREQUENCY     (1=WEEKLY)                     
TAPEEARN DS    CL7                 GROSS EARNINGS                               
         DC    CL2' '              SEPARATION CODE                              
         DC    CL2' '              WEEKS WORKED                                 
         DC    CL20' '             FILLER                                       
TULNQ    EQU   *-TUD                                                            
         EJECT                                                                  
OWTAPED  DSECT                                                                  
OWRECLN  DS    F                   RECORD LENGTH                                
OWRECRD  DS    CL4940              RECORD                                       
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTSSN  DS    CL9                 S/S NUMBER                                   
SORTAGY2 DS    CL6                 AGENCY                                       
SORTCKDT DS    PL3                 CHECK DATE                                   
SORTCHK  DS    CL8                 CHECK NUMBER                                 
SORTGATE DS    CL2                 GATES/MCDONALD STATE NUMBER                  
*                                                                               
SORTRUND DS    PL3                 RUN DATE                                     
SORTCAT  DS    CL3                 CAST CATEGORY                                
SORTJOB  DS    CL31                CAST CATEGORY / TASK (P+)                    
SORTSOT  DS    CL3                 TAXABLE STATE                                
SORTFRST DS    PL3                 FIRST SERVICES OR WORK OR SHOOT DATE         
SORTSES  DS    CL1                 Y=SESSION / N=RESIDUAL                       
SORTEARN DS    XL4                 GROSS EARNINGS                               
SORTYTDG DS    XL4                 YTD GROSS EARNINGS                           
SORTSUIW DS    XL4                 SUI WAGES                                    
SORTSPNH DS    XL4                 SUBJ TO P&H                                  
SORTFEDT DS    XL4                 FEDERAL TAXES                                
SORTSTAT DS    XL4                 STATE TAXES                                  
SORTLOCT DS    XL4                 LOCAL TAXES                                  
SORTFICT DS    XL4                 FICA TAXES                                   
SORTMEDT DS    XL4                 MEDICARE TAXES                               
SORTLIEN DS    XL4                 LIEN WITHHELD                                
SORTMDED DS    XL4                 MISC DEDUCTIONS                              
SORTSUTW DS    XL4                 SUTA WAGES                                   
SORTNET  DS    XL4                 NET EARNINGS                                 
SORTCRP  DS    CL3                 EMPLOYER CORP CODE                           
SORTEMP  DS    CL3                 EMPLOYER                                     
SORTAGY  DS    CL6                 AGENCY                                       
SORTINV  DS    CL6                 INVOICE                                      
SORTW4TY DS    CL1                 W4 TYPE                                      
SORTCYCS DS    XL3                 CYCLE START                                  
SORTCYCE DS    XL3                 CYCLE END                                    
SORTUN   DS    CL3                 UNION                                        
SORTCAUT DS    CL3                 CAST UNIT                                    
SORTLNQ  EQU   *-SORTD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
BL       DS    CL1                                                              
PRNTSSN  DS    CL9                 S/S NUMBER                                   
BC1      DS    CL1                                                              
PRNTLAST DS    CL16                LAST NAME                                    
BC2      DS    CL1                                                              
PRNTFRST DS    CL16                FIRST NAME                                   
BC3      DS    CL1                                                              
PRNTMID  DS    CL1                 MIDDLE INITIAL                               
         DS    CL1                                                              
BC4      DS    CL1                                                              
PRNTW4TY DS    CL1                 W4 TYPE                                      
BC5      DS    CL1                                                              
PRNTCHKD DS    CL8                 CHECK DATE                                   
BC6      DS    CL1                                                              
PRNTCHK  DS    CL8                 CHECK NUMBER                                 
BC7      DS    CL1                                                              
PRNTTYPE DS    CL3                 SES(SION)/RES(IDUAL)                         
         DS    CL1                                                              
BC8      DS    CL1                                                              
PRNTUNIT DS    CL3                 TAX UNIT                                     
BC9      DS    CL1                                                              
PRNTEARN DS    CL12                GROSS EARNINGS                               
BR       DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPFED                                                       
         EJECT                                                                  
         PRINT OFF                                                              
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSOFDATD                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053TAREP1E   03/08/16'                                      
         END                                                                    
