*          DATA SET TAREP17    AT LEVEL 070 AS OF 12/30/14                      
*PHASE T70317E,*                                                                
*                                                                               
         TITLE 'T70317 - TALENT PAYROLL REGISTER'                               
T70317   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70317,RA,R9                                                   
         L     RC,0(R1)            RC = A(GENCON WORKING STORAGE)               
         USING GEND,RC                                                          
         L     R8,ASPOOLD          R8 = A(SPOOL DSECT)                          
         USING SPOOLD,R8                                                        
         L     R7,ASUBSYSD         R7 = A(CONTROLLER WORKING STORAGE)           
         USING SUBSYSD,R7                                                       
         LA    R6,BUFF             R6 = A(LOCAL WORKING STORAGE)                
         LA    R6,8(R6)                                                         
         USING PAYROLLD,R6                                                      
         EJECT                                                                  
* MAIN DRIVER - THE WORK IS BROKEN UP INTO TWO PASSES.  THE FIRST PASS          
* READS AND PROCESSES ALL THE RECORDS FROM TALDIR/TALFIL THAT MUST BE           
* READ AND WRITES OUT SORTER RECORDS WITH THE INFORMATION IT EXTRACTED          
* FROM THE FILE.  THE SECOND PASS READS BACK THE SORTER RECORDS AND             
* FROM THE INFORMATION THEY CONTAIN PRINTS CHECKS IN THE REGISTER.              
*                                                                               
MAIN     DS    0H                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE SYSTEM WORKING STORAGE            
*                                                                               
         CLI   MODE,PRINTREP       DO ALL THROUGH MODE PRINTREP                 
         BNE   MX                                                               
*                                                                               
         BAS   RE,INITADDR         INITIALIZE ADDRESSES                         
*                                                                               
         BAS   RE,INITCTR          INITIALIZE CONTROL VARIABLES                 
*                                                                               
         GOTO1 DOTRACE,DMCB,PAYROLLD,SRTREC-PAYROLLD,=C'PYRD',4,=C'R'           
*                                                                               
         LA    R2,SRTKEYL          INSERT SORT KEY LENGTH INTO SORTCARD         
         LA    R3,SORTCARD+15                                                   
         EDIT  (R2),(2,(R3))                                                    
*                                                                               
         LHI   R2,SRTRECL    SET L'SORT RECORD IN RECCARD                       
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(4),DUB+5(3)                                           
*                                                                               
         GOTO1 DOTRACE,DMCB,SORTCARD,160,=C'SORTCARD',8,=C'R'                   
*                                                                               
*                                  OPEN SORTER FILE                             
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         BAS   RE,CHKPASS1         CHECK RECORDS PASS 1 - PUT SORTRECS          
*                                                                               
         BAS   RE,CHKPASS2         CHECK RECORDS PASS 1 - GET SORTRECS          
*                                                                               
MX       B     XIT                                                              
         SPACE 2                                                                
* INITIALIZE ADDRESSES.                                                         
*                                                                               
INITADDR NTR1                                                                   
         L     RF,ATWA             RF = A(TWA)                                  
         USING T703FFD,RF                                                       
*                                                                               
         L     RE,TWADCONS         SAVE ADDRESSES OF TWADCON ROUTINES           
         USING TWADCOND,RE                                                      
         MVC   BINSRCH,TBINSRCH                                                 
         L     R2,TSPFUSER         R2=A(USER AREA ACROSS REQUESTS)              
         OC    0(4,R2),0(R2)       IF ALREADY HAVE ADDRESS OF TABLE             
         BNZ   IA20                DON'T LOAD IT AGAIN                          
*                                                                               
         CLI   REQTOA,C'Y'         IF LOADING A VERSION OF TABLE                
         BNE   IA05                                                             
         LOAD  EP=TAREPTOA,ERRET=LOADERR                                        
         B     IA10                                                             
IA05     LOAD  EP=TAREPTO,ERRET=LOADERR                                         
         B     IA10                                                             
LOADERR  DC    H'0'                                                             
IA10     ST    R0,0(R2)                                                         
*                                                                               
IA20     MVC   ATOTTAB,0(R2)       SAVE A(TOTALS ACCUMULATORS TABLE)            
         DROP  RE,RF                                                            
*                                                                               
         LA    RF,SRTCHECK         SAVE A(CHECK RECORD IOAREA)                  
         ST    RF,ASRTCHK                                                       
         LA    R1,MYSPECS          SET UP SPECS FOR REPORT                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET HEADHOOK                                 
         ST    R1,HEADHOOK                                                      
         B     XIT                                                              
         EJECT                                                                  
* INITIALIZE PROGRAM CONTROL VARIABLES.                                         
*                                                                               
INITCTR  NTR1                                                                   
         L     RE,ATOTTAB          CLEAR TOTALS ACCUMULATORS TABLE              
         L     RF,=A(MAXTOTS*TOTLEN+1)                                          
         XCEFL                                                                  
         XC    NUMTOTS,NUMTOTS     CLEAR NUMBER OF TOTTAB ENTRIES               
         B     XIT                                                              
         EJECT                                                                  
* CHECK PASS 1 - READ THROUGH THE INVOICE PASSIVE KEYS BY DUE DATE              
* AND PROCESS EACH INVOICE IN TURN.  LOWER LEVEL ROUTINES WILL EXTRACT          
* INFORMATION FROM THE INVOICE RECORDS,  LOOP THROUGH THE CHECK RECORDS         
* FOR EACH INVOICE, AND WRITE A SORT RECORD FOR EACH CHECK.                     
*                                                                               
CHKPASS1 NTR1                                                                   
         LA    R3,KEY              BUILD CHECK DATE PASSIVE INVOICE KEY         
         USING TLINPD,R3               FOR START DATE                           
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINKCDQ                                                 
         MVC   TLINKDTE,REQSTART                                                
*                                                                               
         GOTO1 HIGH                READ FIRST INVOICE                           
*                                                                               
CPO10    CLI   TLINPKEY,TLINKCDQ   WHILE STILL CHECK DATE POINTER               
         BNE   CPO100                                                           
         CLC   TLINKDTE,REQEND     AND NOT PAST END DATE                        
         BH    CPO100                                                           
         MVC   SVIKEY,KEY          SAVE INVOICE KEY                             
*                                                                               
         CLC   REQAGY,SPACES       IF FILTERING BY AGENCY                       
         BNH   *+14                                                             
         CLC   REQAGY,TLINKAGY     COMPARE AGENCIES                             
         BNE   CPO90                                                            
*                                                                               
         CLC   REQEMP,SPACES       IF FILTERING BY EMPLOYER                     
         BNH   *+14                                                             
         CLC   REQEMP,TLINKEMP     COMPARE EMPLOYERS                            
         BNE   CPO90                                                            
*                                                                               
CPO20    GOTO1 DOTRACE,DMCB,KEY,96,=C'INVOICE KEY',11,=C'5'                     
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         GOTO1 DOTRACE,DMCB,AIO,0,=C'INVOICE RECORD',14,=C'I'                   
*                                                                               
         L     R4,AIO              R4 = A(INVOICE STATUS ELEMENT)               
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
*                                                                               
         TM    TAINSTAT,TAINSERR   SKIP INVOICE IF IN ERROR                     
         BO    CPO90                                                            
*                                                                               
         BAS   RE,PROCINVC         PROCESS ALL CHECKS FOR THIS INVOICE          
*                                                                               
CPO90    MVC   KEY,SVIKEY          RESTORE INVOICE KEY READING SEQUENCE         
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 SEQ                 GET NEXT KEY AND LOOP BACK                   
         B     CPO10                                                            
*                                                                               
CPO100   BAS   RE,TOTSORT          ADD SORT RECORDS FOR TOTALS                  
*                                                                               
         GOTO1 DOTRACE,DMCB,KEY,96,=C'INVOICE KEY',11,=C'5'                     
*                                                                               
CPOX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* PROCESS THE CURRENT INVOICE - READ ALL CHECKS FOR THE INVOICE AND             
* PROCESS EACH IN TURN.                                                         
*                                                                               
PROCINVC NTR1                                                                   
         L     R4,AIO              R4 = A(INVOICE RECORD)                       
         USING TLIND,R4                                                         
*                                                                               
         MVC   SRTAGY,TLINAGY      SAVE AGENCY IN SORT RECORD                   
         MVC   SRTINV,TLININV           INVOICE                                 
         XC    SRTINV,XFFS              (UNCOMPLEMENTED)                        
*                                                                               
         MVI   ELCODE,TAPDELQ      R4 = A(PAYMENT DETAILS ELEMENT)              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         TM    TAPDPST1,TAPDPBNP   IGNORE BNP INVOICES                          
         BO    PIX                                                              
*                                                                               
         MVC   SRTADJS,TAPDADJS    SAVE PAYROLL ADJUSTMENT INDICATORS           
*                                                                               
         CLI   SRTADJS,0           IF THIS IS AN ADJUSTMENT INVOICE             
         BE    PI10                                                             
*                                                                               
         MVC   SRTAAGY,SRTAGY      THEN SAVE ADJUSTMENT AGY/INV                 
         MVC   SRTAINV,SRTINV                                                   
*                                                                               
         XC    SRTAGY,SRTAGY       PRE-CLEAR AGY/INV                            
         XC    SRTINV,SRTINV                                                    
*                                                                               
         L     R4,AIO              R4 = A(ORIGINAL AGY/INV ELEMENT)             
         MVI   ELCODE,TAOIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PI20                SKIP IF NONE FOUND                           
         USING TAOID,R4                                                         
*                                                                               
         MVC   SRTAGY,TAOIAGY      SAVE AGY/INV IN SORT RECORD                  
         MVC   SRTINV,TAOIINV                                                   
         B     PI20                                                             
*                                                                               
PI10     CLI   REQADJ,C'Y'         ELSE IF REQUESTED TO PRINT ADJS ONLY         
         BE    PIX                 THEN SKIP THIS INVOICE                       
*                                                                               
PI20     BAS   RE,CHKLOOP          LOOP THROUGH AND PROCESS CHECK RECS          
*                                                                               
PIX      B     XIT                                                              
         EJECT                                                                  
* LOOP THROUGH THE CHECK RECORDS FOR THIS INVOICE AND PROCESS EACH IN           
* TURN.                                                                         
*                                                                               
CHKLOOP  NTR1                                                                   
         LA    R3,KEY              BUILD FIRST CHECK KEY FOR CURRENT            
         USING TLCKD,R3                AGENCY/INVOICE                           
         XC    TLCKKEY,TLCKKEY                                                  
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,SRTAGY                                                   
         MVC   TLCKINV,SRTINV                                                   
*                                                                               
         CLI   SRTADJS,0           IF THIS IS AN ADJUSTMENT INVOICE             
         BE    *+16                                                             
         MVC   TLCKAGY,SRTAAGY     THEN SET AGY/INV FOR ADJUSTMENT              
         MVC   TLCKINV,SRTAINV                                                  
*                                                                               
         MVC   FILENAME,CHKDIR     READ FIRST KEY                               
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                  WHILE NOT END OF AGENCY/INVOICE              
CL10     CLC   TLCKKEY(TLCKSORT-TLCKD),KEYSAVE                                  
         BNE   CL100                                                            
*                                                                               
         MVC   SVCKEY,TLCKKEY      SAVE CHECK KEY                               
*                                                                               
         GOTO1 DOTRACE,DMCB,KEY,96,=C'CHECK KEY',9,=C'6'                        
*                                                                               
         MVC   AIO,ASRTCHK         READ CHECK RECORD INTO SORT RECORD           
         MVC   FILENAME,CHKFIL                                                  
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 DOTRACE,DMCB,ASRTCHK,0,=C'CHECK RECORD',12,=C'C'                 
*                                                                               
         L     R4,ASRTCHK          R4 = A(CHECK DETAILS ELEMENT)                
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4                                                         
*                                                                               
         OC    REQRUNS,REQRUNS     IF RUN DATE FILTER SPECIFIED                 
         BZ    CL20                                                             
         CLC   TACDRUN,REQRUNS     THEN FILTER INVOICES WITH RUN DATES          
         BL    CL90                    BEFORE START DATE OR AFTER END           
         CLC   TACDRUN,REQRUNE                                                  
         BH    CL90                                                             
*                                                                               
CL20     TM    SRTADJS,TAPDADTT    IF THIS IS NOT A TAX TRANSFER                
         BO    CL30                                                             
         TM    SRTADJS,TAPDADST    OR A SSN TRANSFER                            
         BO    CL30                                                             
         OC    TACDCHK,TACDCHK     THEN SKIP IF NO CHECK NUMBER                 
         BZ    CL90                                                             
*                                                                               
CL30     BAS   RE,PROCCHK          PROCESS CHECK RECORD                         
*                                                                               
         USING TLCKD,R3                                                         
CL90     MVC   KEYSAVE,SVCKEY      RESTORE CHECK KEY READING SEQUENCE           
         MVC   FILENAME,CHKDIR                                                  
         GOTO1 SEQ                 GET NEXT KEY AND LOOP BACK                   
         XC    FILENAME,FILENAME                                                
         B     CL10                                                             
*                                                                               
CL100    GOTO1 DOTRACE,DMCB,KEY,96,=C'CHECK KEY',9,=C'6'                        
*                                                                               
CLX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* PROCESS THE CHECK RECORD THAT HAS BEEN READ INTO THE END OF THE SORT          
* RECORD.  THE ROUTINE WILL READ SEVERAL SUPPORT RECORDS AND BUILD THE          
* REST OF THE SORT RECORD WHICH IT WRITES TO THE SORT FILE.                     
*                                                                               
PROCCHK  NTR1                                                                   
*                                  PRE-CLEAR CHECK AMOUNTS                      
         XC    SRTAMNTS(SRTAMNTL),SRTAMNTS                                      
*                                                                               
         LA    R3,KEY              R3 = A(CHECK KEY)                            
         USING TLCKD,R3                                                         
*                                                                               
         MVC   SRTSSN,TLCKSSN      SAVE SSN IN SORT RECORD                      
*                                                                               
         BAS   RE,EXTRW4           EXTRACT W4 INFO FROM W4 RECORD               
*                                                                               
         BAS   RE,EXTRCPAY         EXTRACT PAY INFO FROM CHECK RECORD           
         BAS   RE,EXTRCAST                 CAST INFO                            
         BAS   RE,EXTRDET                  CHECK DETAILS                        
         BAS   RE,EXTRYTD                  CHECK YTD DETAILS                    
         BAS   RE,EXTRTAX                  TAX WITHHOLDINGS                     
         BAS   RE,EXTRDUE                  DUECOMP WITHHOLDINGS                 
         BAS   RE,EXTRLIEN                 LIEN WITHHOLDINGS                    
         BAS   RE,EXTROTH                  OTHER DEDUCTIONS                     
*                                                                               
*                                  TOTAL DEDUCTIONS =                           
         L     RF,SRTFTAX          FEDERAL TAX +                                
         A     RF,SRTFICA          FICA +                                       
         A     RF,SRTSTAX          STATE TAX +                                  
         A     RF,SRTSDI           SDI +                                        
         A     RF,SRTSUI           SUI +                                        
         A     RF,SRTGST           GST +                                        
         A     RF,SRTLTAX          LOCAL TAX +                                  
         A     RF,SRTCTAX          CANADIAN TAX +                               
         A     RF,SRTMDED          MISC DED + UNION DUES +                      
         A     RF,SRTMPT           MPTRF +                                      
         A     RF,SRTCHAR          PERMANENT CHARITIES +                        
         A     RF,SRTDIRCT         DIRECT DEPOSIT                               
         A     RF,SRTWIRE          WIRE TRANSFER                                
         A     RF,SRTACOM          AGENCY COMMISSION                            
         ST    RF,SRTTDED                                                       
*                                  ADD AMNTS TO TOTAL FOR CURR                  
         GOTO1 ADDTOT,DMCB,SRTCUR,XFFS,XFFS,XFFS                                
*                                                                               
*                                  ADD AMNTS TO TOTAL FOR CURR/EMP              
         GOTO1 ADDTOT,DMCB,SRTCUR,SRTEOR,XFFS,XFFS                              
*                                                                               
         CLI   REQSORT,C'I'        IF SORTING BY INVOICE                        
         BNE   PC10                                                             
*                                  ADD AMNTS TO TOTAL FOR CURR/EMP/AGY          
         GOTO1 ADDTOT,DMCB,SRTCUR,SRTEOR,SRTAGY,XFFS                            
*                                                                               
*                                  ADD TO TOTAL FOR CURR/EMP/AGY/INV            
         GOTO1 ADDTOT,DMCB,SRTCUR,SRTEOR,SRTAGY,SRTINV                          
*                                                                               
PC10     BAS   RE,BLDSORT          BUILD SORT KEY                               
*                                                                               
*                                  PUT SORT RECORD TO SORTER FILE               
         GOTO1 SORTER,DMCB,=C'PUT',SRTREC                                       
*                                                                               
         GOTO1 DOTRACE,DMCB,SRTREC,SRTCHECK-SRTREC,=C'SORT PUT',8,=C'P'         
*                                                                               
         GOTO1 DOTRACE,DMCB,ASRTCHK,0,=C'SRTCHECK',8,=C'P'                      
*                                                                               
PCX      B     XIT                                                              
         EJECT                                                                  
* READ THE W4 RECORD FOR THE CURRENT PERFORMER AND EXTRACT INFORMATION          
* FROM IT INTO THE SORT RECORD.                                                 
*                                                                               
EXTRW4   NTR1                                                                   
*                                  SET NAME TO 'UNKNOWN' UNTIL FOUND            
         MVC   SRTNAME,=CL32'* UNKNOWN *'                                       
*                                                                               
         MVI   SRTTYPE,0           PRECLEAR PERFORMER TYPE                      
*                                                                               
*                                  READ W4 RECORD                               
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SRTSSN)                               
         BNE   EWX                 DONE IF NOT FOUND                            
*                                                                               
         GOTO1 DOTRACE,DMCB,AIO,0,=C'W4 RECORD',9,=C'W'                         
*                                                                               
         L     R4,AIO              R4 = A(W4 DETAILS ELEMENT)                   
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   EWX                 DONE IF NOT FOUND                            
         USING TAW4D,R4                                                         
*                                                                               
         MVC   SRTNAME,TAW4NAM2    SAVE PERFORMER NAME IN SORT RECORD           
         MVC   SRTTYPE,TAW4TYPE         TYPE                                    
*                                                                               
EWX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* EXTRACT INFORMATION FROM THE PAYMENT DETAILS ELEMENT IN THE CHECK             
* RECORD INTO THE SORT RECORD.                                                  
*                                                                               
EXTRCPAY NTR1                                                                   
         L     R4,ASRTCHK          R4 = A(PAYMENT DETAILS ELEMENT)              
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
*                                                                               
         MVC   SRTW4TY,TAPDW4TY    SAVE PAYMENT W4 TYPE IN SORT RECORD          
         MVC   SRTEOR,TAPDEMP           EMPLOYER                                
         MVC   SRTCLI,TAPDCLI           CLIENT                                  
         MVC   SRTADJS,TAPDADJS         PAYROLL ADJUSTMENT INDICATORS           
         MVC   SRTMDED,TAPDMDED         MISC DED = (MDED + DUES)                
         AF    SRTMDED,TAPDDUES                                                 
*                                                                               
         MVI   SRTCUR,SRTCURUS     SET CURRENCY FLAG IN SORTREC                 
         TM    TAPDSTAT,TAPDSCAN                                                
         BZ    *+8                                                              
         MVI   SRTCUR,SRTCURCA                                                  
         TM    TAPDPST2,TAPDPEUR                                                
         BZ    *+8                                                              
         MVI   SRTCUR,SRTCUREU                                                  
*                                                                               
ECPX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* EXTRACT INFORMATION FROM THE CAST DETAILS ELEMENT IN THE CHECK                
* RECORD INTO THE SORT RECORD.                                                  
*                                                                               
EXTRCAST NTR1                                                                   
*                                  PRE-CLEAR CAST ELEMENT VARIABLES             
         XC    SRTCASTS(SRTCASTL),SRTCASTS                                      
*                                                                               
         L     R4,ASRTCHK          R4 = A(CAST DETAILS ELEMENT)                 
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ECAX                RETURN IF NOT FOUND                          
         USING TACAD,R4                                                         
*                                                                               
         MVC   SRTUN,TACAUN        SAVE UNION IN SORT RECORD                    
         MVC   SRTNCDE,TACANCDE         AGENT CODE                              
*                                                                               
         L     R4,ASRTCHK          IF TAX ID ELEMENT FOUND                      
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ECAX                                                             
         USING TATID,R4                                                         
*                                                                               
         MVC   SRTSSN,TATIID       SAVE CORPORATION ID AS SSN                   
*                                                                               
         BAS   RE,EXTRW4           EXTRACT INFO FROM W4 RECORD                  
*                                                                               
ECAX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* EXTRACT AMOUNTS FROM THE CHECK DETAILS ELEMENT IN THE CHECK RECORD            
* INTO THE SORT RECORD.                                                         
*                                                                               
EXTRDET  NTR1                                                                   
         L     R4,ASRTCHK          R4 = A(CHECK DETAILS ELEMENT)                
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACDD,R4                                                         
*                                                                               
         MVC   SRTNTAX,TACDNTAX    MOVE NON-TAXABLE EARNINGS TO SORTREC         
         MVC   SRTAMNT,TACDNET          NET CHECK AMOUNT                        
         MVC   SRTCHK,TACDCHK           CHECK NUMBER                            
         MVC   SRTDTE,TACDDTE           CHECK DATE                              
         MVC   SRTSEQ,TACDSEQ           CHECK ORDER PROCESSED NUMBER            
*                                                                               
         TM    SRTADJS,TAPDADST    IF CHECK IS A SSN TRANSFER                   
         BZ    EDX                                                              
         CLI   SRTW4TY,TAW4TYIN    AND IT'S FOR AN INDIVIDUAL                   
         BE    *+12                                                             
         CLI   SRTW4TY,TAW4TYES    OR AN ESTATE                                 
         BNE   EDX                                                              
         MVC   AIO,ASRTCHK         GET FEDERAL WITHHOLDING EL.                  
         MVI   ELCODE,TACWELQ                                                   
         GOTO1 GETL,DMCB,=C'FD '                                                
         MVC   AIO,AIO1                                                         
         BNE   EDX                                                              
         L     R4,TGELEM           R4 = A(WITHHOLDING ELEMENT)                  
         USING TACWD,R4                                                         
*                                                                               
         TM    TACWSTAT,TACWSTAX   IF FEDERAL IS NOT TAXABLE                    
         BO    EDX                                                              
         XC    SRTAMNT,SRTAMNT     THEN DON'T INCLUDE NET FROM THIS CHK         
*                                                                               
EDX      B     XIT                                                              
         EJECT                                                                  
* EXTRACT AMOUNTS FROM THE CHECK YTDS ELEMENT IN THE CHECK RECORD               
* INTO THE SORT RECORD.                                                         
*                                                                               
EXTRYTD  NTR1                                                                   
         MVC   AIO,ASRTCHK         GET YTD ELEMENT FOR CHECKS                   
         MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         MVC   AIO,AIO1                                                         
         BNE   EYX                                                              
         L     R4,TGELEM           R4 = A(YTD ELEMENT)                          
         USING TAYED,R4                                                         
*                                                                               
         MVC   SRTEARN,TAYETERN    MOVE TAXABLE EARNINGS TO SORT REC            
*                                                                               
         MVC   SRTYEARN,TAYEEARN   MOVE YTD TAXABLE EARNINGS TO SORTREC         
         MVC   SRTYNTAX,TAYENTAX        YTD NON-TAXABLE                         
*                                                                               
         CLC   =C'P+',SRTEOR       P+ CANADIAN                                  
         JNE   EYX                                                              
         L     R4,ASRTCHK                                                       
         MVI   ELCODE,TAATELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   EYX                                                              
         L     R4,TGELEM                                                        
         CLI   TAYELEN,TAYELN2Q                                                 
         JL    EYX                                                              
         OC    TAYEUTAX,TAYEUTAX                                                
         JZ    EYX                                                              
         MVC   SRTYNTAX,TAYEUTAX                                                
*                                                                               
EYX      B     XIT                                                              
         EJECT                                                                  
* EXTRACT AMOUNTS FROM THE CHECK WITHHOLDING ELEMENTS AND YTD WITHHLD           
* ELEMENTS IN THE CHECK RECORD RECORD INTO THE SORT RECORD.                     
*                                                                               
EXTRTAX  NTR1                                                                   
         XC    SRTSOR,SRTSOR       PRE-CLEAR SOR AND SOW IN SORT RECORD         
         XC    SRTSOW,SRTSOW                                                    
*                                                                               
         L     R4,ASRTCHK          R4 = A(FIRST CHECK WITHHLD ELEMENT)          
         MVI   ELCODE,TACWELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ET100               DONE IF NONE FOUND                           
         USING TACWD,R4                                                         
*                                                                               
         USING TACYD,R5                                                         
ET10     MVC   AIO,ASRTCHK         GET YTD ELEMENT FOR THIS UNIT                
         MVI   ELCODE,TACYELQ                                                   
         GOTO1 GETL,DMCB,(3,TACWUNIT)                                           
         BE    ET15                                                             
         XC    ELEM,ELEM           CAN'T FIND IT, SET UP DUMMY TACY             
         LA    R5,ELEM                                                          
         MVI   TACYEL,TACYELQ                                                   
         MVI   TACYLEN,TACYLN2Q                                                 
         MVC   TACYUNIT,TACWUNIT                                                
         MVC   AIO,AIO1                                                         
         B     ET18                                                             
*                                                                               
ET15     MVC   AIO,AIO1                                                         
         L     R5,TGELEM           R5 = A(YTD ELEMENT)                          
*                                                                               
ET18     CLC   TACWUNIT,=C'FD '    IF UNIT IS NOT FEDERAL                       
         BE    ET30                                                             
*                                                                               
         CLI   SRTSOR+2,C' '       THEN IF SOR IS NOT A CITY                    
         BH    ET20                                                             
         TM    TACWSTAT,TACWSRES   THEN IF PERFORMER IS RESIDENT                
         BZ    ET20                                                             
         MVC   SRTSOR,TACWUNIT     THEN SAVE SOR IN SORT RECORD                 
*                                                                               
ET20     CLI   SRTSOW+2,C' '       IF SOW IS NOT A CITY                         
         BH    ET30                                                             
         TM    TACWSTAT,TACWSWRK   THEN IF PERFORMER WORKED THERE               
         BZ    ET30                                                             
         MVC   SRTSOW,TACWUNIT     THEN SAVE SOW IN SORT RECORD                 
*                                                                               
ET30     CLC   TACWUNIT,=C'FD '    IF UNIT IS FEDERAL                           
         BNE   ET40                                                             
         MVC   SRTFTAX,TACWTAX     THEN SAVE FEDERAL TAX IN SORT RECORD         
         MVC   SRTFICA,TACWFICA              FICA                               
         MVC   SRTYFED,TACYTAX               YTD FEDERAL TAX                    
         MVC   SRTYFIC,TACYFICA              YTD FICA                           
         B     ET90                                                             
*                                                                               
ET40     CLI   TACWUNIT+2,C' '     ELSE IF UNIT IS A CITY                       
         BNH   ET50                                                             
         L     RF,TACWTAX          THEN ADD LOCAL TAX TO LOCAL TAX              
         A     RF,SRTLTAX              IN SORT RECORD                           
         ST    RF,SRTLTAX                                                       
         BAS   RE,ET300                                                         
         B     ET90                                                             
*                                                                               
ET50     CLC   TACWUNIT,=C'CN '    ELSE IF UNIT IS CANADA                       
         BNE   ET60                                                             
         MVC   SRTCTAX,TACWTAX     THEN SAVE CANADA TAX IN SORT RECORD          
         MVC   SRTYCAN,TACYTAX               YTD CANADIAN TAX                   
         MVC   SRTGST,TACWGST      SAVE GST TO GST IN SORT RECORD               
         MVC   SRTYGST,TACYGST     SAVE YTD GST TO YTD GST IN SORT REC          
         B     ET90                                                             
*                                                                               
ET60     L     RF,TACWTAX          ELSE ADD STATE TAX TO STATE TAX              
         A     RF,SRTSTAX              IN SORT RECORD                           
         ST    RF,SRTSTAX                                                       
*                                                                               
         L     RF,TACWSDI          ADD SDI TO SDI IN SORT RECORD                
         A     RF,SRTSDI                                                        
         CLI   TACWLEN,TACWLN2Q                                                 
         BL    *+8                                                              
         A     RF,TACWSFLI                                                      
         ST    RF,SRTSDI                                                        
*                                                                               
         L     RF,TACWSUI          ADD SUI TO SUI IN SORT RECORD                
         A     RF,SRTSUI                                                        
         ST    RF,SRTSUI                                                        
         BAS   RE,ET300                                                         
         B     ET90                                                             
                                                                                
ET90     MVI   ELCODE,TACWELQ      BUMP TO NEXT CW ELEMENT                      
         BAS   RE,NEXTEL                                                        
         BE    ET10                REPEAT UNTIL NO MORE CW ELEMENTS             
*                                                                               
ET100    BAS   RE,PKCANTAX         P+ CANADIAN TAX                              
*                                                                               
         BAS   RE,SETMULTI         SET MULTI-STATE/MULTI-CITY STATUS            
                                                                                
         CLC   TGEMP,=C'P+ '                                                    
         BNE   ET150                                                            
         L     R4,ASRTCHK                                                       
         MVI   ELCODE,TATUELQ                                                   
         USING TATUD,R4                                                         
         BAS   RE,GETEL                                                         
         B     ET120                                                            
ET110    BAS   RE,NEXTEL                                                        
ET120    BNE   ET150                                                            
         CLC   =C'FD ',TATUUNIT                                                 
         BE    ET110                                                            
         CLC   =C'CN ',TATUUNIT                                                 
         BE    ET110                                                            
                                                                                
         MVI   ELCODE,TACYELQ                                                   
         MVC   AIO,ASRTCHK                                                      
         GOTO1 GETL,DMCB,(3,TATUUNIT)                                           
         BE    ET130                                                            
         MVI   ELCODE,TATUELQ                                                   
         B     ET110                                                            
                                                                                
ET130    L     R5,TGELEM                                                        
         BAS   RE,ET400                                                         
         MVI   ELCODE,TATUELQ                                                   
         B     ET110                                                            
*                                                                               
ET150    L     R4,ASRTCHK          R4 = A(FIRST OTHER DEDUCT ELEMENT)           
         MVI   ELCODE,TAODELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ET200               DONE IF NONE FOUND                           
         USING TAODD,R4                                                         
*                                                                               
         CLI   TAODTYPE,TAODTYPF   IF DEDUCTION TYPE IS FOREIGN FEDTAX          
         BNE   ET200                                                            
*                                                                               
         MVC   AIO,ASRTCHK         THEN GET YTD ELEMENT FOR FEDERAL             
         MVI   ELCODE,TACYELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         L     R5,TGELEM           R5 = A(YTD ELEMENT)                          
*                                                                               
         MVC   SRTFTAX,TAODAMT     SAVE FEDERAL TAX IN SORT RECORD              
         MVC   SRTYFED,TACYTAX          YTD FEDERAL TAX                         
*                                                                               
         USING TACXD,R4                                                         
ET200    L     R4,ASRTCHK          R4 = A(FIRST CHECK WITHHLD ELEMENT)          
         MVI   ELCODE,TACXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ET500                                                            
         L     RF,SRTGST                                                        
         ICM   RE,15,TACXPST                                                    
         SR    RF,RE                                                            
         ST    RF,SRTGST           ADD PST INTO GST                             
         L     RF,SRTYGST                                                       
         ICM   RE,15,TACXPSTY                                                   
         SR    RF,RE                                                            
         ST    RF,SRTYGST          ADD PST INTO GST                             
         B     ET500                                                            
                                                                                
ET300    CLC   TGEMP,=C'P+ '       SKIP IF EMPLOYER P+                          
         BER   RE                                                               
                                                                                
ET400    CLI   TACYUNIT+2,C' '     ELSE IF UNIT IS A CITY                       
         BNH   ET450                                                            
         L     RF,TACYTAX          ADD YTD LOCAL TAX TO YTD LOCAL TAX           
         A     RF,SRTYLOC              IN SORT RECORD                           
         ST    RF,SRTYLOC                                                       
         BR    RE                                                               
                                                                                
ET450    L     RF,TACYTAX          ADD YTD STATE TAX TO YTD STATE TAX           
         A     RF,SRTYSTA              IN SORT RECORD                           
         ST    RF,SRTYSTA                                                       
                                                                                
         L     RF,TACYSDI          ADD YTD SDI TO YTD SDI                       
         A     RF,SRTYSDI              IN SORT RECORD                           
         CLI   TACYLEN,TACYLN2Q                                                 
         BL    *+8                                                              
         A     RF,TACYSFLI                                                      
         ST    RF,SRTYSDI                                                       
                                                                                
         L     RF,TACYSUI          ADD YTD SUI TO YTD SUI                       
         A     RF,SRTYSUI              IN SORT RECORD                           
         ST    RF,SRTYSUI                                                       
         BR    RE                                                               
         DROP  R5                                                               
                                                                                
ET500    CLC   TGEMP,=C'P+ '       EMPLOYER P+                                  
         BNE   ETX                                                              
                                                                                
         L     R4,ASRTCHK          ACCUMULATE YTD SDI/SUI FOR NON-TATU          
         MVI   ELCODE,TACYELQ      TACY ELEMENTS                                
         USING TACYD,R4                                                         
         BAS   RE,GETEL                                                         
         B     ET520                                                            
ET510    MVI   ELCODE,TACYELQ                                                   
         BAS   RE,NEXTEL                                                        
ET520    BNE   ETX                                                              
         CLC   =C'FD ',TACYUNIT                                                 
         BE    ET510                                                            
         CLC   =C'CN ',TACYUNIT                                                 
         BE    ET510                                                            
                                                                                
         MVI   ELCODE,TATUELQ                                                   
         MVC   AIO,ASRTCHK                                                      
         GOTO1 GETL,DMCB,(3,TACYUNIT)                                           
         BE    ET510                                                            
                                                                                
         CLI   TACYUNIT+2,C' '     ELSE IF UNIT IS A CITY                       
         BNH   ET530                                                            
         L     RF,TACYTAX          ADD YTD LOCAL TAX TO YTD LOCAL TAX           
         A     RF,SRTYLOC              IN SORT RECORD                           
         ST    RF,SRTYLOC                                                       
         B     ET510                                                            
                                                                                
ET530    L     RF,TACYTAX          ADD YTD STATE TAX TO YTD STATE TAX           
         A     RF,SRTYSTA              IN SORT RECORD                           
         ST    RF,SRTYSTA                                                       
                                                                                
         L     RF,TACYSDI          ADD YTD SDI TO YTD SDI                       
         A     RF,SRTYSDI              IN SORT RECORD                           
         CLI   TACYLEN,TACYLN2Q                                                 
         BL    *+8                                                              
         A     RF,TACYSFLI                                                      
         ST    RF,SRTYSDI                                                       
                                                                                
         L     RF,TACYSUI          ADD YTD SUI TO YTD SUI                       
         A     RF,SRTYSUI              IN SORT RECORD                           
         ST    RF,SRTYSUI                                                       
         B     ET510                                                            
         DROP  R4                                                               
***********************************************************************         
ETX      B     XIT                                                              
         EJECT                                                                  
* ROUTINE SETS MULTI-STATE, MULTI-CITY STATUS                                   
*                                                                               
SETMULTI NTR1                                                                   
         USING TACAD,R4                                                         
         L     R4,ASRTCHK                                                       
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         TM    TACASTA3,TACASPPL   PAYROLL PLUS?                                
         BO    SM55                                                             
                                                                                
         XR    R0,R0               R0=STATE COUNTER                             
         XR    R2,R2               R2=CITY COUNTER                              
                                                                                
         USING STATED,R3                                                        
         LA    R3,BLOCK                                                         
         XC    0(SDLNQ,R3),0(R3)                                                
                                                                                
         USING TACWD,R4                                                         
         L     R4,ASRTCHK                                                       
         MVI   ELCODE,TACWELQ      READ ALL CHECK WITHHOLDING ELEMENTS          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SM10     BAS   RE,NEXTEL                                                        
         BNE   SM50                                                             
                                                                                
         CLC   TACWUNIT,=C'FD '    REJECT FEDERAL WITHHOLDING ELEMENTS          
         BE    SM10                                                             
         TM    TACWSTAT,TACWSWRK   AND NON-WORKING ELEMENTS                     
         BZ    SM10                                                             
                                                                                
         CLI   TACWUNIT+2,C' '     IF UNIT IS A STATE                           
         BNE   SM30                AND THIS IS NOT THE FIRST STATE              
         LTR   R0,R0               ENCOUNTERED                                  
         BNZ   SM80                SET AS MULTI-STATE/MULTI-CITY                
         AHI   R0,1                                                             
                                                                                
         LA    RE,SMI                                                           
         CLC   =C'MI',TACWUNIT     IF UNIT IS MICHIGAN                          
         BE    SM20                                                             
         LA    RE,SNY                                                           
         CLC   =C'NY',TACWUNIT     OR NEW YORK                                  
         BE    SM20                                                             
         LA    RE,SOH                                                           
         CLC   =C'OH',TACWUNIT     OR OHIO                                      
         BE    SM20                                                             
         LA    RE,SPA                                                           
         CLC   =C'PA',TACWUNIT     OR PENNSYLVINIA                              
         BNE   SM10                SAVE INCOME TAX IN TABLE                     
SM20     MVC   0(L'TACWTAX,RE),TACWTAX                                          
         B     SM10                                                             
                                                                                
SM30     LTR   R2,R2               IF UNIT IS A CITY                            
         BNZ   SM80                AND THIS IS NOT THE FIRST CITY               
         AHI   R2,1                ENCOUNTERED                                  
                                                                                
         LA    RE,SDET                                                          
         CLC   =C'DET',TACWUNIT    IF UNIT IS DETROIT                           
         BE    SM40                                                             
         LA    RE,SNYC                                                          
         CLC   =C'NYC',TACWUNIT    OR NEW YORK CITY                             
         BE    SM40                                                             
         LA    RE,SCIN                                                          
         CLC   =C'CIN',TACWUNIT    OR CINCINNATI                                
         BE    SM40                                                             
         LA    RE,SCLV                                                          
         CLC   =C'CLV',TACWUNIT    OR CLEVELAND                                 
         BE    SM40                                                             
         LA    RE,SPHL                                                          
         CLC   =C'PHL',TACWUNIT    OR PHILADELPHIA                              
         BNE   SM10                SAVE INCOME TAX IN TABLE                     
SM40     MVC   0(L'TACWTAX,RE),TACWTAX                                          
         B     SM10                                                             
         DROP  R4                                                               
                                                                                
SM50     CLC   SMI,SDET                                                         
         BNE   SM80                                                             
         CLC   SNY,SNYC                                                         
         BNE   SM80                                                             
         CLC   SPA,SPHL                                                         
         BNE   SM80                                                             
                                                                                
         L     RE,SCIN                                                          
         A     RE,SCLV                                                          
         C     RE,SOH                                                           
         BE    XIT                                                              
         DROP  R3                                                               
                                                                                
SM55     XR    R0,R0               P+ - CHECK MULTI STATE/CITY                  
         XR    R2,R2                                                            
                                                                                
         USING TACWD,R4                                                         
         L     R4,ASRTCHK                                                       
         MVI   ELCODE,TACWELQ      READ ALL CHECK WITHHOLDING ELEMENTS          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SM60     BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
                                                                                
         CLC   TACWUNIT,=C'FD '    SKIP FEDERAL                                 
         BE    SM60                                                             
         CLI   TACWUNIT+2,C' '     STATE?                                       
         BNE   SM65                                                             
         LTR   R0,R0               ALREADY FOUND A STATE?                       
         BNZ   SM80                YES - THEN RETURN MLT                        
         AHI   R0,1                                                             
         B     SM60                                                             
                                                                                
SM65     LTR   R2,R2               ALREADY FOUND A CITY?                        
         BNZ   SM80                YES - THEN RETURN MLT                        
         AHI   R2,1                                                             
         B     SM60                                                             
                                                                                
SM80     MVC   SRTSOW,=C'MLT'                                                   
         B     XIT                                                              
         EJECT                                                                  
* EXTRACT AMOUNTS FROM THE DUE COMPANY WITHHOLDING ELEMENTS IN THE              
* CHECK RECORD INTO THE SORT RECORD.                                            
*                                                                               
EXTRDUE  NTR1                                                                   
         L     R4,ASRTCHK          R4 = A(FIRST DUECOMP WITHHLD ELEM)           
         MVI   ELCODE,TADWELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   EDUX                DONE IF NONE FOUND                           
         USING TADWD,R4                                                         
*                                                                               
EDU10    ICM   RF,15,TADWREC       IF DUECOMP AMOUNT IS POSITIVE                
         BM    EDU20                                                            
*                                                                               
         A     RF,SRTDUED          THEN ADD AMOUNT TO DUECOMP DEBITS            
         ST    RF,SRTDUED                                                       
         B     EDU30                                                            
*                                                                               
EDU20    A     RF,SRTDUEC          ELSE ADD AMOUNT TO DUECOMP CREDITS           
         ST    RF,SRTDUEC                                                       
*                                                                               
EDU30    BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    EDU10               REPEAT UNTIL NO MORE ELEMENTS                
*                                                                               
EDUX     B     XIT                                                              
         EJECT                                                                  
* EXTRACT AMOUNTS FROM THE LIEN WITHHOLDING ELEMENT IN THE CHECK                
* RECORD INTO THE SORT RECORD.                                                  
*                                                                               
EXTRLIEN NTR1                                                                   
         LA    R5,SRTLIENC         REGULAR LIEN                                 
*                                                                               
         L     R4,ASRTCHK          R4 = A(CHECK DETAILS ELEMENT)                
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                 ABEND IF NONE FOUND                          
         DC    H'0'                                                             
         USING TACDD,R4                                                         
         TM    TACDSTA2,TACDSELN   IF EFT LIEN, SAVE IT DIFF                    
         BZ    *+8                                                              
         LA    R5,SRTLIENE         EFT LIEN CHECK                               
*                                                                               
EL10     L     R4,ASRTCHK          R4 = A(LIEN WITHHOLDING ELEMENT)             
         MVI   ELCODE,TALWELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   ELX                 DONE IF NONE FOUND                           
         USING TALWD,R4                                                         
*                                                                               
         ICM   RF,15,TALWREC       IF LIEN AMOUNT IS POSITIVE                   
         BM    EL50                                                             
*                                                                               
         MVC   SRTLIEND,TALWREC    THEN MOVE AMOUNT TO LIEN DEBITS              
         B     ELX                                                              
*                                                                               
EL50     MVC   0(4,R5),TALWREC     ELSE MOVE AMOUNT TO LIEN CREDITS             
*                                                                               
ELX      B     XIT                                                              
         EJECT                                                                  
* EXTRACT AMOUNTS FROM THE OTHER DEDUCTIONS ELEMENTS IN THE CHECK               
* RECORD INTO THE SORT RECORD.                                                  
*                                                                               
EXTROTH  NTR1                                                                   
*                                                                               
         MVI   ELCODE,TANUELQ                                                   
         MVC   AIO,ASRTCHK                                                      
         USING TANUD,R5                                                         
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TANUTPAC))                                     
         BNE   EO10                                                             
         L     R5,TGELEM           R5 = A(DIRECT DEPOSIT ELEMENT)               
         MVC   TGFULL,TANUMBER                                                  
         XC    TGFULL,=X'FFFFFFFF'                                              
         L     RF,TGFULL                                                        
         AHI   RF,1                                                             
         STCM  RF,15,SRTACOM                                                    
         STCM  RF,15,SRTYACOM                                                   
*                                                                               
EO10     MVI   ELCODE,TAODELQ                                                   
         MVC   AIO,ASRTCHK                                                      
         USING TAODD,R5                                                         
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPD))                                     
         BNE   *+14                                                             
         L     R5,TGELEM           R5 = A(DIRECT DEPOSIT ELEMENT)               
         MVC   SRTDIRCT,TAODAMT                                                 
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPW))                                     
         BNE   *+14                                                             
         L     R5,TGELEM           R5 = A(WIRE TRANSFER ELEMENT)                
         MVC   SRTWIRE,TAODAMT                                                  
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPM))                                     
         BNE   *+14                                                             
         L     R5,TGELEM           R5 = A(MPTRF ELEMENT)                        
         MVC   SRTMPT,TAODAMT                                                   
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPP))                                     
         BNE   *+14                                                             
         L     R5,TGELEM           R5 = A(PERMANENT CHARITIES ELEMENT)          
         MVC   SRTCHAR,TAODAMT                                                  
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TAODTYPT))                                     
         BNE   EOX                                                              
         TM    SRTADJS,TAPDADTR    SKIP MINOR CHK IN TAX/ISSUECASE              
         BZ    *+14                IF GENERATING CHECK TO TRUSTEE               
         OC    SRTAMNT,SRTAMNT                                                  
         BZ    EOX                                                              
         L     R5,TGELEM           R5 = A(W4 TRUSTEE DEDUCTION ELEMENT)         
         ICM   RF,15,TAODAMT       IF AMOUNT IS POSITIVE                        
         BM    *+14                                                             
         MVC   SRTTRSTD,TAODAMT    THEN MOVE AMOUNT TO DEBITS                   
         B     *+10                                                             
         MVC   SRTTRSTC,TAODAMT    ELSE MOVE AMOUNT TO CREDITS                  
*                                                                               
EOX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE FINDS OR, IF NECESSARY, ADDS THE TOTTAB ENTRY SPECIFIED          
* BY PARAMETERS 1-4,  THE CURRENCY, EMPLOYER, AGENCY, AND INVOICE.              
* IT THEN ADDS THE CHECK AMOUNTS FOR THE CURRENT CHECK TO THE TABLE             
* ENTRY.  IF THE CHECK IS AN ADJUSTMENT CHECK, IT WILL ADD AN                   
* ADDITIONAL ENTRY FOR THE ADJUSTMENT TYPE, ALLOWING FOR SEPARATE               
* TOTALS FOR ADJUSTMENTS.                                                       
*                                                                               
ADDTOT   NTR1                                                                   
         LA    R5,BLOCK            R5 = A(LOOKUP ENTRY)                         
         USING TOTTABD,R5                                                       
*                                                                               
         XC    BLOCK(256),BLOCK    BUILD LOOKUP ENTRY                           
         L     RF,0(R1)            CURRENCY IN PARM 1                           
         MVC   TOTCUR,0(RF)                                                     
         L     RF,4(R1)            EMPLOYER IN PARM 2                           
         MVC   TOTEOR,0(RF)                                                     
         L     RF,8(R1)            AGENCY IN PARM 3                             
         MVC   TOTAGY,0(RF)                                                     
         L     RF,12(R1)           INVOICE IN PARM 4                            
         MVC   TOTINV,0(RF)                                                     
*                                                                               
         BAS   RE,LOOKTOT          LOOK UP ENTRY AND ADD AMOUNTS TO IT          
*                                                                               
         CLI   SRTADJS,0           IF CHECK IS A PAYROLL ADJUSTMENT             
         BE    ATX                                                              
*                                                                               
         MVC   TOTADJS,SRTADJS     ADD ADJUSTMENT INDICATOR TO LOOKUP           
*                                                                               
         BAS   RE,LOOKTOT          LOOK UP ENTRY AND ADD AMOUNTS TO IT          
*                                                                               
ATX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE LOOKS UP THE TOTTAB ENTRY POINTED TO BY R5 IN TOTTAB             
* AND ADDS THE CHECK AMOUNTS TO IT.                                             
*                                                                               
LOOKTOT  NTR1                                                                   
*                                  LOOK UP ENTRY IN TABLE                       
         GOTO1 BINSRCH,DMCB,(X'01',(R5)),ATOTTAB,NUMTOTS,TOTLEN,       X        
               (0,L'TOTCUR+L'TOTEOR+L'TOTAGY+L'TOTINV+L'TOTADJS),      X        
               MAXTOTS                                                          
*                                                                               
         OC    1(3,R1),1(R1)       DIE IF NONE FOUND OR ADDED                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NUMTOTS,8(R1)       UPDATE NUMBER OF ENTRIES                     
*                                                                               
         L     R5,0(R1)            R5 = A(ENTRY FOUND)                          
*                                                                               
         LA    R3,TOTAMNTS         R3 = A(TOTAL AMOUNTS)                        
         LA    R4,SRTAMNTS         R3 = A(SORT RECORD AMOUNTS)                  
         LA    RF,SRTAMNTL         RF = # OF AMOUNTS                            
         SRL   RF,2                                                             
*                                                                               
LT10     ICM   RE,15,0(R3)         ADD SRTAMNT TO TOTAMNT                       
         A     RE,0(R4)                                                         
         STCM  RE,15,0(R3)                                                      
*                                                                               
         LA    R3,4(R3)            BUMP TO NEXT AMOUNT                          
         LA    R4,4(R4)                                                         
         BCT   RF,LT10             REPEAT UNTIL NO MORE AMOUNTS                 
*                                                                               
LTX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE LOOPS THROUGH THE TOTTAB ENTRIES AND ADDS A SORT RECORD          
* FOR EACH ONE.                                                                 
*                                                                               
TOTSORT  NTR1                                                                   
         OC    NUMTOTS,NUMTOTS     RETURN IF NO TOTALS TO PROCESS               
         BZ    TSX                                                              
*                                                                               
         LA    RE,SRTREC           CLEAR SORT RECORD                            
         LHI   RF,SRTRECL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R5,ATOTTAB          R5 = A(FIRST TOTTAB ENTRY)                   
         USING TOTTABD,R5                                                       
         L     R4,NUMTOTS          R4 = # OF TOTTAB ENTRIES                     
*                                                                               
TS10     MVC   SRTCUR,TOTCUR       SAVE CURRENCY IN SORT RECORD                 
         MVC   SRTEOR,TOTEOR            EMPLOYER                                
         MVC   SRTAGY,TOTAGY            AGENCY                                  
         MVC   SRTINV,TOTINV            INVOICE                                 
         MVC   SRTADJS,TOTADJS          PAYROLL ADJUSTMENTS INDICATOR           
*                                                                               
*                                  SAVE AMOUNTS IN SORT RECORD                  
         MVC   SRTAMNTS(SRTAMNTL),TOTAMNTS                                      
*                                                                               
         BAS   RE,BLDSORT          BUILD SORT KEY                               
*                                                                               
*                                  PUT SORT RECORD TO SORTER FILE               
         GOTO1 SORTER,DMCB,=C'PUT',SRTREC                                       
*                                                                               
         GOTO1 DOTRACE,DMCB,SRTREC,SRTCHECK-SRTREC,=C'TOT PUT',7,=C'P'          
*                                                                               
         LA    R5,TOTLEN(R5)       BUMP TO NEXT TOTTAB ENTRY                    
         BCT   R4,TS10             REPEAT UNTIL NO MORE ENTRIES                 
*                                                                               
TSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE BUILDS THE SORT KEY FROM THE DATA IN THE SORT RECORD.            
*                                                                               
BLDSORT  NTR1                                                                   
*                                  PRE-CLEAR SORT KEY                           
         XC    SRTKEY(SRTKEYL),SRTKEY                                           
*                                                                               
         OC    SRTNAME,SRTNAME     IF NO NAME IN SORT RECORD                    
         BNZ   *+8                                                              
         MVI   SRTKTOT,1           THEN SET TOTALS INDICATOR IN SORTKEY         
*                                                                               
         MVC   SRTKCUR,SRTCUR      SAVE CURRENCY IN SORT KEY                    
         MVC   SRTKEOR,SRTEOR           EMPLOYER                                
         MVC   SRTKSEQ,SRTSEQ           CHECK ORDER PROCESSED NUMBER            
*                                                                               
         CLI   REQSORT,C'N'        IF SORTING BY NAME                           
         BNE   BS10                                                             
         MVC   SRTKNAM,SRTNAME     SAVE NAME IN SORT KEY                        
         MVC   SRTKDTE,SRTDTE           CHECK DATE                              
         B     BSX                                                              
*                                                                               
BS10     MVC   SRTKAGY,SRTAGY      ELSE SAVE AGENCY IN SORT KEY                 
         MVC   SRTKINV,SRTINV                INVOICE                            
*                                                                               
BSX      B     XIT                                                              
         EJECT                                                                  
* CHECKS PASS2 - READ BACK THE SORT RECORDS AND PRINT A CHECK BOX FOR           
* EACH ONE.                                                                     
*                                                                               
CHKPASS2 NTR1                                                                   
         XC    SVCUR,SVCUR         CLEAR SAVED CURRENCY                         
         XC    SVEOR,SVEOR         CLEAR SAVED EMPLOYER                         
         XC    SVAGY,SVAGY         CLEAR SAVED AGENCY                           
         XC    SVINV,SVINV         CLEAR SAVED INVOICE                          
         MVI   LINE,X'99'          FORCE FIRST PAGE EJECT                       
*                                                                               
CPT10    LA    R2,15               RESET CHECKS BUFFER COUNTER                  
         BAS   RE,INITBUF          INITIALIZE PRINT BUFFER                      
*                                                                               
*                                  GET NEXT SORT RECORD                         
CPT20    GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    CPT100              DONE IF NO RETURN ADDRESS                    
*                                                                               
         L     R0,DMCB+4           MOVE SORT RECORD TO SRTREC                   
         LHI   R1,SRTRECL                                                       
         LA    RE,SRTREC                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 DOTRACE,DMCB,SRTREC,SRTCHECK-SRTREC,=C'SORT GET',8,=C'G'         
*                                                                               
         GOTO1 DOTRACE,DMCB,ASRTCHK,0,=C'SRTCHECK',8,=C'G'                      
         MVI   LINE,X'99'                                                       
*                                                                               
         CLI   SRTKTOT,1           IF RECORD IS A TOTAL RECORD                  
         BNE   CPT50                                                            
*                                                                               
         C     R2,=F'15'           THEN IF NOT FIRST CHECK OF BUFFER            
         BE    *+8                                                              
         BAS   RE,PRPAGE           THEN PRINT CHECKS BUFFER TO PAGE             
*                                                                               
         MVC   SVCUR,SRTCUR        SAVE CURRENCY                                
         MVC   SVEOR,SRTEOR             EMPLOYER                                
         MVC   SVAGY,SRTAGY             AGENCY                                  
         MVC   SVINV,SRTINV             INVOICE                                 
*                                                                               
         MVI   TOTPAGE,C'Y'        SET TOTALS PAGE INDICATOR                    
*                                                                               
         LA    R2,15               RESET CHECKS BUFFER COUNTER                  
         BAS   RE,INITBUF          INITIALIZE PRINT BUFFER                      
         BAS   RE,PRCHECK          PRINT TOTALS CHECK TO BUFFER                 
         BAS   RE,PRPAGE           PRINT BUFFER TO REPORT PAGE                  
*                                                                               
         MVI   TOTPAGE,C'N'        RESET TOTALS PAGE INDICATOR                  
*                                                                               
         B     CPT10               LOOP BACK TO START OF PAGE                   
*                                                                               
CPT50    MVC   SVCUR,SRTCUR        ELSE SAVE CURRENCY                           
         MVC   SVEOR,SRTEOR                  EMPLOYER                           
         MVC   SVAGY,SRTAGY                  AGENCY                             
         MVC   SVINV,SRTINV                  INVOICE                            
*                                                                               
         BAS   RE,PRCHECK          PRINT CHECK TO BUFFER                        
*                                                                               
         BCT   R2,CPT20            REPEAT UNTIL END OF PAGE                     
*                                                                               
         BAS   RE,PRPAGE           PRINT BUFFER TO REPORT PAGE                  
*                                                                               
         B     CPT10               LOOP BACK TO START OF PAGE                   
*                                                                               
CPT100   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
CPTX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE INITIALIZES THE PRINT BUFFER TO HAVE BLANK LINES WITH            
* ROW HEADINGS TO THE LEFT.                                                     
*                                                                               
INITBUF  NTR1                                                                   
         LA    RE,PRBUFFER         SET ENTIRE BUFFER TO SPACES                  
         L     RF,=A(51*132)                                                    
         ICM   R1,15,=X'40000000'                                               
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,PRBUFFER         INSERT HEADINGS FOR FIRST ROW OF             
         MVC   1(4,R3),=C'NAME'        BOXES                                    
         LA    R3,132(R3)                                                       
         MVC   1(11,R3),=C'SSN/SOR/SOW'                                         
         LA    R3,132(R3)                                                       
         MVC   1(15,R3),=C'AGY/CLI/UNI/AGT'                                     
         LA    R3,132(R3)                                                       
         MVC   1(15,R3),=C'INV NUM/CHK NUM'                                     
         LA    R3,132(R3)                                                       
         MVC   1(13,R3),=C'TAXABLE GROSS'                                       
         LA    R3,132(R3)                                                       
         MVC   1(11,R3),=C'NON TAXABLE'                                         
         LA    R3,132(R3)                                                       
         MVC   1(11,R3),=C'FEDERAL TAX'                                         
         LA    R3,132(R3)                                                       
         MVC   1(4,R3),=C'FICA'                                                 
         LA    R3,132(R3)                                                       
         MVC   1(9,R3),=C'STATE TAX'                                            
         LA    R3,132(R3)                                                       
         MVC   1(7,R3),=C'SDI/SUI'                                              
         LA    R3,132(R3)                                                       
         MVC   1(7,R3),=C'GST/HST'                                              
         LA    R3,132(R3)                                                       
         MVC   1(9,R3),=C'LOCAL TAX'                                            
         LA    R3,132(R3)                                                       
         MVC   1(12,R3),=C'CANADIAN TAX'                                        
         LA    R3,132(R3)                                                       
         MVC   1(8,R3),=C'AGY COMM'                                             
         LA    R3,132(R3)                                                       
         MVC   1(5,R3),=C'OTHER'                                                
         LA    R3,132(R3)                                                       
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTALS PAGE                               
         BNE   *+8                                                              
         LA    R3,8*132(R3)        THEN LEAVE 8 EXTRA LINES FOR OTHERS          
*                                                                               
         MVC   1(13,R3),=C'TOTAL DED/NET'                                       
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTALS PAGE                               
         BE    IBX                 THEN RETURN                                  
*                                                                               
         LA    RE,PRBUFFER         COPY HEADINGS FOR FIRST ROW OF BOXES         
         LR    R0,RE                   TO SECOND ROW OF BOXES                   
         A     RE,=A(17*132)                                                    
         L     RF,=A(17*132)                                                    
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,PRBUFFER         COPY HEADINGS FOR FIRST ROW OF BOXES         
         LR    R0,RE                   TO THIRD ROW OF BOXES                    
         A     RE,=A(34*132)                                                    
         L     RF,=A(17*132)                                                    
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
IBX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PRINTS A CHECK TO THE CHECKS BUFFER, PRBUFFER.  IT USES          
* R2, THE CHECK COUNTER, TO POSITION THE CHECK WITHIN THE BUFFER.               
*                                                                               
PRCHECK  NTR1                                                                   
         SR    R0,R0               THIS CODE POSTITIONS R3 TO THE TOP           
         LA    R1,15                   LEFT CORNER OF THE BOX WHERE THE         
         SR    R1,R2                   CHECK WILL GO WITHIN THE CHECKS          
         D     R0,=F'5'                BUFFER (LOTS OF UNPLEASENT MATH)         
         LR    RF,R1                                                            
         M     RE,=A(17*132)                                                    
         LR    R3,RF                                                            
         LR    RF,R0                                                            
         M     RE,=F'23'                                                        
         LA    R3,17(R3,RF)                                                     
         LA    R3,PRBUFFER(R3)                                                  
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTAL RECORD                              
         BNE   PRC10               THEN PRINT 'TOTAL' FOR NAME                  
         MVC   0(16,R3),=CL16'TOTAL'                                            
         GOTO1 PRADJS,DMCB,1       PRINT ADJUSTMENT INDICATOR IF ANY            
*                                                                               
         CLI   REQSORT,C'N'        IF SORTING BY NAME                           
         BNE   PC5                                                              
         LA    R3,4*132(R3)        THEN SKIP TO AMOUNTS                         
         B     PRC20                                                            
*                                                                               
PC5      LA    R3,3*132(R3)        ELSE SKIP TO AGENCY                          
*                                                                               
         CLC   SRTAGY,XFFS         IF AGENCY IS X'FF'S                          
         BNE   PC6                                                              
         MVC   0(3,R3),=C'ALL'     THEN PRINT 'ALL' FOR AGENCY                  
         B     PC7                                                              
*                                                                               
PC6      MVC   0(6,R3),SRTAGY      ELSE PRINT AGENCY                            
*                                                                               
PC7      LA    R3,132(R3)          BUMP TO NEXT LINE                            
*                                                                               
         CLC   SRTINV,XFFS         IF INVOICE IS X'FF'S                         
         BNE   PC8                                                              
         MVC   0(3,R3),=C'ALL'     THEN PRINT 'ALL' FOR INVOICE                 
         B     PC9                                                              
*                                                                               
PC8      OC    SRTINV,SRTINV       ELSE IF INVOICE NUMBER NOT ZEROS             
         BZ    PC9                                                              
*                                  THEN PRINT INVOICE NUMBER                    
         GOTO1 TINVCON,DMCB,SRTINV,0(R3),DATCON                                 
*                                                                               
PC9      LA    R3,132(R3)          BUMP TO AMOUNTS                              
         B     PRC20                                                            
*                                                                               
PRC10    MVC   BLOCK(44),SPACES    ELSE FILL BLOCK WITH SPACES                  
*                                                                               
         CLI   SRTTYPE,C'C'        IF PERFORMER TYPE IS CORP                    
         BNE   PRC12               CHOP CORP NAME INTO 22 BYTE BLOCKS           
         GOTO1 CHOPPER,DMCB,(32,SRTNAME),(22,BLOCK),2                           
         B     PRC15                                                            
*                                                                               
PRC12    MVC   BLOCK(16),SRTNAME   ELSE MOVE FIRST AND LAST TO BLOCK            
         MVC   BLOCK+22(16),SRTNAME+16                                          
*                                                                               
PRC15    GOTO1 SQUASHER,DMCB,BLOCK,44 PRINT NAME                                
         MVC   0(22,R3),BLOCK                                                   
         LA    R3,132(R3)                                                       
*                                                                               
         MVC   0(9,R3),SRTSSN      SSN                                          
         MVC   14(3,R3),SRTSOR     STATE OF RESIDENCE                           
         MVC   18(3,R3),SRTSOW     STATE OF WORK                                
         LA    R3,132(R3)                                                       
*                                                                               
         MVC   0(6,R3),SRTAGY      AGENCY                                       
         MVC   7(6,R3),SRTCLI      CLIENT                                       
         MVC   14(3,R3),SRTUN      UNION                                        
*                                                                               
         OC    SRTNCDE,SRTNCDE     IF AGENT IS NON-ZERO                         
         BZ    PRC17                                                            
*                                  PRINT AGENT                                  
         GOTO1 TRNSAGT,DMCB,(X'40',SRTNCDE),18(R3)                              
*                                                                               
PRC17    LA    R3,132(R3)                                                       
*                                  INVOICE NUMBER/CHECK NUMBER                  
         GOTO1 TINVCON,DMCB,SRTINV,0(R3),DATCON                                 
         MVC   14(8,R3),SRTCHK                                                  
         GOTO1 PRADJS,DMCB,2       PRINT ADJUSTMENT INDICATOR IF ANY            
         LA    R3,132(R3)                                                       
*                                  TAXABLE EARNINGS/YTD TAXABLE EARN.           
PRC20    GOTO1 EDITAMNT,DMCB,(1,SRTEARN),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYEARN),1                                     
         LA    R3,132(R3)                                                       
*                                  NON-TAXABLE EARN./YTD NON-TAXABLE            
         GOTO1 EDITAMNT,DMCB,(1,SRTNTAX),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYNTAX),1                                     
         LA    R3,132(R3)                                                       
*                                  FEDERAL TAX/YTD FEDERAL TAX                  
         GOTO1 EDITAMNT,DMCB,(1,SRTFTAX),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYFED),1                                      
         LA    R3,132(R3)                                                       
*                                  FICA/YTD FICA                                
         GOTO1 EDITAMNT,DMCB,(1,SRTFICA),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYFIC),1                                      
         LA    R3,132(R3)                                                       
*                                  STATE TAX/YTD STATE TAX                      
         GOTO1 EDITAMNT,DMCB,(1,SRTSTAX),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYSTA),1                                      
         LA    R3,132(R3)                                                       
*                                                                               
         CLC   SRTSSN,=C'801252029'                                             
         BNE   *+8                                                              
         B     *+4                                                              
*                                                                               
         L     R5,SRTSDI           SDI+SUI/YTD SDI+SUI                          
         A     R5,SRTSUI                                                        
         ST    R5,FULL                                                          
         GOTO1 EDITAMNT,DMCB,(1,FULL),0                                         
         L     R5,SRTYSDI                                                       
         A     R5,SRTYSUI                                                       
         ST    R5,FULL                                                          
         GOTO1 EDITAMNT,DMCB,(2,FULL),1                                         
         LA    R3,132(R3)                                                       
*                                  GST/YTD GST                                  
         GOTO1 EDITAMNT,DMCB,(1,SRTGST),0                                       
         GOTO1 EDITAMNT,DMCB,(2,SRTYGST),1                                      
         LA    R3,132(R3)                                                       
*                                  LOCAL TAX/YTD LOCAL TAX                      
         GOTO1 EDITAMNT,DMCB,(1,SRTLTAX),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYLOC),1                                      
         LA    R3,132(R3)                                                       
*                                  CANADIAN TAX/YTD CANADIAN TAX                
         GOTO1 EDITAMNT,DMCB,(1,SRTCTAX),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYCAN),1                                      
         LA    R3,132(R3)                                                       
*                                  AGENCY COMMISSION                            
         GOTO1 EDITAMNT,DMCB,(1,SRTACOM),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTYACOM),1                                     
         LA    R3,132(R3)                                                       
*                                                                               
         LA    R4,1                R4 = NUMBER OF LINES FOR OTHER DEDS          
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTALS PAGE                               
         BNE   *+8                                                              
         LA    R4,8(R4)            THEN USE 8 EXTRA LINE                        
*                                                                               
         OC    SRTDUED,SRTDUED     IF DUE COMPANY DEBIT APPLIED                 
         BZ    PRC30                                                            
         MVC   0(6,R3),=C'DUE DR'  THEN PRINT 'DUE DR     XXX.XX'               
         GOTO1 EDITAMNT,DMCB,(2,SRTDUED),0                                      
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC30    OC    SRTDUEC,SRTDUEC     IF DUE COMPANY CREDIT APPLIED                
         BZ    PRC40                                                            
         MVC   0(6,R3),=C'DUE CR'  THEN PRINT 'DUE CR     XXX.XX'               
         GOTO1 EDITAMNT,DMCB,(2,SRTDUEC),0                                      
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC40    OC    SRTLIEND,SRTLIEND   IF LIEN DEBIT APPLIED                        
         BZ    PRC45                                                            
         MVC   0(7,R3),=C'LIEN DR' THEN PRINT 'LIEN DR    XXX.XX'               
         GOTO1 EDITAMNT,DMCB,(2,SRTLIEND),0                                     
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC45    OC    SRTLIENC,SRTLIENC   IF LIEN CREDIT APPLIED                       
         BZ    PRC48                                                            
         MVC   0(7,R3),=C'LIEN CR' THEN PRINT 'LIEN CR    XXX.XX'               
         GOTO1 EDITAMNT,DMCB,(2,SRTLIENC),0                                     
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC48    OC    SRTLIENE,SRTLIENE   IF LIEN EFT APPLIED                          
         BZ    PRC50                                                            
         MVC   0(8,R3),=C'LIEN EFT' THEN PRINT 'LIEN EFT   XXX.XX'              
         GOTO1 EDITAMNT,DMCB,(2,SRTLIENE),0                                     
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC50    OC    SRTTRSTD,SRTTRSTD   IF W4 TRUSTEE DEBIT APPLIED                  
         BZ    PRC55                                                            
         MVC   0(8,R3),=C'TRUST DR' THEN PRINT 'TRUST DR XXX.XX'                
         GOTO1 EDITAMNT,DMCB,(2,SRTTRSTD),0                                     
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC55    OC    SRTTRSTC,SRTTRSTC   IF W4 TRUSTEE CREDIT APPLIED                 
         BZ    PRC60                                                            
         MVC   0(8,R3),=C'TRUST CR' THEN PRINT 'TRUST CR XXX.XX'                
         GOTO1 EDITAMNT,DMCB,(2,SRTTRSTC),0                                     
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC60    OC    SRTMDED,SRTMDED     IF MISC DEDUCTIONS/UNION DUES TAKEN          
         BZ    PRC65               THEN PRINT 'MISC DED  XXX.XX'                
         MVC   0(8,R3),=C'MISC DED'                                             
         GOTO1 EDITAMNT,DMCB,(2,SRTMDED),0                                      
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC65    OC    SRTDIRCT,SRTDIRCT   IF DIRECT DEPOSIT                            
         BZ    PRC68               THEN PRINT 'DIRECT  XXX.XX'                  
         MVC   0(6,R3),=C'DIRECT'                                               
         GOTO1 EDITAMNT,DMCB,(2,SRTDIRCT),0                                     
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC68    OC    SRTWIRE,SRTWIRE     IF WIRE TRANSFER                             
         BZ    PRC70               THEN PRINT 'WIRE    XXX.XX'                  
         MVC   0(4,R3),=C'WIRE'                                                 
         GOTO1 EDITAMNT,DMCB,(2,SRTWIRE),0                                      
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC70    OC    SRTMPT,SRTMPT       IF MPTRF DEDUCTED                            
         BZ    PRC80                                                            
         MVC   0(5,R3),=C'MPTRF'   THEN PRINT 'MPTRF  XXX.XX'                   
         GOTO1 EDITAMNT,DMCB,(2,SRTMPT),0                                       
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC80    OC    SRTCHAR,SRTCHAR     IF PERMANENT CHARITIES DEDUCTED              
         BZ    PRC90               THEN PRINT 'PERM CHAR  XXX.XX'               
         MVC   0(9,R3),=C'PERM CHAR'                                            
         GOTO1 EDITAMNT,DMCB,(2,SRTCHAR),0                                      
         LA    R3,132(R3)                                                       
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    PRC100                                                           
*                                                                               
PRC90    LA    R3,132(R3)          BLANK OUT REMAINDER OF OTHER DEDS            
         BCT   R4,PRC90                                                         
*                                  TOTAL DEDUCTIONS/NET CHECK AMOUNT            
PRC100   GOTO1 EDITAMNT,DMCB,(1,SRTTDED),0                                      
         GOTO1 EDITAMNT,DMCB,(2,SRTAMNT),0                                      
         LA    R3,132(R3)                                                       
*                                                                               
PRCX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PRINTS AN INDICATOR ON THE CHECK AS TO WHICH TYPE OF             
* PAYROLL ADJUSTMENT THE CHECK IS.  IF THE CHECK IS NOT AN ADJUSTMENT           
* THE ROUTINE DOES NOTHING.  OTHERWISE IT PRINTS THE APPROPRIATE                
* MESSAGE IN THE LOCATION SPECIFIED BY PARAMETER 1 (1 = PRINT AFTER             
* 'TOTAL' IN NAME FIELD, 2 = PRINT BEFORE CHECK NUMBER).                        
* R3 POINTS TO THE PLACE IN THE BUFFER TO PRINT.                                
*                                                                               
PRADJS   NTR1                                                                   
         CLI   SRTADJS,0           IF NOT PAYROLL ADJ THEN RETURN               
         BE    PRAX                                                             
*                                                                               
         TM    SRTADJS,TAPDADVD    IF VOID CHECK                                
         BZ    PRA20                                                            
*                                                                               
         CLI   3(R1),1             THEN IF PARM 1 IS 1                          
         BNE   PRA10                                                            
         MVC   6(5,R3),=C'VOIDS'   THEN PRINT 'VOIDS' IN NAME FIELD             
         B     PRAX                                                             
*                                                                               
PRA10    MVI   13(R3),C'V'         ELSE PRINT 'V' BEFORE CHECK NUMBER           
         B     PRAX                                                             
*                                                                               
PRA20    TM    SRTADJS,TAPDADRI    ELSE IF REISSUE CHECK                        
         BZ    PRA40                                                            
*                                                                               
         CLI   3(R1),1             THEN IF PARM 1 IS 1                          
         BNE   PRA30                                                            
         MVC   6(8,R3),=C'REISSUES'   THEN PRINT 'REISSUES' IN NAME FLD         
         B     PRAX                                                             
*                                                                               
PRA30    MVI   13(R3),C'R'         ELSE PRINT 'R' BEFORE CHECK NUMBER           
         B     PRAX                                                             
*                                                                               
PRA40    TM    SRTADJS,TAPDADIS    ELSE IF ISSUE (MANUAL) CHECK                 
         BZ    PRA60                                                            
*                                                                               
         CLI   3(R1),1             THEN IF PARM 1 IS 1                          
         BNE   PRA50                                                            
         MVC   6(6,R3),=C'ISSUES'     THEN PRINT 'ISSUES' IN NAME FLD           
         B     PRAX                                                             
*                                                                               
PRA50    MVI   13(R3),C'I'         ELSE PRINT 'I' BEFORE CHECK NUMBER           
         B     PRAX                                                             
*                                                                               
PRA60    TM    SRTADJS,TAPDADTR    ELSE IF TAX REFUND CHECK                     
         BZ    PRA80                                                            
*                                                                               
         CLI   3(R1),1             THEN IF PARM 1 IS 1                          
         BNE   PRA70                                                            
         MVC   6(11,R3),=C'TAX REFUNDS'    THEN PRINT 'TAX REFUNDS'             
         B     PRAX                                                             
*                                                                               
PRA70    MVI   13(R3),C'T'         ELSE PRINT 'T' BEFORE CHECK NUMBER           
         B     PRAX                                                             
*                                                                               
PRA80    TM    SRTADJS,TAPDADTT    ELSE IF TAX TRANSFER CHECK                   
         BZ    PRA100                                                           
*                                                                               
         CLI   3(R1),1             THEN IF PARM 1 IS 1                          
         BNE   PRA90                                                            
         MVC   6(13,R3),=C'TAX TRANSFERS'   THEN PRINT 'TAX TRANSFERS'          
         B     PRAX                                                             
*                                                                               
PRA90    MVI   13(R3),C'X'         ELSE PRINT 'X' BEFORE CHECK NUMBER           
         B     PRAX                                                             
*                                                                               
PRA100   TM    SRTADJS,TAPDADST    ELSE IF SSN TRANSFER CHECK                   
         BZ    PRA120                                                           
*                                                                               
         CLI   3(R1),1             THEN IF PARM 1 IS 1                          
         BNE   PRA110                                                           
         MVC   6(13,R3),=C'SSN TRANSFERS'   THEN PRINT 'SSN TRANSFERS'          
         B     PRAX                                                             
*                                                                               
PRA110   MVI   13(R3),C'S'         ELSE PRINT 'S' BEFORE CHECK NUMBER           
         B     PRAX                                                             
*                                                                               
PRA120   DS    0H                                                               
*                                                                               
PRAX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE EDITS THE AMOUNT PASSED INTO THE CORRECT PLACE AND FOR           
* THE CORRECT WIDTH DEPENDING ON THE INPUT PARAMETERS:                          
*                                                                               
*    PARM 1 - AMOUNT                                                            
*             BYTE 0 = COLUMN NUMBER (1/2)                                      
*    PARM2  - 0 MEANS ALWAYS EDIT THIS AMOUNT                                   
*             1 MEANS DON'T EDIT THIS AMOUNT IF THIS IS A TOTALS PAGE           
*                                                                               
EDITAMNT NTR1                                                                   
         CLI   7(R1),1             IF OPTION TO SKIP TOTALS PAGE                
         BNE   EA5                                                              
         CLI   TOTPAGE,C'Y'        AND WE ARE ON A TOTALS PAGE                  
         BE    EAX                 THEN RETURN                                  
*                                                                               
EA5      L     R2,0(R1)            R2 = A(AMOUNT)                               
*                                                                               
         CLI   0(R1),2             IF COLUMN 2                                  
         BNE   EA10                                                             
         LA    R3,11(R3)           THEN BUMP R3 TO COLUMN 2                     
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTALS PAGE                               
         BNE   EA10                                                             
         LA    R3,5(R3)            THEN BUMP R3 BY 5                            
*                                                                               
EA10     LA    R4,10               R4 = WIDTH = 10                              
*                                                                               
         CLI   0(R1),0             IF COLUMN 2                                  
         BE    *+8                                                              
         LA    R4,11               THEN R4 = 11                                 
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTALS PAGE                               
         BNE   *+8                                                              
         LA    R4,5(R4)            THEN BUMP R4 BY 5                            
*                                                                               
*                                  EDIT INTO LARGEST POSSIBLE WIDTH             
         EDIT  (4,0(R2)),(16,BLOCK),2,ALIGN=RIGHT,MINUS=Y,FLOAT=-               
*                                                                               
         LA    RF,16               RF = A(EDITED AMOUNT)                        
         SR    RF,R4                                                            
         LA    RF,BLOCK(RF)                                                     
*                                                                               
         BCTR  R4,0                MOVE EDITED AMOUNT TO OUTPUT AREA            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RF)                                                    
*                                                                               
EAX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PRINTS THE CONTENTS OF THE PRINT BUFFER, PRBUFFER, TO            
* THE PRINTER.  IT USES BOXES TO SEPARATE THE CHECKS FROM ONE ANOTHER.          
*                                                                               
PRPAGE   NTR1                                                                   
         L     R3,ABOX             R3 = A(BOX DEFINITION DSECT)                 
         USING BOXD,R3                                                          
*                                                                               
         MVI   BOXYORN,C'Y'        INITIALIZE BOX CONTROL VARIABLES             
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         MVC   BOXROWS,SPACES      PRE-CLEAR ROW AND COLUMN DEFINITIONS         
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTALS PAGE                               
         BNE   PRP10                                                            
         MVI   BOXROWS+5,C'T'      DEFINE TOP LINE OF TOTALS PAGE               
         MVI   BOXROWS+30,C'B'            BOTTOM LINE                           
         MVI   BOXCOLS,C'L'               LEFT LINE                             
         MVI   BOXCOLS+16,C'C'            CENTER LINE                           
         MVI   BOXCOLS+49,C'R'            RIGHT LINE                            
         B     PRP20                                                            
*                                                                               
PRP10    MVI   BOXROWS+5,C'T'      ELSE DEFINE TOP LINE OF NORMAL PAGE          
         MVI   BOXROWS+22,C'M'                 MIDDLE LINES                     
         MVI   BOXROWS+39,C'M'                                                  
         MVI   BOXROWS+56,C'B'                 BOTTOM LINE                      
         MVI   BOXCOLS,C'L'                    LEFT LINE                        
         MVI   BOXCOLS+16,C'C'                 CENTER LINES                     
         MVI   BOXCOLS+39,C'C'                                                  
         MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+85,C'C'                                                  
         MVI   BOXCOLS+108,C'C'                                                 
         MVI   BOXCOLS+131,C'R'                RIGHT LINE                       
*                                                                               
PRP20    BAS   RE,CALLSPL          PRINT TOP OF BOX                             
*                                                                               
         LA    R2,PRBUFFER         R2 = A(FIRST LINE IN PRINTER BUFFER)         
         LA    R4,51               R4 = NUMBER OF LINES IN PRBUFFER             
*                                                                               
         CLI   TOTPAGE,C'Y'        IF TOTALS PAGE                               
         BNE   *+8                                                              
         LA    R4,25               ONLY PRINT 25 LINES                          
*                                                                               
PRP30    MVC   P,0(R2)             MOVE PRINT LINE TO P AND PRINT               
         BAS   RE,CALLSPL                                                       
         LA    R2,132(R2)                                                       
         BCT   R4,PRP30            REPEAT UNTIL NO MORE PRINT LINES             
*                                                                               
         MVC   BOXROWS,SPACES      CLEAR ROW AND COLUMN DEFINITIONS             
         MVC   BOXCOLS,SPACES                                                   
*                                                                               
         MVI   LINE,99             FORCE PAGE EJECT                             
*                                                                               
PRPX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE WILL PASS THE FIRST 4 PARAMETERS ALONG TO 'TRACE' IF             
* THE BYTE POINTED TO BY PARM 5 IS ONE OF THE REQUESTED TRACE BYTES.            
*                                                                               
DOTRACE  NTR1                                                                   
         L     RF,16(R1)           RF = A(TRACE BYTE ARGUMENT)                  
*                                                                               
         LA    R0,60               TEST 60 TRACE BYTES AGAINST ARGUMENT         
         LA    RE,REQTRACE                                                      
*                                                                               
DT10     CLI   0(RE),C' '          IF END OF TRACE BYTES THEN DONE              
         BNH   DTX                                                              
         CLC   0(1,RE),0(RF)       IF MATCH THEN DO TRACE                       
         BE    DT20                                                             
         LA    RE,1(RE)            ELSE BUMP TO NEXT TRACE BYTE                 
         BCT   R0,DT10                                                          
*                                                                               
         B     DTX                 NO MATCH - RETURN                            
*                                                                               
DT20     GOTO1 TRACE               DO TRACE                                     
*                                                                               
DTX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE IS HOOKED BY SPOOL EVERY TIME THERE IS A PAGE BREAK.             
*                                                                               
HOOK     NTR1                                                                   
         MVC   HEAD3+10(3),=C'US$' PUT CURRENCY INTO HEAD3                      
         CLI   SVCUR,SRTCURCA                                                   
         BNE   *+10                                                             
         MVC   HEAD3+10(4),=C'CAN$'                                             
         CLI   SVCUR,SRTCUREU                                                   
         BNE   *+10                                                             
         MVC   HEAD3+10(4),=C'EURO'                                             
*                                                                               
         MVC   HEAD4+10(3),SVEOR   PUT EMPLOYER INTO HEAD4                      
*                                                                               
         CLC   SVEOR,=X'FFFFFF'    IF EMPLOYER 'ALL'                            
         BNE   HK10                                                             
         MVC   HEAD4+10(3),=C'ALL' THEN PUT 'ALL' INTO HEAD4                    
         B     HK20                                                             
*                                                                               
HK10     XC    BLOCK(36+8),BLOCK   ELSE PUT EMPLOYER NAME INTO HEAD4            
         MVI   BLOCK,36+8                                                       
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'88',SVEOR),BLOCK                          
         MVC   HEAD4+19(36),BLOCK+8                                             
*                                                                               
HK20     L     RF,ATWA             RF = A(TWA)                                  
         USING T703FFD,RF                                                       
         MVC   HEAD3+62(8),SPRPER  DISPLAY START DATE CENTERED                  
*                                                                               
         CLC   SPRPER(8),SPRPER+9  IF START DATE = END DATE                     
         BE    *+10                                                             
         MVC   HEAD3+58(17),SPRPER THEN CENTER ENTIRE PERIOD                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* CALCULATE CANADIAN TAX FOR P+                                                 
*                                                                               
PKCANTAX NTR1                                                                   
         L     R2,SRTCTAX                                                       
*                                                                               
         USING TAATD,R4                                                         
         L     R4,ASRTCHK                                                       
         MVI   ELCODE,TAATELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PKCANX                                                           
         CLI   TAATLEN,TAATLN2Q                                                 
         BL    PKCANX                                                           
         B     *+8                                                              
PKCAN10  BAS   RE,NEXTEL                                                        
         BNE   PKCAN20                                                          
         CLC   TAATUNIT,=C'CN '                                                 
         JNE   PKCAN15                                                          
         A     R2,TAATPP                                                        
         A     R2,TAATEI                                                        
         A     R2,TAATPIP                                                       
PKCAN15  A     R2,TAATTAX          ACCUMULATE CANADIAN TAXES                    
         B     PKCAN10                                                          
*                                                                               
PKCAN20  ST    R2,SRTCTAX                                                       
*                                                                               
         SR    R2,R2                                                            
         USING TACYD,R4                                                         
         L     R4,ASRTCHK                                                       
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PKCANX                                                           
         B     *+8                                                              
PKCAN30  BAS   RE,NEXTEL                                                        
         BNE   PKCAN40                                                          
         CLC   TACYUNIT,=C'FD '                                                 
         JE    PKCAN30                                                          
         CLC   TACYUNIT,=C'CN '                                                 
         JE    PKCAN35                                                          
         A     R2,TACYUPP                                                       
         A     R2,TACYUEI                                                       
         A     R2,TACYUPIP                                                      
PKCAN35  A     R2,TACYUTAX         ACCUMULATE CANADIAN TAXES                    
         B     PKCAN30                                                          
*                                                                               
PKCAN40  ST    R2,SRTYCAN                                                       
*                                                                               
PKCANX   B     XIT                                                              
         EJECT                                                                  
* MISCELLANEOUS CODE                                                            
*                                                                               
CALLSPL  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,59,C'PAYROLL  REGISTER'                                       
         SSPEC H1,107,REPORT                                                    
         SSPEC H1,123,PAGE                                                      
         SSPEC H2,59,C'-----------------'                                       
         SSPEC H2,107,REQUESTOR                                                 
         SSPEC H3,1,C'CURRENCY'                                                 
         SSPEC H4,1,C'EMPLOYER'                                                 
         DC    X'00'                                                            
*                                                                               
XFFS     DC    16X'FF'                                                          
CHKDIR   DC    CL8'CHKDIR'                                                      
CHKFIL   DC    CL8'CHKFIL'                                                      
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,00,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XXXX'                                  
*                                                                               
*        REPORT PAGE BUFFER                                                     
*                                                                               
PRBUFFER DS    XL(51*132)                                                       
         EJECT                                                                  
       ++INCLUDE TAREPPYRD                                                      
         EJECT                                                                  
*TAREPFFD                                                                       
*TAREPF7D                                                                       
*DDGENTWA                                                                       
*DDTWADCOND                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*CTGENFILE                                                                      
*ACGENBOTH                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF7D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
STATED   DSECT                                                                  
SMI      DS    F                   MICHIGAN                                     
SNY      DS    F                   NEW YORK                                     
SOH      DS    F                   OHIO                                         
SPA      DS    F                   PENNSYLVANIA                                 
SDET     DS    F                   DETROIT                                      
SNYC     DS    F                   NEW YORK CITY                                
SCIN     DS    F                   CINCINNATI                                   
SCLV     DS    F                   CLEVELAND                                    
SPHL     DS    F                   PHILADELPHIA                                 
SDLNQ    EQU   *-STATED                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070TAREP17   12/30/14'                                      
         END                                                                    
