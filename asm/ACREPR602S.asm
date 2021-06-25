*          DATA SET ACREPR602S AT LEVEL 025 AS OF 03/14/00                      
*PHASE ACR602A,+0                                                               
*INCLUDE QSORT                                                                  
*INCLUDE ACGETSTD                                                               
*INCLUDE PERVERT                                                                
*INCLUDE PERCALL                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'EMPLOYEE UTILIZATION REPORT'                                    
**********************************************************************          
*        REQUESTED OFF THE 1R LEDGER                                 *          
*                                                                    *          
*        JOB READS 1R AND                                            *          
*                                                                    *          
*        MOS START AND END ARE REQUIRED                              *          
*                                                                    *          
*        OPTION 1 = REPORTS                                          *          
*                 = A - DETAIL                                       *          
*                 = B - EMPLOYEE SUMMARY                             *          
*                 = C - DEPARTMENT SUMMARY (DEFAULT)                 *          
*                 = D - WEEKLY DEPARTMENT SUMMARY                    *          
*                 = E - EMPLOYEE SUMMARY + DEPARTMENT SUMMARY        *          
*                 = F - DETAIL + WEEKLY DEPARTMENT SUMMARY           *          
*                 = G - DETAIL + DEPARTMENT SUMMARY                  *          
*                 = H - DETAIL + EMPLOYEE SUMMARY                    *          
*                 = I - DEPARTMENT SUMMARY + WEEKLY DEPT SUMMARY     *          
*                 = J - DETAIL + WEEKLY DEPARTMENT METRIC            *          
*                 = K - EMPLOYEE SUMMARY + MONTHLY DEPT METRIC       *          
*                 = L - WEEKLY DEPARTMENT METRIC                     *          
*                 = M - MONTHLY DEPARTMENT METRIC                    *          
*                 = N - DETAIL + MONTHLY + WEEKLY DEPT METRIC        *          
*                 = Z - ALL REPORTS                                  *          
*                                                                    *          
*        OPTION 2 = M (DEFAULT) SHOW HOURS BASED ON MOA              *          
*                   T SHOW HOURS BASED ON TRANSACTION DATE           *          
*                                                                    *          
*        OPTION 3 = Y TO DEDUCT VACATION FROM STANDARD HOURS         *          
*                                                                    *          
*        OPTION 4 = Y TO SHOW CONTRA-ACCOUNT DATA IN DETAIL REPORT   *          
*                                                                    *          
*        OPTION 5 = B=B TIME ONLY  R=R TIME ONLY ON DEPT AND WEEKLY  *          
*                   SUMMARIES                                        *          
*                                                                    *          
*        OPTION 6 = STANDARD HOURS NUMBER OVERRIDE                   *          
*                                                                    *          
*        OPTION 7 = TARGET PERCENTAGE NUMBER OVERRIDE                *          
*                                                                    *          
**********************************************************************          
         EJECT                                                                  
ACR602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR6**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         USING ACR6D,RC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   MYMODE,MODE         SAVE MODE FROM MONACC                        
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LEDGF                                                            
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRNS                                                            
         CLI   MODE,PROCTIME                                                    
         BE    PTIME                                                            
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR RUN FIRST                                              *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RF                                                       
         LA    RF,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RF                                                               
*                                                                               
         XC    DTENUM,DTENUM       CLEAR NUMBER OF PERIODS/MONTH                
         ZAP   DMPCNT,=P'0'                                                     
         LA    RE,ATYPES                                                        
         LA    RF,RELOTAB          RELOCATE MY A TYPES                          
         MVC   0(ATYPLNQ,RE),0(RF)                                              
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         USING BOXD,R2                                                          
         L     RE,=A(HDHOOK)                                                    
         ST    RE,HEADHOOK                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
*                                                                               
         L     R0,=A(BUFFSIZE)     ACQUIRE STORAGE                              
         GETMAIN R,LV=(0)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,APERTAB          SAVE A(TABLE AREA)                           
         SR    R4,R4                                                            
         L     R5,=A(PERMAX)                                                    
         ST    R5,MAXPER                                                        
*                                                                               
         L     R1,APERTAB                                                       
         A     R1,=A(PERSIZE)                                                   
         ST    R1,ACLITAB                                                       
         L     R5,=A(CLIMAX)                                                    
         ST    R5,MAXCLI                                                        
*                                                                               
         L     R1,ACLITAB          SET CALENDER AND STD HRS KEY TABLS           
         A     R1,=A(CLISIZE)                                                   
         ST    R1,ACALKEYS                                                      
         AH    R1,=Y(GPBUFSZQ)                                                  
         ST    R1,ASTDKEYS                                                      
         AH    R1,=Y(GSBUFSZQ)                                                  
         ST    R1,ACALREC                                                       
         AH    R1,=Y(IOLNQ)                                                     
         ST    R1,ASTDHRS                                                       
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         MVC   MCUSRDMP(4),APERTAB      PRINT PERSON TABLE IN DUMP              
         MVC   MCUSRDMP+4(4),ACLITAB                                            
*                                                                               
         MVC   MCUSRDMP+8(4),ACALKEYS   AND THE 2 KEY TABLES + CAL REC          
         L     RE,ASTDHRS                                                       
         AH    RE,=Y(GSOPLENQ)                                                  
         ST    RE,ABUCKS                                                        
         AH    RE,=Y(BUCKSLN)                                                   
         ST    RE,APERBLK                                                       
         AH    RE,=Y(LPERBLK)                                                   
         STCM  RE,15,MCUSRDMP+12                                                
*                                                                               
         BAS   RE,NOT                                                           
         BAS   RE,CLI                                                           
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
*                                  STYLE DETAIL REPORT                          
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR REQUEST FIRST                                          *          
**********************************************************************          
         SPACE 1                                                                
         USING CPYRECD,RF                                                       
REQF     DS    0H                                                               
         XC    FLGCST,FLGCST       CLEAR COST SYSTEM FLAG                       
         L     RF,ADCOMP           ADDRESS OF COMPANY RECORD                    
         LA    RE,ACCORFST(RF)     BUMP TO ELEMENT                              
         USING CPYELD,RE                                                        
REQF10   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),CPYELQ        X'10' COMPANY ELEMNT                         
         BE    REQF20                                                           
         SR    R1,R1                                                            
         IC    R1,CPYLN                                                         
         AR    RF,R1                                                            
         B     REQF10                                                           
*                                                                               
REQF20   TM    CPYSTAT5,CPYSNCST   TEST FOR NEW COST                            
         BZ    *+8                                                              
         OI    FLGCST,FLGNCST      SET FLAG FOR RUNNING NEW COST                
         DROP  RE,RF                                                            
*                                                                               
         LA    R0,TARGLNQ          # OF PERCENTAGE BUCKETS                      
         LA    R1,THISSTD          INITIALIZE ALL PERCENTAGES                   
         ZAP   0(L'THISSTD,R1),=P'0'                                            
         LA    R1,L'THISSTD(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         L     RE,ACALREC          CLEAR FOR EACH REQUEST                       
         LA    RF,IOLNQ                                                         
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',1)                           
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         XC    DATESARE,DATESARE                                                
*                                                                               
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
         OC    ACMMSTR,ACMMSTR                                                  
         BZ    REQF30                                                           
         MVC   MONSTART,ACMMSTR                                                 
         MVC   WORK(2),MONSTART                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(9,MOSPRSTR)                                
         MVI   DATESARE,MOSDATES                                                
*                                                                               
REQF30   CLI   ACMMEND,X'FF'                                                    
         BE    REQF40                                                           
         MVC   MONEND,ACMMEND                                                   
         MVC   WORK(2),MONEND                                                   
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(9,MOSPREND)                                
         MVI   DATESARE,MOSDATES                                                
*                                                                               
REQF40   BAS   RE,SETBUR           SET BURSON SWITCH                            
         GOTO1 ASETCAL,DMCB,(RC)   SET CALENDER DATE FILTERS                    
*                                                                               
         OC    DATESARE,DATESARE   NEED DATES TO RUN THIS BABY                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,BLDMON           BUILD MONTH TABLE                            
         BAS   RE,GETLEVS          GET HEIRARCHY LEVEL STRUCTURE                
         GOTO1 APER,DMCB,(RC)      BUILD PERSON TABLE                           
*                                                                               
         MVI   TIMETYPE,ALLTIME    SET TIME TYPE FILTER                         
         CLI   QOPT5,C'B'                                                       
         BNE   *+8                                                              
         MVI   TIMETYPE,BTIME                                                   
         CLI   QOPT5,C'R'                                                       
         BNE   *+8                                                              
         MVI   TIMETYPE,RTIME                                                   
*                                                                               
         MVI   METRFILT,MFPROD+MFUTIL+MFBILL                                    
         CLI   QOPT8,C' '          ANY METRIC TYPE FILTERING                    
         BNH   REQF50              NOPE                                         
         CLI   QOPT8,C'B'          BILLABILITY ONLY                             
         BNE   *+8                                                              
         MVI   METRFILT,MFBILL                                                  
         CLI   QOPT8,C'U'          UTILIZATION ONLY                             
         BNE   *+8                                                              
         MVI   METRFILT,MFUTIL                                                  
         CLI   QOPT8,C'P'          PRODUCTIVITY ONLY                            
         BNE   *+8                                                              
         MVI   METRFILT,MFPROD                                                  
*                                                                               
REQF50   CLI   DATESARE,CALDATES                                                
         BNE   REQFX                                                            
         GOTO1 DATCON,DMCB,(1,CALSTR),(5,CALPRST)                               
         GOTO1 DATCON,DMCB,(1,CALEND),(5,CALPREND)                              
         GOTO1 DATCON,DMCB,(1,CALMSTR),(5,CALPRMST)                             
         GOTO1 DATCON,DMCB,(1,CALMEND),(5,CALPRMND)                             
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* GET LEDGER PROFILES, SET REPORT OPTIONS, GET BUDGET LEVELS         *          
**********************************************************************          
         SPACE 1                                                                
LEDGF    DS    0H                                                               
         SR    R3,R3                                                            
         GOTO1 ABLDPRO,DMCB,(RC),(R3)   GET PROFILE                             
         BAS   RE,SETBUR    PROFILE OPTIONS HARDCODED FOR BURSON(6/94)          
*                                                                               
         MVC   REPOPT,PRFORMAT     GET REPORT FORMAT                            
         CLI   QOPT1,C' '          TEST USER OVERRIDE                           
         BE    *+10                NO                                           
         MVC   REPOPT,QOPT1                                                     
*                                                                               
         LA    RE,REPTAB           RE=A(REPORT TABLE)                           
LEDG10   CLI   0(RE),EOF           TEST FOR EOT                                 
         BE    LEDG20                                                           
         CLC   REPOPT,0(RE)        MATCH ON OPTION 1                            
         BE    LEDG20                                                           
         LA    RE,L'REPTAB(RE)                                                  
         B     LEDG10                                                           
*                                                                               
LEDG20   MVC   REPMASK,1(RE)       EXTRACT REPORT MASK                          
*                                                                               
         MVC   DATEOPT,PRDATES     SET DATE OPTION                              
         CLI   QOPT2,C' '          TEST USER INPUT OPTION                       
         BE    *+10                                                             
         MVC   DATEOPT,QOPT2                                                    
*                                                                               
         MVC   VACOPT,PRVAC        SET VACATION OPTION                          
         CLI   QOPT3,C' '          TEST USER INPUT OPTION                       
         BE    *+10                                                             
         MVC   VACOPT,QOPT3                                                     
*                                                                               
* FIND OUT WHAT LEVEL THE USER HAS DEFINED BUDS AT                              
*                                                                               
         CLI   QOPT6,C' '          STD HRS REQUEST OVERRIDE                     
         BE    *+14                                                             
         MVC   PRSTDHR,QOPT6                                                    
         NI    PRSTDHR,X'0F'       ISOLATE BINARY VALUE                         
*                                                                               
         CLI   QOPT7,C' '          TARGET PERCENTAGE REQUEST OVERRIDE           
         BE    *+14                                                             
         MVC   PRTRGPCT,QOPT7                                                   
         NI    PRTRGPCT,X'0F'      ISOLATE BINARY VALUE                         
*                                                                               
         USING BUDD,R2                                                          
         LA    R2,BUSTDHRS         READ FOR STANDARD HOURS                      
         MVC   BUDNUM,PRSTDHR                                                   
         XC    BUDSWCH,BUDSWCH    CLEAR BUDGET SWITCH                           
         BAS   RE,SETBUD                                                        
*                                                                               
         LA    R2,BUTRGPCT         TARGET PCT                                   
         MVC   BUDNUM,PRTRGPCT                                                  
         BAS   RE,SETBUD                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL A                                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,RF                                                       
LEVAF    DS    0H                                                               
         L     RF,ADHEIRA                                                       
         MVC   NCOFFICE,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,LEVLNQA               LEVEL A INDIVIDUAL LENGTH               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   NCOFFICE(0),ACTKACT      MOVE IN OFFICE CODE                     
         GOTO1 ANEWCAL,DMCB,(RC),NCOFFICE                                       
         NI    BUDSTAT,X'FF'-GOTLEVA                                            
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL B                                                  *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         NI    BUDSTAT,X'FF'-GOTLEVB                                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL C                                                  *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         NI    BUDSTAT,X'FF'-GOTLEVC                                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCACC PROCESSING - GET BUDGET NUMBERS FOR REPORTING ACCOUNTS ONLY*          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
PACC     DS    0H                                                               
         L     R4,ADACC                                                         
         MVC   PERCODE,ACTKACT                                                  
         DROP  R4                                                               
*                                                                               
         XC    APERSON,APERSON     CLEAR PERSON TABLE ADCON                     
         L     R3,NPER                                                          
         GOTO1 BINSRCH,DMCB,PERCODE,APERTAB,(R3),PERTABL,PERKEYL,0              
         CLI   0(R1),0             TEST IF RECORD FOUND                         
         BNE   PACCX                                                            
*                                                                               
         USING PERTABD,R3                                                       
         L     R3,0(R1)            GET A(ENTRY)                                 
         XC    PERTSTAT,PERTSTAT   STATUS BYTE                                  
         MVC   PERCODE,PERREP      EXTRACT REPORTING ACCOUNT                    
         MVC   PERNAME,PERRNAME    AND ITS NAME                                 
         ST    R3,APERSON                                                       
*                                                                               
         CLC   PERACC,PERREP       TEST FOR REPORTING ACCOUNT                   
         BNE   PACCX               NO-ALL DONE                                  
         TM    BUDSTAT,GOTLEVA+GOTLEVB+GOTLEVC                                  
         BO    PACC30              HANDLE THE ACCOUNT LEVEL                     
*                                                                               
         TM    BUDSTAT,GOTLEVA     TEST IF SEARCH AT LEVEL A DONE               
         BO    PACC10              YES                                          
*                                                                               
         MVI   MYMODE,LEVAFRST                                                  
         OI    BUDSTAT,GOTLEVA                                                  
         GOTO1 GETBUD,DMCB,ADHEIRA                                              
*                                                                               
PACC10   TM    BUDSTAT,GOTLEVB                                                  
         BO    PACC20                                                           
*                                                                               
         MVI   MYMODE,LEVBFRST                                                  
         OI    BUDSTAT,GOTLEVB                                                  
         GOTO1 GETBUD,DMCB,ADHEIRB                                              
*                                                                               
PACC20   TM    BUDSTAT,GOTLEVC                                                  
         BO    PACC30                                                           
*                                                                               
         OI    BUDSTAT,GOTLEVC                                                  
         MVI   MYMODE,LEVCFRST                                                  
         GOTO1 GETBUD,DMCB,ADHEIRC                                              
*                                                                               
PACC30   MVI   MYMODE,PROCACC                                                   
         GOTO1 GETBUD,DMCB,ADACC                                                
*                                                                               
         BAS   RE,INITPERB                                                      
*                                                                               
         LA    R0,13                                                            
         LA    RE,PERSTHRS                                                      
         ZAP   0(L'PERSTHRS,RE),=P'0'                                           
         LA    RE,L'PERSTHRS(RE)                                                
         BCT   R0,*-10                                                          
*                                                                               
         LA    R2,BUSTDHRS         SEED BUDGET FIGURES INTO TABLE               
         USING BUDD,R2                                                          
*                                                                               
         CLI   BUDMODE,X'FF'       IS MODE INVALID?                             
         BE    PACCX               YES, GET OUT                                 
         TM    BUDSWCH,BUDSET      WERE BUDGET RECORDS SET?                     
         BO    *+12                  YES - SET STD HOURS                        
         TM    FLGCST,FLGNCST        NO  - ARE WE RUNNING ON NEW COST?          
         BO    PACC40                YES - CHECK STD HOURS ON NEW COST          
*                                                                               
         LA    R0,NMTHS                                                         
         LA    R1,BUDAMNT                                                       
         LA    RE,PERSTHRS                                                      
         ZAP   0(L'PERSTHRS,RE),0(L'BUDAMNT,R1)                                 
         LA    R1,L'BUDAMNT(R1)                                                 
         LA    RE,L'PERSTHRS(RE)                                                
         BCT   R0,*-14                                                          
         B     PACC80                                                           
         DROP  R2                                                               
*                                                                               
* SET PERSTHRS FROM NEW COST STD HRS RECORD                                     
*                                                                               
PACC40   LA    R6,NMTHS                                                         
         LA    R2,MONTHTAB                                                      
         L     R4,ASTDHRS                                                       
         LA    R5,PERSTHRS                                                      
PACC50   OC    0(L'MONTHTAB,R2),0(R2) IS A MONTH DEFINED IN TABLE               
         BZ    PACC60                 NOPE, DONE                                
*                                                                               
         L     R4,ADACC                                                         
         LA    R4,3(R4)            ONLY PASS 12 BYTES OF ACCOUNT                
         GOTO1 VGETSTD,DMCB,(R2),ASTDKEYS,ASTDHRS,(R4),ACALKEYS,ACALREC         
         BNE   PACC80              STD HRS NOT DEFINED                          
*                                                                               
         OI    PERTSTAT,PERTSTD                                                 
*                                                                               
         USING GSOPD,R4                                                         
         L     R4,ASTDHRS                                                       
         ZAP   0(L'PERSTHRS,R5),GSOHRS                                          
         LA    R5,L'PERSTHRS(R5)                                                
         LA    R2,L'MONTHTAB(R2)                                                
         BCT   R6,PACC50                                                        
*                                                                               
PACC60   CLI   GSONPER,8                                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* EXTRACT PERIOD DATA FROM LAST MNTH (IE WEEKLY)                                
*                                                                               
         USING GSOPERD,R2                                                       
         USING PERTABD,R3                                                       
         LA    R2,GSOPDATA                                                      
         LA    R3,PERWKSTD                                                      
         SR    R1,R1                                                            
         ICM   R1,1,GSONPER        NUMBER OF PERIODS                            
         BZ    PACC80              IF NONE SKIP                                 
PACC70   ZAP   0(L'PERWKSTD,R3),GSOPHRS                                         
         LA    R2,GSOPLN1Q(R2)                                                  
         LA    R3,L'PERWKSTD(R3)                                                
         BCT   R1,PACC70                                                        
*                                                                               
* ADJUST MONTHLY STANDARD HOURS BASED ON HIRE/TERM MONTH                        
*                                                                               
PACC80   L     R3,APERSON                                                       
         LA    RE,PERSTHRS         RE=A(PERSON STANDARD HOUR BUCKET)            
         LA    R1,MONTHTAB         R1=A(MONTH)                                  
         SR    R0,R0                                                            
         IC    R0,NMONTHS                                                       
*                                                                               
PACC90   CLC   0(L'MONTHTAB,R1),PERLOHIR TEST MONTH BEFORE HIRE MONTH           
         BL    PACC100             YES                                          
         OC    PERTERM,PERTERM                                                  
         BZ    PACC110                                                          
         CLC   0(L'MONTHTAB,R1),PERTERM TEST MONTH AFTER TERM DATE              
         BNH   PACC110             NO                                           
*                                                                               
PACC100  ZAP   0(L'PERSTHRS,RE),=P'0'                                           
*                                                                               
PACC110  LA    RE,L'PERSTHRS(RE)                                                
         LA    R1,L'MONTHTAB(R1)                                                
         BCT   R0,PACC90                                                        
*                                                                               
         USING BUDD,R2                                                          
         LA    R2,BUTRGPCT                                                      
         ZAP   PERTARG,BUDAMNT                                                  
         CLI   PRMTHTPC,C'Y'       TRG% ENTERED MONTHLY?                        
         BNE   PACCX               NO                                           
*                                                                               
         LA    R0,11               YES, GET THE REST OF THEM                    
         LA    RE,PERTARG+L'PERTARG                                             
         LA    RF,BUDAMNT+L'BUDAMNT                                             
PACC120  ZAP   0(L'PERTARG,RE),0(L'BUDAMNT,RF)                                  
         LA    RE,L'PERTARG(RE)                                                 
         LA    RF,L'BUDAMNT(RF)                                                 
         BCT   R0,PACC120                                                       
*                                                                               
PACCX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* GET DATA FROM TRANSACTION                                          *          
* AT ENTRY, APERSON=A(0) OR A(1R TABLE ENTRY FOR ACCOUNT)            *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNELD,R5                                                        
PTRNS    DS    0H                                                               
         L     R5,ADTRANS                                                       
         CLI   TRNEL,TRNELQ      THIS IS A 44 EL?                               
         BNE   PTRNX               NO, BAD TRANS                                
         CLI   TRNTYPE,49            IS TRANS TYPE 49?                          
         BNE   PTRNX                 NO                                         
*                                                                               
         OC    APERSON,APERSON                                                  
         BZ    PTRNX               NO PERSON TABLE ENTRY-SKIP IT                
*                                                                               
         BAS   RE,INITSUMP         INITIALIZE SUMPREC                           
         USING SUMRECD,R7                                                       
         LA    R7,SUMREC                                                        
         MVC   SUMPER,PERCODE      ACCOUNT CODE                                 
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
*                                                                               
         LR    R6,R5                                                            
         MVI   ELCODE,PRTELQ       X'40' - PERSONNEL RATE ELEMENT               
         BAS   RE,NEXTEL2                                                       
         BNE   PTRN10              NONE - SKIP                                  
*                                                                               
         USING PRTELD,R6                                                        
         ZAP   DUB,PRTHOUR                                                      
*                                                                               
         MVI   SUMTYPE,SUMBIL      SET TYPE OF TIME                             
         TM    PRTSTAT,PRTSBILQ                                                 
         BO    PTRN30                                                           
*                                                                               
         MVI   SUMTYPE,SUMRTE                                                   
         TM    PRTSTAT,PRTSRTEQ                                                 
         BO    PTRN30                                                           
*                                                                               
         MVI   SUMTYPE,SUMNOT      IN CASE WE PUT X'40' ELEMS                   
         TM    PRTSTAT,PRTSNOTQ    ON 1R LEDGER                                 
         BNO   PTRNX                                                            
         B     PTRN30                                                           
*                                                                               
PTRN10   LR    R6,R5                                                            
         MVI   ELCODE,SCIELQ       LOOK FOR X'50' ELEMENT                       
PTRN20   BAS   RE,NEXTEL2                                                       
         BNE   PTRNX               REAL TROUBLE                                 
         USING SCIELD,R6                                                        
         CLI   SCITYPE,C'H'        TEST FOR HOURS                               
         BNE   PTRN20              NO                                           
*                                                                               
         MVI   SUMTYPE,SUMNOT      ONLY SB HERE IF ITS NOT-CHARGEABLE           
         ZAP   DUB,SCIAMNT         GET HOURS AMOUNT                             
*                                                                               
PTRN30   TM    FLGCST,FLGNCST      ARE WE RUNNING ON NEW COST?                  
         BZ    PTRN40              NO - SKIP NEXT ROUTINE                       
*                                                                               
         ZAP   PBDHRS,DUB                                                       
         MVC   PBDDATE,TRNDATE                                                  
         MVC   PBDMONTH,ACMMDTE                                                 
         BAS   RE,PUTBYDAT         SEED HOURS INTO SUMREC                       
         B     PTRN100                                                          
*                                                                               
PTRN40   MVC   HALF,ACMMDTE        USE MOA FOR POSTING                          
         CLI   DATEOPT,C'T'        TEST FOR TRANSACTION DATE OVERRIDE           
         BNE   *+10                                                             
         MVC   HALF,TRNDATE                                                     
*                                                                               
         CLC   HALF,MONEND         TEST TRANSACTION IS IN LAST MONTH            
         BNE   PTRN70              NO                                           
*                                                                               
         LA    RE,SUMWEEK          RE=A(WEEKLY BUCKET)                          
         LA    R1,DATETAB          R1=A(WEEK END DATES)                         
         SR    R0,R0                                                            
         ICM   R0,1,DTENUM         NUMBER OF PERIODS IN MONTH                   
         BNZ   *+8                                                              
         LA    R0,NDATES           DEFAULT                                      
*                                                                               
PTRN50   CLC   TRNDATE,0(R1)                                                    
         BNH   PTRN60                                                           
         LA    RE,L'SUMWEEK(RE)                                                 
         LA    R1,L'DATETAB(R1)                                                 
         BCT   R0,PTRN50                                                        
*                                                                               
PTRN60   AP    0(L'SUMWEEK,RE),DUB                                              
*                                                                               
PTRN70   SR    R0,R0                                                            
         IC    R0,NMONTHS                                                       
         LA    RE,MONTHTAB         NOW FIND THE MONTH TO POST AGAINST           
         LA    R4,SUMMONTH                                                      
*                                                                               
PTRN80   CLC   HALF,0(RE)                                                       
         BE    PTRN90                                                           
         LA    RE,L'MONTHTAB(RE)                                                
         LA    R4,L'SUMMONTH(R4)                                                
         BCT   R0,PTRN80                                                        
*                                                                               
         CLI   DATEOPT,C'T'        TEST TRANSACTION DATE OPTION                 
         BE    PTRNX               SKIP ITEM                                    
         DC    H'0'                DUMP FOR MOA OPTION                          
*                                                                               
PTRN90   AP    0(L'SUMMONTH,R4),DUB                                             
*                                                                               
PTRN100  LR    R6,R5                                                            
         MVI   ELCODE,PCIELQ       X'51' -  PROJECT CONTROL INFO ELEM           
         BAS   RE,NEXTEL2          FIND THE CLIENT OR 1N A/C                    
         BNE   PTRN120             IF NO 51 EL, TRY THE CONTRA A/C              
*                                                                               
         USING PCIELD,R6                                                        
         MVC   SUMACC,SPACES                                                    
         MVC   SUMACC(3),PCICLI+3                                               
         CLC   =C'SJ97',PCICLI+1  HARD CODE FUDGE FOR H+K                       
         BE    *+14                NEW BUSINESS AND PROBONO                     
         CLC   =C'SJ98',PCICLI+1                                                
         BNE   PTRN110                                                          
         CLI   SUMTYPE,SUMRTE      TEST IF ENTERED AS 'R' TIME                  
         BNE   *+8                 NO                                           
         MVI   SUMTYPE,SUMNOT      YES-MAKE THEM NON-CHARGEABLE                 
*                                                                               
PTRN110  CLI   SUMTYPE,SUMNOT                                                   
         BNE   PTRN130                                                          
         MVC   SUMACC(4),PCICLI+2 INCLUDE LEDGER VALUE                          
         CLC   =C'1N',PCICLI+1    CHECK FOR POSTING VS 1N                       
         BNE   *+10                                                             
         MVC   SUMACC,PCICLI+2    TAKE WHOLE KEY                                
         B     PTRN130                                                          
*                                                                               
* N TIME, BUT NO 51 EL, AS FROM =INP                                            
* CONTRA ACCOUNT IS 1N/PROJECT CONTROL CODE/SPACE/ACCOUNT                       
* EMULATE 'MVC SUMACC,PCICLI+2' CODE FROM ABOVE USING                           
* CONTRA ACCOUNT                                                                
*                                                                               
PTRN120  CLI   SUMTYPE,SUMNOT         IS THIS N TIME                            
         BNE   PTRNX                  NO                                        
         USING TRNRECD,R6                                                       
         LR    R6,R5                                                            
         SH    R6,DATADISP                                                      
         CLC   TRNKCUNT(2),=C'1N'     1N CONTRA ON N TIME                       
         BNE   PTRNX                                                            
         MVC   SUMACC,SPACES                                                    
         MVC   SUMACC(1),TRNKCLDG       EXTRACT CLIENT FROM C/A                 
         CLI   TRNKCACT+1,C' '          IF 2ND BYTE OF ACCOUNT IS SPACE         
         BNE   *+14                                                             
         MVC   SUMACC+1(9),TRNKCACT+2   EXTRACT ACCOUNT CODE SKIPPING           
         B     *+10                                                             
         MVC   SUMACC+1(9),TRNKCACT     DO NOT SKIP ANY OF ACCOUNT              
*                                                                               
PTRN130  BAS   RE,PUTSUMR                                                       
*                                                                               
PTRNX    B     EXIT                                                             
         DROP  R2,R5,R6,R7                                                      
         EJECT                                                                  
**********************************************************************          
* PROCESS TMS RECORDS                                                *          
**********************************************************************          
         SPACE 1                                                                
PTIME    DS    0H                                                               
         OC    APERSON,APERSON     IS THERE A PERSON RECORD                     
         BZ    EXIT                NOPE                                         
         BAS   RE,INITSUMP         INITIALIZE SUMREC STORAGE                    
         USING SUMRECD,R7                                                       
         LA    R7,SUMREC                                                        
         MVC   SUMPER,PERCODE      ACCOUNT CODE                                 
*                                                                               
         BAS   RE,FLTONCAL                                                      
         BNE   PTIMEX                                                           
*                                                                               
         USING TIMELD,R5                                                        
         L     R5,ADTRANS                                                       
         CLI   TIMEL,TIMELQ                                                     
         BNE   PTIMEX                                                           
         CLI   TIMETYP,TIMEINP     IS THIS INPUT DETAIL                         
         BNE   PTIMEX                                                           
         CLI   TIMLN,TIMILN1Q      AT LEAST INPUT LENGTH                        
         BL    PTIMEX                                                           
*                                                                               
         OI    PERSTAT,PERTS       PERSTON HAS TIMESHEET DATA                   
*                                                                               
         MVC   SUMACC,SPACES                                                    
         LA    RE,TIMACC+2         ADDRESS CLIENT IN ACCOUNT                    
         LA    RF,3                                                             
         CLI   TIMTTYP,TIMTCB      CLIENT BILLABLE TIME                         
         BNE   *+8                                                              
         MVI   SUMTYPE,SUMBIL                                                   
         CLI   TIMTTYP,TIMTCR      CLIENT REALIZATION                           
         BNE   *+8                                                              
         MVI   SUMTYPE,SUMRTE                                                   
         CLI   TIMTTYP,TIMTCN      CLIENT NON BILLABLE                          
         BNE   PTIME10                                                          
         MVI   SUMTYPE,SUMNOT                                                   
         LA    RE,TIMACC+1         SAVE LEDGER/CLIENT  (J/CLIENT)               
         LA    RF,4                                                             
PTIME10  CLI   TIMTTYP,TIMTNC      NON CLIENT                                   
         BNE   PTIME20                                                          
         MVI   SUMTYPE,SUMNOT                                                   
         LA    RE,TIMACC+1         LEDGER/ACCOUNT FOR NON CLIENT                
         LA    RF,13               (N/ACCOUNT)                                  
*                                                                               
PTIME20  BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SUMACC(0),0(RE)                                                  
*                                                                               
         CLC   =C'SJ97',TIMACC     HARD CODE FUDGE FOR H+K                      
         BE    *+14                NEW BUSINESS AND PROBONO                     
         CLC   =C'SJ98',TIMACC                                                  
         BNE   PTIME30                                                          
         CLI   SUMTYPE,SUMRTE      TEST IF ENTERED AS 'R' TIME                  
         BNE   PTIME30             NO                                           
         MVI   SUMTYPE,SUMNOT      YES-MAKE THEM NON-CHARGEABLE                 
         MVC   SUMACC,SPACES       CLEAR ACCOUNT                                
         MVC   SUMACC(4),TIMACC+1  SAVE LEDGER/CLIENT                           
*                                                                               
PTIME30  ZAP   PBDHRS,TIMHRS                                                    
         MVC   PBDMONTH,TIMMOA                                                  
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         L     R2,ACMALTN          A(TIME RECORD)                               
         USING TIMRECD,R2                                                       
         MVC   PBDDATE,TIMKPEDT                                                 
         DROP  R2,RF                                                            
*                                                                               
         BAS   RE,PUTBYDAT                                                      
         BAS   RE,PUTSUMR          PUT SUMREC TO BUFFALO (AND TOTALS)           
*                                                                               
PTIMEX   B     EXIT                                                             
         DROP  R5,R7                                                            
         EJECT                                                                  
**********************************************************************          
* ACCOUNT LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
ACCL     DS    0H                                                               
         TM    FLGCST,FLGNCST      ARE WE RUNNING ON NEW COST?                  
         BNO   ACCLX               NO, DO NOT EXCLUDE                           
*                                                                               
         USING PERD,R1                                                          
         L     R1,APERBLK                                                       
         MVC   PERDMGR,DATAMGR                                                  
         MVC   PERALEDG,ADLEDGER                                                
         MVC   PERADACC,ADACC                                                   
         GOTO1 =V(PERCALL)         GET PERSON BLOCK                             
         BAS   RE,SETINOUT                                                      
         BAS   RE,SETHIRED         SET HIRE FIRE DATES                          
*                                                                               
         TM    PERSTAT,PERTS       ARE THERE TIMESHEETS FOR THIS REQ            
         BO    ACCLX               YES DO NOT EXCLUDE                           
         CLI   DATESARE,CALDATES   IF NOT CALENDAR DATES THEN EXIT              
         BNE   ACCLX                                                            
         BAS   RE,CHKPER           IF ACCOUNT IS ACTIVE                         
         BE    ACCLX               NO TIMESHEETS IS VALID                       
         BAS   RE,DELPER           DELETE PERSON RECORD FROM APERTAB            
*                                                                               
ACCLX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT THE REPORTS AT REQLAST                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         TM    REPMASK,REPDET      TEST DETAIL REPORT NEEDED                    
         BNO   REQL10              NO                                           
         MVC   PAGE,=H'1'          FORCE START AT PAGE 1                        
         MVI   REPMODE,REPMWEEK                                                 
         MVI   RCSUBPRG,0                                                       
         BAS   RE,DETAIL                                                        
*                                                                               
REQL10   TM    REPMASK,REPEMP      TEST EMPLOYEE SUMMARY REQUIRED               
         BNO   REQL20                                                           
         MVC   PAGE,=H'1'          FORCE START AT PAGE 1                        
         MVI   RCSUBPRG,1                                                       
         MVI   REPMODE,REPMMNTH                                                 
         BAS   RE,DETAIL                                                        
*                                                                               
REQL20   TM    REPMASK,REPDPT      TEST DEPT SUMMARY REQUESTED                  
         BNO   REQL30                                                           
         MVC   PAGE,=H'1'          FORCE START AT PAGE 1                        
         MVI   REPMODE,REPMMNTH                                                 
         MVI   RCSUBPRG,2                                                       
         GOTO1 ADEPT,DMCB,(RC)                                                  
*                                                                               
REQL30   TM    REPMASK,REPWK       TEST WEEKLY DEPT SUMMARY REQUESTED           
         BNO   REQL40                                                           
         MVC   PAGE,=H'1'          FORCE START AT PAGE 1                        
         MVI   REPMODE,REPMWEEK                                                 
         MVI   RCSUBPRG,3                                                       
         GOTO1 ADEPT,DMCB,(RC)                                                  
*                                                                               
REQL40   TM    REPMASK,REPMTMT     TEST EMPLOYEE METRIC REPORT REQ              
         BNO   REQL50                                                           
         CLI   ISBURSON,C'Y'                                                    
         BNE   REQL50                                                           
         MVC   PAGE,=H'1'          FORCE START AT PAGE 1                        
         MVI   REPMODE,REPMMNTH                                                 
         MVI   RCSUBPRG,4                                                       
         GOTO1 ADEPT,DMCB,(RC)                                                  
*                                                                               
REQL50   TM    REPMASK,REPWKMT     WEEKLY EMPLOYEE METRIC REQUESTED             
         BNO   REQLX                                                            
         CLI   ISBURSON,C'Y'                                                    
         BNE   REQLX                                                            
         MVC   PAGE,=H'1'          FORCE START AT PAGE 1                        
         MVI   REPMODE,REPMWEEK                                                 
         MVI   RCSUBPRG,5                                                       
         GOTO1 ADEPT,DMCB,(RC)                                                  
*                                                                               
REQLX    BAS   RE,BXBOT            CLOSE ANY OPEN BOXES                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         L     R1,APERTAB          FREE UP ACQUIRED STORAGE                     
         L     R0,=A(BUFFSIZE)                                                  
         FREEMAIN R,A=(1),LV=(0)                                                
         LTR   RF,RF               TEST FOR GOOD RETURN CODE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
GETLEVS  NTR1                                                                   
         LA    R5,SVKEY            GET HEIRARCHY LEVELS                         
         MVC   SVKEY,SPACES                                                     
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   IOKEY(3),SVKEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         USING ACLELD,R6                                                        
         LA    R6,IO                                                            
         MVI   ELCODE,ACLELQ       X'16' - ACCOUNTS LENGTHS ELEMENT             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    LEVELS,LEVELS                                                    
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+4                                                           
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
*                                                                               
*              CONVERT 1,3,5,12 -> 1,2,2,7                                      
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GETL10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GETL20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GETL10                                                        
         B     GETLX                                                            
*                                                                               
GETL20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GETLX    B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* INITIALIZE PERSON BLOCK                                            *          
**********************************************************************          
         SPACE 1                                                                
INITPERB NTR1                                                                   
         XC    PERSTAT,PERSTAT     ASSUME NO PERSON RECORDS                     
         L     R1,APERBLK                                                       
         USING PERD,R1                                                          
         MVC   PERDMGR,DATAMGR                                                  
         MVC   PERCOMP,RCCOMPFL                                                 
         OI    PERFLAGS,PERRECCK                                                
         GOTO1 =V(PERCALL)         GET PERSON BLOCK                             
         TM    PERERR,PERNOREC     TEST NO RECORDS                              
         BO    *+8                                                              
         OI    PERSTAT,PERNC       USING PERSON RECORDS                         
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* RETURN EQ IF PERSON WAS ACTIVE FOR WITHIN THE CALENDER REQ         *          
**********************************************************************          
         SPACE 1                                                                
         USING PERD,R2                                                          
CHKPER   NTR1                                                                   
         L     R2,APERBLK                                                       
         SR    RF,RF                                                            
         ICM   RF,1,PERLNUM                                                     
         BZ    CHKPNO                                                           
         LA    R1,PERLVALS                                                      
         USING PERLVALS,R1                                                      
CHKP10   CLC   PERLSTD,CALEND                                                   
         BH    *+14                                                             
         CLC   PERLENDD,CALSTR                                                  
         BNL   CHKPYES                                                          
*                                                                               
         LA    R1,PERLVLEN(R1)                                                  
         BCT   RF,CHKP10                                                        
         B     CHKPNO                                                           
*                                                                               
CHKPYES  SR    RC,RC                                                            
CHKPNO   LTR   RC,RC                                                            
CHKPX    B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
**********************************************************************          
* DELETE THE PERSON IN PERCODE FROM PERTAB                           *          
**********************************************************************          
         SPACE 1                                                                
DELPER   NTR1                                                                   
         L     R3,NPER                                                          
         GOTO1 BINSRCH,DMCB,(X'80',PERCODE),APERTAB,(R3),PERTABL,      X        
               PERKEYL,0                                                        
         CLI   0(R1),0             TEST IF RECORD FOUND                         
         BNE   DELPX                                                            
*                                                                               
         L     R3,DMCB+8                                                        
         ST    R3,NPER             UPDATE COUNT                                 
DELPX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SET THE IN OUT DATES FOR A PERSON IN PERTAB                        *          
**********************************************************************          
         SPACE 1                                                                
SETINOUT NTR1                                                                   
         L     R3,NPER                                                          
         GOTO1 BINSRCH,DMCB,(X'00',PERCODE),APERTAB,(R3),PERTABL,      X        
               PERKEYL,0                                                        
         CLI   0(R1),0             TEST IF RECORD FOUND                         
         BNE   SETIOX                                                           
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,0(R1)                                                         
         USING PERD,R2                                                          
         L     R2,APERBLK                                                       
         LA    R0,PERLMAX                                                       
         XR    R1,R1                                                            
         ICM   R1,1,PERLNUM                                                     
         BZ    SETIOX                                                           
*                                                                               
         LA    R4,PERINOUT                                                      
         LA    R3,PERLVALS                                                      
         USING PERLVALS,R3                                                      
SETIO10  MVC   0(3,R4),PERLSTD                                                  
         MVC   3(3,R4),PERLENDD                                                 
         LA    R4,6(R4)                                                         
         LA    R3,PERLVLEN(R3)                                                  
         BCT   R1,SETIO10                                                       
*                                                                               
SETIOX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SET THE HIRE AND TERM DATES FOR THE PERSON IN PERTAB               *          
**********************************************************************          
         SPACE 1                                                                
SETHIRED NTR1                                                                   
         L     R3,NPER                                                          
         GOTO1 BINSRCH,DMCB,(X'00',PERCODE),APERTAB,(R3),PERTABL,      X        
               PERKEYL,0                                                        
         CLI   0(R1),0             TEST IF RECORD FOUND                         
         BNE   SETHX                                                            
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,0(R1)                                                         
         USING PERD,R2                                                          
         L     R2,APERBLK                                                       
*                                                                               
         MVC   PERHIRE,PERHIR                                                   
         MVC   PERTERM,PERTRM                                                   
*                                                                               
SETHX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* INITIALIZE SUMPREC ROUTINE                                         *          
**********************************************************************          
         SPACE 1                                                                
INITSUMP NTR1                                                                   
         USING SUMRECD,R7                                                       
         LA    R7,SUMREC                                                        
         XC    SUMREC,SUMREC                                                    
         LA    R1,SUMBUCK                                                       
         LA    R0,SUMNBUCK         LOOP COUNTER                                 
         ZAP   0(L'SUMBUCK,R1),=P'0'  INITIALIZE BUCKETS                        
         LA    R1,L'SUMBUCK(R1)                                                 
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* PBDHRS, PBDDATE (T DATE), PBDMONTH IS MOA                          *          
* ON ENTRY, R3 IS ASSUMED TO POINT TO SUMREC                         *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
PUTBYDAT NTR1  WORK=(R5,GPOPLENQ)                                               
         CLI   DATESARE,CALDATES                                                
         BE    PUTB60                                                           
*                                                                               
         MVC   HALF,PBDMONTH       USE MOA FOR POSTING                          
         CLI   DATEOPT,C'T'        TEST FOR TRANSACTION DATE OVERRIDE           
         BNE   *+10                                                             
         MVC   HALF,PBDDATE                                                     
         CLC   HALF,MONEND         TRANSACTION IS IN LAST MONTH                 
         BNE   PUTB30              NO                                           
*                                                                               
         LA    RE,SUMWEEK          RE=A(WEEKLY BUCKET)                          
         LA    R1,DATETAB          R1=A(WEEK END DATES)                         
         SR    R0,R0                                                            
         ICM   R0,1,DTENUM         NUMBER OF PERIODS IN MONTH                   
         BNZ   *+8                                                              
         LA    R0,NDATES           DEFAULT                                      
*                                                                               
PUTB10   CLC   PBDDATE,0(R1)                                                    
         BNH   PUTB20                                                           
         LA    RE,L'SUMWEEK(RE)                                                 
         LA    R1,L'DATETAB(R1)                                                 
         BCT   R0,PUTB10                                                        
*                                                                               
PUTB20   AP    0(L'SUMWEEK,RE),PBDHRS                                           
*                                                                               
PUTB30   SR    R0,R0                                                            
         IC    R0,NMONTHS                                                       
         LA    RE,MONTHTAB         NOW FIND THE MONTH TO POST AGAINST           
         LA    R4,SUMMONTH                                                      
*                                                                               
PUTB40   CLC   HALF,0(RE)                                                       
         BE    PUTB50                                                           
         LA    RE,L'MONTHTAB(RE)                                                
         LA    R4,L'SUMMONTH(R4)                                                
         BCT   R0,PUTB40                                                        
*                                                                               
         CLI   DATEOPT,C'T'        TEST TRANSACTION DATE OPTION                 
         BE    PBDX                SKIP ITEM                                    
         DC    H'0'                DUMP FOR MOA OPTION                          
*                                                                               
PUTB50   AP    0(L'SUMMONTH,R4),PBDHRS                                          
         B     PBDX                                                             
*                                                                               
PUTB60   LA    R4,MONTHTAB                                                      
         LA    R6,SUMMONTH                                                      
         SR    R0,R0                                                            
         IC    R0,NMONTHS                                                       
*                                                                               
PUTB70   GOTO1 VGETPER,DMCB,(R4),ACALKEYS,(R5),0,ACALREC                        
         USING GPOPD,R5                                                         
         CLC   PBDDATE,GPOSTRT                                                  
         BL    PUTB80                                                           
         CLC   PBDDATE,GPOEND                                                   
         BH    PUTB80                                                           
         AP    0(L'SUMMONTH,R6),PBDHRS                                          
         B     PUTB90                                                           
*                                                                               
PUTB80   LA    R4,L'MONTHTAB(R4)                                                
         LA    R6,L'SUMMONTH(R6)                                                
         BCT   R0,PUTB70                                                        
*                                                                               
PUTB90   GOTO1 VGETPER,DMCB,MONEND,ACALKEYS,(R5),0,ACALREC                      
         USING GPOPD,R5                                                         
         CLC   PBDDATE,GPOSTRT                                                  
         BL    PBDX                                                             
         CLC   PBDDATE,GPOEND                                                   
         BH    PBDX                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GPONPER                                                       
         LA    R5,GPOPERS                                                       
         USING GPOPERD,R5                                                       
         LA    R6,SUMWEEK                                                       
*                                                                               
PUTB100  CLC   PBDDATE,GPOPSTRT                                                 
         BL    PUTB110                                                          
         CLC   PBDDATE,GPOPEND                                                  
         BH    PUTB110                                                          
         AP    0(L'SUMWEEK,R6),PBDHRS                                           
         B     PBDX                                                             
*                                                                               
PUTB110  LA    R6,L'SUMWEEK(R6)                                                 
         LA    R5,GPOPLN1Q(R5)                                                  
         BCT   R0,PUTB100                                                       
*                                                                               
PBDX     B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* FILTER ON CALENDAR                                                 *          
**********************************************************************          
         SPACE 1                                                                
FLTONCAL NTR1                                                                   
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         L     R2,ACMALTN          A(TIME RECORD)                               
         USING TIMRECD,R2                                                       
         CLC   TIMKPEDT,CALSTR                                                  
         BL    FOCX                                                             
         CLC   TIMKPEDT,CALEND                                                  
         BH    FOCX                                                             
         CR    R1,R1                                                            
FOCX     B     EXIT                                                             
         DROP  R2,RF                                                            
         EJECT                                                                  
**********************************************************************          
* PUT SUMREC TOTALS TO BUFFALO                                       *          
*     R3 = A(SUMREC)                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
PUTSUMR  NTR1                                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,SUMREC                                
*                                                                               
* ADD TO THE TOTAL FOR TYPE OF TIME                                             
*                                                                               
         XC    SUMACC,SUMACC                                                    
         MVI   SUMACC,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,SUMREC                                
*                                                                               
* ADD TO CHARGEABLE TIME TOTALS IF APPROPRIATE                                  
*                                                                               
         CLI   SUMTYPE,SUMNOT      TEST NOT CHARGEABLE                          
         BE    PUTS10              YES                                          
*                                                                               
         XC    SUMACC,SUMACC                                                    
         MVI   SUMTYPE,SUMCHG                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,SUMREC                                
*                                                                               
* ADD TO TOTAL HOURS FOR PERSON                                                 
*                                                                               
PUTS10   XC    SUMACC,SUMACC                                                    
         MVI   SUMTYPE,SUMTOT                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,SUMREC                                
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE DETAIL REPORT                             *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
DETAIL   NTR1                                                                   
         LA    R7,SUMREC                                                        
         XC    SUMREC,SUMREC                                                    
         MVC   SUMPER,SPACES       INITIALIZE KEY AREA                          
         XC    LASTBUF,LASTBUF     CLEAR KEY CONTROL AREA                       
*                                                                               
DETL10   GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SUMRECD,1                            
         TM    DMCB+8,X'80'        TEST FOR EOF                                 
         BO    DETLX               NOTHING TO PRINT                             
*                                                                               
         MVC   PERCODE,SUMPER      EXTRACT PERSON ACCOUNT CODE                  
         LA    R0,PERNBUCK         R0=LOOP COUNTER                              
         LA    RE,PERBUCK          RE=A(PERSON BUCKETS)                         
         ZAP   0(L'PERBUCK,RE),=P'0' CLEAR THE BUCKETS                          
         LA    RE,L'PERBUCK(RE)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         L     R2,NPER                                                          
         GOTO1 BINSRCH,DMCB,SUMPER,APERTAB,(R2),PERTABL,PERKEYL,0               
         CLI   0(R1),0             TEST IF RECORD FOUND                         
         BNE   DETL170                                                          
*                                                                               
         L     R6,0(R1)            GET A(ENTRY)                                 
         USING PERTABD,R6                                                       
         MVC   PERNAME,PERRNAME                                                 
         MVC   PERDHIR,PERHIRE                                                  
         MVC   PERDTRM,PERTERM                                                  
         MVC   PERDINOT,PERINOUT   IN/OUTS                                      
*                                                                               
         CLI   REPMODE,REPMMNTH                                                 
         BNE   DETL30                                                           
*                                                                               
         LA    R0,PERNSTD          R0=LOOP COUNTER                              
         LA    R1,PERSHR           R1=O/P POINTER                               
         LA    RE,PERSTHRS         RE=PERSON BUDGET DATA                        
         ZAP   DUB,=P'0'           INITIALIZE YEARLY TOTAL                      
*                                                                               
DETL20   ZAP   0(L'PERSHR,R1),0(L'PERSTHRS,RE)                                  
         AP    DUB,0(L'PERSTHRS,RE)                                             
         LA    R1,L'PERSHR(R1)                                                  
         LA    RE,L'PERSTHRS(RE)                                                
         BCT   R0,DETL20                                                        
*                                                                               
         ZAP   0(L'PERSHR,R1),DUB  SET TOTAL                                    
         B     DETL40                                                           
*                                                                               
DETL30   SR    R1,R1                                                            
         IC    R1,NMONTHS                                                       
         BCTR  R1,0                DEVELOP INDEX TO LAST MONTH                  
         MH    R1,=Y(L'PERSTHRS)                                                
         LA    RE,PERSTHRS(R1)                                                  
         ZAP   THISSTD,0(L'PERSTHRS,RE) GET STANDARD HOURS                      
         GOTO1 AWEEKLY,DMCB,(RC)        EXTRACT FROM BUFFALO                    
*                                                                               
DETL40   CLI   VACOPT,C'Y'         TEST TO DEDUCT VACATION                      
         BNE   DETL80              NO                                           
         OC    VACACC,VACACC       TEST FOR VACATION ACCOUNT                    
         BZ    DETL80              NO                                           
*                                                                               
         XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMNOT                                                   
         MVC   SUMACC,VACACC                                                    
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'                                                     
         BNZ   DETL80              COULD NOT FIND IT                            
*                                                                               
         LA    R0,NWKS             R0=LOOP COUNTER                              
         LA    R1,PERSHR           R1=A(STANDARD HOURS)                         
         LA    RE,SUMWEEK          RE=A(WEEKLY VACATION BUCKETS)                
         CLI   REPMODE,REPMWEEK                                                 
         BE    *+12                                                             
         LA    R0,NMTHS            R0=LOOP COUNTER                              
         LA    RE,SUMMONTH         RE=A(WEEKLY VACATION BUCKETS)                
*                                                                               
         CLI   REPMODE,REPMWEEK                                                 
         BE    DETL60                                                           
*                                                                               
DETL50   SP    0(L'PERSHR,R1),0(L'SUMMONTH,RE)                                  
         BP    *+10                                                             
         ZAP   0(L'PERSHR,R1),=P'0' DO NOT ALLOW NEGATIVE RESULT                
         SP    DUB,0(L'SUMMONTH,RE)                                             
         LA    R1,L'PERSHR(R1)                                                  
         LA    RE,L'SUMMONTH(RE)                                                
         BCT   R0,DETL50                                                        
*                                                                               
         CP    DUB,=P'0'                                                        
         BP    *+10                                                             
         ZAP   DUB,=P'0'                                                        
         ZAP   0(L'PERSHR,R1),DUB                                               
         B     DETL80                                                           
*                                                                               
DETL60   ZAP   THISSTD,=P'0'       RECOMPUTE MONTHLY FIGURE                     
DETL70   SP    0(L'PERSHR,R1),0(L'SUMWEEK,RE)                                   
         BP    *+10                                                             
         ZAP   0(L'PERSHR,R1),=P'0'                                             
         AP    THISSTD,0(L'PERSHR,R1)                                           
         LA    R1,L'PERSHR(R1)                                                  
         LA    RE,L'SUMWEEK(RE)                                                 
         BCT   R0,DETL70                                                        
*                                                                               
DETL80   ZAP   THISTARG,PERTARG    AND TARGET PERCENTAGE                        
         CLI   PRMTHTPC,C'Y'       WAS TARG PCT ENTERED BY MONTH                
         BNE   DETL90              NO                                           
         SR    R1,R1                                                            
         IC    R1,NMONTHS          ADDRESS LAST MONTHS TARG PCT                 
         BCTR  R1,0                                                             
         MH    R1,=Y(L'PERTARG)                                                 
         LA    R1,PERTARG(R1)                                                   
         ZAP   THISTARG,0(L'PERTARG,R1) USE LAST MONTHS AS GENERAL              
*                                                                               
         CLI   REPMODE,REPMWEEK    IS THIS A WEEKLY                             
         BE    DETL90              YES                                          
         LA    R0,NMTHS            SET TARG PCT FOR COLS                        
         LA    R1,EMPTARG                                                       
         LA    RE,PERTARG                                                       
         ZAP   0(L'EMPTARG,R1),0(L'PERTARG,RE)                                  
         LA    R1,L'EMPTARG(R1)                                                 
         LA    RE,L'PERTARG(RE)                                                 
         BCT   R0,*-14                                                          
         ZAP   0(L'EMPTARG,R1),=P'0'  ZAP THIS FIELD SO THAT EDIT WORKS         
         DROP  R6                                                               
*                                                                               
DETL90   XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE      READ THE BILLABLE TIME TOTALS                
         MVI   SUMTYPE,SUMBIL                                                   
         MVI   SUMACC,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'        TEST NOT FOUND                               
         BNZ   DETL100             YES                                          
*                                                                               
         GOTO1 ASETBKS,DMCB,(RC),PERBHR                                         
*                                                                               
DETL100  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE      READ THE 'R' TIME TOTALS                     
         MVI   SUMTYPE,SUMRTE                                                   
         MVI   SUMACC,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'        TEST NOT FOUND                               
         BNZ   DETL110             YES                                          
*                                                                               
         GOTO1 ASETBKS,DMCB,(RC),PERRHR                                         
*                                                                               
DETL110  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE      READ THE CHARGEABLE TOTALS                   
         MVI   SUMTYPE,SUMCHG                                                   
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'        TEST NOT FOUND                               
         BNZ   DETL120             YES                                          
*                                                                               
         GOTO1 ASETBKS,DMCB,(RC),PERCHG                                         
*                                                                               
DETL120  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE      READ THE NON-CHARGEABLE TOTALS               
         MVI   SUMTYPE,SUMNOT                                                   
         MVI   SUMACC,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'        TEST NOT FOUND                               
         BNZ   DETL130             YES                                          
*                                                                               
         GOTO1 ASETBKS,DMCB,(RC),PERNHR                                         
*                                                                               
DETL130  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE      READ THE OVERALL TOTALS                      
         MVI   SUMTYPE,SUMTOT                                                   
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'        TEST NOT FOUND                               
         BZ    DETL140                                                          
*                                                                               
         LA    R0,6                TREAT NOT FOUND AS ZERO RECORD               
         LA    R1,PERTOT                                                        
         ZAP   0(L'PERBUCK,R1),=P'0'                                            
         LA    R1,L'PERBUCK(R1)                                                 
         BCT   R0,*-10                                                          
         B     DETL150                                                          
*                                                                               
DETL140  GOTO1 ASETBKS,DMCB,(RC),PERTOT                                         
*                                                                               
DETL150  MVI   FORCEHED,C'Y'                                                    
         CLI   REPMODE,REPMMNTH                                                 
         BNE   DETL160                                                          
         CLI   ISBURSON,C'Y'                                                    
         BE    DETL160                                                          
         GOTO1 APRTEMP,DMCB,(RC)     PRINT THE SUMMARY FOR EMPLOYEE             
         B     DETL170                                                          
*                                                                               
DETL160  BAS   RE,PRTDET           PRINT LAST PERSON                            
DETL170  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,X'FF'                                                    
         B     DETL10                                                           
*                                                                               
DETLX    B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE DETAIL REPORT FOR ONE EMPLOYEE            *          
* AT ENTRY, THE PERSON BUCKETS (PERBUCK) CONTAIN THE DATA FOR        *          
* REPORT.  ONE PAGE IS PRINTED UNLESS THE CONTRA-ACCOUNT DETAIL      *          
* OPTION IS ELECTED                                                  *          
**********************************************************************          
         SPACE 1                                                                
PRTDET   NTR1                                                                   
         LA    RE,PERSHR                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,PERAHR                                                        
         ST    RE,ASTANDRD                                                      
         GOTO1 ACALCTOT,DMCB,(RC),ASTANDRD                                      
*                                                                               
         XR    R2,R2                                                            
         LA    R3,MONEND           ASSUME WEEKLY                                
         CLI   REPMODE,REPMWEEK                                                 
         BE    *+12                                                             
         LA    R3,MONTHTAB                                                      
         LA    R2,NMONTHS          SET N'MONTHS PARM TO 0                       
*                                                                               
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   PRTDET05                                                         
         GOTO1 ASETHRS,DMCB,(RC),PERAHR,(R2),(R3)                               
         GOTO1 ACALCTOT,DMCB,(RC),PERAHR                                        
*                                                                               
         LA    RE,PERAHR           IF NO AHRS, DON'T PRINT                      
         LA    RF,NMTHS                                                         
         CP    0(8,RE),=P'0'                                                    
         BNE   *+16                                                             
         LA    RE,8(RE)                                                         
         BCT   RF,*-14                                                          
         B     PRTDTX                                                           
*                                                                               
         USING DETD,R2                                                          
PRTDET05 LA    R2,P                                                             
         TM    TIMETYPE,BTIME+RTIME                                             
         BZ    PRTDT60                                                          
         MVC   P+1(16),=C'CHARGEABLE HOURS'                                     
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         TM    TIMETYPE,BTIME                                                   
         BNO   PRTDT20                                                          
*                                                                               
         MVC   P+1(7),=C'B HOURS'                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         CLI   QOPT4,C'Y'          TEST FOR CONTRA-ACCOUNT DETAIL               
         BNE   PRTDT10                                                          
         GOTO1 CONTRA,SUMBIL                                                    
*                                                                               
PRTDT10  MVC   DETMSG(7),=C'B HOURS'                                            
         GOTO1 DETOUT,DMCB,PERBHR,0                                             
*                                                                               
         TM    METRFILT,MFBILL                                                  
         BNO   PRTDT20                                                          
*                                                                               
         MVC   DETMSG(18),=C'B HOURS % TO TOTAL'                                
         GOTO1 DETPER,DMCB,PERBHR,PERTOT,PERPCT                                 
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
*                                                                               
         MVC   DETMSG(19),=C'% TO STANDARD HOURS'                               
         CLI   PRAVAIL,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DETMSG(20),=C'% TO AVAILABLE HOURS'                              
*                                                                               
         CLI   ISBURSON,C'Y'                                                    
         BNE   *+10                                                             
         MVC   DETMSG(20),=CL20'BILLABILITY'                                    
*                                                                               
         GOTO1 DETPER,DMCB,PERBHR,ASTANDRD,PERPCT                               
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTDT20  TM    TIMETYPE,RTIME                                                   
         BNO   PRTDT40                                                          
*                                                                               
         MVC   P+1(7),=C'R HOURS'                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         CLI   QOPT4,C'Y'                                                       
         BNE   PRTDT30                                                          
         GOTO1 CONTRA,SUMRTE                                                    
*                                                                               
PRTDT30  MVC   DETMSG(7),=C'R HOURS'                                            
         GOTO1 DETOUT,DMCB,PERRHR,0                                             
*                                                                               
         MVC   DETMSG(18),=C'R HOURS % TO TOTAL'                                
         GOTO1 DETPER,DMCB,PERRHR,PERTOT,PERPCT                                 
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
*                                                                               
         MVC   DETMSG(19),=C'% TO STANDARD HOURS'                               
         CLI   PRAVAIL,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DETMSG(20),=C'% TO AVAILABLE HOURS'                              
         GOTO1 DETPER,DMCB,PERRHR,ASTANDRD,PERPCT                               
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTDT40  TM    TIMETYPE,BTIME+RTIME                                             
         BNO   PRTDT60                                                          
         MVC   P+1(16),=C'TOTAL CHARGEABLE'                                     
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   DETMSG(16),=C'TOTAL B + R TIME'                                  
         GOTO1 DETOUT,DMCB,PERCHG,0                                             
*                                                                               
         TM    METRFILT,MFPROD                                                  
         BNO   PRTDT50                                                          
         MVC   DETMSG(16),=C'% B + R TO TOTAL'                                  
         CLI   ISBURSON,C'Y'                                                    
         BNE   *+10                                                             
         MVC   DETMSG(16),=CL16'PRODUCTIVITY'                                   
         GOTO1 DETPER,DMCB,PERCHG,PERTOT,PERPCT                                 
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
*                                                                               
PRTDT50  MVC   DETMSG(19),=C'% TO STANDARD HOURS'                               
         CLI   PRAVAIL,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DETMSG(20),=C'% TO AVAILABLE HOURS'                              
         GOTO1 DETPER,DMCB,PERCHG,ASTANDRD,PERPCT                               
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTDT60  TM    TIMETYPE,NTIME                                                   
         BNO   PRTDT80                                                          
         MVC   P+1(19),=C'NON-CHARGEABLE TIME'                                  
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         CLI   QOPT4,C'Y'                                                       
         BNE   PRTDT70                                                          
         GOTO1 CONTRA,SUMNOT                                                    
*                                                                               
PRTDT70  MVC   DETMSG(7),=C'N HOURS'                                            
         GOTO1 DETOUT,DMCB,PERNHR,0                                             
*                                                                               
         MVC   DETMSG(18),=C'N HOURS % TO TOTAL'                                
         GOTO1 DETPER,DMCB,PERNHR,PERTOT,PERPCT                                 
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
*                                                                               
         MVC   DETMSG(19),=C'% TO STANDARD HOURS'                               
         CLI   PRAVAIL,C'Y'                                                     
         BNE   *+10                                                             
         MVC   DETMSG(20),=C'% TO AVAILABLE HOURS'                              
         GOTO1 DETPER,DMCB,PERNHR,ASTANDRD,PERPCT                               
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTDT80  MVC   P+1(14),=C'TOTAL REPORTED'                                       
         GOTO1 DETOUT,DMCB,PERTOT,0                                             
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+1(14),=C'STANDARD HOURS'                                       
         CLI   PRAVAIL,C'Y'                                                     
         BNE   *+10                                                             
         MVC   P+1(15),=CL15'AVAILABLE HOURS'                                   
         GOTO1 DETOUT,DMCB,ASTANDRD,0                                           
*                                                                               
         CLI   ISBURSON,C'Y'                                                    
         BNE   PRTDT90                                                          
         TM    METRFILT,MFUTIL                                                  
         BNO   PRTDT90                                                          
*                                                                               
         GOTO1 ACALCTOT,DMCB,(RC),PERAHR                                        
         MVC   P+1(15),=CL15'UTILIZATION %'                                     
         GOTO1 DETPER,DMCB,PERTOT,PERAHR,PERPCT                                 
         GOTO1 DETOUT,DMCB,PERPCT,1                                             
*                                                                               
PRTDT90  CLI   REPMODE,REPMMNTH    IS THIS A MONTHLY                            
         BNE   PRTDTX              NO                                           
         CLI   PRMTHTPC,C'Y'       USING MONTHLY TARGET PERCENTAGE              
         BNE   PRTDTX              NO, PRINT IN HEADER                          
         MVC   P+1(8),=CL8'TARGET %'                                            
         GOTO1 DETOUT,DMCB,EMPTARG,0,0                                          
*                                                                               
PRTDTX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT OUT THE CONTRA-ACCOUNT DETAIL FOR EACH        *          
* TYPE OF TIME--CALLED BY PRTDET, AT ENTRY, R1=TIME TYPE             *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
CONTRA   NTR1                                                                   
         LA    R7,SUMREC                                                        
         XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE      BUILD BUFFALO KEY                            
         STC   R1,SUMTYPE                                                       
         MVC   LASTBUF,SUMRECD                                                  
         MVI   ANYDET,C'N'         SET SWITCH FOR NO DETAIL                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SUMREC,1                             
*                                                                               
CONT10   TM    DMCB+8,X'90'                                                     
         BNZ   CONT60                                                           
         CLC   SUMRECD(SUMACC-SUMRECD),LASTBUF                                  
         BNE   CONT60              ALL DONE FOR THIS TYPE                       
         CLI   SUMACC,X'FF'        TEST FOR TOTAL RECORD                        
         BE    CONT60              YES-ALL DONE                                 
*                                                                               
         LA    R0,NWKS             LOOP COUNTER                                 
         LA    RE,SUMWEEK          TEST FOR ANY ACTIVITY IN MONTH               
         CLI   REPMODE,REPMWEEK                                                 
         BE    *+12                                                             
         LA    R0,NMTHS                                                         
         LA    RE,SUMMONTH         TEST FOR ANY ACTIVITY IN MONTH               
*                                                                               
         CP    0(L'SUMWEEK,RE),=P'0'                                            
         BNE   CONT20              YES                                          
         LA    RE,L'SUMWEEK(RE)                                                 
         BCT   R0,*-14                                                          
         B     CONT50              GET NEXT RECORD                              
*                                                                               
CONT20   MVI   ANYDET,C'Y'                                                      
         GOTO1 ASETBKS,DMCB,(RC),PERGHR                                         
*                                                                               
         CLI   SUMTYPE,SUMNOT      TEST FOR 'N' TIME                            
         BNE   *+12                NO                                           
         CLI   SUMACC,C'N'         TEST FOR 1N CONTRA                           
         BE    CONT30                                                           
*                                                                               
         LA    R4,SUMACC                                                        
         CLI   SUMTYPE,SUMNOT                                                   
         BNE   *+8                                                              
         LA    R4,1(R4)            'N' TIME INCLUDE LEDGER CODE                 
*                                                                               
         L     R2,NCLI                                                          
         GOTO1 BINSRCH,DMCB,(R4),ACLITAB,(R2),CLITABL,CLIKEYL,(R2)              
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         USING CLITABD,RE                                                       
         MVC   P+1(3),0(R4)        SHOW CODE                                    
         MVC   P+5(L'CLINAME),CLINAME                                           
         B     CONT40                                                           
*                                                                               
CONT30   L     R2,NNOTS                                                         
         GOTO1 BINSRCH,DMCB,SUMACC+1,ANOTTAB,(R2),NOTTABL,NOTKEYL,(R2)          
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         USING NOTTABD,RE                                                       
         MVC   P+1(28),NOTNAME                                                  
*                                                                               
CONT40   LA    R2,P                                                             
         GOTO1 DETOUT,DMCB,PERGHR,0                                             
*                                                                               
CONT50   GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,SUMREC,1                              
         B     CONT10                                                           
*                                                                               
CONT60   CLI   ANYDET,C'Y'         TEST FOR ANY DETAIL                          
         BNE   CONTX                                                            
         GOTO1 ACREPORT                                                         
*                                                                               
CONTX    B     EXIT                                                             
         DROP  R7,RE                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO OUTPUT A DETAIL PRINT LINE                          *          
* AT ENTRY, R2=A(PRINT LINE), P1=A(BUCKETS TO PRINT)                 *          
*           P2=0 FOR 2 DECIMALS  =1 FOR 1 DECIMAL                    *          
**********************************************************************          
         EJECT                                                                  
         USING DETD,R2                                                          
DETOUT   NTR1                                                                   
         L     R4,0(R1)                                                         
         LR    R6,R4               SAVE ADDRESS OF 1ST PARAM                    
         MVC   BYTE,7(R1)                                                       
         XC    FLGSET,FLGSET       FLAG FOR TOTAL RUN                           
         CLI   REPMODE,REPMWEEK                                                 
         BNE   DETO40                                                           
*                                                                               
* DO WEEKLY                                                                     
*                                                                               
         LA    R5,DETDISPS         R5=A(DISPLACEMENTS INTO PRINT LINE)          
         SR    R1,R1                                                            
         ICM   R1,1,DTENUM         ACTUAL # OF PERIODS IN MONTH(NEWCAL)         
         BNZ   *+8                                                              
         LA    R1,5                HARCODE FOR NOW UNTIL I FIX REPORT           
*                                                                               
         CH    R1,=H'5'            IF # OF PERIODS < 5                          
         BNL   *+8                                                              
         OI    FLGSET,FLGPERD      SET FLAG                                     
*                                                                               
DETO10   L     R3,0(R5)            DISP TO COLUMN                               
         LA    R3,DETD(R3)         ADDRESS OF COLUMN                            
         CLI   BYTE,1              TEST FOR 1 DECIMAL PLACE O/P                 
         BE    DETO20                                                           
         EDIT  (P8,(R4)),(10,(R3)),2,ZERO=NOBLANK,MINUS=YES                     
         B     DETO30                                                           
*                                                                               
DETO20   EDIT  (P8,(R4)),(9,(R3)),1,ZERO=NOBLANK,MINUS=YES                      
DETO30   LA    R5,L'DETDISPS(R5)   NEXT DISPLACEMENT                            
         LA    R4,L'PERBUCK(R4)    NEXT BUCKET                                  
         BCT   R1,DETO10                                                        
*                                                                               
* CHECK IF ALL COLUMNS HAVE BEEN PRINTED                                        
*                                                                               
         TM    FLGSET,FLGPERD      ARE THERE 5 PERIODS?                         
         BZ    *+8                 YES - LEAVE OUTPUT ALONE                     
         LA    R5,L'DETDISPS(R5)   NO  - SKIP TO NEXT DISPLACEMENT              
*                                                                               
* DO MONTH TOTALS                                                               
*                                                                               
         TM    FLGSET,FLGTOT       DID WE RUN TOTALS YET?                       
         BO    DETOX               YES - EXIT                                   
         OI    FLGSET,FLGTOT       ELSE - SET FLAG AND DO MONTH TOTAL           
         LA    R1,1(R1)            RESET R1 TO 1                                
         LA    R4,NWKS*BUCKLN(R6)  SET R4 TO MONTH TOTAL - WEEKLY               
         B     DETO10              DO ONE MORE LOOP                             
*                                                                               
* DO MONTHLY                                                                    
*                                                                               
DETO40   ST    R4,FULL             SAVE A(BUCKS)                                
         LA    R5,DTEMDISP         R5=A(DISPLACEMENTS INTO PRINT LINE)          
         SR    R6,R6                                                            
         IC    R6,NMONTHS                                                       
*                                                                               
         USING EMPSUMD,R2                                                       
DETO50   L     R3,0(R5)            DISP TO COLUMN                               
         LA    R3,EMPSUMD(R3)      ADDRESS OF COLUMN                            
         GOTO1 DETEMP,(R4)                                                      
*                                                                               
         LA    R5,L'DTEMDISP(R5)   NEXT DISPLACEMENT                            
         LA    R4,8(R4)            NEXT BUCKET                                  
         BCT   R6,DETO50                                                        
*                                                                               
         LA    R3,EMPMON2-EMPMON1+1(R3) O/P POSITION                            
         L     R4,FULL             PRINT YEARLY TOTAL                           
         LA    R4,12*8(R4)                                                      
*                                                                               
         GOTO1 DETEMP,(R4)                                                      
*                                                                               
DETOX    GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO EDIT OUT NUMBERS FOR EMPOUT SUB-ROUTINE             *          
* AT ENTRY, R1=A(INPUT) R3=A(OUTPUT)                                 *          
**********************************************************************          
         SPACE 1                                                                
DETEMP   NTR1                                                                   
         XC    EBLOCK,EBLOCK                                                    
         ST    R1,EBAIN            INPUT=8 BYTE PACKED NUMBERS                  
         MVI   EBLIN,8                                                          
         MVI   EBTIN,C'P'                                                       
         ST    R3,EBAOUT                                                        
         MVI   EBLOUT,6                                                         
         MVI   EBDECS,2            DEFAULT IS TWO DECIMAL PLACES                
         CLI   BYTE,1                                                           
         BNE   *+8                                                              
         MVI   EBDECS,1                                                         
         CP    0(8,R1),=P'0'       TEST FOR NEGATIVE NUMBER                     
         BNL   *+8                                                              
         OI    EBOPT,EBOQMEY       YES-ELECT MINUS OPTION                       
         GOTO1 ADEDITOR,DMCB,EBLOCK                                             
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO COMPUTE A ROW OF PERCENTAGE BUCKETS FOR DETAIL      *          
* REPORT                                                             *          
* AT ENTRY, P1 - BYTE 0 = OR SHIFT AMOUNT TO ADJUST DIVIDEND         *          
*                BYTES 1-3 = A(DIVIDEND BUCKETS)                     *          
*           P2 - BYTE 0=0 FOR ROW OF BUCKETS, 1=ONE BUCKET           *          
*                BYTES 1-3=A(DIVISOR)                                *          
*           P3 - A(OUTPUT BUCKETS)                                   *          
**********************************************************************          
         EJECT                                                                  
DETPER   NTR1                                                                   
         LM    R3,R5,0(R1)                                                      
         SR    R6,R6                                                            
         IC    R6,0(R1)            GET DIVIDEND ADJUSTMENT                      
         MVC   BYTE,4(R1)          EXTRACT DIVISOR CHARACTERISTIC               
         LA    R0,13               R0=LOOP COUNTER                              
*                                                                               
DETP10   ZAP   0(L'PERBUCK,R5),=P'0' INITIALIZE RESULT TO ZERO                  
         ZAP   PL16,0(L'PERBUCK,R3) GET DIVIDEND                                
         LA    RE,4(R6)            SET SHIFT AMOUNT                             
         SRP   PL16,0(RE),0        MIN SHIFT IS *1000 *10 FOR ROUND             
         CP    0(L'PERBUCK,R4),=P'0' TEST FOR ZERO DIVISOR                      
         BE    DETP20              YES                                          
*                                                                               
         DP    PL16,0(L'PERBUCK,R4)                                             
         SRP   PL16(8),64-1,5      ROUND TO ONE DECIMAL PLACE                   
         ZAP   0(L'PERBUCK,R5),PL16(8) SET RESULT                               
*                                                                               
DETP20   LA    R3,L'PERBUCK(R3)                                                 
         LA    R5,L'PERBUCK(R5)                                                 
         CLI   BYTE,1              TEST FOR SINGLE DIVISOR                      
         BE    *+8                 YES                                          
         LA    R4,L'PERBUCK(R4)                                                 
         BCT   R0,DETP10                                                        
*                                                                               
DETPERX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* GET NAME FROM NAME ELEMENT                                         *          
*        R6 IS ADDRESS OF THE 20 ELEMENT                             *          
*        R3 IS ADDRESS OF 36 BYTE AREA                               *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R6                                                        
GETNAME  NTR1                                                                   
         CLI   NAMEL,NAMELQ        ARE WE AT THE 20 ELEMENT?                    
         BE    *+6                 YES - CONTINUE                               
         DC    H'0'                NO - DIE!                                    
*                                                                               
         MVC   0(L'NAMEREC,R3),SPACES                                           
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   0(0,R3),NAMEREC                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* SETBUD - FIND OUT WHAT LEVELS THE USER HAS DEFINED HIS             *          
*          BUDGETS AT SO YOU CAN READ FOR THEM WITH THE CORRECT      *          
*          ACCOUNT (WHEN YOU HAVE TO)                                *          
* BUDGET NUMBER IS AT 0(R2), RETURNS MODE TO READ IT AT IN 1(R2)     *          
**********************************************************************          
         SPACE 1                                                                
         USING BUDD,R2                                                          
SETBUD   NTR1                                                                   
         MVI   BUDMODE,X'FF'       INIT MODE TO INVALID                         
         ZAP   BUDAMNT,=P'0'                                                    
*                                                                               
         USING BUDRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    BUDKEY(ACCORFST),BUDKEY  CLEAR KEY                               
         MVI   BUDKTYP,BUDKTYPQ         BUDGET RECORD                           
         MVC   BUDKCPY,QCOMPANY                                                 
         MVC   BUDKNO1+1(1),BUDNUM      BUD NUMBER                              
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         CLC   IOKEY((BUDKNO1-BUDKEY)+L'BUDKNO1),SVKEY                          
         BNE   SETBX                  CAN'T FIND THIS BUD                       
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         MVI   ELCODE,BIVELQ       X'1C' - GET BUDGET VALID'TN ELEMENT          
         LA    R6,IO                                                            
         AH    R6,DISP2            ADD DISPLACEMENT TO FIRST ELEMENT            
*                                                                               
SETB30   BAS   RE,NEXTEL                                                        
         BNE   SETBX                                                            
*                                                                               
         USING BIVELD,R6                                                        
         CLC   BIVAUNT(2),=C'1R'                                                
         BNE   SETB30                                                           
         CLC   =C'1C',BIVVCUNT                                                  
         BNE   SETB30                                                           
         CLI   BIVACLV,0                                                        
         BNE   SETB40                                                           
         MVI   BUDMODE,LEDGFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB40   CLI   BIVACLV,1          SET MODE AT WHICH TO READ THIS BUD            
         BNE   SETB50                                                           
         MVI   BUDMODE,LEVAFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB50   CLI   BIVACLV,2                                                        
         BNE   SETB60                                                           
         MVI   BUDMODE,LEVBFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB60   CLI   BIVACLV,3                                                        
         BNE   SETB70                                                           
         MVI   BUDMODE,LEVCFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB70   CLI   BIVACLV,4                                                        
         BNE   SETBX                                                            
         MVI   BUDMODE,PROCACC                                                  
*                                                                               
SETBX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* GETBUD - READS BUDGET RECORD  AT EMPLOYEE LEVEL OF 1R              *          
*          P1=A(ACCOUNT RECORD)                                      *          
*          P2=RC                                                     *          
* PRMTHTPC=Y, TARGET PERCENTAGES ARE MONTHLY VALUES                  *          
**********************************************************************          
         SPACE 1                                                                
         USING BUDD,R2                                                          
GETBUD   NTR1                                                                   
         L     R5,0(R1)            GET A(ACCOUNT RECORD)                        
         LA    R0,2                                                             
         LA    R2,BUDAREA                                                       
GETB10   CLC   BUDMODE,MYMODE      DO THEY WANT THIS BUD AT THIS LEVEL          
         BNE   *+8                 NO                                           
         BAS   RE,READBUD                                                       
         LA    R2,BUDDLN(R2)                                                    
         BCT   R0,GETB10                                                        
GETBX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* READ BUDGET RECORDS                                                *          
**********************************************************************          
         SPACE 1                                                                
READBUD  NTR1                                                                   
         LA    RE,BUDAMNT          CLEAR BUDGET AMOUNT BUCKETS                  
         LA    R0,NMTHS                                                         
         ZAP   0(L'BUDAMNT,RE),=P'0'                                            
         LA    RE,L'BUDAMNT(RE)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         USING BUDRECD,R4                                                       
         LA    R4,SVKEY                                                         
*                                                                               
         XC    BUDKEY(ACCORFST),BUDKEY   CLEAR KEY                              
         MVI   BUDKTYP,BUDKTYPQ          BUDGET RECORD                          
         MVC   BUDKCPY,QCOMPANY          COMPANY                                
         MVC   BUDKCULA,0(R5)            1R ACCOUNT                             
         MVC   BUDKWORK,SPACES                                                  
         MVC   BUDKCCPY(15),SPACES       SPACE OUT CONTRA                       
         MVC   BUDKCCPY,QCOMPANY         COMPANY                                
         MVC   BUDKCUNT(2),=C'1C'        1C CONTRA                              
         MVC   BUDKBUDN+1(1),BUDNUM      TYPE OF BUDGET                         
         NI    BUDKBUDN+1,X'0F'          MAKE IT BINARY                         
         GOTO1 =A(DMHIGHDR),DMCB,(RC)    READ HIGH                              
         CLC   SVKEY((BUDKBUDN-BUDKEY)+L'BUDKBUDN),IOKEY                        
         BNE   READB99                 NO MATCH, BUDGET IS INCOMPLETE           
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)    GET RECORD                             
         USING BAMELD,R6                                                        
         LA    R6,IO               RETURN THE AMOUNT FOR THIS MONTH             
         AH    R6,DISP2                  BUMP TO FIRST ELEMENT                  
*                                                                               
READB10  CLI   0(R6),0                      END OF RECORD?                      
         BE    READB99                      YES                                 
         CLI   BAMEL,BAMELQ                 X'1D' - BUDGET AMT ELEMENT          
         BNE   READB50                      NO, GET NEXT EL                     
*                                                                               
         CLC   BUDNUM,BUTRGPCT+(BUDNUM-BUDD)  TEST TARGET PERCENT               
         BNE   *+12                                                             
         CLI   PRMTHTPC,C'Y'       TARG PCT ENTERED AS MONTHLY VAL              
         BNE   READB40                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NMONTHS          R0=LOOP COUNTER                              
         LA    R1,BUDAMNT          R1=A(BUDGET BUCKET)                          
         LA    RE,MONTHTAB         RE=A(MONTH  -  YYMM PWO)                     
*                                                                               
READB20  CLC   BAMMNTH,0(RE)      FIND THE MONTH FOR ELEMENT                    
         BE    READB30                                                          
         LA    R1,L'BUDAMNT(R1)                                                 
         LA    RE,L'MONTHTAB(RE)                                                
         BCT   R0,READB20                                                       
         B     READB50             NOT IN REPORT TIME PERIOD                    
*                                                                               
READB30  ZAP   0(L'BUDAMNT,R1),BAMBUDG   SLOT NUMBER                            
         B     READB45                                                          
*                                                                               
READB40  CLC   BAMMNTH(1),MONEND  MATCH ON YEAR                                 
         BNE   READB50                                                          
         AP    BUDAMNT,BAMBUDG                                                  
*                                                                               
READB45  OI    BUDSWCH,BUDSET      SET SWITCH TO SHOW BUDGET RECS USED          
READB50  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                        IN SEACH OF X'1D'                   
         B     READB10                                                          
*                                                                               
READB99  L     RE,LASTIO                                                        
         MVC   SVKEY,0(RE)         RE-READ THE LAST ACCOUNT                     
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD A TABLE OF 1N (NOT-BILLABLE TIME)             *          
* ACCOUNTS AND THEIR NAMES                                           *          
**********************************************************************          
         SPACE 1                                                                
NOT      NTR1                                                                   
         L     R5,ANOTTAB                                                       
         USING NOTTABD,R5                                                       
         SR    R2,R2                                                            
         MVC   SVKEY,SPACES                                                     
         LA    R4,SVKEY                                                         
         USING CHDRECD,R4                                                       
         MVC   CHDKCPY,RCCOMPFL                                                 
         MVC   CHDKUNT(2),=C'1N'                                                
*                                                                               
NOT10    LA    R4,SVKEY                                                         
         MVI   CHDKWRK,X'FF'                                                    
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   IOKEY(3),SVKEY                                                   
         BNE   NOTX                                                             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         MVC   SVKEY,IOKEY                                                      
         LA    R6,IO               TEST FOR LOW LEVEL ACCOUNT                   
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   NOT10                                                            
*                                                                               
         XC    NOTTABD(NOTTABL),NOTTABD                                         
         MVC   NOTACC,CHDKACT                                                   
         LA    R6,IO                                                            
         MVI   ELCODE,NAMELQ      GET NAME ELEMENT                              
         BAS   RE,GETEL                                                         
         LA    R3,NOTNAME                                                       
         BAS   RE,GETNAME                                                       
         CLC   =C'VACATION',NOTNAME TEST FOR VACATION ACCOUNT                   
         BNE   *+10                                                             
         MVC   VACACC,CHDKLDG      YES-EXTRACT LEDGER + ACCOUNT CODE            
*                                                                               
         LA    R2,1(R2)            INCREMENT ENTRY COUNT                        
         LA    R5,NOTTABL(R5)                                                   
         LA    RF,NOTMAX                                                        
         CR    R2,RF                                                            
         BNH   NOT10                                                            
         DC    H'0'                                                             
*                                                                               
NOTX     ST    R2,NNOTS                                                         
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO BUILD A CLIENT NAME TABLE                            *         
***********************************************************************         
         SPACE 1                                                                
CLI      NTR1                                                                   
         LA    R4,SVKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY(ACCORFST),SPACES                                          
         MVC   ACTKCPY,RCCOMPFL                                                 
         L     R6,ADCMPEL                                                       
         USING CPYELD,R6                                                        
         MVC   ACTKUNT(2),CPYPROD                                               
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         CLC   IOKEY(3),SVKEY            ARE THE 2 KEYS THE SAME?               
         BE    *+6                         THEY MUST BE                         
         DC    H'0'                        IF NOT - DIE                         
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)    GET RECORD                             
         LA    R6,IO                                                            
         MVI   ELCODE,ACLELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLELD,R6                                                        
         MVC   CLILEN,ACLVALS     GET CLIENT LENGTH                             
         SR    R2,R2               R2=ENTRY COUNTER                             
         L     R5,ACLITAB          R5=A(CLIENT ENTRY)                           
         USING CLITABD,R5                                                       
*                                                                               
CLI10    LA    R4,SVKEY            BUMP TO NEXT CLIENT                          
         SR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         LA    R1,ACTKCULA+3(R1)                                                
         MVI   0(R1),X'FF'                                                      
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   IOKEY(3),SVKEY      TEST SAME LEDGER                             
         BNE   CLIX                NO-ALL DONE                                  
         MVC   SVKEY,IOKEY         RESET KEY                                    
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         LA    R4,IO               R4=A(CLIENT RECORD)                          
         MVC   CLICODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   CLICODE(0),ACTKACT  EXTRACT CLIENT CODE                          
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,NAMELQ      GET NAME ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   CLI10               NONE-SKIP RECORD                             
*                                                                               
         LA    R3,WORK                                                          
         BAS   RE,GETNAME                                                       
         GOTO1 CHOPPER,DMCB,(36,WORK),(L'CLINAME,CLINAME),(C'P',1),0            
         LA    R2,1(R2)                                                         
         LA    R5,CLITABL(R5)      NEXT TABLE ENTRY                             
         C     R2,MAXCLI           TEST FOR TABLE OVERFLOW                      
         BL    CLI10               NO                                           
         DC    H'0'                                                             
*                                                                               
CLIX     ST    R2,NCLI             SAVE ENTRY COUNT                             
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD A TABLE OF MONTHS IN REQUEST                  *          
* OUTPUTS ARE MONSTART,MONEND, MONTHTAB=TABLE, NMONTHS=N'TABLE       *          
* ENTRIES                                                            *          
**********************************************************************          
         SPACE 1                                                                
BLDMON   NTR1                                                                   
         SR    R3,R3               R3=COUNTER                                   
         LA    R5,MONTHTAB         R5=A(OUTPUT)                                 
         MVC   FULL(2),MONSTART                                                 
         MVI   FULL+2,1            SET DAY                                      
*                                                                               
BLDM10   MVC   0(L'MONTHTAB,R5),FULL SET MONTH                                  
         GOTO1 DATCON,DMCB,(1,FULL),(0,WORK)                                    
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,1    ADD ONE MONTH                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,FULL)                                  
*                                                                               
         LA    R5,L'MONTHTAB(R5)                                                
         LA    R3,1(R3)            INCREMENT MONTH COUNT                        
         CH    R3,=Y(NMTHS)                                                     
         BNL   *+14                                                             
         CLC   FULL(2),MONEND      TEST IF END REACHED                          
         BNH   BLDM10              NO                                           
         STC   R3,NMONTHS          SAVE COUNT OF MONTHS                         
*                                                                               
BLDMX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO SET SWITCH IF BURSON RUN                                *          
**********************************************************************          
         SPACE 1                                                                
SETBUR   NTR1                                                                   
         LA    RF,BURAGYS                                                       
         XC    ISBURSON,ISBURSON                                                
         LA    R0,NBURSONS                                                      
SETBUR10 CLC   RCCOMPFL,0(RF)                                                   
         BNE   SETBUR20                                                         
         MVI   ISBURSON,C'Y'                                                    
         MVI   PRAVAIL,C'N'                                                     
         MVI   PRAGYHRS,C'Y'                                                    
SETBUR20 LA    RF,1(RF)                                                         
         BCT   R0,SETBUR10                                                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CLOSE OUT ANY OPEN BOXES                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING BOXD,R4                                                          
BXBOT    NTR1                                                                   
         L     R4,ADBOX                                                         
         ZIC   RF,LINE                                                          
         LA    RE,BOXROWS(RF)                                                   
         BCTR  RE,0                                                             
         MVI   0(RE),C'B'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R6,DISP2,ELCODE                                                  
         EJECT                                                                  
**********************************************************************          
* GETEL # 2                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R6,DATADISP,ELCODE,2                                            
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(QSORT)                                                         
         DC    V(GETPER)                                                        
         DC    V(GETSTD)                                                        
*                                                                               
         DC    A(NOTTAB)                                                        
         DC    A(BUFFALOC)                                                      
         DC    A(PER)              BUILD PERSON TABLE FROM 1R LEDGER            
         DC    A(CAL)              OLD CALEDAR                                  
         DC    A(BUILDPRO)         BUILD PROFILES                               
         DC    A(SETCALFL)         SET NEW CALENDAR                             
         DC    A(NEWCAL)           NEW CALENDAR                                 
         DC    A(MONPER)           MONTH PERCENTAGE                             
         DC    A(DEPT)             PRINT DEPARTMENT SUMMARY                     
         DC    A(WEEKLY)           DEVELOP WKLY STD HRS                         
         DC    A(SETBUCKS)         SET THE PERBUCKS                             
         DC    A(SETAHRS)          SET THE AGENCY HOURS                         
         DC    A(CALCTOT)          SET THE YEARLY/MONTHLY TOTAL                 
         DC    A(PRTEMP)           PRINT EMPLOYEE SUMMARY                       
*                                                                               
REPTAB   DS    0XL2                                                             
         DC    C'A',AL1(REPDET)                                                 
         DC    C'B',AL1(REPEMP)                                                 
         DC    C'C',AL1(REPDPT)                                                 
         DC    C'D',AL1(REPWK)                                                  
         DC    C'E',AL1(REPEMP+REPDPT)                                          
         DC    C'F',AL1(REPDET+REPWK)                                           
         DC    C'G',AL1(REPDET+REPDPT)                                          
         DC    C'H',AL1(REPDET+REPEMP)                                          
         DC    C'I',AL1(REPDPT+REPWK)                                           
         DC    C'J',AL1(REPDET+REPWKMT)                                         
         DC    C'K',AL1(REPEMP+REPMTMT)                                         
         DC    C'L',AL1(REPWKMT)                                                
         DC    C'M',AL1(REPMTMT)                                                
         DC    C'N',AL1(REPDET+REPMTMT+REPWKMT)                                 
         DC    C'Z',AL1(REPDET+REPEMP+REPDPT+REPWK+REPWKMT+REPMTMT)             
         DC    X'FF',AL1(REPDPT)   DEFAULT                                      
*                                                                               
BURAGYS  DC    AL1(BURSON,DDS3,DDSB,BURTST)                                     
NBURSONS EQU   *-BURAGYS                                                        
BURSON   EQU   X'94'                                                            
BURTST   EQU   X'6C'                                                            
DDS3     EQU   X'EF'                                                            
DDSB     EQU   X'DB'                                                            
*                                                                               
DETDISPS DC    AL4(DETWEEK1-DETD)                                               
         DC    AL4(DETWEEK2-DETD)                                               
         DC    AL4(DETWEEK3-DETD)                                               
         DC    AL4(DETWEEK4-DETD)                                               
         DC    AL4(DETWEEK5-DETD)                                               
         DC    AL4(DETMON-DETD)                                                 
DETCOLS  EQU   (*-DETDISPS)/4                                                   
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
DTEMDISP DC    AL4(EMPMON1-EMPSUMD)                                             
         DC    AL4(EMPMON2-EMPSUMD)                                             
         DC    AL4(EMPMON3-EMPSUMD)                                             
         DC    AL4(EMPMON4-EMPSUMD)                                             
         DC    AL4(EMPMON5-EMPSUMD)                                             
         DC    AL4(EMPMON6-EMPSUMD)                                             
         DC    AL4(EMPMON7-EMPSUMD)                                             
         DC    AL4(EMPMON8-EMPSUMD)                                             
         DC    AL4(EMPMON9-EMPSUMD)                                             
         DC    AL4(EMPMONA-EMPSUMD)                                             
         DC    AL4(EMPMONB-EMPSUMD)                                             
         DC    AL4(EMPMONC-EMPSUMD)                                             
DTEPCOLS EQU   (*-DTEMDISP)/4                                                   
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R8,R9,RB                                                         
         EJECT                                                                  
**********************************************************************          
* DUMP ROUTINES                                                      *          
**********************************************************************          
         SPACE 1                                                                
DUMP     DS    0H                                                               
         NMOD1 0,**DUMP**                                                       
         L     RC,0(R1)            RESET RC                                     
         L     R2,4(R1)            MESSAGE                                      
         L     R3,8(R1)            FIELD TO PRINT                               
         L     R4,12(R1)           LENGTH OF FIELD                              
         GOTO1 =V(PRNTBL),DMCB,(L'MSG,(R2)),(R3),C'DUMP',(R4),=C'2D',(CX        
               'P',PRINT)                                                       
         XIT1                                                                   
*                                                                               
* THESE TWO LINES ARE THE CALL TO DUMP                                          
*        MVC   MSG,=CL10'ACC LAST'                                              
*        GOTO1 =A(DUMP),DMCB,(RC),MSG,APERSON,PERTABL                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE DEPARTMENT SUMMARY REPORT.  THIS ROUTINE  *          
* ALSO GENERATES THE WEEKLY DEPT SUMMARY (RCSUBPRG=3)                *          
* AND THE EMPLOYEE METRICS SUMMARY (RCSUBPRG=4)                      *          
* AND THE WEEKLY VERSION OF THE EMPLOYEE METRICS SUMMARY (RCSUBPRG=5)*          
**********************************************************************          
         SPACE 1                                                                
DEPT     DS    0H                                                               
         NMOD1 0,**DEPT**,R8,R9                                                 
         L     RC,0(R1)                                                         
         MVI   FORCEHED,C'Y'                                                    
         USING BUCKSD,R4                                                        
         L     R4,ABUCKS                                                        
         LA    R1,REQBIL                                                        
         LA    R0,REQNBUCK                                                      
         BAS   RE,INITBUCK                                                      
*                                                                               
         USING SUMRECD,R7                                                       
         LA    R7,SUMREC                                                        
         XC    SUMREC,SUMREC                                                    
         MVC   SUMPER,SPACES       SET UP TO READ FIRST PERSON                  
         XC    LEVSCDE(LEVSDSC),LEVSCDE   CLEAR LEVEL DESCRIPTIONS              
         XC    LASTBUF,LASTBUF                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SUMREC,1                             
         TM    DMCB+8,X'80'        TEST FOR EOF                                 
         BZ    DEPT20              NO                                           
         B     DEPTX               YES-EXIT SINCE NO DATA                       
*                                                                               
DEPT10   GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SUMREC,1                             
         TM    DMCB+8,X'80'                                                     
         BO    DEPT180             EOF                                          
*                                                                               
         XC    TOTSTAT,TOTSTAT                                                  
*                                                                               
* PRODUCE TOTALS AND REFRESH HEADERS, IF NEEDED                                 
*                                                                               
DEPT20   BAS   RE,SETCDE           SET LEVEL CODES                              
         LA    R2,SUMPER           TEST FOR CHANGE IN LEVEL C                   
         LA    R3,LASTBUF                                                       
         GOTO1 COMP1R,DMCB,LEVC    COMPARE FOR LEVEL C                          
         BE    DEPT60              NO TOTALS NEEDED                             
*                                                                               
         OC    LASTBUF,LASTBUF     TEST FIRST RECORD                            
         BZ    *+8                                                              
         BAS   RE,SUBTOT           NO-GENERATE PREVIOUS SUB-DEPT TOTAL          
         LA    R1,SUBBIL           CLEAR THE BUCKETS                            
         LA    R0,SUBNBUCK                                                      
         BAS   RE,INITBUCK                                                      
*                                                                               
         USING ACTRECD,R5                                                       
         LA    R5,SVKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY    GET NAME                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         LA    R2,ACTKACT                                                       
         LA    R3,SUMPER                                                        
         GOTO1 MOVE1R,DMCB,LEVC                                                 
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,COMPLEN          GET  LENGTH                                  
         AH    R1,=H'3'            ADD 3 FOR CUL FOR EX                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),ACTKEY     COMPARE KEY WITH RETURNED KEY                
         BE    *+6                 HAS TO BE EQUAL                              
         DC    H'0'                IF NOT - DIE                                 
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)    READ RECORD                            
         LA    R6,IO              IO AREA                                       
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL3                                                        
         LA    R3,LEVCNME          EXTRACT NAME                                 
         BAS   RE,GETNM2                                                        
*                                                                               
         LA    R2,SUMPER                                                        
         LA    R3,LASTBUF                                                       
         GOTO1 COMP1R,DMCB,LEVB                                                 
         BE    DEPT50                                                           
*                                                                               
         OC    LASTBUF,LASTBUF                                                  
         BZ    *+12                                                             
         OI    TOTSTAT,TOTDPT                                                   
         BAS   RE,DEPTOT           PRINT PREVIOUS DEPT TOTALS                   
         LA    R1,DEPBIL                                                        
         LA    R0,DEPNBUCK                                                      
         BAS   RE,INITBUCK                                                      
         MVI   FORCEHED,C'Y'       BREAK PAGE ON DEPT CHANGE                    
         XC    TOTSTAT,TOTSTAT                                                  
*                                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY    GET NAME                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         LA    R2,ACTKACT                                                       
         LA    R3,SUMPER                                                        
         GOTO1 MOVE1R,DMCB,LEVB                                                 
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         SR    R1,R1                                                            
         IC    R1,COMPLEN          GET  LENGTH                                  
         AH    R1,=H'3'            ADD 3 FOR CUL FOR EX                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),ACTKEY     COMPARE KEY WITH RETURNED KEY                
         BE    *+6                 HAS TO BE EQUAL                              
         DC    H'0'                IF NOT - DIE                                 
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)    READ RECORD                            
         LA    R6,IO               IO AREA                                      
         MVI   ELCODE,NAMELQ       GET NAME ELEMENT                             
         BAS   RE,GETEL3                                                        
         LA    R3,LEVBNME          EXTRACT NAME                                 
         BAS   RE,GETNM2                                                        
*                                                                               
         LA    R2,SUMPER           TEST FOR CHANGE IN LEVEL A                   
         LA    R3,LASTBUF                                                       
         GOTO1 COMP1R,DMCB,LEVA    COMPARE FOR LEVEL A                          
         BE    DEPT50              NO                                           
*                                                                               
         OC    LASTBUF,LASTBUF                                                  
         BZ    DEPT30                                                           
         MVI   FORCEHED,C'N'                                                    
         OI    TOTSTAT,TOTOFF                                                   
         BAS   RE,OFFTOT           PRINT PREVIOUS OFFICE TOTALS                 
         MVI   FORCEHED,C'Y'                                                    
         XC    TOTSTAT,TOTSTAT                                                  
*                                                                               
DEPT30   MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY    GET NAME                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         LA    R2,ACTKACT                                                       
         LA    R3,SUMPER                                                        
         GOTO1 MOVE1R,DMCB,LEVA                                                 
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         SR    R1,R1                                                            
         IC    R1,COMPLEN          GET  LENGTH                                  
         AH    R1,=H'3'            ADD 3 FOR CUL FOR EX                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),SVKEY      COMPARE KEY WITH RETURNED KEY                
         BE    *+6                 HAS TO BE EQUAL                              
         DC    H'0'                IF NOT - DIE                                 
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)    READ RECORD                            
         LA    R6,IO               IO AREA                                      
         MVI   ELCODE,NAMELQ       GET NAME ELEMENT                             
         BAS   RE,GETEL3                                                        
         LA    R3,LEVANME          EXTRACT NAME                                 
         BAS   RE,GETNM2                                                        
*                                                                               
         TM    FLGCST,FLGNCST      ARE WE RUNNING ON NEW COST?                  
         BZ    DEPT40              NO - DON'T GO TO CALENDAR                    
         GOTO1 ANEWCAL,DMCB,(RC),LEVACDE                                        
DEPT40   LA    R1,OFFBIL                                                        
         LA    R0,OFFNBUCK                                                      
         BAS   RE,INITBUCK                                                      
*                                                                               
* DO FIRST LEVEL C, IF NEEDED                                                   
*                                                                               
DEPT50   LA    R2,SUMPER           TEST FOR CHANGE IN LEVEL C                   
         LA    R3,LASTBUF                                                       
         GOTO1 COMP1R,DMCB,LEVC   COMPARE FOR LEVEL C                           
         BE    DEPT60             NO                                            
*                                                                               
         GOTO1 ACREPORT                                                         
         LA    R2,P                                                             
         USING DSD,R2                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'LEVCDSC),LEVCDSC    LEVEL C DESCRIPTION                   
         MVC   WORK+L'LEVCDSC+19(L'LEVCCDE),LEVCCDE                             
*                                                                               
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         MVC   DSSTAFF+1(DSBOX1-DSSTAFF-1),WORK                                 
         GOTO1 ACREPORT                                                         
         DROP  R2                                                               
*                                                                               
* SET PERDATA FROM PERTAB                                                       
*                                                                               
DEPT60   MVC   LASTBUF,SUMPER              SAVE BUFFALO KEY                     
         MVC   LSTLEVS(LEVSDSC),LEVSCDE    UPDATE PREVIOUS LEVELS               
         MVC   PERCODE,SUMPER              SET 1R (PERSON) ACCOUNT CODE         
         L     R3,NPER                                                          
         GOTO1 BINSRCH,DMCB,SUMPER,APERTAB,(R3),PERTABL,PERKEYL,0               
         CLI   0(R1),0             TEST IF KEY FOUND                            
         BNE   DEPT170                                                          
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,0(R1)                                                         
         MVC   PERNAME,PERRNAME    GET REPORTING NAME                           
         MVC   PERDINOT,PERINOUT   IN/OUTS                                      
         MVC   PERDHIR,PERHIRE                                                  
         MVC   PERDTRM,PERTERM                                                  
*                                                                               
         CLI   REPMODE,REPMMNTH                                                 
         BE    DEPT80                                                           
*                                                                               
         CLI   RCSUBPRG,3          WEEKLY?                                      
         BE    DEPT70                                                           
         CLI   RCSUBPRG,5                                                       
         BNE   DEPT100                                                          
*                                                                               
DEPT70   LA    R2,MONEND                                                        
         SR    R3,R3               SET N'MONTHS PARM TO 0                       
         B     DEPT90                                                           
*                                                                               
DEPT80   LA    R2,MONTHTAB                                                      
         LA    R3,NMONTHS          SET N'MONTHS PARM TO 0                       
*                                                                               
DEPT90   LA    R0,13                                                            
         LA    R1,EMPAHR                                                        
         BAS   RE,INITBUCK                                                      
*                                                                               
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   DEPT100                                                          
         GOTO1 ASETHRS,DMCB,(RC),EMPAHR,(R3),(R2)                               
         GOTO1 ACALCTOT,DMCB,(RC),EMPAHR                                        
*                                                                               
         GOTO1 NOHRS,EMPAHR       ELIMINATE INACTIVE ACCOUNTS                   
         BE    DEPT170                                                          
*                                                                               
DEPT100  LA    R0,NMTHS                                                         
         LA    R1,EMPSTD                                                        
         LA    RE,PERSTHRS         GET STANDARD HOURS                           
         ZAP   0(L'EMPSTD,R1),0(L'PERSTHRS,RE)                                  
         LA    R1,L'EMPSTD(R1)                                                  
         LA    RE,L'PERSTHRS(RE)                                                
         BCT   R0,*-14                                                          
*                                                                               
         ZAP   THISTARG,PERTARG    GET TARGET PCT                               
         LA    R0,NMTHS                                                         
         LA    R1,EMPTARG                                                       
         LA    RE,PERTARG                                                       
         ZAP   0(L'EMPTARG,R1),0(L'PERTARG,RE)                                  
         LA    R1,L'EMPTARG(R1)                                                 
         LA    RE,L'PERTARG(RE)                                                 
         BCT   R0,*-14                                                          
         DROP  R6                                                               
*                                                                               
         CLI   PRMTHTPC,C'Y'       WAS TARG PCT ENTERED BY MONTH                
         BNE   DEPT110             NO                                           
         SR    R1,R1                                                            
         IC    R1,NMONTHS          ADDRESS LAST MONTHS TARG PCT                 
         BCTR  R1,0                                                             
         MH    R1,=Y(L'EMPTARG)                                                 
         LA    R1,EMPTARG(R1)                                                   
         ZAP   THISTARG,0(L'EMPTARG,R1) USE LAST MONTHS AS GENERAL              
*                                                                               
DEPT110  CLI   REPMODE,REPMWEEK    TEST FOR WEEKLY SUMMARY                      
         BNE   DEPT120             NO                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NMONTHS          ADDRESS LAST MONTHS STD HRS                  
         BCTR  R1,0                                                             
         MH    R1,=Y(L'EMPSTD)                                                  
         LA    R1,EMPSTD(R1)                                                    
         ZAP   THISSTD,0(L'EMPSTD,R1) EXTRACT MONTH'S STD HOURS                 
*                                                                               
         LA    R0,13                                                            
         LA    R1,EMPSTD                                                        
         BAS   RE,INITBUCK         CLEAR OUT THE STD BUCKETS                    
*                                                                               
         GOTO1 AWEEKLY,DMCB,(RC)       COMPUTE THE WEEKLY HOURS                 
         MVC   EMPSTD(8*L'EMPSTD),PERSHR                                        
*                                                                               
DEPT120  BAS   RE,ADJSTD           ADJUST STANDARD HOURS FOR VACATION           
         LA    R0,13               CHARGEABLE BUCKETS                           
         LA    R1,EMPBIL           INITIALIZE BILLABLE AND                      
         BAS   RE,INITBUCK                                                      
*                                                                               
         LA    R0,13                                                            
         LA    R1,EMPRTE                                                        
         BAS   RE,INITBUCK                                                      
*                                                                               
         LA    R0,13                                                            
         LA    R1,EMPNOT                                                        
         BAS   RE,INITBUCK                                                      
*                                                                               
         XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMBIL      GET BILLABLE HOURS TOTAL REC                 
         MVI   SUMACC,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'        TEST NOT FOUND                               
         BNZ   DEPT130             YES                                          
*                                                                               
         GOTO1 ASETBKS,DMCB,(RC),EMPBIL                                         
*                                                                               
         GOTO1 ADDBUCK,DMCB,EMPBIL,SUBBIL                                       
         GOTO1 ADDBUCK,DMCB,EMPBIL,DEPBIL                                       
         GOTO1 ADDBUCK,DMCB,EMPBIL,OFFBIL                                       
         GOTO1 ADDBUCK,DMCB,EMPBIL,REQBIL                                       
*                                                                               
DEPT130  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMRTE      GET R TIME TOTAL RECORD                      
         MVI   SUMACC,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'                                                     
         BNZ   DEPT140                                                          
*                                                                               
         GOTO1 ASETBKS,DMCB,(RC),EMPRTE                                         
*                                                                               
         GOTO1 ADDBUCK,DMCB,EMPRTE,SUBRTE                                       
         GOTO1 ADDBUCK,DMCB,EMPRTE,DEPRTE                                       
         GOTO1 ADDBUCK,DMCB,EMPRTE,OFFRTE                                       
         GOTO1 ADDBUCK,DMCB,EMPRTE,REQRTE                                       
*                                                                               
DEPT140  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMNOT      GET N TIME TOTAL RECORD                      
         MVI   SUMACC,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'                                                     
         BNZ   DEPT150                                                          
*                                                                               
         GOTO1 ASETBKS,DMCB,(RC),EMPNOT                                         
*                                                                               
         GOTO1 ADDBUCK,DMCB,EMPNOT,SUBNOT                                       
         GOTO1 ADDBUCK,DMCB,EMPNOT,DEPNOT                                       
         GOTO1 ADDBUCK,DMCB,EMPNOT,OFFNOT                                       
         GOTO1 ADDBUCK,DMCB,EMPNOT,REQNOT                                       
*                                                                               
DEPT150  GOTO1 ACALCTOT,DMCB,(RC),EMPSTD                                        
         GOTO1 ADDBUCK,DMCB,EMPSTD,SUBSTD                                       
         GOTO1 ADDBUCK,DMCB,EMPSTD,DEPSTD                                       
         GOTO1 ADDBUCK,DMCB,EMPSTD,OFFSTD                                       
         GOTO1 ADDBUCK,DMCB,EMPSTD,REQSTD                                       
*                                                                               
         GOTO1 ADDBUCK,DMCB,EMPAHR,SUBAHR                                       
         GOTO1 ADDBUCK,DMCB,EMPAHR,DEPAHR                                       
         GOTO1 ADDBUCK,DMCB,EMPAHR,OFFAHR                                       
         GOTO1 ADDBUCK,DMCB,EMPAHR,REQAHR                                       
*                                                                               
         CLI   RCSUBPRG,4          EMPLOYEE METRIC RUN                          
         BE    DEPT160                                                          
         CLI   RCSUBPRG,5          EMPLOYEE METRIC RUN                          
         BE    DEPT160                                                          
         BAS   RE,PRTPER           PRINT THE PERSON'S SUMMARY                   
         B     DEPT170                                                          
*                                                                               
DEPT160  BAS   RE,PRTMETRC         PRINT THE EMPLOYEE METRIC                    
*                                                                               
DEPT170  XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,X'FF'       TOTALS LINE                                  
         B     DEPT10              BUMP TO NEXT ACCOUNT                         
*                                                                               
DEPT180  BAS   RE,SUBTOT           PRINT LAST SUB-TOTALS                        
         BAS   RE,DEPTOT                                                        
         BAS   RE,OFFTOT                                                        
         BAS   RE,REQTOT                                                        
*                                                                               
DEPTX    XIT1                                                                   
         DROP  R4,R7                                                            
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
*     R7 = A(SUMREC)                                                 *          
**********************************************************************          
         SPACE 1                                                                
SETCDE   NTR1                                                                   
         MVC   LEVSCDE(LVCDLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,SUMREC           FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R2),0(R1)                                                    
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO ELIMINATE INACTIVE ACCOUNTS                         *          
**********************************************************************          
         SPACE 1                                                                
NOHRS    NTR1                                                                   
         LA    R0,NMTHS                                                         
         LA    R2,NMTHS*BUCKLN(R1)    ADDRESS YEARLY TOTAL                      
         CLI   REPMODE,REPMWEEK       IS THIS A WEEKLY                          
         BNE   *+12                   NO, DO YEAR TOTALS                        
         LA    R0,NWKS                                                          
         LA    R2,NWKS*BUCKLN(R1)                                               
*                                                                               
         CP    0(BUCKLN,R2),=P'0'                                               
         B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE 1R ACCOUNT SUMMARY                        *          
**********************************************************************          
         SPACE 1                                                                
PRTPER   NTR1                                                                   
         LA    RE,EMPSTD                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,EMPAHR                                                        
         ST    RE,ASTANDRD                                                      
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R2,P                                                             
         USING DSD,R2                                                           
         SR    R0,R0                                                            
         IC    R0,OFFSET                                                        
         SR    R1,R1                                                            
         IC    R1,PERSLEN                                                       
         LA    R5,PERCODE                                                       
         AR    R5,R0                                                            
         EX    R1,*+4                                                           
         MVC   DSSTAFF(0),0(R5)                                                 
         MVC   DSNAME,PERNAME                                                   
         EDIT  (P8,THISTARG),(5,DSTARG),2                                       
*                                                                               
         TM    TIMETYPE,BTIME      WANT B TIME                                  
         BNO   PRTPER10            NO                                           
         MVI   DSTYPE,C'B'                                                      
         GOTO1 AMONPER,DMCB,(RC),EMPBIL,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
*                                                                               
PRTPER10 TM    TIMETYPE,RTIME                                                   
         BNO   PRTPERX                                                          
         MVI   DSTYPE,C'R'                                                      
         GOTO1 AMONPER,DMCB,(RC),EMPRTE,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
*                                                                               
PRTPERX  B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE 1R ACCOUNT METRIC                         *          
* ON ENTRY, RCSUBPRG=4 - MONTHLY VERSION                             *          
*           RCSUBPRG=5 - WEEKLY VERSION                              *          
**********************************************************************          
         SPACE 1                                                                
PRTMETRC NTR1                                                                   
         LA    RE,EMPSTD                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,EMPAHR                                                        
         ST    RE,ASTANDRD                                                      
*                                                                               
         LA    R2,P                                                             
         USING PMETRICD,R2                                                      
         IC    R0,OFFSET                                                        
         SR    R1,R1                                                            
         IC    R1,PERSLEN                                                       
         LA    R5,PERCODE                                                       
         AR    R5,R0                                                            
         EX    R1,*+4                                                           
         MVC   PMSTAFF(0),0(R5)                                                 
         MVC   PMNAME,PERNAME                                                   
*                                                                               
         LA    R5,PMMNOFF          PRINT OFFSET FOR MONTHLY REPORT              
         LA    R4,PMMNMET          PRINT OFFSET FOR "METRIC" COL                
         CLI   RCSUBPRG,5          IS THIS THE WEEKLY                           
         BNE   PRTM10              NO                                           
         EDIT  (P8,THISTARG),(5,PMTARG),2                                       
         LA    R5,PMWKOFF                                                       
         LA    R4,PMWKMET                                                       
*                                                                               
PRTM10   TM    METRFILT,MFPROD                                                  
         BNO   PRTM20                                                           
         MVC   0(6,R4),=C'PROD %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,EMPRTE                                                    
         GOTO1 ADDTME,EMPBIL                                                    
         GOTO1 ADDBUCK,DMCB,EMPBIL,DENOM                                        
         GOTO1 ADDBUCK,DMCB,EMPRTE,DENOM                                        
         GOTO1 ADDBUCK,DMCB,EMPNOT,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
PRTM20   TM    METRFILT,MFBILL                                                  
         BNO   PRTM30                                                           
         MVC   0(6,R4),=C'BILL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,EMPBIL                                                    
         GOTO1 ADDBUCK,DMCB,ASTANDRD,DENOM                                      
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
PRTM30   TM    METRFILT,MFUTIL                                                  
         BNO   PMX                                                              
         MVC   0(6,R4),=C'UTIL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,EMPRTE                                                    
         GOTO1 ADDTME,EMPBIL                                                    
         GOTO1 ADDTME,EMPNOT                                                    
         GOTO1 ADDBUCK,DMCB,EMPAHR,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
         CLI   RCSUBPRG,5          IS THIS THE WEEKLY                           
         BE    PMX                 YES, TARG % IS A COL                         
         MVC   0(6,R4),=C'TARG %'                                               
         MVI   SPACING,2                                                        
*                                                                               
         GOTO1 ACALCTOT,DMCB,(RC),EMPTARG                                       
         GOTO1 DPTOUT,DMCB,EMPTARG,0,0,(R5)                                     
PMX      B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* MOVE 1R ACCOUNT AT SPECIFIED LEVEL WITH LAST ACCOUNT               *          
*      AT ENTRY : R2 = A(CODE FIELD TO MOVE INTO)                    *          
*               : R3 = A(LEVEL CODE)                                 *          
*               : R7 = A(SUMREC)                                     *          
*         P1    : LENGTH                                             *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
MOVE1R   NTR1                                                                   
         L     R5,0(R1)                                                         
         SR    R1,R1                                                            
         ICM   R1,1,0(R5)                                                       
         BZ    MOVE1RX            NO LEVEL - EXIT                               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),0(R3)       LEVEL CODE                                   
         STC   R1,COMPLEN          SAVE LENGTH FOR LATER USE                    
MOVE1RX  B     DEPTX                                                            
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* COMPARE 1R ACCOUNT AT SPECIFIED LEVEL WITH LAST ACCOUNT            *          
*      AT ENTRY : R2 = A(COMPARE FIELD)                              *          
*               : R3 = A(COMPARE FIELD)                              *          
*               : R7 = A(SUMREC)                                     *          
*         P1    : LENGTH                                             *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
COMP1R   NTR1                                                                   
         L     R5,0(R1)            COMPARE FOR LEVEL                            
         SR    R1,R1                                                            
         ICM   R1,1,0(R5)                                                       
         BZ    COMPX               NO LEVEL - EXIT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     COMPX                                                            
         CLC   0(0,R2),0(R3)      TEST FOR CHANGE IN LEVEL C                    
*                                                                               
COMPX    B     DEPTX               EXIT WITH CONDITION CODE                     
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO INITIALIZE BUCKETS                                  *          
**********************************************************************          
         SPACE 1                                                                
INITBUCK NTR1                                                                   
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO ADD ONE ROW OF MONTHLY BUCKETS INTO ANOTHER         *          
* AT ENTRY, P1=A(INPUT BUCKETS) P2=A(BUCKETS TO ADD INTO)            *          
**********************************************************************          
         SPACE 1                                                                
ADDBUCK  NTR1                                                                   
         LA    R0,13                                                            
         LM    RE,RF,0(R1)                                                      
*                                                                               
ADDBK10  AP    0(8,RF),0(8,RE)                                                  
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,ADDBK10                                                       
         B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO ADJUST THE STANDARD HOURS BUCKETS FOR VACATION      *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
ADJSTD   NTR1                                                                   
         CLI   VACOPT,C'Y'         TEST TO DEDUCT VACATN FROM STANDARD          
         BNE   ADJSTDX                                                          
         OC    VACACC,VACACC       TEST FOR A VACATION ACCOUNT                  
         BZ    ADJSTDX             NO                                           
*                                                                               
         XC    SUMRECD(SUMKEYL),SUMRECD                                         
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMNOT      LOOK FOR VACATION RECORD                     
         MVC   SUMACC,VACACC                                                    
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'                                                     
         BNZ   ADJSTDX                                                          
*                                                                               
         LA    R0,NMTHS            R0=LOOP COUNTER                              
         LA    R1,EMPSTD           R1=A(STANDARD HOURS DATA)                    
         LA    RE,SUMMONTH         RE=A(VACATION MONTHLY BUCKETS)               
         CLI   RCSUBPRG,2          TEST FOR DEPT SUMMARY                        
         BE    ADJSTD10            YES                                          
         LA    R0,5                                                             
         LA    RE,SUMWEEK                                                       
*                                                                               
ADJSTD10 SP    0(L'EMPSTD,R1),0(L'SUMMONTH,RE)                                  
         BP    *+10                                                             
         ZAP   0(L'EMPSTD,R1),=P'0' DO NOT ALLOW NEGATIVE RESULT                
         LA    R1,L'EMPSTD(R1)                                                  
         LA    RE,L'SUMMONTH(RE)                                                
         BCT   R0,ADJSTD10                                                      
*                                                                               
ADJSTDX  B     DEPTX                                                            
         DROP R7                                                                
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE SUB-DEPARTMENT TOTALS                     *          
**********************************************************************          
         SPACE 1                                                                
SUBTOT   NTR1                                                                   
         GOTO1 ACREPORT                                                         
         USING BUCKSD,R6                                                        
         L     R6,ABUCKS                                                        
         LA    RE,SUBSTD                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,SUBAHR                                                        
         ST    RE,ASTANDRD                                                      
*                                                                               
         LA    R2,P                                                             
         USING DSD,R2                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK+3(5),=C'TOTAL'                                              
         MVC   WORK+9(6),LSTCCDE                                                
         MVC   WORK+16(36),LSTCNME                                              
*                                                                               
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         MVC   DSSTAFF+1(DSBOX1-DSSTAFF-1),WORK                                 
*                                                                               
         CLI   RCSUBPRG,4                                                       
         BE    SUBT20                                                           
         CLI   RCSUBPRG,5                                                       
         BE    SUBT20                                                           
*                                                                               
         TM    TIMETYPE,BTIME      TEST TIME FILTERING                          
         BNO   SUBT10              NO                                           
*                                                                               
         MVI   DSTYPE,C'B'                                                      
         GOTO1 AMONPER,DMCB,(RC),SUBBIL,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
*                                                                               
SUBT10   TM    TIMETYPE,RTIME                                                   
         BNO   SUBTX                                                            
*                                                                               
         MVI   DSTYPE,C'R'                                                      
         GOTO1 AMONPER,DMCB,(RC),SUBRTE,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
         B     SUBTX                                                            
*                                                                               
         USING PMETRICD,R2                                                      
SUBT20   LA    R5,PMMNOFF          PRINT OFFSET FOR MONTHLY REPORT              
         LA    R4,PMMNMET          PRINT OFFSET FOR "METRIC" COL                
         CLI   RCSUBPRG,5          IS THIS THE WEEKLY                           
         BNE   *+12                                                             
         LA    R5,PMWKOFF                                                       
         LA    R4,PMWKMET                                                       
*                                                                               
         TM    METRFILT,MFPROD                                                  
         BNO   SUBT30                                                           
         MVC   0(6,R4),=C'PROD %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,SUBRTE                                                    
         GOTO1 ADDTME,SUBBIL                                                    
         GOTO1 ADDBUCK,DMCB,SUBBIL,DENOM                                        
         GOTO1 ADDBUCK,DMCB,SUBRTE,DENOM                                        
         GOTO1 ADDBUCK,DMCB,SUBNOT,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
SUBT30   TM    METRFILT,MFBILL                                                  
         BNO   SUBT40                                                           
         MVC   0(6,R4),=C'BILL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,SUBBIL                                                    
         GOTO1 ADDBUCK,DMCB,ASTANDRD,DENOM                                      
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
SUBT40   TM    METRFILT,MFUTIL                                                  
         BNO   SUBTX                                                            
         MVC   0(6,R4),=C'UTIL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,SUBRTE                                                    
         GOTO1 ADDTME,SUBBIL                                                    
         GOTO1 ADDTME,SUBNOT                                                    
         GOTO1 ADDBUCK,DMCB,SUBAHR,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
SUBTX    B     DEPTX                                                            
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE DEPARTMENT TOTALS                         *          
**********************************************************************          
         SPACE 1                                                                
DEPTOT   NTR1                                                                   
         GOTO1 ACREPORT                                                         
         USING BUCKSD,R6                                                        
         L     R6,ABUCKS                                                        
         LA    RE,DEPSTD                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,DEPAHR                                                        
         ST    RE,ASTANDRD                                                      
*                                                                               
         LA    R2,P                                                             
         USING DSD,R2                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK+3(5),=C'TOTAL'                                              
         MVC   WORK+9(6),LSTBCDE                                                
         MVC   WORK+16(36),LSTBNME                                              
*                                                                               
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         MVC   DSSTAFF+1(DSBOX1-DSSTAFF-1),WORK                                 
*                                                                               
         CLI   RCSUBPRG,4                                                       
         BE    DEPTOT30                                                         
         CLI   RCSUBPRG,5                                                       
         BE    DEPTOT30                                                         
*                                                                               
         TM    TIMETYPE,BTIME                                                   
         BNO   DEPTOT20                                                         
*                                                                               
         MVI   DSTYPE,C'B'                                                      
         GOTO1 AMONPER,DMCB,(RC),DEPBIL,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
*                                                                               
DEPTOT20 TM    TIMETYPE,RTIME                                                   
         BNO   DEPTOTX                                                          
*                                                                               
         MVI   DSTYPE,C'R'                                                      
         GOTO1 AMONPER,DMCB,(RC),DEPRTE,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
         B     DEPTOTX                                                          
*                                                                               
         USING PMETRICD,R2                                                      
DEPTOT30 LA    R5,PMMNOFF          PRINT OFFSET FOR MONTHLY REPORT              
         LA    R4,PMMNMET          PRINT OFFSET FOR "METRIC" COL                
         CLI   RCSUBPRG,5          IS THIS THE WEEKLY                           
         BNE   *+12                                                             
         LA    R5,PMWKOFF                                                       
         LA    R4,PMWKMET                                                       
*                                                                               
         MVC   0(6,R4),=C'PROD %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,DEPRTE                                                    
         GOTO1 ADDTME,DEPBIL                                                    
         GOTO1 ADDBUCK,DMCB,DEPBIL,DENOM                                        
         GOTO1 ADDBUCK,DMCB,DEPRTE,DENOM                                        
         GOTO1 ADDBUCK,DMCB,DEPNOT,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
         MVC   0(6,R4),=C'BILL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,DEPBIL                                                    
         GOTO1 ADDBUCK,DMCB,ASTANDRD,DENOM                                      
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
         MVC   0(6,R4),=C'UTIL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,DEPRTE                                                    
         GOTO1 ADDTME,DEPBIL                                                    
         GOTO1 ADDTME,DEPNOT                                                    
         GOTO1 ADDBUCK,DMCB,DEPAHR,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
DEPTOTX  B     DEPTX                                                            
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE OFFICE TOTALS                             *          
**********************************************************************          
         SPACE 1                                                                
OFFTOT   NTR1                                                                   
         GOTO1 ACREPORT                                                         
         USING BUCKSD,R6                                                        
         L     R6,ABUCKS                                                        
         LA    RE,OFFSTD                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,OFFAHR                                                        
         ST    RE,ASTANDRD                                                      
*                                                                               
         USING DSD,R2                                                           
         LA    R2,P                                                             
         MVC   WORK,SPACES                                                      
         MVC   WORK+3(5),=C'TOTAL'                                              
         MVC   WORK+9(6),LSTACDE                                                
         MVC   WORK+16(36),LSTANME                                              
*                                                                               
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         MVC   DSSTAFF+1(DSBOX1-DSSTAFF-1),WORK                                 
*                                                                               
         CLI   RCSUBPRG,4                                                       
         BE    OFFT20                                                           
         CLI   RCSUBPRG,5                                                       
         BE    OFFT20                                                           
*                                                                               
         TM    TIMETYPE,BTIME                                                   
         BNO   OFFT10                                                           
*                                                                               
         MVI   DSTYPE,C'B'                                                      
         GOTO1 AMONPER,DMCB,(RC),OFFBIL,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
*                                                                               
OFFT10   TM    TIMETYPE,RTIME                                                   
         BNO   OFFTX                                                            
*                                                                               
         MVI   DSTYPE,C'R'                                                      
         GOTO1 AMONPER,DMCB,(RC),OFFRTE,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
         B     OFFTX                                                            
*                                                                               
         USING PMETRICD,R2                                                      
OFFT20   LA    R5,PMMNOFF          PRINT OFFSET FOR MONTHLY REPORT              
         LA    R4,PMMNMET          PRINT OFFSET FOR "METRIC" COL                
         CLI   RCSUBPRG,5          IS THIS THE WEEKLY                           
         BNE   *+12                NO                                           
         LA    R5,PMWKOFF                                                       
         LA    R4,PMWKMET                                                       
*                                                                               
         MVC   0(6,R4),=C'PROD %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,OFFRTE                                                    
         GOTO1 ADDTME,OFFBIL                                                    
         GOTO1 ADDBUCK,DMCB,OFFBIL,DENOM                                        
         GOTO1 ADDBUCK,DMCB,OFFRTE,DENOM                                        
         GOTO1 ADDBUCK,DMCB,OFFNOT,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
         MVC   0(6,R4),=C'BILL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,OFFBIL                                                    
         GOTO1 ADDBUCK,DMCB,ASTANDRD,DENOM                                      
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
         MVC   0(6,R4),=C'UTIL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,OFFRTE                                                    
         GOTO1 ADDTME,OFFBIL                                                    
         GOTO1 ADDTME,OFFNOT                                                    
         GOTO1 ADDBUCK,DMCB,OFFAHR,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
OFFTX    B     DEPTX                                                            
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE REQUEST TOTALS                            *          
**********************************************************************          
         SPACE 1                                                                
REQTOT   NTR1                                                                   
         GOTO1 ACREPORT                                                         
         USING BUCKSD,R6                                                        
         L     R6,ABUCKS                                                        
         LA    RE,REQSTD                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,REQAHR                                                        
         ST    RE,ASTANDRD                                                      
*                                                                               
         USING DSD,R2                                                           
         LA    R2,P                                                             
         MVC   WORK,SPACES                                                      
         MVC   DSSTAFF+1(17),=C'TOTAL FOR REQUEST'                              
*                                                                               
         CLI   RCSUBPRG,4                                                       
         BE    REQT20                                                           
         CLI   RCSUBPRG,5                                                       
         BE    REQT20                                                           
*                                                                               
         TM    TIMETYPE,BTIME                                                   
         BNO   REQT10                                                           
*                                                                               
         MVI   DSTYPE,C'B'                                                      
         GOTO1 AMONPER,DMCB,(RC),REQBIL,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
*                                                                               
REQT10   TM    TIMETYPE,RTIME                                                   
         BNO   REQTX                                                            
*                                                                               
         MVI   DSTYPE,C'R'                                                      
         GOTO1 AMONPER,DMCB,(RC),REQRTE,ASTANDRD,MONPCT                         
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,DETPOFF                             
         B     REQTX                                                            
*                                                                               
         USING PMETRICD,R2                                                      
REQT20   LA    R5,PMMNOFF          PRINT REQSET FOR MONTHLY REPORT              
         LA    R4,PMMNMET          PRINT OFFSET FOR "METRIC" COL                
         CLI   RCSUBPRG,5          IS THIS THE WEEKLY                           
         BNE   *+12                NO                                           
         LA    R5,PMWKOFF                                                       
         LA    R4,PMWKMET                                                       
*                                                                               
         MVC   0(6,R4),=C'PROD %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,REQRTE                                                    
         GOTO1 ADDTME,REQBIL                                                    
         GOTO1 ADDBUCK,DMCB,REQBIL,DENOM                                        
         GOTO1 ADDBUCK,DMCB,REQRTE,DENOM                                        
         GOTO1 ADDBUCK,DMCB,REQNOT,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
         MVC   0(6,R4),=C'BILL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,REQBIL                                                    
         GOTO1 ADDBUCK,DMCB,ASTANDRD,DENOM                                      
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
*                                                                               
         MVC   0(6,R4),=C'UTIL %'                                               
         LA    R0,13                                                            
         LA    R1,NUMER                                                         
         BAS   RE,INITBUCK                                                      
         LA    R0,13                                                            
         LA    R1,DENOM                                                         
         BAS   RE,INITBUCK                                                      
*                                                                               
         GOTO1 ADDTME,REQRTE                                                    
         GOTO1 ADDTME,REQBIL                                                    
         GOTO1 ADDTME,REQNOT                                                    
         GOTO1 ADDBUCK,DMCB,REQAHR,DENOM                                        
         GOTO1 AMONPER,DMCB,(RC),NUMER,DENOM,MONPCT                             
         GOTO1 DPTOUT,DMCB,MONPCT,YEARPCT,1,(R5)                                
REQTX    B     DEPTX                                                            
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE TO OUTPUT AN DEPARTMENT SUMMARY PRINT LINE              *          
* AT ENTRY, P1=A(BUCKETS TO PRINT) P2=A(YEAR TOTAL)                  *          
*           P3=0 FOR 2 DECIMAL PLACES, 1=1 DECIMAL PLACE             *          
*           P4=OFFSET INTO P TO START BUCKETS                        *          
* NOTE TOTAL IS 13TH BUCK FOR MONTHLY OR 6TH FOR WEEKLY              *          
**********************************************************************          
         SPACE 1                                                                
DPTOUT   NTR1                                                                   
         L     R4,0(R1)                                                         
         MVC   BYTE,11(R1)                                                      
         L     R3,12(R1)                                                        
         LA    R3,P(R3)                                                         
         XC    FLGSET,FLGSET       FLAG FOR PERIOD COUNT                        
         SR    R6,R6                                                            
         IC    R6,NMONTHS                                                       
         LA    R2,NMTHS*BUCKLN(R4) ADDRESS YEARLY TOTAL                         
         CLI   REPMODE,REPMWEEK                                                 
         BNE   DPTOUT10                                                         
         SR    R6,R6                                                            
         ICM   R6,1,DTENUM         ACTUAL # OF PERIODS IN MONTH(NEWCAL)         
         BNZ   *+8                                                              
         LA    R6,5                HARCODE FOR NOW UNTIL I FIX REPORT           
         LA    R2,NWKS*BUCKLN(R4)  ADDRESS MONTH TOTAL                          
*                                                                               
         CH    R6,=H'5'            IF # OF PERIODS < 5                          
         BNL   *+8                                                              
         OI    FLGSET,FLGPERD      SET FLAG                                     
*                                                                               
DPTOUT10 XC    EBLOCK,EBLOCK                                                    
         ST    R4,EBAIN            R4=A(NUMBER TO EDIT)                         
         MVI   EBTIN,C'P'                                                       
         MVI   EBLIN,8                                                          
         ST    R3,EBAOUT                                                        
         MVI   EBLOUT,6                                                         
         MVI   EBDECS,2                                                         
         CLI   BYTE,1                                                           
         BNE   *+8                                                              
         MVI   EBDECS,1                                                         
         OI    EBOPT,EBOQZEN                                                    
         CP    0(8,R4),=P'0'                                                    
         BNL   *+8                                                              
         OI    EBOPT,EBOQMEY                                                    
         GOTO1 ADEDITOR,DMCB,EBLOCK                                             
*                                                                               
         LA    R4,8(R4)            NEXT BUCKET                                  
         LA    R3,7(R3)                                                         
         BCT   R6,DPTOUT10                                                      
*                                                                               
         TM    FLGSET,FLGPERD      ARE THERE 5 PERIODS?                         
         BZ    *+8                 YES - LEAVE OUTPUT ALONE                     
         LA    R3,7(R3)            NO  - SKIP TO NEXT POSITION                  
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         ST    R2,EBAIN                                                         
         MVI   EBTIN,C'P'                                                       
         MVI   EBLIN,8                                                          
         ST    R3,EBAOUT                                                        
         MVI   EBLOUT,5                                                         
         MVI   EBDECS,2                                                         
         CLI   BYTE,1                                                           
         BNE   *+8                                                              
         MVI   EBDECS,1                                                         
         OI    EBOPT,EBOQZEN                                                    
         CP    0(8,R2),=P'0'                                                    
         BNL   *+8                                                              
         OI    EBOPT,EBOQMEY                                                    
         GOTO1 ADEDITOR,DMCB,EBLOCK                                             
*                                                                               
         GOTO1 ACREPORT                                                         
         B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* GET NAME FROM NAME ELEMENT                                         *          
*        R6 IS ADDRESS OF THE 20 ELEMENT                             *          
*        R3 IS ADDRESS OF 36 BYTE AREA                               *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R6                                                        
GETNM2   NTR1                                                                   
         CLI   NAMEL,NAMELQ        ARE WE AT THE 20 ELEMENT?                    
         BE    *+6                 YES - CONTINUE                               
         DC    H'0'                NO - DIE!                                    
*                                                                               
         MVC   0(L'NAMEREC,R3),SPACES                                           
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   0(0,R3),NAMEREC                                                  
         B     DEPTX                                                            
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* ADD CORRECT TIME ROUTINES                                          *          
*     R TIME - RATES                                                 *          
*     B TIME - BILLING                                               *          
*     N TIME - NOT                                                   *          
**********************************************************************          
         SPACE 1                                                                
ADDTME   NTR1                                                                   
         LR    R2,R1                                                            
         GOTO1 ADDBUCK,DMCB,(R2),NUMER                                          
         B     DEPTX                                                            
         EJECT                                                                  
**********************************************************************          
* GETEL # 3                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R6,DISP2,ELCODE,3                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R2,R5,R8,R9,RB                                                   
         EJECT                                                                  
**********************************************************************          
* SET AGENCY HOURS                                                   *          
*     NOTE:  PERDATA MUST BE SET                                     *          
*     P1  = PL8 BUCKET TO SET                         (R2)           *          
*     P2 0=WEEKLY, OR A(NMONTHLY BUCKETS TO SET)      (R3)           *          
*     P3  = A(X'YYMM') MONTH  (OR MONTH LIST)         (R4)           *          
**********************************************************************          
         SPACE 1                                                                
SETAHRS  DS    0H                                                               
         NMOD1 GPOPLENQ,**STHR**                                                
         LR    R5,RC                                                            
         L     RC,0(R1)                                                         
         LM    R2,R4,4(R1)                                                      
         LA    R0,NWKS+1           ASSUME WEEKLY BUCKS                          
         LTR   R3,R3               IS N'MONTHS SET                              
         BZ    *+8                 NO, WEEKLY IT IS                             
         LA    R0,NMTHS+1          CLEAR MONTHLY BUCKETS                        
         LR    RE,R2               GET A(BUCKETS)                               
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LTR   R3,R3               IS N'MONTHS SET                              
         BZ    SETAH10             NO, CALC WEEKLY  VALUE                       
         SR    R6,R6                                                            
         IC    R6,0(R3)                                                         
SETAH10  GOTO1 VGETPER,DMCB,(R4),ACALKEYS,(R5),0,ACALREC                        
         USING GPOPD,R5                                                         
         SR    R1,R1                                                            
         IC    R1,GPONPER                                                       
         LA    RF,GPOPERS                                                       
*                                                                               
         USING GPOPERD,RF                                                       
SETAH20  DS    0H                                                               
         BAS   RE,NWRK             ADJUST GPOPNWRK FOR IN-OUT                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,GPOPNWRK         NUMBER OF WORKING DAYS                       
         CVD   RE,DUB                                                           
         MP    DUB,=P'800'                                                      
         AP    0(8,R2),DUB                                                      
*                                                                               
         LTR   R3,R3               IS THIS WEEKLY                               
         BNZ   *+8                 NO, ACCUMULATE IN ONE BUCK                   
         LA    R2,8(R2)                                                         
         LA    RF,GPOPLN1Q(RF)                                                  
         BCT   R1,SETAH20          GET NEXT PERIOD                              
*                                                                               
         LTR   R3,R3               IS THIS WEEKLY                               
         BZ    SETAHX              YES, ALL DONE                                
*                                                                               
         LA    R4,2(R4)            GET NEXT MONTH                               
         LA    R2,8(R2)            GET NEXT BUCKET                              
         BCT   R6,SETAH10                                                       
*                                                                               
SETAHX   XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* FOR THE PERIOD IN THE GPOPERD BLOCK AT 0(RF), CALCULATE THE        *          
* NUMBER OF WORKING DAYS THE EMPLOYEE IN PERDATA WAS ACTIVE          *          
**********************************************************************          
         SPACE 1                                                                
         USING GPOPERD,R4                                                       
NWRK     NTR1                                                                   
         LR    R4,RF               ADDRESS OF GPOPERD DATA                      
         LA    R2,PERDINOT         ADDRESS EMPLOYEES INOUTS                     
         LA    R0,PERLMAX          MAX INOUTS                                   
         SR    R3,R3               ACCUM IN R3                                  
*                                                                               
*        BAS   RE,DUMPPERD                                                      
*        BAS   RE,DUMPINOT                                                      
*                                                                               
* FOR THE 2 START DATES, TAKE THE HIGHEST                                       
*                                                                               
NWRK40   LA    R5,GPOPSTRT                                                      
         CLC   GPOPSTRT,0(R2)                                                   
         BH    *+8                                                              
         LA    R5,0(R2)                                                         
*                                                                               
         LA    R1,CALSTR                                                        
         CLI   REPMODE,REPMWEEK                                                 
         BNE   *+8                                                              
         LA    R1,CALMSTR          USE LAST MONTH START DATE                    
         CLC   0(3,R1),0(R5)                                                    
         BNH   *+6                                                              
         LR    R5,R1                                                            
*                                                                               
* FOR THE 2 END DATES, TAKE THE LOWEST                                          
*                                                                               
         LA    R6,GPOPEND                                                       
         CLC   GPOPEND,3(R2)                                                    
         BL    *+8                                                              
         LA    R6,3(R2)                                                         
*                                                                               
         LA    R1,CALEND                                                        
         CLI   REPMODE,REPMWEEK                                                 
         BNE   *+8                                                              
         LA    R1,CALMEND          USE LAST MONTH START DATE                    
         CLC   0(3,R1),0(R6)                                                    
         BNL   *+6                                                              
         LR    R6,R1                                                            
*                                                                               
         CLC   0(3,R5),0(R6)       IS THIS A VALID PERIOD (ST <= END)           
         BH    NWRK50                                                           
*                                                                               
         GOTO1 =V(GETNWRK),DMCB,(R5),(R6),0                                     
         L     R1,DMCB                                                          
         AR    R3,R1               ACCUMULATE NWORKING                          
*                                                                               
NWRK50   LA    R2,6(R2)            NEXT INOUT GROUP                             
         BCT   R0,NWRK40                                                        
         STC   R3,GPOPNWRK         SAVE IN GPOP BLOCK                           
         BAS   RE,DUMPPERD                                                      
         B     SETAHX                                                           
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* DUMP ROUTINES                                                      *          
**********************************************************************          
         SPACE 1                                                                
DUMPPERD NTR1                                                                   
         CLC   QUESTOR(6),=C'DUMPEL'                                            
         BNE   SETAHX                                                           
         LA    R6,=C'PERD'                                                      
         LA    R2,GPOPLN1Q                                                      
         GOTO1 =V(PRNTBL),DMCB,(4,(R6)),(R4),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
         B     SETAHX                                                           
*                                                                               
DUMPINOT NTR1                                                                   
         CLC   QUESTOR(6),=C'DUMPEL'                                            
         BNE   SETAHX                                                           
         LA    R6,=C'INOT'                                                      
         LA    R3,18                                                            
         GOTO1 =V(PRNTBL),DMCB,(4,(R6)),(R2),C'DUMP',(R3),=C'2D',(C'P',X        
               PRINT)                                                           
         B     SETAHX                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R5,RB                                                            
         EJECT                                                                  
***********************************************************************         
* SET THE PERBUCK IN R1 WITH:                                         *         
*     SUMWEEK DATA WHEN REPMODE=WEEK                                  *         
*     SUMMONTH DATA, WHEN REPMODE=MNTH                                *         
* TOTALS BUCKETS IN EITHER 8*8(R1) OR 12*8(R1)                        *         
***********************************************************************         
         SPACE 1                                                                
         USING SUMRECD,R7                                                       
SETBUCKS DS    0H                                                               
         NMOD1 0,**STBK**                                                       
         L     RC,0(R1)                                                         
         LA    R7,SUMREC                                                        
         L     RF,4(R1)            CLEAR ALL BUCKS TO START                     
         LR    R1,RF               SAVE ADDRESS                                 
         LA    R0,13                                                            
         ZAP   0(BUCKLN,RF),=P'0'                                               
         LA    RF,BUCKLN(RF)                                                    
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,NWKS             N'WEEKLY BUCKETS                             
         LA    R2,NWKS*BUCKLN(R1)  WEEKLY TOTAL ADDRESS                         
         LA    R4,SUMWEEK          WEEKLY AMOUNTS FROM BUFFALO                  
         CLI   REPMODE,REPMWEEK    IS THIS A WEEKLY REPORT                      
         BE    SETBK10             YES                                          
         LA    R0,NMTHS            SET MONTHLY ADDRESSES                        
         LA    R2,NMTHS*BUCKLN(R1)                                              
         LA    R4,SUMMONTH                                                      
*                                                                               
SETBK10  ZAP   0(BUCKLN,R1),0(BUCKLN,R4)                                        
         AP    0(BUCKLN,R2),0(BUCKLN,R4)     ACCUMULATE TOTAL                   
         LA    R1,BUCKLN(R1)                                                    
         LA    R4,BUCKLN(R4)                                                    
         BCT   R0,SETBK10                                                       
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO DEVELOP THE WEEKLY STANDARD HOURS TO 2              *          
* DECIMAL PLACES                                                     *          
*         R6 = A(PERSON TABLE ON ENTRY)                              *          
**********************************************************************          
         SPACE 1                                                                
         USING PERTABD,R6                                                       
WEEKLY   DS    0H                                                               
         NMOD1 0,**WKLY**                                                       
         L     RC,0(R1)            RESET RC                                     
         TM    PERTSTAT,PERTSTD    NEW STANDARD HOURS IN USE                    
         BNO   WKLY20              NO, CALC WEEKLY AMOUNTS                      
*                                                                               
         LA    R0,NWKS             EXTRACT NEW STD HRS VALUES                   
         LA    R4,PERSHR                                                        
         LA    R3,PERWKSTD                                                      
         CLI   REPMODE,REPMWEEK                                                 
         BE    WKLY10                                                           
*                                                                               
         LA    R0,NMTHS            EXTRACT NEW STD HRS VALUES                   
         LA    R3,PERSTHRS                                                      
WKLY10   ZAP   0(L'PERSHR,R4),0(L'PERWKSTD,R3)                                  
         LA    R3,L'PERWKSTD(R3)                                                
         LA    R4,L'PERSHR(R4)                                                  
         BCT   R0,WKLY10                                                        
         B     WKLYX                                                            
*                                                                               
WKLY20   LA    R0,NWKS             R0=LOOP COUNTER                              
         SR    RE,RE                                                            
         IC    RE,NWORKING         RE=N'WORKING DAYS IN MONTH                   
         ZAP   DUB,THISSTD                                                      
         CVB   RF,DUB              RF=N'STANDARD HOURS IN MONTH                 
         LA    R1,WEEKTAB          R1=A(WEEKLY WORKING DAYS TABLE)              
         LA    R4,PERSHR           R4=A(WEEKLY STANDARD HOURS)                  
*                                                                               
WKLY30   SR    R3,R3                                                            
         IC    R3,0(R1)            GET WORKING DAYS FOR WEEK                    
         MR    R2,RF               * STANDARD HOURS FOR MONTH                   
         SLDA  R2,1                                                             
         DR    R2,RE               ROUNDED DIVIDE FOR RATIO BY                  
         LTR   R3,R3               N'WORKING DAYS IN MONTH                      
         BM    *+8                                                              
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         CVD   R3,DUB                                                           
         ZAP   0(L'PERSHR,R4),DUB                                               
*                                                                               
         LA    R1,L'WEEKTAB(R1)                                                 
         LA    R4,L'PERSHR(R4)                                                  
         BCT   R0,WKLY30                                                        
*                                                                               
WKLYX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO CALCULATE THE OF THE BUCKETS IN 0(R1)               *          
**********************************************************************          
         SPACE 1                                                                
CALCTOT  DS    0H                                                               
         NMOD1 0,**CALC**                                                       
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
         LA    R0,NMTHS                                                         
         LA    R2,NMTHS*BUCKLN(R3) ADDRESS YEARLY TOTAL                         
         CLI   REPMODE,REPMWEEK    IS THIS A WEEKLY                             
         BNE   *+12                NO, DO YEAR TOTALS                           
         LA    R0,NWKS                                                          
         LA    R2,NWKS*BUCKLN(R3)                                               
*                                                                               
         ZAP   0(BUCKLN,R2),=P'0'                                               
         AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R3,BUCKLN(R3)                                                    
         BCT   R0,*-10                                                          
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* SET CALSTR AND CALEND, CALENDER DATE TIMESHEET FILTERS             *          
* SET CALMSTR AND CALMEND X'YMD' PERIOD START DATES AND PERIOD END   *          
* DATES OF LAST MONTH OF CALENDER REQUEST                            *          
* SET MONSTART TO X'YM' OF CAL MON START, IF PRESENT                 *          
* SET MONEND TO X'YM' OF CAL MON END, IF PRESENT                     *          
**********************************************************************          
         SPACE 1                                                                
SETCALFL DS    0H                                                               
         NMOD1 GPOPLENQ,**CAFL**                                                
         LR    R5,RC               USE R5 FOR LWS (GETPER CALL)                 
         L     RC,0(R1)            RESTORE R6'S RC                              
*                                                                               
         TM    FLGCST,FLGNCST      ARE WE RUNNING ON NEW COST?                  
         BZ    SETCX               NO - EXIT                                    
*                                                                               
         XC    CALSTR,CALSTR                                                    
         MVC   CALEND,=XL3'FFFFFF'                                              
         XC    CALMSTR,CALMSTR                                                  
         MVC   CALMEND,=XL3'FFFFFF'                                             
         USING ACQD,R3                                                          
         L     R3,ADQSTACK         REQUEST CARD STACK                           
         CLI   ACQTYP1,ACQDATE     DATE FILTER?                                 
         BNE   SETCX               NOPE                                         
         CLI   ACQFLT1,ACQDTPER    CALENDER PERIOD REQUEST                      
         BNE   SETCX               NOPE                                         
*                                                                               
         CLC   ACQDTSTR,SPACES     ANY START DATE                               
         BNH   SETC20              NO, CHECK END DATE                           
         MVI   DATESARE,CALDATES                                                
         CLI   ACQDTSTR+4,C' '     SPECIFICE DATE REQUEST                       
         BNH   SETC10              NO, GET START DATE FROM CALENDER             
*                                                                               
         GOTO1 DATCON,DMCB,(0,ACQDTSTR),(1,CALSTR)                              
         GOTO1 SCGETCAL,DMCB,CALSTR,MONSTART,ACALREC                            
         B     SETC20              CHECK END DATE                               
*                                                                               
SETC10   MVC   WORK(4),ACQDTSTR                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,PBDDATE)                                 
         LA    R4,PBDDATE                                                       
         GOTO1 VGETPER,DMCB,(R4),ACALKEYS,(R5),0,ACALREC                        
         USING GPOPD,R5                                                         
         MVC   CALSTR,GPOSTRT     SET START DATE                                
         MVC   MONSTART,PBDDATE   AND REPORT START DATE '                       
*                                                                               
SETC20   CLC   ACQDTEND,SPACES     ANY END DATE                                 
         BNH   SETCX               NOPE, DONE                                   
*                                                                               
         CLI   ACQDTEND+4,C' '     SPECIFICE DATE REQUEST                       
         BNH   SETC30              NO, GET START DATE FROM CALENDER             
         GOTO1 DATCON,DMCB,(0,ACQDTEND),(1,CALEND)                              
         GOTO1 SCGETCAL,DMCB,CALEND,MONEND,ACALREC                              
*                                                                               
* SET START AND END DATES FOR LAST MONTH (CALMSTR AND CALMEND)                  
*                                                                               
         GOTO1 VGETPER,DMCB,MONEND,ACALKEYS,(R5),0,ACALREC                      
         USING GPOPD,R5                                                         
*                                                                               
         MVC   CALMSTR,GPOSTRT     USE PERIOD START, UNLESS REQ START           
         CLC   CALSTR,GPOSTRT      IS HIGHER                                    
         BNH   *+10                                                             
         MVC   CALMSTR,CALSTR                                                   
*                                                                               
         MVC   CALMEND,GPOEND      USE PERIOD END, UNLESS REQ END IS            
         CLC   CALEND,GPOEND       LOWER                                        
         BNL   *+10                                                             
         MVC   CALMEND,CALEND                                                   
         B     SETCX               DONE                                         
*                                                                               
SETC30   MVC   WORK(4),ACQDTEND                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,PBDDATE)  CONVERT TO X'YM'               
         LA    R4,PBDDATE                                                       
*        GETPER USES WORK!!!                                                    
         GOTO1 VGETPER,DMCB,(R4),ACALKEYS,(R5),0,ACALREC                        
         USING GPOPD,R5                                                         
         MVC   CALEND,GPOEND       SET END DATE                                 
         MVC   MONEND,PBDDATE      AND REPORT END MONTH                         
         MVC   CALMEND,GPOEND                                                   
         MVC   CALMSTR,GPOSTRT                                                  
*                                                                               
SETCX    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* P1 = DATE TO FIND MONTH FOR                                        *          
* P2 A (A X'YYMM' RETURN MONTH                                       *          
* P3 = A(CALENDER RECORD BUFFER)                                     *          
**********************************************************************          
         SPACE 1                                                                
SCGETCAL NTR1                                                                   
         L     R5,0(R1)            A(YYMM TO GET CAL FOR)                       
         L     R8,4(R1)            A(RETURN MONTH)                              
         L     R9,8(R1)            A(CALENDER RECORD BUFFER)                    
*                                                                               
         LR    R6,R9                                                            
         BAS   RE,CHKCASP          IS THIS CALENDER RECORD THERE                
         BE    SCGET10             YES                                          
         BAS   RE,GETCASP          GET KEY                                      
         LA    R6,SVKEY                                                         
         BAS   RE,CHKCASP          CHECK KEY                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)  READ CALENDER RECORD                     
         LA    R6,IO                                                            
         LR    RE,R9               SAVE IN CALENDER BUFFER                      
         LA    RF,IOLNQ                                                         
         LR    R0,R6                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
SCGET10  LA    R6,ACCRFST-ACCKEY(R6)                                            
*                                                                               
SCGET20  CLI   0(R6),0                                                          
         BE    SCGETX                                                           
         CLI   0(R6),TMPELQ        PERIOD ELEMENT                               
         BNE   SCGET30                                                          
*                                                                               
* RETURN MONTH OF THIS DATE IN P2                                               
*                                                                               
         USING TMPELD,R6                                                        
         MVC   0(2,R8),TMPMTH      DEFAULT TO HIGHEST MATCH                     
         CLC   TMPSTART,0(R5)      DOES THIS START AFTER MY MONTH               
         BH    SCGET30             YES                                          
         CLC   TMPEND,0(R5)        DOES THIS END AFTER MY MONTH                 
         BL    SCGET30             YES                                          
*        MVC   0(2,R8),TMPMTH      RETURN MONTH OF THIS DATE IN P2              
         B     SCGETX                                                           
*                                                                               
SCGET30  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     SCGET20                                                          
*                                                                               
SCGETX   B     SETCX                                                            
         EJECT                                                                  
**********************************************************************          
* BUILD KEY AND READ CALENDAR RECORD                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING CASRECD,R6                                                       
GETCASP  NTR1                                                                   
         LA    R6,SVKEY                                                         
         XC    CASPAS,CASPAS       BUILD PASSIVE POINTER TO CAL REC             
         MVI   CASPTYP,CASPTYPQ                                                 
         MVI   CASPSUB,CASPSUBQ                                                 
         MVC   CASPCPY,RCCOMPFL                                                 
         MVC   CASPEDTE,0(R5)                   P1 IS DATE TO READ FOR          
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                    READ HIGH              
         CLC   IOKEY((CASPCPY-CASPAS)+L'CASPCPY),SVKEY   SAME CPY?              
         BE    *+6                                                              
         DC    H'0'                DIE                                          
         B     SETCX                                                            
         EJECT                                                                  
**********************************************************************          
* CHECK CALENDAR KEY                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING CASRECD,R6                                                       
CHKCASP  NTR1                                                                   
         CLC   CASPEDTE,0(R5)      DID I GET A DATE LOWER THAN I READ           
         BNL   *+8                                                              
         B     CCASPNO                                                          
*                                                                               
         CLC   CASPSDTE,0(R5)      ANY CALENDER FOR THIS DATE                   
         BNH   CCASPOK             YES                                          
*                                                                               
CCASPNO  CR    R4,RB                                                            
         B     *+6                                                              
CCASPOK  CR    RB,RB                                                            
         B     SETCX                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R3,R6,RB                                                         
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PRINT THE EMPLOYEE SUMMARY REPORT                   *          
* AT ENTRY, PERCODE CONTAINS 1R ACCOUNT CODE FOR REPORT              *          
**********************************************************************          
         SPACE 1                                                                
PRTEMP   DS    0H                                                               
         NMOD1 0,**PTM**,R8                                                     
         L     RC,0(R1)            RESET RC                                     
         USING SUMRECD,R7                                                       
         LA    R7,SUMREC                                                        
         XC    SUMREC,SUMREC                                                    
         USING EMPSUMD,R2                                                       
         LA    R2,P                                                             
*                                                                               
         LA    RE,PERSHR                                                        
         CLI   PRAGYHRS,C'Y'                                                    
         BNE   *+8                                                              
         LA    RE,PERAHR                                                        
         ST    RE,ASTANDRD                                                      
         LA    RF,NMTHS*BUCKLN(RE)                                              
         ST    RF,ASTANDYR                                                      
*                                                                               
         LA    RE,MONTOT           CLEAR TOTAL AND CHARGEABLE BUCKETS           
         LA    R0,13*2             LOOP COUNTER                                 
         ZAP   0(L'MONTOT,RE),=P'0'                                             
         LA    RE,L'MONTOT(RE)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMTOT      GET THE TOTALS FIRST                         
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'        TEST NOT FOUND                               
         BNZ   PRTMP10             TREAT NOT FOUND AS ZERO RECORD               
*                                                                               
         MVC   MONTOT(MONLNQ),SUMMONTH                                          
         LA    RE,MONTOT           FIND THE YEARLY TOTAL                        
         LA    R0,12                                                            
         AP    YEARTOT,0(L'MONTOT,RE)                                           
         LA    RE,L'MONTOT(RE)                                                  
         BCT   R0,*-10                                                          
*                                                                               
PRTMP10  MVI   SUMTYPE,SUMCHG      GET CHARGEABLE TIME                          
         MVC   LASTBUF,SUMRECD     SAVE THE KEY                                 
         GOTO1 BUFFALO,DMCB,=C'GET',ABUFF,SUMREC,1                              
         TM    DMCB+8,X'90'                                                     
         BNZ   PRTMP20             DID NOT FIND IT                              
*                                                                               
         MVC   MONCHG(MONLNQ),SUMMONTH                                          
         LA    RE,MONCHG                                                        
         LA    R0,12                                                            
         AP    YEARCHG,0(L'MONCHG,RE)                                           
         LA    RE,L'MONCHG(RE)                                                  
         BCT   R0,*-10                                                          
*                                                                               
PRTMP20  MVC   EMPTIT(23),=C'BILLABLE HOURS (B TIME)'                           
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         XC    SUMRECD(SUMKEYL),SUMRECD RE-BUILD THE KEY                        
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMBIL      READ BILLABLE TIME RECORDS                   
         MVC   LASTBUF,SUMRECD     SAVE THE KEY                                 
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SUMREC,1                             
         TM    DMCB+8,X'80'        TEST FOR EOF                                 
         BO    *+14                YES                                          
         CLC   SUMRECD(SUMACC-SUMRECD),LASTBUF                                  
         BE    PRTMP30             FOUND ONE RECORD                             
*                                                                               
         MVC   EMPMON1(4),=C'NONE'                                              
         GOTO1 ACREPORT                                                         
         B     PRTMP60                                                          
*                                                                               
PRTMP30  MVC   EMPACC(L'SUMACC),SUMACC SHOW THE CLIENT CODE                     
         L     R3,NCLI             GET THE CLIENT NAME                          
         GOTO1 BINSRCH,DMCB,SUMACC,ACLITAB,(R3),CLITABL,CLIKEYL,(R3)            
         CLI   0(R1),0             TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         USING CLITABD,RE                                                       
         MVC   EMPACC+4(L'CLINAME),CLINAME                                      
         DROP  RE                                                               
*                                                                               
         MVC   MONBUCK(MONLNQ),SUMMONTH                                         
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
         GOTO1 EMPOUT,DMCB,MONBUCK,YEARBUCK,0                                   
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,SUMREC,1                              
*                                                                               
         CLI   SUMACC,X'FF'        TEST FOR TOTAL RECORD                        
         BE    PRTMP40             YES                                          
         CLC   SUMRECD(SUMACC-SUMRECD),LASTBUF  TEST SAME PERSON                
         BE    PRTMP30             YES-GO BACK AND PROCESS IT                   
*                                                                               
         LA    R1,MONBUCK          NO-ZERO TOTAL RECORD                         
         LA    R0,13                                                            
         BAS   RE,INITBK2                                                       
         B     PRTMP50                                                          
*                                                                               
PRTMP40  MVC   MONBUCK(MONLNQ),SUMMONTH                                         
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
*                                                                               
PRTMP50  GOTO1 ACREPORT                                                         
         MVC   EMPSUB(13),=C'TOTAL B HOURS'                                     
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
         GOTO1 EMPOUT,DMCB,MONBUCK,YEARBUCK,0                                   
*                                                                               
         GOTO1 AMONPER,DMCB,(RC),MONBUCK,MONTOT,MONPCT                          
         MVC   EMPSUB(16),=C'% TO TOTAL HOURS'                                  
         GOTO1 EMPOUT,DMCB,MONPCT,YEARPCT,1                                     
*                                                                               
         GOTO1 AMONPER,DMCB,(RC),MONBUCK,ASTANDRD,MONPCT                        
         MVC   EMPSUB(19),=C'% TO STANDARD HOURS'                               
         GOTO1 EMPOUT,DMCB,MONPCT,YEARPCT,1                                     
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
PRTMP60  MVC   EMPTIT(27),=C'NON-BILLABLE HOURS (R TIME)'                       
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         XC    SUMRECD(SUMKEYL),SUMRECD RE-BUILD THE KEY                        
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMRTE      READ NON-BILLABLE TIME RECS                  
         MVC   LASTBUF,SUMRECD     SAVE THE KEY                                 
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SUMREC,1                             
         TM    DMCB+8,X'80'        TEST FOR EOF                                 
         BO    *+14                YES                                          
         CLC   SUMRECD(SUMACC-SUMRECD),LASTBUF                                  
         BE    PRTMP70             FOUND ONE RECORD                             
*                                                                               
         MVC   EMPMON1(4),=C'NONE'                                              
         GOTO1 ACREPORT                                                         
         B     PRTMP100                                                         
*                                                                               
PRTMP70  MVC   EMPACC(L'SUMACC),SUMACC SHOW THE CLIENT CODE                     
         L     R3,NCLI             GET THE CLIENT NAME                          
         GOTO1 BINSRCH,DMCB,SUMACC,ACLITAB,(R3),CLITABL,CLIKEYL,(R3)            
         CLI   0(R1),0             TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         USING CLITABD,RE                                                       
         MVC   EMPACC+4(L'CLINAME),CLINAME                                      
         DROP  RE                                                               
*                                                                               
         MVC   MONBUCK(MONLNQ),SUMMONTH                                         
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
         GOTO1 EMPOUT,DMCB,MONBUCK,YEARBUCK,0                                   
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,SUMREC,1                              
*                                                                               
         CLI   SUMACC,X'FF'        TEST FOR TOTAL RECORD                        
         BE    PRTMP80             YES                                          
         CLC   SUMRECD(SUMACC-SUMRECD),LASTBUF  TEST SAME PERSON                
         BE    PRTMP70             YES-GO BACK AND PRINT DETAIL                 
*                                                                               
         LA    R1,MONBUCK          NO-ZERO TOTAL RECORD WAS                     
         LA    R0,13               NOT RETURNED                                 
         BAS   RE,INITBK2                                                       
         B     PRTMP90                                                          
*                                                                               
PRTMP80  MVC   MONBUCK(MONLNQ),SUMMONTH                                         
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
*                                                                               
PRTMP90  GOTO1 ACREPORT                                                         
         MVC   EMPSUB(13),=C'TOTAL R HOURS'                                     
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
         GOTO1 EMPOUT,DMCB,MONBUCK,YEARBUCK,0                                   
*                                                                               
         GOTO1 AMONPER,DMCB,(RC),MONBUCK,MONTOT,MONPCT                          
         MVC   EMPSUB(16),=C'% TO TOTAL HOURS'                                  
         GOTO1 EMPOUT,DMCB,MONPCT,YEARPCT,1                                     
*                                                                               
         GOTO1 AMONPER,DMCB,(RC),MONBUCK,ASTANDRD,MONPCT                        
         MVC   EMPSUB(19),=C'% TO STANDARD HOURS'                               
         GOTO1 EMPOUT,DMCB,MONPCT,YEARPCT,1                                     
*                                                                               
PRTMP100 GOTO1 ACREPORT                                                         
         CP    YEARCHG,=P'0'                                                    
         BE    PRTMP110                                                         
*                                                                               
         MVC   EMPSUB(19),=C'TOTAL CHARGEABLE HRS'                              
         GOTO1 ACALCTOT,DMCB,(RC),MONCHG                                        
         GOTO1 EMPOUT,DMCB,MONCHG,YEARCHG,0                                     
*                                                                               
         MVC   EMPSUB(16),=C'% TO TOTAL HOURS'                                  
         GOTO1 AMONPER,DMCB,(RC),MONCHG,MONTOT,MONPCT                           
         GOTO1 EMPOUT,DMCB,MONPCT,YEARPCT,1                                     
*                                                                               
         MVC   EMPSUB(19),=C'% TO STANDARD HOURS'                               
         GOTO1 AMONPER,DMCB,(RC),MONCHG,ASTANDRD,MONPCT                         
         GOTO1 EMPOUT,DMCB,MONPCT,YEARPCT,1                                     
*                                                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTMP110 MVC   EMPTIT,=CL20'NON-CHARGEABLE HOURS'                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         XC    SUMRECD(SUMKEYL),SUMRECD RE-BUILD THE KEY                        
         MVC   SUMPER,PERCODE                                                   
         MVI   SUMTYPE,SUMNOT      READ NON-CHARGEABLE TIME RECS                
         MVC   LASTBUF,SUMRECD     SAVE THE KEY                                 
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,SUMREC,1                             
         TM    DMCB+8,X'80'        TEST FOR EOF                                 
         BO    *+14                YES                                          
         CLC   SUMRECD(SUMACC-SUMRECD),LASTBUF                                  
         BE    PRTMP120            FOUND ONE RECORD                             
*                                                                               
         MVC   EMPMON1(4),=C'NONE'                                              
         GOTO1 ACREPORT                                                         
         B     PRTMP170                                                         
*                                                                               
PRTMP120 CLI   SUMACC,C'J'         TEST FOR CLIENT 'N' TIME                     
         BNE   PRTMP130                                                         
*                                                                               
         MVC   EMPACC(3),SUMACC+1                                               
         L     R3,NCLI             GET THE CLIENT NAME                          
         GOTO1 BINSRCH,DMCB,SUMACC+1,ACLITAB,(R3),CLITABL,CLIKEYL,(R3)          
         CLI   0(R1),0             TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)                                                         
         USING CLITABD,RE                                                       
         MVC   EMPACC+4(L'CLINAME),CLINAME                                      
         B     PRTMP140                                                         
         DROP  RE                                                               
*                                                                               
PRTMP130 L     R0,NNOTS                                                         
         GOTO1 BINSRCH,DMCB,SUMACC+1,ANOTTAB,(R0),NOTTABL,NOTKEYL,(R0)          
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,0(R1)            GET A(ENTRY)                                 
         USING NOTTABD,RE                                                       
         MVC   EMPACC(L'NOTNAME),NOTNAME   EXTRACT 1N NAME                      
         DROP  RE                                                               
*                                                                               
PRTMP140 MVC   MONBUCK(MONLNQ),SUMMONTH                                         
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
         GOTO1 EMPOUT,DMCB,MONBUCK,YEARBUCK,0                                   
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,SUMREC,1                              
*                                                                               
         CLI   SUMACC,X'FF'        TEST FOR TOTAL RECORD                        
         BE    PRTMP150                                                         
         CLC   SUMRECD(SUMACC-SUMRECD),LASTBUF                                  
         BE    PRTMP120                                                         
*                                                                               
         LA    R1,MONBUCK                                                       
         LA    R0,13                                                            
         BAS   RE,INITBK2                                                       
         B     PRTMP160                                                         
*                                                                               
PRTMP150 MVC   MONBUCK(MONLNQ),SUMMONTH                                         
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
*                                                                               
PRTMP160 MVC   EMPSUB(5),=C'TOTAL'                                              
         GOTO1 ACALCTOT,DMCB,(RC),MONBUCK                                       
         GOTO1 EMPOUT,DMCB,MONBUCK,YEARBUCK,0                                   
*                                                                               
PRTMP170 GOTO1 ACREPORT                                                         
         MVC   EMPACC(20),=C'TOTAL REPORTED HOURS'                              
         GOTO1 ACALCTOT,DMCB,(RC),MONTOT                                        
         GOTO1 EMPOUT,DMCB,MONTOT,YEARTOT,0                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   EMPSUB(19),=C'% TO STANDARD HOURS'                               
         GOTO1 AMONPER,DMCB,(RC),MONTOT,ASTANDRD,MONPCT                         
         GOTO1 EMPOUT,DMCB,MONPCT,YEARPCT,1                                     
*                                                                               
         MVC   EMPACC(14),=C'STANDARD HOURS'                                    
         GOTO1 ACALCTOT,DMCB,(RC),ASTANDRD                                      
         GOTO1 EMPOUT,DMCB,ASTANDRD,ASTANDYR,0                                  
*                                                                               
PRTEMPX  XIT1                                                                   
         DROP  R2,R7                                                            
         EJECT                                                                  
**********************************************************************          
* BUCKET ROUTINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
INITBK2  NTR1                                                                   
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     PRTEMPX                                                          
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO OUTPUT AN EMPLOYEE SUMMARY PRINT LINE                *         
* AT ENTRY, R2=A(PRINT LINE), P1=A(BUCKETS TO PRINT) P2=A(YEAR TOTAL) *         
*           P3=2 FOR 2 DECIMALS  1=1 DECIMAL                          *         
***********************************************************************         
         SPACE 1                                                                
EMPOUT   NTR1                                                                   
         USING EMPSUMD,R2                                                       
         L     R4,0(R1)                                                         
         L     R6,4(R1)                                                         
         MVC   BYTE,11(R1)                                                      
         LA    R5,EMPDISPS         R5=A(DISPLACEMENTS INTO PRINT LINE)          
         SR    R0,R0                                                            
         IC    R0,NMONTHS                                                       
*                                                                               
EMPO10   L     R3,0(R5)            DISP TO COLUMN                               
         LA    R3,EMPSUMD(R3)      ADDRESS OF COLUMN                            
         GOTO1 EMPED,(R4)                                                       
         LA    R5,L'EMPDISPS(R5)   NEXT DISPLACEMENT                            
         LA    R4,8(R4)            NEXT BUCKET                                  
         BCT   R0,EMPO10                                                        
*                                                                               
         LA    R3,EMPMON2-EMPMON1+1(R3) O/P POSITION                            
         GOTO1 EMPED,(R6)                                                       
*                                                                               
         GOTO1 ACREPORT                                                         
         B     PRTEMPX                                                          
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO EDIT OUT NUMBERS FOR EMPOUT SUB-ROUTINE             *          
* AT ENTRY, R1=A(INPUT) R3=A(OUTPUT)                                 *          
**********************************************************************          
         SPACE 1                                                                
EMPED    NTR1                                                                   
         XC    EBLOCK,EBLOCK                                                    
         ST    R1,EBAIN            INPUT=8 BYTE PACKED NUMBERS                  
         MVI   EBLIN,8                                                          
         MVI   EBTIN,C'P'                                                       
         ST    R3,EBAOUT                                                        
         MVI   EBLOUT,6                                                         
         MVI   EBDECS,2            DEFAULT IS TWO DECIMAL PLACES                
         CLI   BYTE,1                                                           
         BNE   *+8                                                              
         MVI   EBDECS,1                                                         
         CP    0(8,R1),=P'0'       TEST FOR NEGATIVE NUMBER                     
         BNL   *+8                                                              
         OI    EBOPT,EBOQMEY       YES-ELECT MINUS OPTION                       
         GOTO1 ADEDITOR,DMCB,EBLOCK                                             
         B     PRTEMPX                                                          
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
EMPDISPS DC    AL4(EMPMON1-EMPSUMD)                                             
         DC    AL4(EMPMON2-EMPSUMD)                                             
         DC    AL4(EMPMON3-EMPSUMD)                                             
         DC    AL4(EMPMON4-EMPSUMD)                                             
         DC    AL4(EMPMON5-EMPSUMD)                                             
         DC    AL4(EMPMON6-EMPSUMD)                                             
         DC    AL4(EMPMON7-EMPSUMD)                                             
         DC    AL4(EMPMON8-EMPSUMD)                                             
         DC    AL4(EMPMON9-EMPSUMD)                                             
         DC    AL4(EMPMONA-EMPSUMD)                                             
         DC    AL4(EMPMONB-EMPSUMD)                                             
         DC    AL4(EMPMONC-EMPSUMD)                                             
EMPCOLS  EQU   (*-EMPDISPS)/4                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R8,RB                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO COMPUTE A ROW OF MONTHLY PERCENTAGE BUCKETS         *          
* AT ENTRY, P1=A(DIVIDEND BUCKETS)                                   *          
*           P2 BYTE 0=0 FOR ROW OF BUCKETS, 1=ONE BUCKET             *          
*              BYTES 1-3=A(DIVISOR)                                  *          
*           P3=A(OUTPUT BUCKETS)                                     *          
*           P4=RC (LOCAL)                                            *          
**********************************************************************          
         SPACE 1                                                                
MONPER   DS    0H                                                               
         NMOD1 0,**MNPR**                                                       
         L     RC,0(R1)            RESTORE RC                                   
         LM    R3,R5,4(R1)                                                      
         MVC   BYTE,8(R1)          EXTRACT DIVISOR CHARACTERISTIC               
         LA    R0,13               R0=LOOP COUNTER                              
*                                                                               
MONP10   ZAP   0(BUCKLN,R5),=P'0' INITIALIZE RESULT TO ZERO                     
         ZAP   PL16,0(BUCKLN,R3) GET DIVIDEND                                   
         SRP   PL16,4,0            *1000 *10 FOR ROUNDING                       
         CP    0(BUCKLN,R4),=P'0' TEST FOR ZERO DIVISOR                         
         BE    MONP20                YES                                        
*                                                                               
         DP    PL16,0(BUCKLN,R4)                                                
         SRP   PL16(8),64-1,5      ROUND TO ONE DECIMAL PLACE                   
         ZAP   0(BUCKLN,R5),PL16(8) SET RESULT                                  
*                                                                               
MONP20   LA    R3,BUCKLN(R3)                                                    
         LA    R5,BUCKLN(R5)                                                    
         CLI   BYTE,1              TEST FOR SINGLE DIVISOR                      
         BE    *+8                 YES                                          
         LA    R4,BUCKLN(R4)                                                    
         BCT   R0,MONP10                                                        
*                                                                               
MONPERX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO CREATE A PERSON REPORTING ACCOUNT TABLE             *          
* EMPLOYEES MAY BE MOVED FROM ONE ACCOUNT TO ANOTHER                 *          
* ROUTINE DETERMINES THE ACCOUNT EMPLOYEE TIME SHOULD                *          
* BE REPORTED UNDER IF THERE ARE MULTIPLE ACCOUNTS FOR THE           *          
* SAME PERSON.                                                       *          
**********************************************************************          
         SPACE 1                                                                
PER      DS    0H                                                               
         NMOD1 0,**PER**                                                        
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTFLD,RECTYPE,0                                  
         MVC   SVKEY,SPACES                                                     
         LA    R4,SVKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,QACCOUNT                                                 
*                                                                               
         LA    R1,14               SET KEY COMPARE LENGTH                       
         LA    R0,L'QACCOUNT       R0=LOOP COUNTER                              
         LA    RE,QACCOUNT+L'QACCOUNT-1                                         
         CLI   0(RE),C' '          TEST FOR LAST SIGNIFICANT CHARACTER          
         BH    *+12                                                             
         BCTR  RE,0                BACK UP CHARACTER POINTER                    
         BCTR  R1,0                DECREMENT KEY LENGTH                         
         BCT   R0,*-12                                                          
         STC   R1,COMPLEN                                                       
         B     *+12                                                             
*                                                                               
* READ ALL 1R ACCOUNTS                                                          
*                                                                               
PER10    LA    R4,SVKEY                                                         
         MVI   ACTKEY+ACTKEND,X'FF'   BUMP TO NEXT 1R ACCOUNT                   
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         B     PER30                                                            
*                                                                               
PER20    GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
PER30    SR    R1,R1                                                            
         IC    R1,COMPLEN          GET KEY COMPARE LENGTH                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),SVKEY                                                   
         BNE   PER90               DONE READING THE LEDGER                      
*                                                                               
         LA    R2,LEVD             START AT LOWEST LEVEL                        
         LA    R3,LEVLNQD          INDIVIDUAL LENGTH OF LOWEST                  
         LA    R0,LEVELQ           ASSUME IT IS A 4 LEVEL STRUCTURE             
         SR    R1,R1                                                            
         IC    R1,LEVNUM           ACTUAL LEVEL NUMBER                          
         SR    R0,R1                                                            
         BZ    PER40               IF ZERO - 4 LEVEL STRUCTURE                  
*                                                                               
         SH    R2,=Y(L'LEVELS)     BUMP UP ONE LEVEL OF HEIRARCHY               
         SH    R3,=Y(L'LEVLNQS)    BUMP UP TO PREV INDIVIDUAL LENGTH            
         BCT   R0,*-8                                                           
*                                                                               
PER40    SR    R0,R0                                                            
         IC    R0,0(R2)            R0 = COMBINED LENGTHS OF LEVELS              
         SR    R1,R1                                                            
         IC    R1,0(R3)            R1 = INDIVIDUAL LENGTH OF LEVEL              
         SR    R0,R1               GET DISPLACEMENT                             
         LA    R4,IOKEY                                                         
         LA    R5,ACTKACT                                                       
         AR    R5,R0               BUMP IN KEY                                  
         BCTR  R1,0                DECREMENT LENGTH FOR EX MVC                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      IS LOWEST LEVEL FILLED IN YET?               
         BE    PER20               NO - READ NEXT                               
*                                                                               
         STC   R0,OFFSET           SAVE OFFSET IN ACCOUNT CODE                  
         STC   R1,PERSLEN          SAVE EX LENGTH                               
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         MVC   SVKEY,IOKEY         RESET THE KEY                                
         LA    R3,SORTREC                                                       
         USING SORTD,R3                                                         
         XC    SORTREC,SORTREC     BUILD A SORT RECORD                          
         MVC   SORTRLEN,=Y(SORTRECL)                                            
         MVC   SORTACC,ACTKACT     FULL 1R ACCOUNT CODE                         
         LA    R5,ACTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,OFFSET                                                        
         SR    R1,R1                                                            
         IC    R1,PERSLEN                                                       
         AR    R5,R0                                                            
         EX    R1,*+4                                                           
         MVC   SORTPER(0),0(R5)    STAFF CODE                                   
*                                                                               
         USING NAMELD,R6                                                        
         LA    R6,IO                                                            
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL4           GET THE ACCOUNT NAME                         
         BNE   PER10               NO NAME, MUSTN'T WANT THIS RECORD            
*                                                                               
         MVC   SORTNAME,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   SORTNAME(0),NAMEREC                                              
*                                                                               
         USING EMPELD,R6                                                        
         LA    R6,IO                                                            
         MVI   ELCODE,EMPELQ       X'56' - SEARCH FOR EMPLOYEE ELEMENT          
         BAS   RE,GETEL4                                                        
         BNE   PER80                                                            
*                                                                               
* IWEX, MCOR, AND CBUT SAYS ITS OK AS OF 6/10/91 TO REINSTITUTE CHECK           
* OF HIRE/TERM DATE AGAINST REPORT START/END FOR H+K                            
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
*                                                                               
         CLI   ISBURSON,C'Y'       HIRE/FIRE FOR BURSON DONE @ ACCLAST          
         BNE   PER50                                                            
         OC    EMPTRM,EMPTRM       TEST FOR TERMINATION DATE                    
         BZ    PER70               NONE                                         
         B     PER60                                                            
*                                                                               
PER50    CLC   EMPHIR(2),MONEND    TEST IF HIRED AFTER REPORT END               
         BH    PER10               YES                                          
         OC    EMPTRM,EMPTRM       TEST FOR TERMINATION DATE                    
         BZ    PER70               NONE                                         
         CLC   EMPTRM(2),ACMMSTR   TEST TERMINATED BEFORE REPORT                
         BL    PER10               START                                        
*                                                                               
PER60    MVC   SORTTRM,EMPTRM                                                   
         XC    SORTTRM,=X'FFFFFF'  GET THE COMPLIMENT FOR SORTING               
*                                                                               
PER70    MVC   SORTHIRE,EMPHIR                                                  
         MVC   SORTTERM,EMPTRM                                                  
*                                                                               
PER80    GOTO1 ADSORTER,DMCB,=C'PUT',SORTD                                      
         B     PER10               READ NEXT RECORD                             
*                                                                               
* USE THE SORT FILE TO BUILD A TABLE OF 1R ACCOUNTS AND THEIR                   
* REPORTING ACCOUNTS                                                            
*                                                                               
PER90    L     R5,APERTAB                                                       
         USING PERTABD,R5                                                       
         SR    R2,R2               R2=COUNT OF TABLE ENTRIES                    
         XC    LASTPER,LASTPER                                                  
         XC    APERFST,APERFST                                                  
*                                                                               
PER100   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R3,15,4(R1)                                                      
         BZ    PER140                                                           
         USING SORTD,R3                                                         
*                                                                               
         XC    PERTABD(PERTABL),PERTABD CLEAR TABLE ENTRY                       
         MVC   PERHIRE,SORTHIRE                                                 
         MVC   PERLOHIR,SORTHIRE                                                
         MVC   PERTERM,SORTTERM                                                 
*                                                                               
         LA    R0,PERLMAX        CLEAR IN OUT DATES                             
         LA    R1,PERINOUT                                                      
PER110   MVC   0(3,R1),=XL3'FFFFFF' ASSURE FAILURE IF NOT SET                   
         XC    3(3,R1),3(R1)                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,PER110                                                        
*                                                                               
         LA    R0,PERNBUD                                                       
         LA    R1,PERSTHRS         CLEAR BUDGET BUCKETS                         
         ZAP   0(L'PERSTHRS,R1),=P'0'                                           
         LA    R1,L'PERSTHRS(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         CLC   SORTPER,LASTPER     TEST FOR NEW PERSON                          
         BE    PER120              NO                                           
*                                                                               
         ST    R5,APERFST          SAVE A(FIRST ENTRY FOR PERSON)               
         MVC   LASTPER,SORTPER     UPDATE LAST PERSON CODE                      
         MVC   LASTACC,SORTACC     AND THE 1R ACCOUNT CODE                      
         MVC   LASTNAME,SORTNAME   AND THE NAME                                 
         B     PER130              BUILD RECORD                                 
*                                                                               
PER120   CLI   ISBURSON,C'Y'     BURSON REPORTS BASED ON HAVING STDHRS          
         BE    *+14                                                             
         OC    SORTTRM,SORTTRM     TEST FOR TERMINATION DATE                    
         BNZ   PER130              YES-REPORT IT UNDER FIRST ACCOUNT            
         MVC   PERACC,SORTACC                                                   
         MVC   PERREP,SORTACC      REPORT IT UNDER ITS OWN ACCOUNT              
         MVC   PERRNAME,SORTNAME                                                
         LA    R2,1(R2)                                                         
         LA    R5,PERTABL(R5)                                                   
         B     PER100                                                           
*                                                                               
PER130   MVC   PERACC,SORTACC      1R ACCOUNT CODE                              
         MVC   PERREP,LASTACC      REPORTING ACCOUNT                            
         MVC   PERRNAME,LASTNAME   AND NAME                                     
         LA    R2,1(R2)            INCREMENT ENTRY COUNT                        
         LA    R5,PERTABL(R5)                                                   
         L     RE,APERFST          GET FIRST ENTRY FOR PERSON                   
         CLC   SORTHIRE,PERLOHIR-PERTABD(RE) TEST FOR LOWEST HIRE DATE          
         BH    *+10                NO                                           
         MVC   PERLOHIR-PERTABD(L'PERLOHIR,RE),SORTHIRE                         
         B     PER100                                                           
*                                                                               
PER140   ST    R2,NPER                                                          
         L     R0,=A(PERMAX)                                                    
         CR    R2,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         LTR   R2,R2               TEST FOR ANY ENTRIES                         
         BZ    PEREXIT                                                          
         GOTO1 VQSORT,DMCB,APERTAB,(R2),PERTABL,PERKEYL,0                       
*                                                                               
PEREXIT  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SORT CONSTANTS                                                     *          
**********************************************************************          
         SPACE 1                                                                
SORTFLD  DC    CL80'SORT FIELDS=(5,15,A),FORMAT=BI,WORK=1'                      
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=100'                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* GETEL # 4                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R6,DISP2,ELCODE,4                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R2,R4,R6,RB                                                      
         EJECT                                                                  
**********************************************************************          
* NEWCAL - SET WEEKTAB - N'WORKING DAYS PER WEEK IN PERIOD(MONTH)    *          
*              DATETAB - BUCKET DATES                                *          
*              NWORKING - NUMBER OF WORKING DAYS IN A PERIOD (MONTH) *          
*              PRWEEKS - EBCDIC VERSION OF WEEKTAB                   *          
*             IF YOU FIND A CALENDER RECORD                          *          
*          P1 IS RC                                                  *          
*          P2 IS A(OFFICE) OR 0 FOR REQUEST LEVEL                    *          
**********************************************************************          
         SPACE 1                                                                
NEWCAL   DS    0H                                                               
         NMOD1 NCWORKL,**NCAL**                                                 
         LR    R8,RC                                                            
         L     RC,0(R1)                                                         
         L     R4,4(R1)            A(OFFICE) OR 0                               
*                                                                               
         TM    FLGCST,FLGNCST      ARE WE RUNNING ON NEW COST?                  
         BZ    NEWC30              NO - RUN OLD CALENDAR                        
*                                                                               
         USING NCWORKD,R8                                                       
         GOTO1 VGETPER,DMCB,MONEND,ACALKEYS,NCPERDAT,(R4),ACALREC               
*                                                                               
         USING GPOPD,R2                                                         
         LA    R2,NCPERDAT                                                      
         CLI   GPONPER,0           ANY PERIOD DATA RETURNED                     
         BE    NEWC30              NOPE, SET UP USING TABLES                    
*                                                                               
         MVC   NWORKING,GPONWORK                                                
         SR    R1,R1                                                            
         IC    R1,GPONPER          NUMBER OF PERIODS DEFINED                    
         CLI   GPONPER,8           GT 8                                         
         BNH   *+6                                                              
         DC    H'0'                DIE UNTILL I EXPAND DATETAB, ETC             
*                                                                               
         STC   R1,DTENUM           SAVE NUMBER OF PERIODS                       
         XC    DATETAB(L'DATETAB*NDATES),DATETAB                                
         XC    WEEKTAB(NWKS),WEEKTAB                                            
*                                                                               
         LA    R2,GPOPERS                                                       
         USING GPOPERD,R2                                                       
         LA    R3,WEEKTAB                                                       
         LA    R4,DATETAB                                                       
NEWC10   MVC   0(L'WEEKTAB,R3),GPOPNWRK                                         
         MVC   0(L'DATETAB,R4),GPOPEND                                          
         LA    R2,GPOPLN1Q(R2)                                                  
         LA    R3,L'WEEKTAB(R3)                                                 
         LA    R4,L'DATETAB(R4)                                                 
         BCT   R1,NEWC10                                                        
*                                                                               
* NOTE FOLLOWING WON'T WORK IF WEEKTAB ENTRY IS GT 9                            
*                                                                               
         LA    R1,NPRWKS                                                        
         LA    R2,WEEKTAB                                                       
         LA    R3,PRWEEKS          CONVERT BINARY TO EBCDIC                     
NEWC20   MVI   0(R3),X'F0'                                                      
         MVN   0(1,R3),0(R2)                                                    
         LA    R2,L'WEEKTAB(R2)                                                 
         LA    R3,L'PRWEEKS(R3)                                                 
         BCT   R1,NEWC20                                                        
         B     NEWCX                                                            
*                                                                               
NEWC30   DS    0H                                                               
         GOTO1 ACAL,DMCB,(RC)      GENERATE THE REPORT CALENDAR                 
NEWCX    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R2,R8,RB                                                         
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  DS    0H                                                               
         NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMHIGHDR DS    0H                                                               
         NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMREADDR DS    0H                                                               
         NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMGETREC DS    0H                                                               
         NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R3,RB                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD CALENDAR VALUES FOR REPORT                               
* ON EXIT, WEEKTAB CONTAINS NUMBER OF WORKING DAYS IN EACH                      
*          WEEK OF MONTH                                                        
*          DATETAB CONTAINS LAST DAY IN THE FIRST FOUR WEEK                     
*          OF MONTH                                                             
*          NDAYS=N'DAYS IN MONTH  NWORKING=N'WORKING DAYS IN MONTH              
*          PRWEEKS=PRINTABLE N'DAYS IN EACH WEEK                                
**********************************************************************          
         SPACE 1                                                                
CAL      DS    0H                                                               
         NMOD1 0,**CAL**                                                        
         L     RC,0(R1)                                                         
         MVC   WORK(2),MONEND      PACKED YYMM                                  
         MVI   WORK+2,X'01'        DD FOT GETDAY                                
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+10)                                 
         GOTO1 GETDAY,DMCB,WORK+10,WORK   FIRST DAY OF WEEK INTO WORK           
         MVC   STDAY,WORK          SAVE DAY EXPRESSION                          
         MVC   STDAYNUM,0(R1)      SAVE DAY NUMBER                              
*                                                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB(8),MENDMM       PACKED MONTH                                 
         CVB   R1,DUB              NOW BINARY                                   
         BCTR  R1,0                MAKE IT AN OFFSET                            
         LA    R2,MONTAB           TABLE OF DAYS IN MONTH                       
         AR    R2,R1                                                            
         MVC   NDAYS,0(R2)         SAVE N'DAYS IN MONTH                         
         CLI   MENDMM,2            IS THIS FEB                                  
         BNE   CAL10               NO, I'M OK                                   
*                                                                               
         ZAP   DUB,=P'0'           CHECK FOR LEAP YEAR                          
         MVO   DUB(8),MENDYY       PACK THE YEAR                                
         DP    DUB,=PL2'4'         LEAP YEARS ARE EVENLY DIVISABLE BY 4         
         CP    DUB+6(2),=P'0'      ANY REMAINDER?                               
         BNE   CAL10               YES-NOT A LEAP YEAR                          
         MVI   NDAYS,29            LEAP YEAR                                    
*                                                                               
CAL10    LA    R2,DAYTABL          MONACCS TABLE OF DAYS                        
         LA    R3,NUMDAYTB         TABLE OF DAYS IN WEEK 1                      
         LA    R4,LASTTAB          TABLE OF DAYS IN WEEK 5                      
         LA    R1,7                                                             
*                                                                               
CAL20    CLC   STDAY,0(R2)         WORK IS FIRST DAY OF MONTH                   
         BE    CAL30                                                            
         LA    R2,3(R2)            NEXT DAY                                     
         LA    R3,1(R3)            FIRST WEEK TABLE                             
         LA    R4,4(R4)            LAST WEEK TABLE                              
         BCT   R1,CAL20                                                         
         DC    H'0'                                                             
CAL30    MVC   WEEK1DYS,0(R3)      SAVE # OF DAYS IN FIRST WEEK                 
*                                                                               
         LA    R1,28               GET # OF DAYS IN LAST WEEK                   
         LA    R0,4                                                             
CAL40    CLM   R1,1,NDAYS                                                       
         BE    CAL50                                                            
         LA    R4,1(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CAL40                                                         
*                                                                               
CAL50    MVC   WEEK5DYS,0(R4)          STORE DAYS IN WEEK 5                     
         MVC   WEEK2DYS(3),=X'050505'  MIDDLE WEEKS ARE 5 DAYS                  
*                                                                               
         LA    R0,NWKS             RECALC DAYS IN MONTH                         
         SR    R1,R1               FROM PHYSICAL DAYS TO WORKING DAYS           
         SR    R3,R3                                                            
         LA    R2,WEEKTAB                                                       
CAL60    IC    R1,0(R2)                                                         
         AR    R3,R1                                                            
         LA    R2,1(R2)                                                         
         BCT   R0,CAL60                                                         
         STC   R3,NWORKING                                                      
*                                                                               
         LA    R1,NWKS                                                          
         LA    R2,WEEKTAB                                                       
         LA    R3,PRWEEKS          CONVERT BINARY TO EBCDIC                     
*                                                                               
CAL70    MVI   0(R3),X'F0'                                                      
         MVN   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,CAL70                                                         
*                                                                               
* CREATE BUCKET DATES                                                           
* NOTE: THE NUMBER OF DAYS IN WEEK CALCULATED UP TO THE FIRST                   
* FRIDAY OF THE MONTH, THE REVENUE FOR WEEK ONE IS ALL REVENUE                  
* THRU THE FIRST SUNDAY AFTER THE FIRST FRIDAY, CAPISE                          
*                                                                               
         LA    R2,DATETAB          PRIME DATETAB WITH THIS MONTH                
         LA    R1,NDATES           NUMBER OF DATES                              
CAL80    MVC   0(2,R2),MONEND                                                   
         LA    R2,3(R2)                                                         
         BCT   R1,CAL80                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,WEEK1DYS                                                      
*                                                                               
         CLC   =C'SUN',STDAY       MONTH START ON SUNDAY                        
         BNE   *+8                                                              
         LA    R1,1(R1)            ADD 1 DAY                                    
*                                                                               
         CLC   =C'SAT',STDAY       MONTH START ON SATURDAY                      
         BNE   *+8                                                              
         LA    R1,2(R1)            ADD 2 DAYS                                   
*                                                                               
         LA    R2,DATETAB                                                       
         LA    R0,NDATES                                                        
CAL90    CVD   R1,DUB                                                           
         MVO   HALF(2),DUB+6(2)    MAKE X'0XXC' INTO X'XXC0'                    
         MVC   2(1,R2),HALF        SAVE THE XX AS YYMMXX                        
         LA    R1,7(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   R0,CAL90                                                         
*                                                                               
* ADJUST THE MONTH'S TABLE FOR FEDERAL HOLIDAYS                                 
*                                                                               
         LA    R3,HOLTAB                                                        
         USING RUNXTRAD,R2                                                      
         L     R2,VEXTRAS                                                       
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         CLI   MCAGCTRY,CTRYCAN    IS THIS A CANADIAN AGENCY                    
         BNE   *+8                 NO                                           
         LA    R3,HOLTABCA         USE THE LIST OF CANADIAN HOLIDAYS            
*                                                                               
CAL100   CLI   0(R3),EOF           TEST FOR EOT                                 
         BE    CALX                                                             
         CLC   MENDYY,0(R3)        MATCH ON YEAR                                
         BE    CAL110                                                           
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CAL100                                                           
*                                                                               
CAL110   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         SH    R0,=H'2'            ADJUST LENGTH FOR YEAR/LENGTH                
         SRL   R0,1                DIVIDE BY TWO                                
         LA    R3,2(R3)            POINT TO FIRST HOLIDAY                       
*                                                                               
CAL120   CLC   MENDMM,0(R3)        MATCH ON MONTH OF HOLIDAY                    
         BE    CAL140              YES                                          
CAL130   LA    R3,2(R3)                                                         
         BCT   R0,CAL120                                                        
         B     CALX                                                             
*                                                                               
CAL140   SR    RE,RE                                                            
         IC    RE,NWORKING                                                      
         BCTR  RE,0                DECREMENT N'WORKING DAYS IN MO.              
         STC   RE,NWORKING                                                      
*                                                                               
         LA    R1,PRWEEKS                                                       
         LA    R2,NDATES                                                        
         LA    RE,DATETAB          DECREMENT DAYS IN HOLIDAYS'S                 
         LA    RF,WEEKTAB          WEEK                                         
CAL150   CLC   0(2,R3),1(RE)       TEST AGAINST MONTH AND DAY                   
         BNH   CAL160                                                           
         LA    RE,L'DATETAB(RE)                                                 
         LA    RF,L'WEEKTAB(RF)                                                 
         LA    R1,L'PRWEEKS(R1)                                                 
         BCT   R2,CAL150                                                        
*                                                                               
CAL160   SR    R4,R4                                                            
         IC    R4,0(RF)                                                         
         BCTR  R4,0                DECREMENT N'DAYS IN WEEK                     
         STC   R4,0(RF)                                                         
         MVN   0(1,R1),0(RF)                                                    
         B     CAL130                                                           
*                                                                               
CALX     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* TABLES FOR DATE CALCULATION (DAYS IN WEEK1, MONTHS)                *          
**********************************************************************          
         SPACE 1                                                                
NUMDAYTB DC    AL1(5,4,3,2,1,5,5)                                               
*                                                                               
* 28 29 30 31 DAYS IN MON                                                       
* DAY OF FIRST OF MONTH                                                         
*                                                                               
MONTAB   DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
*                                                                               
         DS    0H                                                               
LASTTAB  DC    AL1(0,1,2,3)       MON                                           
         DC    AL1(1,2,3,4)       TUE       DAYS IN LAST WEEK OF                
         DC    AL1(2,3,4,5)       WED       MONTH                               
         DC    AL1(3,4,5,5)       THU                                           
         DC    AL1(4,5,5,5)       FRI                                           
         DC    AL1(0,0,0,1)       SAT                                           
         DC    AL1(0,0,1,2)       SUN                                           
*                                                                               
* TABLE OF WORKING DAYS                                                         
* FIRST DAY OF MONTH BY RUN DATE                                                
*                                                                               
WORKTAB  DC    AL1(1,1,1,1,1,0,0) MON TUE WED ...                 1             
         DC    AL1(2,2,2,2,1,0,1)                                 2             
         DC    AL1(3,3,3,2,1,1,2)                                 3             
         DC    AL1(4,4,3,2,2,2,3)                                 4             
         DC    AL1(5,4,3,3,3,3,4)                                 5             
         DC    AL1(5,4,4,4,4,4,5)                                 6             
         DC    AL1(5,5,5,5,5,5,5)                                 7             
         DC    AL1(6,6,6,6,6,5,5)                                 8             
         DC    AL1(7,7,7,7,6,5,6)                                 9             
         DC    AL1(8,8,8,7,6,6,7)                                10             
         DC    AL1(9,9,8,7,7,7,8)                                11             
         DC    AL1(10,9,8,8,8,8,9)                               12             
         DC    AL1(10,9,9,9,9,9,10)                              13             
         DC    AL1(10,10,10,10,10,10,10)                         14             
         DC    AL1(11,11,11,11,11,10,10)                         15             
         DC    AL1(12,12,12,12,11,10,11)                         16             
         DC    AL1(13,13,13,12,11,11,12)                         17             
         DC    AL1(14,14,13,12,12,12,13)                         18             
         DC    AL1(15,14,13,13,13,13,14)                         19             
         DC    AL1(15,14,14,14,14,14,15)                         20             
         DC    AL1(15,15,15,15,15,15,15)                         21             
         DC    AL1(16,16,16,16,16,15,15)                         22             
         DC    AL1(17,17,17,17,16,15,16)                         23             
         DC    AL1(18,18,18,17,16,16,17)                         24             
         DC    AL1(19,19,18,17,17,17,18)                         25             
         DC    AL1(20,19,18,18,18,18,19)                         26             
         DC    AL1(20,19,19,19,19,19,20)                         27             
         DC    AL1(20,20,20,20,20,20,20)                         28             
         DC    AL1(21,21,21,21,21,20,20)                         29             
         DC    AL1(22,22,22,22,21,20,21)                         30             
         DC    AL1(23,23,23,22,21,21,22)                         31             
*                                                                               
HOLTAB   DS    0C                                                               
         DC    X'90',AL1(18),X'01010219053007040903112211231225'                
         DC    X'91',AL1(18),X'01010218052707040902112811291225'                
         DC    X'92',AL1(18),X'01010217052507030907112611271225'                
         DC    X'93',AL1(18),X'01010215053107050906112511261224'                
         DC    X'94',AL1(18),X'01030221053007040905112411251226'                
         DC    X'95',AL1(18),X'01020220052907040904112311241225'                
         DC    X'96',AL1(18),X'01010219052707040902112811291225'                
         DC    AL1(EOF)                                                         
*                                                                               
HOLTABCA DS    0C                  CANADIAN HOLIDAYS                            
         DC    X'91',AL1(22),X'0101032905200624070108050912101412251226X        
               '                                                                
         DC    X'92',AL1(22),X'0101041705180701080309071012122512251228X        
               '                                                                
         DC    X'93',AL1(20),X'010104090524070108020906101112271228'            
         DC    X'94',AL1(20),X'010304080523070108010905101012261227'            
         DC    X'95',AL1(20),X'010204140522070308010904100912251226'            
         DC    X'96',AL1(20),X'010104050520070108050902101412251226'            
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GO TO GETPROF WITH A COMPOSITE KEY SO IT RETURNS YOU A              *         
*    COMPOSITE PROFILE.                                               *         
*    P1 IS RC                                                         *         
*    P2 IS A  OF ACCOUNT                                              *         
*    0(R3) IS A(ACCOUNT YOU WANT TO GET A PROFILE FOR)                *         
***********************************************************************         
         SPACE 1                                                                
BUILDPRO DS    0H                                                               
         NMOD1 0,*BPROF                                                         
         L     RC,0(R1)                                                         
         USING PROFKD,R2                                                        
         LA    R2,WORK                                                          
         XC    PROFKEY,PROFKEY                                                  
         XC    PROFILES,PROFILES                                                
         MVI   PROFKSYS,C'A'                                                    
         MVC   PROFKPGM,=C'0R6'                                                 
         MVC   PROFKAGY,ALPHAID                                                 
         MVC   PROFKUNL,=C'1R'                                                  
         LTR   R3,R3               WAS AN ACCOUNT PASSED                        
         BZ    *+10                NO                                           
         MVC   PROFKACC,0(R3)                                                   
         GOTO1 GETPROF,DMCB,PROFKEY,PROFILES,DATAMGR                            
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R2,RB                                                            
         EJECT                                                                  
**********************************************************************          
* HEADHOOK - CALLED FROM ACPRINT                                     *          
**********************************************************************          
         SPACE 1                                                                
HDHOOK   DS    0H                                                               
         NMOD1 0,*HEADHK                                                        
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
*                                                                               
         CLI   TIMETYPE,BTIME                                                   
         BNE   *+10                                                             
         MVC   HEAD4+60(11),=C'B TIME ONLY'                                     
*                                                                               
         CLI   TIMETYPE,RTIME                                                   
         BNE   *+10                                                             
         MVC   HEAD4+60(11),=C'R TIME ONLY'                                     
*                                                                               
         CLI   TIMETYPE,NTIME                                                   
         BNE   *+10                                                             
         MVC   HEAD4+60(11),=C'N TIME ONLY'                                     
*                                                                               
         USING BOXD,R5                                                          
         L     R5,ADBOX                                                         
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXROWS+6,C'T'                                                   
         LA    RE,BOXROWS+9        RE=A(MIDLINE POSITION)                       
         CLI   RCSUBPRG,1          TEST FOR EMPLOYEE SUMMARY                    
         BNE   *+8                                                              
         LA    RE,BOXROWS+8                                                     
         MVI   0(RE),C'M'                                                       
         MVI   BOXROWS+58,C'B'                                                  
         CLI   RCSUBPRG,0                                                       
         BNE   HDH100                                                           
*                                                                               
         LA    RE,BOXCOLS                                                       
         MVI   DETLBOX-DETD(RE),C'L'                                            
         MVI   DETBOX1-DETD(RE),C'C'                                            
         MVI   DETBOX2-DETD(RE),C'C'                                            
         MVI   DETBOX3-DETD(RE),C'C'                                            
         MVI   DETBOX4-DETD(RE),C'C'                                            
         MVI   DETBOX5-DETD(RE),C'C'                                            
         MVI   DETBOX6-DETD(RE),C'C'                                            
         MVI   DETRBOX-DETD(RE),C'R'                                            
*                                                                               
         MVC   HEAD5+11(L'PERCODE),PERCODE                                      
         MVC   HEAD5+25(L'PERNAME),PERNAME                                      
         OC    PERDHIR,PERDHIR     TEST FOR HIRE DATE                           
         BZ    HDH65               NONE                                         
*                                                                               
         MVC   HEAD5+100(9),=C'HIRE DATE'                                       
         GOTO1 DATCON,DMCB,(1,PERDHIR),(17,HEAD5+110)                           
         OC    PERDTRM,PERDTRM     TEST FOR TERMINATION DATE                    
         BZ    HDH65               NO                                           
*                                                                               
         MVC   HEAD6+100(9),=C'TERM DATE'                                       
         GOTO1 DATCON,DMCB,(1,PERDTRM),(17,HEAD6+110)                           
*                                                                               
HDH65    EDIT  (P8,THISTARG),(6,HEAD6+11),2,ALIGN=LEFT,ZERO=NOBLANK             
         MVC   HEAD9+33(1),PRWEEK1    VARIBLE HEADER DATA                       
         CLI   PRWEEK1,C'1'                                                     
         BE    *+8                                                              
         MVI   HEAD9+38,C'S'       PLURAL DAYS                                  
         MVC   HEAD9+81(1),PRWEEK5                                              
         CLI   PRWEEK5,C'1'                                                     
         BE    *+8                                                              
         MVI   HEAD9+86,C'S'                                                    
*                                                                               
         CLI   DATESARE,MOSDATES                                                
         BNE   HDH80                                                            
         MVC   HEAD3+58(8),=C'MONTH OF'                                         
         MVC   HEAD3+67(6),MOSPREND                                             
*                                                                               
HDH80    CLI   DATESARE,CALDATES                                                
         BNE   HDHX                                                             
         MVC   HEAD4+48(14),=C'TIMESHEETS FOR'                                  
         MVC   HEAD4+63(8),CALPRMST                                             
         MVC   HEAD4+72(4),=C'THRU'                                             
         MVC   HEAD4+77(8),CALPRMND                                             
         B     HDHX                                                             
*                                                                               
HDH100   CLI   RCSUBPRG,1          EMPSUM HEADER                                
         BNE   HDH200                                                           
*                                                                               
         LA    RE,BOXCOLS                                                       
         MVI   EMPLBOX-EMPSUMD(RE),C'L'                                         
         MVI   EMPBOX1-EMPSUMD(RE),C'C'                                         
*                                                                               
         MVC   HEAD5+11(L'PERCODE),PERCODE                                      
         MVC   HEAD5+25(L'PERNAME),PERNAME                                      
         OC    PERDHIR,PERDHIR     TEST FOR HIRE DATE                           
         BZ    HDH105              NONE                                         
*                                                                               
         MVC   HEAD5+100(9),=C'HIRE DATE'                                       
         GOTO1 DATCON,DMCB,(1,PERDHIR),(17,HEAD5+110)                           
         OC    PERDTRM,PERDTRM                                                  
         BZ    HDH105                                                           
*                                                                               
         MVC   HEAD6+100(9),=C'TERM DATE'                                       
         GOTO1 DATCON,DMCB,(1,PERDTRM),(17,HEAD6+110)                           
*                                                                               
HDH105   MVI   HEAD6+1,0           SPACE KEEPER                                 
         CLI   PRMTHTPC,C'Y'       USING MONTHLY TARGET PERCENTAGE              
         BE    HDH106              YES, WILL PRINT AS REPORT ROW                
         MVC   HEAD6+1(8),=C'TARGET %'                                          
         EDIT  (P8,TARGET),(6,HEAD6+11),2,ALIGN=LEFT,ZERO=NOBLANK               
*                                                                               
HDH106   LA    R2,HEAD8+(EMPMON1-EMPSUMD)                                       
         LA    R3,MONTHTAB         DISPLAY THE REPORT MONTHS                    
         ZIC   R6,NMONTHS          R6=LOOP COUNTER                              
         LA    RE,BOXCOLS+(EMPBOX2-EMPSUMD)                                     
*                                                                               
HDH110   ZAP   DUB,=P'0'                                                        
         MVO   DUB,1(1,R3)         GET PWO MONTH                                
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   1(3,R2),0(R1)       SLOT MONTH INTO PRINT POSITION               
         MVI   0(RE),C'C'                                                       
*                                                                               
HDH120   LA    R2,EMPMON2-EMPMON1(R2)  NEXT O/P POSITION                        
         LA    RE,EMPMON2-EMPMON1(RE)  NEXT BOX POSITION                        
         LA    R3,L'MONTHTAB(R3)                                                
         BCT   R6,HDH110                                                        
*                                                                               
         MVC   2(5,R2),=C'TOTAL'                                                
         MVI   1(RE),C'R'          RIGHT HAND SIDE OF BOX                       
         BAS   RE,HDHDATES                                                      
         B     HDHX                                                             
*                                                                               
* RCSUBPRG=2, MONTHLY DEPT SUMMARY                                              
* RCSUBPRG=4, MONTHLY EMPLOYEE METRIC                                           
*                                                                               
HDH200   CLI   RCSUBPRG,2                                                       
         BE    HDH210                                                           
         CLI   RCSUBPRG,4                                                       
         BE    HDH220                                                           
         B     HDH300                                                           
*                                                                               
HDH210   LA    RE,BOXCOLS                                                       
         MVI   DSLBOX-DSD(RE),C'L'                                              
         MVI   DSBOX1-DSD(RE),C'C'                                              
         MVI   DSBOX2-DSD(RE),C'C'                                              
         MVI   DSBOX3-DSD(RE),C'C'                                              
         LA    RE,DETPOFF+COLWIDTH-1(RE)                                        
         B     HDH230                                                           
*                                                                               
HDH220   LA    RE,BOXCOLS                                                       
         MVI   PMLBOX-PMETRICD(RE),C'L'                                         
         MVI   PMCOL1-PMETRICD(RE),C'C'                                         
         MVI   PMMCOL1-PMETRICD(RE),C'C'                                        
         LA    RE,PMMNOFF+COLWIDTH-1(RE)                                        
*                                                                               
HDH230   MVC   HEAD5+1(15),LEVADSC                                              
         MVC   HEAD5+15(4),LEVACDE                                              
         MVC   HEAD5+20(L'LEVANME),LEVANME                                      
         MVC   HEAD6+1(15),LEVBDSC                                              
         MVC   HEAD6+15(4),LEVBCDE                                              
         MVC   HEAD6+20(L'LEVBNME),LEVBNME                                      
*                                                                               
         TM    TOTSTAT,TOTOFF+TOTDPT  IF THIS IS AN OFF/DPT TOTAL               
         BZ    HDH240                         DON'T PRINT LEVB NAME             
         MVC   HEAD5+15(4),LSTACDE            AND PRINT PREVIOUS                
         MVC   HEAD5+20(L'LSTANME),LSTANME    LEVEL A NAME                      
         MVC   HEAD6+1(19+L'LEVBNME),SPACES   CLEAR LEVEL B HEADS               
*                                                                               
HDH240   LA    R3,MONTHTAB         DISPLAY THE REPORT MONTHS                    
         SR    R6,R6                                                            
         IC    R6,NMONTHS          R6=LOOP COUNTER                              
         LA    R2,HEAD8+DETPOFF                                                 
         CLI   RCSUBPRG,2                                                       
         BE    HDH250                                                           
         LA    R2,HEAD8+PMMNOFF                                                 
*                                                                               
HDH250   ZAP   DUB,=P'0'                                                        
         MVO   DUB,1(1,R3)         GET PWO MONTH                                
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   1(3,R2),0(R1)       SLOT MONTH INTO PRINT POSITION               
         MVI   0(RE),C'C'          NOTE BOX POSITION                            
*                                                                               
         LA    R2,COLWIDTH(R2)     NEXT O/P POSITION                            
         LA    RE,COLWIDTH(RE)     NEXT BOX COLUMN POSITION                     
         LA    R3,L'MONTHTAB(R3)                                                
         BCT   R6,HDH250                                                        
*                                                                               
         MVC   1(5,R2),=C'TOTAL'                                                
         MVI   1(RE),C'R'          RIGHT SIDE OF BOX AFTER TOTAL                
         BAS   RE,HDHDATES                                                      
         B     HDHX                                                             
*                                                                               
HDH300   CLI   RCSUBPRG,3          TEST FOR WEEKLY DEPT SUMMARY                 
         BE    HDH310                                                           
         CLI   RCSUBPRG,5          TEST FOR WEEKLY EMPLOYEE METRIC              
         BE    HDH320                                                           
         B     HDHX                                                             
*                                                                               
HDH310   LA    RE,BOXCOLS                                                       
         MVI   DSLBOX-DSD(RE),C'L'                                              
         MVI   DSBOX1-DSD(RE),C'C'                                              
         MVI   DSBOX2-DSD(RE),C'C'                                              
         MVI   DSBOX3-DSD(RE),C'C'                                              
         LA    RE,DETPOFF+COLWIDTH-1(RE)                                        
         B     HDH330                                                           
*                                                                               
HDH320   LA    RE,BOXCOLS                                                       
         MVI   PMLBOX-PMETRICD(RE),C'L'                                         
         MVI   PMCOL1-PMETRICD(RE),C'C'                                         
         MVI   PMCOL2-PMETRICD(RE),C'C'                                         
         MVI   PMCOL3-PMETRICD(RE),C'C'                                         
         LA    RE,PMWKOFF+COLWIDTH-1(RE)                                        
*                                                                               
HDH330   LA    R0,5                                                             
         MVI   0(RE),C'C'                                                       
         LA    RE,COLWIDTH(RE)                                                  
         BCT   R0,*-8                                                           
         MVI   0(RE),C'R'                                                       
*                                                                               
         MVC   HEAD5+1(15),LEVADSC                                              
         MVC   HEAD5+15(4),LEVACDE                                              
         MVC   HEAD5+20(L'LEVANME),LEVANME                                      
         MVC   HEAD6+1(15),LEVBDSC                                              
         MVC   HEAD6+15(4),LEVBCDE                                              
         MVC   HEAD6+20(L'LEVBNME),LEVBNME                                      
*                                                                               
         TM    TOTSTAT,TOTOFF+TOTDPT  IF THIS IS AN OFF/DPT TOTAL               
         BZ    HDH350                         DON'T PRINT LEVB NAME             
         MVC   HEAD5+15(4),LSTACDE            AND PRINT PREVIOUS                
         MVC   HEAD5+20(L'LSTANME),LSTANME    LEVEL A NAME                      
         MVC   HEAD6+1(19+L'LEVBNME),SPACES   CLEAR LEVEL B HEADS               
*                                                                               
HDH350   LA    RE,HEAD9+40                                                      
         CLI   RCSUBPRG,5                                                       
         BNE   *+8                                                              
         LA    RE,HEAD9+42                                                      
*                                                                               
         MVC   0(1,RE),PRWEEK1       VARIBLE HEADER DATA                        
         MVC   7(1,RE),PRWEEK2                                                  
         MVC   14(1,RE),PRWEEK3                                                 
         MVC   21(1,RE),PRWEEK4                                                 
         MVC   28(1,RE),PRWEEK5                                                 
*                                                                               
         CLI   PRWEEK1,C'1'                                                     
         BE    *+8                                                              
         MVI   5(RE),C'S'       PLURAL DAYS                                     
         CLI   PRWEEK5,C'1'                                                     
         BE    *+8                                                              
         MVI   33(RE),C'S'                                                      
*                                                                               
         BAS   RE,HDHDATES                                                      
*                                                                               
HDHX     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* HEADLINE DATES                                                     *          
**********************************************************************          
         SPACE 1                                                                
HDHDATES NTR1                                                                   
         CLC   MOSPRSTR,SPACES                                                  
         BNH   HDHD30                                                           
         MVC   HEAD3+54(4),=C'FROM'                                             
         MVC   HEAD3+59(6),MOSPRSTR                                             
         MVC   HEAD3+66(4),=C'THRU'                                             
         MVC   HEAD3+71(6),MOSPREND                                             
*                                                                               
HDHD30   CLI   DATESARE,CALDATES                                                
         BNE   HDHDX                                                            
         MVC   HEAD4+48(14),=C'TIMESHEETS FOR'                                  
         MVC   HEAD4+63(8),CALPRST                                              
         MVC   HEAD4+72(4),=C'THRU'                                             
         MVC   HEAD4+77(8),CALPREND                                             
HDHDX    XIT1                                                                   
*                                                                               
BOXRC    DC    A(0)                                                             
COLWIDTH EQU   7                   6 BYTES DATA, 1 BYTE BOX                     
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
**********************************************************************          
* 1N ACCOUNT TABLE                                                   *          
**********************************************************************          
         SPACE 1                                                                
NOTTAB   DC    (NOTMAX*NOTTABL)X'00'                                            
         EJECT                                                                  
**********************************************************************          
* BUFFALO CSECT                                                      *          
**********************************************************************          
         SPACE 1                                                                
         BUFF  LINES=3000,ROWS=1,COLUMNS=20,FLAVOR=PACKED,KEYLIST=(26, X        
               A)                                                               
         EJECT                                                                  
*                                                                               
*              DSECT FOR STORAGE AREA                                           
*                                                                               
ACR6D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
VQSORT   DS    V                                                                
VGETPER  DS    V                                                                
VGETSTD  DS    V                                                                
*                                                                               
ANOTTAB  DS    A                                                                
ABUFF    DS    A                                                                
APER     DS    A                                                                
ACAL     DS    A                                                                
ABLDPRO  DS    A                                                                
ASETCAL  DS    A                                                                
ANEWCAL  DS    A                                                                
AMONPER  DS    A                                                                
ADEPT    DS    A                   PRINT DEPARTMENT SUMMARY                     
AWEEKLY  DS    A                   DEVELOP WKLY STD HRS                         
ASETBKS  DS    A                   SET THE PERBUCKS                             
ASETHRS  DS    A                   SET THE AGENCY HOURS                         
ACALCTOT DS    A                   SET THE YEARLY TOTAL                         
APRTEMP  DS    A                   PRINT EMPLOYEE SUMMARY                       
ATYPLNQ  EQU   *-ATYPES                                                         
*                                                                               
ADBOX    DS    A                                                                
SAVERE   DS    A                                                                
SORTREC  DS    CL(SORTRECL)        SORT RECORD AREA                             
LASTPER  DS    CL(L'SORTPER)                                                    
LASTACC  DS    CL(L'SORTACC)                                                    
LASTNAME DS    CL(L'SORTNAME)                                                   
*                                                                               
APERTAB  DS    A                   A(1R TABLE)                                  
NPER     DS    F                   N'ENTRIES                                    
MAXPER   DS    F                                                                
ACLITAB  DS    A                   A(CLIENT TABLE)                              
NCLI     DS    F                   N'CLIENTS                                    
MAXCLI   DS    F                                                                
APERSON  DS    A                   A(1R TABLE ENTRY)                            
APERFST  DS    A                   A(FIRST PERTAB ENTRY FOR PERSON)             
ACALKEYS DS    A                   A(NEW CALENDER KEY TABLE)                    
ASTDKEYS DS    A                   A(STD HRS KEY TABLE)                         
ACALREC  DS    A                   A(CALENDER RECORD) (FOR GETSTD)              
ASTDHRS  DS    A                   A(STDHRS O/P BUFFER)                         
ABUCKS   DS    A                   A(1R TOTALS BUCKTESI)                        
APERBLK  DS    A                   A(PERCAL BUFFER                              
ASTANDRD DS    A                   A(THE BUCKED TYPE THEY CALL STDHRS)          
ASTANDYR DS    A                   A(THE YEARLY TOTAL BUCK FOR ABOVE)           
*                                                                               
NNOTS    DS    F                                                                
NOTMAX   EQU   400                 MAXIMUM ENTRIES IN 1N TABLE                  
VACACC   DS    CL(L'SUMACC)        VACATION ACCOUNT                             
*                                  Y=DEDUCT VACATION FROM STD HOURS             
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LVCDLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVBNME  DS    CL36                LEVEL B NAME                                 
LEVCNME  DS    CL36                LEVEL C NAME                                 
LEVSDSC  EQU   *-LEVSCDE           TOTAL LEVEL CODES AND NAMES                  
*                                                                               
LSTLEVS  DS    0CL12               LAST (PREVIOUS) LEVEL CODES                  
LSTACDE  DS    CL12                LAST LEVEL A CODE                            
LSTBCDE  DS    CL12                LAST LEVEL B CODE                            
LSTCCDE  DS    CL12                LAST LEVEL C CODE                            
LSTDCDE  DS    CL12                LAST LEVEL D CODE                            
*                                                                               
LSTANME  DS    CL36                LAST LEVEL A NAME                            
LSTBNME  DS    CL36                LAST LEVEL B NAME                            
LSTCNME  DS    CL36                LAST LEVEL C NAME                            
*                                                                               
PERSLEN  DS    XL1                 LENGTH OF PERSON LEVEL FIELD                 
OFFSET   DS    XL1                 OFFSET INTO KEY TO READ PERSON CODE          
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
*                                                                               
COMPLEN  DS    XL1                                                              
CLILEN   DS    XL1                                                              
EMPLEVA  EQU   1                                                                
EMPLEVB  EQU   2                                                                
EMPLEVC  EQU   3                                                                
EMPLEVD  EQU   4                                                                
REPOPT   DS    CL1                                                              
ISBURSON DS    CL1                 Y, USE BURSON STYLE                          
*                                                                               
FLGCST   DS    XL1                 COST FLAG                                    
FLGNCST  EQU   X'80'               RUNNING ON NEW COST                          
*                                                                               
FLGSET   DS    XL1                 FLAG FOR TOTAL DETOUT RUN                    
FLGTOT   EQU   X'80'               MONTH TOTAL HAS BEEN EXECUTED                
FLGPERD  EQU   X'40'               PERIODS IN MONTH ARE LESS THAN 5             
*                                                                               
TIMETYPE DS    CL1                 TYPE OF TIME ON REPORT(OPT 5)                
BTIME    EQU   X'80'                                                            
RTIME    EQU   X'40'                                                            
NTIME    EQU   X'20'                                                            
ALLTIME  EQU   BTIME+RTIME+NTIME                                                
*                                                                               
DATESARE DS    CL1                 TYPE OF DATE FILTERING TO USE                
CALDATES EQU   X'80'                                                            
MOSDATES EQU   X'40'                                                            
*                                                                               
REPMASK  DS    XL1                                                              
REPDET   EQU   X'80'               DETAIL REPORT                                
REPEMP   EQU   X'40'               EMPLOYEE SUMMARY                             
REPDPT   EQU   X'20'               DEPARTMENT SUMMARY                           
REPWK    EQU   X'10'               WEEKLY DEPARTMENT SUMMARY                    
REPWKMT  EQU   X'08'               WEEKLY EMPLOYEE METRC                        
REPMTMT  EQU   X'04'               MONTHLY EMPLOYEE METRIC                      
*                                                                               
REPMODE  DS    CL1                 FOR DEPT SUMMARIES, WEEKLY OR MTHLY          
REPMMNTH EQU   X'80'                                                            
REPMWEEK EQU   X'40'                                                            
*                                                                               
METRFILT DS    CL1                 QOPT9, METRIC FILTER                         
MFPROD   EQU   X'01'                                                            
MFUTIL   EQU   X'02'                                                            
MFBILL   EQU   X'04'                                                            
*                                                                               
EOF      EQU   X'FF'               EOF MARKER                                   
DATEOPT  DS    CL1                 M=MOA T=TRANSACTION DATE                     
VACOPT   DS    CL1                 VACATION OPTION                              
ANYDET   DS    CL1                 Y=CONTRA-DETAIL PRINTED                      
TOTSTAT  DS    XL1                 TYPE OF TOTAL CURRENTLY PRINTING             
TOTOFF   EQU   X'80'                                                            
TOTDPT   EQU   X'40'               TOTAL IS DEPARTMENT                          
*                                                                               
DMPCNT   DS    PL8                                                              
MYMODE   DS    XL1                                                              
BUDSTAT  DS    CL1                                                              
GOTACC   EQU   1                                                                
GOTLEVC  EQU   2                                                                
GOTLEVB  EQU   4                                                                
GOTSVGP  EQU   8                                                                
GOTLEVA  EQU   16                                                               
*                                                                               
*        BUDGET AREA               THREE LEVELS, COVERED BY BUDD                
BUDAREA  DS    0C                                                               
BUSTDHRS DS    XL3                                                              
         DS    12PL8                                                            
BUTRGPCT DS    XL3                                                              
         DS    12PL8                                                            
*                                                                               
PROFILES DS    0CL16               PROFILES SAVED AT THE LEDGER LEVEL           
PRFORMAT DS    CL1                 REPORT FORMAT                                
PRDATES  DS    CL1                 MOA OR TRANSACTION DATE OPTION               
PRVAC    DS    CL1                 VACATION OPTION                              
PRSTDHR  DS    CL1                 STANDARD HOURS BUD NUM                       
PRTRGPCT DS    CL1                 TARGET PERCENTAGE BUD NUM                    
PRMTHTPC DS    CL1                 TARGET PERCENTAGE IS MONTHLY                 
PRAGYHRS DS    CL1                 YES, USE AGENCY HOURS IN CALCS               
PRAVAIL  DS    CL1                 YES, PRINT AVAILABLE HOURS FOR STDHR         
         DS    CL(L'PROFILES-(*-PROFILES))  SPARE                               
*                                                                               
PBDHRS   DS    PL6                 PUTBYDAT INTERFACE                           
PBDDATE  DS    CL3                 X'YMD'                                       
PBDMONTH DS    XL2                 X'YM'                                        
*                                                                               
MOSPRSTR DS    CL6                 START DATE FOR HEADER                        
MOSPREND DS    CL6                 ASOF DATE TO PRINT IN REPORT HEADER          
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
MONSTART DS    PL2                 START MONTH FOR REPORT                       
MONEND   DS    0PL2                END MONTH FOR REPORT                         
MENDYY   DS    PL1                 MONTH END YY                                 
MENDMM   DS    PL1                 MONTH END MM                                 
*                                  AND MONTH FOR WEEKLY VERSIONS                
NMONTHS  DS    XL1                 N'MONTHS IN REPORT                           
MONTHTAB DS    12PL2               TABLE OF REQUESTED MONTHS                    
CALSTR   DS    XL3                 START DATE FOR FIRST CALENDER MONTH          
CALEND   DS    XL3                 END DATE FOR LAST CAL MONTH                  
CALMSTR  DS    XL3                 START/END DATES OF THIS CAL MONTH            
CALMEND  DS    XL3                                                              
CALPRST  DS    CL8                 CALENDER START                               
CALPREND DS    CL8                 CALENDER END                                 
CALPRMST DS    CL8                 CALENDER START FOR LAST MONTH                
CALPRMND DS    CL8                 CALENDER END FOR LAST MONTH                  
*                                                                               
* PERSON BUCKET AREA                                                            
*                                                                               
PERDATA  DS    0C                                                               
PERCODE  DS    CL12                1R ACCOUNT CODE                              
PERNAME  DS    CL36                1R ACCOUNT NAME                              
PERDHIR  DS    PL3                 HIRE DATE                                    
PERDTRM  DS    PL3                 TERMINATION DATE                             
PERDINOT DS    0XL18               IN OUTS                                      
PERDST1  DS    XL3                 DATE FIRST IN DEPT                           
PERDEN1  DS    XL3                 DATE FIRST LEFT DEPT                         
PERDST2  DS    XL3                 DATE NEXT IN DEPT                            
PERDEN2  DS    XL3                 DATE NEXT LEFT DEPT                          
PERDST3  DS    XL3                 DATE NEXT IN DEPT                            
PERDEN3  DS    XL3                 DATE NEXT LEFT DEPT                          
PERBUCK  DS    0PL8                MONTHLY OR WEEKLY BUCKS                      
PERBHR   DS    13PL8                                                            
PERRHR   DS    13PL8                                                            
PERNHR   DS    13PL8                                                            
PERCHG   DS    13PL8                                                            
PERTOT   DS    13PL8                                                            
PERSHR   DS    13PL8                                                            
PERAHR   DS    13PL8                                                            
PERGHR   DS    13PL8               GENERAL BUCKETS                              
PERNBUCK EQU   (*-PERBHR)/L'PERBUCK                                             
PERDATAL EQU   *-PERDATA                                                        
*                                                                               
PERSTAT  DS    XL1                                                              
PERNC    EQU   X'80'               PERSON IS ON NEW COST RECORDS                
PERTS    EQU   X'40'               PERSON HAS TIME SHEET DATA FOR REQ           
*                                                                               
THISSTD  DS    PL8                                                              
THISTARG DS    PL8                                                              
TARGET   DS    PL8                 TARGET PERCENTAGE                            
PERPCT   DS    13PL8               BUCKETS FOR PERCENTAGE OUTPUT                
TARGLNQ  EQU   (*-THISSTD)/L'THISSTD                                            
*                                                                               
MONBUCK  DS    12PL8                                                            
MONLNQ   EQU   *-MONBUCK           MONTHLY BUCKET LENGTH                        
YEARBUCK DS    PL8                                                              
MONTOT   DS    12PL8                                                            
YEARTOT  DS    PL8                                                              
MONCHG   DS    12PL8                                                            
YEARCHG  DS    PL8                                                              
MONPCT   DS    12PL8                                                            
YEARPCT  DS    PL8                                                              
*                                                                               
* STORAGE AREAS FOR DEPARTMENT SUMMARY REPORT                                   
*                                                                               
EMPBIL   DS    12PL8                                                            
         DS    PL8                                                              
*                                                                               
EMPRTE   DS    12PL8                                                            
         DS    PL8                                                              
*                                                                               
EMPNOT   DS    12PL8                                                            
         DS    PL8                                                              
*                                                                               
EMPSTD   DS    12PL8                                                            
         DS    PL8                                                              
*                                                                               
EMPAHR   DS    12PL8                                                            
         DS    PL8                                                              
*                                                                               
EMPTARG  DS    12PL8                                                            
         DS    PL8                 NEED AN EXTRA FIELD FOR EMPEDIT              
*                                                                               
EMPNBUCK EQU   (*-EMPBIL)/L'EMPBIL                                              
NUMER    DS    13PL8                                                            
DENOM    DS    13PL8                                                            
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
*                                                                               
* INTERIM AND FINAL VALUES GENERATED BY CAL SUB-ROUTINE                         
*                                                                               
NDAYS    DS    X                                                                
NWORKING DS    X                                                                
STDAY    DS    CL3                 START DAY OF LAST MONTH                      
STDAYNUM DS    XL1                 DAY NUMBER                                   
*                                                                               
NCOFFICE DS    CL2                 OFFICE FOR NEWCAL INTERFACE                  
*                                                                               
* CALENDER DATA FOR A THE END MONTH OF THE REQUEST                              
*                                                                               
WEEKTAB  DS    0XL1                NUMBER OF WORKING DAYS IN WEEK               
WEEK1DYS DS    X                                                                
WEEK2DYS DS    X                                                                
WEEK3DYS DS    X                                                                
WEEK4DYS DS    X                                                                
WEEK5DYS DS    X                                                                
WEEK6DYS DS    X                                                                
WEEK7DYS DS    X                                                                
WEEK8DYS DS    X                                                                
NWKS     EQU   (*-WEEKTAB)/L'WEEKTAB          NUMBER OF WEEKS                   
NMTHS    EQU   12                             NUMBER OF MONTHS                  
*                                                                               
PRWEEKS  DS    0CL1                PRINTABLE NUMBER OF DAYS IN WEEK             
PRWEEK1  DS    CL1                                                              
PRWEEK2  DS    CL1                                                              
PRWEEK3  DS    CL1                                                              
PRWEEK4  DS    CL1                                                              
PRWEEK5  DS    CL1                                                              
PRWEEK6  DS    CL1                                                              
PRWEEK7  DS    CL1                                                              
PRWEEK8  DS    CL1                                                              
NPRWKS   EQU   (*-PRWEEKS)/L'PRWEEKS          NUMBER OF WEEKS                   
*                                                                               
DTENUM   DS    XL1                 # OF PERIODS RETURNED FROM GETPER            
DATETAB  DS    0CL3                TABLE OF PACKED DATES FOR                    
DATE1    DS    CL3                 PUTTING REVENUE IN BUCKETS                   
DATE2    DS    CL3                                                              
DATE3    DS    CL3                                                              
DATE4    DS    CL3                                                              
DATE5    DS    CL3                                                              
DATE6    DS    CL3                                                              
DATE7    DS    CL3                                                              
DATE8    DS    CL3                                                              
NDATES   EQU   (*-DATETAB)/L'DATETAB                                            
*                                                                               
PL16     DS    PL16                FOR MULTIPLYING RATE                         
MSG      DS    CL10                MESSAGE FOR DUMP                             
*                                                                               
DISP2    DS    H                   MY DISPLACEMENT TO ELEMENT                   
LASTBUF  DS    CL(SUMKEYL)         PREVIOUS BUFFALO KEY                         
SUMREC   DS    CL(SUMRECL)                                                      
SVKEY    DS    CL42                                                             
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
PROFKD   DSECT                         TO COVER GETPROFS KEY                    
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
*-------------------------------------------------------------------            
* HIGH LEVEL TOTALS                                                             
*  STORAGE IS ABUCKS                                                            
*-------------------------------------------------------------------            
*                                                                               
BUCKSD   DSECT                                                                  
SUBBIL   DS    12PL8                                                            
SUBBILYR DS    PL8                                                              
SUBRTE   DS    12PL8                                                            
SUBRTEYR DS    PL8                                                              
SUBNOT   DS    12PL8                                                            
SUBNOTYR DS    PL8                                                              
SUBSTD   DS    12PL8                                                            
SUBSTDYR DS    PL8                                                              
SUBAHR   DS    12PL8                                                            
SUBAHRYR DS    PL8                                                              
SUBNBUCK EQU   (*-SUBBIL)/L'SUBBIL                                              
BUCKLN   EQU   8                                                                
*                                                                               
DEPBIL   DS    12PL8                                                            
DEPBILYR DS    PL8                                                              
DEPRTE   DS    12PL8                                                            
DEPRTEYR DS    PL8                                                              
DEPNOT   DS    12PL8                                                            
DEPNOTYR DS    PL8                                                              
DEPSTD   DS    12PL8                                                            
DEPSTDYR DS    PL8                                                              
DEPAHR   DS    12PL8                                                            
DEPAHRYR DS    PL8                                                              
DEPNBUCK EQU   (*-DEPBIL)/L'DEPBIL                                              
*                                                                               
OFFBIL   DS    12PL8                                                            
OFFBILYR DS    PL8                                                              
OFFRTE   DS    12PL8                                                            
OFFRTEYR DS    PL8                                                              
OFFNOT   DS    12PL8                                                            
OFFNOTYR DS    PL8                                                              
OFFSTD   DS    12PL8                                                            
OFFSTDYR DS    PL8                                                              
OFFAHR   DS    12PL8                                                            
OFFAHRYR DS    PL8                                                              
OFFNBUCK EQU   (*-OFFBIL)/L'OFFBIL                                              
*                                                                               
REQBIL   DS    12PL8                                                            
REQBILYR DS    PL8                                                              
REQRTE   DS    12PL8                                                            
REQRTEYR DS    PL8                                                              
REQNOT   DS    12PL8                                                            
REQNOTYR DS    PL8                                                              
REQSTD   DS    12PL8                                                            
REQSTDYR DS    PL8                                                              
REQAHR   DS    12PL8                                                            
REQAHRYR DS    PL8                                                              
REQNBUCK EQU   (*-REQBIL)/L'REQBIL                                              
NBUCKS   EQU   (*-BUCKSD)/L'REQBIL                                              
BUCKSLN  EQU   *-BUCKSD                                                         
*                                                                               
**************************                                                      
* DSECT FOR BUDGET AREA                                                         
**************************                                                      
BUDD     DSECT                                                                  
BUDNUM   DS    CL1                                                              
BUDMODE  DS    CL1                                                              
BUDSWCH  DS    XL1                 BUDGET SWITCH(BUD REC/STD HRS REC)           
BUDSET   EQU   X'80'               IF SET - AMOUNTS SET FROM BUD RECS           
BUDAMNT  DS    PL8                                                              
         DS    11PL8                                                            
BUDDLN   EQU   *-BUDD                                                           
         EJECT                                                                  
* DSECT TO COVER SORT RECORD                                                    
*                                                                               
SORTD    DSECT                                                                  
SORTRLEN DS    H                                                                
         DS    H                                                                
SORTPER  DS    CL12                STAFF CODE                                   
SORTTRM  DS    PL3                 TERMINATION DATE (COMPLEMENT)                
SORTACC  DS    CL12                1R ACCOUNT CODE                              
SORTNAME DS    CL36                                                             
SORTHIRE DS    PL3                                                              
SORTTERM DS    PL3                                                              
SORTRECL EQU   *-SORTD                                                          
*                                                                               
* DSECT TO COVER PERSON TABLE                                                   
*                                                                               
PERTABD  DSECT                                                                  
PERACC   DS    CL12                1R ACCOUNT CODE                              
PERKEYL  EQU   *-PERTABD                                                        
PERTSTAT DS    XL1                 STATUS BYTE                                  
PERTSTD  EQU   X'80'               NEW STANDARD HRS USED                        
PERSTHRS DS    12PL3               STANDARD HOURS                               
PERNSTD  EQU   (*-PERSTHRS)/L'PERSTHRS                                          
PERWKSTD DS    6PL3                STD HRS FOR LAST MONTH BY PERIOD             
PERTARG  DS    PL3                 TARGET PERCENTAGE                            
         ORG   PERTARG                                                          
PERMTARG DS    12PL3               MONTHLY TARGET PCT BUCKETS                   
PERNBUD  EQU   (*-PERSTHRS)/L'PERSTHRS                                          
PERREP   DS    CL12                1R REPORTING ACCOUNT CODE                    
PERRNAME DS    CL36                                                             
PERHIRE  DS    PL3                 HIRE DATE                                    
PERTERM  DS    PL3                 TERMINATION DATE                             
PERLOHIR DS    PL3                 LOWEST HIRE DATE                             
PERINOUT DS    XL(PERLMAX*6)       SETS OF INOUTS (2 XL3 DATES)                 
PERTABL  EQU   *-PERTABD                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER CLIENT TABLE                                        *          
**********************************************************************          
         SPACE 1                                                                
CLITABD  DSECT                                                                  
CLICODE  DS    CL6                 CLIENT CODE                                  
CLIKEYL  EQU   *-CLITABD                                                        
CLINAME  DS    CL24                CLIENT NAME                                  
CLITABL  EQU   *-CLITABD                                                        
         SPACE 2                                                                
* DSECT TO COVER THE 1N TABLE                                                   
*                                                                               
NOTTABD  DSECT                                                                  
NOTACC   DS    CL12                ACCOUNT CODE                                 
NOTKEYL  EQU   *-NOTTABD                                                        
NOTNAME  DS    CL36                ACCOUNT NAME                                 
NOTTABL  EQU   *-NOTTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER BUFFALO SUMMARY RECORD                                         
*                                                                               
SUMRECD  DSECT                                                                  
SUMPER   DS    CL12                1R ACCOUNT CODE                              
SUMTYPE  DS    XL1                 TIME TYPE                                    
SUMTOT   EQU   1                   TOTAL TIME                                   
SUMCHG   EQU   2                   CHARGEABLE TIME (B+R)                        
SUMBIL   EQU   3                                                                
SUMRTE   EQU   4                   R TIME                                       
SUMNOT   EQU   5                                                                
SUMACC   DS    CL13                CLIENT CODE OR 1N ACCOUNT CODE               
SUMKEYL  EQU   *-SUMRECD                                                        
*                                                                               
SUMBUCK  DS    0PL8                                                             
*                                                                               
SUMWEEK  DS    0PL8                WEEKLY BUCKETS                               
SUMWEEK1 DS    PL8                                                              
SUMWEEK2 DS    PL8                                                              
SUMWEEK3 DS    PL8                                                              
SUMWEEK4 DS    PL8                                                              
SUMWEEK5 DS    PL8                                                              
SUMWEEK6 DS    PL8                                                              
SUMWEEK7 DS    PL8                                                              
SUMWEEK8 DS    PL8                                                              
SUMWKLNQ EQU   *-SUMWEEK                                                        
SUMWKNUM EQU   (*-SUMWEEK)/L'SUMWEEK                                            
*                                                                               
SUMMONTH DS    0PL8                MONTHLY BUCKETS                              
         DS    12PL8                                                            
*                                                                               
SUMNBUCK EQU   (*-SUMBUCK)/L'SUMBUCK                                            
SUMRECL  EQU   *-SUMRECD                                                        
         SPACE 2                                                                
* DSECT TO COVER DETAIL REPORT PRINT LINE                                       
*                                                                               
DETD     DSECT                                                                  
DETLBOX  DS    XL1                 LEFT HAND BOX LINE                           
         DS    CL4                 SPARE                                        
DETMSG   DS    CL20                MESSAGE AREA                                 
         DS    CL5                                                              
DETBOX1  DS    XL1                                                              
DETWEEK1 DS    CL10                                                             
         DS    CL1                 BETWEEN COLUMNS                              
DETBOX2  DS    XL1                                                              
DETWEEK2 DS    CL10                                                             
         DS    CL1                 BETWEEN COLUMNS                              
DETBOX3  DS    XL1                                                              
DETWEEK3 DS    CL10                                                             
         DS    CL1                 BETWEEN COLUMNS                              
DETBOX4  DS    CL1                                                              
DETWEEK4 DS    CL10                                                             
         DS    CL2                 BETWEEN COLUMNS                              
DETBOX5  DS    CL1                                                              
DETWEEK5 DS    CL10                                                             
         DS    CL1                 BETWEEN COLUMNS                              
DETBOX6  DS    CL1                                                              
DETMON   DS    CL10                                                             
DETRBOX  DS    CL1                                                              
         SPACE 2                                                                
* DSECT TO COVER EMPLOYEE SUMMARY PRINT LINE                                    
*                                                                               
EMPSUMD  DSECT                                                                  
EMPLBOX  DS    CL1                                                              
EMPTIT   DS    CL20                SUB-TITLE AREA                               
         ORG   EMPSUMD                                                          
         DS    CL2                                                              
EMPACC   DS    CL28                ACCOUNT OR CLIENT                            
         DS    CL9                                                              
EMPBOX1  DS    CL1                                                              
         ORG   EMPACC                                                           
         DS    CL3                                                              
EMPSUB   DS    CL20                                                             
         ORG                                                                    
EMPMON1  DS    CL6                                                              
EMPBOX2  DS    CL1                                                              
EMPMON2  DS    CL6                                                              
         DS    CL1                                                              
EMPMON3  DS    CL6                                                              
         DS    CL1                                                              
EMPMON4  DS    CL6                                                              
         DS    CL1                                                              
EMPMON5  DS    CL6                                                              
         DS    CL1                                                              
EMPMON6  DS    CL6                                                              
         DS    CL1                                                              
EMPMON7  DS    CL6                                                              
         DS    CL1                                                              
EMPMON8  DS    CL6                                                              
         DS    CL1                                                              
EMPMON9  DS    CL6                                                              
         DS    CL1                                                              
EMPMONA  DS    CL6                                                              
         DS    CL1                                                              
EMPMONB  DS    CL6                                                              
         DS    CL1                                                              
EMPMONC  DS    CL6                                                              
         DS    CL1                                                              
EMPTOT   DS    CL6                                                              
         DS    CL1                                                              
         SPACE 2                                                                
* DSECT TO COVER DEPARTMENT SUMMARY PRINT LINE                                  
*                                                                               
DSD      DSECT                                                                  
DSLBOX   DS    CL1                 LEFT HAND BOX                                
         DS    CL1                 SPARE                                        
DSSTAFF  DS    CL7                 STAFF CODE                                   
         DS    CL1                 SPARE                                        
DSNAME   DS    CL18                NAME                                         
DSBOX1   DS    CL1                 START FORSTN                                 
DSTARG   DS    CL5                                                              
DSBOX2   DS    CL1                 BOX POSITION                                 
DSTYPE   DS    CL1                 TIME TYPE FOR EMPLOYEE                       
         DS    CL3                 SPARE                                        
DSBOX3   DS    CL1                 BOX POSITION                                 
DSCOLS   DS    0C                  START COLUMN DATA                            
DETPOFF  EQU   DSCOLS-DSD                                                       
         EJECT                                                                  
* DSECT TO COVER EMPLOYEE METRIC PRINT LINE                                     
*                                                                               
PMETRICD DSECT                                                                  
PMREC    DS    0CL132                                                           
PMLBOX   DS    CL1                 LEFT HAND BOX                                
         DS    CL1                 SPARE                                        
PMSTAFF  DS    CL7                 STAFF CODE                                   
         DS    CL1                 SPARE                                        
PMNAME   DS    CL18                NAME                                         
PMCOL1   DS    CL1                 BOX POSITION                                 
PMTARG   DS    CL5                 TARG PCT                                     
PMCOL2   DS    CL1                 BOX POSITION                                 
PMWKMET  DS    CL6                 METRIC FOR WEEKLY VERSION                    
PMCOL3   DS    CL1                                                              
PMWKCOLS DS    0C                  START OF WEELKY COLS                         
PMWKOFF  EQU   *-PMREC                                                          
         ORG   PMTARG                                                           
PMMNMET  DS    CL6                 METRIC FOR MONTHLY REPORT                    
PMMCOL1  DS    CL1                                                              
PMMNCOLS DS    0C                  START OF MONTHLY                             
PMMNOFF  EQU   *-PMREC                                                          
         EJECT                                                                  
*                                                                               
* DATAMGR INTERFACE WORK AREA                                                   
*                                                                               
DMWORKD  DSECT                                                                  
DMKEY1   DS    CL(ACCKLEN)                                                      
DMKEYSV  DS    CL(ACCKLEN)                                                      
DMCOMM   DS    CL8                 DATAMGR COMMAND                              
DMIO     DS    CL2042                                                           
DMWKLNQ  EQU   *-DMWORKD                                                        
       ++INCLUDE ACGETSTDD                                                      
       ++INCLUDE ACPERCALLD                                                     
BUFFSIZE EQU   (GSBUFSZQ+GPBUFSZQ+2042+GSOPLENQ+BUCKSLN+LPERBLK+CLISIZEX        
               +PERSIZE)                                                        
LPERBLK  EQU   1000                                                             
PERMAX   EQU   11000                                                            
PERSIZE  EQU   PERMAX*PERTABL                                                   
CLIMAX   EQU   7000                                                             
CLISIZE  EQU   CLIMAX*CLITABL                                                   
*        DDLOGOD                                                                
*        DDMASTD                                                                
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACREPPROFD                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDREPXTRAD                                                             
*        DDBOXEQUS                                                              
*        DDREMOTED                                                              
*        ACQD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPPROFD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACREPR602S03/14/00'                                      
         END                                                                    
