*          DATA SET ACREPP102  AT LEVEL 100 AS OF 05/14/09                      
*PHASE ACP102A                                                                  
*INCLUDE UNDERLIN                                                               
*INCLUDE ACLIST                                                                 
         TITLE 'WORK-CODE REPORT'                                               
*-------------------------------------------------------------------            
*              HISTORY                                                          
*-------------------------------------------------------------------            
*        11/03/92  ADDED ROUTINE SETREV, WHICH PERFORMS THE FOLLOWING           
*                  FUNCTION:                                                    
*                  IF OPTION 1 = B, DISABLE ALL REVERSAL FILTERING.             
*        11/06/92  ADDED FLTMOS, SIBCE MONACC WON'T FILTER BY MOS               
*                  UNLESS THE DATES ARE IN QMOSSTR-QMOSEND                      
*        02/18/98  YEAR 2000 CHANGES                                            
*                                                                               
*-------------------------------------------------------------------            
         EJECT ,                                                                
ACWKREP  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP1**,R9,RR=R5                                              
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
         LA    R6,P                                                             
*                                                                               
         USING ACP1D,RC                                                         
         USING PRINTD,R6           R6=PRINT LINE                                
*                                                                               
         LA    R7,BUFFREC                                                       
         LA    R8,SORTREC                                                       
*                                                                               
         USING BUFFD,R7            R7=BUFFALO RECORD                            
         USING SORTD,R8            R8=SORTER RECORD                             
*                                                                               
         ST    R5,RELO                                                          
         EJECT ,                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   ACP3                                                             
*&&US                                                                           
         L     RF,=A(SAVERC)                                                    
         A     RF,RELO                                                          
         ST    RC,0(RF)            SAVE REG C                                   
         L     RF,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,RF                                                      
*                                                                               
         L     RF,ADMASTD                                                       
*                                                                               
         USING MASTD,RF                                                         
*                                                                               
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     RF,=A(HOOK)                                                      
         A     RF,RELO                                                          
         ST    RF,HEADHOOK                                                      
         L     RF,=A(QTRIT)                                                     
         A     RF,RELO                                                          
         ST    RF,QUART                                                         
*&&                                                                             
         LA    RF,VTYPES           RELOCATE EXTERNS                             
         LA    RE,EXTERNS                                                       
ACP1     CLI   0(RF),X'FF'                                                      
         BE    ACP1B               END OF TABLE                                 
         L     R1,0(RF)                                                         
         A     R1,RELO                                                          
         ST    R1,0(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         B     ACP1                                                             
ACP1B    LA    RF,AMOUNTS          CLEAR ACCUMS                                 
         LA    RE,6                                                             
ACP2     MVC   0(24,RF),=3PL8'0'                                                
         LA    RF,24(RF)                                                        
         BCT   RE,ACP2                                                          
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT ,                                                                
ACP3     CLI   MODE,REQFRST                                                     
         BNE   ACP10                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   SKIPSW,C'N'                                                      
         MVC   QSTART+4(2),=C'01'                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(9,PERIOD)                                
         GOTO1 (RF),(R1),,(1,START)                                             
         MVI   PERIOD+6,C'-'                                                    
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(X'30',QEND),(X'20',QEND),(1,0)                      
         GOTO1 DATCON,DMCB,(0,QEND),(9,PERIOD+7)                                
         GOTO1 (RF),(R1),,(1,END)                                               
*                                                                               
         BAS   RE,SETREV           SET REVERSAL FILTERING                       
*                                                                               
         CLI   QOPT2,C' '                                                       
         BE    ACP3B                                                            
         MVC   COUNTERS(12),=4PL3'0'                                            
         ZAP   TOTQPCT,=P'0'                                                    
         ZAP   TOTYPCT,=P'0'                                                    
         ZAP   QTOT,=P'0'                                                       
         ZAP   YTOT,=P'0'                                                       
         GOTO1 QUART,DMCB,(RC)     GENERATE START OF QUARTER                    
         SPACE 1                                                                
ACP3B    L     RF,ADCOMP           SET UP MEDIA TABLE                           
         AH    RF,DATADISP                                                      
         LA    RE,MEDTAB                                                        
         MVI   MEDTAB,X'FF'                                                     
         LA    R0,MEDTABN                                                       
ACP4     CLI   0(RF),0                                                          
         BE    ACP9                                                             
         CLI   0(RF),X'11'                                                      
         BE    ACP8                                                             
ACP6     ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     ACP4                                                             
*                                                                               
         USING ACMEDIAD,RF                                                      
*                                                                               
ACP8     MVC   0(1,RE),ACMDCODE                                                 
         MVC   1(15,RE),ACMDDESC                                                
         MVC   16(24,RE),=3PL8'0'                                               
         LA    RE,L'MEDTAB(RE)                                                  
         MVI   0(RE),X'FF'                                                      
         BCT   R0,ACP6                                                          
         SPACE 1                                                                
ACP9     L     RF,=A(OPTTAB)       OPTION TABLE                                 
ACP9B    CLC   QOPT2,0(RF)                                                      
         BE    ACP9D                                                            
         LA    RF,L'OPTTAB(RF)                                                  
         CLI   0(RF),X'FF'                                                      
         BNE   ACP9B                                                            
         DC    H'0'                INVALID OPTION                               
         SPACE 1                                                                
ACP9D    MVC   SORTSW,1(RF)                                                     
         MVC   STATUS,2(RF)                                                     
         MVC   RCSUBPRG,3(RF)                                                   
         MVC   MYHEAD8,4(RF)       FOR BEGINNING OF HEAD8                       
         MVC   MYRPTNM,13(RF)      REPORT NAME                                  
         CLI   SORTSW,C'Y'                                                      
         BNE   ACP9F                                                            
         L     RF,24(RF)          ADDRESS OF SORT INFORMATION                   
         A     RF,RELO                                                          
         MVC   KEYLEN,0(RF)       KEY LENGTH                                    
         MVC   KEYDISP,1(RF)      DISPLACEMENT OF SORT KEY                      
         MVC   NAMELEN,2(RF)      NAME LENGTH                                   
         MVC   NAMEDIS,3(RF)      DISPLACEMENT OF SORT NAME                     
*                                                                               
         MVC   MINLEN,4(RF)       MINOR LENGTH                                  
         MVC   MINDISP,5(RF)      DISPLACEMENT OF MINOR KEY                     
         MVC   MNAMLEN,6(RF)      NAME LENGTH                                   
         MVC   MNAMDIS,7(RF)      DISPLACEMENT OF MINOR NAME                    
*                                                                               
ACP9F    TM    STATUS,BUFFIT                                                    
         BZ    EXIT                                                             
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         MVC   BUFFREC,SPACES                                                   
         B     EXIT                                                             
         EJECT ,                                                                
ACP10    CLI   MODE,LEVAFRST                                                    
         BNE   ACP15                                                            
         MVI   CLIACTV,C'N'                                                     
         L     R2,ADHEIRA                                                       
         MVC   LEVAACNT,3(R2)      SAVE CLIENT CODE                             
         MVC   LEVANAME,SPACES     AND NAME                                     
         L     R2,ADLVANAM                                                      
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEVANAME(0),ACNMNAME                                             
         TM    STATUS,CLISORT      CLIENT LEVEL SORT                            
         BZ    EXIT                                                             
         BAS   RE,SORTINIT                                                      
         B     EXIT                                                             
         SPACE 3                                                                
ACP15    CLI   MODE,LEVBFRST                                                    
         BNE   ACP20                                                            
         ZAP   JCOUNT,=P'0'                                                     
         L     R1,ADHEIRB                                                       
         L     R2,ADLDGHIR                                                      
*                                                                               
         USING ACHEIRD,R2                                                       
*                                                                               
         ZIC   RE,ACHRLEVA                                                      
         LA    R1,3(RE,R1)                                                      
         MVC   LEVBACNT,0(R1)      SAVE PRODUCT CODE                            
         MVC   LEVBNAME,SPACES     AND NAME                                     
         L     R2,ADLVBNAM                                                      
*                                                                               
         USING ACNAMED,R2                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEVBNAME(0),ACNMNAME                                             
         TM    STATUS,PRODSORT     PRODUCT LEVEL SORT                           
         BZ    EXIT                                                             
         BAS   RE,SORTINIT                                                      
         B     EXIT                                                             
         EJECT ,                                                                
ACP20    CLI   MODE,PROCACC                                                     
         BNE   ACP30                                                            
         ZAP   WCOUNT,=P'0'                                                     
         MVI   JOBSW,0             SET JOB PRINT PENDING                        
         L     R1,ADACC                                                         
         L     R2,ADLDGHIR                                                      
*                                                                               
         USING ACHEIRD,R2                                                       
*                                                                               
         ZIC   RE,ACHRLEVA                                                      
         LA    R1,3(RE,R1)                                                      
         MVC   ACCOUNT,0(R1)       SAVE PRODUCT AND JOB CODE                    
         TM    STATUS,JOBSORT      JOB LEVEL SORT                               
         BZ    EXIT                                                             
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BNM   ACP22                                                            
         CLI   QOPT4,C'J'          SUPPRESS JOB LEVEL DETAIL                    
         BE    EXIT                                                             
         CLI   QOPT4,C'P'          SUPPRESS JOB, PRODUCT LEVEL DETAIL           
         BE    EXIT                                                             
*                                                                               
         USING BIND,R2                                                          
*                                                                               
ACP22    L     R2,AJOBTAB                                                       
         XC    BININ,BININ                                                      
         MVI   BINACTV,C'N'                                                     
         MVI   SORTACTV,C'N'                                                    
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         SPACE 3                                                                
ACP30    CLI   MODE,ANALFRST                                                    
         BNE   ACP40                                                            
         ZAP   SCOUNT,=P'0'                                                     
         MVI   ANALSW,0            SET W-CODE-PRINT PENDING                     
         B     EXIT                                                             
         SPACE 3                                                                
ACP40    CLI   MODE,SBACFRST                                                    
         BNE   ACP500                                                           
         ZAP   TCOUNT,=P'0'                                                     
         MVI   SUPPSW,0            SET SUPPLIER -PRINT PENDING                  
         B     EXIT                                                             
         EJECT ,                                                                
ACP500   CLI   MODE,PROCTRNS                                                    
         BNE   ACP70                                                            
         L     R2,ADTRANS                                                       
*                                                                               
         USING TRANSD,R2                                                        
*                                                                               
         CLI   0(R2),X'44'                                                      
         BNE   EXIT                                                             
*                                                                               
         USING RUNXTRAD,RF                                                      
*                                                                               
         L     RF,VEXTRAS          IS THERE A LIST RECORD                       
         OC    VLISTREC,VLISTREC                                                
         BNZ   *+16                                                             
         CLI   QOPT2,C' '          IF OPT2 NOT A BLANK THEN                     
         BNE   ACP510              WANT EVERYTHING                              
         B     EXIT                ELSE GET NOTHING                             
         SPACE 1                                                                
         GOTO1 VACLIST,DMCB,VLISTREC,TRNSANAL                                   
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'E'                                                        
         BE    EXIT                EXCLUDE THIS WORK-CODE                       
ACP510   LR    R3,R2                                                            
         SH    R3,DATADISP                                                      
*                                                                               
         USING ACKEYD,R3                                                        
*                                                                               
*&&UK                                                                           
         CLC   ACKEYWRK,=C'**'                                                  
         BE    EXIT                FORGET ORDERS                                
*&&                                                                             
*                                                                               
* IF OPT1 ^= B, USE MONTH-OF-SERVICE DATE AS SELECTION CRITERIA                 
*                                                                               
         CLI   QOPT1,C'B'                                                       
         BE    ACP520                                                           
         BAS   RE,FLTMOS                                                        
         BNE   EXIT                                                             
*                                                                               
         SPACE 1                                                                
ACP520   CLI   QOPT1,C' '          ALL ITEMS                                    
         BNE   ACP540                                                           
*                                                                               
ACP530   BAS   RE,SUMIT            GET BILLED                                   
         B     ACP570                                                           
         EJECT ,                                                                
* IF OPT1 IS 'U', ELIMINATE BILLING AND ITEMS FULLY BILLED.IF UNBILLED,         
* USE MONTH-OF-SERVICE DATE AS SELECTION CRITERIA                               
         SPACE 1                                                                
ACP540   CLI   QOPT1,C'U'          UNBILLED ITEMS ONLY                          
         BNE   ACP550                                                           
         CLC   TRNSANAL,=C'99'     ELIMINATE BILLING,                           
         BE    EXIT                                                             
         OC    ACDTUSED,ACDTUSED    IF FULLY BILLED ITEMS OR                    
         BNZ   EXIT                                                             
         BAS   RE,SUMIT                                                         
         CLI   BILLED,C'Y'                                                      
         BE    EXIT                                                             
         B     ACP570                                                           
         SPACE 3                                                                
* IF OPT1 IS 'B', ELIMINATE UNBILLED ITEMS. IF FULLY BILLED, USE                
* ACDTUSED AS SELECTION CRITERIA. IF PARTIALLY BILLED, SUMIT WILL               
* PLUG IN BILLDATE FROM X'4B' ELEMENT.                                          
         SPACE 1                                                                
ACP550   CLI   QOPT1,C'B'          TREAT OPTION OTHER THAN 'B' OR 'U'           
         BNE   ACP530               AS BLANK                                    
         BAS   RE,SUMIT                                                         
         CLI   BILLED,C'N'         ELIMINATE UNBILLED ITEMS                     
         BE    EXIT                                                             
         MVC   DATE(L'BILLDATE),BILLDATE                                        
         OC    ACDTUSED,ACDTUSED                                                
         BZ    ACP570                                                           
         CLI   BILLDATE,C' '                                                    
         BNE   ACP560                                                           
         GOTO1 DATCON,DMCB,(2,ACDTUSED),(1,DATE)                                
ACP560   CLC   DATE(2),START                                                    
         BL    EXIT                                                             
         CLC   DATE(2),END                                                      
         BH    EXIT                                                             
ACP570   MVC   SVANAL,TRNSANAL                                                  
         MVC   AMOUNTS(24),=3PL8'0'                                             
         CLI   JOBSW,0             PRINT JOB,W-CODE AND SUPPLIER                
         BNE   ACP590                                                           
         MVI   JOBSW,1                                                          
         MVI   CLIACTV,C'Y'                                                     
         TM    STATUS,CLISORT+PRODSORT  DON'T PRINT JOB IF A                    
         BM    ACP590                   HIGH LEVEL SORT                         
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BNM   ACP580                                                           
         CLI   QOPT4,C'J'          SUPPRESS JOB LEVEL DETAIL                    
         BE    ACP590                                                           
         CLI   QOPT4,C'P'          SUPPRESS JOB, PRODUCT LEVEL DETAIL           
         BE    ACP590                                                           
ACP580   BAS   RE,PRINTJOB                                                      
         SPACE 1                                                                
ACP590   TM    STATUS,SUPPLIER     SUPPLIERS ONLY                               
         BO    ACP610                                                           
         CLI   ANALSW,0                                                         
         BNE   ACP610                                                           
         MVI   ANALSW,1                                                         
*                                  GET  WORK-CODE NAME                          
         GOTO1 =A(WCNAME),DMCB,(RC),RR=RELO                                     
         CLI   SORTSW,C'Y'         IF I'M SORTING                               
         BE    ACP600              DON'T MOVE TO PRINT LINE                     
         MVC   P+1(2),SVANAL                                                    
         MVC   P+4(15),SVANALNM                                                 
         BAS   RE,MYREPORT                                                      
         B     ACP610                                                           
         SPACE 1                                                                
ACP600   DS    0H                  SAVE FOR SORTER                              
         MVC   SORTWC2,SVANAL                                                   
         CLI   KEYDISP,X'00'       IS MAJOR SORT DISP 0 ?                       
         BNE   *+10                NO, THEN MUST BE SUPPLIER                    
         MVC   SORTWC,SVANAL       YES, THEN IT'S WORKCODE                      
         MVC   SORTWCNM,SVANALNM                                                
         MVC   SORTSTAT,SVANALST                                                
         TM    STATUS,WORKCODE+WORKSUP IF A WORK-CODE REPORT                    
         BNM   ACP610                                                           
         MVC   BUFFKEY(2),SVANAL   AND BUFFALO                                  
         MVC   BUFFNAME(15),SVANALNM                                            
         SPACE 1                                                                
ACP610   TM    STATUS,WORKCODE     WORK-CODES ONLY                              
         BO    ACP630                                                           
         CLI   SUPPSW,0                                                         
         BNE   ACP630                                                           
         MVI   SUPPSW,1                                                         
         L     R3,ADSUBAC                                                       
*                                                                               
         USING TRSUBHD,R3                                                       
*                                                                               
         ZIC   R4,TRSBLEN                                                       
         SH    R4,=H'17'                                                        
         CLI   SORTSW,C'Y'         AM I WAITING FOR SORT                        
         BE    ACP620                                                           
         GOTO1 CHOPPER,DMCB,((R4),TRSBNAME),(28,PNAME),(C'P',2)                 
         B     ACP630                                                           
         SPACE 1                                                                
ACP620   MVC   SORTSUPP,TRSBACNT   SAVE SUPPLIER CODE                           
         MVC   SORTSPNM,SPACES                                                  
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SORTSPNM(0),TRSBNAME   AND NAME                                  
         TM    STATUS,WORKCODE+WORKSUP IF NOT A WORK-CODE REPORT                
         BM    ACP630                                                           
         MVC   BUFFKEY,SORTSUPP    SAVE FOR BUFFALO                             
         MVC   BUFFNAME,SORTSPNM                                                
         SPACE 1                                                                
*  OPTION FOR BILLED ITEMS ONLY                                                 
         SPACE 1                                                                
ACP630   DS    0H                                                               
         MVI   OPTSW,C'Y'                                                       
         CLI   QOPT1,C'B'          BILLED ITEMS ONLY ?                          
         BNE   ACP650              NO, SKIP THIS                                
         CLI   BILLED,C'Y'         YES, IS ITEM FULLY BILLED ?                  
         BNE   PARTIAL             NO, MUST BE PARTIAL                          
         SPACE 1                                                                
ACP640   MVI   OPTSW,C'N'          YES, RESET SWITCH                            
         ZAP   AMOUNTS(8),NET       USE NET TOTAL                               
         B     ACP690               DO CALCS AND PRINT IT                       
         SPACE 1                                                                
* OPTION FOR UNBILLED ITEMS ONLY                                                
         SPACE 1                                                                
ACP650   CLI   QOPT1,C'U'          NOT BILLED ONLY?                             
         BNE   ACP680              NO, SKIP THIS                                
*        CLI   BILLED,C'N'                                                      
*        BE    *+16                                                             
ACP660   MVI   OPTSW,C'N'          YES, RESET SWITCH                            
         SPACE 1                                                                
ACP670   ZAP   AMOUNTS(8),UNBILLED USE UNBILLED TOTAL                           
         MVC   BILLDATE,SPACES     CLEAR DATE AND INVOICE NUMBER                
         MVC   BILLNO,SPACES                                                    
         B     ACP690                                                           
         SPACE 1                                                                
* OPTION FOR BILLED AND UNBILLED ITEMS                                          
         SPACE 1                                                                
ACP680   CLI   BILLED,C'Y'         IS ITEM FULLY BILLED                         
         BE    ACP640              YES, TREAT AS QOPT1 = B                      
         CLI   BILLED,C'N'         IS ITEM NOT BILLED                           
         BE    ACP660              YES, TREAT AS QOPT1 = U                      
         CLI   QOPT2,C' '          ARE WE PRINTING DETAIL ?                     
         BE    ACP670              YES, PRINT UNBILLED PORTION FIRST            
         MVI   OPTSW,C'N'          NO, RESET SWITCH                             
         ZAP   AMOUNTS(8),NET      COMBINE BILLED AND UNBILLED                  
         AP    AMOUNTS(8),UNBILLED                                              
         SPACE 1                                                                
* PRINT  FULLY  BILLED AND UNBILLED ITEMS                                       
         SPACE 1                                                                
ACP690   CLI   QOPT2,C' '          AM I PRINTING NOW                            
         BE    *+16                                                             
         CLI   QOPT3,C'G'          OPT2 PROFILE TO SHOW AS GROSS                
         BNE   ACP740              DEFAULT IS NET                               
         B     ACP700                                                           
         MVC   PREF,TRNSREF        FILL IN REST OF LINE                         
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,PDATE)                               
         SPACE 1                                                                
ACP700   TM    TRNSSTAT,X'01'                                                   
         BO    ACP740              NON-COMMISSIONABLE ITEM                      
         CLC   TRNSANAL,=C'99'                                                  
         BNE   ACP710                                                           
         ZAP   AMOUNTS+8(8),TRNSNARR+15(6)                                      
         CLI   QOPT2,C' '          AM I PRINTING NOW                            
         BNE   ACP740                                                           
         ZAP   AMOUNTS+16(8),AMOUNTS(8)                                         
         AP    AMOUNTS+16(8),AMOUNTS+8(8)   GROSS                               
         B     ACP760                                                           
         SPACE 1                                                                
*        CLI   BILLED,C'N'         IS ITEM NOT BILLED ?                         
*        BE    ACP730              YES, GET DISCOUNT AND COMMISSION             
*        ZAP   AMOUNTS+8(8),COMM   NO, USE ALREADY CALCULATED AMOUNT            
*        B     ACP740                                                           
*                                                                               
         USING GOBLOCKD,R3                                                      
*                                                                               
ACP710   L     R3,ADGOBLOC                                                      
         MVC   GOSELWC,TRNSANAL                                                 
         GOTO1 GETOPT,DMCB,ADGOBLOC                                             
         XC    GOSELWC,GOSELWC                                                  
         ZAP   PL13,GOAGYCOM        COMMISSION RATE                             
         ZAP   DUB,AMOUNTS(8)       DEBIT POSTING                               
*                                                                               
         DROP  R3                                                               
*                                                                               
*&&US                                                                           
         LR    R3,R2               ADDRESS X'44' ELEMENT AGAIN                  
         SH    R3,DATADISP         GET BACK TO RECORD                           
*                                                                               
         USING ACKEYD,R3                                                        
*                                                                               
         OC    ACDTUSED,ACDTUSED   FULLY BILLED, GET CASH DISCOUNT              
         BNZ   ACP720               FROM X'50' ELEMENT                          
         AP    DUB,CASH            USE CASH DISCOUNT FROM X'4B' ELEMENT         
         B     ACP730              BRANCH AROUND                                
         SPACE 1                                                                
ACP720   LR    R3,R2                                                            
         MVI   ELCODE,X'50'        CD ELEMENT                                   
         BAS   RE,FIRSTEL                                                       
         BNE   ACP730                                                           
*                                                                               
         USING TRCASHD,R3                                                       
*                                                                               
         CLI   TRCSTYPE,C'D'                                                    
         BNE   ACP730                                                           
         AP    DUB,TRCSAMNT        ADD BACK TO NET FOR COMM. CALC.              
*&&                                                                             
ACP730   MP    PL13,DUB            COMM RATE X POSTING                          
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   DUB,PL13                                                         
         ZAP   AMOUNTS+8(8),DUB    COMMISSION                                   
         SPACE 1                                                                
ACP740   ZAP   AMOUNTS+16(8),AMOUNTS(8)                                         
         AP    AMOUNTS+16(8),AMOUNTS+8(8)   GROSS/YTD                           
         CLI   QOPT2,C' '          AM I PRINTING TRANS. DETAILS                 
         BE    ACP750                                                           
         ZAP   AMOUNTS(8),=P'0'    CLEAR CURRENT MONTH                          
         ZAP   AMOUNTS+8(8),=P'0'  CLEAR QTD                                    
         CLC   DATE(2),QTRST                                                    
         BL    *+10                                                             
         ZAP   AMOUNTS+8(8),AMOUNTS+16(8)                                       
         CLC   DATE(2),END                                                      
         BNE   ACP780                                                           
         ZAP   AMOUNTS(8),AMOUNTS+16(8)                                         
         B     ACP780                                                           
         SPACE 1                                                                
ACP750   CLI   BILLED,C'N'         IF TRANSACTION WAS BILLED                    
         BE    ACP760                                                           
         MVC   PBILLNO,=C'  **  '  MOVE BILL NUMBER AND                         
         MVC   PBILLDTE,=C'   **   '    DATE TO PRINT LINE.                     
         CLI   BILLNO,C' '         WAS THERE A 4B ELEMENT                       
         BE    ACP760                                                           
         MVC   PBILLNO,BILLNO                                                   
         GOTO1 DATCON,DMCB,(1,BILLDATE),(8,PBILLDTE)                            
         SPACE 1                                                                
ACP760   LA    R4,AMOUNTS                                                       
         CLI   PROGPROF,C'Y'       PROFILE TO SUPPRESS ZERO AMOUNTS             
         BNE   ACP770                                                           
         CLC   0(24,R4),=3PL8'0'                                                
         BNE   ACP770                                                           
         MVC   P,SPACES                                                         
         B     EXIT                                                             
         SPACE 1                                                                
ACP770   BAS   RE,FORMAT                                                        
         BAS   RE,MYREPORT                                                      
         MVI   SKIPSW,C'Y'                                                      
         SPACE 1                                                                
ACP780   AP    TCOUNT,=P'1'                                                     
         BAS   RE,ADDEM        ADD TO COUNTERS, MEDIA TABLE, BUFFALO            
         CLI   SORTSW,C'Y'                                                      
         BNE   ACP810                                                           
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BNM   ACP790                                                           
         CLI   QOPT4,C'J'          SUPPRESS JOB LEVEL DETAIL                    
         BE    ACP810                                                           
         CLI   QOPT4,C'P'          SUPPRESS JOB, PRODUCT LEVEL DETAIL           
         BE    ACP810                                                           
ACP790   ZAP   SORTCURR,AMOUNTS    PUT RECORD TO SORTER                         
         ZAP   SORTQTD,AMOUNTS+8(8)                                             
         ZAP   SORTYTD,AMOUNTS+16(8)                                            
         TM    STATUS,JOBSORT      JOB LEVEL SORT                               
         BO    ACP820                                                           
         GOTO1 ADSORTER,DMCB,=C'PUT',SORTREC                                    
*                                                                               
ACP800   MVI   SORTACTV,C'Y'       SORT IS ACTIVE                               
*                                                                               
ACP810   CLI   OPTSW,C'Y'          IS SWITCH SET FOR PARTIALLY BILLED ?         
         BE    PARTIAL             YES, HANDLE BILLED PORTION NOW               
         B     EXIT                                                             
*                                                                               
ACP820   GOTO1 BINADD,DMCB,SORTREC,AJOBTAB                                      
         MVI   BINACTV,C'Y'                                                     
         B     ACP800                                                           
         EJECT ,                                                                
ACP70    CLI   MODE,SBACLAST                                                    
         BNE   ACP80                                                            
         CP    TCOUNT,=P'0'                                                     
         BE    ACP78                                                            
         CLI   SORTSW,C'Y'         GET OUT IF SORTING                           
         BE    ACP76                                                            
         LA    R4,STOTS                                                         
         CP    TCOUNT,=P'1'        REGULAR REPORT                               
         BE    ACP76                                                            
         MVC   LEVEL,=C'SUPPLIER'                                               
         LA    R2,SPACES                                                        
         XR    R3,R3                                                            
         BAS   RE,TOTALS                                                        
ACP76    AP    SCOUNT,=P'1'                                                     
ACP78    ZAP   TCOUNT,=P'0'                                                     
         MVC   STOTS(24),=3PL8'0'                                               
         B     EXIT                                                             
         EJECT ,                                                                
ACP80    CLI   MODE,ANALLAST                                                    
         BNE   ACP90                                                            
         CLI   SORTSW,C'Y'                                                      
         BE    ACP86                                                            
         CP    SCOUNT,=P'1'        NO SUPPLIERS IMPLIES NO TRANSACTION          
         BL    ACP88                                                            
         BE    ACP86                                                            
         LA    R4,WTOTS                                                         
         MVC   LEVEL,=CL8'W-CODE'                                               
         LA    R2,SVANAL                                                        
         LA    R3,1                                                             
         BAS   RE,TOTALS                                                        
ACP86    AP    WCOUNT,=P'1'                                                     
ACP88    MVC   WTOTS(24),=3PL8'0'                                               
         ZAP   SCOUNT,=P'0'                                                     
         B     EXIT                                                             
         EJECT ,                                                                
ACP90    CLI   MODE,ACCLAST                                                     
         BNE   ACP95                                                            
         CLI   SORTSW,C'Y'         AM I SORTING                                 
         BNE   ACP91                                                            
         TM    STATUS,JOBSORT                                                   
         BO    ACP90B                                                           
         CP    SCOUNT,=P'1'                                                     
         BL    ACP94               DON'T ADD TO JOB COUNTER                     
         B     ACP93                                                            
ACP90B   DS    0H                                                               
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BNM   ACP90D                                                           
         CLI   QOPT4,C'J'          SUPPRESS JOB LEVEL DETAIL                    
         BE    ACP93                                                            
         CLI   QOPT4,C'P'          SUPPRESS JOB, PRODUCT LEVEL DETAIL           
         BE    ACP93                                                            
ACP90D   BAS   RE,GETSORT                                                       
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BM    ACP91               JUST CLEAR IT                                
         GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,(X'80',3)                          
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
ACP91    LA    R2,WCOUNT                                                        
         CLI   SORTSW,C'Y'        IF SORTING                                    
         BNE   *+8                                                              
         LA    R2,SCOUNT           THEN USE SUPPLIER COUNT                      
         CP    0(L'COUNTERS,R2),=P'1'                                           
         BE    ACP93                                                            
         BL    ACP94                                                            
         CLC   JTOTS(24),=3PL8'0'                                               
         BE    ACP94                                                            
         MVC   LEVEL,=CL8'JOB'                                                  
         LA    R2,ACCOUNT                                                       
         LA    R3,L'ACCOUNT-1                                                   
         LA    R4,JTOTS                                                         
         BAS   RE,TOTALS                                                        
ACP93    AP    JCOUNT,=P'1'                                                     
ACP94    MVC   JTOTS(24),=3PL8'0'                                               
         ZAP   TOTQPCT,=P'0'                                                    
         ZAP   TOTYPCT,=P'0'                                                    
         ZAP   QTOT,=P'0'                                                       
         ZAP   YTOT,=P'0'                                                       
         ZAP   WCOUNT,=P'0'                                                     
         B     EXIT                                                             
         EJECT ,                                                                
ACP95    CLI   MODE,LEVBLAST                                                    
         BNE   ACP100                                                           
         LA    R4,JCOUNT                                                        
         CLI   MYHEAD8,C'J'        IF REPORT IS NOT AT JOB LEVEL                
         BE    *+8                                                              
         LA    R4,SCOUNT           THEN USE SUPPLIER COUNT                      
         CLI   SORTSW,C'Y'         AM I SORTING                                 
         BNE   ACP95B                                                           
         TM    STATUS,JOBSORT                                                   
         BO    ACP95B              JUST PRINT TOTALS                            
         TM    STATUS,CLISORT                                                   
         BO    ACP97               HOLD OFF UNTIL LATER                         
         BAS   RE,GETSORT                                                       
ACP95B   TM    STATUS,BUFFIT    AM I GETTING HIGH LVLS FROM BUFFALO             
         BZ    ACP96                                                            
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BNM   ACP95E              JUST CLEAR IT IF NOT                         
         CLI   QOPT4,C'P'          SUPPRESS JOB,PROD LEVEL DETAIL               
         BE    ACP95D                                                           
         CP    0(L'COUNTERS,R4),=P'1'                                           
         BL    ACP95D                                                           
         CLI   QOPT4,C'J'          SUPPRESS JOB LEVEL DETAIL                    
         BE    *+14                                                             
         CP    0(L'COUNTERS,R4),=P'1'                                           
         BE    ACP95D                                                           
         MVI   FORCEHED,C'Y'       SUMMARY ON NEW PAGE                          
         LA    R2,1                LEVEL FOR BUFFALO                            
         LA    R3,LEVBACNT                                                      
         MVC   LEVNAME,=C'PRODUCT'                                              
         BAS   RE,GETBUFF                                                       
ACP95D   GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,(X'80',2)                          
         B     ACP95F                                                           
ACP95E   GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,(X'80',3)                          
ACP95F   GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BNM   ACP96                                                            
         CLI   QOPT4,C'P'          SUPPRESS JOB,PROD LEVEL DETAIL               
         BE    ACP97                                                            
         SPACE 1                                                                
ACP96    CP    0(L'COUNTERS,R4),=P'2'                                           
         BL    ACP97                                                            
         BAS   RE,MYREPORT                                                      
         MVC   LEVEL,=CL8'PRODUCT'                                              
         LA    R2,LEVBACNT                                                      
         LA    R3,L'LEVBACNT-1                                                  
         LA    R4,PTOTS                                                         
         BAS   RE,TOTALS                                                        
ACP97    TM    STATUS,BUFFIT                                                    
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PTOTS(24),=3PL8'0'                                               
         ZAP   JCOUNT,=P'0'                                                     
         ZAP   TOTQPCT,=P'0'                                                    
         ZAP   TOTYPCT,=P'0'                                                    
         ZAP   QTOT,=P'0'                                                       
         ZAP   YTOT,=P'0'                                                       
         B     EXIT                                                             
         EJECT ,                                                                
ACP100   CLI   MODE,LEVALAST                                                    
         BNE   ACP120                                                           
         TM    STATUS,CLISORT      HOLDING ALL PRINTING UNTIL NOW               
         BZ    *+8                                                              
         BAS   RE,GETSORT                                                       
         TM    STATUS,BUFFIT    AM I GETTING HIGH LVLS FROM BUFFALO             
         BZ    ACP110                                                           
         TM    STATUS,WORKCODE+SUPPLIER                                         
         BNM   ACP105              JUST CLEAR IT                                
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,2                LEVEL FOR BUFFALO                            
         LA    R3,LEVAACNT                                                      
         MVC   LEVNAME,=CL7'CLIENT'                                             
         BAS   RE,GETBUFF                                                       
         GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,2,(X'80',3)                          
         B     ACP108                                                           
ACP105   GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,(X'80',3)                          
ACP108   GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,1,(X'80',2)                        
         SPACE 1                                                                
ACP110   CLI   CLIACTV,C'Y'                                                     
         BNE   EXIT                                                             
         BAS   RE,MYREPORT                                                      
         LA    R2,SPACES                                                        
         XR    R3,R3                                                            
         LA    R4,CTOTS                                                         
         MVC   LEVEL,=CL8'CLIENT'                                               
         BAS   RE,TOTALS                                                        
         MVC   CTOTS(24),=3PL8'0'                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   TOTQPCT,=P'0'                                                    
         ZAP   TOTYPCT,=P'0'                                                    
         ZAP   QTOT,=P'0'                                                       
         ZAP   YTOT,=P'0'                                                       
         B     EXIT                                                             
         EJECT ,                                                                
ACP120   CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         TM    STATUS,BUFFIT                                                    
         BZ    ACP121                                                           
         LA    R2,3                                                             
         LA    R3,SPACES                                                        
         CLI   RCSUBPRG,0                                                       
         MVI   RCSUBPRG,7                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,8                                                       
         MVC   LEVNAME,SPACES                                                   
         BAS   RE,GETBUFF                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,1,(X'80',3)                        
ACP121   MVI   RCSUBPRG,1                                                       
         CLI   SORTSW,C'Y'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,6                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   AMOUNTS(24),=3PL8'0'                                             
         LA    R3,MEDTAB                                                        
ACP122   CLI   0(R3),X'FF'                                                      
         BE    ACP125                                                           
         CLC   16(24,R3),=3PL8'0'                                               
         BE    ACP124                                                           
         MVC   P+3(1),0(R3)                                                     
         MVC   P+7(15),1(R3)                                                    
         LA    R2,16(R3)                                                        
         LA    R4,P+23                                                          
         LA    R5,3                                                             
         LA    R6,AMOUNTS                                                       
ACP123   EDIT  (P8,0(R2)),(15,0(R4)),2,COMMAS=YES,MINUS=YES                     
         AP    0(8,R6),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R4,16(R4)                                                        
         LA    R6,8(R6)                                                         
         BCT   R5,ACP123                                                        
         MVC   HEAD1+46(L'MYRPTNM),MYRPTNM                                      
         GOTO1 ACREPORT                                                         
ACP124   LA    R3,L'MEDTAB(R3)                                                  
         B     ACP122                                                           
         SPACE 1                                                                
ACP125   CLC   AMOUNTS(24),=3PL8'0'                                             
         BE    EXIT                                                             
         GOTO1 ACREPORT                                                         
         MVC   P+7(10),=C'* TOTALS *'                                           
         LA    R2,AMOUNTS                                                       
         LA    R4,P+23                                                          
         LA    R5,3                                                             
ACP126   EDIT  (P8,0(R2)),(15,0(R4)),2,COMMAS=YES,MINUS=YES                     
         LA    R2,8(R2)                                                         
         LA    R4,16(R4)                                                        
         BCT   R5,ACP126                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
TOTALS   NTR1                                                                   
*                                  R2=A(ACCOUNT CODE OR SPACES)                 
*                                  R3=L'ACCOUNT CODE-1                          
*                                  R4=A(ACCUMS)                                 
*                                                                               
         USING PRINTD,R6           R6=PRINT LINE                                
*                                                                               
         LA    R6,P                                                             
         MVC   PTOTFOR(10),TOTFOR                                               
         BAS   RE,FORMAT                                                        
         MVC   PTOTFOR+11(L'LEVEL),LEVEL                                        
         LA    R5,PTOTFOR+L'LEVEL+10                                            
*                                                                               
TOTALS2  CLI   0(R5),C' '          BUMP BACKWARDS TO END OF NAME                
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         B     TOTALS2                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R5),0(R2)                                                    
         MVI   SPACING,2                                                        
         BAS   RE,MYREPORT                                                      
         MVI   SKIPSW,C'N'                                                      
         B     EXIT                                                             
         SPACE 3                                                                
MYREPORT NTR1                                                                   
         MVC   HEAD1+46(L'MYRPTNM),MYRPTNM                                      
         CLI   RCSUBPRG,7                                                       
         BNL   MYRPT1                                                           
         MVC   HEAD5+89(L'PERIOD),PERIOD                                        
         MVC   HEAD5+9(L'LEVAACNT),LEVAACNT                                     
         MVC   HEAD5+16(36),LEVANAME                                            
         MVC   HEAD8+1(L'MYHEAD8),MYHEAD8                                       
         MVC   HEAD8+L'MYHEAD8+2(16),=C'NUMBER AND NAME/'                       
         GOTO1 ADSQUASH,DMCB,HEAD8+1,35                                         
MYRPT1   CLI   QOPT2,C' '                                                       
         BE    MYRPT2                                                           
         MVC   HEAD6+79(13),=C'NET AMOUNTS  '                                   
         CLI   QOPT3,C'G'          GROSS DOLLARS OPTION                         
         BNE   *+10                                                             
         MVC   HEAD6+79(13),=C'GROSS AMOUNTS'                                   
         CLI   QOPT5,C'S'          SUPPRESS CURRENT MONTH                       
         BE    MYRPT2                                                           
         MVC   HEAD8+53(7),=C'CURRENT'                                          
         MVC   HEAD9+53(7),=C' MONTH '                                          
MYRPT2   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
*              FORMAT A LINE OF ACCUMULATORS                                    
         SPACE 2                                                                
*                                  R4=A(ACCUMS)                                 
*                                  R5=A(LAST MAJOR SORT KEY)                    
*                                  R6=A(MAJOR SORT KEY)                         
FORMAT   NTR1                                                                   
         CLI   SORTSW,C'Y'         SPECIAL COLUMNS                              
         BNE   FORMATC                                                          
         CLI   MAJOR,C'Y'                                                       
         BE    FORMATA                                                          
         CLI   MINOR,C'Y'          AM DOING A MINOR KEY CHANGE                  
         BNE   FORMATA                                                          
         LR    R5,R6               THEN PASS GETKEY CURRENT KEY                 
         MVI   MINOR,C'N'                                                       
*                                                                               
         USING PRINTD,R6                                                        
*                                                                               
FORMATA  LA    R6,P                                                             
         CLC   PTOTFOR(10),TOTFOR  IS THIS A TOTAL LINE                         
         BE    FORMATB                                                          
         BAS   RE,GENPCT           CALCULATE PERCENTAGES                        
         LA    R5,QPCT                                                          
         B     *+8                                                              
FORMATB  LA    R5,TOTQPCT          USE PCT TOTALS                               
         LA    R3,5                                                             
         LA    RF,PNET                                                          
         CLI   QOPT5,C'S'          SUPPRESS CURRENT MONTH                       
         BE    FORMATE2                                                         
         B     FORMATD                                                          
FORMATC  LA    R3,3                                                             
         LA    RF,PNET                                                          
FORMATD  CLI   SORTSW,C'Y'                                                      
         BNE   FORMATE                                                          
         CH    R3,=H'3'            QTD PCT                                      
         BE    FORMATF                                                          
         CH    R3,=H'1'            YTD PCT                                      
         BE    FORMATF                                                          
FORMATE  EDIT  (P8,0(R4)),(15,0(RF)),2,COMMAS=YES,MINUS=YES                     
FORMATE2 LA    R4,8(R4)                                                         
         LA    RF,16(RF)                                                        
         B     FORMATG                                                          
FORMATF  CLC   PTOTFOR(10),TOTFOR  IS THIS A TOTAL LINE                         
         BNE   *+14                                                             
         CP    0(8,R5),=P'0'       THEN DON'T PRINT ZERO PERCENT                
         BE    FORMATF2                                                         
         EDIT  (P8,0(R5)),(7,0(RF)),2,MINUS=YES                                 
*&&US                                                                           
         CLI   6(RF),C'-'          NO PERCENT IF NEGATIVE                       
         BE    *+8                                                              
         MVI   6(RF),X'6C'         PERCENT SIGN                                 
*&&                                                                             
FORMATF2 LA    R5,8(R5)                                                         
         LA    RF,8(RF)                                                         
FORMATG  BCT   R3,FORMATD                                                       
         CLC   PTOTFOR(10),TOTFOR  IS THIS A TOTAL LINE                         
         BNE   EXIT                                                             
         ZAP   TOTQPCT,=P'0'       CLEAR PCT ACCUMS                             
         ZAP   TOTYPCT,=P'0'                                                    
         ZAP   QTOT,=P'0'          AND RUNNING TOTALS                           
         ZAP   YTOT,=P'0'                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*              GENERATE PERCENTAGES                                             
         SPACE 2                                                                
*                                  R4=A(ACCUMS)                                 
*                                  R5=A(SORT KEY) FOR GETKEY                    
GENPCT   NTR1                                                                   
         TM    STATUS,WORKCODE+SUPPLIER   W/C, SUPP ONLY                        
         BM    GENPCT0                                                          
         CLI   MODE,REQLAST                                                     
         BE    GENPCT0                                                          
         CLI   MAJOR,C'Y'          NEED BUFFALO RECORD                          
         BNE   *+12                                                             
         BAS   RE,GETKEY           GET TOTALS FROM BUFFALO                      
         MVI   MAJOR,C'N'                                                       
         LA    R5,BUFFCURR         ELSE USE SAME TOTALS AS LAST TIME            
         B     GENPCT1                                                          
GENPCT0  LA    R5,JTOTS                                                         
         CLI   MODE,ACCLAST                                                     
         BE    GENPCT1                                                          
         LA    R5,PTOTS                                                         
         CLI   MODE,LEVBLAST                                                    
         BE    GENPCT1                                                          
         LA    R5,CTOTS                                                         
GENPCT1  ZAP   QPCT,=P'0'                                                       
         ZAP   YPCT,=P'0'                                                       
         LA    R0,2                                                             
         LA    R4,8(R4)            SHIFT TO QTD                                 
         LA    R5,8(R5)                                                         
         LA    R6,QPCT                                                          
         LA    RE,QTOT                                                          
         LA    RF,TOTQPCT                                                       
GENPCT2  AP    0(8,RE),0(8,R4)     KEEP RUNNING TOTAL                           
         ZAP   PL13,0(8,RE)        USE THAT TO GET PCT SO FAR                   
         MP    PL13,=P'20000'      NEED 4 DECIMAL PLACES/MULT. BY 2             
         ZAP   DUB,0(8,R5)         TOTAL                                        
         BZ    GENPCT4             CANNOT DIVIDE BY ZERO                        
         DP    PL13,DUB+2(6)       (ACCUM X 10000)/TOTAL                        
         CP    PL13(7),=P'0'                                                    
         BL    *+10                                                             
         AP    PL13(7),=P'1'                                                    
         ZAP   DUB,PL13(7)         ROUND IT                                     
         DP    DUB,=P'2'                                                        
         ZAP   0(8,R6),DUB(7)      ROUNDED RUNNING PCT.                         
         SP    0(8,R6),0(8,RF)     SUBTRACT LAST TOTAL PCT. TO GET THIS         
         AP    0(8,RF),0(8,R6)     AND ADD TO RUNNING TOTAL                     
GENPCT4  LA    R4,8(R4)            SKIP TO YTD                                  
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,GENPCT2                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*              GET HIGH LEVELS TOTALS FROM BUFFALO                              
         SPACE 2                                                                
*                                  R5=A(SORT KEY) TO MATCH                      
GETKEY   NTR1                                                                   
         CLI   FIRST,C'H'          DO I HAVE TO READ HIGH                       
         BE    GETKEY1                                                          
         CLI   FIRST,C'Y'          IS THIS FIRST TIME                           
         BNE   GETKEY2                                                          
         XC    BUFFREC,BUFFREC                                                  
GETKEY1  MVI   FIRST,C'N'                                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFFREC,1                           
         TM    DMCB+8,X'80'                                                     
         BZ    GETKEY4                                                          
         B     GETKEYX             NOTHING IN BUFFALO                           
         SPACE 1                                                                
GETKEY2  GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFFREC,1                            
         TM    DMCB+8,X'80'                                                     
         BZ    GETKEY4                                                          
         B     GETKEYX             NOTHING ELSE IN BUFFALO                      
         SPACE 1                                                                
GETKEY4  LA    R3,BUFFKEY                                                       
         TM    STATUS,WORKSUP                                                   
         BO    *+8                                                              
         LA    R3,1(R3)            SKIP COMPANY CODE IF SUPPLIER                
         ZIC   R4,KEYLEN                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R3)       SORT KEY = BUFFALO KEY                       
         BE    EXIT                OK - GET OUT                                 
         BH    GETKEY2             SORT KEY HIGHER - GET NEXT                   
         LA    R3,0(R4,R3)         NOT FOUND - MUST BE ZERO                     
         ZIC   R4,0(R3)                                                         
         BCTR  R4,0                DECREASE BUFFKEY FOR NEXT READ               
         STC   R4,0(R3)                                                         
GETKEYX  MVI   FIRST,C'H'          FORCE A READ HIGH NEXT TIME                  
         MVC   BUFFCURR(24),=3PL8'0'  MOVE IN ZEROES TO BUFF. ACCUMS            
         B     EXIT                                                             
         EJECT ,                                                                
*              PRINT A JOB NUMBER AND NAME                                      
*                                                                               
PRINTJOB NTR1                                                                   
         CLI   SKIPSW,C'Y'         DO I NEED TO SKIP A LINE                     
         BNE   *+8                                                              
         BAS   RE,MYREPORT                                                      
         MVC   P+1(L'ACCOUNT),ACCOUNT  PRODUCT AND JOB CODE                     
         L     R3,ADACCNAM                                                      
*                                                                               
         USING ACNAMED,R3                                                       
*                                                                               
         ZIC   R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),ACNMNAME                                                 
         GOTO1 VUNDER,DMCB,(50,P+1),PSECOND+1                                   
         MVI   SPACING,2                                                        
         BAS   RE,MYREPORT                                                      
         B     EXIT                                                             
         EJECT ,                                                                
* GET BILLING TOTALS AND DETERMINE IF ITEM IS FULLY BILLED, PARTIALLY           
* BILLED OR UNBILLED                                                            
         SPACE 1                                                                
         USING TRBDETD,R3                                                       
         USING ACKEYD,R2                                                        
         SPACE 1                                                                
SUMIT    NTR1                                                                   
         L     R3,ADTRANS                                                       
         LR    R2,R3                                                            
         SH    R2,DATADISP                                                      
         MVI   BILLED,C'N'                                                      
         MVC   BILLNO,SPACES                                                    
         MVC   BILLDATE,SPACES                                                  
         ZAP   ACCEPT,=P'0'        ACCEPTED NET AMOUNT                          
         ZAP   NET,=P'0'           TOTAL NET AMOUNT                             
         ZAP   CASH,=P'0'          TOTAL CASH DISCOUNT                          
         ZAP   COMM,=P'0'          TOTAL COMMISSION                             
         ZAP   UNBILLED,=P'0'      TOTAL UNBILLED AMOUNT                        
         ZAP   TOTAL,=P'0'         ACCUMULATOR                                  
         ZAP   CONVERT,=P'0'       CONVERSION AREA                              
*                                                                               
         LR    R5,R3               SAVE ADDRESS OF TRANSACTION                  
*                                                                               
         CLC   ACKEYWRK,=C'99'     BILLING S/B IGNORED                          
         BE    SUM140                                                           
*                                                                               
         MVI   ELCODE,X'4B'                                                     
SUM020   BAS   RE,NEXTEL                                                        
         BNE   SUM140                                                           
         CLC   TRBDNO,SPACES       BILL NUMBER BLANK OR ZERO,                   
         BNH   SUM020               MEANS NOT BILLED.                           
         GOTO1 DATCON,DMCB,(2,TRBDDTE),(1,WORK)                                 
         OC    ACDTUSED,ACDTUSED   IF NOT FULLY BILLED USE X'4B' AMOUNT         
         BZ    SUM060                                                           
         ST    R3,SAVE4B           SAVE THIS 4B IN CASE NO MORE                 
*                                                                               
SUM030   CLC   TRBDRUN,ACDTUSED    IS THIS THE RIGHT 4B?                        
         BE    SUM040              YES                                          
         MVI   ELCODE,X'4B'        NO, KEEP LOOKING                             
         BAS   RE,NEXTEL                                                        
         BE    SUM030                                                           
         L     R3,SAVE4B           USE FIRST ONE                                
*                                                                               
SUM040   GOTO1 DATCON,DMCB,(2,TRBDDTE),(1,WORK)                                 
         ZAP   NET,TRNSAMNT-TRANSD(L'TRNSAMNT,R5)                               
         ZAP   TOTAL,NET                                                        
         B     SUM080              GET CASH DISCOUNT AND COMMISSION             
*                                                                               
SUM060   ICM   RF,15,TRBDAMNT                                                   
         CVD   RF,CONVERT          BILLED AMOUNT                                
         AP    NET,CONVERT         ADD TO TOTAL NET BILLED                      
         CLI   QOPT1,C'B'          PARTIALLY BILLED ITEM AND REQUESTED          
         BNE   SUM065               BILLED PORTION ONLY                         
         CLC   WORK(2),START         IF NOT WITHIN RANGE, DON'T PROCESS         
         BL    SUM020               ANY FURTHER                                 
         CLC   WORK(2),END                                                      
         BH    SUM020                                                           
         SPACE 1                                                                
SUM065   ZAP   TOTAL,CONVERT       SAVE AS COMMISSIONABLE AMOUNT                
         AP    ACCEPT,TOTAL        ADD ACCEPTED DOLLARS HERE                    
         SPACE 1                                                                
SUM080   MVC   BILLNO,TRBDNO                                                    
         MVC   BILLDATE,WORK                                                    
         ICM   RF,15,TRBDCSD                                                    
         CVD   RF,CONVERT          CASH DISCOUNT                                
         AP    CASH,CONVERT        ADD TO TOTAL NET CASH DISCOUNT               
         AP    TOTAL,CONVERT       ADD TO COMMISSIONABLE AMOUNT                 
         TM    TRBDCMST,X'80'      IS COMMISSION RATE 4 DECIMALS ?              
         BO    SUM100              YES, GET PACKED VALUE                        
         SR    RF,RF               NO, CLEAR REGISTER                           
         ICM   RF,12,TRBDCMR        VALUE IS BINARY                             
         CVD   RF,CONVERT          MAKE IT DECIMAL                              
         MP    TOTAL,CONVERT       GET THE COMMISSION AMOUNT                    
         SRP   TOTAL,64-4,5        MAKE IT 2 DECIMAL PLACES                     
         B     *+16                                                             
SUM100   MP    TOTAL,TRBDCMP       GET COMMISSION AMOUNT                        
         SRP   TOTAL,64-6,5        MAKE IT 4 DECIMAL PLACES                     
         AP    COMM,TOTAL          ADD IT TO TOTAL COMMISSION AMOUNT            
         OC    ACDTUSED,ACDTUSED   IF NOT FULLY BILLED, GET NEXT                
         BZ    SUM020                                                           
SUM120   MVI   BILLED,C'Y'         INDICATE FULLY BILLED IF = TO NET            
         B     EXIT                                                             
         SPACE 1                                                                
SUM140   OC    ACDTUSED,ACDTUSED   IF FULLY BILLED AND NO VALID 4B              
         BNZ   SUM180              SET DUMMY BILLNO                             
*                                                                               
         CLI   QOPT1,C'B'                                                       
         BNE   *+10                                                             
         ZAP   NET,ACCEPT                                                       
         ZAP   NET,NET             IF NET ZERO, GET UNBILLED AMOUNT             
         BZ    SUM160               AND LEAVE BILLED AS 'N'                     
         MVI   BILLED,C'P'         INDICATE PARTIAL BILLED                      
         CP    NET,TRNSAMNT-TRANSD(L'TRNSAMNT,R5)                               
         BE    SUM120              IF EQUAL, INDICATE FULLY BILLED              
SUM160   ZAP   UNBILLED,TRNSAMNT-TRANSD(L'TRNSAMNT,R5)                          
         SP    UNBILLED,NET        IF NOT, GET UNBILLED AMOUNT                  
         B     EXIT                 AND LEAVE AS BILLED AS SET                  
*                                                                               
SUM180   MVC   BILLNO,=CL6'NONE'                                                
         ZAP   NET,TRNSAMNT-TRANSD(L'TRNSAMNT,R5)                               
         ZAP   TOTAL,NET                                                        
         GOTO1 DATCON,DMCB,(2,ACDTUSED),(1,WORK)                                
         MVC   BILLDATE,WORK                                                    
         MVI   BILLED,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*           THIS ROUTINE WILL READ AND PRINT X'4B' ELEMENTS           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRBDETD,R3                                                       
         SPACE 1                                                                
PARTIAL  BC    0,PAR000                                                         
         MVI   PARTIAL+1,X'F0'                                                  
         L     R3,ADTRANS                                                       
         ST    R3,SAVER3                                                        
PAR000   L     R3,SAVER3                                                        
PAR020   MVI   ELCODE,X'4B'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PAR100                                                           
         CLC   TRBDNO,SPACES       BILL NUMBER BLANK OR ZERO,                   
         BNH   PAR020               MEANS NOT BILLED.                           
         ST    R3,SAVER3           SAVE ELEMENT ADDRESS                         
         GOTO1 DATCON,DMCB,(2,TRBDDTE),(1,WORK)                                 
         CLI   QOPT1,C'B'                                                       
         BNE   PAR025                                                           
         CLC   WORK(2),START         IF NOT WITHIN RANGE, DON'T PROCESS         
         BL    PAR020               ANY FURTHER                                 
         CLC   WORK(2),END                                                      
         BH    PAR020                                                           
PAR025   MVC   BILLNO,TRBDNO                                                    
         MVC   BILLDATE,WORK                                                    
         ICM   RF,15,TRBDAMNT                                                   
         CVD   RF,CONVERT          BILLED AMOUNT                                
         ZAP   NET,CONVERT         SAVE AS NET AMOUNT                           
         ZAP   TOTAL,CONVERT       SAVE AS COMMISSIONABLE AMOUNT                
         SPACE 1                                                                
         ICM   RF,15,TRBDCSD                                                    
         CVD   RF,CONVERT          CASH DISCOUNT                                
         AP    TOTAL,CONVERT       ADD TO COMMISSIONABLE AMOUNT                 
         SPACE 1                                                                
         TM    TRBDCMST,X'80'      IS COMMISSION RATE 4 DECIMALS ?              
         BO    PAR040              YES, GET PACKED VALUE                        
         SR    RF,RF               NO, CLEAR REGISTER                           
         ICM   RF,12,TRBDCMR        VALUE IS BINARY                             
         CVD   RF,CONVERT          MAKE IT DECIMAL                              
         MP    TOTAL,CONVERT       GET THE COMMISSION AMOUNT                    
         SRP   TOTAL,64-4,5        MAKE IT 2 DECIMAL PLACES                     
         B     *+16                                                             
PAR040   MP    TOTAL,TRBDCMP       GET COMMISSION AMOUNT                        
         SRP   TOTAL,64-6,5        MAKE IT 4 DECIMAL PLACES                     
         ZAP   COMM,TOTAL          SAVE IT AS TOTAL COMMISSION                  
         ZAP   AMOUNTS(8),NET                                                   
         ZAP   AMOUNTS+(8),=P'0'                                                
         CLI   QOPT2,C' '          AM I PRINTING DETAIL ?                       
         BE    PAR060              YES, GET REFERNECE NUMBER                    
         CLI   QOPT3,C'G'          SHOW DOLLARS AS GROSS ?                      
         BNE   ACP740              NO, USE NET                                  
         B     PAR080              YES, GET COMMISSION                          
*                                                                               
         USING TRANSD,R2                                                        
*                                                                               
PAR060   L     R2,ADTRANS                                                       
         MVC   PREF,TRNSREF        FILL IN REST OF LINE                         
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,PDATE)                               
*                                                                               
PAR080   ZAP   AMOUNTS+8(8),COMM                                                
         B     ACP740                                                           
*                                                                               
PAR100   MVI   OPTSW,C'N'          RESET SWITCH WHEN DONE                       
         MVI   PARTIAL+1,X'00'                                                  
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*              ADD TO COUNTERS, MEDIA TABLE AND BUFFALO                         
         SPACE 2                                                                
ADDEM    NTR1                                                                   
         LA    RF,5                                                             
         LA    RE,STOTS                                                         
ADDEM2   LA    R1,3                                                             
         LA    R4,AMOUNTS                                                       
ADDEM4   AP    0(8,RE),0(8,R4)                                                  
         LA    RE,8(RE)                                                         
         LA    R4,8(R4)                                                         
         BCT   R1,ADDEM4                                                        
         BCT   RF,ADDEM2                                                        
         SPACE 1                                                                
         LA    RE,MEDTAB           ADD TO MEDIA TABLE                           
         L     RF,ADACC                                                         
ADDEM6   CLI   0(RE),X'FF'                                                      
         BE    ADDEM10                                                          
*&&US*&& CLC   0(1,RE),9(RF)       MEDIA IN TABLE VS JOB MEDIA                  
*&&UK*&& CLC   0(1,RE),8(RF)                                                    
         BE    ADDEM8                                                           
         LA    RE,L'MEDTAB(RE)                                                  
         B     ADDEM6                                                           
ADDEM8   LA    R1,AMOUNTS                                                       
         AP    16(8,RE),0(8,R1)                                                 
         AP    24(8,RE),8(8,R1)                                                 
         AP    32(8,RE),16(8,R1)                                                
         SPACE 1                                                                
ADDEM10  TM    STATUS,BUFFIT       AM I USING BUFFALO                           
         BZ    EXIT                                                             
         MVC   BUFFCURR(24),AMOUNTS ADD TO BUFFALO TABLE                        
         OC    BUFFKEY,SPACES                                                   
         OC    BUFFNAME,SPACES                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFFREC                              
         B     EXIT                                                             
         EJECT ,                                                                
*              PRINT HIGH LEVELS FROM BUFFALO                                   
         SPACE 2                                                                
*                                  R2=LEVEL                                     
*                                  0(R3)=HIGH LEVEL ACCOUNT                     
*                                  3(R3)=HIGH LEVEL ACCOUNT NAME                
GETBUFF  NTR1                                                                   
         XC    BUFFREC,BUFFREC                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFFREC,(R2)                        
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                NOTHING TO PRINT                             
         MVC   P+1(L'LEVNAME),LEVNAME                                           
         MVC   P+10(6),0(R3)                                                    
         MVC   P+17(36),3(R3)                                                   
         GOTO1 VUNDER,DMCB,(53,P+1),PSECOND+1                                   
         MVI   SPACING,2                                                        
         BAS   RE,MYREPORT                                                      
         B     GETBUFF4                                                         
GETBUFF2 GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFFREC,(R2)                         
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                PRINT TOTALS                                 
GETBUFF4 LA    RF,BUFFKEY                                                       
         CLI   MYRPTNM,C'S'        IF SUPPLIER LIST                             
         BNE   *+8                                                              
         LA    RF,1(RF)            SKIP COMPANY CODE                            
         MVC   P+1(L'BUFFKEY-1),0(RF)                                           
         MVC   P+16(L'BUFFNAME),BUFFNAME                                        
         GOTO1 ADSQUASH,DMCB,P+1,52                                             
         LA    R4,BUFFCURR                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,MYREPORT                                                      
         B     GETBUFF2                                                         
         EJECT ,                                                                
*              GET RECORDS FROM SORTER                                          
         SPACE 2                                                                
GETSORT  NTR1                                                                   
         CLI   SORTACTV,C'Y'       IS THERE ANYTHING TO GET                     
         BE    GETS01              YES                                          
         CLI   BINACTV,C'Y'        NO, IS THERE BINSRCH DATA ?                  
         BNE   GETS42              NO                                           
*                                                                               
GETS01   MVI   FIRST,C'Y'          INITIALIZE GETKEY SWITCHES                   
         TM    STATUS,PRODSORT     DO I NEED TO PRINT PRODUCT                   
         BZ    GETS02                                                           
         CLI   SKIPSW,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,MYREPORT                                                      
         MVC   P+1(L'LEVBACNT),LEVBACNT                                         
         MVC   P+2+L'LEVBACNT(36),LEVBNAME                                      
         GOTO1 VUNDER,DMCB,(44,P+1),PSECOND+1                                   
         MVI   SPACING,2                                                        
         BAS   RE,MYREPORT                                                      
*                                                                               
GETS02   XC    LASTKEY,LASTKEY                                                  
         MVC   STOTS(24),=3PL8'0'                                               
         MVC   WTOTS(24),=3PL8'0'                                               
         MVI   SKIPSW,C'N'                                                      
         ZAP   TCOUNT,=P'0'        USE FOR OPT2=3,4,5                           
         ZAP   SCOUNT,=P'0'        FOR OPT2=2                                   
         ZIC   R1,KEYDISP                                                       
         LA    R5,LASTKEY                                                       
         LA    R5,0(R1,R5)         R5=A(LAST SORT KEY)                          
         ZIC   R1,MINDISP                                                       
         LA    R3,LASTKEY                                                       
         LA    R3,0(R1,R3)         R3=A(LAST MINOR KEY)                         
*                                                                               
         USING BIND,R1                                                          
*                                                                               
         L     R1,AJOBTAB                                                       
         SR    R6,R6                                                            
         ICM   R6,15,BININ                                                      
         ST    R6,SAVER6                                                        
         LA    R1,BINTABLE                                                      
         ST    R1,SAVER1                                                        
*                                                                               
         DROP  R1                                                               
*                                                                               
GETS04   CLI   BINACTV,C'Y'                                                     
         BNE   GETS08                                                           
         BAS   RE,BINGET                                                        
         B     GETS10                                                           
*                                                                               
GETS08   GOTO1 ADSORTER,DMCB,=C'GET'                                            
*                                                                               
GETS10   L     R8,DMCB+4                                                        
         LTR   R8,R8                                                            
         BNZ   *+8                                                              
         LA    R8,SORTREC                                                       
         ZIC   R1,KEYDISP                                                       
         LR    R6,R8                                                            
         LA    R6,0(R1,R6)         R6=A(SORT KEY)                               
         ZIC   R1,MINDISP                                                       
         LR    R2,R8                                                            
         LA    R2,0(R1,R2)         R2=A(MINOR KEY)                              
         CLC   DMCB+4(4),=F'0'                                                  
         BNE   GETS12                                                           
         MVC   SORTREC,SPACES      LAST TIME                                    
         B     GETS14                                                           
*                                                                               
GETS12   OC    LASTKEY,LASTKEY     FIRST TIME                                   
         BZ    GETS26                                                           
*                                                                               
GETS14   ZIC   R1,KEYLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R6)       MAJOR CONTROL BREAK                          
         BE    GETS30                                                           
         AP    SCOUNT,=P'1'                                                     
         TM    STATUS,WORKSUP                                                   
         BZ    *+12                                                             
         TM    LASTSTAT,X'40'      SUPPRESS W/C DETAIL                          
         BO    GETS16                                                           
         TM    STATUS,SUPPLIER+WORKCODE PRINT LAST MINOR FIRST                  
         BNM   GETS32                   IF NOT MAJOR SORT ONLY                  
*                                                                               
GETS16   ZIC   R1,MINLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       DON'T WANT MINOR BREAK AGAIN                 
         LA    R4,STOTS                                                         
         TM    STATUS,WORKSUP                                                   
         BZ    *+12                                                             
         TM    LASTSTAT,X'40'      SUPPRESS W/C DETAIL                          
         BO    *+12                                                             
         TM    STATUS,SUPPLIER+WORKCODE MAJOR SORT ONLY                         
         BNM   GETS18                                                           
         BAS   RE,FORMAT           TOTALS ON SAME LINE AS NAME                  
         BAS   RE,MYREPORT                                                      
         MVI   SKIPSW,C'Y'                                                      
         TM    STATUS,WORKSUP                                                   
         BO    GETS20                                                           
         B     GETS26                                                           
*                                                                               
GETS18   CP    TCOUNT,=P'1'        DO I NEED TOTAL LINE                         
         BH    GETS22                                                           
*                                                                               
GETS20   ZAP   TOTQPCT,=P'0'       CLEAR PCT ACCUMS                             
         ZAP   TOTYPCT,=P'0'                                                    
         ZAP   QTOT,=P'0'                                                       
         ZAP   YTOT,=P'0'                                                       
         B     GETS26                                                           
*                                                                               
GETS22   ST    R2,DOUBLE           TOTALS ON NEW LINE                           
         ST    R3,DOUBLE+4                                                      
         LA    R2,SPACES                                                        
         XR    R3,R3                                                            
         MVC   LEVEL,=C'SUPPLIER'                                               
         TM    STATUS,WORKSUP                                                   
         BZ    GETS24                                                           
         MVC   LEVEL,=C'W-CODE  '                                               
         LA    R2,LASTWC                                                        
         LA    R3,1                                                             
*                                                                               
GETS24   BAS   RE,TOTALS                                                        
         L     R2,DOUBLE                                                        
         L     R3,DOUBLE+4                                                      
*                                                                               
GETS26   CLC   SORTWC(L'SORTREC),SPACES                                         
         BE    GETS42              LAST TIME                                    
         MVC   STOTS(24),=3PL8'0'                                               
         TM    STATUS,SUPPLIER+WORKCODE MAJOR SORT ONLY                         
         BM    GETS28                                                           
         ZAP   TCOUNT,=P'0'                                                     
         CLI   SKIPSW,C'Y'         DO I NEED TO SKIP A LINE                     
         BNE   *+8                                                              
         BAS   RE,MYREPORT                                                      
*                                                                               
GETS28   ZIC   R1,KEYLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),0(R6)                                                     
*                                                                               
         ZIC   R1,NAMEDIS          ADDRESS KEY NAME                             
         LA    RF,0(R1,R6)                                                      
         ZIC   R1,NAMELEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+16(0),0(RF)                                                    
         GOTO1 ADSQUASH,DMCB,P+1,52                                             
         TM    STATUS,SUPPLIER+WORKCODE IF NOT A MAJOR ONLY SORT                
         BM    GETS40                                                           
         MVI   MAJOR,C'Y'                                                       
         TM    STATUS,WORKSUP                                                   
         BZ    *+12                                                             
         TM    SORTSTAT,X'40'      SUPPRESS W/C DETAIL                          
         BO    GETS40                                                           
         BAS   RE,MYREPORT         THEN PRINT IT NOW                            
         B     GETS38                                                           
*                                                                               
GETS30   TM    STATUS,SUPPLIER+WORKCODE IF NOT MAJOR ONLY SORT THEN             
         BM    GETS40                                                           
         TM    STATUS,WORKSUP                                                   
         BZ    *+12                                                             
         TM    SORTSTAT,X'40'      SUPPRESS W/C DETAIL                          
         BO    GETS40                                                           
         ZIC   R1,MINLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R2)       CHECK FOR MINOR CONTROL BREAK                
         BE    GETS38                                                           
         MVI   MINOR,C'Y'          GOING TO FORMAT WITH SAME SORT KEY           
*                                                                               
GETS32   ZIC   R1,MINLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+4(0),0(R3)                                                     
*                                                                               
         ZIC   R1,MNAMDIS          ADDRESS MINOR KEY NAME                       
         LA    RF,0(R1,R3)                                                      
         ZIC   R1,MNAMLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+19(0),0(RF)                                                    
         GOTO1 ADSQUASH,DMCB,P+4,52                                             
         LA    R4,WTOTS                                                         
         CLI   PROGPROF,C'Y'       PROFILE TO SUPPRESS ZERO AMOUNTS             
         BNE   GETS34                                                           
         CLC   0(24,R4),=3PL8'0'                                                
         BNE   GETS34                                                           
         MVC   P,SPACES                                                         
         B     GETS36                                                           
*                                                                               
GETS34   BAS   RE,FORMAT                                                        
         BAS   RE,MYREPORT                                                      
         AP    TCOUNT,=P'1'                                                     
         MVI   SKIPSW,C'Y'                                                      
         MVC   WTOTS(24),=3PL8'0'                                               
*                                                                               
GETS36   ZIC   R1,KEYLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R6)       GO BACK IF MAJOR CONTROL BREAK               
         BNE   GETS16                                                           
*                                                                               
GETS38   DS    0H                  ADD NEW RECORDS TO ACCUMS                    
         AP    WTOTS(8),SORTCURR   MINOR TOTALS                                 
         AP    WTOTS+8(8),SORTQTD                                               
         AP    WTOTS+16(8),SORTYTD                                              
*                                                                               
GETS40   AP    STOTS(8),SORTCURR   MAJOR TOTALS                                 
         AP    STOTS+8(8),SORTQTD                                               
         AP    STOTS+16(8),SORTYTD                                              
         MVC   LASTKEY,SORTWC                                                   
         B     GETS04              GET NEXT RECORD                              
*                                                                               
GETS42   CLI   BINACTV,C'Y'                                                     
         BNE   GETSRTX                                                          
         MVI   BINACTV,C'N'                                                     
         B     EXIT                                                             
*                                                                               
GETSRTX  GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     EXIT                                                             
         SPACE 3                                                                
SORTINIT NTR1                                                                   
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,(40,ASORTA)                       
         MVI   SORTACTV,C'N'                                                    
         MVI   BINACTV,C'N'                                                     
         XC    SORTREC,SORTREC                                                  
         B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET REVERSAL STATUS BASED ON QOPT1                                     
*---------------------------------------------------------------------          
*                                                                               
SETREV   NTR1                                                                   
         CLI   QOPT1,C'B'          BILLED ITEMS RUN                             
         BNE   SR50                                                             
*                                                                               
         MVI   FCREVOVR,FCREVTRY+FCREVTRN   TURN OFF REVERSAL FILTERING         
         B     SRX                                                              
*                                                                               
         USING ACMD,RF                                                          
*                                                                               
SR50     L     RF,AMONACC          SET MONACCS MOS FILTERS                      
         MVC   ACMMSTR,START                                                    
         MVC   ACMMEND,END                                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
SRX      B     EXIT                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        FILTER TRANSACTIONS BY MOS                                             
*        NOTE: MONACC WON'T FILTER BY MOS UNLESS ACMIMOSR IS SET                
*              WHEN QMOSEND IS TESTED FOR NON SPACES                            
*---------------------------------------------------------------------          
*                                                                               
         USING ACMD,RF                                                          
*                                                                               
FLTMOS   NTR1                                                                   
         L     RF,AMONACC          DO MONACCS MOS FILTERS                       
         CLC   ACMMDTE,ACMMSTR                                                  
         BL    FMNO                                                             
         CLC   ACMMDTE,ACMMEND                                                  
         BH    FMNO                                                             
         MVC   DATE(2),ACMMDTE     SET MOS OF TRAN                              
         MVI   DATE+2,X'00'                                                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         CR    RF,RF               SET CC EQ                                    
         B     FMX                                                              
*                                                                               
FMNO     CR    RF,RB               SET CC NEQ                                   
*                                                                               
FMX      B     EXIT                                                             
         EJECT ,                                                                
*              ADD ITEM TO BINARY TABLE                                         
*                                                                               
         USING BIND,R7                                                          
*                                                                               
BINADD   NTR1  ,                                                                
         L     R7,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    XIT                 NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R5,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,*-14                                                          
         B     XIT                                                              
*                                                                               
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R4)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,BINBIN                                                        
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
*              GET ITEMS TO BINARY TABLE                                        
*                                                                               
         USING BIND,R7                                                          
*                                                                               
BINGET   NTR1  ,                                                                
         L     R6,SAVER6                                                        
         LTR   R6,R6               ANYTHING LEFT ?                              
         BZ    BING02              NO, SEND LAST                                
         L     R1,SAVER1                                                        
         ST    R1,DMCB+4                                                        
         BCTR  R6,0                                                             
         ST    R6,SAVER6                                                        
         LA    R1,SORTLN(R1)                                                    
         ST    R1,SAVER1                                                        
         B     BINGXIT                                                          
*                                                                               
BING02   XC    BININ,BININ         CLEAR FOR NEXT TIME                          
         MVC   DMCB+4(4),=F'0'                                                  
*                                                                               
BINGXIT  XIT1                                                                   
         EJECT ,                                                                
         GETEL R3,DATADISP,ELCODE                                               
         SPACE 2                                                                
VTYPES   DS    0F                                                               
         DC    A(SORTA)                                                         
         DC    V(UNDERLIN)                                                      
         DC    V(ACLIST)                                                        
         DC    A(BUFFALOC)                                                      
         DC    A(JOBTAB)                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(95)'                                  
         SPACE 2                                                                
TOTFOR   DC    C'TOTALS FOR'                                                    
         SPACE 2                                                                
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              OPTION TABLE                                                     
*                                                                               
*              +0 = OPTION 2                                                    
*              +1 = Y IF SORTING, ELSE N                                        
*              +2 = STATUS BYTE    X'01' 1 = CLIENT LEVEL SORT                  
*                                  X'02' 1 = PRODUCT LEVEL SORT                 
*                                  X'04' 1 = JOB LEVEL SORT                     
*                                  X'08' 1 = USING BUFFALO                      
*                                  X'10' 1 = REPORT ON WORK-CODE                
*                                  X'20' 1 = REPORT ON SUPPLIER                 
*                                  X'40' 1 = REPORT ON W-C/SUPP                 
*              +3 = SPROG                                                       
*              +4 = HEAD8+1                                                     
*             +13 = REPORT NAME                                                 
*             +22 = 2 BYTES SPARE                                               
*             +24 = A(SORT INFO)                                                
*                                                                               
         DS    0F                                                               
OPTTAB   DS    0CL28                                                            
         DC    C' N',AL1(WORKSUP)                  REGULAR REPORT               
         DC    X'00',CL9'JOB',CL9'WORK-CODE',CL2' ',AL4(0)                      
         SPACE 1                                                                
         DC    C'1Y',AL1(BUFFIT+WORKCODE+JOBSORT)  ALL LEVELS, WC ONLY          
         DC    X'02',CL9'JOB',CL9'WORK-CODE',CL2' ',AL4(WCSORT)                 
         SPACE 1                                                                
         DC    C'2Y',AL1(JOBSORT+BUFFIT+SUPPLIER)  ALL LEVELS, SUP ONLY         
         DC    X'03',CL9'JOB',CL9'SUPPLIER',CL2' ',A(SUPPSORT)                  
         SPACE 1                                                                
         DC    C'3Y',AL1(JOBSORT+WORKSUP+BUFFIT)   JOB LEVEL, WC/SUPP           
         DC    X'05',CL9'JOB',CL9'WORK-CODE',CL2' ',AL4(WCSORT)                 
         SPACE 1                                                                
         DC    C'4Y',AL1(PRODSORT+WORKSUP+BUFFIT)  PROD LEVEL, WC/SUPP          
         DC    X'05',CL9'PRODUCT',CL9'WORK-CODE',CL2' ',A(WCSORT)               
         SPACE 1                                                                
         DC    C'5Y',AL1(CLISORT+WORKSUP+BUFFIT)   CLT LEVEL, WC/SUPP           
         DC    X'03',CL9'WORK-CODE',CL9'WORK-CODE',CL2' ',A(WCSORT)             
         SPACE 1                                                                
         DC    C'6Y',AL1(JOBSORT+BUFFIT)           JOB LEVEL, SUPP/WC           
         DC    X'04',CL9'JOB',CL9'SUPPLIER',CL2' ',A(SUPPSORT)                  
         SPACE 1                                                                
         DC    C'7Y',AL1(PRODSORT+BUFFIT)          PROD LEVEL, SUPP/WC          
         DC    X'04',CL9'PRODUCT',CL9'SUPPLIER',CL2' ',A(SUPPSORT)              
         SPACE 1                                                                
         DC    C'8Y',AL1(CLISORT+BUFFIT)           CLT LEVEL, SUPP/WC           
         DC    X'02',CL9'SUPPLIER',CL9'SUPPLIER',CL2' ',A(SUPPSORT)             
         DC    X'FF'                                                            
*                                                                               
*              SORT INFORMATION TABLE                                           
*                                                                               
*             +0 = SORT KEY LENGTH - 1                                          
*             +1 = DISPLACEMENT OF SORT KEY FROM BEG. OF SORT RECORD            
*             +2 = SORT NAME LENGTH - 1                                         
*             +3 = DISPLACEMENT OF SORT NAME FROM BEG. OF SORT KEY              
*                                                                               
*             +4 = MINOR KEY LENGTH - 1                                         
*             +5 = DISPLACEMENT OF MINOR KEY FROM BEG. OF SORT RECORD           
*             +6 = MINOR NAME LENGTH - 1                                        
*             +7 = DISPLACEMENT OF MINOR NAME FROM BEG. OF SORT KEY             
*                                                                               
SUPPSORT DC    AL1(13,3,35,16,1,17,14,38)                                       
WCSORT   DC    AL1(1,0,14,55,13,3,35,16)                                        
*                                                                               
CLISORT  EQU   X'01'                                                            
PRODSORT EQU   X'02'                                                            
JOBSORT  EQU   X'04'                                                            
BUFFIT   EQU   X'08'                                                            
WORKCODE EQU   X'10'                                                            
SUPPLIER EQU   X'20'                                                            
WORKSUP  EQU   X'40'                                                            
         EJECT ,                                                                
*              BOX ROUTINES (HOOK)                                              
         SPACE 2                                                                
*&&US                                                                           
         ENTRY HOOK                                                             
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R7,ADBOX                                                         
*                                                                               
         USING BOXD,R7                                                          
*                                                                               
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+6,C'T'        SET ROWS                                     
         MVI   MYROW+9,C'M'                                                     
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL,C'L'          SET LH MARGIN                                
         SPACE 1                                                                
*                                  FIND SPROG                                   
         CLI   RCSUBPRG,1                                                       
         BE    HOOK16                                                           
         BL    HOOK0                                                            
         CLI   RCSUBPRG,6                                                       
         BE    HOOK16                                                           
         CLI   RCSUBPRG,5                                                       
         BNH   HOOK2345                                                         
         B     HOOKX                                                            
         SPACE 1                                                                
HOOK0    MVI   MYCOL+97,C'C'       REGULAR REPORT                               
         MVI   MYCOL+47,C'C'                                                    
         B     HOOKALL                                                          
HOOK2345 MVI   MYCOL+88,C'C'       SORT OPTIONS                                 
         MVI   MYCOL+104,C'C'                                                   
         CLI   QOPT5,C'S'          SUPPRESS CURRENT MONTH                       
         BE    HOOKALL                                                          
         MVI   MYCOL+47,C'C'                                                    
HOOKALL  MVI   MYCOL+64,C'C'       BOTH                                         
         MVI   MYCOL+80,C'C'                                                    
         MVI   MYCOL+113,C'R'                                                   
         B     HOOKX                                                            
         SPACE 1                                                                
HOOK16   MVI   MYCOL+22,C'C'       MEDIA SUMMARY                                
         MVI   MYCOL+38,C'C'                                                    
         MVI   MYCOL+54,C'C'                                                    
         MVI   MYCOL+70,C'R'                                                    
         B     HOOKX                                                            
         SPACE 1                                                                
HOOKX    MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
SAVERC   DC    A(0)                                                             
*&&                                                                             
         EJECT ,                                                                
*              GET A WORK-CODE NAME                                             
*                                                                               
WCNAME   NMOD1 0,**WCNM**                                                       
         L     RC,0(,R1)                                                        
         XC    SVANALST,SVANALST                                                
         CLC   SVANAL,=C'**'                                                    
         BNE   WCNAME1                                                          
         MVC   SVANALNM,=CL15'ORDERS'                                           
         B     WCNAMEEX                                                         
*                                                                               
WCNAME1  MVC   SVANALNM,=CL15'BILLING' IF 12 EL NOT FOUND THEN IT               
         L     R4,ADLEDGER             MUST BE BILLING.                         
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
*                                                                               
WCNAME2  CLI   0(R4),0                                                          
         BE    WCNAMEEX            NOT FOUND                                    
         CLI   0(R4),X'12'                                                      
         BNE   WCNAME4                                                          
*                                                                               
         USING ACANALD,R4                                                       
*                                                                               
         CLC   SVANAL,ACANCODE                                                  
         BNE   WCNAME4                                                          
         MVC   SVANALNM,ACANDESC   MOVE IN NAME                                 
         MVC   SVANALST,ACANSTAT   AND STATUS BYTE                              
         B     WCNAMEEX                                                         
*                                                                               
WCNAME4  IC    R3,1(,R4)                                                        
         AR    R4,R3                                                            
         B     WCNAME2                                                          
*                                                                               
WCNAMEEX XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*              ROUTINE TO GENERATE START OF LAST QUARTER                        
*                                                                               
QTRIT    NMOD1 0,*QUART*                                                        
         L     RC,0(,R1)                                                        
         MVC   QTRST,QSTART                                                     
         CLC   QSTART,QEND                                                      
         BNL   QTRITX                                                           
         MVC   WORK(6),QSTART                                                   
*                                                                               
QTRIT1   GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'3'    ADD 3 MONTHS                 
         CLC   QEND,WORK                                                        
         BL    QTRITX                                                           
         MVC   QTRST,WORK                                                       
         B     QTRIT1                                                           
*                                                                               
QTRITX   GOTO1 DATCON,DMCB,(0,QTRST),(1,QTRST)                                  
         XMOD1                                                                  
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
         ENTRY SORTA                                                            
SORTA    DS    0D                                                               
         DS    41000C                                                           
         SPACE 3                                                                
         BUFF  LINES=300,ROWS=3,COLUMNS=3,FLAVOR=PACKED,KEYLIST=(15,A),X        
               COMMENT=36                                                       
         EJECT ,                                                                
MXJOB    EQU   1000                                                             
         DS    0D                  JOB LEVEL DATA                               
         DC    CL8'**JOBTAB*'                                                   
JOBTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SORTLN)         RECORD LENGTH                                
         DC    AL4(SORTKLN)        DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXJOB)          MAX. IN TABLE                                
         DC    AL1(SORTBKN)        NUMBER OF BUCKETS                            
         DC    AL1(SORTBK-SORTWC)  DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXJOB*SORTLN)C     TABLE                                        
         EJECT ,                                                                
PRINTD   DSECT                     DSECT FOR PRINT LINE                         
         DS    CL4                                                              
PNAME    DS    CL28                                                             
PTOTFOR  EQU   *-24                                                             
         DS    CL1                                                              
PREF     DS    CL6                                                              
         DS    CL1                                                              
PDATE    DS    CL8                                                              
         DS    CL1                                                              
PNET     DS    CL15                                                             
         DS    CL1                                                              
PCOMM    DS    CL15                                                             
         DS    CL1                                                              
PGROSS   DS    CL15                                                             
         DS    CL1                                                              
PBILLNO  DS    CL6                                                              
         DS    CL1                                                              
PBILLDTE DS    CL8                                                              
         DS    CL1                                                              
         SPACE 2                                                                
SORTD    DSECT                     DSECT FOR SORTER RECORDS                     
SORTWC   DS    CL2                                                              
SORTSUPP DS    CL15                                                             
SORTWC2  DS    CL2                                                              
SORTKLN  EQU   *-SORTD             KEY LENGTH                                   
*                                                                               
SORTSPNM DS    CL36                                                             
SORTWCNM DS    CL15                                                             
SORTSTAT DS    CL1                                                              
*                                                                               
SORTBK   DS    0PL8                                                             
SORTCURR DS    PL8                                                              
SORTQTD  DS    PL8                                                              
SORTYTD  DS    PL8                                                              
SORTBKN  EQU   (*-SORTBK)/L'SORTBK                                              
SORTLN   EQU   *-SORTD             SORT RECORD LENGTH                           
         SPACE 2                                                                
BUFFD    DSECT                     DSECT FOR BUFFALO RECORDS                    
BUFFKEY  DS    CL15                                                             
BUFFNAME DS    CL36                                                             
BUFFCURR DS    PL8                                                              
BUFFQTD  DS    PL8                                                              
BUFFYTD  DS    PL8                                                              
         EJECT ,                                                                
ACP1D    DSECT                                                                  
SAVER1   DS    F                   SAVE AREA FOR REGISTER 1                     
SAVER3   DS    F                   SAVE AREA FOR REGISTER 3                     
SAVER6   DS    F                   SAVE AREA FOR REGISTER 6                     
SAVE4B   DS    F                   SAVE AREA FOR FIRST 4B ELEMENT               
RELO     DS    F                                                                
EXTERNS  DS    0F                  ADCONS AND VTYPES                            
ASORTA   DS    A                                                                
VUNDER   DS    V                                                                
VACLIST  DS    V                                                                
ADBUFC   DS    A                                                                
AJOBTAB  DS    A                                                                
QUART    DS    A                   ADDRESS OF QTRIT                             
         SPACE 1                                                                
SORTSW   DS    CL1                 Y IF SORTING, ELSE N                         
STATUS   DS    CL1                 SEE OPTTAB FOR DETAILS                       
*                                                                               
KEYLEN   DS    CL1                           "                                  
KEYDISP  DS    CL1                           "                                  
NAMELEN  DS    CL1                           "                                  
NAMEDIS  DS    CL1                                                              
*                                                                               
MINLEN   DS    CL1                           "                                  
MINDISP  DS    CL1                           "                                  
MNAMLEN  DS    CL1                           "                                  
MNAMDIS  DS    CL1                                                              
*                                                                               
FIRST    DS    CL1                 FIRST TIME SWITCH FOR GETKEY                 
MAJOR    DS    CL1                 IS THIS FIRST TIME FOR MAJOR KEY             
MINOR    DS    CL1                 ARE WE STILL IN SAME RECORD                  
ELCODE   DS    CL1                                                              
PL13     DS    PL13                                                             
         SPACE 1                                                                
AMOUNTS  DS    3PL8                ACCUMULATORS FOR TRANSACTIONS                
STOTS    DS    3PL8                                 SUPPLIER                    
WTOTS    DS    3PL8                                 WORK-CODE                   
JTOTS    DS    3PL8                                 JOB                         
PTOTS    DS    3PL8                                 PRODUCT                     
CTOTS    DS    3PL8                                 CLIENT                      
         SPACE 1                                                                
         DS    0D                  MUST BE ALIGNED ON DOUBLEWORD                
QPCT     DS    PL8                                  QTD PCT                     
YPCT     DS    PL8                                  YTD PCT                     
TOTQPCT  DS    PL8                                  TOTAL QTD PCT               
TOTYPCT  DS    PL8                                  TOTAL YTD PCT               
QTOT     DS    PL8                                  RUNNING QTR TOTAL           
YTOT     DS    PL8                                  RUNNING YEAR TOTAL          
         SPACE 1                                                                
COUNTERS DS    0PL3                COUNTERS FOR ALL LOWER LEVELS                
TCOUNT   DS    PL3                                                              
SCOUNT   DS    PL3                                                              
WCOUNT   DS    PL3                                                              
JCOUNT   DS    PL3                                                              
         SPACE 1                                                                
PERIOD   DS    CL13                REQUEST PERIOD                               
SVANAL   DS    CL2                 CURRENT WORK-CODE                            
SVANALNM DS    CL15                        AND NAME                             
SVANALST DS    CL1                         AND STATUS                           
         SPACE 1                                                                
JOBSW    DS    CL1                 SWITCHES TO DETERMINE IF I'VE                
ANALSW   DS    CL1                 PRINTED NAME YET                             
SUPPSW   DS    CL1                                                              
OPTSW    DS    CL1                 SWITCH IF OPT1 = BLANK                       
         SPACE 1                                                                
START    DS    PL3                 PACKED START DATE                            
END      DS    PL3                        END DATE                              
DATE     DS    PL3                        TRANS. DATE                           
QTRST    DS    CL6                 START OF QUARTER                             
         SPACE 1                                                                
ADBOX    DS    F                   BOXES                                        
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
         SPACE 1                                                                
SKIPSW   DS    CL1                 DO I NEED TO SKIP LINE BEFORE NAME           
CLIACTV  DS    CL1                 IS CLIENT ACTIVE                             
BILLED   DS    CL1                 IS TRANSACTION BILLED                        
BILLDATE DS    PL3                 BILL DATE                                    
BILLNO   DS    CL6                 BILL NUMBER                                  
         SPACE 1                                                                
MEDTAB   DS    0CL40               MEDIA TABLE                                  
         DS    CL(40*40)                                                        
MEDTABN  EQU   (*-MEDTAB)/L'MEDTAB                                              
         DS    CL1                 SPACE FOR ONE MORE FF IN MEDTAB              
*                                                                               
BUFFREC  DS    CL75                BUFFALO RECORD                               
SORTREC  DS    CL95                RECORDS TO SORTER                            
SORTACTV DS    CL1                 SORTER ACTIVITY SWITCH                       
BINACTV  DS    CL1                 BINSRCH ACTIVITY SWITCH                      
         SPACE 1                                                                
LEVEL    DS    CL8                 LEVEL NAME FOR TOTALS ROUTINE                
MYRPTNM  DS    CL9                 REPORT NAME FOR HEAD1                        
MYHEAD8  DS    CL9                 NAME FOR HEAD8                               
         SPACE 1                                                                
LEVNAME  DS    CL7                 NAME OF CURRENT LEVEL FOR GETBUFF            
ACCOUNT  DS    CL12                CURRENT PRODUCT AND JOB CODE                 
LEVAACNT DS    CL6                 CURRENT CLIENT CODE AND                      
LEVANAME DS    CL36                               NAME                          
LEVBACNT DS    CL6                 CURRENT PRODUCT CODE AND                     
LEVBNAME DS    CL36                                NAME                         
         SPACE 1                                                                
LASTKEY  DS    0CL72               LAST KEY FROM SORTER                         
LASTWC   DS    CL2                                                              
LASTSUPP DS    CL15                                                             
LASTWC2  DS    CL2                                                              
LASTSPNM DS    CL36                                                             
LASTWCNM DS    CL15                                                             
LASTSTAT DS    CL1                                                              
         SPACE 1                                                                
         DS    0D                                                               
NET      DS    PL8                 = TRNSAMNT IF FULLY BILLED                   
*                                  = TRBDAMNT IF PARTIALLY BILLED               
UNBILLED DS    PL8                 = 0        IF FULLY BILLED                   
*                                  = TRNSAMNT - TOTAL TRBDAMNT                  
*                                    IF PARTIALLY BILLED                        
CASH     DS    PL8                 = CASH DISCOUNT                              
*                                                                               
COMM     DS    PL8                 = TOTAL COMMISSION                           
*                                                                               
CONVERT  DS    PL8                 = USED FOR BINARY CONVERSIONA                
*                                                                               
ACCEPT   DS    PL8                 = TOTAL BILLING ACCEPTED                     
*                                                                               
TOTAL    DS    PL13                = ACCUMULATOR                                
         EJECT ,                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER OF ENTRIES                            
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT OF KEY                          
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER OF ENTRIES                    
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINSTAT  DS    XL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                BINSRCH TABLE                                
         EJECT ,                                                                
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT ,                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100ACREPP102 05/14/09'                                      
         END                                                                    
