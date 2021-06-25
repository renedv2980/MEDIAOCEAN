*          DATA SET ACREPTM02  AT LEVEL 021 AS OF 12/11/09                      
*PHASE ACTM02A,+0                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'TIMESHEET CONVERSION'                                           
***********************************************************************         
*        QOPT1 = * = FIRST TIME CONVERSION REPORT                     *         
*                E = PRINT ONLY THE ERROR REPORT                      *         
*                S = SERIOUS ERRORS                                   *         
*        QOPT2 = T = TRACE CHANGES                                    *         
*        QOPT3 = W = REPORT BY WEEK (DEFAULT BY PERIOD)               *         
*        QOPT4 = C = FILTER CONTRA OF SJ BY 1R ACCOUNTS               *         
*                P = ONLY PROCESS PERSON LEDGER                       *         
*                B = ONLY PROCESS PRODUCTION LEDGER                   *         
*        QOPT5 = I = IGNORE HOURS X RATE = AMOUNT CHECK *                       
*        QOPT6 = D = DELETE OLD CLUSTERS *                                      
*                N = FIX OLD STYLE 1N CONTRAS (1ND VAC  TO 1NVAC)     *         
*        QOPT7 = F = WRITE TMS RECORDS DIRECTLY TO FILE                         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* 1. BILLABLE, REAL TIME AND TAX DETAILS COME FROM SJ.                *         
*                                                                     *         
*       JOBD  - READ TRANSACTION AND TAX DETAIL. BUILD A TMS          *         
*               RECORD WITH KEY FOR TAX SORT AND WRITE TO SORTER.     *         
*       BTIM  - GET SORT RECORDS MERGE TAX INFO WITH BILLABLE         *         
*               TIME ITEMS. WRITE MERGED TMS RECORDS TO TMSWRK FILE.  *         
*                                                                     *         
* 2. NON-BILLABLE AND NON-CLIENT FROM PERSON (1R) RECORDS.            *         
*       PTIM  - READ ALL 1R TRANSACTIONS. KEEP TRACK OF BILLABLE      *         
*               HOURS AND COST IN BUFFALO TO COMPARE WITH TOTALS      *         
*               FROM SJ.                                              *         
*               GENERATE TMS RECORDS FOR NON-BILLABLE, WRITE          *         
*               TO TMSWRK FILE.                                       *         
*                                                                     *         
* 3. MERGE BILLABLE(SJ) WITH NON-BILLABLE(1R).                        *         
*       REQL  - GET ALL RECORDS FROM TMSWRK AND WRITE TO SORTER.      *         
*               THE SORT KEY IS ACCOUNT FILE KEY - TIMKEY.            *         
*       MRGR  - GET RECORDS FROM SORT MERGE DETAILS FOR RECORDS       *         
*               WITH EQUAL KEYS.                                      *         
*               ADDC - ADDS CLUSTER TO TABLE OF CLUSTERS FOR THIS KEY.*         
*                                                                     *         
*       PUTR  - WRITE THE FINALLY MERGED RECORDS TO OUT FILE.         *         
***********************************************************************         
         EJECT                                                                  
ACTM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTM**,R9,R8    BASE REGISTERS 11, 9 AND 8                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACTMD,RC            RC=A(SAVE W/S)                               
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,PROCACC        ACCOUNT                                      
         BE    PRAC                                                             
         CLI   MODE,REQLAST        REQUEST LAST - PRINT SUMMARY                 
         BE    REQL                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         GOTO1 DATCON,DMCB,(5,TODAYP),(1,TODAYP)                                
*                                                                               
         L     RF,ATMSWRK          SET TRANSACTION DCB                          
         USING IHADCB,RF                                                        
         LA    R1,TMSRLNQ          RECORD LENGTH                                
         STCM  R1,3,DCBLRECL                                                    
         MH    R1,=H'10'                                                        
         STCM  R1,3,DCBBLKSI       AND BLOCKSIZE                                
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         B     XIT                                                              
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     ZAP   PITEM,=P'0'                                                      
         ZAP   PHOUR,=P'0'                                                      
         ZAP   PAMNT,=P'0'                                                      
         ZAP   BUFEIN,=P'0'                                                     
         MVI   ELSW,C'N'                                                        
*                                                                               
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'132'    SET WIDTH FOR PDUMPS                         
*                                                                               
         MVI   ACRQLEN,0                                                        
         CLI   QOPT4,C'C'          FILTER CONTRA(ON SJ)                         
         BNE   REQF3                                                            
         LA    RF,QACCOUNT+L'QACCOUNT-1                                         
         LA    R0,L'QACCOUNT       GET LENGTH OF INPUT ACCOUNT                  
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         AH    R0,=H'1'            PLUS UNIT AND LEDGER                         
         STC   R0,ACRQLEN                                                       
*                                                                               
REQF3    L     R4,ADCMPNAM                                                      
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
*                                                                               
         XC    STDATE,STDATE       INITIALIZE START/END DATES                   
         MVC   ENDDATE,EFFS                                                     
         CLC   QSTART,SPACES                                                    
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STDATE)                                
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDDATE)                                 
*                                                                               
         USING CPYELD,R4                                                        
REQF20   MVI   STATUS,0                                                         
         L     R4,ADCMPEL                                                       
         TM    CPYSTAT1,CPYSCOST   TEST IF COMPANY ON COST                      
         BZ    *+8                                                              
         OI    STATUS,NCOST                                                     
         TM    CPYSTAT4,CPYSOFF2   NEW OFFICES                                  
         BZ    *+8                                                              
         OI    STATUS,NOFFC                                                     
         TM    CPYSTAT3,CPYSPC1C   PC=1C                                        
         BNO   *+8                                                              
         OI    PCSW,TIMSPCC                                                     
         TM    CPYSTAT3,CPYSPCSJ   PC=SJ                                        
         BNO   *+8                                                              
         OI    PCSW,TIMSPCJ                                                     
         BAS   RE,CLNDR            READ CALENDAR RECORD/BUILD TABLE             
*                                                                               
         L     RE,AEMPTAB                                                       
         MVI   0(RE),X'FF'         MARK END OF TABLE                            
         ST    RE,AEMPCURR         CURRENT ENTRY                                
         AH    RE,=Y((EMPMAX-1)*EMPDLNQ)                                        
         ST    RE,AEMPLAST         MAX LAST ENTRY                               
*                                                                               
         L     R2,ATMSWRK                                                       
         OPEN  ((R2),(OUTPUT))     OPEN WORK FILE                               
         L     R2,ATAPEOUT         OPEN OUTPUT TAPE                             
         OPEN  ((R2),(OUTPUT))                                                  
         CLI   QOPT4,C'P'          PERSON LEDGER ONLY                           
         BE    XIT                                                              
*                                                                               
         BAS   RE,JOBD             READ JOB DETAIL AND PUT TO SORT              
         BAS   RE,BTIM             MERGE BILLABLE TIME AND TAX ITEMS            
*                                                                               
         CLI   QOPT4,C'B'          PRODUCTION ONLY                              
         BNE   XIT                                                              
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ACLELD,R4                                                        
LDGF     L     R4,ADLDGHIR         HEIRARCHY ELEMENT                            
         MVC   LEVELA,ACLVLEN      LEVEL LENGTHS                                
         MVC   LEVELB,ACLVLEN+(L'ACLVALS)                                       
         MVC   LEVELC,ACLVLEN+(L'ACLVALS*2)                                     
         MVC   LEVELD,ACLVLEN+(L'ACLVALS*3)                                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRAC     L     R1,ADHEIRC                                                       
         MVC   ODS,3(R1)           OFFICE/DEPT/SUB                              
*                                                                               
         L     RE,AEMPCURR         NEXT AVAILABLE SLOT                          
         USING EMPD,RE                                                          
         L     R1,ADACC                                                         
         MVC   EMPCODE,3(R1)       SAVE 1R CODE                                 
         MVI   EMPSTA,0            CLEAR STATUS                                 
         LA    RE,EMPDLNQ(RE)                                                   
         C     RE,AEMPLAST         CHECK SIZE                                   
         BNH   *+6                                                              
         DC    H'0'                TOO BIG                                      
         MVI   EMPCODE,X'FF'       MARK END                                     
         ST    RE,AEMPCURR                                                      
*                                                                               
         MVC   PERCDE,SPACES       PERSON CODE                                  
         SR    RF,RF                                                            
         IC    RF,LEVELD           ISOLATE PERSON CODE                          
         SR    RE,RE                                                            
         IC    RE,LEVELC                                                        
         SR    RF,RE               RF = EX MOVE COUNT                           
         L     R1,ADACC                                                         
         MVC   PERACC,0(R1)        SAVE PERSON ACCOUNT                          
         LA    R1,3(RE,R1)                                                      
         SH    RF,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+4                                                           
         MVC   PERCDE(0),0(R1)                                                  
*                                                                               
         XC    PERNME,PERNME       GET PERSON NAME                              
         L     RF,ADACCNAM                                                      
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   PERNME(0),NAMEREC                                                
*                                                                               
         BAS   RE,PTIM             GET PERSON TRANSACTIONS                      
         B     XIT                                                              
         DROP  RE,RF                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
         SPACE 1                                                                
REQL     L     R2,ATMSWRK                                                       
         CLOSE ((R2))                                                           
         LA    R1,TMSRLNQ           RECORD LENGTH FOR TIME DETAILS              
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         LA    R1,TMSKEY-TMSD+1      DISP. TO KEY FOR TIME RECORD               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(3),DUB+6(2)                                          
         LA    R1,TMSKLNQ           LENGTH OF KEY FOR TIME RECORD               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+17(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
*                                                                               
         L     R2,ATMSWRK          GET NEW TIME RECORDS                         
         OPEN  ((R2),(INPUT))                                                   
         L     R5,ASRTIO           PUT JOB DETAIL TO SORT                       
*                                                                               
REQL3    GET   (R2),(R5)                                                        
         GOTO1 ADSORTER,DMCB,=C'PUT',(R5)                                       
         L     R2,ATMSWRK          CLOSE FOR JOB DETAIL                         
         B     REQL3                                                            
*                                                                               
REQL5    L     R2,ATMSWRK          CLOSE FOR JOB DETAIL                         
         CLOSE ((R2))                                                           
*        L     R2,ATAPEOUT         OPEN OUTPUT TAPE                             
*        OPEN  ((R2),(OUTPUT))                                                  
         BAS   RE,MRGR             MERGE RECORDS FROM PERSON/JOB                
         BAS   RE,RPT              PRINT CONVERSION REPORT                      
         BAS   RE,SUM              SUMMARY REPORT                               
         L     R2,ATAPEOUT         CLOSE OUTPUT FILE                            
         CLOSE ((R2))                                                           
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',1)                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ JOB DETAILS AND CREATE SORT RECORDS FOR TIME AND TAX ITEMS     *         
***********************************************************************         
         SPACE 1                                                                
         USING TMSD,R5                                                          
JOBD     NTR1  ,                                                                
         LA    R1,TMSRLNQ           RECORD LENGTH FOR TIME DETAILS              
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         LA    R1,TMSXK-TMSD+1      DISP. TO KEY FOR TAX SORT                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(3),DUB+6(2)                                          
         LA    R1,TMSXKLNQ          LENGTH OF KEY FOR TAX SORT                  
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+17(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
*                                                                               
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL    COMPANY                                      
         MVC   TRNKUNT(2),PRODUL   'SJ'                                         
         BAS   RE,HIGH             READ LEDGER                                  
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                NO LEDGER RECORD                             
*                                                                               
JOBD3    LA    R2,DIR                                                           
         BAS   RE,SEQ                                                           
         CLC   TRNKUNT(2),PRODUL   FINISHED SJ                                  
         BNE   XIT                                                              
         CLC   TRNKWORK,=C'**'     TEST ORDERS                                  
         BE    JOBD3                                                            
         CLC   TRNKWORK(TRNKSTA-TRNKWORK),SPACES TEST ACCOUNT LEVEL             
         BNE   *+12                                                             
         BAS   RE,GETOFC           GET OFFICE CLI/PRD OFFICE                    
         B     JOBD3                                                            
*                                                                               
         CLC   CHDKSPCS-CHDRECD(L'CHDKSPCS,R2),SPACES                           
         BNE   JOBD7                                                            
         OC    CHDKNULL-CHDRECD(L'CHDKNULL,R2),CHDKNULL-CHDRECD(R2)             
         BNZ   JOBD7                                                            
         XC    CNTRNME,CNTRNME     CLEAR CONTRA NAME                            
         MVC   CNDA,TRNKDA         SAVE DA OF CONTRA                            
         B     JOBD3                                                            
*                                                                               
JOBD7    CLI   TRNKREF,C' '        TEST TRANSACTION                             
         BNH   JOBD3                                                            
         TM    TRNKSTAT,TRNSDRFT   SKIP DRAFT TRANSACTIONS                      
         BO    JOBD3                                                            
         TM    TRNKSTAT,TRNSDELT   SKIP DELETED TRANSACTIONS                    
         BO    JOBD3                                                            
         CLC   TRNKDATE,STDATE     TEST TRANSACTION DATE                        
         BL    JOBD3                                                            
         CLC   TRNKDATE,ENDDATE                                                 
         BH    JOBD3                                                            
         CLC   TRNKCUNT(2),PERSUL  CONTRA OF 1R                                 
         BNE   JOBD3                                                            
         CLI   TRNKSTYP,49         BILLABLE TIME                                
         BE    JOBD13                                                           
         CLI   TRNKSTYP,41         REALIZATION                                  
         BE    JOBD13                                                           
         CLI   TRNKSTYP,57         W/O                                          
         BE    JOBD13                                                           
         CLI   TRNKSTYP,34         TRANSFERS (INCLUDE REVERSALS)                
         BE    JOBD13                                                           
         B     JOBD3                                                            
*                                                                               
JOBD13   CLI   ACRQLEN,0           TEST ANY CONTRA FILTER                       
         BE    JOBD14                                                           
         SR    R1,R1                                                            
         IC    R1,ACRQLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QUNIT(0),TRNKULC                                                 
         BNE   JOBD3                                                            
*                                                                               
JOBD14   L     R2,AIO1                                                          
         BAS   RE,GET              GET JOB TRANSACTION                          
         LA    R4,TRNRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING TRXELD,R4                                                        
JOBD15   CLI   0(R4),TRXELQ        X TRX                                        
         BNE   *+12                                                             
         TM    TRXSTA2,TRXSNTMS    SKIP UPLOADS FOR TMS                         
         BO    JOBD3                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   JOBD15                                                           
*                                                                               
         L     R5,ASRTIO                                                        
         BAS   RE,CLRTMS           CLEAR TIME RECORD                            
         LA    R7,TMSINP                                                        
         USING TIMELD,R7                                                        
         MVC   TIMACC,TRNKEY+1     SJ ACCOUNT                                   
         MVC   TIMTSK,TRNKWORK     WORKCODE                                     
         MVC   TIMOFF,JOBOFC       OFFICE CODE                                  
         MVI   TIMTTYP,TIMTCB      BILLABLE                                     
*                                                                               
         MVI   TMSSRC,TMSSJOB      SOURCE IS JOB                                
         MVC   TMSXCPJ,TRNKEY+1    CLIENT/PROD/JOB - TAX KEY                    
         MVC   TMSTXTSK,TRNKWORK                                                
         MVI   TMSXIND,TMSXITAX    SET TAX TYPE                                 
         CLC   TRNKCCPY+1(2),PERSUL TEST CONTRA '1R'                            
         BNE   JOBD16                                                           
         XC    TMSTXTSK,TMSTXTSK   CLEAR TAX WORKCODE                           
         MVI   TMSXIND,TMSXIPER    SET PERSON TYPE                              
         MVC   TMSPACC,TRNKCCPY    PERSON ACCOUNT                               
*                                                                               
JOBD16   MVC   TMSCOFC,SPACES                                                   
         TM    STATUS,NOFFC                                                     
         BZ    *+10                                                             
         MVC   TMSCOFC,JOBOFC      OFFICE CODE                                  
         MVC   TMSCOST,JOBCOST     AND COST CODE                                
         MVC   TMSTDTE,TRNKDATE    TRANSACTION DATE                             
         MVC   TMSTREF,TRNKREF     TRANSACTION REFERENCE                        
         LA    R4,TRNRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING TRNELD,R4                                                        
JOBD17   CLI   0(R4),TRNELQ        TRANSACTION ELEMENT                          
         BNE   JOBD3                                                            
         MVC   TMSTRNT,TRNTYPE     SAVE TYPE                                    
         CLI   TRNTYPE,57                                                       
         BNE   *+8                                                              
         OI    TMSSTAT,TMSSW57                                                  
         CLI   TRNTYPE,49                                                       
         BNE   JOBD18                                                           
         TM    TRNRSTAT,TRNSREVS   MARK 49 REVERSALS                            
         BNO   JOBD18                                                           
         OI    TMSSTAT,TMSSR49                                                  
*                                                                               
JOBD18   MVC   TMSXBREF,TRNBTCH    BATCH REFERENCE                              
         MVC   TMSXREF,TRNREF      REFERENCE                                    
         CLI   TRNTYPE,34          34'S ARE ADJUSTMENTS                         
         BNE   *+8                                                              
         OI    TIMIND,TIMIADJ                                                   
         CLI   TRNTYPE,57          57'S ARE ADJ. AND W/O                        
         BNE   *+8                                                              
         OI    TIMIND,TIMIADJ                                                   
         ZAP   TIMAMNT,TRNAMNT     AMOUNT                                       
         CLI   TMSXIND,TMSXITAX    TEST TAX ITEM                                
         BE    *+10                                                             
         ZAP   TMSXBASE,TIMAMNT    AMOUNT IS BASIS                              
         BAS   RE,NARR             GET NARRATIVE                                
         B     JOBD31                                                           
*                                                                               
         USING SPDELD,R4                                                        
JOBD19   CLI   0(R4),SPDELQ        SUBSIDIARY POSTING                           
         BNE   JOBD21                                                           
         SR    R1,R1                                                            
         IC    R1,SPDLN                                                         
         SH    R1,=Y(SPDLN1Q+1)                                                 
         BNP   JOBD31                                                           
         EX    R1,*+4                                                           
         MVC   TIMINC(0),SPDACCS                                                
         B     JOBD31                                                           
*                                                                               
JOBD21   CLI   0(R4),PRTELQ        PERSONNEL RATE RECORD                        
         BNE   JOBD23                                                           
         BAS   RE,PRTL             POST PERSONNEL RATE ELEMENT                  
         B     JOBD31                                                           
*                                                                               
         USING TRSELD,R4                                                        
JOBD23   CLI   0(R4),TRSELQ        STATUS ELEMENT                               
         BNE   JOBD25                                                           
         BAS   RE,TRSL                                                          
         B     JOBD31                                                           
*                                                                               
         USING SUTELD,R4                                                        
JOBD25   CLI   0(R4),SUTELQ        TAX ITEMS                                    
         BNE   JOBD27                                                           
         CLI   SUTLN,SUTLN2Q       ONLY NEW TAX ELEMENTS                        
         BL    JOBD27                                                           
         CLI   TMSXIND,TMSXITAX    TEST TAX ITEM                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TMSTAX,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,TMSTAX           TAX DETAIL AREA                              
         MVI   TIMELD,TIMELQ       ELEMENT CODE                                 
         MVI   TIMLN,TIMTHDQ+TIMTMINQ                                           
         MVI   TIMSEQ,0                                                         
         MVI   TIMETYP,TIMETAX                                                  
         MVC   TIMTWC,TMSTXTSK     TAX TASK                                     
         MVC   TIMTLOC,SUTLOC+3    LOCALITY CODE                                
         ZAP   TIMTBAS,SUTBAS      BASIS                                        
         MVI   TIMTSTA,0                                                        
         MVI   TIMTMINI,1          NUMBER OF MINI'S                             
         MVC   TIMTACC,SPACES      LOCAL ACCOUNT ** NOT IN OLD ELEMENT          
         ZAP   TIMTRATE,SUTRTE     RATE                                         
         LA    RF,TMSINP                                                        
         ZAP   TIMTAMNT,TIMAMNT-TIMELD(L'TIMAMNT,RF) TRANS AMT IS TAX           
         LA    R7,TMSINP                                                        
         B     JOBD31                                                           
*                                                                               
         USING SPAELD,R4                                                        
JOBD27   CLI   0(R4),SPAELQ        1C ACCOUNT ELEM                              
         BNE   JOBD28                                                           
         CLI   SPATYPE,SPATCCST    CLIENT COSTING ACCOUNT                       
         BNE   JOBD31                                                           
         MVC   TMSCOST(1),QCOMPANY                                              
         MVC   TMSCOST+1(L'SPAAULA),SPAAULA                                     
*                                                                               
         USING PTAELD,R4                                                        
JOBD28   CLI   0(R4),PTAELQ        PROD ACTIVITY ELEMENT                        
         BNE   JOBD31                                                           
*                                                                               
JOBD31   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   JOBD19                                                           
*                                                                               
         BAS   RE,WKDT             GET W/E DATES                                
         BAS   RE,BUFJ             ADD JOB DETAILS TO BUFFALO                   
         GOTO1 ADSORTER,DMCB,=C'PUT',TMSD                                       
         CLI   TMSTRNT,57          TEST W/O                                     
         BNE   JOBD3                                                            
         OI    TIMIND,TIMIWO                                                    
         ZAP   DUB,TIMHRS                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   TIMHRS,DUB                                                       
         ZAP   DUB,TIMAMNT                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   TIMAMNT,DUB                                                      
         BAS   RE,BUFJ             ADD JOB DETAILS TO BUFFALO                   
         GOTO1 ADSORTER,DMCB,=C'PUT',TMSD                                       
         B     JOBD3                                                            
         EJECT                                                                  
***********************************************************************         
* GET BILLABLE JOBS AND TAX ITEMS FROM SORT                           *         
***********************************************************************         
         SPACE 1                                                                
BTIM     NTR1  ,                                                                
         L     R6,ATAXIO           R6=SAVED TAX DETAIL                          
         MVI   TMSXK-TMSD(R6),0    INIT TAXIO AREA                              
*                                                                               
BTIM3    L     R5,ASRTIO           R5=CURRENT RECORD FROM SORT                  
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   RE,15,DMCB+4        GET SORTED JOBS                              
         BZ    BTIM15                                                           
         L     R0,ASRTIO           SAVE CURRENT SORT RECORD                     
         LA    R1,TMSRLNQ                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   TMSXIND,TMSXITAX    TEST CURRENT ITEM A TAX                      
         BNE   BTIM7               IF NOT TRY TO MATCH TO SAVED TAX             
         CLI   TMSXK-TMSD(R6),0    TEST SAVED TAX RECORD                        
         BE    *+8                 NO, SAVED TAX                                
         BAS   RE,DMPTXE           UNMATCHED TAX ITEM(PRINT ERROR)              
         L     RE,ASRTIO           SAVE CURRENT TAX ITEM                        
         L     R0,ATAXIO                                                        
         LA    R1,TMSRLNQ                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     BTIM3                                                            
*                                                                               
BTIM7    CLI   TMSXK-TMSD(R6),0    TEST SAVED TAX RECORD                        
         BE    BTIM13              NO, SAVED TAX - WRITE TO OUTPUT              
         CLC   TMSXK(TMSXCLNQ),TMSXK-TMSD(R6)                                   
         BNE   BTIM13              NOT EQUAL - WRITE TO OUTPUT                  
         MVC   TMSTAX,TMSTAX-TMSD(R6)  MERGE THE TAX DETAIL                     
         MVI   TMSXK-TMSD(R6),0    INIT TAXIO AREA                              
*                                                                               
BTIM13   CLI   QOPT8,C'J'          CONVERSION DRIVEN BY SJ?                     
         BNE   BTIM3                                                            
         L     R5,ASRTIO                                                        
         L     R2,ATMSWRK                                                       
         PUT   (R2),(R5)           GENERATE NEW TIME RECORDS                    
         B     BTIM3                                                            
*                                                                               
BTIM15   CLI   TMSXK-TMSD(R6),0    TEST SAVED TAX RECORD                        
         BE    *+8                                                              
         BAS   RE,DMPTXE           UNMATCHED TAX ITEM(PRINT ERROR)              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ PERSON TRANSACTIONS AND EXTRACT TIMESHEET DETAIL               *         
***********************************************************************         
         SPACE 1                                                                
PTIM     NTR1  ,                                                                
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,PERACC     C/U/L ACCOUNT                                
         BAS   RE,HIGH             READ LEDGER                                  
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                NO ACCOUNT RECORD                            
*                                                                               
PTIM3    LA    R2,DIR                                                           
         BAS   RE,SEQ                                                           
         CLC   TRNKCULA,PERACC     SAME ACCOUNT                                 
         BNE   XIT                                                              
         CLI   TRNKDATE,C' '       TEST TRANSACTION                             
         BH    PTIM4                                                            
         CLI   QOPT6,C'N'          FIX 1N CONTRA?                               
         BNE   PTIM3                                                            
         CLC   TRNKCUNT(2),NONCUL  1N CONTRA                                    
         BNE   PTIM3                                                            
         CLI   TRNKCACT+1,C' '     OLD STYLE 1N CONTRA 1ND VAC                  
         BNE   PTIM3                                                            
         BAS   RE,FIXCON           FIX 1N CONTRA HEADERS/BUCKETS                
         B     PTIM3                                                            
PTIM4    TM    TRNKSTAT,TRNSDELT   SKIP DELETED TRANSACTIONS                    
         BO    PTIM3                                                            
         BAS   RE,TTIM             TEST TMS TIME                                
         BE    PTIM3                                                            
         TM    TRNKSTAT,TRNSDRFT   SKIP DRAFT TRANSACTIONS                      
         BO    PTIM3                                                            
         CLC   TRNKDATE,STDATE     TEST TRANSACTION DATE                        
         BL    PTIM3                                                            
         CLC   TRNKDATE,ENDDATE                                                 
         BH    PTIM3                                                            
         CLI   TRNKSTYP,27         TYPE 27, 41 AND 49                           
         BE    PTIM5                                                            
         CLI   TRNKSTYP,34         TYPE 34                                      
         BE    PTIM5                                                            
         CLI   TRNKSTYP,41                                                      
         BE    PTIM5                                                            
         CLI   TRNKSTYP,49                                                      
         BNE   PTIM3                                                            
*                                                                               
PTIM5    L     R2,AIO1                                                          
         BAS   RE,GET              GET TRANSACTION RECORD                       
*                                                                               
         CLI   QOPT6,C'N'          FIX 1N CONTRA?                               
         BNE   PTIM6                                                            
         CLC   TRNKCUNT(2),NONCUL  1N CONTRA                                    
         BNE   PTIM6                                                            
         CLI   TRNKCACT+1,C' '     OLD STYLE 1N CONTRA 1ND VAC                  
         BNE   PTIM6                                                            
         BAS   RE,NEW1N            FIX 1N CONTRA                                
*                                                                               
PTIM6    AP    RINP,=P'1'          COUNT INPUT FROM 1R                          
         L     R5,ASRTIO                                                        
         USING TMSD,R5                                                          
         BAS   RE,CLRTMS           CLEAR TMS WORK RECORD                        
*                                                                               
         LA    R7,TMSINP                                                        
         USING TIMELD,R7                                                        
         MVI   TIMLN,TIMILN1Q      LENGTH FOR NON-BILLABLE                      
         MVI   TIMTTYP,TIMTCN      CLIENT NON-BILLABLE                          
         MVI   TMSSRC,TMSSPER                                                   
         MVC   TMSPACC,PERACC      PERSON ACCOUNT                               
         MVC   TMSTDTE,TRNKDATE    TRANSACTION DATE                             
         MVC   TMSTREF,TRNKREF     TRANSACTION REFERENCE                        
         MVC   TMSCOST,TRNKCCPY    COSTING CODE                                 
         MVC   TMSCOFC,SPACES                                                   
         TM    STATUS,NOFFC                                                     
         BZ    *+10                                                             
         MVC   TMSCOFC,TRNKOFF     CLIENT OFFICE                                
         MVC   TMSNAME,PERNME      NAME                                         
         MVC   TMSPER,PERCDE       PERSON CODE                                  
         MVC   TMSODS,ODS          OFFICE/DEPT/SUB                              
         CLC   TMSCOST+1(2),NONCUL TEST 1N LEDGER                               
         BNE   *+14                                                             
         MVI   TIMTTYP,TIMTNC      NON-CLIENT                                   
         MVC   TIMACC,TMSCOST+1    1N ACCOUNT                                   
         BAS   RE,WKDT             GET W/E DATES                                
         LA    R4,TRNRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING TRNELD,R4                                                        
PTIM7    CLI   0(R4),TRNELQ        TRANSACTION ELEMENT                          
         BNE   PTIM3                                                            
         MVC   TMSXBREF,TRNBTCH    BATCH REFERENCE                              
         MVC   TMSXREF,TRNREF      REFERENCE                                    
         MVC   TIMOFF,TRNOFFC                                                   
         CLI   TRNTYPE,34          34'S ARE ADJUSTMENTS                         
         BNE   *+8                                                              
         OI    TIMIND,TIMIADJ      SHOULD BE MARKED ADJUSTED                    
         ZAP   TIMAMNT,TRNAMNT     AMOUNT                                       
         BAS   RE,NARR             GET NARRATIVE                                
         B     PTIM31                                                           
*                                                                               
PTIM13   CLI   0(R4),PRTELQ        PERSONNEL RATE RECORD                        
         BNE   PTIM15                                                           
         BAS   RE,PRTL             POST PERSONNEL RATE ELEMENT                  
         B     PTIM31                                                           
*                                                                               
         USING SCIELD,R4                                                        
PTIM15   CLI   0(R4),SCIELQ        SUBSIDIARY CASH ELEMENT                      
         BNE   PTIM17                                                           
         CLI   SCITYPE,SCITHOUR    TEST HOURS ELEMENT                           
         BNE   PTIM31                                                           
         ZAP   TIMHRS,SCIAMNT                                                   
         B     PTIM31                                                           
*                                                                               
         USING PCIELD,R4                                                        
PTIM17   CLI   0(R4),PCIELQ        PROJECT CONTROL INFO ELEMENT                 
         BNE   PTIM21                                                           
         CLC   NONCUL,TIMACC       1N ACCOUNT                                   
         BE    PTIM31                                                           
         MVC   TMSXCPJ,PCICLI+1    CLI/PRD/JOB INPUT ON TIMESHEET               
         MVC   TIMACC,PCICLI+1                                                  
         CLI   PCILN,PCILN2Q                                                    
         BL    PTIM31                                                           
         CLC   PCIPRJT+1(2),PCNTUL                                              
         BNE   *+10                                                             
         OC    TIMSTAT,PCSW        SET PROJECT CONTROL OPTION                   
         MVC   TIMTSK,PCITSK       TASK CODE                                    
         CLC   TIMACC+8(6),SPACES  TEST JOB FROM SJ                             
         BH    PTIM31              GOT ONE                                      
         CLC   PCIPRJT+3(12),SPACES                                             
         BNH   PTIM31                                                           
         MVC   TIMACC+2(12),PCIPRJT+3 GET PROJECT CPJ                           
         B     PTIM31                                                           
*                                                                               
         USING TRSELD,R4                                                        
PTIM21   CLI   0(R4),TRSELQ        STATUS ELEMENT                               
         BNE   PTIM31                                                           
         BAS   RE,TRSL                                                          
*                                                                               
PTIM31   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   PTIM13                                                           
         OC    TIMACC,SPACES                                                    
         OC    TIMTSK,SPACES                                                    
         CLI   TIMTTYP,TIMTCB      BILLABLE                                     
         BNE   PTIM35                                                           
         CLC   TIMTSK,SPACES                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PTIM35   LA    R7,TMSINP                                                        
         BAS   RE,BUFP             ADD PERSON RECORD TO BUFFALO SUMMARY         
         BAS   RE,PUTP                                                          
         B     PTIM3                                                            
         EJECT                                                                  
***********************************************************************         
* POST THE PERSONNEL RATE ELEMENT                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PRTELD,R4                                                        
PRTL     CLI   TIMTTYP,TIMTNC      1N TIME                                      
         BER   RE                                                               
         MVC   TIMREFF,PRTSTRT     EFFECTIVE DATE OF RATE                       
         OC    PRTRATE,PRTRATE                                                  
         BZ    *+10                                                             
         ZAP   TIMRATE,PRTRATE     HOURLY RATE (2DP)                            
         ZAP   TIMHRS,PRTHOUR      NUMBER OF HOURS                              
         TM    PRTSTAT,PRTSADJ                                                  
         BZ    *+8                                                              
         OI    TIMRBSTA,TIMRBADJ   RATE WAS ADJUSTED                            
*                                                                               
         MVI   TIMTTYP,TIMTCB                                                   
         MVI   TIMLN,TIMILN2Q                                                   
         MVC   TIMINC,SPACES                                                    
         TM    PRTSTAT,PRTSBILQ    CLIENT BILLABLE                              
         BZ    PRTL3                                                            
         ZAP   DUB,TIMHRS          HOURS                                        
         MP    DUB,TIMRATE         X RATE                                       
         SRP   DUB,64-2,5                                                       
         CLC   TRNKUNT(2),PRODUL   FOR SJ AMOUNT SHOULD = CALCULATION           
         BE    *+10                                                             
         ZAP   TIMAMNT,DUB                                                      
         CP    TIMAMNT,DUB                                                      
         BER   RE                                                               
*                                                                               
         ZAP   DUB,TIMHRS                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   TIMHRS,DUB          REVERSE HOURS - TRY AGAIN                    
         MP    DUB,TIMRATE         X RATE                                       
         SRP   DUB,64-2,5                                                       
         CP    TIMAMNT,DUB                                                      
         BER   RE                                                               
         ZAP   DUB,TIMHRS                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   TIMHRS,DUB          BACK TO WHAT IT WAS                          
         CLI   QOPT5,C'I'          IGNORE AMOUNT CHECK                          
         BER   RE                                                               
         DC    H'0'                                                             
*                                                                               
PRTL3    CP    TIMAMNT,=P'0'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TIMTTYP,TIMTCR                                                   
         TM    PRTSTAT,PRTSRTEQ    CLIENT REALIZATION                           
         BZ    PRTL5                                                            
         ZAP   DUB,TIMHRS          HOURS                                        
         MP    DUB,TIMRATE         X RATE                                       
         SRP   DUB,64-2,5                                                       
         ZAP   TIMAMNT,DUB                                                      
         BR    RE                                                               
*                                                                               
PRTL5    MVI   TIMTTYP,TIMTCN                                                   
         MVI   TIMLN,TIMILN1Q                                                   
         TM    PRTSTAT,PRTSNOTQ    CLIENT NON-BILLABLE                          
         BNZR  RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD NARRATIVE                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R4                                                        
NARR     SR    R1,R1               GET NARRATIVE                                
         CLI   TRNTYPE,57                                                       
         BER   RE                                                               
         IC    R1,TRNLN                                                         
         SH    R1,=Y(TRNLN1Q)                                                   
         LTR   R1,R1                                                            
         BNPR  RE                                                               
         LA    R7,TMSNARR                                                       
         CH    R1,=Y(L'TIMNARR)                                                 
         BL    *+8                                                              
         LH    R1,=Y(L'TIMNARR)                                                 
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TIMNARR(0),TRNNARR                                               
         MVI   TIMEL,TIMELQ                                                     
         LA    R1,TIMHLNQ+1(R1)                                                 
         STC   R1,TIMLN                                                         
         MVI   TIMSEQ,0                                                         
         MVI   TIMETYP,TIMENAR                                                  
         LA    R7,TMSINP                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* POST THE TRANSACTION STATUS ELEMENT                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRSELD,R4                                                        
TRSL     ST    RE,SVRE                                                          
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,TIMADAT)                              
         MVC   TMSXADTE,TIMADAT                                                 
         MVC   TMSXMOA,TRSPMOS     MOA                                          
         MVC   TIMMOA,TRSPMOS                                                   
         TM    TRSSTAT2,TRSSTADJ                                                
         BZ    *+8                                                              
         OI    TIMIND,TIMIADJ      ADJUSTED TIMESHEET                           
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TMS RECORDS                                                *          
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R2                                                       
TTIM     CLC   TIMKREF,=C'*TIME*'  TEST TIME RECORD                             
         BNER  RE                                                               
         CLI   TIMKRI1,TIMKRI1Q                                                 
         BNER  RE                                                               
         CLI   TIMKRI2,TIMKRI2Q                                                 
         BNER  RE                                                               
         CLI   TIMKRI3,TIMKRI3Q                                                 
         BNER  RE                                                               
         CLI   TIMKRI4,TIMKRI4Q                                                 
         BNER  RE                                                               
*                                                                               
TTIM3    NTR1  ,                                                                
         L     R2,AIO1                                                          
         BAS   RE,GET              GET TRANSACTION RECORD                       
         L     R5,ASRTIO                                                        
         USING TMSD,R5                                                          
         LA    R7,TIMRFST                                                       
         USING TIMELD,R7                                                        
*                                                                               
TTIM5    CLI   TIMEL,0             END OF RECORD                                
         BE    XIT                                                              
         CLI   TIMEL,TIMELQ        TIME ELEMENT                                 
         BNE   TTIM6                                                            
         CLI   TIMETYP,TIMEINP     TYPE - INPUT                                 
         BE    TTIM7                                                            
*                                                                               
TTIM6    SR    R0,R0                                                            
         IC    R0,TIMLN                                                         
         AR    R7,R0                                                            
         B     TTIM5                                                            
*                                                                               
TTIM7    BAS   RE,CLRTMS           CLEAR TMS WORK RECORD                        
         MVI   TMSSRC,TMSSPER                                                   
         MVC   TMSPACC,TIMKCPY     PERSON ACCOUNT                               
         MVC   TMSCOFC,TIMKOFF                                                  
         MVC   TMSCOST,TIMKCCPY    COSTING CODE                                 
         MVC   TMSWKND,TIMKPEDT                                                 
         MVC   TMSTDTE,TIMKPEDT    TRANSACTION DATE                             
         MVC   TMSTREF,TIMKREF     TRANSACTION REFERENCE                        
         MVC   TMSNAME,PERNME      NAME                                         
         MVC   TMSPER,PERCDE       PERSON CODE                                  
         MVC   TMSODS,ODS          OFFICE/DEPT/SUB                              
         SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TMSINP(0),TIMEL     SAVE TIME INPUT                              
         LR    R6,R7                                                            
         LA    R7,1(R1,R7)                                                      
         CLI   TIMEL,TIMELQ                                                     
         BNE   TTIM17                                                           
         CLI   TIMETYP,TIMETAX     TEST TAX ELEMENT                             
         BNE   TTIM11                                                           
         MVC   TMSTAX,TIMEL                                                     
         IC    R1,TIMLN                                                         
         AR    R7,R1                                                            
         CLI   TIMEL,TIMELQ                                                     
         BNE   TTIM17                                                           
*                                                                               
TTIM11   CLI   TIMETYP,TIMENAR     TEST NARRATIVE ELEMENT                       
         BNE   TTIM17                                                           
         MVC   TMSNARR,TIMEL                                                    
*                                                                               
TTIM17   LA    R7,TMSINP                                                        
         BAS   RE,BUFP                                                          
         BAS   RE,PUTP             GENERATE NEW TIME RECORDS                    
TTIM19   LR    R7,R6                                                            
         B     TTIM6                                                            
         EJECT                                                                  
***********************************************************************         
* PUT PERSON DATA TO WORK FILE                                        *         
***********************************************************************         
         SPACE 1                                                                
PUTP     NTR1  ,                                                                
         CLI   QOPT8,C'J'          CONVERSION DRIVEN BY SJ?                     
         BNE   PUTP02                                                           
         CLC   TIMTSK,SPACES       BILLABLE/REAL/CLIENT FROM JOB                
         BH    XIT                                                              
*        CLI   TIMTTYP,TIMTCB      BILLABLE FROM JOB                            
*        BE    XIT                                                              
PUTP02   L     R2,ATMSWRK                                                       
         PUT   (R2),(R5)           GENERATE NEW TIME RECORDS                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MERGE PERSON(1R) RECORD WITH JOB(SJ) RECORDS                        *         
***********************************************************************         
         SPACE 1                                                                
MRGR     NTR1  ,                                                                
         L     R5,ATMSIO                                                        
         BAS   RE,CLRTMS                                                        
         L     RF,ANEWC                                                         
         MVI   0(RF),X'FF'         SET END OF CLUSTER TABLE                     
         MVI   SEQN,1              KEEP NUMBER OF ITEMS                         
         MVI   DTYPE,0             DATA TYPE                                    
*                                                                               
MRGR3    L     R6,ASRTIO                                                        
         MVI   0(R6),X'FF'                                                      
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   RE,15,DMCB+4        GET SORTED RECORDS                           
         BZ    MRGR23                                                           
         L     R0,ASRTIO           SAVE CURRENT SORT RECORD                     
         LA    R1,TMSRLNQ                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   TMSKEY,0            TEST FIRST TIME                              
         BNE   MRGR7                                                            
*                                                                               
MRGR5    L     R0,ATMSIO           MOVE INPUT TO TMSIO                          
         L     RE,ASRTIO                                                        
         LA    R1,TMSRLNQ                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     MRGR11                                                           
*                                                                               
MRGR7    CLC   TMSKEY(TMSKLNQ),TMSKEY-TMSD(R6)   SAME KEY                       
         BNE   MRGR23                                                           
         L     R0,ATMSIO           MOVE INPUT TO TMSIO                          
         L     RE,ASRTIO                                                        
         LA    R1,TMSRLNQ                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
MRGR11   BAS   RE,ADDC             TO NEW CLUSTER ELEMENT TABLE                 
         B     MRGR3                                                            
*                                                                               
MRGR23   CLC   TMSKEY(15),TMSKEY-TMSD(R6)   TEST PERSON                         
         BE    *+8                                                              
         BAS   RE,PTOTS                                                         
         MVC   TKEY,TMSKEY         SAVE KEY                                     
         BAS   RE,PUTBK            WRITE NEW RECORDS TO FILE                    
         L     RF,ANEWC                                                         
         MVI   0(RF),X'FF'         SET END OF CLUSTER TABLE                     
         MVI   SEQN,1              KEEP NUMBER OF ITEMS                         
         MVI   DTYPE,0             DATA TYPE                                    
         CLI   0(R6),X'FF'         EOF                                          
         BNE   MRGR5                                                            
         BAS   RE,CHEK1R           CHECK ALL 1R ACCOUNT RECS TO TAPE            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD INPUT RECORD CLUSTER TO TABLE OF ELEMENT CLUSTERS               *         
***********************************************************************         
         SPACE 1                                                                
ADDC     NTR1  ,                                                                
         LA    R0,MAXEC            R0 = MAXIMUM NUMBER CLUSTERS                 
         L     R3,ATMSIO           RF = INPUT RECORD                            
         LA    R2,TMSINP-TMSD(R3)                                               
         L     R5,ANEWC            R5 = TABLE NEW CLUSTERS                      
         USING ECD,R5                                                           
*                                                                               
ADDC3    LA    R7,ECDINP                                                        
         USING TIMELD,R7                                                        
         CLI   0(R5),X'FF'         TEST EOT                                     
         BE    ADDC17              NOT FOUND ADD NEW ENTRY                      
         AH    R5,=Y(ECDLNQ)       NOT FOUND LOOK AT NEXT TABLE ENTRY           
         BCT   R0,ADDC3                                                         
         DC    H'0'                CLUSTER TABLE IS FULL                        
*                                                                               
ADDC17   XC    ECD(ECDLNQ),ECD     CLEAR A SLOT                                 
         TM    TMSSTAT-TMSD(R3),TMSSR49     REVERSED 49                         
         BNO   *+8                                                              
         MVI   ECDTYP,ECDTR49                                                   
         TM    TMSSTAT-TMSD(R3),TMSSW57     WRITE OFF                           
         BNO   *+8                                                              
         MVI   ECDTYP,ECDTW57                                                   
         MVC   ECDINP,TMSINP-TMSD(R3) SAVE NEW CLUSTER DATA                     
         MVC   ECDTAX,TMSTAX-TMSD(R3)                                           
         MVC   ECDNAR,TMSNARR-TMSD(R3)                                          
         MVC   TIMSEQ,SEQ                  SET SEQUENCE                         
         LA    R7,ECDTAX                                                        
         CLI   0(R7),0                                                          
         BE    *+10                                                             
         MVC   TIMSEQ,SEQ                  SET SEQUENCE                         
         LA    R7,ECDNAR                                                        
         CLI   0(R7),0                                                          
         BE    *+10                                                             
         MVC   TIMSEQ,SEQ                                                       
         LA    R5,ECDLNQ(R5)                                                    
         MVI   0(R5),X'FF'         END OF TABLE                                 
         SR    R0,R0                                                            
         IC    R0,SEQN                                                          
         AH    R0,=H'1'            ADD 1 TO SEQUENCE                            
         STC   R0,SEQN                                                          
*                                                                               
ADDCX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PUT NEW/CHANGED RECORDS TO FILE                                     *         
***********************************************************************         
         SPACE 1                                                                
PUTBK    NTR1  ,                                                                
         MVI   SEQN,0              GET HIGHEST SEQUENCE NUMBER                  
         ZAP   RITEM,=P'0'         COUNT NUMBER OF ITEMS                        
         ZAP   RHOUR,=P'0'         ADD HOURS                                    
         ZAP   RAMNT,=P'0'         AND AMOUNTS                                  
         BAS   RE,MATC             MATCH NEW REVERSED 49'S                      
         BAS   RE,GETC             GET OLD CLUSTERS                             
         BAS   RE,MRGC             MERGE NEW INTO OLD                           
         CLI   QOPT6,C'D'          DELETE OLD UNNEEDED CLUSTERS                 
         BNE   *+8                                                              
         BAS   RE,DELC                                                          
         BAS   RE,REVN             ADD REVISION NUMBERS                         
         BAS   RE,BLDR             BUILD NEW RECORDS                            
         BAS   RE,DELR             DELETE OLD RECORDS                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MATCH REVERSED INPUT TYPE 49'S AND WRITEOFFS ON NEW CLUSTERS                  
***********************************************************************         
         SPACE 1                                                                
MATC     NTR1  ,                                                                
         L     R2,ANEWC            R2=NEW CLUSTER TABLE                         
         USING ECD,R2                                                           
         B     *+8                                                              
*                                                                               
MATC03   LA    R2,ECDLNQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         LA    R7,ECDINP-ECD(R2)   R7=NEW DATA                                  
         USING TIMELD,R7                                                        
         TM    ECDSTAT,ECDSMAT     MATCHED                                      
         BO    MATC03                                                           
         CLI   ECDTYP,ECDTR49                                                   
         BE    *+12                                                             
         CLI   ECDTYP,ECDTW57                                                   
         BNE   MATC03                                                           
         LR    R4,R2               R4=NEXT NEW CLUSTER ITEM                     
*                                                                               
MATC05   LA    R4,ECDLNQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BE    MATC03                                                           
         TM    ECDSTAT-ECD(R4),ECDSMAT  ALREADY MATCHED                         
         BO    MATC05                                                           
*        CLC   ECDTYP,ECDTYP-ECD(R4)   MUST BE SAME TYPE                        
*        BNE   MATC05                                                           
         MVI   BYTE,C'R'           REVERSE CHECK                                
         BAS   RE,INPCMP           COMPARE INPUT                                
         BNE   MATC05              GET NEXT ITEM                                
         BAS   RE,TAXCMP           COMPARE TAX                                  
         BNE   MATC05              GET NEXT                                     
         BAS   RE,NARCMP           COMPARE NARRATIVE                            
         BNE   MATC05                                                           
         OI    ECDSTAT-ECD(R2),ECDSMAT   FLAG BOTH AS USED                      
         OI    ECDSTAT-ECD(R4),ECDSMAT                                          
         B     MATC03                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD CLUSTER FROM RECORDS ON FILE                                  *         
***********************************************************************         
         SPACE 1                                                                
GETC     NTR1  ,                                                                
         L     R5,AOLDC                                                         
         USING ECD,R5                                                           
         MVI   0(R5),X'FF'         END OF CLUSTER                               
         XC    DKEY,DKEY                                                        
         XC    RDA,RDA             CLEAR DISK ADDRESSES                         
         MVI   NDA,0               MAXIMUM NUMBER OF RECORDS                    
         MVC   DKEY,TKEY                                                        
         GOTO1 HIGH                                                             
*                                                                               
GETC02   CLC   DIR(L'TKEY),DKEY    TEST SAME PER/CONTRA/DATE                    
         BNE   XIT                                                              
         CLC   DIR+(TRNKREF-TRNKEY)(6),=C'*TIME*'                               
         BNE   XIT                 FOR FIRST CONVERSION                         
*        BNE   GETC17              FOR FILE FIXES                               
         L     R2,AIO1                                                          
         BAS   RE,GET              GET THE RECORD                               
         CLC   DIR(L'TKEY),0(R2)   TEST CORRECT RECORD                          
         BNE   XIT                                                              
         SR    R1,R1                                                            
         IC    R1,NDA              NUMBER OF RECORDS                            
         LA    R1,1(R1)                                                         
         STC   R1,NDA                                                           
         CH    R1,=Y(MAXDA)                                                     
         BNH   *+6                                                              
         DC    H'0'                TOO MANY RECORDS                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R3,RDA(R1)                                                       
         MVC   0(4,R3),DA          SAVE DISK ADDRESS                            
         LA    R7,TIMRFST-TIMRECD(R2)                                           
         USING TIMELD,R7                                                        
*                                                                               
GETC03   CLI   0(R7),0                                                          
         BE    GETC17                                                           
         CLI   TIMEL,TIMELQ        MUST BE 8B                                   
         BNE   GETC13                                                           
         CLI   TIMETYP,TIMEINP     MUST HAVE INPUT DATA                         
         BNE   GETC13                                                           
         CLC   TIMSEQ,SEQN                                                      
         BNH   *+10                                                             
         MVC   SEQN,TIMSEQ                                                      
         XC    ECD(ECDLNQ),ECD                                                  
         MVC   ECDINP,TIMEL        SAVE INPUT DATA                              
         SR    RE,RE                                                            
         IC    RE,TIMLN                                                         
         AR    RE,R7                                                            
*                                                                               
GETC07   CLI   0(RE),TIMELQ        ADD TAX AND NARRATIVE TO TABLE               
         BNE   GETC11                                                           
         CLC   TIMSEQ,TIMSEQ-TIMELD(RE)                                         
         BNE   GETC11                                                           
         LA    RF,ECDTAX                                                        
         CLI   TIMETYP-TIMELD(RE),TIMETAX                                       
         BE    GETC09                                                           
         LA    RF,ECDNAR                                                        
         CLI   TIMETYP-TIMELD(RE),TIMENAR                                       
         BNE   GETC11                                                           
*                                                                               
GETC09   SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),0(RE)                                                    
         LA    RE,1(R1,RE)                                                      
         B     GETC07                                                           
*                                                                               
GETC11   LA    R5,ECDLNQ(R5)                                                    
         MVI   0(R5),X'FF'                                                      
GETC13   SR    R0,R0                                                            
         IC    R0,TIMLN                                                         
         AR    R7,R0                                                            
         B     GETC03                                                           
*                                                                               
GETC17   BAS   RE,SEQ              GET NEXT                                     
         B     GETC02                                                           
         EJECT                                                                  
***********************************************************************         
* MERGE NEW CLUSTER INTO OLD                                          *         
***********************************************************************         
         SPACE 1                                                                
MRGC     NTR1  ,                                                                
         L     R2,ANEWC            R2=NEW CLUSTER TABLE                         
         B     *+8                                                              
*                                                                               
MRGC03   LA    R2,ECDLNQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         TM    ECDSTAT-ECD(R2),ECDSMAT   MATCHED 49 / 57                        
         BO    MRGC03                                                           
         LA    R7,ECDINP-ECD(R2)   R7=NEW DATA                                  
         USING TIMELD,R7                                                        
         L     R4,AOLDC            R4=OLD CLUSTER TABLE                         
         B     *+8                                                              
*                                                                               
MRGC05   LA    R4,ECDLNQ(R4)                                                    
         CLI   0(R4),X'FF'         NOT FOUND - ADD IT                           
         BE    MRGC17                                                           
         TM    ECDSTAT-ECD(R4),ECDSKEP                                          
         BO    MRGC05              ALREADY KEPT                                 
         MVI   BYTE,0                                                           
         BAS   RE,INPCMP           TEST SAME INPUT                              
         BNE   MRGC05                                                           
         BAS   RE,TAXCMP           TEST TAX ITEMS                               
         BNE   MRGC05                                                           
         BAS   RE,NARCMP           NARRATIVE                                    
         BNE   MRGC05                                                           
         OI    ECDSTAT-ECD(R4),ECDSKEP                                          
         B     MRGC03              MATCHED OLD TABLE                            
*                                                                               
MRGC17   SR    R3,R3               UPDATE SEQUENCE                              
         IC    R3,SEQN                                                          
         LA    R3,1(R3)                                                         
         STC   R3,SEQN                                                          
         MVC   TIMSEQ,SEQN                                                      
         LA    RF,ECDTAX-ECD(R2)                                                
         MVC   TIMSEQ-TIMELD(1,RF),SEQN                                         
         LA    RF,ECDNAR-ECD(R2)                                                
         MVC   TIMSEQ-TIMELD(1,RF),SEQN                                         
         MVC   0(ECDLNQ,R4),0(R2)                                               
         BAS   RE,DMPEL                                                         
         LA    R7,ECDNAR-ECD(R4)                                                
         CLI   1(R7),0                                                          
         BE    *+8                                                              
         BAS   RE,DMPEL                                                         
         OI    ECDSTAT-ECD(R4),ECDSKEP KEEP IT                                  
         LA    R4,ECDLNQ(R4)                                                    
         MVI   0(R4),X'FF'                                                      
         B     MRGC03                                                           
         EJECT                                                                  
***********************************************************************         
* DELETE BAD ELEMENT CLUSTERS                                         *         
***********************************************************************         
         SPACE 1                                                                
DELC     NTR1  ,                                                                
         L     R2,AOLDC            R2=OLD CLUSTER TABLE                         
         B     *+8                                                              
*                                                                               
DELC03   LA    R2,ECDLNQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         LA    R7,ECDINP-ECD(R2)   R7=OLD DATA                                  
         TM    ECDSTAT-ECD(R2),ECDSKEP  TEST KEEP STATUS                        
         BO    DELC03                                                           
         USING TIMELD,R7                                                        
         LR    R4,R2               R4=NEXT NEW CLUSTER ITEM                     
*                                                                               
DELC05   LA    R4,ECDLNQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BE    DELC07              NO MATCH DELETE                              
         TM    ECDSTAT-ECD(R4),ECDSKEP  TEST KEEP                               
         BO    DELC05                                                           
         MVI   BYTE,C'R'           REVERSE CHECK                                
         BAS   RE,INPCMP           COMPARE INPUT                                
         BNE   DELC05              GET NEXT ITEM                                
         BAS   RE,TAXCMP           COMPARE TAX                                  
         BNE   DELC05              GET NEXT                                     
         BAS   RE,NARCMP           COMPARE NARRATIVE                            
         BNE   DELC05                                                           
         OI    ECDSTAT-ECD(R2),ECDSKEP                                          
         OI    ECDSTAT-ECD(R4),ECDSKEP                                          
         B     DELC03                                                           
*                                                                               
DELC07   OI    ECDSTAT-ECD(R2),ECDSDEL  MARK IT DELETE                          
         LA    R7,ECDINP-ECD(R2)                                                
         USING TIMELD,R7                                                        
         MVI   TIMEL,X'00'                                                      
         BAS   RE,DMPEL            HEX OUT THE DATA TO BE DELETED               
         LA    R7,ECDNAR-ECD(R2)                                                
         CLI   1(R7),0                                                          
         BE    DELC03                                                           
         MVI   TIMEL,X'00'                                                      
         BAS   RE,DMPEL                                                         
         B     DELC03                                                           
         EJECT                                                                  
***********************************************************************         
* COMPARE INPUT DETAIL                                                *         
***********************************************************************         
         SPACE 1                                                                
INPCMP   TM    ECDSTAT-ECD(R4),ECDSUS      ALREADY USED                         
         BO    NO                                                               
         LA    R6,ECDINP-ECD(R4)   R6=NEW DATA                                  
         CLC   TIMINP(TIMSTAT-TIMINP),TIMINP-TIMELD(R6)                         
         BNE   NO                                                               
         ZAP   DUB,TIMHRS                                                       
         CLI   BYTE,C'R'           REVERSE CHECK                                
         BNE   *+10                                                             
         MP    DUB,=P'-1'                                                       
         CP    DUB,TIMHRS-TIMELD(L'TIMHRS,R6)                                   
         BNE   NO                                                               
*                                                                               
         CLI   TIMLN,TIMILN2Q      TEST BILLABLE                                
         BL    YES                                                              
         CP    TIMRATE,TIMRATE-TIMELD(L'TIMRATE,R6)                             
         BNE   NO                                                               
         CLC   TIMINC,TIMINC-TIMELD(R6)                                         
         BNE   NO                                                               
         ZAP   DUB,TIMAMNT                                                      
         CLI   BYTE,C'R'           REVERSE CHECK                                
         BNE   *+10                                                             
         MP    DUB,=P'-1'                                                       
         CP    DUB,TIMAMNT-TIMELD(L'TIMAMNT,R6)                                 
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* COMPARE TAX DETAIL                                                  *         
***********************************************************************         
         SPACE 1                                                                
TAXCMP   LA    R1,ECDTAX-ECD(R2)   TEST SAME TAX                                
         LA    RF,ECDTAX-ECD(R4)                                                
         CLC   1(1,R1),1(RF)       TEST LENGTH OF TAX ELEMENT                   
         BNE   NO                                                               
         SR    R3,R3                                                            
         ICM   R3,1,1(R1)          LENGTH ANY DATA                              
         BZ    YES                                                              
         AH    R1,=Y(TIMHLNQ)                                                   
         AH    RF,=Y(TIMHLNQ)                                                   
         SH    R3,=Y(TIMHLNQ+1)                                                 
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(RF)                                                    
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* COMPARE NARRATIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
NARCMP   LA    R1,ECDINP-ECD(R2)                                                
         TM    TIMIND-TIMELD(R1),TIMIADJ                                        
         BO    YES                                                              
         LA    R1,ECDNAR-ECD(R2)   TEST SAME NARRATIVE                          
         LA    RF,ECDNAR-ECD(R4)                                                
         CLC   1(1,R1),1(RF)       TEST LENGTH OF TAX ELEMENT                   
         BNE   NO                                                               
         SR    R3,R3                                                            
         ICM   R3,1,1(R1)                                                       
         BZ    YES                                                              
         AH    R1,=Y(TIMHLNQ)                                                   
         AH    RF,=Y(TIMHLNQ)                                                   
         SH    R3,=Y(TIMHLNQ+1)                                                 
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(RF)                                                    
         BNE   NO                                                               
         B     YES                 MATCHED - GET NEXT                           
*                                                                               
NO       LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
YES      CR    RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ADD REVISION NUMBER TO CLUSTER                                      *         
***********************************************************************         
         SPACE 1                                                                
REVN     NTR1  ,                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO1R                                                         
         OC    ACTKCULA,ACTKCULA   1ST TIME?                                    
         BZ    REVN02                                                           
         CLC   ACTKCULA,TKEY       SAME 1R?                                     
         BE    REVN03              YES- CONTINUE WITH CURRENT AIO1R             
         CLC   PERSUL,ACTKUNT                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TAPE1R           PUT 1R ACCOUNT RECORD TO TAPE                
*                                                                               
REVN02   MVC   DKEY,SPACES                                                      
         MVC   DKEY(15),TKEY                                                    
         BAS   RE,READ             READ ACCOUNT RECORD                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1R                                                         
         BAS   RE,GET                                                           
REVN03   SR    R0,R0                                                            
         LA    R2,ACTRFST-ACTRECD(R2)                                           
*                                                                               
REVN04   CLI   0(R2),X'30'                                                      
         BE    REVN05                                                           
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     REVN04                                                           
*                                                                               
         USING RSTELD,R2                                                        
REVN05   SR    R3,R3                                                            
         ICM   R3,3,RSTSNUM                                                     
         LA    R3,1(R3)                                                         
         L     R5,AOLDC                                                         
         USING ECD,R5                                                           
         B     *+8                                                              
*                                                                               
REVN07   LA    R5,ECDLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BE    REVN11                                                           
         LA    R7,ECDINP                                                        
         USING TIMELD,R7                                                        
         OC    TIMLINE#,TIMLINE#                                                
         BNZ   REVN07                                                           
         STCM  R3,3,TIMLINE#                                                    
         LA    R3,1(R3)                                                         
         B     REVN07                                                           
*                                                                               
REVN11   STCM  R3,3,RSTSNUM                                                     
         L     RE,ASRTIO                                                        
         CLI   0(RE),X'FF'                                                      
         BNE   *+8                                                              
         BAS   RE,TAPE1R           PUT LAST 1R ACCOUNT RECORD TO TAPE           
         B     XIT                                                              
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PUT 1R ACCOUNT RECORD TO TAPE                                       *         
***********************************************************************         
*        SPACE 1                                                                
TAPE1R   ST    RE,SVRE                                                          
         USING ACTRECD,R2                                                       
         L     R2,AIO1R                                                         
*                                  ADD DUMMY TMS EL TO IDENTIFY IN LOAD         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),ACTRECD,TMSDUMEL,=C'ADD=END'            
         LR    R4,R2                                                            
         BAS   RE,PUTTAPE          PUT 1R ACCOUNT RECORD TO TAPE                
*                                                                               
         USING EMPD,RE                                                          
         L     RE,AEMPTAB          MARK THIS 1R UPDATED                         
TAPE1R02 CLI   EMPCODE,X'FF'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   EMPCODE,ACTKACT                                                  
         BE    *+12                                                             
         LA    RE,EMPDLNQ(RE)                                                   
         B     TAPE1R02                                                         
         OI    EMPSTA,EMPUSED                                                   
         L     RE,SVRE                                                          
         BR    RE                                                               
         DROP  R2,RE                                                            
         EJECT                                                                  
***********************************************************************         
* ADD REVISION NUMBER TO CLUSTER                                      *         
***********************************************************************         
*        SPACE 1                                                                
*EVN     NTR1  ,                                                                
*        MVC   DKEY,SPACES                                                      
*        MVC   DKEY(15),TKEY                                                    
*        BAS   RE,READ             READ ACCOUNT RECORD                          
*        CLI   8(R1),0                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        L     R2,AIO2                                                          
*        BAS   RE,GET                                                           
*        SR    R0,R0                                                            
*        LA    R2,ACTRFST-ACTRECD(R2)                                           
*                                                                               
*EVN03   CLI   0(R2),X'30'                                                      
*        BE    REVN05                                                           
*        CLI   0(R2),0                                                          
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        IC    R0,1(R2)                                                         
*        AR    R2,R0                                                            
*        B     REVN03                                                           
*                                                                               
*        USING RSTELD,R2                                                        
*EVN05   SR    R3,R3                                                            
*        ICM   R3,3,RSTSNUM                                                     
*        LA    R3,1(R3)                                                         
*        L     R5,AOLDC                                                         
*        USING ECD,R5                                                           
*        B     *+8                                                              
*                                                                               
*EVN07   LA    R5,ECDLNQ(R5)                                                    
*        CLI   0(R5),X'FF'                                                      
*        BE    REVN11                                                           
*        LA    R7,ECDINP                                                        
*        USING TIMELD,R7                                                        
*        OC    TIMLINE#,TIMLINE#                                                
*        BNZ   REVN07                                                           
*        STCM  R3,3,TIMLINE#                                                    
*        LA    R3,1(R3)                                                         
*        B     REVN07                                                           
*                                                                               
*EVN11   STCM  R3,3,RSTSNUM                                                     
*        GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO2,DMWORK                        
*        ORG   *-2                                                              
*        CLI   RCWRITE,C'N'                                                     
*        BE    *+6                                                              
*        BASR  RE,RF                                                            
*        B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK FOR 1R'S WITH NO ACTIVITY TO INSURE ACCOUNT IS ADDED TO TAPE  *         
***********************************************************************         
         SPACE 1                                                                
CHEK1R   NTR1  ,                                                                
         USING EMPD,R3                                                          
         L     R3,AEMPTAB          CHECK FOR 1R'S NOT PUT TO TAPE               
CHEK01   CLI   EMPCODE,X'FF'                                                    
         BE    XIT                                                              
         TM    EMPSTA,EMPUSED                                                   
         BZ    *+12                                                             
CHEK02   LA    R3,EMPDLNQ(R3)                                                   
         B     CHEK01                                                           
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),RCCOMPFL                                                 
         MVC   DKEY+1(2),PERSUL                                                 
         MVC   DKEY+3(L'EMPCODE),EMPCODE                                        
         BAS   RE,READ             READ ACCOUNT RECORD                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         BAS   RE,GET                                                           
         LR    R4,R2                                                            
*                                  ADD DUMMY TMS EL TO IDENTIFY IN LOAD         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R4),TMSDUMEL,=C'ADD=END'               
         BAS   RE,PUTTAPE          PUT 1R ACCOUNT RECORD TO TAPE                
         B     CHEK02                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD NEW RECORDS FROM CLUSTER TABLES                               *         
***********************************************************************         
         SPACE 1                                                                
BLDR     NTR1  ,                                                                
         L     R0,AIO1             CLEAR OUTPUT RECORD IO                       
         LA    R1,MAXRLNQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO1             R4 = RECORD TO BE BUILT                      
         USING TIMRECD,R4                                                       
         MVC   TIMKEY(L'TKEY),TKEY PER/OFF/CONTRA/DATE                          
         MVC   TIMKREF,=C'*TIME*'                                               
         MVI   TIMKSBR,0                                                        
         MVI   TIMRRI1,TIMKRI1Q                                                 
         MVI   TIMRRI2,TIMKRI2Q                                                 
         MVI   TIMRRI3,TIMKRI3Q                                                 
         MVI   TIMRRI4,TIMKRI4Q                                                 
         LA    R1,TIMRFST-TIMRECD                                               
         STCM  R1,3,TIMRLEN                                                     
         L     R5,AOLDC            R5 = CLUSTER TABLE                           
         CLI   0(R5),X'FF'                                                      
         BE    XIT                 TABLE IS EMTPY                               
*                                                                               
BLDR3    TM    ECDSTAT-ECD(R5),ECDSDEL TEST DELETED ENTRY                       
         BO    BLDR7                                                            
         TM    ECDSTAT-ECD(R5),ECDSKEP MARK SURE IT'S KEPT                      
         BO    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,BLDM             BUILD MINIO CLUSTER                          
         SR    R0,R0                                                            
         IC    R0,TIMKSBR                                                       
         SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN        R1 = RECORD LENGTH                           
         LH    R3,MINLEN           R3 = MINIO CLUSTER LENGTH                    
         AR    R1,R3                                                            
         CH    R1,=Y(MAXRLNQ)      TEST RECORD LENGTH                           
         BNH   BLDR5                                                            
         BAS   RE,PUTR             PUT THE CURRENT RECORD                       
         AH    R0,=H'1'            BUMP SUBREFERENCE                            
         OI    DTYPE,DTMLT         SET MULTI-RECORD FLAG                        
         LA    R1,TIMRFST-TIMRECD  BEGIN NEW RECORD                             
         STCM  R1,3,TIMRLEN                                                     
         XC    TIMRFST(4),TIMRFST                                               
*                                                                               
BLDR5    STC   R0,TIMKSBR                                                       
         SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN        R1 = RECORD LENGTH                           
         LA    R2,TIMKEY(R1)       R2 = EOR                                     
         LH    R3,MINLEN           R3 = MINIO CLUSTER LENGTH                    
         LA    RE,MINCLST          RE = MINO CLUSTER                            
         LR    RF,R3               RF = LENGTH OF MINIO CLUSTER                 
         MVCL  R2,RE               CLUSTER TO RECORD                            
         ICM   R1,3,TIMRLEN        R1 = RECORD LENGTH                           
         LH    R3,MINLEN           R3 = MINIO CLUSTER LENGTH                    
         AR    R1,R3                                                            
         STCM  R1,3,TIMRLEN        UPDATE RECORD LENGTH                         
         AR    R1,R4                                                            
         MVI   0(R1),0             EOR                                          
         LA    RE,MINCLST                                                       
         OC    TIMRLMOS,TIMRLMOS                                                
         BZ    *+14                                                             
         CLC   TIMRLMOS,TIMMOA-TIMELD(RE)                                       
         BL    *+10                                                             
         MVC   TIMRLMOS,TIMMOA-TIMELD(RE)  SET LOWEST  MOA                      
         CLC   TIMRHMOS,TIMMOA-TIMELD(RE)                                       
         BH    *+10                                                             
         MVC   TIMRHMOS,TIMMOA-TIMELD(RE)  SET HIGHEST MOA                      
*                                                                               
BLDR7    AH    R5,=Y(ECDLNQ)       R5 = TO NEXT CLUSTER                         
         CLI   0(R5),X'FF'         TEST EOT                                     
         BNE   BLDR3                                                            
         BAS   RE,PUTR             PUT THE LAST RECORD TO TAPE                  
         XIT                                                                    
         EJECT                                                                  
***********************************************************************         
* BUILD CLUSTER RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING ECD,R5                                                           
BLDM     NTR1  ,                                                                
         LA    R7,MINCLST          CLUSTER AREA                                 
         USING TIMELD,R7                                                        
         SR    R1,R1                                                            
         ICM   R1,1,ECDINP+1                                                    
         BNZ   *+6                                                              
         DC    H'0'                NO DATA                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R7),0(R5)       INPUT DATA TO MINI AREA                      
         TM    TIMIND,TIMIADJ      ADJUST HOURS                                 
         BNO   *+8                                                              
         OI    DTYPE,DTADJ                                                      
         CLI   TIMTTYP,TIMTCR      BILLABLE OR REAL                             
         BH    BLDM5                                                            
         OI    DTYPE,DTBIL         FLAG BILLABLE                                
*                                                                               
BLDM5    SR    R0,R0               R0 = LENGTH OF CLUSTER                       
         IC    R0,TIMLN                                                         
         AR    R7,R0               R4 = NEXT TYPE                               
*                                                                               
BLDM7    SR    R1,R1               ADD TAX DATA                                 
         ICM   R1,1,ECDTAX+1                                                    
         BZ    BLDM9                                                            
         EX    R1,*+4                                                           
         MVC   0(0,R7),ECDTAX                                                   
         OI    DTYPE,DTTAX         FLAG TAXABLE                                 
         LA    R1,1(R1)                                                         
         AR    R0,R1                                                            
         AR    R7,R1                                                            
*                                                                               
BLDM9    SR    R1,R1               ADD NARRATIVE                                
         ICM   R1,1,ECDNAR+1                                                    
         BZ    BLDMX                                                            
         AR    R0,R1               ADD TO MINLEN                                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R7),ECDNAR                                                   
         OI    DTYPE,DTNAR         FLAG NARRATIVE                               
*                                                                               
BLDMX    STH   R0,MINLEN                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PUT RECORD TO OUTPUT TAPE FILE                                      *         
***********************************************************************         
         SPACE 1                                                                
PUTR     NTR1  ,                                                                
         CP    RITEM,=P'0'                                                      
         BNE   PUTR03                                                           
         ZAP   BUFEIN,=P'0'                                                     
         BAS   RE,BUFO             ADD TO BUFFALO OUTPUT RECORDS                
         XC    RDA,RDA                                                          
*        CLI   ELSW,C'N'                                                        
*        BE    XIT                                                              
*        MVC   P+1(20),=CL20'NO RECORD CHANGES'                                 
*        GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         MVI   ELSW,C'N'                                                        
         B     XIT                                                              
*                                                                               
PUTR03   L     R4,AIO1                                                          
         USING TIMRECD,R4                                                       
         CLI   TIMRFST,0                                                        
         BE    XIT                 NO RECORD                                    
         BAS   RE,BUFO             ADD TO BUFFALO OUTPUT RECORDS                
         MVI   ELSW,C'N'                                                        
         SR    R3,R3                                                            
         ICM   R3,3,TIMRLEN                                                     
         LA    RF,0(R3,R4)         RF=EOR                                       
         MVI   0(RF),0             MARK EOR                                     
         LA    R3,1(R3)                                                         
         STCM  R3,3,TIMRLEN                                                     
         BAS   RE,PUTTAPE          PUT TO TAPE                                  
*                                                                               
         CLI   QOPT7,C'F'          WRITE TMS RECORDS DIRECTLY TO FILE?          
         BNE   XIT                                                              
         OC    RDA,RDA             ANY RECORDS ON FILE                          
         BNZ   PUTR05                                                           
         L     R2,AIO1                                                          
         BAS   RE,ADD              ADD THE RECORD                               
         B     XIT                                                              
*                                                                               
PUTR05   LA    R3,RDA                                                           
         SR    R0,R0                                                            
         IC    R0,NDA              MAX NUMBER OF RECORDS                        
PUTR07   OC    0(4,R3),0(R3)                                                    
         BNZ   PUTR09                                                           
         LA    R3,4(R3)                                                         
         BCT   R0,PUTR07                                                        
         DC    H'0'                OUT OF LUCK                                  
*                                                                               
PUTR09   MVC   DA,0(R3)                                                         
         XC    0(4,R3),0(R3)       CLEAR USED RECORD                            
         L     R2,AIO2             GET INTO IO2                                 
         BAS   RE,GET                                                           
         CLC   TKEY,0(R2)          TEST PER/CONTRA/DATE                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         BAS   RE,PUT              PUT NEW RECORD                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELETE UNUSED RECORDS                                               *         
***********************************************************************         
         SPACE 1                                                                
DELR     NTR1  ,                                                                
         OC    RDA,RDA                                                          
         BZ    XIT                                                              
         LA    R3,RDA                                                           
         SR    R5,R5                                                            
         IC    R5,NDA                                                           
DELR03   OC    0(4,R3),0(R3)                                                    
         BNZ   DELR05                                                           
DELR04   LA    R3,4(R3)                                                         
         BCT   R5,DELR03                                                        
         B     XIT                                                              
*                                                                               
DELR05   MVC   DA,0(R3)                                                         
         L     R2,AIO2                                                          
         BAS   RE,GET                                                           
         OI    TIMRSTA-TIMRECD(R2),TRNSDELT                                     
         BAS   RE,PUT                                                           
         BAS   RE,DMPDEL                                                        
         MVC   DKEY,0(R2)                                                       
         BAS   RE,READ                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DIR                                                           
         OI    TIMKSTA-TIMRECD(R2),TRNSDELT                                     
         BAS   RE,WRT                                                           
         AP    RDEL,=P'1'                                                       
         XC    0(4,R3),0(R3)                                                    
         B     DELR04                                                           
         EJECT                                                                  
***********************************************************************         
* FIX THE OLD STYLE 1N CONTRAS HEADERS AND BUCKETS  --PUT TO TAPE     *         
***********************************************************************         
         SPACE 1                                                                
         USING CHDRECD,R2                                                       
FIXCON   NTR1  ,                                                                
         L     R2,AIO1                                                          
         BAS   RE,GET              GET THE RECORD                               
         BAS   RE,NEW1N            REMOVE AN= FORM 1N CONTRA                    
         OC    CHDKNULL,CHDKNULL   FIX X'43' ON CONTRA HEADERS                  
         BNZ   FIXC06                                                           
         LA    R1,CHDRFST                                                       
         USING CACELD,R1                                                        
FIXC02   CLI   CACEL,0             END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CACEL,CACELQ                                                     
         BE    FIXC04                                                           
         SR    R0,R0                                                            
         IC    R0,CACLN                                                         
         AR    R1,R0                                                            
         B     FIXC02                                                           
FIXC04   MVC   CACCNTA,CHDKCACT                                                 
*                                                                               
FIXC06   LR    R4,R2                                                            
         BAS   RE,PUTTAPE          PUT TO TAPE                                  
         B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* MAKE NEW STLYE 1N CONTRA FROM OLD STYLE                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CHDRECD,R2                                                       
NEW1N    ST    RE,SVRE                                                          
         L     R2,AIO1                                                          
         CLI   CHDKCACT+1,C' '     MAKE SURE OLD STYLE                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK,SPACES         REMOVE ANALYSIS= FROM 1N CONTRA              
         MVC   WORK(10),CHDKCACT+2                                              
         MVC   CHDKCACT,WORK                                                    
         L     RE,SVRE                                                          
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PUT RECORD TO TAPE  R4 IS ADDR OF RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R4                                                       
PUTTAPE  NTR1  ,                                                                
         SR    R1,R1                                                            
         ICM   R1,3,ACCRLEN                                                     
         LR    RF,R4                                                            
         SH    RF,=H'4'            SET TAPE RECORD LENGTH                       
         XC    0(4,RF),0(RF)                                                    
         AH    R1,=H'4'                                                         
         STCM  R1,3,0(RF)                                                       
         L     R2,ATAPEOUT                                                      
         PUT   (R2),(RF)           PUT OUT ACCFILE RECORD TO TAPE               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD JOB TRANSACTIONS TO BUFFALO                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TMSD,R5                                                          
BUFJ     NTR1  ,                                                                
         LA    R7,TMSINP                                                        
         USING TIMELD,R7                                                        
         OC    CNTRNME,CNTRNME                                                  
         BNZ   BUFJ3                                                            
         OC    CNDA,CNDA                                                        
         BZ    BUFJ3                                                            
         MVC   DA,CNDA                                                          
         L     R2,AIO1                                                          
         BAS   RE,GET              GET CONTRA HEADER                            
         USING CHDRECD,R2                                                       
         LA    R4,CHDRFST                                                       
*                                                                               
         USING CACELD,R4                                                        
         CLI   0(R4),CACELQ        TRANSACTION ELEMENT                          
         BNE   BUFJ3                                                            
         SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         BNP   BUFJ3                                                            
         EX    R1,*+4              BATCH REFERENCE                              
         MVC   CNTRNME(0),CACNAME                                               
*                                                                               
BUFJ3    BAS   RE,BUFCLR           INITIALIZE A BUFFALO RECORD                  
         CLI   TMSXIND,TMSXITAX    TEST TAX DATA                                
         BE    XIT                 DON'T ACCUMULATE TAX DETAIL                  
         MVC   BUFACC,TMSPACC+3    PERSON                                       
         MVC   BUFWKND,TMSWKND     W/E                                          
         MVC   TMSNAME,CNTRNME                                                  
         MVC   BUFNAME,TMSNAME     PERSON NAME                                  
         CLI   BUFNAME,C' '                                                     
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   BUFJ5                                                            
         ZAP   BUFJBH,TIMHRS       BILLABLE HOURS                               
         ZAP   BUFJBC,TIMAMNT      COST                                         
         B     BUFOX                                                            
*                                                                               
BUFJ5    CLI   TIMTTYP,TIMTCN      ADD REAL AND NON-BILLABLE                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZAP   BUFJRH,TIMHRS       REAL HOURS                                   
         B     BUFOX                                                            
         EJECT                                                                  
***********************************************************************         
* ADD PERSON TRANSACTIONS TO BUFFALO                                  *         
***********************************************************************         
         SPACE 1                                                                
BUFP     NTR1  ,                                                                
         LA    R7,TMSINP                                                        
         USING TIMELD,R7                                                        
         BAS   RE,BUFCLR           INITIALIZE A BUFFALO RECORD                  
         MVC   BUFACC,TMSPACC+3    PERSON                                       
         MVC   BUFWKND,TMSWKND     W/E                                          
         MVC   BUFNAME,TMSNAME     PERSON NAME                                  
         CLI   BUFNAME,C' '                                                     
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   BUFP3                                                            
         ZAP   BUFPBC,TIMAMNT      BILLABLE COST                                
         ZAP   BUFPBH,TIMHRS       AND HOURS                                    
         B     BUFOX                                                            
*                                                                               
BUFP3    LA    R1,BUFPRH           REAL HOURS AND CLIENT                        
         CLI   TIMTTYP,TIMTCN                                                   
         BNH   *+8                                                              
         LA    R1,BUFPOH           OTHER HOURS                                  
         ZAP   0(8,R1),TIMHRS                                                   
         B     BUFOX                                                            
         EJECT                                                                  
***********************************************************************         
* ADD OUTPUT RECORDS TO BUFFALO ACCUMS                                *         
***********************************************************************         
         SPACE 1                                                                
BUFO     NTR1  ,                                                                
         BAS   RE,BUFCLR           INITIALIZE A BUFFALO RECORD                  
         L     R5,ATMSIO                                                        
         LA    R7,TMSINP                                                        
         USING TIMELD,R7                                                        
         MVC   BUFACC,TMSPACC+3    PERSON                                       
         MVC   BUFNAME,TMSNAME                                                  
         CLI   BUFNAME,C' '                                                     
         BH    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1                                                          
         USING TIMRECD,R4                                                       
         MVC   BUFWKND,TIMKPEDT                                                 
         LA    R7,TIMRFST                                                       
         SR    R0,R0                                                            
*                                                                               
BUFO3    CLI   0(R7),TIMELQ        TEST POSTING ELEMENT                         
         BNE   BUFO9                                                            
         CLI   TIMETYP,TIMEINP                                                  
         BNE   BUFO9                                                            
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   BUFO5                                                            
         AP    BUFTBH,TIMHRS       BILLABLE HOURS                               
         AP    BUFTBC,TIMAMNT      BILLABLE COST                                
         B     BUFO9                                                            
*                                                                               
BUFO5    LA    R1,BUFTRH           REAL HOURS AND CLIENT                        
         CLI   TIMTTYP,TIMTCN                                                   
         BNH   *+8                                                              
         LA    R1,BUFTOH           OTHER HOURS                                  
         AP    0(8,R1),TIMHRS                                                   
*                                                                               
BUFO9    ICM   R0,1,TIMLN          GET NEXT ELEMENT                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R7,R0                                                            
         CLI   0(R7),0             TEST EOR                                     
         BNE   BUFO3                                                            
*                                                                               
BUFOX    CLI   QOPT3,C'W'          REPORT BY WEEK                               
         BE    *+10                                                             
         XC    BUFWKND,BUFWKND                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         ZAP   BUFEIN,=P'0'                                                     
         XC    BUFKEY,BUFKEY                                                    
         MVI   BUFKEY,X'FF'        TOTAL                                        
         BASR  RE,RF                                                            
         B     XIT                                                              
*                                                                               
BUFCLR   LA    R1,BUFACUM          CLEAR BUFFALO ACCUMS                         
         LA    R0,11                                                            
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ CALENDAR RECORD - BUILD WEEK ENDING TABLE                      *         
***********************************************************************         
         SPACE 1                                                                
CLNDR    NTR1  ,                                                                
         LA    R2,DKEY                                                          
         USING CASRECD,R2                                                       
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,RCCOMPFL    COMPANY                                      
         BAS   RE,HIGH                                                          
         CLC   DKEY(CASKEMOA-CASKEY),DIR                                        
         BE    *+12                                                             
         BAS   RE,WKND             BUILD LIST OF WEEK ENDING DATES              
         B     XIT                                                              
*                                                                               
         USING CALNDRD,R6                                                       
         L     R6,ACALTAB          R6 = A(CALENDAR TABLE)                       
         MVI   0(R6),X'FF'                                                      
         SR    R3,R3               R3 = NUMBER OF TABLE ENTRIES                 
*                                                                               
CLNDR3   L     R2,AIO1                                                          
         BAS   RE,GET              GET THE CALENDAR RECORD                      
         CLI   CASKOFC,C' '                                                     
         BH    CLNDR15             SKIP OFFICE RECORDS                          
         LA    R4,CASRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING TMPELD,R4                                                        
CLNDR5   CLI   0(R4),TMPELQ        X'88' ELEMENT                                
         BE    CLNDR9                                                           
         CLI   0(R4),0                                                          
         BE    CLNDR15             EOR                                          
CLNDR7   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CLNDR5                                                           
*                                                                               
CLNDR9   CLC   TMPSTART(1),STDATE                                               
         BL    CLNDR7                                                           
         CLC   TMPEND(1),ENDDATE                                                
         BH    CLNDR7                                                           
         MVC   CALMTH,TMPMTH       MONTH                                        
         MVC   CALSTART,TMPSTART   PERIOD START DATE                            
         MVC   CALEND,TMPEND       PERIOD END DATE                              
         SR    R1,R1                                                            
         ICM   R1,7,CALEND         SET END DATE (COMPLIMENT)                    
         LNR   R1,R1                                                            
         STCM  R1,7,CALENDC                                                     
         LA    R6,CALLNQ(R6)       BUMP TO NEXT AVAILABLE ENTRY                 
         MVI   0(R6),X'FF'         MARK NEW END OF TABLE                        
         LA    R3,1(R3)            BUMP COUNT                                   
         CH    R3,=Y(CALMAX)                                                    
         BL    CLNDR7                                                           
         DC    H'0'                CALENDAR TABLE FULL                          
*                                                                               
CLNDR15  BAS   RE,SEQ              GET NEXT CALENDAR RECORD                     
         CLC   DKEY(CASKEMOA-CASKEY),DIR                                        
         BE    CLNDR3                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF WEEK ENDING DATES                                     *         
***********************************************************************         
         SPACE 1                                                                
WKND     NTR1  ,                                                                
         USING CALNDRD,R6                                                       
         L     R6,ACALTAB          R6 = A(CALENDAR TABLE)                       
         MVI   0(R6),X'FF'                                                      
         SR    R3,R3               R3 = NUMBER OF TABLE ENTRIES                 
         MVC   WORK(6),QSTART                                                   
*                                                                               
WKND3    GOTO1 DATCON,DMCB,(0,WORK),(1,CALSTART)                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'6'                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,CALEND)                                
         SR    R1,R1                                                            
         ICM   R1,7,CALEND         SET END DATE (COMPLIMENT)                    
         LNR   R1,R1                                                            
         STCM  R1,7,CALENDC                                                     
         LA    R6,CALLNQ(R6)       BUMP TO NEXT AVAILABLE ENTRY                 
         MVI   0(R6),X'FF'         MARK NEW END OF TABLE                        
         LA    R3,1(R3)            BUMP COUNT                                   
         CH    R3,=Y(CALMAX)                                                    
         BL    *+6                                                              
         DC    H'0'                CALENDAR TABLE FULL                          
         GOTO1 ADDAY,DMCB,WORK+6,WORK,F'1'                                      
         CLC   WORK(6),QEND                                                     
         BNH   WKND3                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET WEEK ENDING DATE FROM CALENDAR TABLE                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CALNDRD,R6                                                       
WKDT     L     R6,ACALTAB          R6 = A(CALENDAR TABLE)                       
*                                                                               
WKDT3    CLC   TMSTDTE,CALSTART    GET WEEKENDING DATE                          
         BNL   *+6                                                              
         DC    H'0'                BEFORE CALENDAR START                        
         CLC   TMSTDTE,CALEND                                                   
         BNH   WKDT5                                                            
         LA    R6,CALLNQ(R6)       NEXT PAIR OF START/END                       
         B     WKDT3                                                            
*                                                                               
WKDT5    MVC   TMSWKND,CALEND      SET END DATE                                 
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET CLIENT/PRODUCT OFFICE                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
GETOFC   NTR1  ,                                                                
         LA    R3,JOBOFC           CLEAR JOB LEVEL                              
         MVC   0(16,R3),SPACES                                                  
         CLI   ACTKACT+6,C' '      TEST JOB RECORD                              
         BH    *+14                                                             
         LA    R3,PRDOFC           CLEAR PRODUCT                                
         MVC   0(16,R3),SPACES                                                  
         CLI   ACTKACT+3,C' '      TEST PRODUCT RECORD                          
         BH    *+14                                                             
         LA    R3,CLIOFC           CLEAR CLIENT                                 
         MVC   0(16,R3),SPACES                                                  
         L     R2,AIO1                                                          
         BAS   RE,GET              GET JOB TRANSACTION                          
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING PPRELD,R4                                                        
GETOFC3  CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),PPRELQ        PRODUCTION PROFILE                           
         BE    GETOFC5                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETOFC3                                                          
*                                                                               
GETOFC5  MVC   0(L'CLIOFC,R3),PPRGAOFF   GET OFFICE CODE                        
         MVC   2(L'CLICOST,R3),PPRCOST   AND COSTING CODE                       
         CLI   JOBOFC,C' '         USE JOB                                      
         BH    *+10                                                             
         MVC   JOBOFC,PRDOFC       OR PRODUCT                                   
         CLI   JOBOFC,C' '                                                      
         BH    *+10                                                             
         MVC   JOBOFC,CLIOFC       OR CLIENT                                    
*                                                                               
         CLI   JOBCOST,C' '                                                     
         BH    *+10                                                             
         MVC   JOBCOST,PRDCOST                                                  
         CLI   JOBCOST,C' '                                                     
         BH    *+10                                                             
         MVC   JOBCOST,CLICOST                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
RPT      NTR1  ,                                                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
*                                                                               
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7         R7=ADDRESSES WIDE PRINT                      
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         MVI   RCSUBPRG,0          SET CORRECT HEADINGS                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         XC    BUFKEY,BUFKEY                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFREC,1                             
         CLI   8(R1),X'80'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
RPT3     MVC   PLEIN,SPACES                                                     
         MVC   PLEOT,SPACES                                                     
         CLI   QOPT1,C'*'                                                       
         BNE   RPT4                                                             
         ZAP   BUFEIN,=P'0'                                                     
         CLC   BUFPBC(16),BUFJBC   PERSON COST & HOURS VS. JOB                  
         BE    *+8                                                              
         MVI   PLEOT,C'E'                                                       
         CLC   BUFJBC(16),BUFTBC   JOB COST & HOURS VS. OUTPUT                  
         BE    *+8                                                              
         MVI   PLEOT,C'E'                                                       
         CLC   BUFPRH(16),BUFTRH   PERSON REAL & OTHER VS. OUTPUT               
         BE    *+8                                                              
         MVI   PLEOT,C'E'                                                       
         B     RPT7                                                             
*                                                                               
RPT4     CP    BUFEIN,=P'0'        TEST ERRORS                                  
         BE    *+8                                                              
         MVI   PLEOT,C'E'                                                       
         CLC   BUFPBC(24),BUFJBC   PERSON VS JOB TOTALS                         
         BE    *+8                                                              
         MVI   PLEOT,C'S'                                                       
         CLI   QOPT1,C' '          PRINT EVERYTHING                             
         BE    RPT5                                                             
         CLI   PLEOT,C' '                                                       
         BE    RPT11               NO ERRORS                                    
         CLI   QOPT1,C'E'          ALL ERRORS                                   
         BE    RPT5                                                             
         CLI   PLEOT,C'E'          SKIP NON-CRITICAL ERRORS                     
         BE    RPT11                                                            
*                                                                               
RPT5     EDIT  BUFEIN,(4,PLEIN)    ERRORS IN                                    
         ZAP   BUFEIN,=P'0'                                                     
*                                                                               
RPT7     MVC   PLACC,BUFACC        ACCOUNT CODE                                 
         OC    PLACC,XSPACES                                                    
         MVC   PLNME,BUFNAME       NAME                                         
         OC    PLNME,XSPACES                                                    
         OC    BUFWKND,BUFWKND                                                  
         BZ    RPT9                                                             
         GOTO1 DATCON,DMCB,(1,BUFWKND),(5,PLEND)                                
*                                                                               
RPT9     CLI   BUFKEY,X'FF'                                                     
         BNE   *+10                                                             
         MVC   PLNME+6(9),=C'**TOTAL**'                                         
         LA    R4,BUFPBC           PERSON ACCUMS                                
         LA    R3,4                                                             
         LA    R5,PLPER                                                         
         BAS   RE,EDIT                                                          
         LA    R4,BUFJBC           JOB ACCUMS                                   
         LA    R3,3                                                             
         LA    R5,PLJOB                                                         
         BAS   RE,EDIT                                                          
         LA    R4,BUFTBC           OUTPUT ACCUMS                                
         LA    R3,4                                                             
         LA    R5,PLOUT                                                         
         BAS   RE,EDIT                                                          
         GOTO1 ACREPORT                                                         
*                                                                               
RPT11    GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFREC,1                              
         CLI   8(R1),X'80'                                                      
         BE    XIT                                                              
         CLI   BUFKEY,X'FF'                                                     
         BE    RPT9                                                             
         B     RPT3                                                             
*                                                                               
EDIT     CLI   BUFKEY,X'FF'        TEST TOTALS                                  
         BE    EDITT                                                            
         EDIT  (P8,(R4)),(11,0(R5)),2                                           
         LA    R4,8(R4)                                                         
         LA    R5,12(R5)                                                        
         BCTR  R3,0                                                             
EDIT3    EDIT  (P8,(R4)),(8,0(R5)),2                                            
         LA    R4,8(R4)                                                         
         LA    R5,9(R5)                                                         
         BCT   R3,EDIT3                                                         
         BR    RE                                                               
*                                                                               
EDITT    EDIT  (P8,(R4)),(11,0(R5)),2                                           
         LA    R4,8(R4)                                                         
         LA    R5,10(R5)                                                        
         LA    R2,L'XP(R5)                                                      
         BCTR  R3,0                                                             
EDITT3   EDIT  (P8,(R4)),(10,0(R2)),2                                           
         LA    R4,8(R4)                                                         
         LA    R5,9(R5)                                                         
         LA    R2,9(R2)                                                         
         XR    R5,R2                                                            
         XR    R2,R5                                                            
         XR    R5,R2                                                            
         BCT   R3,EDITT3                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUMMARY                                                             *         
***********************************************************************         
         SPACE 1                                                                
SUM      NTR1  ,                                                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'                                                 
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         LA    R3,RCNTS                                                         
         B     *+8                                                              
SUM3     LA    R3,L'RCNTS(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         MVC   XP+1(16),4(R3)                                                   
         EDIT  (P4,0(R3)),(7,XP+20)                                             
         GOTO1 ACREPORT                                                         
         B     SUM3                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R2),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    RADD,=P'1'                                                       
         BAS   RE,DMPADD                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    RPUT,=P'1'                                                       
         BAS   RE,DMPPUT                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AN IO AREA                                               *         
*  R5 = A(IO AREA)                                                              
***********************************************************************         
         SPACE 1                                                                
CLRTMS   NTR1  ,                                                                
         LR    R0,R5                                                            
         LA    R1,TMSRLNQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ZAP   TMSXBASE,=P'0'      TAX BASIS                                    
         LA    R7,TMSINP                                                        
         USING TIMELD,R7                                                        
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMILN2Q      LENGTH FOR BILLABLE                          
         MVI   TIMSEQ,0                                                         
         MVC   TIMINC,SPACES                                                    
         MVI   TIMETYP,TIMEINP     INPUT DETAILS                                
         ZAP   TIMAMNT,=P'0'       AMOUNT                                       
         ZAP   TIMHRS,=P'0'        HOURS                                        
         ZAP   TIMRATE,=P'0'       RATE                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DMPTMS   NTR1  ,                    TMS RECORD                                  
         LA    R0,L'TMSMSG                                                      
         LA    R2,TMSMSG                                                        
         L     R3,ASRTIO                                                        
         LA    R4,TMSRLNQ                                                       
         B     DMPX                                                             
*                                                                               
DMPTXE   CP    DMPECNT,DMPEMAX     TAX ERROR                                    
         BHR   RE                                                               
         AP    DMPECNT,=P'1'                                                    
         NTR1  ,                    TAX ERROR                                   
         LA    R0,L'TAXEMSG                                                     
         LA    R2,TAXEMSG                                                       
         L     R3,ATAXIO                                                        
         LA    R4,TMSRLNQ                                                       
         B     DMPX                                                             
         SPACE 1                                                                
*                                                                               
DMPT     NTR1  ,                                                                
         LA    R1,DMPMSG                                                        
DMPT1    CP    1(2,R1),DMPTMAX     PASSED MAX COUNT                             
         BH    DMPT3                                                            
         CLI   0(R1),X'FF'         EOT                                          
         BE    DMPT5                                                            
         SR    RF,RF                                                            
         IC    RF,0(R1)            DATA TYPE                                    
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    DTYPE,0             TEST DATA TYPE                               
         BO    DMPT5                                                            
*                                                                               
DMPT3    CLI   0(R1),X'FF'         EOT                                          
         BE    XIT                                                              
         LA    R1,23(R1)                                                        
         B     DMPT1                                                            
*                                                                               
DMPT5    AP    1(2,R1),=P'1'                                                    
         LA    R0,20                                                            
         LA    R2,3(R1)                                                         
         L     R3,AIO1             IO AREA                                      
         SH    R3,=H'4'            LESS 4 FOR LENGTH                            
         SR    R4,R4                                                            
         ICM   R4,3,0(R3)                                                       
*                                                                               
DMPX     LA    R5,=C'2D'                                                        
         GOTO1 ,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)            
         GOTO1 PRNTBL                                                           
         B     XIT                                                              
         EJECT                                                                  
DMPGET   NTR1  ,                                                                
         LA    R2,=CL20'GET RECORD'                                             
         L     R3,AIO2             IO AREA                                      
         SR    R4,R4                                                            
         ICM   R4,3,TIMRLEN-TIMRECD(R3)                                         
         LA    R5,=C'2D'                                                        
         GOTO1 ,DMCB,(20,(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)              
         GOTO1 PRNTBL                                                           
         B     XIT                                                              
*                                                                               
DMPDEL   NTR1  ,                                                                
         LA    R2,=CL20'DELETE RECORD'                                          
         B     DMPPUT3            IO AREA                                       
*                                                                               
DMPADD   NTR1  ,                                                                
         LA    R2,=CL20'ADD RECORD'                                             
         B     DMPPUT3            IO AREA                                       
*                                                                               
DMPPUT   NTR1  ,                                                                
         LA    R2,=CL20'PUT RECORD'                                             
DMPPUT3  AP    PITEM,RITEM         ADD RECORD TOTALS TO PERSON TOTALS           
         AP    PHOUR,RHOUR                                                      
         AP    PAMNT,RAMNT                                                      
         CP    RITEM,=P'0'         RECORD CHANGES                               
         BE    XIT                                                              
         CLI   QOPT2,C'T'                                                       
         BNE   XIT                                                              
         MVC   P+1(14),=C'RECORD CHANGES'                                       
         MVC   P+21(6),=C'ITEMS='                                               
         EDIT  RITEM,(3,P+27),ALIGN=LEFT                                        
         MVC   P+31(6),=C'HOURS='                                               
         EDIT  RHOUR,(7,P+37),2,MINUS=YES,ALIGN=LEFT                            
         MVC   P+51(6),=C'AMNTS='                                               
         EDIT  RAMNT,(7,P+57),2,MINUS=YES,ALIGN=LEFT                            
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
*                                                                               
         L     R3,AIO1             IO AREA                                      
         SR    R4,R4                                                            
         ICM   R4,3,TIMRLEN-TIMRECD(R3)                                         
         LA    R5,=C'2D'                                                        
         GOTO1 ,DMCB,(20,(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)              
         GOTO1 PRNTBL                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* TRACE ELEMENT ADDS/DELETES                                          *         
***********************************************************************         
         SPACE 1                                                                
DMPEL    NTR1  ,                   DUMP ELEMENT DATA                            
         CLI   TIMETYP,TIMEINP                                                  
         BNE   DMPEL3                                                           
         AP    RITEM,=P'1'                                                      
         AP    BUFEIN,=P'1'        ADD TO PERSON ACTIVITY                       
         ZAP   DUB,TIMHRS                                                       
         CLI   TIMEL,TIMELQ                                                     
         BE    *+10                                                             
         MP    DUB,=P'-1'          DELETE HOURS                                 
         AP    RHOUR,DUB                                                        
         CLI   TIMLN,TIMILN2Q                                                   
         BL    DMPEL3                                                           
         ZAP   DUB,TIMAMNT                                                      
         CLI   TIMEL,TIMELQ                                                     
         BE    *+10                                                             
         MP    DUB,=P'-1'          DELETE COST                                  
         AP    RAMNT,DUB                                                        
*                                                                               
DMPEL3   CLI   QOPT2,C'T'                                                       
         BNE   XIT                                                              
         SR    R4,R4                                                            
         IC    R4,1(R7)                                                         
         LA    R5,=C'2D'                                                        
         GOTO1 ,DMCB,0,(R7),C'DUMP',(R4),(R5),(C'P',PRINT)                      
         GOTO1 PRNTBL                                                           
         MVI   ELSW,C'Y'                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* TRACE PERSON TOTALS                                                 *         
***********************************************************************         
         SPACE 1                                                                
PTOTS    CLI   QOPT2,C'T'                                                       
         BNER  RE                                                               
         CP    PITEM,=P'0'                                                      
         BER   RE                                                               
         NTR1  ,                   PERSON TOTALS                                
         MVC   P+1(14),=C'PERSON CHANGES'                                       
         MVC   P+21(6),=C'ITEMS='                                               
         EDIT  PITEM,(3,P+27),ALIGN=LEFT                                        
         MVC   P+31(6),=C'HOURS='                                               
         EDIT  PHOUR,(7,P+37),2,MINUS=YES,ALIGN=LEFT                            
         MVC   P+51(6),=C'AMNTS='                                               
         EDIT  PAMNT,(7,P+57),2,MINUS=YES,ALIGN=LEFT                            
         MVC   P+70(14),1(R3)      PERSON CODE                                  
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         ZAP   PITEM,=P'0'         ITEMS                                        
         ZAP   PHOUR,=P'0'         HOURS                                        
         ZAP   PAMNT,=P'0'         DOLLARS                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
*                                                                               
HELLO    DC    V(HELLO)            HELLO                                        
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
*                                                                               
ABUFF    DC    A(BUFFALOC)                                                      
ANEWC    DC    A(NEWC)             NEW ELEMENT CLUSTER                          
AOLDC    DC    A(OLDC)             OLD ELEMENT CLUSTER                          
AIO1     DC    A(IO1)              IO AREA #1                                   
AIO2     DC    A(IO2)              IO AREA #2                                   
AIO1R    DC    A(IO3)              IO AREA #3 SAVED 1R ACCOUNT RECORD           
ASRTIO   DC    A(SRTIO)            SORT IO AREA                                 
ATAXIO   DC    A(TAXIO)            TAX IO AREA                                  
ATMSIO   DC    A(TMSIO)            TIME RECORD IO                               
ACALTAB  DC    A(CALTAB)           A(CALENDAR TABLE)                            
AEMPTAB  DC    A(EMPTAB)           A(EMPLOYEE TABLE)                            
ATAPEOUT DC    A(TAPEOUT)          A(OUTPUT TAPE DCB)                           
ATMSWRK  DC    A(TMSWRK)           A(WORK FILE DCB)                             
*                                                                               
EFFS     DC    48X'FF'                                                          
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
*                                                                               
PRODUL   DC    C'SJ'                                                            
PERSUL   DC    C'1R'                                                            
COSTUL   DC    C'1C'                                                            
NONCUL   DC    C'1N'                                                            
INCMUL   DC    C'SI'                                                            
SUSPUL   DC    C'SK'                                                            
ANALUL   DC    C'12'                                                            
PCNTUL   DC    C'1J'                                                            
TMSDUMEL DS    0CL5                                                             
TMSDEL   DC    X'01'                                                            
TMSDLEN  DC    X'05'                                                            
TMSDCOD  DC    CL3'TMS'                                                         
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=999 '                                     
SORTCARD DC    C'SORT FIELDS=(999,999,A),FORMAT=BI,WORK=1 '                     
*                                                                               
TAXEMSG  DC    C'UNMATCHED TAX ITEM'                                            
DMPECNT  DC    PL2'0'              DUMP COUNT - ERRORS                          
DMPEMAX  DC    PL2'10'             MAX DUMP COUNT                               
*                                                                               
DMPMSG   DC    AL1(DTTAX),PL2'0',CL20'TAX DATA'                                 
         DC    AL1(DTBIL),PL2'0',CL20'BILLABLE DATA'                            
         DC    AL1(DTMLT),PL2'0',CL20'MULTI-RECORD DATA'                        
         DC    AL1(DTNAR),PL2'0',CL20'NARRATIVE'                                
         DC    AL1(DTADJ),PL2'0',CL20'ADJUSTED HOURS'                           
         DC    X'FF',PL2'0',CL20'ALL OTHER'                                     
DMPTMAX  DC    PL2'10'             MAX DUMP COUNT                               
*                                                                               
TMSMSG   DC    C'TIME WORK RECORD'                                              
*                                                                               
RCNTS    DS    0XL20                                                            
RADD     DC    PL4'0',CL16'RECORDS ADDED'                                       
RPUT     DC    PL4'0',CL16'RECORDS CHANGED'                                     
RDEL     DC    PL4'0',CL16'RECORDS DELETED'                                     
RINP     DC    PL4'0',CL16'INPUT 1R RECORDS'                                    
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFALO CSECT AND DCB'S                                             *         
***********************************************************************         
         SPACE 1                                                                
         BUFF  FLAVOR=PACKED,                                          X        
               KEYLIST=(15,A),                                         X        
               COMMENT=36,                                             X        
               LINES=100,                                              X        
               COLUMNS=12,                                             X        
               ROWS=1                                                           
         SPACE 2                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
*                                                                               
TMSWRK   DCB   DDNAME=TMSWRK,DSORG=PS,MACRF=(PM,GM),                   X        
               RECFM=FB,LRECL=10,BLKSIZE=100,EODAD=REQL5                        
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         CLI   RCSUBPRG,1          SET CORRECT HEADINGS                         
         BE    BX100                                                            
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+(PLCL-PLD),C'L'                                          
         MVI   BOXCOLS+(PLC1-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC2-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC3-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC4-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC5-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC6-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC7-PLD),C'C'                                          
         MVI   BOXCOLS+(PLCR-PLD),C'R'                                          
         B     BX200                                                            
*                                                                               
BX100    MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+14,C'C'                                                  
         MVI   BOXCOLS+29,C'C'                                                  
         MVI   BOXCOLS+44,C'R'                                                  
*                                                                               
BX200    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DC    C'**NEW***'          NEW CLUSTER AREA                            
NEWC     DC    (MAXEC*ECDLNQ)X'00'                                              
MAXEC    EQU   500                 MAXIMUM ELEMENT CLUSTERS                     
*                                                                               
         DC    C'**OLD***'          OLD CLUSTER AREA                            
OLDC     DC    (MAXEC*ECDLNQ)X'00'                                              
*                                                                               
         DC    C'**IO1***'                                                      
         DC    F'0'                      IOAREA #1                              
IO1      DC    (MAXRLNQ)X'00'                                                   
MAXRLNQ  EQU   1990                INSTEAD OF 2000 TO BE SAFE                   
*                                                                               
         SPACE 1                                                                
         DC    C'**IO2***'                                                      
         DC    F'0'                      IOAREA #2                              
IO2      DC    (MAXRLNQ)X'00'                                                   
*                                                                               
         SPACE 1                                                                
         DC    C'**IO3***'                                                      
         DC    F'0'                      IOAREA #3                              
IO3      DC    (MAXRLNQ)X'00'                                                   
*                                                                               
         DS    0D                        SORT RECORD AREA                       
         DC    C'*SRTIO**'                                                      
SRTIO    DC    (TMSRLNQ)X'00'                                                   
*                                                                               
         DS    0D                        TIME WORK RECORD                       
         DC    C'*TMSIO**'                                                      
TMSIO    DC    (TMSRLNQ)X'00'                                                   
*                                                                               
         DS    0D                        TAX RECORD AREA                        
         DC    C'*TAXIO**'                                                      
TAXIO    DC    (TMSRLNQ)X'00'                                                   
*                                                                               
         DS    0D                                                               
         DC    C'*CALTAB*'                                                      
CALTAB   DC    (CALMAX*CALLNQ)X'00'      CALENDAR TABLE                         
CALMAX   EQU   216                       MAX NUMBER ENTRIES IN TABLE            
*                                                                               
         DS    0D                                                               
         DC    C'*EMPTAB*'                                                      
EMPTAB   DC    (EMPMAX*EMPDLNQ)X'00'     1R EMPLOYEE TABLE                      
EMPMAX   EQU   1500                      MAX NUMBER ENTRIES IN TABLE            
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
*                                                                               
ACTMD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
AEMPLAST DS    A                   ADDRESS OF MAX LAST EMP ENTRY                
AEMPCURR DS    A                   ADDRESS OF CURRENT EMP ENTRY                 
SVRE     DS    F                   SAVE RE                                      
*                                                                               
ODS      DS    CL8                 OFFICE/DEPT/SUB                              
PERCDE   DS    CL8                 PERSON CODE                                  
PERACC   DS    CL15                PERSON ACCOUNT CODE                          
PERNME   DS    CL36                PERSON NAME                                  
TKEY     DS    CL(TIMKREF-TIMRECD) KEY DATA                                     
MAXDA    EQU   2                   MAXIMUM NUMBER OF RECORDS                    
NDA      DS    XL1                 NUMBER OF RECORDS                            
         DS    0F                                                               
RDA      DS    XL(4*MAXDA)         RECORD DISK ADDRESSES                        
*                                                                               
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
SVKEY    DS    CL(L'ACCKEY)        KEY SAVE AREA                                
*                                                                               
CMPNAME  DS    CL36                                                             
STATUS   DS    XL1                 STATUS BYTE                                  
NCOST    EQU   X'80'               COMPANY IS ON NEW COSTING                    
NOFFC    EQU   X'40'               NEW OFFICES                                  
PCSW     DS    XL1                 PROJECT CONTROL SWITCH(SEE TIMSTAT)          
ELSW     DS    XL1                                                              
*                                                                               
SEQN     DS    XL1                 SEQUENCE NUMBER                              
MINLEN   DS    H                   LENGTH OF MINO CLUSTER                       
MINCLST  DS    CL500               MINIO CLUSTER                                
*                                                                               
DTYPE    DS    X                   DATA TYPE                                    
DTTAX    EQU   X'80'               RECORD HAS TAX DATA                          
DTBIL    EQU   X'40'               RECORD HAS BILLABLE DATA                     
DTMLT    EQU   X'20'               DATA REQUIRES MULTIPLE RECORDS               
DTNAR    EQU   X'10'               RECORD HAS NARRATIVE DATA                    
DTADJ    EQU   X'08'               ADJUSTED HOURS                               
*                                                                               
CLIOFC   DS    CL2                 CLIENT OFFICE                                
CLICOST  DS    CL15                CLIENT COSTING ACCOUNT                       
PRDOFC   DS    CL2                 PRODUCT OFFICE                               
PRDCOST  DS    CL15                PRODUCT COSTING ACCOUNT                      
JOBOFC   DS    CL2                 JOB OFFICE                                   
JOBCOST  DS    CL15                JOB COSTING ACCOUNT                          
ACRQLEN  DS    XL1                 LENGTH OF CONTRA FILTER                      
*                                                                               
RITEM    DS    PL3                 RECORD ITEMS ADDED/DELETED                   
RHOUR    DS    PL5                        HOURS ADDED/DELETED                   
RAMNT    DS    PL6                        AMOUNTS ADDED/DELETED                 
PITEM    DS    PL3                 PERSON ITEMS                                 
PHOUR    DS    PL5                                                              
PAMNT    DS    PL6                                                              
*                                                                               
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
STDATE   DS    PL3                 START DATE                                   
ENDDATE  DS    PL3                 END DATE                                     
*                                                                               
CNTRNME  DS    CL36                CONTRA NAME FROM SJ POSTING                  
CNDA     DS    F                                                                
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
         EJECT                                                                  
*              BUFFALO RECORD                                                   
*                                                                               
BUFREC   DS    0C                                                               
BUFKEY   DS    0CL15               *** BUFFALO KEY ***                          
BUFACC   DS    CL12                1R ACCOUNT                                   
BUFWKND  DS    PL3                 WEEK ENDING DATE                             
*                                                                               
BUFDATA  DS    0C                  *** BUFFALO DATA ***                         
BUFNAME  DS    CL36                LAST NAME                                    
*                                                                               
BUFACUM  DS    0PL8                                                             
*                                  INPUT - PERSON                               
BUFPBC   DS    PL8                 BILLABLE COST                                
BUFPBH   DS    PL8                 BILLABLE HOURS                               
BUFPRH   DS    PL8                 REAL HOURS                                   
BUFPOH   DS    PL8                 ALL OTHER HOURS                              
*                                                                               
*                                  INPUT - SJ                                   
BUFJBC   DS    PL8                 BILLABLE COST                                
BUFJBH   DS    PL8                 BILLABLE HOURS                               
BUFJRH   DS    PL8                 REALIZATION HOURS                            
*                                                                               
*                                  OUTPUT - TIME RECORD                         
BUFTBC   DS    PL8                 BILLABLE COST                                
BUFTBH   DS    PL8                 BILLABLE HOURS                               
BUFTRH   DS    PL8                 REAL HOURS                                   
BUFTOH   DS    PL8                 ALL OTHER HOURS                              
BUFEIN   DS    PL8                 INPUT ERRORS                                 
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TIME RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
TMSD     DSECT                                                                  
TMSKEY   DS    0C                  KEY FOR TIMRECD SORT                         
TMSPACC  DS    CL15                PERSON (1R) ACCOUNT                          
TMSCOFC  DS    CL2                 CLIENT OFFICE CODE                           
TMSCOST  DS    CL15                1C COSTING ACCOUNT                           
TMSWKND  DS    PL3                 WEEK ENDING DATE                             
TMSKLNQ  EQU   *-TMSKEY                                                         
*                                                                               
TMSINP   DS    CL(TIMILN2Q)        TIME INPUT DETAIL                            
TMSTAX   DS    CL(TIMTHDQ+TIMTMINQ)  TAX DATA                                   
TMSNARR  DS    CL(TIMTHDQ+L'TIMNARR) NARRARTIVE DATA                            
*                                                                               
TMSSTAT  DS    XL1                 STATUS                                       
TMSSR49  EQU   X'01'               TYPE 49 MARKED AS REVERSED                   
TMSSW57  EQU   X'80'               TYPE 57                                      
TMSTRNT  DS    XL1                 TRANSACTION TYPE                             
*                                                                               
                                                                                
TMSSRC   DS    XL1                 SOURCE                                       
TMSSPER  EQU   0                   PERSON                                       
TMSSJOB  EQU   1                   JOB                                          
*                                                                               
TMSTDTE  DS    PL3                 TRANSACTION DATE                             
TMSTREF  DS    CL6                 TRANSACTION REFERENCE NUMBER                 
TMSTXTSK DS    CL2                 TASK FOR TAX POSTING                         
*                                                                               
TMSPER   DS    CL8                 PERSON                                       
TMSODS   DS    CL8                 OFFICE/DEPT/SUB                              
TMSNAME  DS    CL36                ACCOUNT NAME                                 
*                                                                               
TMSXK    DS    0C                  KEY FOR FIRST SORT TO MATCH TAX DATA         
TMSXCPJ  DS    CL14                CLIENT/PROD/JOB                              
TMSXADTE DS    PL3                 ACTIVITY DATE                                
TMSXMOA  DS    PL2                 MONTH OF SERVICE                             
TMSXBREF DS    CL6                 BATCH REFERENCE                              
TMSXBASE DS    PL6                 BASIS                                        
TMSXCLNQ EQU   *-TMSXK             LENGTH FOR COMPARE                           
TMSXREF  DS    CL6                 REFERENCE                                    
TMSXIND  DS    XL1                 TAX, NON-TAX INDICATOR                       
TMSXITAX EQU   0                   TAX ITEM                                     
TMSXIPER EQU   1                   NON-TAX ITEM                                 
TMSXKLNQ EQU   *-TMSXK             KEY LENGTH FOR TAX SORT                      
TMSRLNQ  EQU   *-TMSD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ELEMENT CLUSTER DATA                                 *         
***********************************************************************         
         SPACE 2                                                                
ECD      DSECT                                                                  
ECDINP   DS    CL(TIMILN2Q)        INPUT DATA                                   
ECDTAX   DS    CL50                TAX DATA                                     
ECDNAR   DS    CL110               NARRATIVE                                    
ECDTYP   DS    XL1                                                              
ECDTR49  EQU   1                   TYPE 49 REVERSAL                             
ECDTW57  EQU   2                   TYPE 57 WRITE OFF                            
ECDSTAT  DS    XL1                 STATUS                                       
ECDSUS   EQU   X'80'               ITEM USED                                    
ECDSMAT  EQU   X'40'               MATCHED  49 OR 57                            
ECDSKEP  EQU   X'08'               KEEP IT                                      
ECDSDEL  EQU   X'04'               DELETE IT                                    
ECDLNQ   EQU   *-ECD                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER CALENDAR ENTRY                                       *         
***********************************************************************         
         SPACE 1                                                                
CALNDRD  DSECT                                                                  
CALSTART DS    PL3                 YYMMDD - WEEK START DATE                     
CALEND   DS    PL3                 YYMMDD - WEEK END DATE                       
CALENDC  DS    PL3                 COMPLIMENT                                   
CALMTH   DS    PL2                 YYMM                                         
CALLNQ   EQU   *-CALNDRD                                                        
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER EMPLOYEE ENTRY                                       *         
***********************************************************************         
         SPACE 1                                                                
EMPD     DSECT                                                                  
EMPCODE  DS    CL12                1R CODE                                      
EMPSTA   DS    XL1                 STATUS                                       
EMPUSED  EQU   X'80'               ADDED TO TAPE                                
EMPDLNQ  EQU   *-EMPD                                                           
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER REPORT PRINT LINE                                    *         
***********************************************************************         
         SPACE 1                                                                
PLD      DSECT                                                                  
PLCL     DS    CL1                                                              
PLACC    DS    CL12                ACCOUNT CODE                                 
PLC1     DS    CL1                                                              
PLNME    DS    CL20                PERSON'S NAME                                
PLC2     DS    CL1                                                              
PLEND    DS    CL8                 WEEK ENDING DATE                             
PLC3     DS    CL1                                                              
PLPER    DS    CL38                PERSON ACCUMS                                
PLC4     DS    CL1                                                              
PLJOB    DS    CL29                JOB ACCUMS                                   
PLC5     DS    CL1                                                              
PLOUT    DS    CL38                OUTPUT ACCUMS                                
PLC6     DS    CL1                                                              
PLEIN    DS    CL4                 ERRORS IN                                    
PLC7     DS    CL1                                                              
         DS    CL1                                                              
PLEOT    DS    CL3                 ERRORS OUT                                   
PLCR     DS    CL1                                                              
         ORG   PLD+164                                                          
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACREPTM02 12/11/09'                                      
         END                                                                    
