*          DATA SET ACREPR302S AT LEVEL 072 AS OF 05/01/02                      
*PHASE ACR302A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE SORTER                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE ACLIST                                                                 
         TITLE 'CLIENT FEE ANALYSIS'                                            
ACR302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR3**,R9                                                    
         L     RA,0(R1)                                                         
*                                                                               
         USING ACWORKD,RA                                                       
********************************************************************            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         MVI   PROGPROF,C'Y'       FORCE SUBDEPT TOTALS                         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
********************************************************************            
         USING ACR3D,RC                                                         
         LA    RC,SPACEND                                                       
********************************************************************            
*        REQUESTED OFF THE 1R LEDGER                                *           
*                                                                   *           
*        READS 1R ACCOUNT RECORD - GETS NAME                        *           
*        READS 1R TRANSACTIONS -                                    *           
*             X'40' - HOURS         -  ACPSHOUR                     *           
*                   - BILLING RATE  -  ACPSRATE                     *           
*             X'44' - TIMESHEET DATE - TRNSDATE                                 
*                   - TRANSACTION OFFICE - TRNSOFFC                             
*             X'51' - IF LEN = X'11' USE CLINT/PROD  - ACPCCLI      *           
*                     IF LEN = X'22' USE CL/PR/JOB   - ACPCPRJ      *           
*        START END MONTHS - MMM/YY  MMM/YY                          *           
*                           READ BY 1R                              *           
*                                                                   *           
*        MOA RANGE          MMM/YY       TIME BILLED FOR MMM/YY ONLY*           
*                           -MMM/YY      TIME BILLED THRU MMM/YY    *           
*                           MMM/YY-MMM/YY BILLED MMM/YY THRU MMM/YY *           
*                           BLANK        TIME BILLED ETERNITY       *           
*                           READ BY TIME BILLED.                    *           
*                                                                   *           
*        OPTION 1           E     - SUPRRES BILLABLE AMT FORMAT A   *           
*                                                                   *           
*        OPTION 3           B- B TIME                                           
*                           R- R TIME                                           
*                           N- N TIME                                           
**********************************************************************          
* PROGRAM DUMPS ARE CAUSED BY THE FOLLOWING:                        *           
*  1) NO NAME ELEMENT                                               *           
*  2) INCORRECT FORMAT OPTION(OPTION 3)                             *           
*  3) BAD READ OR WRITE                                             *           
**********************************************************************          
*  6/4/92   MOVED GETMAIN CALL TO RUNFRST, ADDED SETBUFF            *           
**********************************************************************          
         EJECT                                                                  
*************************                                                       
* ROUTINE FOR RUN FIRST *                                                       
*************************                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
*                                                                               
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
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
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFC                                       
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
*                                                                               
         BAS   RE,GETBUFF          ALLOCATE STORAGE                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**************************************************                              
*              ROUTINE FOR REQUEST FIRST         *                              
**************************************************                              
*                                                                               
REQF     CLI   MODE,REQFRST                                                     
         BNE   FRSTLEVA                                                         
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
         XC    PRTSTAT,PRTSTAT                                                  
         MVC   PAGE,=H'1'                                                       
*                                                                               
         BAS   RE,SETBUFF          CLEAR STORAGE, SET BINSRCH PARMS             
*                                       ** INIT FOR SORT **                     
         XC    ALSORT,ALSORT                 CLEAR A(LAST SORT)                 
         LA    R1,SRTKYLN                    SORT KEY LENGTH                    
         CVD   R1,DUB                        CONVERT KEY LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
*                                                                               
         LA    R1,SRTLNQ                     SORT RECORD LENGTH                 
         CVD   R1,DUB                        CONVERT REC LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
*                                       ** PACK PERIOD DATES **                 
         XC    STRDATE,STRDATE                                                  
         MVC   ENDDATE,=X'FFFFFF'                                               
         CLC   QSTART,SPACES                                                    
         BE    REQF10                                                           
         MVC   WORK(6),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,STRDATE) START=YMD PACKED                
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         MVC   WORK(6),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,ENDDATE) ENDATE=YMD PACKED               
*                                                                               
         USING ACMD,R2                                                          
REQF20   L     R2,AMONACC                                                       
         CLI   ACMMEND,X'FF'          IS THERE A MOS END DATE?                  
         BE    REQF30              NO, SET DATES                                
         MVC   MYMEND(2),ACMMEND                                                
         MVC   MYMSTR(2),ACMFDTE     FISCAL START DATE (FROM MONACC)            
         OC    ACMMSTR,ACMMSTR       DO THEY WANT A MOS START                   
         BZ    *+10                NO                                           
         MVC   MYMSTR(2),ACMMSTR    YES, THIS OVERRIDES FDATE                   
         B     REQF40                                                           
*                                                                               
REQF30   GOTO1 DATCON,DMCB,(4,RCDATE),(1,WORK) PACK TODAY                       
         MVC   MYMSTR,WORK                                                      
         MVI   MYMSTR+1,1          JAN THIS YEAR IS DEFAULT MOS START           
         USING ACCOMPD,R2                                                       
         L     R2,ADCMPEL                                                       
         CLI   ACMPSTM,0           DO THEY HAVE A FISCAL START MON              
         BE    REQF35              NO                                           
         ZIC   R1,ACMPSTM          YES, CONVERT IT TO NUMERIC                   
         N     R1,=F'15'           TURN OFF HI BITS                             
         TM    ACMPSTM,X'F0'       WAS IT F1-F9                                 
         BO    *+8                 YES, DONE                                    
         LA    R1,15(R1)           NO, ADD COMVERSION FACTOR                    
         STC   R1,MYMSTR+1                                                      
*                                                                               
REQF35   MVC   MYMEND,WORK         USE TODAYS YYMM AS END MOS                   
         CLC   MYMEND,MYMSTR       END GREATER THAN START ?                     
         BNL   REQF40              EQUAL IS OK ALSO                             
         MVI   WORK+1,X'0F'                                                     
         SP    WORK(2),=P'10'      SUBTRACT 1 FROM YEAR                         
         MVC   MYMSTR(1),WORK                                                   
*                                                                               
REQF40   MVC   CURMON,MYMEND                                                    
         MVC   WORK,MYMEND                                                      
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,CURMONP) FOR HEADER                      
         MVC   WORK,MYMSTR                                                      
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,FRMMONP) FOR HEADER                      
*                                                                               
         USING RUNXTRAD,R2                                                      
         MVI   LISTSW,C'N'                                                      
         L     R2,VEXTRAS          LOOK FOR CLIENT LIST                         
         ICM   R4,15,VLISTREC                                                   
         BZ    REQF60                                                           
         USING ACLISTD,R4                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                BAD LIST REC                                 
         CLI   ACLITYPE,C'A'       ACCOUNT LIST?                                
         BNE   REQF60              NO FAGETABOUDIT                              
         MVI   LISTSW,C'Y'         MUST CHECK LIST                              
*                                                                               
REQF60   L     R2,AOFFLST          INIT TABLES                                  
         XC    0(2,R2),0(R2)                                                    
         L     R2,ADEPLST                                                       
         XC    0(2,R2),0(R2)                                                    
         L     R2,ASDELST                                                       
         XC    0(2,R2),0(R2)                                                    
         L     R2,AEMPLST                                                       
         XC    0(2,R2),0(R2)                                                    
         USING BIND,R2                                                          
         L     R2,ACLOSTAB                                                      
         XC    BININ,BININ                                                      
*                                                                               
         USING ACCOMPD,R2                                                       
         MVI   NEWOFF,C'N'                                                      
         L     R2,ADCMPEL                                                       
         TM    ACMPSTA4,X'01'                                                   
         BNO   REQFX                                                            
         MVI   NEWOFF,C'Y'                                                      
REQFX    B     XIT                                                              
         EJECT                                                                  
*****************************************                                       
*        SAVE 1R OFFICE CODE/NAME                                               
*****************************************                                       
FRSTLEVA CLI   MODE,LEVAFRST       1R OFFICE                                    
         BNE   FRSTLEVB            NO                                           
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         USING ACKEYACC,R3                                                      
         L     R3,ADHEIRA                                                       
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVANAM                                                      
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME          GET NAME                                     
         L     R3,AOFFLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*****************************************                                       
*        SAVE 1R DEPARTMENT NAME                                                
*****************************************                                       
FRSTLEVB CLI   MODE,LEVBFRST       1R DEP                                       
         BNE   FRSTLEVC            NO                                           
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         USING ACKEYACC,R3                                                      
         L     R3,ADHEIRB                                                       
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVBNAM                                                      
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME          GET NAME                                     
         L     R3,ADEPLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*****************************************                                       
*        SAVE 1R SUB DEPARTMENT NAME                                            
*****************************************                                       
FRSTLEVC CLI   MODE,LEVCFRST       1R SUB DEP                                   
         BNE   PROCAC              NO                                           
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         USING ACKEYACC,R3                                                      
         L     R3,ADHEIRC                                                       
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADLVCNAM                                                      
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME          GET NAME                                     
         L     R3,ASDELST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         B     XIT                                                              
         DROP  R2,R3                                                            
         SPACE 2                                                                
PROCAC   CLI   MODE,PROCACC           IS IT A TRANSACTION?                      
         BNE   ACTUAL                 NO - GET NEXT RECORD                      
         XC    ACCSTAT,ACCSTAT                                                  
         EJECT                                                                  
*****************************************                                       
*  GET DATA FROM TRANSACTION            *                                       
*  OFFICE FROM 44 EL                    *                                       
*  RATE AND HOURS FROM 40 EL            *                                       
*  CLI, PRO AND TASK FROM 50 EL         *                                       
*  IF TRNSDATE < STRDATE OR > END REJECT *                                      
*  IF TRNSDATE > STRDATE OR < END,      *                                       
*  USE FOR YTD FIGURES                  *                                       
*  IF JOB IS FOUND ON THE 51 PUT OUT A  *                                       
*  JOB RECORD W/ WORK DATE AND TASK     *                                       
*****************************************                                       
*                                                                               
ACTUAL   CLI   MODE,PROCTRNS          IS IT A TRANSACTION?                      
         BNE   REQLST                 NO - GET NEXT RECORD                      
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'        THIS IS A 44 EL?                             
         BNE   XIT                 NO, BAD TRANS                                
         CLI   TRNSTYPE,49            IS TRANS TYPE 49?                         
         BNE   XIT                    NO - SKIP                                 
         CLC   TRNSDATE,STRDATE    DATE FILTERING                               
         BL    XIT                                                              
         CLC   TRNSDATE,ENDDATE                                                 
         BH    XIT                                                              
*                                                                               
         TM    TRNSSTAT,X'20'      IS THIS A REVERSAL?                          
         BO    XIT                 YES                                          
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         CLC   ACMMDTE,MYMSTR                                                   
         BL    XIT                                                              
         CLC   ACMMDTE,MYMEND                                                   
         BH    XIT                                                              
*                                                                               
         BAS   RE,OFFCHK           CHECK OFFICE SECURITY                        
         BNE   XIT                 FAILED                                       
*                                                                               
ACTU09   TM    ACCSTAT,GOTACC                                                   
         BO    ACTU10                                                           
         MVI   ACCSTAT,GOTACC                                                   
         BAS   RE,NEWACC                                                        
*                                                                               
ACTU10   LA    R6,SRTREC                                                        
         USING SRTD,R6                                                          
*                                                                               
         MVC   SRTOFF,TRNSOFFC        TRANSACTION OFFICE                        
         CLI   QOPT2,C'*'             FILTER ON SJ OFFICE                       
         BNH   ACTU11                 NO                                        
         CLC   SRTOFF(1),QOPT2        ONE CHARACTER FOR NOW                     
         BNE   XIT                                                              
ACTU11   CLC   QOFFICE,SPACES                                                   
         BE    ACTU12                                                           
         CLC   QOFFICE,TRNSOFFC                                                 
         BNE   XIT                                                              
*                                                                               
         USING ACMD,R2                                                          
ACTU12   L     R2,AMONACC             DATE PACKED YMD                           
         MVC   SAVEDATE,ACMMDTE                                                 
         DROP  R2                                                               
         MVC   SAVETRDT,TRNSDATE                                                
*                                                                               
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING ACPERSD,R4                                                       
*                                                                               
         TM    ACPSSTAT,X'80'+X'20' 'B' TYPE OR 'R' TYPE                        
         BZ    XIT                 NON- BILLED ITEM, DO NOT USE                 
*                                                                               
         CLI   QOPT3,C' '          FILTER TIME?                                 
         BE    ACTU13              NO                                           
         CLI   QOPT3,C'B'          B TIME ONLY                                  
         BNE   ACTU12A             NO                                           
         TM    ACPSSTAT,X'80'      IS THIS B TIME?                              
         BO    ACTU13              YES                                          
         B     XIT                 NO REJECT                                    
*                                                                               
ACTU12A  CLI   QOPT3,C'R'          R TIME ONLY                                  
         BNE   ACTU13              NO, ACCEPT ALL TIME                          
         TM    ACPSSTAT,X'20'      IS THIS R TIME?                              
         BO    ACTU13              YES                                          
         B     XIT                 NO REJECT                                    
*                                                                               
ACTU13   ZAP   SAVERATE,ACPSRATE                                                
         ZAP   SAVEHOUR,ACPSHOUR                                                
*                                                                               
         L     R4,ADTRANS                                                       
         MVI   ELCODE,X'51'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING ACPCD,R4                                                         
         MVC   SRTCLI,ACPCCLI+3                                                 
         CLC   QSELECT,SPACES      ANY LIMIT CLIENT (OR LIST)                   
         BE    ACTU18              NO                                           
         CLI   LISTSW,C'Y'         DONE WITH A LIST                             
         BE    ACTU15              YES                                          
         CLC   SRTCLI,QSELECT      NO, QSELECT IS THE CLIENT YOU WANT           
         BNE   XIT                                                              
         B     ACTU18                                                           
*                                                                               
ACTU15   L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         GOTO1 ACLIST,DMCB,VLISTREC,SRTCLI                                      
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB,C'E'           EXCLUDE?                                     
         BE    XIT                 YES                                          
         DROP  RF                                                               
*                                                                               
ACTU18   MVI   SRTTYPE,PRODTYPE                                                 
         MVC   SRTPROD,ACPCCLI+6                                                
         MVC   SRTJOB,SPACES                                                    
         ZAP   PL16,SAVERATE       CALCULATE BILLABLE AMOUNT                    
         MP    PL16,SAVEHOUR                                                    
         SRP   PL16,64-2,5                                                      
         ZAP   SRTYAMT,PL16        BUMP YTD AMOUNT                              
         ZAP   SRTYHR,SAVEHOUR     BUMP YTD HOURS                               
         ZAP   SRTCAMT,=P'0'       THESE FIELDS NOT USED IN JOB RECORD          
         ZAP   SRTCHR,=P'0'                                                     
         CLC   SAVEDATE,CURMON  IS THIS THE SAME MONTH AS CURMON                
         BNE   ACTU20                                                           
         ZAP   SRTCAMT,SRTYAMT     BUMP CURRENT MONTH AMOUNT                    
         ZAP   SRTCHR,SRTYHR       BUMP CURRENT MONTH HOURS                     
         XC    SRTSUBR,SRTSUBR                                                  
*                                                                               
ACTU20   BAS   RE,PUTSORT                                                       
*                                                                               
         CLI   ACPCLEN,X'11'          IS THERE A PROJECT ACCOUNT?               
         BNH   XIT                    NO                                        
         CLC   ACPCPRJT+9(6),SPACES   IS THERE A JOB IN THE PROJECT             
         BE    XIT                 NO                                           
         MVI   SRTTYPE,PRODJOB                                                  
         BAS   RE,PUTSORT                                                       
*                                                                               
         MVI   SRTTYPE,JOBTYPE                                                  
         MVC   SRTCLI,ACPCPRJT+3                                                
         MVC   SRTPROD,ACPCPRJT+6                                               
         MVC   SRTJOB,ACPCPRJT+9                                                
         MVC   SRTDATE,SAVETRDT    TIME SHEET DATE                              
*                                                                               
         ZAP   SRTHOUR,SAVEHOUR                                                 
         ZAP   SRTRATE,SAVERATE                                                 
         ZAP   SRTAMNT,PL16        BILLABLE AMOUNT CALCULATED ABOVE             
         ZAP   SRTYAMT,=P'0'       THIS FIELDS NOT USED IN JOB RECORD           
         USING ACKEYACC,R1                                                      
         L     R1,ADTRANS                                                       
         SH    R1,DATADISP                                                      
         MVC   SRTSUBR,ACKEYSBR    NEED TO KEEP TRANS UNIQUE AT JOB             
         DROP  R1                                                               
         BAS   RE,PUTSORT                                                       
         MVC   SRTJOB,SPACES       CLEAR OUT JOB LEVEL SORT KEYS                
         XC    SRTDATE,SRTDATE                                                  
         XC    SRTSUBR,SRTSUBR                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
*        LAST REQUEST - GET RECORDS FROM SORTER                                 
*                       PRODUCE REPORT                                          
********************************************************************            
REQLST   DS    0H                                                               
         CLI   MODE,REQLAST                                                     
         BNE   RUNLST                                                           
*                                                                               
         MVC   SRTREC(SRTLNQ),SPACES      INIT FOR FIRST THRU                   
*                                                                               
REQL10   BAS   RE,GETSORT          RETURN A SORT RECORD IN THISREC              
*                                                                               
         CLC   THISREC(SRTLNQ),SPACES ANYTHING RETURNED FROM SORT               
         BE    XIT                 NO                                           
*                                                                               
         LA    R6,THISREC          A(RECORD TO PROCESS                          
         LA    R3,SRTBUCKS                PROCESS SAVED RECORD                  
         MVC   BYTE,DETMASK        PRINT ALL BUCKETS AT THIS LEVEL              
         BAS   RE,PRTBUCKS         PROCESS SAVED RECORD                         
         BAS   RE,PRINTEM                                                       
*                                                                               
         USING TOTTBLD,R2          ADD TO EMPLOYEE TOTALS                       
         LA    R2,TOTTBL                                                        
         LA    R4,TOTTBL           FIRST CHECK THE BUCKET                       
         BAS   RE,NEEDTOTS                                                      
         LA    R2,TOTBUCKS                                                      
         LA    R1,NUMBUCKS                                                      
REQL20   AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,REQL20                                                        
*                                                                               
********************************************************************            
*        LOOK FOR LAST OF LEVEL                                                 
********************************************************************            
*                                                                               
         LA    R2,TOTTBL                                                        
         SR    R1,R1                                                            
*                                                                               
REQL50   IC    R1,TOTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   THISREC(0),SRTREC   DO I NEED TOTALS AT THIS LEVEL               
         BE    REQL190             NO                                           
*                                                                               
         CLI   TOTLEV,1           PRINT A BLANL AFTER LAST DETAIL REC           
         BNE   REQL55                                                           
         CLI   REPSTAT,JOBTYPE                                                  
         BE    REQL60                                                           
REQL55   CLI   TOTLEV,2                                                         
         BNE   REQL70                                                           
         CLI   REPSTAT,PRODTYPE                                                 
         BNE   REQL70                                                           
REQL60   BAS   RE,PRINTEM                                                       
*                                                                               
REQL70   LA    R3,TOTBUCKS                                                      
         MVC   BYTE,TOTWANT        DO I PRINT TOTALS AT THIS LEVEL              
         NC    BYTE,REPSTAT                                                     
         BZ    REQL150             NO                                           
         CLI   TOTNEED,1           MORE THAN 1 RECORD IN THIS TOTAL?            
         BNE   REQL150             NO                                           
*                                                                               
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P+12(L'TOTTYPE),TOTTYPE                                          
         CLI   TOTLEV,3            OVERALL SUBDEPT TOTALS                       
         BNE   REQL80                                                           
         MVC   P+1(25),SPACES                                                   
         MVC   P+1(17),=C'OVERALL TOTAL FOR'                                    
         MVC   P+19(2),SRTCAT                                                   
*                                                                               
REQL80   CLI   TOTLEV,2            SUBDEP OR EMPLOYEE SUB TOTS                  
         BH    REQL90              NO                                           
         MVC   P+1(35),SPACES      YES, INDENT                                  
         MVC   P+3(10),=C'TOTALS FOR'                                           
         MVC   P+14(L'TOTTYPE),TOTTYPE                                          
         CLI   TOTLEV,1            WAS THIS FOR EMPLOYEE?                       
         BE    *+10                YES                                          
         MVC   P+23(2),SRTCAT      PRINT S-DEP CODE                             
REQL90   CLI   TOTLEV,8            OFFICE TOTALS                                
         BNE   REQL100                                                          
         MVI   RCSUBPRG,6          DONT PRINT CLIENT IN HEADER                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+19(2),HDOFF                                                    
*                                                                               
REQL100  CLI   TOTLEV,7            CLIENT TOTALS                                
         BNE   REQL110                                                          
         MVC   P+19(3),HDCLI                                                    
REQL110  CLI   TOTLEV,6            PRODUC TOTALS                                
         BNE   REQL120                                                          
         MVC   P+20(3),HDPRO                                                    
REQL120  CLI   TOTLEV,5            JOB TOTALS                                   
         BNE   REQL130                                                          
         MVC   P+16(6),SVJOB                                                    
*                                                                               
REQL130  CLI   TOTLEV,3            SUBDEP TOTALS                                
         BE    REQL132             YES                                          
         CLI   TOTLEV,2            SUBDEP TOTALS                                
         BNE   REQL135             YES                                          
REQL132  CLI   PROGPROF,C'Y'       INCLUDE 1R SUBTOTALS?                        
         BE    REQL140             YES                                          
         CLI   TOTLEV,1            LET EMPLOYEE TOTALS THRU, THOUGH             
         BE    REQL140                                                          
         MVC   P,SPACES                                                         
         B     REQL150                                                          
*                                                                               
REQL135  CLI   TOTLEV,3            OFFICE/DEPT TOTALS                           
         BNE   REQL136             NO                                           
         CLI   TOTLEV,2            SUBDEP TOTALS                                
         BNE   REQL140             YES                                          
REQL136  CLI   PROGPROF,C'Y'       THESE NOT WANTED?                            
*        BNE   REQL140             NO                                           
         BE    REQL140             NO                                           
         MVC   P,SPACES            OFF/DEP NOT WANTED                           
         B     REQL150                                                          
*                                                                               
REQL140  MVC   BYTE,TOTMASK                                                     
         BAS   RE,PRTBUCKS                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
REQL150  BAS   RE,PRTAFFIL         PRINT JOB AFFILIATED TOTALS                  
         CLI   TOTLEV,7            DID I JUST PRINT CLIENT LEVEL                
         BNE   REQL160                                                          
         MVC   SAVETYPE,SRTTYPE    PRINT SUMMARY                                
         BAS   RE,PRTSUM                                                        
         MVI   FORCEHED,C'Y'        FORCE T-O-F                                 
         MVI   REPSTAT,PRODTYPE    SWITCH REPORT TYPES                          
         MVC   OFFSETS,PRODOFF      SET OFFSETS                                 
         MVI   RCSUBPRG,1                                                       
         BAS   RE,SETMASKS         RESET PRINT MASKS                            
         CLI   SRTTYPE,JOBTYPE     WAS THIS LAST CLIENT FOR A JOB REP           
         BE    REQL180             YES, DONT BUMP THESE TOTALS                  
*                                                                               
REQL160  LA    R4,TOTTBLLN(R2)     ADDRESS NEXT TABLE ENRTY                     
         BAS   RE,NEEDTOTS         IF THERE IS STUFF, WE WILL NEED TOTS         
         LA    R4,TOTTBLLN(R3)     HIGHER LEVEL BUCKETS                         
         LA    R1,NUMBUCKS                                                      
REQL170  AP    0(BUCKLN,R4),0(BUCKLN,R3)                                        
         LA    R3,BUCKLN(R3)                                                    
         LA    R4,BUCKLN(R4)                                                    
         BCT   R1,REQL170                                                       
*                                                                               
REQL180  LA    R2,TOTTBLLN(R2)     GET NEXT LEVEL ENTRY                         
         CLI   TOTLEV,9            DID I JUST BUMP REPORT LEVEL TOTS            
         BNE   REQL50              NO LOOK FOR NEXT LAST.                       
         CLI   SRTREC,X'FF'        ARE THERE MORE SORT RECORDS?                 
         BE    REQL200             NO, DO REPORT TOTALS                         
*                                                                               
REQL190  LA    R1,TOTTBL           DID I GET INTO THE TABLE                     
         CR    R2,R1                                                            
         BE    REQL10              NO                                           
*                                                                               
         LA    R1,TOTTBLLN         YES, BACK UP TO THE LEVEL THAT IS            
         SR    R2,R1               DIFFRENT                                     
         BAS   RE,PRTFRST          PRINT ANY FIRSTS YOU MIGHT NEED              
         B     REQL10              GET NEXT SORT REC                            
         SPACE 2                                                                
*                                                                               
REQL200  EQU   *                   REPORT TOTALS                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
         LA    R2,TOTHIGH                                                       
         LA    R3,TOTBUCKS                                                      
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P+12(L'TOTTYPE),TOTTYPE                                          
         MVC   BYTE,TOTMASK                                                     
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
         BAS   RE,PRTAFFIL                                                      
*                                                                               
         MVI   SAVETYPE,REQTYPE    PRINT SUMMARY                                
         BAS   RE,PRTSUM                                                        
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
*                                                                               
*                                                                               
RUNLST   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
*                                                                               
         BAS   RE,RELBUFF                                                       
         B     XIT                                                              
         EJECT                                                                  
******************                                                              
PUTSORT  NTR1                             ** PUT RECORD TO SORT **              
******************                                                              
         USING SRTD,R6                                                          
         LA    R6,SRTREC                                                        
         CLI   QOPT1,C' '          DID THEY REQUEST A SPECIFIC REPORT           
         BE    PUT50               NO                                           
         CLI   QOPT1,C'I'          DID THEY REQUEST A SPECIFIC REPORT           
         BE    PUT50               NO                                           
         CLI   QOPT1,C'A'          PRODUCT REPORT ONLY                          
         BE    PUT10                                                            
         CLI   QOPT1,C'O'          PRODUCT REPORT ONLY                          
         BE    PUT10                                                            
         CLI   QOPT1,C'B'          JOB  REPORT ONLY                             
         BE    PUT20                                                            
         CLI   QOPT1,C'P'          JOB  REPORT ONLY                             
         BE    PUT20                                                            
         B     XIT                                                              
*                                                                               
PUT10    CLI   SRTTYPE,PRODTYPE                                                 
         BE    PUT50                                                            
         B     XIT                                                              
*                                                                               
PUT20    CLI   SRTTYPE,JOBTYPE     JOB ONLY REPORT                              
         BNE   XIT                                                              
*                                                                               
PUT50    CLI   QOPT4,C' '          FILTER CLOSED, NOT CLOSED                    
         BE    PUT70               NO                                           
         CLI   SRTTYPE,JOBTYPE     JOB SORT RECORD                              
         BNE   PUT70               NO                                           
         BAS   RE,GETJOB                                                        
         CLI   QOPT4,C'C'          WANT CLOSED ONLY                             
         BNE   PUT60                                                            
         CLI   BYTE,C'Y'           Y= CLOSED                                    
         BE    PUT70                                                            
         B     XIT                                                              
*                                                                               
PUT60    CLI   BYTE,C'Y'           OPEN JOBS ONLY                               
         BE    XIT                 JOB IS CLOSED                                
*                                                                               
PUT70    GOTO1 SORTER,DMCB,=C'PUT',(R6)                                         
         MVI   ALSORT,1                      ACTIVITY SWITCH                    
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
GETSORT  NTR1                      RETURN A SORT RECORD IN THISREC              
*                                  SUM SORT RECORDS WITH DUP KEYS               
*                                  WHEN ALSORT=0 THER ARE NO MORE               
**************************************************************                  
*                                                                               
         MVC   THISREC(SRTLNQ),SRTREC                                           
GETS10   GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         ST    R6,ALSORT                  ADDRESS OF LAST SORT                  
         LTR   R6,R6                      END OF RECORDS FROM SORT              
         BNZ   GETS20                     NO                                    
         CLC   SRTREC(SRTLNQ),SPACES      IS THERE A PREVIOUS SORT              
         BE    XIT                        NO                                    
         MVI   SRTREC,X'FF'               YES, FORCE LAST'S                     
         B     GETSX                                                            
*                                                                               
GETS20   MVC   SRTREC(SRTLNQ),0(R6)       SAVE CURRENT SORT RECORD              
         CLC   THISREC(SRTLNQ),SPACES     DO I HAVE ONE SAVED                   
         BNE   GETS50                     YES - CONTINUE                        
         LA    R2,TOTHIGH                 INIT ALL LEVELS                       
         BAS   RE,PRTFRST                                                       
         MVC   THISREC(SRTLNQ),SRTREC     SAVE THIS ONE                         
         B     GETS10                     AND GET NEXT                          
*                                                                               
GETS50   CLI   SRTTYPE,PRODJOB            IS THIS A JOB AFFIL RECORD            
         BNE   GETS60                     NO                                    
         LA    R5,CLIAFFIL                YES - KEEP IN SPECIAL BUCK            
         AP    0(L'SRTCHR,R5),SRTCHR       CUR HOURS                            
         AP    8(L'SRTCAMT,R5),SRTCAMT     CUR AMOUNT                           
         AP    16(L'SRTYHR,R5),SRTYHR      YTD HOURS                            
         AP    24(L'SRTYAMT,R5),SRTYAMT    YTD AMOUNT                           
         B     GETS10                      AND GET NEXT RECORD                  
*                                                                               
GETS60   CLI   SRTTYPE,JOBTYPE           JOB (INVOICE) SORT RECORD              
         BE    GETSX               YES, DON'T SUM RECORDS                       
*                                                                               
         CLC   THISREC(SRTKYLN),SRTREC   SAME KEY                               
         BNE   GETSX                      NO - PROCESS SAVED ONE                
*                                                                               
         LA    R5,THISREC                 YES - ADD'EM UP                       
         AP    SRTCHR-SRTD(L'SRTCHR,R5),SRTCHR      CUR HOURS                   
         AP    SRTCAMT-SRTD(L'SRTCAMT,R5),SRTCAMT   CUR AMOUNT                  
         AP    SRTYHR-SRTD(L'SRTYHR,R5),SRTYHR      YTD HOURS                   
         AP    SRTYAMT-SRTD(L'SRTYAMT,R5),SRTYAMT   YTD AMOUNT                  
         B     GETS10                     AND GET NEXT                          
*                                                                               
GETSX    LA    R6,THISREC                                                       
         MVC   REPSTAT,SRTTYPE                                                  
         BAS   RE,SETMASKS                                                      
*                                                                               
*                                  PUT TO BUFFALO                               
         MVC   BUFKEY,SPACES                                                    
         MVC   BUFDATA,SPACES                                                   
         MVC   BUFTYPE,SRTTYPE                                                  
         MVC   BUFBUCKS(4*BUCKLN),SRTBUCKS                                      
*                                                                               
         CLI   PROGPROF,C'Y'                                                    
         BE    GETSX30                                                          
         CLI   SRTTYPE,PRODTYPE    NO JOB LEVEL BUCKS                           
         BNE   GETSXX                                                           
         MVC   BUFACCT,SRTEMPL                                                  
         MVI   BUFTYPE,REQTYPE     PUT OUT A REQUEST EMPLOYEE LINE              
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFREC                                
         MVI   BUFTYPE,PRODTYPE    PUT OUT A CLIENT EMPLOYEE LINE               
         B     *+10                                                             
*                                                                               
GETSX30  MVC   BUFACCT(L'SRTSUB),SRTSUB                                         
         MVC   BUFDATA,SRTEMPL     SAVE OFF DEP SDE                             
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFREC                                
GETSXX   XC    PRTSTAT,PRTSTAT                                                  
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
PRTBUCKS NTR1                   -PRINT THE 4 BUCKETS AT 0(R3)                   
*                               -BYTE CONTAINS BINARY MASK                      
*                                TO EITHER PRINT OF IGNORE A BUCKET             
*                               -REPSTAT MUST BE SET TO PRODTYPE OR             
*                                JOBTYPE FOR THE TYPE OF REPORT YOU ARE         
*                                PRODUCING                                      
**************************************************************                  
         LA    R2,OFFSETS          TABLE OF OFSETS FOR THE REPORT               
         CLI   REPSTAT,PRODTYPE    BUT IS THIS THE PRODTYPE REPORT              
         BE    PRTB05              YES, PRINT BUCKETS AT FIRST OFFSET           
         TM    BYTE,X'10'          PRINT DATE NOW?                              
         BNO   *+8                                                              
         BAS   RE,PRTDATE          JOB REP, PRINT DATE FIRST                    
         LA    R2,1(R2)            JOB REPORT BUCKETS-2ND OFFSET                
*                                                                               
PRTB05   TM    BYTE,X'08'          PRINT FIRST BUCKET                           
         BNO   PRTB10                                                           
         BAS   RE,PRTHRS           FIRST BUCKET IS ALWAYS HOURS                 
*                                                                               
PRTB10   LA    R3,BUCKLN(R3)       NEXT BUCKET                                  
         LA    R2,1(R2)            NEXT OFFSET                                  
         TM    BYTE,X'04'                                                       
         BNO   PRTB30                                                           
         CLI   REPSTAT,PRODTYPE    DOING CLI/PROD RECORDS                       
         BE    PRTB20              YES                                          
         BAS   RE,PRTRATE          SECOND BUCK OF JOB REPORT IS RATE            
         B     PRTB30                                                           
PRTB20   BAS   RE,PRTAMNT          SECOND BUCK OF CLI/PRO REP IS AMNT           
*                                                                               
PRTB30   LA    R3,BUCKLN(R3)       NEXT BUCKET                                  
         LA    R2,1(R2)            NEXT OFFSET                                  
         TM    BYTE,X'02'          PRINT THIS BUCKET                            
         BNO   PRTB50              NO                                           
         CLI   REPSTAT,PRODTYPE                                                 
         BE    PRTB40                                                           
         BAS   RE,PRTAMNT          THIRD BUCK ON JOB IS AMOUNT                  
         B     PRTB50                                                           
PRTB40   BAS   RE,PRTHRS           3RD BUCK FOR PROD REP IS HOURS               
*                                                                               
PRTB50   CLI   REPSTAT,PRODTYPE    PROD TYPE REPORT                             
         BNE   XIT                 NO, 4TH BUCK NOT USED                        
         TM    BYTE,X'01'                                                       
         BNO   XIT                                                              
         LA    R3,BUCKLN(R3)       NEXT BUCK                                    
         LA    R2,1(R2)            NEXT OFFSET                                  
         BAS   RE,PRTAMNT                                                       
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*        PRINT ANY FIRSTS YOU MIGHT NEED                                        
*        0(R2) POINTS TO THE HIGHEST LEVEL TOTAL PRINTED                        
*        PRINT THE NEW FIRST FOR THIS AND LOWER LEVELS                          
********************************************************************            
PRTFRST  NTR1                                                                   
         USING SRTD,R6                                                          
         USING TOTTBLD,R2                                                       
         LA    R6,SRTREC           ADDRESS OF NEXT SORT RECORD                  
         CLI   TOTLEV,5            IS THIS A NEW SJ ACCOUNT                     
         BL    PRFR80              NO, GET 1R ACCOUNTS                          
         BE    *+8                 NO TOF WITH A NEW JOB                        
         MVI   FORCEHED,C'Y'       TOP 'O FORM                                  
         MVI   RCSUBPRG,1                                                       
         MVC   OFFSETS,PRODOFF                                                  
         TM    SRTTYPE,JOBTYPE                                                  
         BNO   PRFR05                                                           
         MVI   RCSUBPRG,2                                                       
         MVC   OFFSETS,JOBOFF                                                   
*                                                                               
PRFR05   CLI   TOTLEV,9            FIRST FOR REQUEST                            
         BNE   PRFR10              NO                                           
*                                                                               
         LA    R1,NUMBUCKS                                                      
         LA    R3,REPAFFIL                                                      
PRFR05A  ZAP   0(BUCKLN,R3),=P'0'                                               
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PRFR05A                                                       
*                                                                               
         BAS   RE,ZAPNBUMP         CLEAR THESE ACCUMS, POINT TO PREV            
*                                                                               
PRFR10   CLI   TOTLEV,8            IS THIS A NEW SJ OFFICE?                     
         BNE   PRFR20              NO                                           
         MVC   HDOFF,SRTOFF                                                     
         MVC   TOTDATA,SPACES                                                   
         MVC   TOTDATA,SRTOFF                                                   
         USING ACKEYD,R4           READ OFFICE NAME RECORD                      
         XC    MYKEY,MYKEY         OFFICE REC KEY PADDED W/ X'00''S             
         LA    R4,MYKEY                                                         
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF    GET SPECIFIC OFFICE                          
         MVC   ACOGCUL(1),RCCOMPFL                                              
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         MVC   ACOGOFC,SRTOFF                                                   
         BAS   RE,GETACCT          DMRG CALL, R4 WILL=A(20 EL)                  
         LA    R5,HDOFFNM                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         LA    R1,NUMBUCKS                                                      
         LA    R3,OFFAFFIL                                                      
PRFR15   ZAP   0(BUCKLN,R3),=P'0'                                               
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PRFR15                                                        
*                                                                               
         BAS   RE,ZAPNBUMP         CLEAR THESE ACCUMS, POINT TO PREV            
*                                                                               
PRFR20   CLI   TOTLEV,7                                                         
         BNE   PRFR40                                                           
         MVC   MYKEY,SPACES                                                     
         MVC   HDCLI,SRTCLI                                                     
         MVC   TOTDATA,SPACES                                                   
         MVC   TOTDATA,SRTCLI                                                   
         MVC   MYKEY,SPACES                                                     
         MVC   MYCUL(1),RCCOMPFL                                                
         MVC   MYCUL+1(2),=C'SJ'                                                
         MVC   MYACCT(3),SRTCLI                                                 
         BAS   RE,GETACCT          DMRG CALL, R4 WILL=A(20 EL)                  
         LA    R5,HDCLINM                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         LA    R1,NUMBUCKS                                                      
         LA    R3,CLIAFFIL                                                      
PRFR25   ZAP   0(BUCKLN,R3),=P'0'                                               
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PRFR25                                                        
*                                                                               
         BAS   RE,ZAPNBUMP         CLEAR THESE ACCUMS, POINT TO PREV            
*                                                                               
PRFR40   CLI   TOTLEV,6            PRODUCT                                      
         MVC   TOTDATA,SPACES                                                   
         BNE   PRFR60                                                           
         MVC   MYACCT,SPACES                                                    
         CLC   SRTPROD,SPACES      ANY PRODUCT THERE                            
         BH    PRFR45              YES                                          
         MVC   HDPRO(4),=C'NONE'                                                
         MVC   HDPRONM,SPACES                                                   
         B     PRFR50                                                           
PRFR45   MVI   HDPRO+3,C' '                                                     
         MVC   HDPRO(3),SRTPROD                                                 
         MVC   MYACCT(3),SRTCLI                                                 
         MVC   MYACCT+3(3),SRTPROD                                              
         MVC   TOTDATA,SRTPROD                                                  
         BAS   RE,GETACCT          DMRG CALL, R4 WILL=A(20 EL)                  
         LA    R5,HDPRONM                                                       
         BAS   RE,GETNAME                                                       
PRFR50   BAS   RE,ZAPNBUMP         CLEAR THESE ACCUMS, POINT TO PREV            
*                                                                               
PRFR60   CLI   TOTLEV,5            JOB                                          
         BNE   PRFR80              MUST BE A NEW 1R ACCOUNT                     
         MVC   TOTDATA,SPACES                                                   
         CLC   SRTJOB,SPACES                                                    
         BH    PRFR65                                                           
         MVC   SVJOB,SPACES                                                     
         MVC   JOBNAME,SPACES                                                   
         B     PRFR70                                                           
PRFR65   MVC   SVJOB,SRTJOB                                                     
         MVC   MYACCT,SPACES                                                    
         MVC   MYACCT(3),SRTCLI                                                 
         MVC   MYACCT+3(3),SRTPROD                                              
         MVC   MYACCT+6(6),SRTJOB                                               
         MVC   TOTDATA,SRTJOB                                                   
         BAS   RE,GETACCT          DMRG CALL, R4 WILL=A(20 EL)                  
         LA    R5,JOBNAME                                                       
         BAS   RE,GETNAME                                                       
         MVC   P+1(L'TOTTYPE),TOTTYPE                                           
         MVC   P+14(6),SVJOB                                                    
         MVI   PSECOND+1,X'BF'                                                  
         MVC   PSECOND+2(17),PSECOND+1                                          
         MVC   P+25(34),JOBNAME                                                 
         MVI   PSECOND+25,X'BF'    START UNDERLINE OF JOBNAME                   
         ZIC   R1,1(R4)            R4 IS STILL A(20 EL) OF JOB                  
         CH    R1,=H'36'           IS JOBNAME GT 36 CHARS                       
         BNH   *+8                 NO                                           
         LA    R1,36               MOVE A MAX OF 34 CHARACTERS                  
         SH    R1,=H'4'            ELCODE, ELLEN, MVC AND FIRST X'BF'           
         BM    PRFR68                                                           
         EX    R1,*+8                                                           
         B     PRFR68                                                           
         MVC   PSECOND+26(0),PSECOND+25                                         
*                                                                               
PRFR68   MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
PRFR70   BAS   RE,ZAPNBUMP         CLEAR THESE ACCUMS, POINT TO PREV            
*                                                                               
PRFR80   XR    R0,R0                                                            
         LA    R0,TOTTBL                                                        
         CR    R2,R0               BEGINNING OF TABLE                           
         BL    XIT                 THEN I'M DONE                                
         CLI   TOTLEV,3            OVERALL SUBTOTAL                             
         BE    PRFR100             DONT PRINT ANY FIRSTS                        
         CLI   TOTLEV,1            EMPLOYEE SUBTOTAL                            
         BE    PRFR85              ALWAYS PRINT FIRSTS                          
         CLI   PROGPROF,C'Y'       PRINT 1R SUBTOTALS?                          
         BNE   PRFR83              YES                                          
         CLI   TOTLEV,4            OFF OR DEP TOTAL                             
         BNE   PRFR85              NO, PRINT                                    
         B     PRFR100             SKIP PRINT                                   
*                                                                               
PRFR83   CLI   TOTLEV,4            IS THIS A 1R SUBTOTAL                        
         BE    PRFR85              YES                                          
         B     PRFR100             NO                                           
         USING LISTD,R3                                                         
PRFR85   LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         LA    R0,SRTEMPL-SRTD     LENGTH OF STUFF BEFORE EMPLOYEE KEY          
         ZIC   R1,TOTLEN           LENGTH OF THIS TABLES SORT KEY               
         SR    R1,R0               DIFF IS LENGTH OF 1R ACCT TO LOOKUP          
         BCTR  R1,0                ADJ FOR MVC                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTKEY(0),SRTEMPL                                               
*                                                                               
         MVC   *+8(2),TOTLIST      OFFSET OF LIST TABLE FOR THIS LEVEL          
         L     R3,FULL                                                          
         BAS   RE,GETLIST          GET NAME FOR THIS ACCOUNT IN WORK            
         LA    R3,WORK                                                          
         CLI   TOTLEV,1                                                         
         BNE   PRFR90                                                           
         MVC   P+10(12),SRTEMPL                                                 
         B     PRFR95                                                           
PRFR90   MVC   P+1(L'TOTTYPE),TOTTYPE                                           
         MVC   P+14(12),LISTKEY                                                 
PRFR95   MVC   P+25(34),LISTNAME                                                
         MVC   TOTDATA,SPACES                                                   
         MVC   TOTDATA,LISTKEY                                                  
         LA    R1,TOTTBL           ADDRESS OF TABLE                             
         CR    R2,R1               AM I ON THE LOWEST LEVEL?                    
         BE    PRFR100             YES                                          
         BAS   RE,PRINTEM          PRINT THIS LEVEL                             
PRFR100  BAS   RE,ZAPNBUMP                                                      
         B     PRFR80                                                           
         EJECT                                                                  
*****************************************                                       
*  AFTER CLIENT, OFFICE OR REPORT TOTALS                                        
*  PRINT TOTALS OF JOB AFFILIATED TIME                                          
****************************************                                        
         USING TOTTBLD,R2                                                       
PRTAFFIL NTR1                                                                   
         CLI   TOTLEV,7                                                         
         BL    PRTAX                                                            
         CLI   REPSTAT,PRODTYPE                                                 
         BNE   PRTAX                                                            
         MVC   BYTE,TOTMASK                                                     
         LA    R3,CLIAFFIL                                                      
         CLI   TOTLEV,7                                                         
         BE    PRTA20                                                           
         LA    R3,OFFAFFIL                                                      
         CLI   TOTLEV,8                                                         
         BE    PRTA20                                                           
         LA    R3,REPAFFIL                                                      
         B     PRTA50              DONT BUMP TOTALS AT REPORT LEVEL             
*                                                                               
PRTA20   LA    R4,NUMBUCKS*BUCKLN(R3) POINT TO NEXT BUCKET                      
         LR    R5,R3               DON'T USE R3                                 
         LA    R0,NUMBUCKS                                                      
PRTA30   AP    0(BUCKLN,R4),0(BUCKLN,R5)   BUMP TOTALS                          
         LA    R4,BUCKLN(R4)                                                    
         LA    R5,BUCKLN(R5)                                                    
         BCT   R0,PRTA30                                                        
*                                                                               
PRTA50   CLI   TOTLEV,8            OFFICE TOTS                                  
         BNE   PRTA60                                                           
         CLI   TOTNEED,1           ONLY PRINT OFFICE TOTS IF NEEDED             
         BNE   PRTAX                                                            
PRTA60   BAS   RE,PRTBUCKS                                                      
         MVC   P+10(28),=C'TIME ENTERED WITH JOB NUMBER'                        
         BAS   RE,PRINTEM                                                       
         MVC   WORKBUCK(NUMBUCKS*BUCKLN),TOTBUCKS                               
         LA    R2,WORKBUCK                                                      
         LA    R1,NUMBUCKS                                                      
PRTA70   SP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PRTA70                                                        
         MVC   P+10(33),=C'TIME ENTERED WITHOUT A JOB NUMBER'                   
         LA    R3,WORKBUCK                                                      
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
PRTAX    B     XIT                                                              
         EJECT                                                                  
*****************************************                                       
*  GET ACCOUNT LEVEL DATA, PUT EMPLOYEE**                                       
*  KEY IN SRT RECORD, SAVE EMPLOYEE NAM**                                       
*  IN TABLE.                           **                                       
****************************************                                        
NEWACC   NTR1                                                                   
         LA    R6,SRTREC                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT                 CLEAR SORT WORK AREA                 
         SPACE 3                                                                
*                                  *BUILD THE  SORT KEY*                        
         USING ACKEYD,R4                                                        
         L     R4,ADACC                                                         
         MVC   SRTEMPL,ACKEYACC+3                                               
         CLI   PROGPROF,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SRTSUB,ACKEYACC+6   SUBDEPT AS SORT FIELD                        
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         USING ACKEYACC,R3                                                      
         L     R3,ADACC                                                         
         MVC   LISTKEY,ACKEYACC+3  SAVE 12 BYTE KEY                             
         L     R4,ADACCNAM                                                      
         LA    R5,LISTNAME                                                      
         BAS   RE,GETNAME          GET NAME                                     
         L     R3,AEMPLST          ADD RECORD TO TABLE                          
         BAS   RE,BUILDLST                                                      
         B     XIT                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        STORE 1R ACCOUNT NAMES                                                 
*        ADD THE LISTD RECORD IN WORK TO THE TABLE AT 0(R3)                     
***********************************************************************         
         USING LISTD,R2                                                         
BUILDLST NTR1                                                                   
         LA    R2,WORK                                                          
         LH    R5,0(R3)            NUMBER IN TABLE                              
         LH    R0,2(R3)            MAX                                          
         CR    R5,R0               IS THERE ANY ROOM?                           
         BL    BLST10                                                           
         DC    H'0'                NO                                           
         DC    C'TBLISFUL'                                                      
         DS    0H                                                               
BLST10   LA    R4,6(R3)            START OF DATA                                
         LA    R1,LISTDLN          LENGTH OF ONE TABLE RECORD                   
         MR    R0,R5               TIMES THE NUMBER IN THE TABLE                
         AR    R4,R1               ADDED TO START ADDRESS IS OFFSET             
         MVC   0(LISTDLN,R4),LISTREC                                            
         LH    R0,0(R3)            BUMP # IN TABLE                              
         AH    R0,=H'1'                                                         
         STH   R0,0(R3)                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        GET 1R ACCOUNT NAMES KEY IS FIRST 12 OF WORK                           
*        TABLE IS AT 0(R3)                                                      
*        RETURN THE NAME IN WORK+12(36)                                         
*        RETURN ONLY THE SIGNIFICANT PORTIONS OF THE KEY                        
***********************************************************************         
         USING LISTD,R3                                                         
GETLIST  NTR1                                                                   
         LH    R0,0(R3)            NUMBER IN TABLE                              
         LTR   R0,R0               ANYTHING THERE                               
         BZ    XIT                 NO                                           
         LH    R2,4(R3)            OFFSET INTO KEY TO RETURN                    
         LA    R3,6(R3)            START OF DATA                                
*                                                                               
GETL10   CLC   LISTKEY,WORK                                                     
         BE    GETL20                                                           
         LA    R3,LISTDLN(R3)      NEXT TABLE ENTRY                             
         BCT   R0,GETL10                                                        
         B     XIT                                                              
GETL20   LA    R1,WORK                                                          
         MVC   LISTNAME-LISTD(L'LISTNAME,R1),LISTNAME                           
         MVC   WORK+50(L'LISTKEY),SPACES                                        
         LA    R4,L'LISTKEY                                                     
         SR    R4,R2               L'TO MOVE IS TOTAL LENGTH - OFFSET           
         BCTR  R4,0                                                             
         AR    R2,R1               OFFSET+ A(KEY) IS DATA TO MOVE               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK+50(0),0(R2)                                                 
         MVC   0(L'LISTKEY,R1),WORK+50                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        R4 IS ADDRESS OF THE 20 ELEMENT                                        
*        R5 IS ADDRESS OF 36 BYTE AREA                                          
***********************************************************************         
         USING ACNAMED,R4                                                       
GETNAME  MVC   0(36,R5),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,GETNMVC                                                       
         BR    RE                                                               
GETNMVC  MVC   0(0,R5),ACNMNAME                                                 
         DROP  R4                                                               
         EJECT                                                                  
*******************                                                  *          
PRINTEM  NTR1                                                                   
*******************                                                             
         CLI   RCSUBPRG,5          REPORT TOTALS                                
         BE    PRNT20                                                           
         MVC   HEAD4+12(2),HDOFF    SJ OFFICE                                   
         MVC   HEAD4+16(36),HDOFFNM                                             
         CLI   RCSUBPRG,6          TOTALS FOR OFFICE                            
         BE    PRNT20                                                           
*                                                                               
         MVC   HEAD5+12(3),HDCLI    SJ CLIENT                                   
         MVC   HEAD5+16(36),HDCLINM                                             
*                                                                               
         MVC   HEAD6+12(4),HDPRO    SJ PRODUCT                                  
         MVC   HEAD6+16(36),HDPRONM                                             
         CLI   PRTSTAT,SUMMARY                                                  
         BNE   PRNT20                                                           
         MVC   HEAD6+1(52),SPACES                                               
         MVC   HEAD6+12(22),=C'*** CLIENT SUMMARY ***'                          
*                                                                               
PRNT20   CLI   REPSTAT,JOBTYPE                                                  
         BE    PRNTX                                                            
         MVC   HEAD4+71(6),CURMONP CURRENT MONTH OF ...                         
*                                                                               
PRNTX    MVC   HEAD6+98(6),FRMMONP POSTINGS FROM ...                            
         MVC   HEAD6+110(6),CURMONP THRU ...                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
CLERSORT NTR1                    **  CLEAR SORT RECORD AREA **                  
**************************************************************                  
         USING SRTD,R6                                                          
         LA    R6,SRTREC                                                        
         XC    SRTREC(SRTLNQ),SRTREC                                            
         LR    R1,R6                         ADDR OF SORT REC INTO R1           
         LA    R1,SRTBUCKS                                                      
         LA    R0,NUMBUCKS                   NUMBER OF BUCKETS INTO R0          
*                                                                               
CLER03   ZAP   0(BUCKLN,R1),=P'0'            CLEAR TO PACKED ZEROS              
         LA    R1,BUCKLN(R1)                 BUMP TO NEXT BUCKET                
         BCT   R0,CLER03                                                        
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*        ZAP THE BUCKETS OF THE SUBTOTAL RECORD AT 0(R2)                        
*        THEN POINT R2 TO THE PREVIOUS BUCKET                                   
**************************************************************                  
         USING TOTTBLD,R2                                                       
ZAPNBUMP EQU   *                                                                
         LA    R3,TOTBUCKS                                                      
         LA    R1,NUMBUCKS                                                      
ZNB10    ZAP   0(BUCKLN,R3),=P'0'                                               
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,ZNB10                                                         
*                                                                               
         LA    R1,TOTTBLLN         POINT TO THE PREVIOUS BUCKET                 
         SR    R2,R1               TO FORCE FIRST FOR LOWER LEVELS              
         BR    RE                                                               
         EJECT                                                                  
**************************************************************                  
*        CALL DATAMRG , MYKEY HAS BEEN SET                                      
*        RETURN IN R4 THE A(20 ELEMENT)                                         
**************************************************************                  
GETACCT  NTR1                                                                   
         LA    R4,BADACCT                                                       
         BAS   RE,READ                                                          
         CLI   DMCB+8,0              TEST FOR ERRORS                            
         BNE   GETAX                 IF ERRORS, RETURN A(DUMMY 20 EL)           
         MVI   ELCODE,X'20'                                                     
         L     R4,ACREC                                                         
         BAS   RE,GETEL                                                         
         BE    GETAX                                                            
         DC    H'0'                                                             
GETAX    XIT1  REGS=(R4)                                                        
         EJECT                                                                  
**************************************************************                  
*        IF THE BUCKET VALUES OF THE TABLE RECORD AT 0(R2)                      
*        ARE NOT ZERO, SET TOTNEED TO 1 SO TOTALS WILL BE PRODUCED              
**************************************************************                  
         USING  TOTTBLD,R4                                                      
NEEDTOTS EQU    *                                                               
         CLI    TOTLEV,2           SUB DEP ALWAYS GETS TOTALS                   
         BE     NEEDTX                     ~~~~~~                               
         CLI    TOTLEV,6           SUB DEP ALWAYS GETS TOTALS                   
         BE     NEEDTX                     ~~~~~~                               
         XC     TOTNEED,TOTNEED                                                 
         LA     R1,TOTBUCKS                                                     
         LA     R0,NUMBUCKS                                                     
NEEDT10  CP     0(BUCKLN,R1),=P'0'                                              
         BE     NEEDT20                                                         
NEEDTX   MVI    TOTNEED,1                                                       
         BR     RE                                                              
NEEDT20  LA     R1,BUCKLN(R1)                                                   
         BCT    R0,NEEDT10                                                      
         BR     RE                                                              
         DROP   R4                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DO OFFICE SECURITY                                                     
*              RETURNS WITH CC EQUAL IF OFFICE IS GOOD                          
*              CC NOT EQUAL IF OFFICE IS NOT WANTED                             
*----------------------------------------------------------------------         
OFFCHK   NTR1                                                                   
         USING TRANSD,R4                                                        
         L     R4,ADTRANS                                                       
*                                                                               
         CLI   NEWOFF,C'Y'                 ON NEW OFFICES?                      
         BE    OFFC40                      YES                                  
*                                                                               
         LA    R0,32                                                            
         L     RF,ADOFFLST                                                      
         CLI   0(RF),0                     ANYTHING THERE                       
         BE    OFFCOK                      NO, NO LIMIT ACCESS                  
*                                                                               
OFFC10   CLI   0(RF),0                     END OF LIST                          
         BE    OFFCNG                      OFFICE NOT FOUND                     
*                                                                               
         CLI   0(RF),C'0'                  OFFICE 0                             
         BE    OFFC20                      SKIP                                 
*                                                                               
         CLC   TRNSOFFC(1),0(RF)                                                
         BE    OFFCOK                      FOUND OFFICE IN LIST                 
*                                                                               
OFFC20   LA    RF,1(RF)                                                         
         BCT   R0,OFFC10                                                        
         B     OFFCNG                                                           
*                                                                               
*        NEW OFFICE VALIDATION                                                  
*                                                                               
OFFC40   L     R1,ADOFFALD                 OFFAL BLOCK                          
         USING OFFALD,R1                                                        
         L     RF,OFFAREQL                 LIST OF VALID OFFICES                
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)                                                       
         BZ    OFFCOK                      OK IF NOTHING IN LIST                
*                                                                               
         LA    RF,2(RF)                                                         
OFFC50   CLC   TRNSOFFC(2),0(RF)                                                
         BE    OFFCOK                      FOUND OFFICE IN LIST                 
         LA    RF,2(RF)                                                         
         BCT   R0,OFFC50                                                        
*                                                                               
OFFCNG   CR    RB,R0                       RETURN A NOT EQUAL                   
         B     OFFCX                                                            
*                                                                               
OFFCOK   CR    RB,RB                       RETURN AN EQUAL                      
OFFCX    B     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
**************************************************************                  
*        PRINT CLIENT LEVEL SUMMARY                                             
*        SAVE TYPE IS SET TO THE TYPE OF SUMMARY YOU WANT TO PRINT              
**************************************************************                  
         USING TOTTBLD,R2                                                       
PRTSUM   NTR1                                                                   
         LA    R2,SUMTOTAL                                                      
         LA    R0,4                                                             
PRTS00   ZAP   0(BUCKLN,R2),=P'0'                                               
         LA    R2,BUCKLN(R2)                                                    
         BCT   R0,PRTS00                                                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   SAVETYPE,REQTYPE                                                 
         BNE   PRTS02                                                           
         CLI   PROGPROF,C'Y'       DO THEY WANT JOB SUMMARIES                   
         BNE   XIT                 NO                                           
         MVI   RCSUBPRG,7                                                       
         B     PRTS05                                                           
*                                                                               
PRTS02   MVI   RCSUBPRG,3          PROD SUMMARY                                 
         CLI   REPSTAT,PRODTYPE                                                 
         BE    PRTS05              JOB TYPE SUMMARY                             
         CLI   PROGPROF,C'Y'       DO THEY WANT JOB SUMMARIES                   
         BNE   XIT                 NO                                           
         MVI   RCSUBPRG,4                                                       
PRTS05   MVI   PRTSTAT,SUMMARY                                                  
         MVC   LASTBUF,SPACES                                                   
         MVC   BUFKEY,SPACES                                                    
         MVC   BUFTYPE,SAVETYPE                                                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ABUFC),BUFKEY,1                         
         MVC   LASTBUF,BUFKEY                                                   
PRTS10   TM    DMCB+8,X'80'                                                     
         BO    XIT                 NO RECS AT ALL                               
         CLC   BUFTYPE,SAVETYPE    NONE OF THIS TYPE                            
         BNE   XIT                                                              
*                                                                               
PRTS15   LA    R3,BUFBUCKS                                                      
         MVI   BYTE,X'EF'                                                       
         CLI   REPSTAT,JOBTYPE                                                  
         BNE   *+8                                                              
         MVI   BYTE,X'EB'          NO TOTAL RATE PLEASE                         
         BAS   RE,PRTBUCKS                                                      
*                                                                               
         CLI   PROGPROF,C'Y'       PRINTING SUBDEPT TYPE SUMMARY                
         BNE   PRTS40              NO PRINT EMPLOYEE SUMMARY                    
         MVC   P+5(L'SRTSUB),BUFACCT                                            
         L     R3,ASDELST                                                       
         LH    R0,0(R3)                                                         
         LTR   R0,R0                                                            
         BZ    PRTS35                                                           
         LA    R3,6(R3)            START OF DATA                                
PRTS20   CLC   0(5,R3),BUFDATA     GET SDEP NAME                                
         BE    PRTS30                                                           
         LA    R3,LISTDLN(R3)                                                   
         BCT   R0,PRTS20                                                        
         B     PRTS35                                                           
*                                                                               
         USING LISTD,R3                                                         
PRTS30   MVC   P+10(36),LISTNAME                                                
PRTS35   B     PRTS50                                                           
         DROP  R3                                                               
*                                                                               
PRTS40   MVC   P+1(12),BUFACCT                                                  
         L     R3,AEMPLST                                                       
         LH    R0,0(R3)                                                         
         LTR   R0,R0                                                            
         BZ    PRTS50                                                           
         LA    R3,6(R3)            START OF DATA                                
PRTS40A  CLC   0(12,R3),BUFACCT    GET EMPLOYEE NAME                            
         BE    PRTS40B                                                          
         LA    R3,LISTDLN(R3)                                                   
         BCT   R0,PRTS40A                                                       
         B     PRTS50                                                           
*                                                                               
         USING LISTD,R3                                                         
PRTS40B  MVC   P+16(36),LISTNAME                                                
*                                                                               
PRTS50   MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
         LA    R2,SUMTOTAL                                                      
         LA    R3,BUFBUCKS                                                      
         LA    R1,NUMBUCKS                                                      
PRTS60   AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PRTS60                                                        
*                                                                               
         MVC   LASTBUF,BUFKEY                                                   
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFC,BUFKEY,1                              
         TM    DMCB+8,X'80'                                                     
         BO    PRTSX               NO RECS AT ALL                               
         CLC   BUFTYPE,SAVETYPE    ARE THEY THIS TYPE                           
         BE    PRTS15                                                           
PRTSX    EQU   *                                                                
         LA    R3,SUMTOTAL                                                      
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P+12(6),=C'CLIENT'                                               
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRINTEM                                                       
         IC    R4,SAVETYPE                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',((R4),ABUFC),(X'80',1)                    
         XC    PRTSTAT,PRTSTAT                                                  
         B     XIT                                                              
         SPACE 3                                                                
         USING LISTD,R3                                                         
         EJECT                                                                  
**************************************************************                  
*        SET PRINT MASKS IN TABLE, DEPENDING ON WHAT TYPE                       
*        OF REPORT YOU ARE PRINTING                                             
**************************************************************                  
         USING TOTTBLD,R2                                                       
SETMASKS NTR1                                                                   
         LA    R1,TOTNUM           NUMBER OF ENTRIES IN TABLE                   
         LA    R2,TOTTBL           NUMBER OF ENTRIES IN TABLE                   
         CLI   QOPT1,C'B'                                                       
         BNH   SETM10                                                           
         LA    R3,MASKO                                                         
         MVC   DETMASK,DETO                                                     
         CLI   REPSTAT,PRODTYPE                                                 
         BE    SETM20                                                           
         LA    R3,MASKP                                                         
         MVC   DETMASK,DETP                                                     
         B     SETM20                                                           
*                                                                               
SETM10   LA    R3,MASKA                                                         
         MVC   DETMASK,DETA                                                     
         CLI   REPSTAT,PRODTYPE                                                 
         BE    SETM20                                                           
         LA    R3,MASKB                                                         
         MVC   DETMASK,DETB                                                     
*                                                                               
SETM20   MVC   TOTMASK,0(R3)                                                    
         LA    R3,1(R3)                                                         
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R1,SETM20                                                        
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*              DATAMGR INTERFACE                                                
**************************************************************                  
HIGH     MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
*        MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'            READ SEQUENTIAL                    
*        MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'            A SPECIFIC READ                    
*                                                                               
GTREC    NTR1                                                                   
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R7)                      
         B     XIT                                                              
         SPACE 4                                                                
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
**************************************************************                  
         EJECT                                                                  
**************************************************************                  
*         EDIT ROUTINES - EDIT THE BUCKET AT 0(R3)                              
*                         0(R2) IS THE OFFSET INTO 'P' TO PRINT                 
**************************************************************                  
*                                                                               
PRTHRS   LA    R4,P                                                             
         ZIC   R1,0(R2)                                                         
         AR    R4,R1                                                            
         EDIT  (P8,(R3)),(9,(R4)),2,MINUS=YES                                   
         BR    RE                                                               
         SPACE 2                                                                
PRTAMNT  LA    R4,P                                                             
         ZIC   R1,0(R2)                                                         
         AR    R4,R1                                                            
         EDIT  (P8,(R3)),(11,(R4)),2,MINUS=YES                                  
         BR    RE                                                               
         SPACE 2                                                                
PRTRATE  LA    R4,P                                                             
         ZIC   R1,0(R2)                                                         
         AR    R4,R1                                                            
         EDIT  (P8,(R3)),(7,(R4)),2,MINUS=YES                                   
         BR    RE                                                               
         SPACE 2                                                                
PRTDATE  LA    R4,P                                                             
         ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         AR    R4,R1                                                            
         GOTO1 DATCON,DMCB,(1,SRTDATE),(5,(R4))                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GET SPACE FOR THE NAME TABLES                                          
*----------------------------------------------------------------------         
GETBUFF  NTR1                                                                   
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF            SAVE BUFF START                              
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RELBUFF  NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         B     XIT                                                              
*----------------------------------------------------------------------         
*        CLEAR GETMAINED SPACE                                                  
*----------------------------------------------------------------------         
SETBUFF  NTR1                                                                   
         L     RE,ABUFF            CLEAR AREA                                   
         L     RF,=A(BUFSIZE)                                                   
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R5,ABUFF            SET ADCONS AND BINSRCH PARMS                 
         LA    R0,MAINNUM                                                       
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         USING BIND,R5                                                          
         L     R2,=A(MAINTAB)                                                   
         USING MAIND,R2                                                         
*                                                                               
SETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         OC    MAINLN,MAINLN       BUILDING A BINSRCH TABLE                     
         BZ    SETB20              NO                                           
*                                                                               
         XC    BININ,BININ                                                      
         XC    BINLEN,BINLEN                                                    
         MVC   BINLEN+3(1),MAINLN                                               
         XC    BINDISPK,BINDISPK                                                
         MVC   BINDISPK+3(1),MAINKLN                                            
         MVC   BINMAX,MAINMAX                                                   
         B     SETB30                                                           
SETB20   MVC   2(2,R5),MAINMAX     BUILD 6 BYTE HEADER STYLE TABLE              
         MVC   4(2,R5),MAINSIG                                                  
SETB30   A     R5,MAINSIZE                                                      
*                                                                               
         LA    R2,MAINLEN(R2)                                                   
         BCT   R0,SETB10                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*        BINSRCH STUFF                                                          
**************************************************************                  
*----------------------------------------------------------------------         
*        ADD AN ITEM TO THE BINSRCH TABLE                                       
*        P1 IS ITEM, P2 IS A(TABLE)                                             
*----------------------------------------------------------------------         
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,ACLOSTAB                                                      
         LA    R4,WORK                                                          
         MVC   DMCB+8(16),BININ    NUMBER, LEN, KEY, MAX                        
         LA    R6,BINTABLE                                                      
         L     R4,0(R1)                                                         
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
*        FING OUT IF A JOB IS OPENED OR CLOSED                                  
*----------------------------------------------------------------------         
         USING BIND,R2                                                          
GETJOB   NTR1                                                                   
         USING CLOSD,R5                                                         
         MVI   READFLAG,C'N'       ASSUME I WON'T                               
*                                                                               
         L     R2,ACLOSTAB                                                      
         MVC   DMCB+8(16),BININ                                                 
         USING SRTD,R6                                                          
         LA    R6,SRTREC                                                        
         MVC   WORK(3),SRTCLI                                                   
         MVC   WORK+3(3),SRTPROD                                                
         MVC   WORK+6(6),SRTJOB                                                 
         MVC   WORK+12,C' '                                                     
         LA    R5,WORK                                                          
         LA    R6,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(X'00',(R5)),(R6)                                   
         CLI   DMCB,0                                                           
         BNE   GETJ20              NOT FOUND                                    
         L     R2,DMCB                                                          
         MVC   WORK(CLOSDLN),0(R2)                                              
         B     GETJX                                                            
*                                                                               
GETJ20   MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),RCCOMPFL                                                
         MVC   MYKEY+1(2),=C'SJ'                                                
         MVC   MYKEY+3(12),WORK                                                 
*                                                                               
         BAS   RE,READ            GET JOB                                       
         MVI   READFLAG,C'Y'       REREAD KEY LATER FOR MONACC                  
         L     R4,ACREC                                                         
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACSTATD,R4                                                       
         MVI   CLOSED,C'N'         ASSUME OPEN                                  
         TM    ACSTSTAT,X'40'                                                   
         BNO   *+8                                                              
         MVI   CLOSED,C'Y'                                                      
         L     R4,ACREC                                                         
         MVC   CLOSKEY,3(R4)                                                    
*                                                                               
         BAS   RE,BINADD                                                        
*                                                                               
GETJX    MVC   BYTE,CLOSED                                                      
         CLI   READFLAG,C'Y'       DO I NEED TO RE-READ                         
         BNE   GETJXX              NO                                           
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         L     R4,ACMALTN                                                       
         MVC   KEY,0(R4)                                                        
         MVC   KEYSAVE,0(R4)                                                    
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(42),0(R4)                                                  
         BAS   RE,READ             RE-READ TRANSACTION                          
GETJXX   B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*              CONSTANTS                                                        
**************************************************************                  
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(BUFFALOC)                                                      
         DC    V(SORTER)                                                        
         DC    V(UNDERLIN)                                                      
         DC    V(ACLIST)                                                        
         DC    V(SQUASHER)                                                      
         DC    X'FF'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         SPACE 2                                                                
BADACCT  DC    X'2023',CL33'** WARNING - ACCOUNT NOT FOUND **'                  
         EJECT                                                                  
**********************************************************************          
         LTORG                                                                  
**********************************************************************          
         EJECT                                                                  
*              BOX ROUTINES (HOOK)                                              
*****************                                                               
BXHOOK   DS    0D                                                               
*****************                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC           RESTORE REG C                                 
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'        SET LH MARGIN                                
         LA    R2,NUMBUCKS         NUMBER OF COLS                               
         LA    R1,BOXCOLS+59       1ST LINE AT 62                               
         CLI   RCSUBPRG,2          JOB TYPE REPORT                              
         BE    BOX20                                                            
         CLI   RCSUBPRG,4          JOB TYPE SUMMARY                             
         BE    BOX20                                                            
BOXIT    MVI   0(R1),C'C'                                                       
         LA    R1,16(R1)           EACH COLUMN = 16                             
         BCT   R2,BOXIT                                                         
         B     BOXX                                                             
*                                                                               
BOX20    MVI   0(R1),C'C'          DATE, HOURS, RATE                            
         LA    R1,10(R1)                                                        
         BCT   R2,BOX20                                                         
         LA    R1,6(R1)            LAST BOX IS 16 WIDE                          
*                                                                               
BOXX     MVI   0(R1),C'R'                                                       
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        TABLE OF SUBTOTAL VALUES                                               
*        LEVEL, LEVEL LENGTH, TITLE,NAME, 4 (BUCKLN)P ACCUMS                    
*        TABLE IS COVERED BY TOTTBLD                                            
**********************************************************************          
TOTTBL   DS    0H                                                               
         DC    AL1(1),AL1(SRTEMPLN),CL12'EMPLOYEE',(4*8)C' ',X'FF'              
         DC    SL2(AEMPLST),AL1(JOBTYPE),AL1(0),CL7' '                          
*                                                                               
         DC    AL1(2),AL1(SRTSDELN),CL12'SUB-DEPT',(4*8)C' ',X'FF'              
         DC    SL2(ASDELST),AL1(JOBTYPE+PRODTYPE),AL1(0),CL7' '                 
*                                                                               
         DC    AL1(3),AL1(SRTSDHLN),CL12'SUB-DEPT',(4*8)C' ',X'FF'              
         DC    SL2(ASDELST),AL1(JOBTYPE+PRODTYPE),AL1(0),CL7' '                 
*                                                                               
         DC    AL1(4),AL1(SRTDEPLN),CL12'STAFF DEPT  ',(4*8)C' ',X'FF'          
         DC    SL2(ADEPLST),AL1(JOBTYPE+PRODTYPE),AL1(0),CL7' '                 
*                                                                               
         DC    AL1(4),AL1(SRTEOFLN),CL12'STAFF OFFICE',(4*8)C' ',X'FF'          
         DC    SL2(AOFFLST),AL1(JOBTYPE+PRODTYPE),AL1(0),CL7' '                 
*                                                                               
         DC    AL1(5),AL1(SRTJOBLN),CL12'JOB       ',(4*8)C' ',X'FF'            
         DC    SL2(0),AL1(JOBTYPE),AL1(0),CL7' '                                
*                                                                               
         DC    AL1(6),AL1(SRTPROLN),CL12'PRODUCT   ',(4*8)C' ',X'FF'            
         DC    SL2(0),AL1(JOBTYPE+PRODTYPE),AL1(0),CL7' '                       
*                                                                               
         DC    AL1(7),AL1(SRTCLILN),CL12'CLIENT    ',(4*8)C' ',X'FF'            
         DC    SL2(0),AL1(JOBTYPE+PRODTYPE),AL1(0),CL7' '                       
*                                                                               
         DC    AL1(8),AL1(SRTOFFLN),CL12'OFFICE    ',(4*8)C' ',X'FF'            
         DC    SL2(0),AL1(PRODTYPE),AL1(0),CL7' '                               
*                                                                               
TOTHIGH  DC    AL1(9),XL1'01',CL12'REPORT    ',(4*8)C' ',X'FF'                  
         DC    SL2(0),AL1(PRODTYPE),AL1(0),CL7' '                               
TOTNUM   EQU   (*-TOTTBL)/TOTTBLLN     NUMBER OF LEVELS IN THE TABLE            
NUMBUCKS EQU   4                                                                
         EJECT                                                                  
*-------------------------------------------------------------------            
*        SUB TOTAL TABLE FOR CLIENT SUMMARY                                     
*-------------------------------------------------------------------            
SUMTBL   DC    AL1(1),AL1(3),CL12'STAFF DEPT  ',(4*8)C' ',X'FF'                 
         DC    SL2(ADEPLST),AL1(0),AL1(0),CL7' '                                
*                                                                               
         DC    AL1(2),AL1(1),CL12'STAFF OFFICE',(4*8)C' ',X'FF'                 
         DC    SL2(AOFFLST),AL1(0),AL1(0),CL7' '                                
*                                                                               
         DC    AL1(3),AL1(0),CL12'ALL STAFF   ',(4*8)C' ',X'FF'                 
         DC    AL4(0),AL1(0),AL1(0),CL7' '                                      
         EJECT                                                                  
**********************************************************************          
*-------------------------------------------------------------------            
**********************************************************************          
*        TABLE OF BUCKET OFFSETS FOR PRODUCT AND JOB TYPE REPORT                
*                                                                               
*-------------------------------------------------------------------            
JOBOFF   DC    AL1(60),AL1(70),AL1(82),AL1(94)                                  
PRODOFF  DC    AL1(66),AL1(80),AL1(98),AL1(112)                                 
**********************************************************************          
*-------------------------------------------------------------------            
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*        PRINT MASKS FOR THE PROD AND JOB TYPE REPORT                           
*        USED IN PRTBUCKS                                                       
**********************************************************************          
*                                                                               
MASKO    DC    X'FF'               EMP                                          
         DC    X'FF'               SDEP                                         
         DC    X'FF'               SDEP                                         
         DC    X'FF'               DEP                                          
         DC    X'FF'               OFF                                          
         DC    X'FF'               JOB                                          
         DC    X'FF'               CLI                                          
         DC    X'FF'               PRO                                          
         DC    X'FF'               OFF                                          
         DC    X'FF'               REP                                          
*                                                                               
MASKP    DC    B'11101011'         EMP                                          
         DC    B'11101011'         SDEP                                         
         DC    B'11101011'         SDEP                                         
         DC    B'11101011'         DEP                                          
         DC    B'11101011'         OFF                                          
         DC    B'11101011'         JOB                                          
         DC    B'11101011'         PRO                                          
         DC    B'11101011'         CLI                                          
         DC    X'00'               OFF                                          
         DC    X'00'               REP                                          
*                                                                               
MASKA    DC    B'11101010'         EMP                                          
         DC    X'FF'               SDEP                                         
         DC    X'FF'               SDEP                                         
         DC    X'FF'               DEP                                          
         DC    X'FF'               OFF                                          
         DC    X'FF'               JOB                                          
         DC    X'FF'               CLI                                          
         DC    X'FF'               PRO                                          
         DC    X'FF'               OFF                                          
         DC    X'FF'               REP                                          
*                                                                               
MASKB    DC    B'11101000'         EMP                                          
         DC    B'11101011'         SDEP                                         
         DC    B'11101011'         SDEP                                         
         DC    B'11101011'         DEP                                          
         DC    B'11101011'         OFF                                          
         DC    B'11101011'         JOB                                          
         DC    B'11101011'         PRO                                          
         DC    B'11101011'         CLI                                          
         DC    X'00'               OFF                                          
         DC    X'00'               REP                                          
DETA     DC    B'11101010'                                                      
DETB     DC    B'11111000'         DETAIL ON B                                  
DETO     DC    X'FF'                                                            
DETP     DC    B'11111110'         OPTION P OR I JOB EMPLOYEE TOT               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BUFFALO CSECT                                                          
*        PROGPROF=N, KEY IS EMPLOYEE                                            
*        PROGPRIF=Y, KEY IS SUB DEPT                                            
*----------------------------------------------------------------------         
*                                                                               
         BUFF  LINES=1400,ROWS=1,COLUMNS=4,FLAVOR=PACKED,KEYLIST=(13,A)X        
               ,COMMENT=5                                                       
         EJECT                                                                  
**********************************************************************          
*        TABLES OF 1R NAME DATA, KEPT AT EACH LEVEL                             
*        TABLE IS-H(NUMBER IN TABLE), Y(MAX IT TABLE), C(TABLE DATA)            
*        TABLE DATA IS COVERED BY LISTD DSECT, BUILT BY BUILDLST                
**********************************************************************          
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
         DC    S(AOFFLST)                                                       
         DC    H'0'                                                             
         DC    Y(OFFMAX)                                                        
         DC    Y(0)                                                             
         DC    A(OFFSIZE)                                                       
*                                                                               
         DC    S(ADEPLST)                                                       
         DC    H'0'                                                             
         DC    Y(DEPMAX)                                                        
         DC    Y(1)                                                             
         DC    A(DEPSIZE)                                                       
*                                                                               
         DC    S(ASDELST)                                                       
         DC    H'0'                                                             
         DC    Y(SDEMAX)                                                        
         DC    Y(3)                                                             
         DC    A(SDESIZE)                                                       
*                                                                               
         DC    S(AEMPLST)                                                       
         DC    H'0'                                                             
         DC    Y(EMPMAX)                                                        
         DC    Y(5)                                                             
         DC    A(EMPSIZE)                                                       
*                                                                               
         DC    S(ACLOSTAB)                                                      
         DC    AL1(CLOSDLN)        LENGTH OF TABLE                              
         DC    AL1(L'CLOSKEY)      KEY LENGTH                                   
         DC    Y(CLOSMAX)                                                       
         DC    Y(0)                                                             
         DC    A(CLOSSIZE)                                                      
*                                                                               
         DC    S(ACREC)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)      KEY LENGTH                                           
         DC    Y(0)                                                             
         DC    Y(0)                                                             
         DC    A(ACRECLN)                                                       
MAINNUM  EQU   (*-MAINTAB)/MAINLEN                                              
EMPMAX   EQU   5500                                                             
EMPSIZE  EQU   6+(EMPMAX*LISTDLN)  6 BYTE HEADER                                
*                                                                               
SDEMAX   EQU   2500                                                             
SDESIZE  EQU   6+(SDEMAX*LISTDLN)                                               
*                                                                               
DEPMAX   EQU   360                                                              
DEPSIZE  EQU   6+(DEPMAX*LISTDLN)                                               
*                                                                               
OFFMAX   EQU   255                                                              
OFFSIZE  EQU   6+(OFFMAX*LISTDLN)                                               
*                                                                               
CLOSMAX  EQU   4000                                                             
CLOSSIZE EQU   BINLENQ+(CLOSMAX*CLOSDLN)                                        
*                                                                               
ACRECLN  EQU   2042                                                             
*                                                                               
BUFSIZE  EQU   EMPSIZE+SDESIZE+DEPSIZE+OFFSIZE+CLOSSIZE+ACRECLN                 
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
*                                                                               
ACR3D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ABUFC    DS    A                                                                
SORTER   DS    V                                                                
UNDERLIN DS    V                                                                
ACLIST   DS    V                                                                
SQUASHER DS    V                                                                
*                                                                               
ABUFF    DS    A                                                                
AOFFLST  DS    A                                                                
ADEPLST  DS    A                                                                
ASDELST  DS    A                                                                
AEMPLST  DS    A                                                                
ACREC    DS    A                                                                
ACLOSTAB DS    A                                                                
*                                                                               
ADBOX    DS    A                                                                
SAVERE   DS    A                                                                
NUMNON   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
DETMASK  DS    CL1                 PRINT MASK FOR DETAILS                       
LISTSW   DS    CL1                 IF WE HAVE A CLIENT LIST, CHECK IT           
NEWOFF   DS    CL1                 AGENCY IS ON NEW OFFICES                     
READFLAG DS    CL1                 NEED TO RESET DATAMGR                        
OFFSETS  DS    CL4                 OFFSETS                                      
MYMEND   DS    CL3                 MOS START                                    
MYMSTR   DS    CL3                 MOS END                                      
BILLED   DS    CL1                 FULLY BILLED SWITCH                          
STRDATE  DS    CL3                 START DATE PACKED YMD                        
ENDDATE  DS    CL3                 END DATE PACKED YMD                          
BILDATE  DS    CL3                 BILLING DATE PACKED YMD                      
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
M1START  DS    PL2                 MOS START DATE PACKED                        
M1END    DS    PL2                 MOS END DATE PACKED                          
CURMON   DS    CL3                 PACKED CURRENT MONTH                         
CURMONP  DS    CL6                 PRINT CURRENT MONTH                          
FRMMONP  DS    CL6                 PRINT "FROM" MONTH                           
ACCSTAT  DS    CL1                                                              
GOTACC   EQU   1                                                                
*                                                                               
HDOFF    DS    CL2                 SJ OFFICE CODE FOR HEADERS                   
HDOFFNM  DS    CL36                SJ OFFICE NAME                               
HDCLI    DS    CL3                 SJ CLIENT CODE FOR HEADERS                   
HDCLINM  DS    CL36                SJ CLIENT NAME                               
HDPRO    DS    CL4                 SJ PRODUC CODE OR =C'NONE'                   
HDPRONM  DS    CL36                SJ PRODUCT NAME                              
SVJOB    DS    CL6                 SJ JOB CODE FOR HEADERS                      
JOBNAME  DS    CL36                SJ JOB NAME                                  
*                                                                               
SAVEDATE DS    CL2                 MDATE                                        
SAVETRDT DS    CL3                 TRNSDATE                                     
SAVEHOUR DS    PL(BUCKLN)                                                       
SAVERATE DS    PL(BUCKLN)                                                       
SUMTOTAL DS    4PL(BUCKLN)                                                      
WORKBUCK DS    (NUMBUCKS)PL(BUCKLN)                                             
*                                                                               
CLIAFFIL DS    (NUMBUCKS)PL(BUCKLN)   NOTE THESE THREE MUST BE CONTIG.          
OFFAFFIL DS    (NUMBUCKS)PL(BUCKLN)                                             
REPAFFIL DS    (NUMBUCKS)PL(BUCKLN)                                             
*                                                                               
PL16     DS    PL16                FOR MULTIPLYING RATE                         
*                                                                               
ALSORT   DS    A                   A(LAST SORT RECORD)                          
SRTREC   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
THISREC  DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
SAVETYPE DS    CL1                                                              
PRTSTAT  DS    CL1                                                              
SUMMARY  EQU   1                                                                
BUFREC   DS    0C                                                               
BUFKEY   DS    0CL13                                                            
BUFTYPE  DS    CL1                                                              
SUMTYPLN EQU   *-BUFKEY                                                         
BUFACCT  DS    CL12                SUB DEPARTMENT                               
BUFDATA  DS    CL5                 COMMENT DATA (KEY TO LOOK UP NAME            
BUFBUCKS DS    4PL8                                                             
BUFRCLN  EQU   *-BUFREC                                                         
*                                                                               
LASTBUF  DS    CL(L'BUFKEY)        PREVIOUS BUFFALO KEY                         
REPSTAT  DS    CL1                                                              
MYKEY    DS    0CL49                                                            
MYCUL    DS    CL3                 COMPANY UNIT LEDGER                          
MYACCT   DS    CL12                ACCOUNT NUMBER                               
         ORG   MYKEY                                                            
         DS    CL49                KEY AREA                                     
*                                                                               
OFFLST   DS    CL(2*255)          OFFICE LIST STOLEN FROM ACMASTER              
REQTYPE  EQU   X'80'               REQUEST TOTAL BUFFALO RECORD                 
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                  **** FORM A SORT *****                       
SRTOFF   DS    CL2                 TRANSACTION OFFICE                           
SRTOFFLN EQU   *-SRTKEY                                                         
SRTSJ    DS    0CL12               SJ  ACCOUNT                                  
SRTCLI   DS    CL3                          -CLIENT                             
SRTTYPE  DS    CL1                 PROD REPORT RECORD OR JOB REPORT             
SRTCLILN EQU   *-SRTKEY                                                         
PRODTYPE EQU   1                   USED TO CONRTOL SORT ORDER                   
PRODJOB  EQU   2                   JOB AFFILIATED TIME      R                   
JOBTYPE  EQU   4                                                                
SRTPROD  DS    CL3                          -PRODUCT                            
SRTPROLN EQU   *-SRTKEY                                                         
SRTJOB   DS    CL6                          -JOB                                
SRTJOBLN EQU   *-SRTKEY                                                         
SRTSUB   DS    CL2                 SUB DEPT FOR SORT                            
SRTSDHLN EQU   *-SRTKEY                                                         
SRTEMPL  DS    0CL12               EMPLOYEE'S  1R ACCOUNT                       
SRTOFFC  DS    CL1                          -OFFICE                             
SRTEOFLN EQU   *-SRTKEY                                                         
SRTDEPT  DS    CL2                          -DEPARTMENT                         
SRTDEPLN EQU   *-SRTKEY                                                         
SRTCAT   DS    CL2                          -CATEGORY                           
SRTSDELN EQU   *-SRTKEY                                                         
SRTSTAF  DS    CL7                          -STAFF NUMBER                       
SRTEMPLN EQU   *-SRTKEY                                                         
SRTDATE  DS    CL3                 TIMESHEET DATE                               
SRTSUBR  DS    CL1                 TASK                                         
SRTKYLN  EQU   *-SRTKEY            SORT KEY LENGTH                              
SRTBUCKS DS    0C                  LOCATION OF BUCKETS                          
BUCKLN   EQU   8                   LENGTH OF BUCKETS                            
SRTHOUR  DS    PL(BUCKLN)          HOUR                                         
SRTRATE  DS    PL(BUCKLN)          RATE                                         
SRTAMNT  DS    PL(BUCKLN)          BILLABLE RATE                                
         DS    PL(BUCKLN)                                                       
         ORG   SRTBUCKS            REDEFINITION FOR CLIENT FEE REPORT           
SRTCHR   DS    PL(BUCKLN)          CURRENT HOURS                                
SRTCAMT  DS    PL(BUCKLN)          AMOUNT = HOURS X RATE                        
SRTYHR   DS    PL(BUCKLN)          YEAR TO DATE HOURS                           
SRTYAMT  DS    PL(BUCKLN)          YEAR TO DATE AMOUNT                          
SBUKCONT EQU   (*-SRTHOUR)/(L'SRTHOUR) NUMBER OF BUCKETS                        
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
**************************                                                      
* DSECT FOR LIST TABLE                                                          
**************************                                                      
LISTD    DSECT                                                                  
LISTREC  DS    0C                                                               
LISTKEY  DS    CL12                                                             
LISTNAME DS    CL36                                                             
LISTDLN  EQU   *-LISTD                                                          
         SPACE 2                                                                
**************************                                                      
* DSECT FOR CLOSED TABLE                                                        
**************************                                                      
CLOSD    DSECT                                                                  
CLOSREC  DS    0C                                                               
CLOSKEY  DS    CL12                                                             
CLOSED   DS    CL1                                                              
CLOSDLN  EQU   *-CLOSD                                                          
         SPACE 2                                                                
*-------------------------------------------------------------------*           
* DSECT FOR THE HEADER PORTION OF BINSRCH TABLES                    *           
*-------------------------------------------------------------------*           
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   REC LENGTH                                   
BINDISPK DS    0F                  DISP TO KEY                                  
BINDISP  DS    CL1                                                              
BINKEY   DS    CL3                                                              
BINMAX   DS    F                   MAX IN THIS TABLE                            
BINLENQ  EQU   *-BIND                                                           
BINTABLE DS    0CL1                TABLE DATA                                   
         SPACE 2                                                                
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
MAINLN   DS    AL1                 RECORD LENGTH IF BINSRCH                     
MAINKLN  DS    AL1                 KEY LENGTH, IF BINSRCH                       
MAINMAX  DS    Y                                                                
MAINSIG  DS    Y                   OFFSET IN KEY OF THIS LEVEL                  
MAINSIZE DS    A                                                                
MAINLEN  EQU   *-MAIND                                                          
         EJECT                                                                  
**************************                                                      
* DSECT FOR SUBTOTAL TABLE                                                      
**************************                                                      
TOTTBLD  DSECT                                                                  
TOTLEV   DS    AL1                 SUB TOTAL LEVEL                              
TOTLEN   DS    AL1                 LENGTH OF THIS LEVEL IN SORT KEY             
TOTTYPE  DS    CL12                SUB TOTAL TYPE                               
TOTBUCKS DS    0C                  A(FIRST BUCKET)                              
         DS    (BUCKLN)P                                                        
         DS    (BUCKLN)P                                                        
         DS    (BUCKLN)P                                                        
         DS    (BUCKLN)P                                                        
TOTMASK  DS    CL1                 PRINT MASK                                   
TOTLIST  DS    SL2                 ADDRESS OF LIST OF THESE NAMES               
TOTWANT  DS    AL1                 PRINT ON JOB OR PROD REPORT                  
TOTNEED  DS    AL1                 DO I NEED TO PRINT SUBTOTS                   
TOTDATA  DS    CL7                 DATA TO PRINT IN TOTAL FOR LINE              
TOTTBLLN EQU   *-TOTTBLD                                                        
         EJECT                                                                  
ACR302   CSECT                                                                  
*                                                                               
         ENTRY RECORD                                                           
RECORD   DS    0D                  DATAMRG AREA                                 
         DS    CL42                KEY                                          
         DS    CL2000              DATA                                         
*                                                                               
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBOXEQUS                                                              
*        DDREMOTED                                                              
*        ACOFFALD                                                               
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACREPR302S05/01/02'                                      
         END                                                                    
