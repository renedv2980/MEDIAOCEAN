*          DATA SET ACREPGF02  AT LEVEL 038 AS OF 04/10/15                      
*PHASE ACGF02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'ACGF02 - BILLING INTERFACE FOR GENERAL FOODS'                   
**********************************************************************          
* HISTORY                                                            *          
* 06/97 - CHANGE SORT TO SORT ON AGENCY                              *          
* 04/98 - EXCLUDE FUTURES (TYPE X) AS PER HWEI                       *          
* 07/17/02 - CODE FOR CLT KRN FOR YNRA GIVE NATURAL 440 NOT PER JOB  *          
*            BUT ONE PER INVOICE                                     *          
**********************************************************************          
         SPACE 1                                                                
ACGF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACGF**,R9                                                    
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACGFD,RC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   MODE,RUNFRST        Q, VERY FIRST TIME AROUND                    
         BE    RUNF                 Y, DO RUN INITIALIZATION                    
         CLI   MODE,REQFRST        Q, FIRST TIME FOR A REQUEST                  
         BE    REQF                 Y, DO REQUEST INITIALIZATION                
         CLI   MODE,PROCACC        Q, AN ACCOUNT RECORD                         
         BE    PACC                 Y, PROCESS ACCOUNT LEVEL RECORD             
         CLI   MODE,PROCTRNS       Q, A TRANSACTION                             
         BE    PTRN                 Y, PROCESS TRANSACTION/ELEMENTS             
         CLI   MODE,ACCLAST        Q, END OF AN ACCOUNT                         
         BE    ACCL                 Y, FINISH UP ACCOUNT DATA                   
         CLI   MODE,REQLAST        Q, END OF REQUEST                            
         BE    REQL                 Y, SORT, PRINT REPORTS & WRITE TAPE         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,=A(HDHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         DROP  R2                                                               
*                                                                               
         USING ACMD,R3                                                          
         L     R3,AMONACC          SET FOR JOBBER CALL                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         XC    OUTCNT,OUTCNT                                                    
         MVC   DSPARM+13(2),ALPHAID                                             
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         MVC   QEND3,=XL3'FF'      INIT DATES                                   
         MVC   QEND2,=XL3'FF'                                                   
         XC    QSTR3,QSTR3                                                      
         XC    QSTR2,QSTR2                                                      
         CLC   QSTART(L'QSTART+L'QEND),SPACES  ANY START OR END                 
         BE    REQF20                                                           
         CLC   QSTART,SPACES                                                    
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTR3)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(2,QSTR2)                                 
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(2,QEND2)                                   
*                                                                               
         USING AGYTABD,R2                                                       
REQF20   MVC   SVAGY,SPACES                                                     
         NI    FLAG,X'FF'-FLGG8K8                                               
         LA    R2,AGYTAB                                                        
         LA    R0,AGYTABNM                                                      
REQF30   CLC   AGYALPHA,ALPHAID                                                 
         BE    *+12                                                             
         LA    R2,AGYTABQ(R2)                                                   
         BCT   R0,REQF30                                                        
*                                                                               
         MVC   SVAGY,AGYCODE                                                    
         TM    AGYSTAT,AGYG8K8     WANT TO READ G8,K8 FOR THIS AGY              
         BNO   *+8                                                              
         OI    FLAG,FLGG8K8                                                     
*                                                                               
         XC    IOSW,IOSW                                                        
         ZAP   TPCT,=P'0'                                                       
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS AN ACCOUNT                                                 *          
*    VALIDATE AN ACCOUNT FOR TAPE                                    *          
*    READ USER FIELDS                                                *          
*    WRITE ESTIMATE RECORDS TO SORT (AS 'X' TYPE)                    *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R4                                                          
PACC     L     R4,DRTAB            CLEAR TABLE AT START OF EACH ACCOUNT         
         XC    BININ,BININ                                                      
         USING TABLED,R4                                                        
         L     R4,BILLTAB                                                       
         XC    TABNUM,TABNUM                                                    
         L     R4,WCTAB                                                         
         XC    TABNUM,TABNUM                                                    
         DROP  R4                                                               
*                                                                               
         ZAP   PKCOM,=P'0'         CLEAR COMM ACCUMLTR PER INV                  
         XC    SVSRTP,SVSRTP       CLEAR SAVED AREA FOR SORT                    
*                                                                               
         MVI   GOTABILL,C'N'       ASSUME NO BILLING ON JOB                     
         MVI   GOTTOTAL,C'N'      TOTAL BILLS EITHER                            
         MVC   SRTPREC,SPACES                                                   
         USING SRTPRECD,R5                                                      
         LA    R5,SRTPREC                                                       
*                                                                               
         USING ACKEYD,R7                                                        
         L     R7,ADACC            POINTS TO ACCOUNT RECORD                     
         MVC   SRTPACCT,ACKEYACC+3 NO CUL                                       
         GOTO1 DATCON,DMCB,(0,QEND),(X'20',WORK)                                
         MVC   SRTPMON,WORK       REQUEST MONTH                                 
         MVC   SRTPAGY,SVAGY                                                    
*                                                                               
* SPECIAL BULLSHIT AS PER TGERDDNY, 9/6/94                                      
*                                                                               
         CLC   ALPHAID,=C'YN'      IS THIS Y+R                                  
         BNE   PACC20              NO                                           
         CLC   ACKEYACC+3(3),=C'KRB'                                            
         BE    PACC10                                                           
         CLC   ACKEYACC+3(3),=C'KRX'                                            
         BE    PACC10                                                           
         CLC   ACKEYACC+3(3),=C'KRN'                                            
         BNE   PACC20                                                           
*                                                                               
PACC10   MVI   SRTPAGY,C'5'                                                     
*                                                                               
PACC20   MVI   FCRDTRNS,C'N'       ASSUME JOB WILL BE REJECTED                  
*                                                                               
         MVI   SRTPMOS,C'*'        INIT USER FIELD VALS W/*'S                   
         MVI   SRTPCOMP,C'*'                                                    
         MVI   SRTPDIV,C'*'                                                     
         MVI   SRTPG4,C'*'                                                      
         MVI   SRTPPROD,C'*'                                                    
*                                                                               
         USING ACPEELD,R7                                                       
         MVI   ELCODE,X'33'       EXCLUDE PEELED JOBS                           
         L     R7,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   PACC30                                                           
         OC    ACPEPLDT,ACPEPLDT                                                
         BNZ   PACCX                                                            
*                                                                               
PACC30   MVI   ELCODE,X'A2'                                                     
         L     R7,ADACC                                                         
         BAS   RE,GETEL                                                         
PACC40   BNE   PACCX                                                            
*                                                                               
         USING ACUFD,R7                                                         
         CLC   ACUFCODE,=C'G5'    TAPE=Y MUST BE SET TO                         
         BNE   PACC50             WRITE THIS JOB TO TAPE                        
         CLI   ACUFDATA,C'Y'                                                    
         BNE   PACCX                                                            
         MVI   FCRDTRNS,C'Y'                                                    
         B     PACC100                                                          
*                                                                               
PACC50   CLC   ACUFCODE,=C'K5'    TAPE=Y MUST BE SET TO                         
         BNE   PACC60             WRITE THIS JOB TO TAPE                        
         CLI   ACUFDATA,C'Y'                                                    
         BNE   PACCX                                                            
         MVI   FCRDTRNS,C'Y'                                                    
         B     PACC100                                                          
*                                                                               
PACC60   LA    R6,L'SRTPMOS-1      BILL MONTH                                   
         LA    R8,SRTPMOS                                                       
*                                                                               
         TM    FLAG,FLGG8K8        DO WE WANT TO READ G8 AND K8 INSTEAD         
         BNO   PACC70                                                           
         CLC   ACUFCODE,=C'G8'                                                  
         BE    PACC90                                                           
         CLC   ACUFCODE,=C'K8'                                                  
         BE    PACC90                                                           
         B     PACC80                                                           
*                                                                               
PACC70   CLC   ACUFCODE,=C'G1'                                                  
         BE    PACC90                                                           
         CLC   ACUFCODE,=C'K1'                                                  
         BE    PACC90                                                           
*                                                                               
PACC80   LA    R6,L'SRTPCOMP+L'SRTPDIV-1  COMP/DIV                              
         LA    R8,SRTPCOMP                                                      
         CLC   ACUFCODE,=C'G2'     ADVERTISING DIVISION                         
         BE    PACC90                                                           
         CLC   ACUFCODE,=C'K2'     ADVERTISING DIVISION                         
         BE    PACC90                                                           
*                                                                               
         LA    R6,L'SRTPPROD-1                                                  
         LA    R8,SRTPPROD                                                      
         CLC   ACUFCODE,=C'G3'     PRODUCT CODE                                 
         BE    PACC90                                                           
         CLC   ACUFCODE,=C'K3'     PRODUCT CODE                                 
         BE    PACC90                                                           
*                                                                               
         LA    R6,L'SRTPG4-1                                                    
         LA    R8,SRTPG4                                                        
         CLC   ACUFCODE,=C'G4'     NATURAL                                      
         BE    PACC90                                                           
         CLC   ACUFCODE,=C'K4'     NATURAL                                      
         BNE   PACC100                                                          
*                                                                               
PACC90   ZIC   R2,ACUFLEN                                                       
         SH    R2,=H'33'                                                        
         BM    PACC100                                                          
         CR    R2,R6               Q, ACTUAL LENGTH GREATER THAN MAX.           
         BNH   *+6                                                              
         LR    R2,R6                Y, ALLOW ONLY THE MAX. LENGTH               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ACUFDATA                                                 
         MVC   SVPG4,SRTPG4        SAVE OFF NATURAL CODE                        
PACC100  BAS   RE,NEXTEL                                                        
         B     PACC40                                                           
*                                                                               
PACCX    B     EXIT                                                             
         DROP  R5,R7                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS A TRANSACTION                                              *          
**********************************************************************          
         SPACE 1                                                                
         USING ACKEYD,R6                                                        
PTRN     L     R6,ADTRANS                                                       
         SH    R6,DATADISP                                                      
         USING TRANSD,R7                                                        
         L     R7,ADTRANS          GETS ADDRESS OF TRAN ELEMENT                 
         CLI   TRNSEL,X'44'        Q, TRAN ELEMENT-44, ALWAYS FIRST             
         BNE   PTRNX                N, EXIT---BAD TRAN                          
         CLC   TRNSANAL,=C'**'     NO P/O'S                                     
         BE    PTRNX                                                            
         CLC   TRNSANAL,=C'99'     BUILD TABLE IF WORK CODE NOT '99'            
         BE    PTRN100                                                          
*                                                                               
* PROCESS MARKED CHARGES ON THE JOB, SAVE TO DETERMINE WORK                     
* CODE VALUES FOR BILLS                                                         
*                                                                               
         USING DRTABD,R5                                                        
         LA    R5,DRTABREC                                                      
*                                                                               
         USING TRANSD,R7                                                        
         L     R7,ADTRANS                                                       
         MVC   DRTWC,TRNSANAL                                                   
         MVC   DRTSTAT,TRNSSTAT                                                 
         ZAP   DUB,TRNSAMNT        HOLD TRANS AMOUNT FOR 49 ELEMENTS            
*                                                                               
         ZAP   DRTCD,=P'0'                                                      
         ZAP   DOUBLE,=P'0'                                                     
         MVI   ELCODE,X'50'        LOOK FOR CD                                  
PTRN10   BAS   RE,NEXTEL                                                        
         BNE   PTRN20              NO CD ON TRAN                                
*                                                                               
         USING TRCASHD,R7                                                       
         CLI   TRCSTYPE,C'D'                                                    
         BNE   PTRN10              LOOK FOR NEXT 50 EL                          
         ZAP   DRTCD,TRCSAMNT      SAVE CD FOR CALCING COMMISSION               
*                                                                               
PTRN20   MVI   ELCODE,PTAELQ                                                    
         L     R3,AMONACC                                                       
         L     R7,ACMAPRO2         POINT TO FIRST 77 ELEM                       
         CLI   0(R7),PTAELQ                                                     
         B     PTRN40                                                           
*                                                                               
PTRN30   BAS   RE,NEXTEL                                                        
PTRN40   BNE   PTRN90                                                           
         USING PTAELD,R7                                                        
         TM    PTASTAT1,PTASREVU   THIS BILL REVERSED?                          
         BO    PTRN50                                                           
         TM    PTASTAT1,PTASREVS                                                
         BO    PTRN60                                                           
         CLI   PTATYPE,PTATRAL     IS THIS A BILLING ELEMENT                    
         BNE   PTRN30                                                           
         TM    PTASTAT1,PTASPEND   IS IT PENDING?                               
         BO    PTRN30                                                           
*        CLC   PTARBLDT,QEND2      FILTER BILL ON DATE                          
*        BH    PTRN30              STILL BILLABLE FOR THIS REQUEST              
         B     PTRN70                                                           
*                                                                               
PTRN50   MVC   DRTNUM,=6X'EE'      EE TO INVOICE                                
         MVC   DRTDATE,PTARDATE    SAVE ORIGINAL BILL DATE                      
         ZAP   DRTAMNT,DUB           TRNSAMNT                                   
         MVI   COMMSTAT,NOTSET                                                  
         BAS   RE,ADDIT              ADD TO BINTABLE                            
         B     PTRN30                                                           
*                                                                               
PTRN60   MVC   DRTDATE,PTADATE     SAVE DATE REVERSED                           
         MVC   DRTNUM,=6X'FF'      FF TO INVOICE                                
         ZAP   DRTAMNT,DUB         TRNSAMNT,                                    
         MP    DRTAMNT,=P'-1'      'REVERSED'                                   
         MVI   COMMSTAT,NOTSET                                                  
         BAS   RE,ADDIT             ADD TO BINTABLE                             
         B     PTRN30                                                           
*                                                                               
PTRN70   MVC   DRTNUM,PTARBLNO     MOVE INVOICE AND AMOUNT TO TABLE             
         MVC   DRTDATE,PTARBLDT                                                 
         ZAP   DRTAMNT,PTANET                                                   
         AP    DOUBLE,PTANET       KEEP TOTAL BILLED ON THIS TRANS              
*                                                                               
         ZAP   DRTCOM,=P'0'                                                     
         TM    DRTSTAT,X'01'       NON COMM CHARGE                              
         BO    PTRN80              YES                                          
         ZAP   PL16,PTANET                                                      
         AP    PL16,DRTCD                                                       
         MP    PL16,PTARCORT                                                    
         SRP   PL16,64-6,5                                                      
         ZAP   DRTCOM,PL16                                                      
*                                                                               
PTRN80   MVI   COMMSTAT,FROM4B                                                  
         BAS   RE,ADDIT                                                         
         B     PTRN30                                                           
*                                                                               
PTRN90   OC    ACDTUSED,ACDTUSED   FULLY BILLED?                                
         BZ    PTRNX               NO                                           
*                                                                               
         CP    DOUBLE,=P'0'        DID I GET AN AMOUNT IN THE 4B                
         BNE   PTRNX               YES                                          
         CLC   ACDTUSED,QEND2      BILLED WITHIN REQUEST RANGE                  
         BH    PTRNX               NO, CONSIDER UNBILLED                        
         CLC   ACDTUSED,QSTR2      BILLED PRIOR                                 
         BL    PTRNX               YES, DON'T USE                               
*                                                                               
         USING TRANSD,R7                                                        
         L     R7,ADTRANS          IF ACDTUSED AND ZERO 4B'S USE TRNS           
         ZAP   DRTAMNT,TRNSAMNT                                                 
         MVC   DRTDATE,ACDTUSED                                                 
         MVI   COMMSTAT,NOTSET                                                  
         BAS   RE,ADDIT                                                         
         B     PTRNX               GET NEXT TRANSACTION                         
         DROP  R5,R6,R7                                                         
*                                                                               
* PROCESS BILLING, WRITE RECORDS TO TAPE                                        
*                                                                               
         USING SRTPRECD,R4                                                      
PTRN100  XC    TESTBILL,TESTBILL                                                
*                                                                               
         USING TRANSD,R7                                                        
         L     R7,ADTRANS          FIND OUT WHAT TYPE OF BILL THIS IS           
         CLI   TRNSNARR,C'P'                                                    
         BNE   *+12                                                             
         OI    TESTBILL,PROGRESS+MARKED                                         
         B     PTRN130                                                          
*                                                                               
         CLI   TRNSNARR,C'C'                                                    
         BNE   *+12                                                             
         OI    TESTBILL,CLIENT+MARKED                                           
         B     PTRN130                                                          
*                                                                               
         CLI   TRNSNARR,C'T'                                                    
         BNE   *+12                                                             
         OI    TESTBILL,TOTAL+MARKED                                            
         B     PTRN130                                                          
*                                                                               
         CLI   TRNSNARR,C'S'                                                    
         BNE   *+12                                                             
         OI    TESTBILL,SPECIAL                                                 
         B     PTRN130                                                          
*                                                                               
         CLI   TRNSNARR,C'O'                                                    
         BNE   *+12                                                             
         OI    TESTBILL,ONELINE+MARKED+TOTAL                                    
         B     PTRN130                                                          
*                                                                               
         CLI   TRNSNARR,C'A'                                                    
         BNE   *+12                                                             
         OI    TESTBILL,ALLOC+MARKED                                            
         B     PTRN130                                                          
*                                                                               
         LA    R0,8                                                             
         LA    R1,TRNSNARR+1                                                    
PTRN110  CLC   0(4,R1),=C'PC E'                                                 
         BE    PTRN120                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,PTRN110                                                       
*                                                                               
PTRN120  OI    TESTBILL,PCTEST                                                  
*                                                                               
PTRN130  MVC   SAVETYPE,TRNSNARR                                                
         TM    TESTBILL,PCTEST                                                  
         BNO   *+8                                                              
         MVI   SAVETYPE,C'E'                                                    
*                                                                               
         TM    TESTBILL,TOTAL                                                   
         BNO   *+8                                                              
         MVI   SAVETYPE,C'F'                                                    
*                                                                               
         L     R7,ADTRANS                                                       
         LA    R4,SRTPREC                                                       
         XC    SVDT2,SVDT2                                                      
         XC    SVDT3,SVDT3                                                      
         MVI   ELCODE,X'60'        LOOK FOR STATUS ELEMENT-60                   
         BAS   RE,NEXTEL           GETS STATUS ELEMENT---MAYBE                  
         BNE   PTRN140             BRANCH IF NO STATUS ELEMENT FOUND            
         USING TRSTATD,R7          STATUS DSECT - ELEMENT-60                    
         MVC   SVDT2,TRSTDATE                                                   
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,SVDT3) SAVE STATUS DATE              
PTRN140  CLC   SVDT2,QEND2         Q, TEST AGAINST END DATE                     
         BH    PTRNX                HIGH SVDT2, BYPASS TRAN                     
*                                                                               
         MVI   GOTABILL,C'Y'                                                    
         TM    TESTBILL,MARKED    DID BILL MARK                                 
         BNZ   PTRN150            YES                                           
         USING BILLD,R2                                                         
         USING TRANSD,R7                                                        
         LA    R2,BILLAREA         SAVE BILL INCASE I NEED TO REVERSE           
         L     R7,ADTRANS          DUE TO SUBSEQUENT PROG/TOT BILL              
         MVC   BILLTYPE,SAVETYPE                                                
         MVC   BILLREF,SPACES      WILL USE REF NUM OF MARKING BILL             
         MVC   BILLDATE,TRNSDATE                                                
         ZAP   BILLNET,TRNSAMNT                                                 
         ZAP   BILLCOM,TRNSNARR+15(6)                                           
         BAS   RE,BILLSAVE                                                      
*                                                                               
PTRN150  CLC   SVDT2,QSTR2        TEST DATE AGAIN                               
         BNL   PTRN160            INCLUDE BILL                                  
         TM    TESTBILL,MARKED    IS THIS A MARKING BILL                        
         BZ    PTRNX               NO                                           
         USING TABLED,R2                                                        
         L     R2,BILLTAB          CLEAR OLD NON MARKINBG BILLS                 
         XC    TABNUM,TABNUM       (REVERSED ON PREVIOUS TAPE)                  
         B     PTRNX                                                            
*                                                                               
         USING TRANSD,R7                                                        
PTRN160  L     R7,ADTRANS               PUT BILL RECORD TO TAPE                 
         ZAP   SVNET,TRNSAMNT                                                   
         ZAP   SVCOM,TRNSNARR+15(6)                                             
*                                                                               
         TM    TESTBILL,MARKED     DID THIS MARK FILE?                          
         BZ    PTRN170             NO                                           
         BAS   RE,GETWCDET         YES, GET WC DETAILS                          
*                                                                               
         TM    TESTBILL,TOTAL      TOTAL BILL?                                  
         BZ    PTRNX               NO, WC TOTALS ARE ENOUGH                     
*                                                                               
         SP    SVNET,WCNET         FOR TOTAL BILLS PUT OUT AN ADJUST            
         SP    SVCOM,WCCOM         FOR ANY PC EST BILLING THAT MIGHT            
*                                  HAVE BEEN ON THE FILE                        
         CP    SVNET,=P'0'         ... PROVIDING ITS NEEDED                     
         BNE   PTRN170                                                          
         CP    SVCOM,=P'0'                                                      
         BE    PTRNX                                                            
*                                                                               
         USING SRTD,R2                                                          
PTRN170  L     R7,ADTRANS               PUT BILL RECORD TO TAPE                 
         LA    R2,SORTAREA                                                      
         MVC   SRTWC,=C'000'                                                    
         MVC   SRTREF,TRNSREF                                                   
         MVC   SRTTYPE,SAVETYPE                                                 
         ZAP   SRTNET,SVNET                                                     
         ZAP   SRTCOM,SVCOM                                                     
         BAS   RE,PUTSORT                                                       
*                                                                               
PTRNX    B     EXIT                                                             
         DROP  R2,R4,R7                                                         
         EJECT                                                                  
**********************************************************************          
* ADD AN ENTRY TO DRTAB                                              *          
**********************************************************************          
         SPACE 1                                                                
ADDIT    NTR1                                                                   
         USING DRTABD,R5                                                        
         CLI   COMMSTAT,FROM4B     BILLED COMMISSION FROM 4B                    
         BE    ADDIT50             YES, DRTCOM IS SET                           
*                                                                               
         ZAP   DRTCOM,=P'0'                                                     
         TM    DRTSTAT,X'01'       NON COMM CHARGE                              
         BO    ADDIT50             YES                                          
*                                                                               
         USING TABLED,R4                                                        
         L     R4,WCTAB            TABLE OF WORK CODE COMMISISONS               
         LH    R0,TABNUM                                                        
         LA    R3,TABDATA                                                       
         LTR   R0,R0               ANYTHING SAVED HERE                          
         BZ    ADDIT20             NO, CALL GETOPT FOR RATE                     
         CH    R0,TABMAX           AM I AT MAX FOR TABLE                        
         BL    *+6                 NO, THERE IS STILL ROOM                      
         DC    H'0'                                                             
*                                                                               
         USING WCTABD,R3                                                        
ADDIT10  CLC   WCTBCODE,DRTWC                                                   
         BE    ADDIT30                                                          
         LA    R3,WCTABLN(R3)                                                   
         BCT   R0,ADDIT10                                                       
*                                  RATE FOR WORK CODE NOT FOUND                 
ADDIT20  L     R6,ADGOBLOC         GET RATE FOR THIS WORKCODE                   
         USING GETOPTD,R6                                                       
         MVC   GOSELWC,DRTWC                                                    
         GOTO1 GETOPT,DMCB,GETOPTD   GET WORK CODE RATE                         
         XC    GOSELWC,GOSELWC       RESET GETOPT                               
         ZAP   WCTBRATE,GOAGYCOM                                                
         MVC   WCTBCODE,DRTWC                                                   
*                                                                               
         LH    R1,TABNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,TABNUM                                                        
*                                                                               
ADDIT30  ZAP   PL16,DRTAMNT                                                     
         AP    PL16,DRTCD          PUT BACK CD TO CALC COMMOSSOPN               
         CLC   DRTNUM,=6X'FF'      IS THIS A REVERSAL ENTRY                     
         BNE   ADDIT40                                                          
         SP    PL16,DRTCD          PUT BACK CD REVERSED                         
         SP    PL16,DRTCD                                                       
*                                                                               
ADDIT40  MP    PL16,WCTBRATE                                                    
         SRP   PL16,64-6,5                                                      
         ZAP   DRTCOM,PL16                                                      
ADDIT50  GOTO1 BINADD,DMCB,(R5),DRTAB                                           
         B     EXIT                                                             
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
**********************************************************************          
* LAST FOR ACCOUNT                                                   *          
*     SEE IF THERE ARE ANY PC EST BILLS FOUND AFTER                  *          
*     TOTAL BILLS, AND REVERSE THEM ALSO                             *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R3                                                          
ACCL     DS    0H                                                               
         OC    SVSRTP,SVSRTP       ARE WE DOING YNRA CLT=KRN                    
         BZ    ACCL20                                                           
*                                                                               
         USING SRTPRECD,R1                                                      
         LA    R1,SRTPREC                                                       
         MVC   SRTPREC,SVSRTP      RESTORE SRTPREC                              
         ZAP   SRTPCOM,PKCOM       FILL IN  COMMISSION FOR LAST INV             
         CP    SRTPCOM,=P'0'       IS COMMISSION DOLLAR ZERO                    
         BE    ACCL20              DON'T ADD TO SORTER                          
         DROP  R1                                                               
*                                                                               
         TM    IOSW,SRTOPEN                                                     
         BO    ACCL10                                                           
         OI    IOSW,SRTOPEN                                                     
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
ACCL10   GOTO1 SORTER,DMCB,=C'PUT',SRTPREC                                      
*                                                                               
ACCL20   CLI   GOTABILL,C'Y'       IS THERE BILLING ON THIS JOB                 
         BE    ACCLX               YES, DON'T PRODUCE 'X' RECORDS               
*                                                                               
         USING ACPEELD,R7                                                       
         MVI   ELCODE,X'33'       EXCLUDE PEELED JOBS                           
         L     R7,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   ACCL30                                                           
         OC    ACPEPLDT,ACPEPLDT                                                
         BNZ   ACCLX                                                            
*                                                                               
ACCL30   BAS   RE,LOOKUP                                                        
         USING SRTD,R2                                                          
         USING JBCOLD,R3                                                        
         USING JBLOCKD,R5                                                       
         LA    R2,SORTAREA                                                      
         MVC   SRTWC,=C'000'                                                    
         MVC   SRTREF,SPACES                                                    
         MVI   SRTTYPE,C'X'                                                     
         ZAP   SRTNET,=P'0'                                                     
         ZAP   SRTCOM,=P'0'                                                     
         LH    R1,JBNROWS                                                       
*                                                                               
ACCL40   CLI   JBCOLTYP,JBCOLTJB   GET TOTAL JOB ROW                            
         BNE   ACCL50                                                           
         AP    SRTNET,JBCOLVAL(6)                                               
         AP    SRTCOM,JBCOLVAL+6(6)                                             
ACCL50   AH    R3,JBLCOL                                                        
         BCT   R1,ACCL40                                                        
*                                                                               
         CP    SRTNET,=P'0'        ANY ESTIMATE FOUND                           
         BE    ACCLX               NO                                           
         BAS   RE,PUTSORT                                                       
*                                                                               
ACCLX    B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* LAST FOR REQUEST                                                   *          
**********************************************************************          
         SPACE 1                                                                
REQL     TM    IOSW,SRTOPEN        Q, ANY INUT RECORDS                          
         BZ    REQLX                N, EXIT                                     
         USING TOTTABD,R2                                                       
         L     R2,TOTTAB                                                        
         LA    R0,TOTNUM                                                        
REQL10   ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTCOM,=P'0'                                                     
         LA    R2,TOTLEN(R2)                                                    
         BCT   R0,REQL10                                                        
*                                                                               
         ZAP   XNET,=P'0'                                                       
         ZAP   XCOM,=P'0'                                                       
         ZAP   BNET,=P'0'                                                       
         ZAP   BCOM,=P'0'                                                       
         XC    SRTPREC,SRTPREC                                                  
*                                                                               
REQL20   MVC   LASTREC,SRTPREC                                                  
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R5,15,DMCB+4        LAST RECORD FROM SORT                        
         BZ    REQL50                                                           
         MVC   SRTPREC(SRTPRECL),0(R5)                                          
*                                                                               
         CLC   LASTREC,SRTPREC     DO I NEED TOTALS                             
         BE    SRT060                                                           
         BAS   RE,TOTALS                                                        
*                                                                               
         USING SRTPRECD,R5                                                      
SRT060   LA    R5,SRTPREC                                                       
         USING PRINTD,R6                                                        
         LA    R6,P                                                             
         OC    LSTAGY,LSTAGY       FIRST TIME THROUGH?                          
         BZ    *+18                                                             
         CLC   LSTAGY,SRTPAGY      SAME AGENCY AS LAST?                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       YES FORCE NEW PAGE                           
*                                                                               
         MVC   HDAGY,SRTPAGY       SAVE AGENCY FOR HEADLINES                    
         MVC   PRCOMP,SRTPCOMP                                                  
         MVC   PRDIV,SRTPDIV                                                    
         MVC   PRPROD,SRTPPROD                                                  
         MVC   PRACCT,SRTPACCT                                                  
         MVC   PRMOS,SRTPMOS                                                    
         MVC   PRINVNO,SRTPINVN                                                 
         MVC   PRTYPE,SRTPTYPE                                                  
         MVC   PRG4,SRTPG4                                                      
         MVC   PRWC,SRTPWC                                                      
*                                                                               
         USING TOTTABD,R2                                                       
         L     R2,TOTTAB           BUMP AMOUNTS INTO THE TABLE                  
         AP    TOTNET,SRTPNET                                                   
         AP    TOTCOM,SRTPCOM                                                   
*                                                                               
         MVC   BUFCOMP,SRTPCOMP                                                 
         ZAP   BUFCOM,SRTPCOM                                                   
         ZAP   BUFNET,SRTPNET                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVI   BUFCOMP,X'FF'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
         CLI   SRTPTYPE,C'X'       ESTIMATE REC                                 
         BNE   REQL30              MUST BE BILLING                              
         AP    XNET,SRTPNET                                                     
         AP    XCOM,SRTPCOM                                                     
         B     REQL40                                                           
*                                                                               
REQL30   AP    BNET,SRTPNET                                                     
         AP    BCOM,SRTPCOM                                                     
*                                                                               
REQL40   ZAP   DOUBLE,SRTPNET                                                   
         ZAP   GROSS,SRTPNET                                                    
         UNPK  SRTPNET,DOUBLE                                                   
         LA    R3,PRNET                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,SRTPCOM                                                   
         AP    GROSS,SRTPCOM                                                    
         UNPK  SRTPCOM,DOUBLE                                                   
         LA    R3,PRCOM                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,GROSS                                                     
         LA    R3,PRGROSS                                                       
         BAS   RE,PRTAMNT                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
         BAS   RE,WRTP                                                          
         B     REQL20                                                           
*                                                                               
REQL50   GOTO1 SORTER,DMCB,=C'END' END THE SORT                                 
         NI    IOSW,X'FF'-SRTOPEN                                               
*                                                                               
         XC    SRTPREC,SRTPREC                                                  
         BAS   RE,TOTALS                                                        
         USING SRTPRECD,R5                                                      
         LA    R5,SRTPREC          PUT TRAILER RECORD TO TAPE                   
         MVI   SRTPREC,C'9'        FILL WITH 9'S                                
         MVC   SRTPREC+1(L'SRTPREC-1),SRTPREC                                   
         MVI   SRTPTYPE,C'T'                                                    
         MVC   SRTPAGY,LSTAGY                                                   
         GOTO1 DATCON,DMCB,(0,QEND),(X'20',WORK)                                
         MVC   SRTPMON,WORK                                                     
         USING TOTTABD,R2                                                       
         L     R2,TOTTAB           GET REPORT TOTAL                             
         LA    R0,TOTNUM                                                        
*                                                                               
REQL60   CLI   TOTLEV,X'FF'        REPORT TOTAL LEVEL                           
         BE    REQL70                                                           
         LA    R2,TOTLEN(R2)                                                    
         BCT   R0,REQL60                                                        
         DC    H'0'                                                             
*                                                                               
REQL70   UNPK  SRTPNET,TOTNET                                                   
         UNPK  SRTPCOM,TOTCOM                                                   
         BAS   RE,WRTP                                                          
*                                                                               
         CLI   QOPT1,C'Y'          Q, TAPE OUTPUT OPTION                        
         BNE   REQL80               N, DON'T CLOSE TAPE OUTPUT                  
         NI    IOSW,X'FF'-TPOPEN    Y, CLOSE TAPE OUTPUT FILE                   
         CLOSE (OUTP)                                                           
*                                                                               
REQL80   MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         MVC   PRWC(9),=C'BILLING $'                                            
         ZAP   DOUBLE,BNET                                                      
         ZAP   GROSS,BNET                                                       
         LA    R3,PRNET                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,BCOM                                                      
         AP    GROSS,BCOM                                                       
         LA    R3,PRCOM                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,GROSS                                                     
         LA    R3,PRGROSS                                                       
         BAS   RE,PRTAMNT                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PRWC(10),=C'ESTIMATE $'                                          
         ZAP   DOUBLE,XNET                                                      
         ZAP   GROSS,XNET                                                       
         LA    R3,PRNET                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,XCOM                                                      
         AP    GROSS,XCOM                                                       
         LA    R3,PRCOM                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,GROSS                                                     
         LA    R3,PRGROSS                                                       
         BAS   RE,PRTAMNT                                                       
         MVI   SPACING,3                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         XC    BUFKEY,BUFKEY       PRODUCE COMPANY SUMMARY                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         LA    R6,P                                                             
         USING PRINTD,R6                                                        
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),BUFREC,1                        
*                                                                               
REQL90   CLI   DMCB+8,X'80'                                                     
         BO    REQLX                                                            
         CLI   BUFCOMP,X'FF'       TOTAL RECORD FROM BUFFALO                    
         BE    REQL100                                                          
*                                                                               
         MVC   PRCOMP,BUFCOMP                                                   
         ZAP   DOUBLE,BUFNET                                                    
         ZAP   GROSS,BUFNET                                                     
         LA    R3,PRSUMNET                                                      
         BAS   RE,PRTAMNT                                                       
         ZAP   DOUBLE,BUFCOM                                                    
         AP    GROSS,BUFCOM                                                     
         LA    R3,PRSUMCOM                                                      
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,GROSS                                                     
         LA    R3,PRSUMGRS                                                      
         BAS   RE,PRTAMNT                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',(0,ADBUFC),BUFREC,1                         
         B     REQL90                                                           
*                                                                               
REQL100  MVC   PRCOMP(5),=C'TOTAL'                                              
         ZAP   DOUBLE,BUFNET                                                    
         ZAP   GROSS,BUFNET                                                     
         LA    R3,PRSUMNET                                                      
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,BUFCOM                                                    
         AP    GROSS,BUFCOM                                                     
         LA    R3,PRSUMCOM                                                      
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,GROSS                                                     
         LA    R3,PRSUMGRS                                                      
         BAS   RE,PRTAMNT                                                       
         MVI   SPACING,3                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   REQLX                                                            
         MVC   PRCOMP(22),=C'TOTAL RECORDS ON TAPE:'                            
         LA    R3,PRSUMNET                                                      
         EDIT  (P4,TPCT),(15,(R3)),COMMAS=YES                                   
         BAS   RE,PRINTIT                                                       
REQLX    B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        REVERSE OLD PCTEST AND SPECIAL AMOUNT BILLS WHEN YOU                   
*        ENCOUNTER A PROGRESSIVE OR TOTAL BILL (WHICH MARK CHARGES)             
*----------------------------------------------------------------------         
REVERSE  NTR1                                                                   
         USING TRANSD,R7                                                        
         MVI   REVFLAG,C'N'                                                     
         L     R7,ADTRANS          FIRST, SEE IF THIS IS A REVERSAL             
         USING BIND,R3                                                          
         L     R3,DRTAB            TABLE OF BILLED DR'S                         
         L     R0,BININ            NUMBER OF ENTRIES IN TABLE                   
         LTR   R0,R0                                                            
         BZ    REV040                                                           
         LA    R3,BINTABLE                                                      
         USING DRTABD,R3                                                        
*                                                                               
REV010   CLI   DRTNUM,X'FF'        IS THIS A DATE REVERSED                      
         BNE   REV030              NO                                           
         CLC   DRTDATE,TRNSNARR+33 IS THIS THE REVERSAL                         
         BNE   REV030              NO                                           
         MVI   REVFLAG,C'Y'                                                     
         B     REV040                                                           
*                                                                               
*EV020   CLI   DRTNUM,X'EE'        IS THIS THE ORIGINAL BILL DATE               
*        BNE   REV030              NO                                           
*        OC    TRNSNARR+35(2),TRNSNARR+35 IS THIS BILL REVERSED                 
*        BZ    REV030              NO                                           
*        CLC   DRTDATE,TRNSNARR+35 IS THIS A REVERSED ORIGINAL BILL             
*        BNE   REV030              NO                                           
*        MVI   REVFLAG,C'R'                                                     
*        B     REV040                                                           
*                                                                               
REV030   LA    R3,DRTABLN(R3)                                                   
         BCT   R0,REV010                                                        
         DROP  R3,R7                                                            
         SPACE 2                                                                
         USING SRTD,R3                                                          
REV040   LA    R3,SORTAREA                                                      
         USING TABLED,R2                                                        
         L     R2,BILLTAB                                                       
         LH    R0,TABNUM                                                        
         LTR   R0,R0               ANYTHING IN THE TABLE?                       
         BZ    EXIT                NO                                           
         LA    R2,TABDATA          POINT TO SAVED DATA                          
         DROP  R2                                                               
         USING BILLD,R2                                                         
         LA    R3,SRTD                                                          
REV050   MVC   SRTWC,=C'000'                                                    
         MVC   SRTREF,INVNUM                                                    
         MVC   SRTTYPE,BILLTYPE                                                 
         ZAP   SRTNET,BILLNET                                                   
         ZAP   SRTCOM,BILLCOM                                                   
         CLI   REVFLAG,C'Y'        IS THIS BILL A REVERSAL                      
         BE    REV060              YES,                                         
         MP    SRTCOM,=P'-1'                                                    
         MP    SRTNET,=P'-1'                                                    
*                                                                               
REV060   BAS   RE,PUTSORT                                                       
         MVI   GOTTOTAL,C'N'                                                    
         LA    R2,BILLDLN(R2)                                                   
         BCT   R0,REV050                                                        
         L     R2,BILLTAB                                                       
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SAVE OLD PCTEST AND SPECIAL AMOUNT BILLS IN CASE YOU                   
*        ENCOUNTER A PROGRESSIVE OR TOTAL BILL (WHICH MARK CHARGES)             
*----------------------------------------------------------------------         
BILLSAVE NTR1                                                                   
         USING TABLED,R2                                                        
         L     R2,BILLTAB          ADDRESS OF TABLE                             
         LH    R5,TABNUM           NUMBER IN TABLE                              
         LH    R3,TABMAX           MAX ALLOWED                                  
         CR    R3,R5               ANY ROOM?                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R2,TABDATA          POINT TO SAVED DATA                          
         LH    R3,=Y(BILLDLN)                                                   
         MR    R4,R3               NUM IN TABLE X LENGTH OF TABLE ENTRY         
         AR    R5,R2                                                            
         MVC   0(BILLDLN,R5),BILLAREA                                           
         L     R2,BILLTAB          UPDATE NUMBER IN TABLE                       
         LH    R5,TABNUM                                                        
         LA    R5,1(R5)                                                         
         STH   R5,TABNUM                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GIVEN A BILL, PUT ITS WORK CODE DETAILS OUT TO SORT                    
*----------------------------------------------------------------------         
GETWCDET NTR1                                                                   
         ZAP   WCNET,=P'0'                                                      
         ZAP   WCCOM,=P'0'                                                      
         USING TRANSD,R7                                                        
         MVI   GOTWCDET,C'N'                                                    
         L     R7,ADTRANS                                                       
         USING BIND,R3                                                          
         L     R3,DRTAB            TABLE OF BILLED DR'S                         
         L     R0,BININ            NUMBER OF ENTRIES IN TABLE                   
         LTR   R0,R0                                                            
         BZ    EXIT                                                             
         LA    R3,BINTABLE                                                      
         USING DRTABD,R3                                                        
GETWC10  CLC   TRNSREF,DRTNUM                                                   
         BNE   GETWC20                                                          
*                                                                               
         MVI   GOTWCDET,C'Y'                                                    
         BAS   RE,PUTDR                                                         
*                                                                               
GETWC20  EQU   *                                                                
         CLI   DRTNUM,X'EE'        IS THIS THE DATE OF AN ORIGINAL BILL         
         BNE   GETWC30             THAT WAS REVERSED, NO                        
         OC    TRNSNARR+35(2),TRNSNARR+35  HAS THIS BILL BEEN REVERSED          
         BZ    GETWC50             NO                                           
         CLC   DRTDATE,TRNSNARR+33                                              
         BNE   GETWC50                                                          
         MVC   DRTNUM,TRNSREF                                                   
         MVI   GOTWCDET,C'Y'                                                    
         BAS   RE,PUTDR                                                         
         MVI   DRTNUM,X'CC'                                                     
         B     GETWC50                                                          
*                                                                               
GETWC30  CLI   DRTNUM,X'FF'        IS THIS A DATE REVERSED                      
         BNE   GETWC50             NO                                           
         CLC   DRTDATE,TRNSNARR+33 IS THIS THE REVERSAL                         
         BNE   GETWC50             NO                                           
         MVC   DRTNUM,TRNSREF                                                   
         BAS   RE,PUTDR                                                         
         MVI   GOTWCDET,C'Y'                                                    
         MVI   DRTNUM,X'CC'                                                     
         B     GETWC50                                                          
*                                                                               
GETWC50  LA    R3,DRTABLN(R3)                                                   
         BCT   R0,GETWC10                                                       
         B     EXIT                FOR NOW                                      
         DROP  R3,R7                                                            
         EJECT                                                                  
*--------------------------------------------------------------------           
*        PUT THE DRTAB ENTRY AT 0(R3) OUT TO SORT                               
*--------------------------------------------------------------------           
         USING SRTD,R2                                                          
         USING DRTABD,R3                                                        
PUTDR    NTR1                                                                   
         LA    R2,SORTAREA                                                      
         MVC   SORTAREA(SRTDLN),SPACES                                          
         MVI   SRTWC,C'X'                                                       
         MVC   SRTWC+1(2),DRTWC                                                 
         MVI   SRTTYPE,C'X'                                                     
         OC    DRTNUM,DRTNUM       IS THIS A BILLED OR BILLABLE RECORD          
         BZ    PUTDR10             BRANCH IF BILLABLE                           
*                                                                               
         MVC   SRTREF,DRTNUM                                                    
         MVC   SRTTYPE,SAVETYPE    BILLTYPE                                     
*                                                                               
PUTDR10  DS    0H                                                               
*                                                                               
         ZAP   SRTNET,DRTAMNT     NET                                           
         ZAP   SRTCOM,DRTCOM      COMMISSION                                    
*                                                                               
         AP    WCNET,DRTAMNT                                                    
         AP    WCCOM,DRTCOM                                                     
         BAS   RE,PUTSORT                                                       
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        PUT A TRANSACTION LEVEL RECORD TO SORT                                 
*        FILL SRTPREC WITH TRANSACTION LEVEL DATA                               
*----------------------------------------------------------------------         
PUTSORT  NTR1                                                                   
         USING SRTD,R2                                                          
         USING SRTPRECD,R3                                                      
         LA    R2,SORTAREA                                                      
         LA    R3,SRTPREC                                                       
         CLI   SRTTYPE,C'X'        DON'T PUT TO SORT IF IT HAS A                
         BE    PUTSX                 TYPE OF X                                  
         MVC   SRTPG4,SVPG4        RESET NATURAL CODE                           
*                                                                               
         MVC   SRTPINVN,SRTREF                                                  
         MVC   SRTPTYPE,SRTTYPE                                                 
         MVC   SRTPWC,SRTWC                                                     
*                                                                               
         ZAP   SRTPNET,=P'0'       INIT NET AMOUNT                              
         ZAP   SRTPCOM,=P'0'       INIT COMMISSION AMOUNT                       
         LA    RE,SRTPNET                                                       
         USING AGYTABD,R4                                                       
         LA    R0,AGYTABNM         NO. OF AGENCIES IN TABLE                     
         LA    R4,AGYTAB           POINT TO AGENCY TABLE                        
PUTS010  CLC   AGYALPHA,ALPHAID    DO WE HAVE A MATCH ON ALPHAID                
         BE    *+12                                                             
         LA    R4,AGYTABQ(R4)      LOOP THROUGH AGENCY TABLE                    
         BCT   R0,PUTS010                                                       
*                                                                               
         TM    AGYSTAT,AGYNATS     DO WE NEED TO PUT IN SPECIAL CODE            
         BNO   PUTS020                                                          
         CLC   SRTPG4,=C'430'      IS NATURAL CODE 430                          
         BNE   PUTS040                                                          
         MVC   SRTPWC,=C'000'      AND SUBNATURAL SHOULD BE 000                 
         B     PUTS030                                                          
PUTS020  DS    0H                                                               
         CLC   SRTPG4,=C'443'      IF NATURAL IS 443                            
         BNE   PUTS028             THEN ....                                    
         MVC   SRTPWC,=C'000'      SUBNATURAL SHOULD BE 000                     
         B     PUTS030                                                          
PUTS028  CLC   SRTPG4,=C'420'      IS NATURAL 420                               
         BNE   PUTS040                                                          
         MVC   SRTPWC,=C'100'      AND SUBNATURAL SHOULD BE 100                 
PUTS030  LA    RE,SRTPCOM          NET TO COMMISION                             
*                                                                               
PUTS040  ZAP   0(L'SRTPNET,RE),SRTNET                                           
*                                                                               
         CLC   ALPHAID,=C'YN'      IS IT YNRA                                   
         BNE   PUTS050                                                          
         CLC   =C'KRN',SRTPACCT    IS CLIENT KRN                                
         BNE   PUTS050                                                          
         CLC   SRTPG4,=C'420'      DON'T DO IT FOR YN,CLT=KRN,NATRL=420         
         BNE   PUTS070                                                          
*                                                                               
PUTS050  AP    SRTPCOM,SRTCOM                                                   
         TM    IOSW,SRTOPEN                                                     
         BO    PUTS060                                                          
         OI    IOSW,SRTOPEN                                                     
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
PUTS060  GOTO1 SORTER,DMCB,=C'PUT',SRTPREC                                      
         B     PUTSX                                                            
*                                                                               
* CODE FOR YNRA CLIENT KRN SEPARATE NET AND COMMISSION LINES                    
*                                                                               
PUTS070  TM    IOSW,SRTOPEN                                                     
         BO    PUTS080                                                          
         OI    IOSW,SRTOPEN                                                     
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
PUTS080  GOTO1 SORTER,DMCB,=C'PUT',SRTPREC                                      
         ZAP   SRTPNET,=P'0'                                                    
         ZAP   SRTPCOM,=P'0'                                                    
         AP    PKCOM,SRTCOM        ACCUM COMMSN PER INVOICE FOR YNRA            
         MVC   SRTPG4,=C'440'                                                   
         MVC   SRTPWC,=C'000'                                                   
*                                                                               
         OC    SVSRTP,SVSRTP            IS IT FIRST TRANSACTION                 
         BNZ   *+10                     NO                                      
         MVC   SVSRTP,SRTPREC                                                   
         CLC   SVSRTP(SRTPLEN1),SRTPREC ARE WE STILL DOING SAME INVOICE         
         BE    PUTSX                                                            
         ZAP   SVSRTP+(SRTPCOM-SRTPRECD)(L'SRTPCOM),PKCOM                       
         CP    PKCOM,=P'0'           IS COMMISSION DOLLAR ZERO                  
         BE    PUTS090               DON'T ADD TO SORTER                        
         GOTO1 SORTER,DMCB,=C'PUT',SVSRTP                                       
         ZAP   PKCOM,=P'0'                                                      
PUTS090  MVC   SVSRTP,SRTPREC           SAVE SRTPREC FOR YNRA COMMISSN          
*                                                                               
PUTSX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CEN,CEC'          RETURN CURRENT ESTIMATE GROSS/COMM           
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
WRTP     NTR1                                                                   
         CLI   QOPT1,C'Y'          TAPE REQUESTED                               
         BNE   EXIT                NO                                           
         TM    IOSW,TPOPEN         HAS TAPE BEEN OPENED                         
         BO    WRTP50              YES                                          
         LH    R2,OUTCNT           NUMBER OF TIMES TAPE OPENED/CLOSED           
         LA    R2,1(R2)                                                         
         STH   R2,OUTCNT                                                        
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (OUTP,(OUTPUT))     NO, OPEN IT                                  
         OI    IOSW,TPOPEN                                                      
*                                                                               
WRTP50   MVC   TPREC,SRTPREC       1ST 7 BYTES NEED TO BE RESET 4 TAPE          
         USING TPRECD,R1                                                        
         LA    R1,TPREC                                                         
         USING SRTPRECD,R2                                                      
         LA    R2,SRTPREC                                                       
         MVC   TPAGY,SRTPAGY       RESET AGENCY ID FOR TAPE(POS 7)              
         MVC   TPCOMP,SRTPCOMP     RESET COMPANY BYTE                           
         MVC   TPDIV,SRTPDIV       RESET DIVISION                               
         MVC   TPPROD,SRTPPROD     RESET PRODUCT                                
*                                                                               
         PUT   OUTP,TPREC                                                       
         AP    TPCT,=P'1'                                                       
         B     EXIT                                                             
         EJECT                                                                  
PRTAMNT  ST    RE,SVRE             GENERAL EDIT ROUTINE                         
         EDIT  (P8,DOUBLE),(15,(R3)),2,COMMAS=YES,MINUS=YES                     
         L     RE,SVRE                                                          
         BR    RE                                                               
*                                                                               
PRINTIT  ST    RE,SVRE                                                          
         GOTO1 ACREPORT            PRINT SUBROUTINE                             
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         SPACE 3                                                                
TPDUMP   ST    RE,SVRE                                                          
         LA    R2,256                                                           
         GOTO1 PRNTBL,DMCB,(5,=C'SRTPREC'),SRTPREC,C'DUMP',(R2),=C'1D'          
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         SPACE 3                                                                
         GETEL R7,DATADISP,ELCODE  GET ELEMENT                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BUMP AMOUNTS, PRODUCE TOTALS ETC                                       
*----------------------------------------------------------------------         
TOTALS   NTR1                                                                   
         USING TOTTABD,R2                                                       
         L     R2,TOTTAB                                                        
         OC    LASTREC,LASTREC   WAS THERE A PREVIOUS SORT REC                  
         BZ    TOTX               YES, SEE IF IT CHANGED                        
TOT30    CLI   TOTLEV,X'FF'                                                     
         BE    TOTX                                                             
         ZIC   R1,TOTCLC                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTPREC(0),LASTREC                                               
         BE    TOTX                                                             
*                                                                               
         USING PRINTD,R6                                                        
         ZIC   R1,TOTOFF                                                        
         LA    R6,P                                                             
         AR    R1,R6                                                            
         MVC   0(5,R1),=C'TOTAL'                                                
         ZAP   DOUBLE,TOTNET                                                    
         ZAP   GROSS,TOTNET                                                     
         LA    R3,PRNET                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,TOTCOM                                                    
         AP    GROSS,TOTCOM                                                     
         LA    R3,PRCOM                                                         
         BAS   RE,PRTAMNT                                                       
*                                                                               
         ZAP   DOUBLE,GROSS                                                     
         LA    R3,PRGROSS                                                       
         BAS   RE,PRTAMNT                                                       
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
*                                                                               
         LA    R3,TOTLEN(R2)       NEXT BUCKET                                  
         AP    TOTNET-TOTTABD(L'TOTNET,R3),TOTNET                               
         AP    TOTCOM-TOTTABD(L'TOTCOM,R3),TOTCOM                               
         ZAP   TOTNET,=P'0'                                                     
         ZAP   TOTCOM,=P'0'                                                     
         LR    R2,R3               BUMP R3                                      
         B     TOT30                                                            
*                                                                               
TOTX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ADD TO A BINSRCH TABLE                                  *          
*         PARAM1 - A(RECORD TO BE ADDED)                             *          
*         PARAM2 - A(BINSRCH PARAMS)                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R3,0(R1)            A(RECORD)                                    
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R0,BINNUMB          NUMBER OF BUCKETS                            
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,*-14                                                          
BINXIT   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SORTCARDS                                                          *          
**********************************************************************          
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,39,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=114'                                   
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
DSPARM   DC    CL20'ACCTAPE.AC0GFXX1'                                           
DDPARM   DC    CL8'OUTP'                                                        
OUTP     DCB   DDNAME=OUTP,DSORG=PS,LRECL=TPRECLN,MACRF=PM,            X        
               BLKSIZE=TPRECLN*BLKFACT                                          
*                                                                               
         DS    0D                  DOUBLEWORD ALIGNMENT FOR TPVOLS              
TPVOLS   DC    PL8'1'              GENERATE TAPE(+1) FOR FIRST TAPE             
SQUASHER DC    V(SQUASHER)                                                      
PRNTBL   DC    V(PRNTBL)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
SORTER   DC    V(SORTER)                                                        
ADBUFC   DC    A(BUFFALOC)                                                      
DRTAB    DC    A(DRTABC)                                                        
XTAB     DC    A(XTABC)            TABLE OF UNMARKED CHARGES                    
BILLTAB  DC    A(BILLTABC)                                                      
WCTAB    DC    A(WCTABC)                                                        
TOTTAB   DC    A(TOTTABC)                                                       
ABOXRC   DC    A(BOXRC)                                                         
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
AGYTAB   DS    0CL3                                                             
         DC    C'H9',C'Q',X'80'    IF NATURAL 430, SUB GO TO 000                
         DC    C'OA',C'M',X'00'                                                 
         DC    C'OM',C'M',X'00'                                                 
         DC    C'YN',C'A',X'40'                                                 
         DC    C'JW',C'W',X'40'                                                 
         DC    C'FC',C'C',X'00'                                                 
         DC    C'WW',C'H',X'00'                                                 
         DC    C'  ',C' ',X'00'                                                 
AGYTABNM EQU   (*-AGYTAB)/L'AGYTAB                                              
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLE TO GENERATE SUB TOTALS                                       *          
**********************************************************************          
*                                                                               
TOTTABC  DS    0D                                                               
*                                                                               
         DC    AL1(1)                                                           
         DC    AL1((SRTPINVN-SRTPRECD)+L'SRTPINVN)                              
         DC    AL1(PRINVNO-PRINTD)                                              
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
*                                                                               
         DC    AL1(3)                                                           
         DC    AL1((SRTPPROD-SRTPRECD)+L'SRTPPROD)                              
         DC    AL1(PRPROD-PRINTD)                                               
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
*                                                                               
         DC    AL1(4)                                                           
         DC    AL1((SRTPDIV-SRTPRECD)+L'SRTPDIV)                                
         DC    AL1(PRDIV-PRINTD)                                                
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
*                                                                               
         DC    AL1(4)                                                           
         DC    AL1((SRTPCOMP-SRTPRECD)+L'SRTPCOMP)                              
         DC    AL1(PRCOMP-PRINTD)                                               
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
*                                                                               
         DC    XL1'FF'             END OF TOTAL TABLE                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    PL8'0'                                                           
         DC    PL8'0'                                                           
TOTNUM   EQU   (*-TOTTABC)/TOTLEN  NUMBER OF LEVELS IN TABLE                    
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF NON-FILE-MARKING BILLS                                        
*----------------------------------------------------------------------         
*                                                                               
BILLTABC DS    0D                                                               
         DS    H                  NUMBER OF BILLS SAVED                         
         DC    Y(BILLMAX)         MAXIMUM NUMBER                                
         DS    (BILLMAX*BILLDLN)C TABLE AREA                                    
BILLMAX  EQU   50                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF WORKCODES AND RATES                                           
*----------------------------------------------------------------------         
*                                                                               
WCTABC   DS    0D                                                               
         DS    H                  NUMBER OF BILSLS SAVED                        
         DC    Y(WCMAX)         MAXIMUM NUMBER                                  
         DS    (WCMAX*WCTABLN)C TABLE AREA                                      
WCMAX    EQU   255                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF BILLED DEBITS ON A JOB                                        
*        KEY AREA IS COVERED BY BIND                                            
*        DATA AREA IS COVERED BY DRTABD                                         
*----------------------------------------------------------------------         
*                                                                               
DRTABC   DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(DRTABLN)        RECORD LENGTH                                
         DC    AL4(DRTKYLN)        DISP OF KEY/ KEY LENGTH                      
         DC    A(DRTABMAX)         MAXIMUM NUMBER OF ENTRIES                    
         DC    AL1(2)              NUMBER OF BUCKETS                            
         DC    AL1(DRTBUCK-DRTABD) DISP TO FIRST BUCK                           
         DC    AL1(0)              SPARE                                        
         DS    (DRTABMAX*DRTABLN)C                                              
DRTABMAX EQU   500                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF UNBILLED DEBITS ON A JOB                                      
*        KEY AREA IS COVERED BY BIND                                            
*        DATA AREA IS COVERED BY DRTABD                                         
*----------------------------------------------------------------------         
*                                                                               
XTABC    DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(DRTABLN)        RECORD LENGTH                                
         DC    AL4(DRTKYLN)        DISP OF KEY/ KEY LENGTH                      
         DC    A(DRTABMAX)         MAXIMUM NUMBER OF ENTRIES                    
         DC    AL1(2)              NUMBER OF BUCKETS                            
         DC    AL1(DRTBUCK-DRTABD) DISP TO FIRST BUCK                           
         DC    AL1(0)              SPARE                                        
         DS    (DRTABMAX*DRTABLN)C                                              
         EJECT                                                                  
**********************************************************************          
* BUFFALO CSECT                                                      *          
**********************************************************************          
         SPACE 1                                                                
         BUFF  LINES=20,ROWS=1,COLUMNS=2,FLAVOR=PACKED,                X        
               KEYLIST=(1,A)                                                    
         EJECT                                                                  
**********************************************************************          
* BOX HOOK ROUTINE                                                   *          
**********************************************************************          
         SPACE 1                                                                
HDHOOK   DS    0D                                                               
         NMOD1 0,*HDHOOK*                                                       
         L     RC,BOXRC                                                         
*                                                                               
         CLC   ALPHAID,=C'YN'             Y+R HAS AGENCY CODE IN HEAD           
         BNE   *+16                                                             
         MVC   HEAD4+1(6),=C'AGENCY'      SETUP AGENCY HEADLINE                 
         MVC   HEAD4+12(L'HDAGY),HDAGY    MOVE AGENCY INTO HEADING              
*                                                                               
         L     R5,ADBOX                                                         
         USING BOXD,R5                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         USING PRINTD,R6                                                        
         LA    R6,BOXCOLS                                                       
         CLI   RCSUBPRG,0                                                       
         BNE   HDH50                                                            
         MVI   PRLEFT,C'L'                                                      
         MVI   PRCOL1,C'C'                                                      
         MVI   PRCOL2,C'C'                                                      
         MVI   PRCOL3,C'C'                                                      
         MVI   PRCOL4,C'C'                                                      
         MVI   PRCOL5,C'C'                                                      
         MVI   PRCOL6,C'C'                                                      
         MVI   PRCOL7,C'C'                                                      
         MVI   PRCOL8,C'C'                                                      
         MVI   PRCOL9,C'C'                                                      
         MVI   PRCOL10,C'C'                                                     
         MVI   PRCOL11,C'C'                                                     
         MVI   PRRIGHT,C'R'                                                     
         B     HDH100                                                           
*                                                                               
HDH50    MVI   PRLEFT,C'L'         SUMMARY BOXES                                
         MVI   PRCOL5,C'C'                                                      
         MVI   PRSUMCOL,C'C'                                                    
         MVI   PRSUMCO1,C'C'                                                    
         MVI   PRSUMRIT,C'R'                                                    
*                                                                               
HDH100   MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         XIT1                                                                   
*                                                                               
BOXRC    DS    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* STORAGE                                                            *          
**********************************************************************          
         SPACE 1                                                                
ACGFD    DSECT                                                                  
SVRE     DS    F                                                                
ADBOX    DS    A                                                                
OUTCNT   DS    H                                                                
*                                                                               
BUFREC   DS    0D                                                               
BUFKEY   DS    0CL1                                                             
BUFCOMP  DS    CL1                                                              
BUFNET   DS    PL8                                                              
BUFCOM   DS    PL8                                                              
*                                                                               
ELCODE   DS    CL1                                                              
COMMSTAT DS    CL1                                                              
FROM4B   EQU   1                   COMMISSION AMOUNT FROM TRBDCMP               
NOTSET   EQU   2                   COMMISSION RATE NOT AVAILABLE                
*                                                                               
TPCT     DS    PL4                                                              
GROSS    DS    PL8                                                              
XNET     DS    PL8                                                              
XCOM     DS    PL8                                                              
BNET     DS    PL8                                                              
BCOM     DS    PL8                                                              
PKCOM    DS    PL8                 COMMISSN ACCUM PER INVOICE FOR YNRA          
*                                                                               
SVSRTP   DS    CL(SRTPRECL)        SAVED AREA FOR SORT FOR YNRA                 
*                                                                               
*        NOTE- STORAGE BELOW IS USED TO PASS DATA                               
*        TO SUB-ROUTINES                                                        
*                                                                               
SRTPREC  DS    CL(SRTPRECL)       SORT INTERFACE                                
TPREC    DS    CL(TPRECLN)         TAPE INTERFACE                               
DRTABREC DS    CL(DRTABLN)         PUTWCDET INTERFACE                           
BILLAREA DS    CL(BILLDLN)         BILLSAVE INTERFACE                           
SORTAREA DS    CL(SRTDLN)          PUTSORT INTERFACE                            
*                                                                               
LSTAGY   DS    0CL1                AGENCY IS FIRST BYTE OF RECORD               
LASTREC  DS    CL(SRTPLEN1)        PREVIOUS RECORD FROM SORT                    
*                                                                               
GOTWCDET DS    CL1                                                              
GOTTOTAL DS    CL1                                                              
GOTABILL DS    CL1                 Y, THIS JOB HAS A BILL                       
TESTBILL DS    CL1                                                              
REVFLAG  DS    CL1                 YES, BILL IS A REVERSAL                      
PROGRESS EQU   X'01'                                                            
TOTAL    EQU   X'02'                                                            
ONELINE  EQU   X'04'                                                            
PCTEST   EQU   X'08'                                                            
ALLOC    EQU   X'10'                                                            
SPECIAL  EQU   X'20'                                                            
CLIENT   EQU   X'40'                                                            
MARKED   EQU   X'80'                                                            
*                                                                               
FLAG     DS    CL1                                                              
FLGG8K8  EQU   X'40'               USE G8 AND K8 USER FIELD CODES               
*                                                                               
SAVETYPE DS    CL1                                                              
*                                                                               
IOSW     DS    CL1                                                              
SRTOPEN  EQU   1                                                                
TPOPEN   EQU   2                                                                
*                                                                               
SVPG4    DS    CL3                  SAVED NATURAL CODE                          
SVDT2    DS    XL2                                                              
SVDT3    DS    XL3                                                              
SVYYMMDD DS    0C                                                               
SVYY     DS    CL2                                                              
SVMMDD   DS    CL4                                                              
SVYYMM   DS    CL2                                                              
SVMOS    DS    CL2                 MOS  FOR TRNSBTCH                            
SVAGY    DS    CL1                 GF AGENCY CODE                               
HDAGY    DS    CL1                 AGENCY CODE FOR THE HEADLINES                
HDADVDV  DS    CL2                 ADVERTISING DIVISION FOR HEADER              
INVNUM   DS    CL6                                                              
PL16     DS    PL16                                                             
SVNET    DS    PL6                                                              
SVCOM    DS    PL6                                                              
WCNET    DS    PL6                 NET AND COMMISSION, FROM WC DETAIL           
WCCOM    DS    PL6                                                              
TODAY    DS    CL6                                                              
QSTR3    DS    CL3                 YYMMDD DATES                                 
QEND3    DS    CL3                                                              
QSTR2    DS    CL2                 PACKED DATES                                 
QEND2    DS    CL2                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF BILLED DEBITS ON A JOB                                        
*----------------------------------------------------------------------         
DRTABD   DSECT                     TABLE OF DR AMOUNTS                          
DRTNUM   DS    CL6                 BILL NUMBER                                  
DRTWC    DS    CL2                 WORK CODE                                    
DRTDATE  DS    CL2                 PACKED BILL DATE                             
DRTKYLN  EQU   *-DRTABD                                                         
DRTSTAT  DS    CL1                 COMMISSIONABLE OR NOT                        
DRTBUCK  EQU   *                                                                
DRTAMNT  DS    PL8                 BILLED FOR W/C                               
DRTCOM   DS    PL8                 COMMISSION FOR A W/C                         
DRTCD    DS    PL8                 CD, TO CALCULATE COMMISSION                  
DRTABLN  EQU   *-DRTABD                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BINSRCH DSECT                                                          
*----------------------------------------------------------------------         
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER OF ENTRIES IN TABLE                   
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT OF KEY                          
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER OF ENTRIES                    
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISPLACEMENT OF 1ST BUCKET                   
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
**********************************************************************          
* SORT RECORD DSECT                                                  *          
**********************************************************************          
         SPACE 1                                                                
SRTPRECD DSECT                                                                  
SRTP     DS    0C                                                               
SRTPAGY  DS    CL1                 M= O+M, A= Y+R                               
SRTPCOMP DS    CL1                 ADVERTISING DIVISION (UF=G2)                 
SRTPDIV  DS    CL1                                                              
SRTPPROD DS    CL4                 PRODUCT CODE         (UF=G3)                 
SRTPACCT DS    CL12                CLI, PRO, JOB                                
SRTPMOS  DS    CL4                 YYMM OF MOS OF BILL                          
SRTPINVN DS    CL6                 BILLING INVOICE NUMBER                       
SRTPLEN1 EQU   *-SRTPRECD                                                       
         DS    CL3                                                              
SRTPTYPE DS    CL1                 BILLING TYPE                                 
SRTPG4   DS    CL3                 GF NATURAL            (UF=G4)                
SRTPWC   DS    CL3                 WORKCODE OR 000 FOR % OF EST BILLS           
SRTPLEN2 EQU   *-SRTPRECD          LENGTH FOR SORT                              
         DS    CL4                                                              
SRTPNET  DS    CL10                NET AMOUNT                                   
SRTPCOM  DS    CL10                COMMISSION                                   
SRTPMON  DS    CL4                 MONTH OF THE DATA ON THIS TAPE               
         ORG   SRTP                                                             
         DS    (SRTPLREC)C                                                      
SRTPRECL EQU   *-SRTPRECD                                                       
SRTPLREC EQU   80                                                               
         EJECT                                                                  
**********************************************************************          
* OUTPUT TAPE DSECT                                                  *          
**********************************************************************          
         SPACE 1                                                                
TPRECD   DSECT                                                                  
TP       DS    0C                                                               
TPCOMP   DS    CL1                 ADVERTISING DIVISION (UF=G2)                 
TPDIV    DS    CL1                                                              
TPPROD   DS    CL4                 PRODUCT CODE         (UF=G3)                 
TPAGY    DS    CL1                 M= O+M, A= Y+R                               
TPACCT   DS    CL12                CLI, PRO, JOB                                
TPMOS    DS    CL4                 YYMM OF MOS OF BILL                          
TPINVNO  DS    CL6                 BILLING INVOICE NUMBER                       
TPLEN1   EQU   *-TPRECD                                                         
         DS    CL3                                                              
TPTYPE   DS    CL1                 BILLING TYPE                                 
TPG4     DS    CL3                 GF NATURAL            (UF=G4)                
TPWC     DS    CL3                 WORKCODE OR 000 FOR % OF EST BILLS           
TPLEN2   EQU   *-TPRECD            LENGTH FOR SORT                              
         DS    CL4                                                              
TPNET    DS    CL10                NET AMOUNT                                   
TPCOM    DS    CL10                COMMISSION                                   
TPTPMON  DS    CL4                 MONTH OF THE DATA ON THIS TAPE               
         ORG   TP                                                               
         DS    (TPLRECL)C                                                       
TPRECLN  EQU   *-TPRECD                                                         
TPLRECL  EQU   80                                                               
BLKFACT  EQU   10                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        TABLE OF BILLS  WHICH DID NOT MARK CHARGES                             
*----------------------------------------------------------------------         
BILLD    DSECT                                                                  
BILLREF  DS    CL6                                                              
BILLDATE DS    CL3                                                              
BILLTYPE DS    CL1                                                              
BILLNET  DS    PL6                                                              
BILLCOM  DS    PL6                                                              
BILLDLN  EQU   *-BILLD                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER TRANSACTION LEVEL INFORMATION NEEDED TO                 
*        PASS A RECORD TO SORT                                                  
*----------------------------------------------------------------------         
SRTD     DSECT                                                                  
SRTREF   DS    CL6                                                              
SRTTYPE  DS    CL1                 BILLTYPE                                     
SRTWC    DS    CL3                 WORK CODE OR 000                             
SRTNET   DS    PL6                                                              
SRTCOM   DS    PL6                                                              
SRTMOS   DS    CL4                 ########################                     
SRTDLN   EQU   *-SRTD                                                           
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER AGENCY TABLE                                            
*----------------------------------------------------------------------         
         SPACE 1                                                                
AGYTABD  DSECT                                                                  
AGYALPHA DS    CL2                                                              
AGYCODE  DS    CL1                                                              
AGYSTAT  DS    XL1                                                              
AGYNATS  EQU   X'80'               NATURAL SUB NATURAL CODE CHECK               
AGYG8K8  EQU   X'40'               READ G8,K8 INSTEAD OF G1,K1                  
AGYTABQ  EQU   *-AGYTABD                                                        
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER HEADER INFO IN TABLES                                   
*----------------------------------------------------------------------         
TABLED   DSECT                                                                  
TABNUM   DS    H                                                                
TABMAX   DS    H                                                                
TABDATA  DS    0C                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER WORK CODE RATE TABLE                                    
*----------------------------------------------------------------------         
WCTABD   DSECT                                                                  
WCTBCODE DS    CL2                                                              
WCTBRATE DS    PL6                                                              
WCTABLN  EQU   *-WCTABD                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER PRINT LINE                                              
*----------------------------------------------------------------------         
PRINTD   DSECT                                                                  
PRLEFT   DS    CL1                                                              
PRCOMP   DS    CL1                                                              
         DS    CL6                                                              
PRCOL1   DS    CL1                                                              
PRDIV    DS    CL1                                                              
         DS    CL7                                                              
PRCOL2   DS    CL1                                                              
PRPROD   DS    CL4                                                              
         DS    CL3                                                              
PRCOL3   DS    CL1                                                              
PRACCT   DS    CL12                CLI, PRO, JOB                                
PRCOL4   DS    CL1                                                              
PRMOS    DS    CL4                 YYMM OF MOS OF BILL                          
         DS    CL2                 YYMM OF MOS OF BILL                          
PRCOL5   DS    CL1                                                              
PRINVNO  DS    CL6                 BILLING INVOICE NUMBER                       
         DS    CL3                                                              
PRCOL6   DS    CL1                                                              
PRTYPE   DS    CL1                 BILLING TYPE                                 
         DS    CL3                                                              
PRCOL7   DS    CL1                                                              
PRG4     DS    CL3                 GF NATURAL            (UF=G4)                
         DS    CL4                                                              
PRCOL8   DS    CL1                                                              
PRWC     DS    CL3                 WORKCODE OR 000 FOR % OF EST BILLS           
         DS    CL8                                                              
PRCOL9   DS    CL1                                                              
PRNET    DS    CL15                NET AMOUNT                                   
PRCOL10  DS    CL1                                                              
PRCOM    DS    CL15                COMMISSION                                   
PRCOL11  DS    CL1                                                              
PRGROSS  DS    CL15                COMMISSION                                   
PRRIGHT  DS    CL1                                                              
*                                                                               
         ORG   PRINVNO                                                          
PRSUMNET DS    CL15                                                             
PRSUMCOL DS    CL1                                                              
PRSUMCOM DS    CL15                                                             
PRSUMCO1 DS    CL1                                                              
PRSUMGRS DS    CL15                                                             
PRSUMRIT DS    CL1                                                              
         EJECT                                                                  
TOTTABD  DSECT                     TO COVER TOTTAB                              
TOTLEV   DS    CL1                                                              
TOTCLC   DS    AL1                                                              
TOTOFF   DS    AL1                 OFFSET TO PRINT "TOTAL"                      
TOTNET   DS    PL8                                                              
TOTCOM   DS    PL8                                                              
TOTLEN   EQU   *-TOTTABD                                                        
         EJECT                                                                  
* ACGENMODES ACGENBOTH ACREPWORKD GETOPTD                                       
GETOPTD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
       ++INCLUDE ACJOBBERD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038ACREPGF02 04/10/15'                                      
         END                                                                    
