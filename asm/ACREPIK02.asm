*          DATA SET ACREPIK02  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACIK02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE RIGHT                                                                  
         TITLE 'ACIK02 - CALVIN KLEIN INTERFACE'                                
ACIK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**IK02*,R9,R8                                                  
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACIKD,RC                                                         
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,ACCFRST                                                     
         BE    ACCF                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
REQF     NI    PROCSW,ALL-(PROCPAY+PROCGL)                                      
         CLI   QOPT1,C'P'                                                       
         BNE   *+12                                                             
         LA    R3,PAYSW                                                         
         OI    PROCSW,PROCPAY                                                   
         CLI   QOPT1,C'G'                                                       
         BNE   *+12                                                             
         LA    R3,GLSW                                                          
         OI    PROCSW,PROCGL                                                    
*                                                                               
         MVC   ATAPE,1(R3)             A(DCB)                                   
         L     R2,ATAPE                                                         
         TM    0(R3),TAPEOPN           TEST TAPE ALREADY OPEN                   
         BO    REQF5                                                            
*                                                                               
         USING IHADCB,R2                                                        
         MVC   DDPARM,DCBDDNAM         DD NAME                                  
         MVC   DSPAGY,ALPHAID          ALPHA ID                                 
         MVC   DSPTYP,QOPT1            TYPE                                     
         DROP  R2                                                               
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),DSPARM                                  
         OPEN  ((R2),(OUTPUT))                                                  
         OI    0(R3),TAPEOPN           SET OPEN SWITCH                          
*                                                                               
REQF5    GOTO1 DATCON,DMCB,(5,0),(1,TODAY1)                                     
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         GOTO1 DATCON,DMCB,(5,0),(20,TODAY20)                                   
         CLC   QSTART,SPACES                                                    
         BE    REQF7                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(2,START2)                                
*                                                                               
REQF7    CLC   QEND,SPACES                                                      
         BE    REQF9                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(2,END2)                                    
*                                                                               
REQF9    LA    R1,SRKLNQ           KEY FOR SORT                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         LA    RF,SORTCRD                                                       
         MVC   16(3,RF),DUB        KEY LENGTH FOR SORT RECORD                   
                                                                                
         LA    R1,SRLNQ            RECORD LENGTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         LA    RF,RECCRD                                                        
         MVC   22(3,RF),DUB        FOR RECCARD                                  
         GOTO1 ADSORTER,DMCB,SORTCRD,RECCRD,0                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ACCOUNT FIRST                                                       *         
***********************************************************************         
                                                                                
ACCF     L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   VNDR,SPACES             RIGHT JUSTIFY VENDOR                     
         MVC   VNDR(L'ACTKACT),ACTKACT                                          
         LA    R0,L'VNDR                                                        
         GOTO1 RIGHT,DMCB,VNDR,(R0)                                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION - PAYABLES                                      *         
***********************************************************************         
                                                                                
PTRN     DS    0H                                                               
PAY      TM    PROCSW,PROCPAY                                                   
         BNO   GEN                                                              
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   0(R4),TRNELQ                                                     
         BNE   XIT                                                              
         L     R2,ADTRANS                                                       
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         TM    TRNSTAT,X'80'           TEST DEBIT                               
         BO    XIT                     YES, SKIP THE DEBITS                     
         CLI   RCRERUN,C'Y'            TEST RERUN                               
         BNE   PAY01                   NO, CHECK ALREADY USED                   
         CLC   TRNKEY+ACCOUSED(2),START2  YES, CHECK START END DATE             
         BL    XIT                                                              
         CLC   TRNKEY+ACCOUSED(2),END2                                          
         BH    XIT                                                              
         B     PAY03                                                            
*                                                                               
PAY01    OC    TRNKEY+ACCOUSED(2),TRNKEY+ACCOUSED  TEST ALREADY USED            
         BNZ   XIT                                                              
         MVC   TRNKEY+ACCOUSED(2),TODAY2           MARK AS USED                 
         MVI   MODE,WRITRANS                                                    
*                                                                               
PAY03    MVI   SREC,C' '                                                        
         MVC   SREC+1(SRLNQ-1),SREC                                             
*                                                                               
         MVC   SRVNDR,TRNKACT          ACCOUNT(VENDOR)                          
         MVC   SRVNDID,VNDR            ACCOUNT (RIGHT JUSTIFIED)                
         MVC   SRINVD,TRNKDATE         INVOICE DATE                             
         MVC   SRINVN(L'TRNREF),TRNREF INVOICE NUMBER                           
         MVC   SRCLI,TRNKCULC+12       CLIENT                                   
         MVC   SRPRD,TRNKREF           PRODUCT IS FIRST 3 OF REF                
         CLC   TRNKULC(2),=C'SJ'                                                
         BNE   *+10                                                             
         MVC   SRCLI(6),TRNKCULC+3                                              
         ZAP   SRAMT,TRNAMNT           AMOUNT                                   
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         SH    R1,=Y(TRNLN1Q+1)        GET NARRATIVE                            
         BNP   PAY05                                                            
         CHI   R1,L'SRNAR-1                                                     
         BNH   *+8                                                              
         LHI   R1,L'SRNAR-1                                                     
         EX    R1,*+4                                                           
         MVC   SRNAR(0),TRNNARR                                                 
*                                                                               
PAY05    MVI   ELCODE,XPYELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   PAY07                                                            
         USING XPYELD,R4                                                        
         MVC   SRINVN(L'XPYINV),XPYINV                                          
*                                                                               
PAY07    MVI   SRTYP,SRHDR             FIRST PUT HEADER                         
         GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
         MVI   SRTYP,SRDTL             THEN DETAIL                              
         BASR  RE,RF                                                            
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION - GENERAL LEDGER                                *         
***********************************************************************         
                                                                                
GEN      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
                                                                                
REQL     DS    0H                                                               
         XC    HEADR,HEADR             CLEAR HEADER                             
REQL3    GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    REQLX                                                            
         MVC   SREC(SRLNQ),0(R2)                                                
         CLI   SRTYP,SRDTL                                                      
         BNE   REQL5                                                            
         OC    HEADR,HEADR             TEST HEADER IS STILL AROUND              
         BZ    *+8                                                              
         BAS   RE,PHDR                 PUT PAYABLE HEADER                       
         BAS   RE,PDTL                 PUT DETAIL RECORD                        
         B     REQL3                                                            
*                                                                               
REQL5    OC    HEADR,HEADR             TEST ALREADY HAVE HEADER                 
         BNZ   REQL7                                                            
         MVC   HEADR,SREC              SAVE HEADER                              
         B     REQL3                                                            
*                                                                               
REQL7    LA    RF,HEADR                ADD TO HEADER TOTAL                      
         AP    SRAMT-SREC(L'SRAMT,RF),SRAMT                                     
         B     REQL3                                                            
*                                                                               
REQLX    MVI   DWNMODE,DWNEOR      END OF REPORT                                
         BAS   RE,DWNL                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PAYABLE HEADER RECORD                                               *         
***********************************************************************         
                                                                                
PHDR     NTR1  ,                                                                
         L     R7,ACVI                                                          
         USING CVI,R7                                                           
         LA    R6,HEADR                                                         
         USING SREC,R6                                                          
         MVC   CVIVNDID,SRVNDID        VENDOR ID                                
         MVC   CVIVIVNO,SRINVN         VENDOR INVOICE                           
         GOTO1 DATCON,DMCB,(1,SRINVD),(20,CVIIVDTE)  INVOICE DATE               
         MVC   CVIPONUM,SRPON          PO NUMBER                                
         MVC   CVIDESC,SRNAR           DESCRIPTION                              
         EDIT  SRAMT,(15,CVIBIVAM),2,MINUS=YES                                  
         MVC   CVIIVAMT,CVIBIVAM       AMOUNT                                   
         MVC   CVIPSTD,TODAY20         POSTING DATE                             
         PUT   PTAPE,(R7)                                                       
         DROP  R6,R7                                                            
*                                                                               
         L     R4,ADCVI                                                         
         BAS   RE,DNL                  DOWNLOAD INVOICE HEADER                  
         XC    HEADR,HEADR                                                      
         ZAP   SEQ,=P'0'                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PAYABLE DETAIL RECORD                                               *         
***********************************************************************         
                                                                                
PDTL     NTR1  ,                                                                
         L     R7,ACVD                                                          
         USING CVD,R7                                                           
         MVC   CVDVNDID,SRVNDID        VENDOR ID                                
         MVC   CVDVIVNO,SRINVN         VENDOR INVOICE                           
         AP    SEQ,=P'1'               COUNT DETAIL                             
         OI    SEQ+L'SEQ-1,X'0F'                                                
         UNPK  CVDDSTSQ,SEQ                                                     
         EDIT  SRAMT,(15,CVDDSTAM),2,MINUS=YES                                  
         PUT   PTAPE,(R7)                                                       
         L     R4,ADCVD                                                         
         BAS   RE,DNL                  DOWNLOAD INVOICE HEADER                  
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD INVOICE HEADER                                             *         
*  R4=A(DOWNLOAD ADDRESS LIST)                                        *         
***********************************************************************         
                                                                                
DNL      NTR1  ,                                                                
DNL3     MVC   DWNMODE,0(R4)                                                    
         MVC   P,SPACES                                                         
         SR    R1,R1                                                            
         ICM   R1,1,1(R4)              R1=LENGTH OF DATA                        
         BZ    DNL5                                                             
         BCTR  R1,0                                                             
         SR    R2,R2                                                            
         ICM   R2,7,2(R4)              R2=DATA                                  
         EX    R1,*+4                                                           
         MVC   P(0),0(R2)                                                       
*                                                                               
DNL5     BAS   RE,DWNL                 DOWNLOAD THE FIELD                       
         LA    R4,5(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DNL3                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RUNL     DS    0H                                                               
         B     XIT                                                              
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD ROUTINES                                                   *         
***********************************************************************         
                                                                                
DWNL     NTR1  ,                                                                
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         LA    R7,DWNBUF                                                        
         USING DLCBD,R7                                                         
         TM    DWNSTAT,DWNINIT     HAS IT BEEN INITIALIZED?                     
         BO    DWNL5                                                            
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
         MVI   BOXYORN,NO          NO BOXES                                     
         MVI   BOXSHADE,0          NO SHADING                                   
         MVC   BOXWIDTH,=AL4(L'XP)                                              
         MVI   DLCBACT,DLCBINIT    START OF REPORT                              
         MVI   XP,C' '                                                          
         MVC   XP+1(L'XP-1),XP                                                  
         LA    RE,XP                                                            
         ST    RE,DLCBAPL          PRINT LINE AREA                              
         LA    RE,DWNHOOK          HOOK ROUTINE ADDRESS                         
         ST    RE,DLCBAPR          PRINT ROUTINE                                
         MVI   DLCXMAXL+1,L'XP     LINE LENGTH                                  
         MVI   DLCXDELC,C' '       DELEMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT                                         
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT                               
         MVI   DLCXEOLC,X'5E'      END OF LINE ,SEMI-COLON                      
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 DLFLD,DLCBD                                                      
         MVI   FORCEHED,YES        TO SEPERATE REQUEST DETAILS                  
         GOTO1 ACREPORT                                                         
         MVI   DLCBFLX,C' '                                                     
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
         MVC   DLCBFLD,DLCBFLX                                                  
         MVC   DLCBFLX,XSPACES                                                  
         OI    DLCBFLG1,DLCBFXFL   USE EXTENDED FIELD FOR TEXT                  
         OI    DWNSTAT,DWNINIT                                                  
*                                                                               
DWNL5    DS    0H                                                               
DWNL7    CLI   DWNMODE,DWNTEXT     PUT TEXT                                     
         BNE   DWNL9                                                            
         MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         MVC   DLCBFLX,P                                                        
         B     DWNLX                                                            
*                                                                               
DWNL9    DS    0H                                                               
DWNL15   CLI   DWNMODE,DWNNUMB     PUT NUMBER                                   
         BNE   DWNL17                                                           
         MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBNUM     NUMBER                                       
         MVC   DLCBFLD,P                                                        
         B     DWNLX                                                            
*                                                                               
DWNL17   CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BNE   DWNL19                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         B     DWNLX                                                            
*                                                                               
DWNL19   CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR                                                  
*                                                                               
DWNLX    GOTO1 DLFLD,DLCBD                                                      
         B     XIT                                                              
                                                                                
DWNHOOK  MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         MVI   SKIPSPEC,C'N'       THIS WILL IGNORE IF FOOTLINE AT END          
         MVI   FORCEHED,C'N'                                                    
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
                                                                                
DLFLD    DC    V(DLFLD)                                                         
RIGHT    DC    V(RIGHT)                                                         
*                                                                               
TAPEOPN  EQU   X'80'                   TAPE IS OPEN                             
PAYSW    DC    AL1(0)                  PAYABLE TAPE CONTROL                     
         DC    AL4(PTAPE)                                                       
*                                                                               
GLSW     DC    AL1(0)                  GL TAPE CONTROL                          
         DC    AL4(GTAPE)                                                       
*                                                                               
ACVI     DC    A(CVI)                                                           
ADCVI    DC    A(DCVI)                                                          
ACVD     DC    A(CVD)                                                           
ADCVD    DC    A(DCVD)                                                          
AGTR     DC    A(GTR)                                                           
*                                                                               
PTAPE    DCB   DDNAME=PTAPE,DSORG=PS,RECFM=U,LRECL=100,BLKSIZE=100,    X        
               MACRF=PM                                                         
*                                                                               
GTAPE    DCB   DDNAME=GTAPE,DSORG=PS,RECFM=U,LRECL=100,BLKSIZE=100,    X        
               MACRF=PM                                                         
*                                                                               
DDPARM   DC    CL8'       '                                                     
DSPARM   DS    0CL20                                                            
         DC    CL13'ACCTAPE.AC0IK'                                              
DSPAGY   DC    CL2'XX'                 AGENCY                                   
DSPTYP   DC    C'X'                    TYPE P=PAYABLES, G=GL                    
         DC    CL4' '                                                           
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(01,000,A),FORMAT=BI,WORK=1'                    
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=(000)'                                 
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PAYABLE INTERFACE HEADER RECORD                                     *         
***********************************************************************         
                                                                                
* FIELDS PREFIXED BY A * ARE FILLED IN BY THIS PROGRAM                          
* FIELDS WITH A ? ARE STILL OPEN ISSUES                                         
*                                                                               
CVI      CSECT                                                                  
CVICPY   DC    CL4'0500'               COMPANY CODE                             
CVIVNDID DC    CL9' '                ?*VENDOR ID                                
CVIEDIN  DC    CL15' '                 EDI NUMBER                               
CVIVIVNO DC    CL22' '                *VENDOR INVOICE NUMBER                    
CVIIVSUF DC    CL3'000'                INVOICE SUFFIX                           
CVIIVBAT DC    CL6'000000'             INVOICE BATCH                            
CVIVCHR  DC    CL10' '                 VOUCHER                                  
CVIAUTHC DC    CL3' '                  AUTHORITY CODE                           
CVIPRCL  DC    CL5'1    '              PROCESS LEVEL                            
CVIIVACL DC    CL4' '                  INVOICE ACCRUAL CODE                     
CVIIVTYP DC    C' '                    INVOICE TYPE                             
CVIOLDVN DC    CL9' '                  OLD VENDOR CODE                          
CVIIVCUR DC    CL5'USD'                INVOICE CURRENCY                         
CVIIVDTE DC    CL8'YYYYMMDD'          *INVOICE DATE                             
CVITXPDT DC    CL8'00000000'           TAX POINT DATE                           
CVIPRCLO DC    CL4' '                  PURCHASE FROM LOACATION                  
CVIPONUM DC    CL7' '                ?*PO NUMBER (P OR I 999999)                
CVIPOREL DC    CL6'000000'             PO RELEASE NUMBER                        
CVIAUTOM DC    C' '                    AUTO MATCH                               
CVIDESC  DC    CL30' '                *DESCRIPTION                              
CVIBIVAM DC    CL15'00000000010000-' ?*BASE INVOICE AMOUNT                      
CVIIVAMT DC    CL15'99999999999999+' ?*INVOICE AMOUNT                           
CVIALLOW DC    CL15'00000000000000 '   ALLOWABLE AMOUNT                         
CVITAXBL DC    CL15'00000000000000'    TAXABLE AMOUNT                           
CVITAXAM DC    CL15'00000000000000'    TAX AMOUNT                               
CVIDISCA DC    CL15'00000000000000 '   DISCOUNT AMOUNT                          
CVIXCHGR DC    CL12'00000000000 '      EXCHANGE RATE                            
CVIANTFL DC    CL1' '                  ANTICIPATION FLAG                        
CVIDISCR DC    CL5'0000 '             ?DISCOUNT RATE                            
CVIDISCD DC    CL8'00000000'           DISCOUNT DATE                            
CVIDUEDT DC    CL8'00000000'           DUE DATE                                 
CVINRCUR DC    CL3'000'                NUMBER OF RECURRENCES                    
CVIRCURF DC    C' '                    RECURRING FREQUENCY                      
CVIREMTO DC    CL4' '                  REMIT TO LOACATION                       
CVICASHC DC    CL4' '                  CASH CODE                                
CVISEPOP DC    C' '                    SEPARATE PAYMENT OPTION                  
CVIPAYIM DC    C'N'                    PAY IMMEDIATE CODE                       
CVIENCLO DC    C'N'                    ENCLOSURE OPTION                         
CVICURRE DC    C' '                    CURRENCY RECALCULATION                   
CVILCD   DC    C'N'                    LOST CASH DISCOUNT                       
CVITAXC  DC    CL10' '                 TAX CODE                                 
CVIINCC  DC    CL4' '                  INCOME CODE                              
CVIHLDC  DC    CL4' '                  HOLD CODE                                
CVIDSBC  DC    CL9' '                  DISTRIBUTION CODE                        
CVITRMC  DC    CL5' '                  TERM CODE                                
CVIIVSTA DC    C'0'                    INVOICE STATUS                           
CVIPSTS  DC    C'0'                    POSTING STATUS                           
CVIPSTD  DC    CL8'YYYYMMDD'         ?*POSTING DATE                             
CVIPAYVD DC    CL9' '                ?*PAY VENDOR                               
CVIPAYN  DC    CL10' '                 PAYMENT NUMBER                           
CVIPAYC  DC    CL3' '                  PAYMENT CODE                             
CVICKDTE DC    CL8'00000000'           CHECK DATE                               
CVIIVGRP DC    CL4' '                  INVOICE GROUP                            
CVIONETV DC    C'N'                    ONE TIME VENDOR                          
CVIVNDNM DC    CL30' '                 VENDOR NAME                              
CVIVNDSN DC    CL30' '                 VENDOR SEARCH NAME                       
CVIVNDA1 DC    CL30' '                 VENDOR ADDRESS - 1                       
CVIVNDA2 DC    CL30' '                 VENDOR ADDRESS - 2                       
CVIVNDA3 DC    CL30' '                 VENDOR ADDRESS - 3                       
CVIVNDA4 DC    CL30' '                 VENDOR ADDRESS - 4                       
CVIVNDA5 DC    CL18' '                 VENDOR ADDRESS - 5                       
CVIVNDST DC    CL2' '                  VENDOR STATE                             
CVIVNDPC DC    CL10' '                 VENDOR POSTAL CODE                       
CVIVNDCT DC    CL30' '                 VENDOR COUNTRY                           
CVIJRNLB DC    CL12' '                 JOURNAL BOOK                             
CVIBKPAM DC    CL5'00000000000000 '  ? BANK PAYMENT AMOUNT                      
CVIBKND  DC    C'0'                    BANK CURRENCY NUMBER DECIMALS            
CVIBKXRT DC    CL12'00000000000 '    ? BANK CURRENCY EXCHANGE RATE              
CVIDISC  DC    CL10' '                 DISCOUNT CODE                            
CVIIVSRC DC    C' '                    INVOICE SOURCE                           
CVIAPPFL DC    C' '                    APPROVED FLAG                            
CVIINCW  DC    CL10' '                 INCOME WITHOLDING FLAG                   
CVILNQ   EQU   *-CVI                                                            
         EJECT                                                                  
***********************************************************************         
* PAYABLE INTERFACE DETAIL RECORD                                     *         
***********************************************************************         
                                                                                
CVD      CSECT                                                                  
CVDCPY   DC    CL4'0500'               COMPANY CODE                             
CVDVNDID DC    CL9' '                ?*VENDOR ID                                
CVDEDIN  DC    CL15' '                 EDI NUMBER                               
CVDVIVNO DC    CL22' '                *VENDOR INVOICE NUMBER                    
CVDIVSUF DC    CL3'000'                INVOICE SUFFIX                           
CVDDSTSQ DC    CL4'9999'             ?*DISTRIBUTION SEQUENCE NUMBER             
CVDDSTAM DC    CL15'00000000000000 ' ?*DISTRIBUTION AMOUNT                      
CVDTAXBL DC    CL15'00000000000000 '   TAXABLE AMOUNT                           
CVDDSTCP DC    CL4'0400'               DISTRIBUTION COMPANY                     
CVDCVBAM DC    CL15'00000000000000 '   CONVERT TO BASE                          
CVDDSTAU DC    CL15' '               ?*DISTRIBUTION ACCOUNT UNIT                
CVDDSTAN DC    CL6' '                ?*DISTRIBUTION ACCOUNT NUMBER              
CVDDSTSB DC    CL4'0000'               DISTRIBUTION SUBACCOUNT NUMBER           
CVDTAXC  DC    CL10' '                 TAX CODE                                 
CVDDESC  DC    CL30' '                 DESCRIPTION                              
CVDDSTRF DC    CL10' '                 DST REFERENCE                            
CVDACTV  DC    CL15' '                 ACTIVITY                                 
CVDASSD  DC    CL30' '                 ASSET DESCRIPTION                        
CVDTAGN  DC    CL12' '                 TAG NUMBER                               
CVDITMN  DC    CL12' '                 ITEM NUMBER                              
CVDITMD  DC    CL12' '                 ITEM DESCRIPTION                         
CVDITMQ  DC    CL8'00000000'           ITEM QUANTITY                            
CVDASST  DC    CL10' '                 ASSET TEMPLATE                           
CVDINSDT DC    CL8'00000000'           INSERVICE DATE                           
CVDPURDT DC    CL8'00000000'           PURCHASE DATE                            
CVDMODLN DC    CL30' '                 MODULE NUMBER                            
CVDSERN  DC    CL30' '                 SERIAL NUMBER                            
CVDHLDAM DC    C' '                    HOLD AMOUNT                              
CVDASSN  DC    CL10'0000000000'        ASSET NUMBER                             
CVDACCAT DC    CL5' '                  ACCOUNT CATEGORY                         
CVDUNTAM DC    CL15'0000000000000 '  ?*UNIT AMOUNT                              
CVDTAXAM DC    CL15'0000000000000 '  ?*TAXABLE AMOUNT                           
CVDASSGR DC    CL10' '                 ASSET GROUP                              
CVDCOMB  DC    CL10' '                 COMBINE                                  
CVDACCU  DC    CL15' '                 ACCOUNTING UNIT                          
CVDTAXP  DC    C' '                    TAX POINT                                
CVDLNQ   EQU   *-CVD                                                            
         EJECT                                                                  
***********************************************************************         
* G/L INTERFACE RECORD                                                *         
***********************************************************************         
                                                                                
GTR      CSECT                                                                  
GTRRUNGR DC    CL12'CRKYYYYMMTT*'    ?*RUN GROUP                                
GTRSEQN  DC    CL6'000000'           ?*SEQUENCE NUMBER                          
GTRCPY   DC    CL4'0000'               COMPANY CODE                             
GTROCPY  DC    CL35'0400XXXXX'       ?*OLD COMPANY CODE                         
GTROACCN DC    CL25'LANGIL000'       ?*OLD ACCOUNT NUMBER                       
GTRSRC   DC    CL2'NL'                 SOURCE CODE                              
GTRDATE  DC    CL8'YYYYMMDD'          *FEED(RUN) DATE                           
GTRREF   DC    CL10' '                 REFERENCE                                
GTRDESC  DC    CL30' '                *DESCREPTION(ACCOUNT NAME)                
GTRCURR  DC    CL5'USD'                CURRENCY                                 
GTRUNTS  DC    CL15'00000000000000 ' ? UNITS                                    
GTRAMNT  DC    CL15'00000000000000 ' ?*TRANSACTION AMOUNT                       
GTRBASE  DC    CL15'00000000000000 ' ?*BASE AMOUNT(SAME AS TRANSACTION)         
GTRBRTE  DC    CL12'10000000000 '    ? BASE RATE                                
GTRSYST  DC    CL2'NL'                 SYSTEM                                   
GTRPGMC  DC    CL5'DDS'                PROGRAM CODE                             
GTRAUTR  DC    C'N'                    AUTO REVERSE                             
GTRPSTDT DC    CL8'YYYYMMDD'         ?*POSTING DATE                             
GTRACTV  DC    CL15' '                 ACTIVITY                                 
GTRACCAT DC    CL5' '                  ACCOUNT CATEGORY                         
GTRDOCN  DC    CL15' '                 DOCUMENT NUMBER                          
GTRBASAM DC    CL15'00000000000000 ' ? BASE AMOUNT                              
GTREFFDT DC    CL8'YYYYMMDD'         ?*EFFECTIVE DATE(SAME AS POSTING)          
GTRJRNBK DC    CL12' '                 JOURNAL BOOOK                            
GTRMXV1  DC    CL20' '                 MATRIX ATTRIBUTE                         
GTRMXV2  DC    CL20' '                 MATRIX ATTRIBUTE                         
GTRMXV3  DC    CL20' '                 MATRIX ATTRIBUTE                         
GTRLNQ   EQU   *-GTR                                                            
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD CONTROL TABLES                                             *         
***********************************************************************         
                                                                                
DCVI     CSECT                                                                  
         DC    AL1(DWNTEXT),AL1(L'CVICPY),AL3(CVICPY)                           
         DC    AL1(DWNTEXT),AL1(L'CVIVNDID),AL3(CVIVNDID)                       
         DC    AL1(DWNTEXT),AL1(L'CVIVIVNO),AL3(CVIVIVNO)                       
         DC    AL1(DWNTEXT),AL1(L'CVIIVDTE),AL3(CVIIVDTE)                       
         DC    AL1(DWNTEXT),AL1(L'CVIPONUM),AL3(CVIPONUM)                       
         DC    AL1(DWNTEXT),AL1(L'CVIDESC),AL3(CVIDESC)                         
         DC    AL1(DWNNUMB),AL1(L'CVIBIVAM),AL3(CVIBIVAM)                       
         DC    AL1(DWNNUMB),AL1(L'CVIIVAMT),AL3(CVIIVAMT)                       
         DC    AL1(DWNTEXT),AL1(L'CVIPSTD),AL3(CVIPSTD)                         
         DC    AL1(DWNTEXT),AL1(L'CVIPAYVD),AL3(CVIPAYVD)                       
         DC    AL1(DWNEOL),AL1(0),AL3(0)                                        
         DC    X'FF'                                                            
*                                                                               
DCVD     CSECT                                                                  
         DC    AL1(DWNTEXT),AL1(L'CVDCPY),AL3(CVDCPY)                           
         DC    AL1(DWNTEXT),AL1(L'CVDVNDID),AL3(CVDVNDID)                       
         DC    AL1(DWNTEXT),AL1(L'CVDVIVNO),AL3(CVDVIVNO)                       
         DC    AL1(DWNTEXT),AL1(L'CVDDSTSQ),AL3(CVDDSTSQ)                       
         DC    AL1(DWNNUMB),AL1(L'CVDDSTAM),AL3(CVDDSTAM)                       
         DC    AL1(DWNTEXT),AL1(L'CVDDSTAU),AL3(CVDDSTAU)                       
         DC    AL1(DWNTEXT),AL1(L'CVDDSTAN),AL3(CVDDSTAN)                       
         DC    AL1(DWNEOL),AL1(0),AL3(0)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PROGRAM DSECT                                                       *         
***********************************************************************         
                                                                                
ACIKD    DSECT                                                                  
PROCSW   DS    X                       PROCESS CONTROL SWITCH                   
PROCPAY  EQU   X'80'                   PROCESSING PAYABLE TAPE                  
PROCGL   EQU   X'40'                   PROCESSING GL TAPE                       
ALL      EQU   X'FF'                                                            
*                                                                               
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
*                                                                               
ATAPE    DS    A                       A(TAPE DCB)                              
*                                                                               
TODAY20  DS    CL8                     YYYYMMDD(CHARACTER)                      
TODAY1   DS    XL3                     YYMMDD(PACKED)                           
TODAY2   DS    XL2                     YYMMDD(COMPRESSED)                       
*                                                                               
START2   DS    XL2                                                              
END2     DS    XL2                                                              
*                                                                               
ELCODE   DS    X                                                                
VNDR     DS    CL(L'CVIVNDID)          VENDOR (RIGHT JUSTIFIED)                 
*                                                                               
SREC     DS    0X                      SORT RECORD                              
SRVNDR   DS    CL14                    UNIT/LEDGER/VENDOR                       
SRINVN   DS    CL(L'CVIVIVNO)          INVOICE NUMBER                           
SRINVD   DS    XL3                     INVOICE DATE                             
SRTYP    DS    XL1                     TYPE                                     
SRHDR    EQU   0                       HEADER                                   
SRDTL    EQU   1                       DETAIL                                   
SRHLNQ   EQU   *-SREC                                                           
SRVNDID  DS    CL(L'CVIVNDID)          VENDOR (RIGHT JUSTIFIED)                 
SRCLI    DS    CL3                     CLIENT CODE                              
SRPRD    DS    CL3                     PRODUCT CODE                             
SRKLNQ   EQU   *-SREC                                                           
SRAMT    DS    PL6                     AMOUNT                                   
SRPON    DS    CL7                     PO NUMBER                                
SRNAR    DS    CL30                    NARRATIVE                                
SRLNQ    EQU   *-SREC                                                           
*                                                                               
HEADR    DS    XL(SRLNQ)               HEADER                                   
SEQ      DS    PL3                     DETAIL SEQUENCE NUMBER                   
*                                                                               
DWNSTAT  DC    X'00'      DOWNLOAD CONTROL STATUS                               
DWNLOAD  EQU   X'80'      DOWNLOAD RUNNING                                      
DWNINIT  EQU   X'40'      BLOCK HAS BEEN INITIALIZED                            
DWNHEAD  EQU   X'20'      HEADLINES HAVE BEEN PROCESSED                         
DWNMICRO EQU   X'10'      DOWNLOAD MICRO FORMAT (MUST SET DWNLOAD BIT           
DWNNOPRT EQU   X'01'      DON'T PRINT THIS DOWNLOAD ENTRY                       
*                                                                               
DWNMODE  DC    X'00'      DOWNLOAD MODE                                         
DWNTEXT  EQU   1          PUT TEXT                                              
DWNNUMB  EQU   2          PUT NUMBER                                            
DWNEOL   EQU   3          END OF LINE                                           
DWNEOR   EQU   4          END OF REPORT                                         
DWNROWS  EQU   5          PUT ROW CODE/NAMES                                    
*                                                                               
DWNBUF   DS    CL300                   DOWNLOAD BUFFER                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR DCB'S                                                     *         
***********************************************************************         
                                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
                                                                                
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
                                                                                
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
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
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
         PRINT ON                                                               
* DDBUFFALOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
* DDCOMFACSC                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPIK02 08/17/00'                                      
         END                                                                    
