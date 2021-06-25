*          DATA SET ACREPP402  AT LEVEL 062 AS OF 05/01/02                      
*PHASE ACP402A                                                                  
*INCLUDE ACCDIV                                                                 
*INCLUDE RIGHT                                                                  
         TITLE 'UNMATCHED ORDERS BY JOB OR EXPENSE ACCOUNT'                     
ACP402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP4**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=GENERAL W/S                               
         LA    RC,SPACEND                                                       
         USING ACP4D,RC            RC=PROGRAM W/S                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,RUNFRST        FIRST TIME THRU MONACC?                      
         BNE   UJ10                NO, GO TO PROCESS A REQUEST                  
         L     RF,=A(BUFFALOC)     YES, SAVE ADDRESS OF BUFFALO                 
         A     RF,RELO                                                          
         ST    RF,ADBUFC                                                        
         GOTO1 PROLLER,DMCB,0,TAB,4,5       INITIALIZE ARRAY FOR JOB,           
*                                           PRODUCT, CLIENT  & REQUEST          
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC INITIALIZE BUFFALO                   
         B     EXIT                BACK TO MONACC                               
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
UJ10     CLI   MODE,REQFRST        FIRST TIME THRU A REQUEST?                   
         BNE   UJ20                NO, GO TO CHECK FOR ACCOUNT RECORD           
         MVI   FORCEHED,C'Y'       YES, SET HEADING ON                          
         MVC   PAGE,=H'1'          SET PAGE TO 1                                
         MVC   PAKSAVE,SPACES      CLEAR PACK SAVE AREA                         
         MVI   CACTIV,C'N'         SET CLIENT TO NOT ACTIVE                     
         MVI   PACTIV,C'N'         SET PRODUCT TO NOT ACTIVE                    
         L     RF,ADCMPEL                                                       
         USING ACCOMPD,RF                                                       
         MVI   TYPE,C'P'           SET PRODUCTION (I.E. NOT EXPENSE)            
         MVI   RCSUBPRG,0          SET PRODUCTION HEADINGS PGM SW ON            
         CLC   QUNIT(2),ACMPJOB    UNIT=S & LEDGER=J?                           
         BE    EXIT                YES, RETURN TO MONACC                        
         MVI   TYPE,C'E'           NO, SET EXPENSE IF NOT SJ                    
         MVI   RCSUBPRG,1          SET EXPENSE HEADINGS PGM SW ON               
         B     EXIT                RETURN TO MONACC                             
         SPACE 1                                                                
UJ20     CLI   MODE,LEVAFRST       NEW CLIENT RECORD?                           
         BNE   UJ30                NO, GO TO CHECK NEW PRODUCT                  
         ZAP   CTPACTIV,=P'0'      YES, BEGIN WITH 0 ACTIVE PRODUCTS            
         B     EXIT                RETURN TO MONACC                             
         SPACE 1                                                                
UJ30     CLI   MODE,LEVBFRST       NEW PRODUCT RECORD?                          
         BNE   UJ40                NO, GO TO CHECK JOBS                         
         ZAP   CTJACTIV,=P'0'      YES, BEGIN WITH 0 ACTIVE JOBS                
         B     EXIT                RETURN TO MONACC                             
         SPACE 1                                                                
UJ40     CLI   MODE,PROCACC        AN ACCOUNT RECORD TO PROCESS?                
         BNE   UJ50                NO, GO TO CHECK FOR TRANSACTION              
         MVI   JACTIV,C'N'         YES, SET JOB TO NOT ACTIVE                   
         B     EXIT                RETURN TO MONACC                             
         EJECT                                                                  
UJ50     CLI   MODE,PROCTRNS       TRANSACTION RECORD TO PROCESS?               
         BNE   UJ100               NO, GO TO CHECK LAST TIME                    
*                                      THRU THIS ACCOUNT                        
         L     R2,ADTRANS          YES, PROCESS THIS TRANSACTION                
         CLI   0(R2),X'44'         TRANSACTION ELEMENT?                         
         BNE   EXIT                NO, RETURN TO MONACC                         
         USING TRANSD,R2           (YES, PROCESS THIS TRANSACTION)              
         CLC   TRNSANAL,=C'**'     PRODUCTION ORDER TRANSACTION?                
         BNE   EXIT                NO, RETURN TO MONACC                         
         XC    BUFREC,BUFREC       YES, CLEAR PGM BUFFER RECORD AREA            
         MVC   BUFDSUPN,SPACES     MOVE SPACES TO SUPPLIER NAME                 
         L     RF,ADSUBAC          PROCESS SUBACCT HDR (ELEMENT X'43')          
         USING TRSUBHD,RF                                                       
         ZIC   RE,TRSBLEN                                                       
         SH    RE,=H'18'                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BUFDSUPN(0),TRSBNAME SAVE SUPPLIER NAME (VAR. LENGTH)            
         MVC   BUFKSUPP,TRSBACNT+1 SAVE UNIT,LEDGER,ACCOUNT                     
         MVC   BUFKONO,TRNSREF     SAVE ORDER NO.                               
         MVC   BUFDDTE,TRNSDATE    SAVE TRANSACTION DATE                        
         ZAP   BUFDAMT,=P'0'       CLEAR ORDER AMOUNT                           
*                                                                               
UJ60     SR    RF,RF               LOOP THRU RECORD FOR EACH ELEMENT            
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    EXIT                YES, RETURN TO MONACC                        
         CLI   0(R2),X'68'         NO, PRODUCTION ORDER AMT. ELEMENT?           
         BNE   UJ60                NO, GO TO REST OF LOOP THRU RECORD           
*                                  YES, PROCESS THIS PRODUCTION ORDER           
*                                  AMOUNT ELEMENT                               
         MVI   JACTIV,C'Y'         SET JOB TO ACTIVE                            
         MVI   PACTIV,C'Y'         SET PRODUCT TO ACTIVE                        
         MVI   CACTIV,C'Y'         SET CLIENT TO ACTIVE                         
         USING ACOAMTD,R2                                                       
UJ62     MVC   BUFKWC,ACOAWC                                                    
         ZAP   BUFDAMT,=P'0'                                                    
         AP    BUFDAMT,ACOAMT      ORDERED AMOUNT                               
         SP    BUFDAMT,ACOAIVAL    LESS INVOICED SO FAR                         
*                                                                               
         CLI   TYPE,C'E'           SKIP IF EXPENSE                              
         BE    UJ88                OTHERWISE CALCULATE COMMISSION               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
         MVC   GOSELWC,ACOAWC                                                   
         MVC   GOAKEY,ACMALTN                                                   
         GOTO1 GETOPT,DMCB,(R6)                                                 
         XC    GOSELWC,GOSELWC                                                  
         XC    GOAKEY,GOAKEY                                                    
         MVC   BUFDCOM,GOAGYCOM    DEDUCED COMMISSION RULE FOR THIS JOB         
         SPACE 1                                                                
UJ88     DS    0H                  SAVE RECORD TO BUFFALO AREA                  
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         MVI   BUFTYPE,1           INDICATE MINOR WORK CODE                     
         B     UJ60                GO TO SEARCH FOR NEXT PRODUCTION             
*                                  ELEMENT                                      
         EJECT                                                                  
UJ100    CLI   MODE,ACCLAST        LAST TIME THRU FOR THIS JOB?                 
         BNE   UJ150               NO, GO TO CHECK FOR BREAKS                   
         CLI   JACTIV,C'Y'         YES, ANY UNMATCHED PRODUCTION ORDERS         
*                                  FOR THIS JOB?                                
         BNE   EXIT                NO, RETURN TO MONACC                         
         MVI   JACTIV,C'N'         YES,TURN OFF FOR NEXT JOB PROCESSING         
         AP    CTJACTIV,=P'1'      ADD 1 TO ACTIVE JOBS COUNTER                 
         MVI   FORCEHED,C'Y'       FORCE HEADING FOR NEW JOB                    
         XC    BUFKEY,BUFKEY       CLEAR BUFFER AREA                            
         XC    ORDTOTS,ORDTOTS     CLEAR TOTALS FOR A SINGLE PRODUCTION         
*                                  ORDER                                        
         MVI   LASTYPE,0           INDICATE                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
UJ102    TM    DMCB+8,X'80'                                                     
         BO    UJ110                                                            
         CLI   BUFTYPE,0                                                        
         BNE   UJ103                                                            
         CLI   LASTYPE,0                                                        
         BE    UJ102E              NO TOTALS FOR SINGLE WORK CODES.             
         MVC   PAK1(18),ORDTOTS                                                 
         MVC   P+33(16),=C'TOTALS FOR ORDER'                                    
         MVC   P+50(6),SVORDNO     ORDER NO. TO PRINT LINE                      
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2           DOUBLESPACE AFTER ORDER TOTALS               
         GOTO1 MYREPORT                                                         
*                                                                               
UJ102E   MVC   ORDTOTS,=3PL6'0'    CLEAR ORDER TOTALS.                          
         MVC   P+1(6),BUFKONO      ORDER NO TO PRINT LINE.                      
         MVC   SVORDNO,BUFKONO     SAVE ORDER NO.                               
*                                                                               
         CLI   TYPE,C'E'           DIFFERENT LAYOUT IF EXPENSE                  
         BE    UJ105                                                            
UJ103    MVC   P+9(2),BUFKWC       WORK CODE.                                   
         BAS   RE,GETWNAME                                                      
         MVC   P+12(15),SVWK       WORK CODE NAME.                              
         MVC   P+30(14),BUFKSUPP   SUPPLIER CODE.                               
         GOTO1 DATCON,DMCB,(1,BUFDDTE),(8,P+48)                                 
         ZAP   PAK1,BUFDAMT                                                     
         ZAP   PL13,BUFDAMT                                                     
         MP    PL13,BUFDCOM                                                     
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   PAK2,PL13                                                        
         ZAP   PAK3,PAK1                                                        
         AP    PAK3,PAK2           GROSS IN PAK3                                
         BAS   RE,FORMAT                                                        
         GOTO1 MYREPORT                                                         
         OC    BUFDWCS,BUFDWCS     PRINT SUBSID. W-CODES                        
         BZ    UJ106                                                            
         MVC   P+1(2),BUFDWCS                                                   
         OC    BUFDWCS+2(2),BUFDWCS+2                                           
         BZ    UJ104                                                            
         MVI   P+3,C','                                                         
         MVC   P+4(2),BUFDWCS+2                                                 
         OC    BUFDWCS+4(2),BUFDWCS+4                                           
         BZ    UJ104                                                            
         MVI   P+6,C','                                                         
         MVC   P+7(2),BUFDWCS+4                                                 
UJ104    GOTO1 MYREPORT                                                         
         B     UJ106                                                            
*                                                                               
UJ105    MVC   P+9(14),BUFKSUPP    SUPPLIER CODE                                
         ZAP   PAK1,BUFDAMT                                                     
         ZAP   PAK2,=P'0'                                                       
         ZAP   PAK3,=P'0'                                                       
         BAS   RE,FORMAT                                                        
         GOTO1 DATCON,DMCB,(1,BUFDDTE),(8,P+61)                                 
         MVC   P+25(36),BUFDSUPN                                                
         GOTO1 MYREPORT                                                         
UJ106    DS    0H                                                               
         GOTO1 PROLLER,DMCB,1,TAB,1                                             
         L     RF,DMCB             ADDRESS LINE 1                               
         AP    0(6,RF),PAK1                                                     
         AP    6(6,RF),PAK2                                                     
         AP    12(6,RF),PAK3                                                    
         AP    18(6,RF),=P'1'      NUMBER OF ORDERS                             
         AP    ORDAMT,PAK1         ACCUMULATE ORDER TOTALS.                     
         AP    ORDCOM,PAK2                                                      
         AP    ORDGRS,PAK3                                                      
         MVC   LASTYPE,BUFTYPE                                                  
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     UJ102                                                            
         SPACE 2                                                                
UJ110    CLI   LASTYPE,0                                                        
         BE    UJ111                                                            
         MVC   PAK1(18),ORDTOTS                                                 
         MVC   P+33(16),=C'TOTALS FOR ORDER'                                    
         MVC   P+50(6),SVORDNO     ORDER NO. TO PRINT LINE                      
         BAS   RE,FORMAT                                                        
         GOTO1 MYREPORT                                                         
*                                                                               
UJ111    GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         GOTO1 MYREPORT            DO JOB TOTALS                                
         GOTO1 PROLLER,DMCB,6,TAB  ADD DOWN                                     
         GOTO1 PROLLER,DMCB,1,TAB,1                                             
         L     R2,DMCB                                                          
         CP    18(6,R2),=P'1'                                                   
         BNE   UJ112                                                            
         MVC   0(24,R2),=4PL6'0'                                                
         B     EXIT                                                             
*                                                                               
UJ112    MVC   SVWK(14),=CL14'JOB'                                              
         MVC   SVWK+4(6),SVJOBCD   JOB CODE TO SVWK (THEN PRINT)                
         CLI   TYPE,C'E'                                                        
         BNE   *+10                                                             
         MVC   SVWK(7),=C'ACCOUNT'                                              
         BAS   RE,PRFORMAT                                                      
         B     EXIT                                                             
         EJECT                                                                  
UJ150    LA    R3,2                OTHER LAST TIMES                             
         MVC   SVWK(14),=CL14'PRODUCT'                                          
         MVC   SVWK+8(3),SVPRODCD  PRODUCT CODE IN SVWK (THEN PRINT)            
         CLI   MODE,LEVBLAST                                                    
         BNE   UJ1502                                                           
         CLI   TYPE,C'E'                                                        
         BE    EXIT                                                             
         CLI   PACTIV,C'Y'                                                      
         BNE   EXIT                NOTHING FOR THIS PRODUCT.                    
         MVI   PACTIV,C'N'                                                      
         AP    CTPACTIV,=P'1'      ADD 1 TO ACTIVE PRODUCTS COUNTER             
         CP    CTJACTIV,=P'1'      MORE THAN 1 ACTIVE JOB?                      
         BNH   UJ152A              NO, GO TO CLEAR PRODUCT CTRS                 
         MVI   FORCEHED,C'Y'       YES, FORCE HEADING FOR PRODUCT               
         OI    JOBHD,1             NO PROD & JOB INFO IN HEADING                
         B     UJ152                                                            
*                                                                               
UJ1502   LA    R3,3                                                             
         MVC   SVWK(14),=CL14'CLIENT'                                           
         MVC   SVWK+7(3),SVCLNTCD  SAVE CLIENT CODE                             
         CLI   MODE,LEVALAST                                                    
         BNE   UJ1504                                                           
         CLI   TYPE,C'E'                                                        
         BE    EXIT                                                             
         CLI   CACTIV,C'Y'                                                      
         BNE   EXIT                NOTHING FOR THIS CLIENT.                     
         MVI   CACTIV,C'N'                                                      
         CP    CTPACTIV,=P'1'      MORE THAN 1 ACTIVE PRODUCT?                  
         BNH   UJ152A              NO, GO TO CLEAR CLIENT CTRS                  
         MVI   FORCEHED,C'Y' YES, FORCE HEADING FOR PRODUCT                     
         OI    PRODHD,1                                                         
         B     UJ152                                                            
         SPACE 1                                                                
UJ1504   LA    R3,4                                                             
         MVC   SVWK(14),=CL14'REQUEST'                                          
         CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'       NEW HEADING FOR REQUEST TOTALS               
UJ152    GOTO1 PROLLER,DMCB,1,TAB,(R3)                                          
         L     R2,DMCB                                                          
         BAS   RE,PRFORMAT                                                      
         B     EXIT                                                             
         SPACE 1                                                                
UJ152A   GOTO1 PROLLER,DMCB,1,TAB,(R3) GET LINE OF ACCUMULATORS                 
         L     R2,DMCB             GET ADDRESS OF ACCUMULATORS                  
         MVC   0(24,R2),=4PL6'0'   ZERO OUT ACCUMULATORS                        
         B     EXIT                RETURN TO MONACC                             
         EJECT                                                                  
*              END-OF-LEVEL TOTAL PRINTING                                      
         SPACE 2                                                                
PRFORMAT NTR1                                                                   
         GOTO1 MYREPORT                                                         
         CLI   MODE,REQLAST                                                     
         BNE   *+10                                                             
         MVC   SVWK(14),=CL14'REQUEST'                                          
         MVC   P+34(10),=C'TOTALS FOR'                                          
         MVC   P+45(14),SVWK                                                    
         GOTO1 =V(RIGHT),DMCB,P+34,22 RIGHT JUSTIFY TOTALS LINE                 
         MVC   PAK1(18),0(R2)                                                   
         CLC   PAKSAVE,PAK1        SAME PRINT AS PREVIOUS -                     
         BE    PRF2                SKIP PRINTING                                
         MVC   PAKSAVE,PAK1                                                     
         BAS   RE,FORMAT                                                        
         GOTO1 MYREPORT                                                         
         MVC   P+40(16),=C'NUMBER OF ORDERS'                                    
         LA    RF,P+66                                                          
         CLI   TYPE,C'P'                                                        
         BE    *+8                                                              
         LA    RF,P+81                                                          
         EDIT  (P6,18(R2)),(4,0(RF))                                            
         GOTO1 MYREPORT                                                         
         GOTO1 MYREPORT                                                         
PRF2     MVC   0(24,R2),=4PL6'0'                                                
         MVC   P,SPACES                                                         
         B     EXIT                                                             
         SPACE 2                                                                
FORMAT   NTR1                                                                   
         LA    RF,PAK1                                                          
         LA    RE,P+60                                                          
         LA    R2,3                                                             
         CLI   TYPE,C'P'                                                        
         BE    FORMT2                                                           
         LA    RE,P+75             DIFFERENT IF EXPENSE                         
         LA    R2,1                                                             
FORMT2   EDIT  (P6,0(RF)),(11,0(RE)),2,MINUS=YES                                
         LA    RF,6(RF)                                                         
         LA    RE,14(RE)                                                        
         BCT   R2,FORMT2                                                        
         B     EXIT                                                             
         EJECT                                                                  
*              FILL HEADLINES AND PRINT                                         
         SPACE 1                                                                
MYREPORT NTR1                                                                   
         CLI   MODE,REQLAST                                                     
         BE    MYREP10                                                          
         CLI   TYPE,C'P'                                                        
         BE    MYREP2                                                           
         L     RF,ADACC            ACCOUNT CODE                                 
         MVC   HEAD7+10(12),3(RF)                                               
         LA    RF,HEAD7+21                                                      
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         LA    R3,2(RF)                                                         
         L     R2,ADACCNAM                                                      
         BAS   RE,GETNAME                                                       
         B     MYREP10                                                          
MYREP2   CLI   PRODHD,1            PRODUCT & JOB IN HEADING?                    
         BE    DOCLNTHD            NO                                           
         GOTO1 =V(ACCDIV),DMCB,ADLDGHIR,ADACC,ACDIVWK,RR=RB                     
         LA    RF,ACDIVWK                                                       
         MVC   SVCLNTCD,1(RF)      SAVE CLIENT CODE                             
         MVC   SVPRODCD,14(RF)     SAVE PRODUCT CODE                            
         MVC   SVJOBCD,27(RF)      SAVE JOB CODE                                
DOCLNTHD MVC   HEAD5+1(6),=C'CLIENT'                                            
         MVC   HEAD5+12(3),SVCLNTCD CLIENT CODE                                 
         CLI   PRODHD,1                                                         
         BE    DOCLNTNM                                                         
         MVC   HEAD6+1(7),=C'PRODUCT'                                           
         MVC   HEAD6+12(3),SVPRODCD PRODUCT CODE                                
         CLI   JOBHD,1             JOB IN HEADING?                              
         BE    DOCLNTNM            NO                                           
         MVC   HEAD7+1(3),=C'JOB'                                               
         MVC   HEAD7+12(6),SVJOBCD JOB CODE                                     
DOCLNTNM L     R2,ADLVANAM         CLIENT NAME                                  
         LA    R3,HEAD5+19         CLIENT NAME                                  
         BAS   RE,GETNAME          CLIENT NAME                                  
         CLI   PRODHD,1                                                         
         BE    NIPRODHD                                                         
         L     R2,ADLVBNAM         PRODUCT NAME                                 
         LA    R3,HEAD6+19         PRODUCT NAME                                 
         BAS   RE,GETNAME          PRODUCT NAME                                 
         CLI   JOBHD,1                                                          
         BE    NIJOBHD                                                          
         L     R2,ADACCNAM         JOB NAME                                     
         LA    R3,HEAD7+19         JOB NAME                                     
         BAS   RE,GETNAME          JOB NAME                                     
NIJOBHD  NI    JOBHD,0                                                          
MYREP10  GOTO1 ACREPORT                                                         
         B     EXIT                                                             
NIPRODHD NI    PRODHD,0                                                         
         B     MYREP10                                                          
         SPACE 1                                                                
GETNAME  ZIC   RF,1(R2)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R3),2(R2)                                                    
         EJECT                                                                  
*              DIG OUT WORK-CODE NAME AND PUT IN SVWK                           
         SPACE 2                                                                
GETWNAME NTR1                                                                   
         L     RF,ADLEDGER                                                      
         AH    RF,DATADISP                                                      
         SR    RE,RE                                                            
         MVC   SVWK(15),=CL15'MISSING'                                          
GETW2    CLI   0(RF),0                                                          
         BE    EXIT                                                             
         CLI   0(RF),X'12'                                                      
         BE    GETW6                                                            
GETW4    IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     GETW2                                                            
*                                                                               
         USING ACANALD,RF                                                       
GETW6    CLC   BUFKWC,ACANCODE                                                  
         BNE   GETW4                                                            
         MVC   SVWK(15),ACANDESC                                                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 2                                                                
ACP4D    DSECT                                                                  
RELO     DS    F                                                                
ADBUFC   DS    A                                                                
TAB      DS    CL8,20PL6                                                        
*                                                                               
ORDTOTS  DS    0CL18                                                            
ORDAMT   DS    PL6                                                              
ORDCOM   DS    PL6                                                              
ORDGRS   DS    PL6                                                              
LASTYPE  DS    C                                                                
*                                                                               
BUFREC   DS    0CL79                                                            
BUFKEY   DS    0CL23                                                            
BUFKONO  DS    CL6                 ORDER NUMBER                                 
BUFTYPE  DS    C                   0=MAJOR WK-CODE, 1=MINOR WK-CODE.            
BUFKWC   DS    CL2                 MAJOR WORK-CODE                              
BUFKSUPP DS    CL14                SUPPLIER CODE                                
BUFDATA  DS    0CL56                                                            
BUFDSUPN DS    CL36                SUPPLIER CODE                                
BUFDDTE  DS    CL3                 ORDER DATE                                   
BUFDWCS  DS    CL6                 MINOR WORK-CODES                             
BUFDAMT  DS    PL6                 ORDER AMOUNT                                 
BUFDCOM  DS    PL4                 COMMISSION RATE (2 PL)                       
BUFDCST  DS    CL1                 COMMISSION STATUS X'80' 4DP                  
TYPE     DS    CL1                                                              
JACTIV   DS    CL1                                                              
PACTIV   DS    C                                                                
CACTIV   DS    C                                                                
LASTORD  DS    CL6                                                              
PAK1     DS    PL6                                                              
PAK2     DS    PL6                                                              
PAK3     DS    PL6                                                              
PAKSAVE  DS    CL18                                                             
PL13     DS    PL13                                                             
DIV      DS    PL12                                                             
ACDIVWK  DS    CL52                FOR ACCDIV                                   
SVWK     DS    CL15                SAVE AREA FOR P LINE INFO                    
SVORDNO  DS    CL6                 SAVE AREA FOR ORDER NO.                      
SVJOBCD  DS    CL6                 SAVE AREA FOR JOB CODE                       
SVPRODCD DS    CL3                 SAVE AREA FOR PRODUCT CODE                   
SVCLNTCD DS    CL3                 SAVE AREA FOR CLIENT CODE                    
JOBHD    DC    X'00'               1=NO JOB INFO IN HEADING                     
PRODHD   DC    X'00'               1=NO PROD & JOB INFO IN HDG                  
CTJACTIV DC    PL3'0'              COUNT ACTIVE JOBS                            
CTPACTIV DC    PL3'0'              COUNT ACTIVE PRODUCTS                        
         EJECT                                                                  
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
*        PRINT ON                                                               
         SPACE 1                                                                
         BUFF  LINES=100,FLAVOR=DATA,COMMENT=56,KEYLIST=(23,A)                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062ACREPP402 05/01/02'                                      
         END                                                                    
