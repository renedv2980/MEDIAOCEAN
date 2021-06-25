*          DATA SET ACREPP202  AT LEVEL 026 AS OF 07/23/13                      
*PHASE ACP202C                                                                  
*INCLUDE ACCDIV                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'ORDER/ESTIMATE SUMMARY'                                         
ACP202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP2**,R8,RR=RE                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACP2D,RC                                                         
         ST    RE,PRELOC                                                        
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   ACP1A                                                            
         L     RF,=A(BUFFALOC)                                                  
         A     RF,PRELOC                                                        
         ST    RF,ADBUFF                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',(RF)                                        
         LA    RF,6                                                             
         LA    RE,BUFAC1                                                        
ACP1     ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,ACP1                                                          
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
ACP1A    CLI   MODE,REQFRST        REQFRST                                      
         BNE   LVAFST                                                           
         GOTO1 PROLLER,DMCB,0,TAB,4,7                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   INTEXTSW,C'N'                                                    
         L     RF,ADCMPEL                                                       
         USING ACCOMPD,RF                                                       
         TM    ACMPSTA2,X'20'                                                   
         BZ    EXIT                                                             
         MVI   INTEXTSW,C'Y'       SHOW BOTH ESTIMATE AMOUNTS                   
         MVI   ACCWDTH,X'19'       WIDTH OF NAME COLUMN                         
         B     EXIT                                                             
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
LVAFST   CLI   MODE,LEVAFRST       FIRST FOR CLIENT                             
         BNE   LVBFST                                                           
         ZAP   PRODCT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
LVBFST   CLI   MODE,LEVBFRST       FIRST FOR PRODUCT                            
         BNE   ACCFST                                                           
         MVI   PRODPEND,0          SET PRODUCT PRINT PENDING                    
         ZAP   JOBCT,=P'0'                                                      
         B     EXIT                                                             
         EJECT                                                                  
ACCFST   CLI   MODE,PROCACC                                                     
         BNE   TRANREC                                                          
         MVC   ACCUMS,=7PL6'0'                                                  
         MVI   JOBACCT,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         LA    RF,6                                                             
         LA    RE,BUFAC1                                                        
*                                                                               
ACCF02   ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,ACCF02                                                        
*                                                                               
         CLI   QOPT2,C' '          CLOSED OPTION                                
         BE    ACCF06                                                           
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLI   QOPT2,C'S'                                                       
         BE    ACCF04                                                           
         TM    ACSTSTAT,X'40'      CLOSED ONLY                                  
         BZ    EXIT                                                             
         B     ACCF06                                                           
*                                                                               
ACCF04   TM    ACSTSTAT,X'40'      SUPRESS CLOSED                               
         BO    EXIT                                                             
*                                                                               
ACCF06   CLI   QOPT3,C' '          LOCKED OPTION                                
         BE    ACCF10                                                           
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLI   QOPT3,C'S'                                                       
         BE    ACCF08                                                           
         TM    ACSTSTAT,X'20'      LOCKED ONLY                                  
         BZ    EXIT                                                             
         B     ACCF10                                                           
*                                                                               
ACCF08   TM    ACSTSTAT,X'20'      SUPRESS LOCKED                               
         BO    EXIT                                                             
*                                                                               
ACCF10   MVI   FCRDTRNS,C'Y'                                                    
         L     R2,ADACCBAL                                                      
         USING ACBALD,R2                                                        
         LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZAP   CHARGE,ACBLDR                                                    
         ZAP   BILLS,ACBLCR                                                     
         SP    UNCOMM,ACBLDR       EST - ORDERS AND CHARGES                     
*                                                                               
         BAS   RE,LOOKUP                                                        
*                                                                               
         USING JBLOCKD,R5                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   ACCF40              NO                                           
         USING MJETABD,R3                                                       
         AP    UNCOMM,MJETVAL+6(6)                                              
         ZAP   EST,MJETVAL+6(6)                                                 
         CLI   INTEXTSW,C'Y'                                                    
         BNE   *+10                                                             
         ZAP   ORIG,MJETVAL                                                     
*                                                                               
ACCF12   CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    EXIT                YES                                          
         CLI   MJETTYP,MJETTWQ     LOOK FOR WORKCODES ONLY                      
         BNE   ACCF14                                                           
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   ACCF14                                                           
         MVI   JOBACCT,C'Y'                                                     
         MVC   BUFKEY,MJETWCD                                                   
         ZAP   BUFAC3,MJETVAL+6(6)                                              
         ZAP   BUFAC6,MJETVAL+6(6)                                              
         CLI   INTEXTSW,C'Y'                                                    
         BNE   *+10                                                             
         ZAP   BUFAC2,MJETVAL                                                   
         BAS   RE,BUFADD                                                        
*                                                                               
ACCF14   XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     ACCF12                                                           
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
ACCF40   AP    UNCOMM,JBCOLVAL+6(6)                                             
         ZAP   EST,JBCOLVAL+6(6)                                                
         CLI   INTEXTSW,C'Y'                                                    
         BNE   *+10                                                             
         ZAP   ORIG,JBCOLVAL                                                    
         LH    R1,JBNROWS                                                       
*                                                                               
ACCF42   CLI   JBCOLTYP,JBCOLTWC   LOOK FOR WORKCODES ONLY                      
         BNE   ACCF44                                                           
         MVI   JOBACCT,C'Y'                                                     
         MVC   BUFKEY,JBCOLWC                                                   
         ZAP   BUFAC3,JBCOLVAL+6(6)                                             
         ZAP   BUFAC6,JBCOLVAL+6(6)                                             
         CLI   INTEXTSW,C'Y'                                                    
         BNE   *+10                                                             
         ZAP   BUFAC2,JBCOLVAL                                                  
         BAS   RE,BUFADD                                                        
*                                                                               
ACCF44   AH    R3,JBLCOL                                                        
         BCT   R1,ACCF42                                                        
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
TRANREC  CLI   MODE,PROCTRNS       TRANSACTIONS                                 
         BNE   ACCLST                                                           
         L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   0(R2),X'44'                                                      
         BNE   EXIT                                                             
         CLI   TRNSTYPE,X'0C'      ORDER                                        
         BNE   ACP34                                                            
         MVI   ELCODE,X'68'        ORDER AMOUNT ELEMENT                         
         BAS   RE,FIRSTEL                                                       
         BNE   EXIT                                                             
         USING ACOAMTD,R2                                                       
         MVI   JOBACCT,C'Y'                                                     
         XC    BUFKEY,BUFKEY                                                    
ACP32    DS    0H                                                               
         LA    RF,ORDS                                                          
         LA    RE,BUFAC1                                                        
         CLI   INTEXTSW,C'Y'                                                    
         BE    *+12                                                             
         LA    RF,ORIG             SKIP FIRST COL IF SHOWING 1 ESTIMATE         
         LA    RE,BUFAC2                                                        
         AP    0(6,RF),ACOAMT      ORDER AMOUNT                                 
         SP    0(6,RF),ACOAIVAL    LESS INVOICED SO FAR                         
         AP    0(8,RE),ACOAMT                                                   
         SP    0(8,RE),ACOAIVAL                                                 
         SP    UNCOMM,ACOAMT                                                    
         AP    UNCOMM,ACOAIVAL                                                  
         SP    BUFAC6,ACOAMT                                                    
         AP    BUFAC6,ACOAIVAL                                                  
*        OC    BUFKEY,BUFKEY       ONLY PUT IN ONE W-CODE(US)                   
*        BNZ   *+10                                                             
         MVC   BUFKEY,ACOAWC                                                    
         BAS   RE,BUFADD                                                        
         MVI   ELCODE,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BE    ACP32                                                            
         B     EXIT                                                             
*                                                                               
         USING TRANSD,R2                                                        
ACP34    TM    TRNSSTAT,X'80'                                                   
         BZ    EXIT                SKIP BILLS                                   
         MVI   JOBACCT,C'Y'                                                     
         LR    R3,R2                                                            
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3                                                        
         MVC   BUFKEY,ACKEYWRK                                                  
         ZAP   BUFAC4,TRNSAMNT     CHARGES                                      
         SP    BUFAC6,TRNSAMNT     UNCOMM EST = EST - ORDS AND CHARGES          
*                                                                               
         USING ACMD,RF                                                          
ACP35    L     RF,AMONACC          PRODUCTION ACTIVITY ELEMENT                  
         L     R2,ACMAPROB                                                      
         USING PRORATAD,R2                                                      
         ZAP   BUFAC5,PA$NETBL                                                  
         AP    BUFAC5,PA$WOFAM     THIS PROG DON'T KNOW ABOUT W/O'S             
*                                                                               
         AP    ALLOCTOT,BUFAC5     ACCUMULATE JOB TOTAL                         
*                                                                               
ACP39    BAS   RE,BUFADD                                                        
         B     EXIT                                                             
         DROP  R2,RF                                                            
         EJECT                                                                  
ACCLST   CLI   MODE,ACCLAST        END-OF-JOB                                   
         BNE   ACP50                                                            
         CLI   FCRDTRNS,C'N'                                                    
         BE    EXIT                                                             
         CLI   PROGPROF,C'Y'       PROFILE TO PRINT ALL JOBS                    
         BE    ACP41                                                            
         CLC   ACCUMS,=7PL6'0'                                                  
         BNE   ACP41               IF NO ACTIVITY - EXIT                        
         CLI   JOBACCT,C'Y'                                                     
         BNE   EXIT                                                             
ACP41    CLI   PRODPEND,0          PRINT PRODUCT CODE-IF NOT DONE YET           
         BNE   ACP42                                                            
         MVI   PRODPEND,1                                                       
         L     R2,ADACC                                                         
         L     R3,ADLDGHIR                                                      
         GOTO1 =V(ACCDIV),DMCB,(R3),(R2),WORK,RR=RB                             
         MVC   P+1(6),WORK+14      PRODUCT CODE                                 
         L     R2,ADLVBNAM                                                      
         LA    R3,P+8                                                           
         BAS   RE,NAMOUT                                                        
         BAS   RE,MYREPORT                                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   ACP42                                                            
         GOTO1 MYREPORT                                                         
*                                                                               
ACP42    DS    0H                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,BUFPRT                                                        
         GOTO1 PROLLER,DMCB,1,TAB,1                                             
*                                  ADDRESS LINE 1                               
         L     RF,DMCB                                                          
         MVC   0(6*6,RF),ACCUMS                                                 
         GOTO1 PROLLER,DMCB,6      ADD DOWN LINE 1                              
         LA    R3,1                                                             
         CLC   ALLOCTOT,BILLS                                                   
         BE    *+8                                                              
         MVI   P+95,C'*'           BILLS/ALLOCATION NOT EQUAL                   
         BAS   RE,FORMAT                                                        
         L     R3,ADLDGHIR                                                      
         L     R2,ADACC                                                         
         GOTO1 =V(ACCDIV),DMCB,(R3),(R2),WORK,RR=RB                             
         MVC   TEMP,SPACES                                                      
         MVC   TEMP(6),WORK+14     PRODUCT                                      
         MVC   TEMP+7(6),WORK+27   JOB                                          
         GOTO1 =V(SQUASHER),DMCB,TEMP,20,RR=PRELOC                              
         L     RF,DMCB+4                                                        
         MVC   P+1(20),TEMP                                                     
         LA    R3,P+2(RF)                                                       
         L     R2,ADACCNAM                                                      
         BAS   RE,NAMOUT                                                        
         BAS   RE,MYREPORT                                                      
         GOTO1 MYREPORT                                                         
         AP    JOBCT,=P'1'                                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 MYREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
ACP50    CLI   MODE,LEVBLAST       END-OF-PRODUCT                               
         BNE   ACP60                                                            
         LA    R3,2                                                             
         BAS   RE,FORMAT                                                        
         CP    JOBCT,=P'1'         IF ONLY 1 JOB-NO PRODUCT TOTALS              
         BH    ACP52                                                            
         MVC   P,SPACES                                                         
         CP    JOBCT,=P'0'                                                      
         BE    EXIT                                                             
         ZAP   JOBCT,=P'0'                                                      
         B     ACP53                                                            
*                                                                               
ACP52    LA    RF,P+26                                                          
         CLI   INTEXTSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    RF,P+15                                                          
         MVC   0(18,RF),=C'TOTALS FOR PRODUCT'                                  
         BAS   RE,MYREPORT                                                      
         BAS   RE,MYREPORT                                                      
ACP53    AP    PRODCT,=P'1'                                                     
         ZAP   JOBCT,=P'0'                                                      
         B     EXIT                                                             
         EJECT                                                                  
ACP60    CLI   MODE,LEVALAST       END-OF-CLIENT                                
         BNE   ACP70                                                            
         CP    PRODCT,=P'1'                                                     
         BH    ACP62                                                            
         MVC   P,SPACES                                                         
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 PROLLER,DMCB,1,TAB,3                                             
         L     RF,DMCB                                                          
         MVC   0(36,RF),=6PL6'0'                                                
         ZAP   PRODCT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
ACP62    DS    0H                                                               
         BAS   RE,MYREPORT                                                      
         LA    R3,3                                                             
         BAS   RE,FORMAT                                                        
         LA    RF,P+26                                                          
         CLI   INTEXTSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    RF,P+15                                                          
         MVC   0(17,RF),=C'TOTALS FOR CLIENT'                                   
         BAS   RE,MYREPORT                                                      
         MVI   FORCEHED,C'Y'                                                    
         ZAP   PRODCT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
ACP70    CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         LA    R3,4                                                             
         BAS   RE,FORMAT                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    RF,P+26                                                          
         CLI   INTEXTSW,C'Y'                                                    
         BNE   *+8                                                              
         LA    RF,P+15                                                          
         MVC   0(18,RF),=C'TOTALS FOR REQUEST'                                  
         BAS   RE,MYREPORT                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              VARIOUS SUB-ROUTINES                                             
*                                                                               
FORMAT   NTR1                                                                   
*                                  ADDRESS CORRECT LINE                         
         GOTO1 PROLLER,DMCB,1,TAB,(R3)                                          
         L     R2,DMCB                                                          
         LA    R2,6(R2)                                                         
         LA    RF,P+48                                                          
         LA    RE,5                                                             
         CLI   INTEXTSW,C'Y'                                                    
         BNE   FMT2                                                             
         LA    RF,P+37                                                          
         LA    RE,6                                                             
         SH    R2,=H'6'                                                         
FMT2     CP    0(6,R2),=P'0'                                                    
         BE    FMT4                                                             
         EDIT  (P6,0(R2)),(11,0(RF)),2,MINUS=YES                                
FMT4     LA    R2,6(R2)                                                         
         LA    RF,12(RF)                                                        
         BCT   RE,FMT2                                                          
*                                  CLEAR LINE JUST PRINTED                      
         GOTO1 PROLLER,DMCB,2,TAB,(R3)                                          
         B     EXIT                                                             
         EJECT                                                                  
BUFADD   NTR1                      ADD A RECORD TO BUFFALO                      
         CLI   QOPT1,C'Y'                                                       
         BNE   BUFAD2                                                           
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFF,BUFREC                               
BUFAD2   LA    RF,6                                                             
         LA    RE,BUFAC1                                                        
BUFAD4   ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,BUFAD4                                                        
         B     EXIT                                                             
         EJECT                                                                  
BUFPRT   NTR1                      PRINT BUFFALO RECORDS                        
         XC    BUFKEY,BUFKEY                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFF,BUFREC,1                            
*                                                                               
BUFP2    TM    DMCB+8,X'80'                                                     
         BO    BUFPXIT                                                          
         MVC   P+5(2),BUFKEY                                                    
         L     R2,ADLEDGER                                                      
         AH    R2,DATADISP         WORK-CODE NAME                               
BUFP2A   CLI   0(R2),0                                                          
         BE    BUFP3                                                            
         CLI   0(R2),X'12'                                                      
         BE    BUFP2E                                                           
BUFP2C   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     BUFP2A                                                           
         USING ACANALD,R2                                                       
BUFP2E   CLC   BUFKEY,ACANCODE                                                  
         BNE   BUFP2C                                                           
         MVC   P+9(15),ACANDESC                                                 
BUFP3    DS    0H                                                               
         LA    R2,BUFAC2                                                        
         LA    RF,P+48                                                          
         LA    RE,5                                                             
         CLI   INTEXTSW,C'Y'                                                    
         BNE   BUFP4                                                            
         LA    RF,P+37                                                          
         LA    RE,6                                                             
         SH    R2,=H'8'                                                         
BUFP4    CP    0(8,R2),=P'0'                                                    
         BE    BUFP6                                                            
         EDIT  (P8,0(R2)),(11,0(RF)),2,MINUS=YES                                
BUFP6    LA    R2,8(R2)                                                         
         LA    RF,12(RF)                                                        
         BCT   RE,BUFP4                                                         
         GOTO1 MYREPORT                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFF,BUFREC,1                             
         B     BUFP2                                                            
*                                                                               
BUFPXIT  DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFF                                    
         B     EXIT                                                             
         EJECT                                                                  
MYREPORT NTR1                                                                   
         CLI   MODE,REQLAST                                                     
         BE    MYRPT1                                                           
         L     RF,ADHEIRA          PUT CLIENT CODE/NAME IN HEADS                
         MVC   HEAD4+10(6),3(RF)                                                
         LA    R3,HEAD4+17                                                      
         L     R2,ADLVANAM                                                      
         USING ACNAMED,R2                                                       
         ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACNMNAME                                                 
MYRPT1   CLI   INTEXTSW,C'Y'                                                    
         BE    MYRPT2                                                           
         MVC   HEAD6+49(21),=C'UNMATCHED    ESTIMATE'                           
         MVC   HEAD7+49(21),=C'  ORDERS     --------'                           
         B     MYRPT4                                                           
*                                                                               
MYRPT2   DS    0H                                                               
         MVC   HEAD6+38(33),=C'UNMATCHED    EXTERNAL    INTERNAL'               
         MVC   HEAD7+38(33),=C'  ORDERS     ESTIMATE    ESTIMATE'               
MYRPT4   DS    0H                                                               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
*                                                                               
NAMOUT   NTR1                                                                   
         MVC   WORK(36),SPACES                                                  
         USING ACNAMED,R2                                                       
         ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         ZIC   RF,ACCWDTH                                                       
         GOTO1 CHOPPER,DMCB,(36,WORK),((RF),0(R3)),(C'P',2)                     
         B     EXIT                                                             
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
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'OE,CE'                                                         
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
ACCWDTH  DC    AL1(36)                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
*                                                                               
ACP2D    DSECT                                                                  
ADBUFF   DS    A                                                                
PRELOC   DS    F                                                                
TAB      DS    CL176               4 LINES X 7 COLUMNS                          
PRODPEND DS    CL1                                                              
ACCUMS   DS    0CL42                                                            
ORDS     DS    PL6                                                              
ORIG     DS    PL6                                                              
EST      DS    PL6                                                              
CHARGE   DS    PL6                                                              
BILLS    DS    PL6                                                              
UNCOMM   DS    PL6                                                              
ALLOCTOT DS    PL6                                                              
JOBCT    DS    PL4                                                              
PRODCT   DS    PL4                                                              
ELCODE   DS    CL1                                                              
INTEXTSW DS    CL1                                                              
JOBACCT  DS    CL1                                                              
TEMP     DS    CL20                                                             
BUFREC   DS    0CL50                                                            
BUFKEY   DS    CL2                                                              
BUFAC1   DS    PL8                 ORDERS                                       
BUFAC2   DS    PL8                 ORIG EST                                     
BUFAC3   DS    PL8                 PRES EST                                     
BUFAC4   DS    PL8                 CHARGES                                      
BUFAC5   DS    PL8                 BILLS                                        
BUFAC6   DS    PL8                 UNCOMM EST                                   
         EJECT                                                                  
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
PRORATAD DSECT                                                                  
       ++INCLUDE ACPRORATAD                                                     
         EJECT                                                                  
         BUFF  LINES=100,ROWS=1,COLUMNS=6,FLAVOR=PACKED,KEYLIST=(2,A)           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACREPP202 07/23/13'                                      
         END                                                                    
