*          DATA SET ACREPI302  AT LEVEL 007 AS OF 04/10/15                      
*PHASE ACI302A                                                                  
*INCLUDE SORTER                                                                 
ACI302   CSECT                                                                  
         PRINT NOGEN                                                            
         TITLE 'ACI302 - PRODUCTION BILLING INTERFACE'                          
ACI302   CSECT                                                                  
         NMOD1 0,*ACI302*,R8,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    R3,TIOA                                                          
         USING TAPED,R3                                                         
         CLI   MODE,RUNFRST                                                     
         BNE   CHKRQFST                                                         
         ST    R5,RELO                                                          
         L     R7,=V(SORTER)                                                    
         A     R7,RELO                                                          
         ST    R7,SORTER                                                        
         L     R7,=A(BUFFALOC)                                                  
         A     R7,RELO                                                          
         ST    R7,ADBUFC                                                        
         EJECT                                                                  
CHKRQFST CLI   MODE,REQFRST                                                     
         BNE   CKLEVA                                                           
         MVI   SVOPNCLS,C'C'     FOR 1ST COMP AFTER SORT GET-B OPTION           
         CLI   QOPT2,C'C'                                                       
         BE    CF10                                                             
         CLI   QOPT2,C'B'                                                       
         BE    CF10                                                             
         MVI   SVOPNCLS,C'O'                                                    
         CLI   QOPT2,C'O'                                                       
         BE    CF10                                                             
         MVI   SVOPNCLS,0                                                       
CF10     GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   ES,C'F'                                                          
         MVI   FSW,C'F'                                                         
         MVI   RCSUBPRG,0                                                       
         L     R7,ADBUFC                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',(R7)                                        
         MVC   HEADS+70(12),=C'PAYABLE DATE'                                    
         MVC   HEADS2+73(9),=C'BILL DATE'                                       
         MVI   PREBUD,C'*'                                                      
         LA    R1,COMPTBL                                                       
CLICO    CLC   QCOMPANY,0(R1)                                                   
         BE    OPENT                                                            
         LA    R1,5(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CLICO                                                            
         ABEND 1                                                                
OPENT    MVC   SVCO,1(R1)                                                       
         MVC   SVCODE,3(R1)                                                     
         CLI   QOPT1,C'Y'                                                       
         BNE   NOOPEN                                                           
         CLI   QOPT2,C'B'          LISTING FOR OPEN AND CLOSED-NO TAPE          
         BE    NOOPEN                                                           
         LA    R7,ACI3TP                                                        
         USING IHADCB,R7                                                        
         MVC   DCBDDNAM+5(2),SVCO                                               
         MVC   DDPARM+5(2),SVCO                                                 
         MVC   DSPARM+13(2),SVCO                                                
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),(0,DSPARM)                              
         OPEN  ((R7),(OUTPUT))                                                  
         OPEN  (ACI3WK,(OUTPUT))                                                
NOOPEN   GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTRT3)                                
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         B     XIT                                                              
DDPARM   DC    CL8'ACI3TXX'                                                     
DSPARM   DC    CL20'ACCTAPE.AC0I3XX1'                                           
         EJECT                                                                  
CKLEVA   CLI   MODE,PROCLEVA                                                    
         BNE   CHKPRACC                                                         
         L     R9,ADHEIRA                                                       
         MVC   FOR4+8(3),3(R9)                                                  
         L     R9,ADLVANAM                                                      
         USING ACNAMED,R9                                                       
         MVC   FOR4+1(6),=C'CLIENT'                                             
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FOR4+12(0),ACNMNAME                                              
         B     XIT                                                              
         EJECT                                                                  
CHKPRACC CLI   MODE,PROCACC                                                     
         BNE   CHKPTRNS                                                         
         L     R1,ADACC                                                         
         MVC   TESTNO,SPACES                                                    
         MVC   TBUDNO,SPACES                                                    
         MVC   SVJOB,6(R1)                                                      
         MVI   TTYPE,C'2'                                                       
         MVI   ELCODE,X'27'                                                     
         L     R9,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACABILLD,R9                                                      
         MVC   TYEAR,ACABACNO                                                   
         MVI   TPMT,C' '                                                        
         MVI   TSORT,C' '        FORCE CURRENT TO BE FIRST                      
         CLI   TYEAR,C' '                                                       
         BE    CURR                                                             
         MVI   TPMT,C'A'                                                        
         MVI   TSORT,C'A'        FORCE ACCRUALS TO BE SECOND                    
         CLI   TYEAR,C'P'                                                       
         BNE   CL39                                                             
         MVI   TPMT,C'P'                                                        
         MVI   TSORT,C'P'        FORCE PRIORS TO BE LAST                        
         MVC   TYEAR,ACABACNO+1                                                 
         B     CL39                                                             
CURR     GOTO1 DATCON,DMCB,(0,QEND),(20,WORK)                                   
         MVC   TYEAR,WORK                                                       
CL39     CLI   ACABLEN,X'39'                                                    
         BNE   XIT                                                              
         MVC   TESTNO,ACABESNO                                                  
         MVC   TBUDNO,ACABBUNO                                                  
         B     XIT                                                              
         EJECT                                                                  
CHKPTRNS CLI   MODE,PROCTRNS                                                    
         BNE   CHKRQLST                                                         
         MVI   OPNORCLS,0                                                       
         CLI   QOPT2,C' '      IF = TO C,O, OR B (CLOSED, OPEN, BOTH            
         BE    NOOPTION                                                         
         CLI   QOPT2,C'Y'                                                       
         BE    NOOPTION                                                         
         BAS   RE,OPFILT     THEN FILTER ACCOUNTS FOR OPTION REQUESTED          
         BNE   XIT                 CURRENT ACCOUNT DOES NOT MEET RQST           
*                                                                               
NOOPTION L     R1,ADACC                                                         
         BAS   RE,FNDBILL                                                       
         BNE   XIT                                                              
         CLI   CODE1REC,C' '                                                    
         BNE   NO1                                                              
         MVC   SVIOA,TIOA                                                       
         MVI   CODE1REC,C'C'       FOR BOTH OR CLOSED                           
         CLI   QOPT2,C'B'                                                       
         BE    NP10                                                             
         CLI   QOPT2,C'C'                                                       
         BE    NP10                                                             
         MVI   CODE1REC,C'O'       FOR OPEN                                     
         CLI   QOPT2,C'O'                                                       
         BE    *+8                                                              
         MVI   CODE1REC,0                                                       
NP10     MVI   CODE1REC+1,C'1'                                                  
         MVI   CODE1REC+2,C'1'                                                  
         MVC   CODE1REC+3(2),SVCODE                                             
         MVC   CODE1REC+5(4),SVBILL+2                                           
         MVC   CODE1REC+9(2),SVBILL                                             
         MVC   CODE1REC+11(23),=C'DDS AGENCY PAYMENT TAPE'                      
         MVC   TIOA,CODE1REC                                                    
         BAS   RE,PUTSORT                                                       
         MVC   TIOA,SVIOA                                                       
NO1      L     R2,ADTRANS                                                       
         L     R6,ADACC                                                         
         CLI   QOPT2,C'T'                                                       
         BNE   NOTRACE                                                          
         MVC   P+6(12),3(R6)                                                    
         MVC   P(6),=C'TRACE='                                                  
         GOTO1 ACREPORT                                                         
         USING TRANSD,R2                                                        
NOTRACE  ZAP   TAMT,TRNSAMNT                                                    
         MVC   TINV,SPACES                                                      
         MVC   TINV(6),TRNSREF                                                  
         MVI   TTYPE,C'2'                                                       
         MVC   TBLANKS,SPACES                                                   
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         EJECT                                                                  
OPFILT   NTR1                                                                   
         L     RE,ADACCSTA                                                      
         USING ACSTATD,RE                                                       
*                                                                               
         MVI   OPNORCLS,C'C'       CLOSED INDICATION TO SORTREC                 
         TM    ACSTSTAT,X'40'      IS ACCOUNT CLOSED                            
         BNZ   *+8                 YES                                          
         MVI   OPNORCLS,C'O'       INDICATE OPEN                                
         CLI   QOPT2,C'B'          ARE WE LISTING BOTH                          
         BE    XIT                 YES                                          
         CLI   QOPT2,C'C'          JUST CLOSED                                  
         BNE   SE10                                                             
         CLI   OPNORCLS,C'C'                                                    
         B     XIT                 RETURN WITH EQUAL OR NE CC                   
SE10     CLI   OPNORCLS,C'O'       IT MUST BE AN OPEN ACT LIST                  
         B     XIT                                                              
         EJECT                                                                  
CHKRQLST CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
GS       GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ENDSRT                                                           
*                                                                               
         CLI   1(R5),C'1'                                                       
         BE    CODE1                                                            
         MVI   ES,C' '                                                          
         LR    R3,R5                                                            
         CLI   PREBUD,C'*'                                                      
         BE    NOBRKPRE                                                         
         LA    R3,TIOA                                                          
         B     CHKINV                                                           
SVSRT    MVC   TIOA,0(R5)                                                       
         CLI   ES,C'E'                                                          
         BE    BRK                                                              
         LA    R3,TIOA                                                          
ADDINV   AP    MINOR,TAMT                                                       
         B     GS                                                               
CHKINV   CLC   TINV,15(R5)                                                      
         BE    SVSRT                                                            
         B     BUDCHK                                                           
BRKINV   CLI   ES,C'A'                                                          
         BNE   *+8                                                              
         MVI   ES,C'E'                                                          
         CP    MINOR,=P'0'                                                      
         BE    SVSRT                                                            
         B     ZAPAMT                                                           
CODE1    MVC   TIOA,0(R5)                                                       
         B     PUTTP                                                            
ZAPAMT   ZAP   TAMT,MINOR                                                       
PUTTP    CLI   QOPT1,C'Y'                                                       
         BNE   NOTP                                                             
         CLI   QOPT2,C'B'                                                       
         BE    NOTP                                                             
         PUT   ACI3WK,TIOA                                                      
         AP    RECS,=P'1'                                                       
NOTP     CLI   TTYPE,C'1'                                                       
         BE    GS                                                               
         BAS   RE,PUTBUF                                                        
         ZAP   MINOR,=P'0'                                                      
         AP    TOT,TAMT                                                         
         MVC   P+35(12),TESTNO                                                  
         CLC   TESTNO,SPACES                                                    
         BNE   *+10                                                             
         MVC   P+35(7),=C'MISSING'                                              
         MVC   P+56(7),TINV                                                     
         EDIT  (P6,TAMT),(12,P+74),2,COMMAS=YES,FLOAT=-                         
         BAS   RE,PRT                                                           
*                                                                               
         CLI   QOPT2,C'B'                                                       
         BNE   *+14                                                             
         CLC   0(1,R5),SVOPNCLS   CHECK FOR CLOSE TO OPEN ACCT BRK              
         BNE   BRK                                                              
*                                                                               
         CLI   ES,C'E'                                                          
         BE    BRK                                                              
BUDCHK   CLC   PREYR,TSORT                                                      
         BNE   BRK                                                              
         CLC   PREBUD,TBUDNO                                                    
         BE    BRKINV                                                           
*                                                                               
BRK      CP    TOT,=P'0'                                                        
         BE    CLIES                                                            
         BAS   RE,PRT                                                           
         MVC   P+26(11),=C'TOTALS FOR '                                         
         MVC   P+37(15),PREBUD                                                  
         CLC   PREBUD,SPACES                                                    
         BNE   *+10                                                             
         MVC   P+37(28),=C'ITEMS WITH NO BUDGET NUMBERS'                        
         EDIT  (P6,TOT),(12,P+74),2,COMMAS=YES,FLOAT=-                          
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         AP    TOTAMT,TOT                                                       
         AP    TOTYR,TOT                                                        
         ZAP   TOT,=P'0'                                                        
*                                                                               
         CLI   QOPT2,C'B'                                                       
         BNE   CLIES                                                            
         CLC   0(1,R5),SVOPNCLS   IF B(BOTH REQUESTED)                          
         BE    CLIES                                                            
         CLI   SVOPNCLS,C'O'       2ND TIME THRU                                
         BE    YEARTOT                                                          
         ZAP   SVCLSTOT,TOTAMT                                                  
         ZAP   TOTAMT,=P'0'                                                     
         B     YEARTOT                                                          
*                                                                               
CLIES    MVC   P,SPACES                                                         
         CLI   ES,C'E'                                                          
         BNE   CLCYR                                                            
         B     YEARTOT                                                          
CLCYR    CLC   PREYR,TSORT                                                      
         BE    NOBRKPRE                                                         
*                                                                               
YEARTOT  MVC   P+8(10),=C'TOTALS FOR'                                           
         MVC   P+19(5),PREYR                                                    
         EDIT  (P6,TOTYR),(12,P+74),2,COMMAS=YES,FLOAT=-                        
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         ZAP   TOTYR,=P'0'                                                      
         MVC   PREYR,TSORT                                                      
*                                                                               
LAB10    CLI   ES,C'E'                                                          
         BE    ENDREQ                                                           
NOBRKPRE MVC   P+11(15),TBUDNO                                                  
         MVC   P+1(5),TSORT                                                     
         CLC   TBUDNO,SPACES                                                    
         BNE   *+10                                                             
         MVC   P+11(7),=C'MISSING'                                              
*                                                                               
         CLC   SVOPNCLS,0(R5)                                                   
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVOPNCLS,0(R5)                                                   
*                                                                               
         CLI   PREBUD,C'*'                                                      
         MVC   PREBUD,TBUDNO                                                    
         MVC   PREYR,TSORT                                                      
         BE    SVSRT                                                            
         B     BRKINV                                                           
         SPACE 2                                                                
ENDSRT   CLI   ES,C'F'            IF -F- THERE ARE NO RECORDS                   
         BE    XIT                                                              
         MVI   ES,C'A'                                                          
         LA    R3,TIOA                                                          
         B     BUDCHK                                                           
         EJECT                                                                  
ENDREQ   MVC   P+1(13),=C'REQUEST TOTAL'                                        
         CLI   QOPT2,C'B'          REFLECT TWO TOTS FOR B REQUEST               
         BNE   EN10                                                             
         BAS   RE,PRT                                                           
         MVC   P+2(15),=C'CLOSED ACCOUNTS'                                      
         EDIT  (P6,SVCLSTOT),(12,P+74),2,COMMAS=YES,FLOAT=-                     
         BAS   RE,PRT                                                           
         MVC   P+2(13),=C'OPEN ACCOUNTS'                                        
EN10     EDIT  (P6,TOTAMT),(12,P+74),2,COMMAS=YES,FLOAT=-                       
         ZAP   TOTAMT,=P'0'                                                     
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         MVC   P+1(12),=C'TAPE RECORDS'                                         
         EDIT  (P6,RECS),(12,P+13),0,COMMAS=YES                                 
         ZAP   RECS,=P'0'                                                       
         CLI   QOPT2,C'B'          FOR B RQST-NO TAPE                           
         BE    *+12                                                             
         CLI   QOPT1,C'Y'                                                       
         BE    CLSTP                                                            
         MVC   P+1(30),=CL30'DRAFT RUN - NO TAPE WRITTEN'                       
         B     BALP                                                             
CLSTP    CLOSE ACI3WK                                                           
BALP     BAS   RE,PRT                                                           
         GOTO1 SORTER,DMCB,=C'END'                                              
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   TOTAMT,=P'0'                                                     
         MVC   BUFKEY,SPACES                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
         TM    DMCB+8,X'80'        NOBODY WANTS AN EMPTY BUFFALO                
         BO    XIT                                                              
CLIKEY   CLI   BUFKEY,X'FF'                                                     
         BE    EOBUF                                                            
         MVC   P+1(15),BUFKEY                                                   
         EDIT  (P8,BUFC1),(12,P+20),2,COMMAS=YES,FLOAT=-                        
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         AP    TOTAMT,BUFC1                                                     
         GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     CLIKEY                                                           
EOBUF    BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         MVC   P+1(13),=C'REQUEST TOTAL'                                        
         EDIT  (P6,TOTAMT),(12,P+20),2,COMMAS=YES,FLOAT=-                       
         BAS   RE,PRT                                                           
         BAS   RE,PRT                                                           
         ZAP   TOTAMT,=P'0'                                                     
         CLI   QOPT1,C'Y'                                                       
         BNE   XIT                                                              
         CLI   QOPT2,C'B'          NO TAPE FOR BOTH REQUEST                     
         BE    XIT                                                              
         GOTO1 SORTER,DMCB,SORTCRD2,RECCRD2                                     
         OPEN  (ACI3WK,(INPUT))                                                 
GETWK    GET   ACI3WK,TIOA                                                      
         BAS   RE,PUTSORT                                                       
         B     GETWK                                                            
EOWK     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    EOJ                                                              
         PUT   ACI3TP,(5)                                                       
         B     EOWK                                                             
EOJ      CLOSE (ACI3TP,,ACI3WK)                                                 
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
PUTBUF   NTR1                                                                   
         MVC   BUFKEY,TBUDNO                                                    
         ZAP   BUFC1,TAMT                                                       
GOBUF    GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         CLI   BUFKEY,X'FF'                                                     
         BE    XIT                                                              
         MVI   BUFKEY,X'FF'                                                     
         B     GOBUF                                                            
         EJECT                                                                  
FNDBILL  NTR1                             FIND BILL EL & CHK DATES              
         L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   NO                                                               
         CLC   TRNSANAL,=C'99'                                                  
         BNE   NO                                                               
         MVC   SVDATE3,TRNSDATE                                                 
         MVC   SVTRDT,TRNSDATE                                                  
         GOTO1 DATCON,DMCB,(1,SVTRDT),(0,SVBILL)                                
         ZIC   RF,TRNSLEN                                                       
         LR    R5,R2                                                            
         AR    R5,RF                                                            
         CLI   0(R5),X'60'                                                      
         BNE   IT02                                                             
         USING TRSTATD,R5                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,SVDATE3)                             
         SPACE 1                                                                
IT02     LR    R9,R2                                                            
         MVI   ELCODE,X'61'                                                     
         BAS   RE,NEXTEL                                                        
         BE    DUDAT                                                            
         MVC   SVDUE,SPACES                                                     
         B     IT03                                                             
         USING TRDUED,R9                                                        
DUDAT    GOTO1 DATCON,DMCB,(2,TRDUDATE),(0,SVDUE)                               
         SPACE 1                                                                
IT03     CLC   SVDATE3,QSTRT3                                                   
         BL    NO                                                               
         CLC   SVDATE3,QEND3                                                    
         BH    NO                                                               
         CLI   FSW,C'F'                                                         
         BNE   YES                                                              
         GOTO1 DATCON,DMCB,(1,SVTRDT),(5,HEADS2+83)                             
         GOTO1 DATCON,DMCB,(0,SVDUE),(5,HEADS+83)                               
         MVI   FSW,C'N'                                                         
         B     YES                                                              
         EJECT                                                                  
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',TIOA                                         
         B     XIT                                                              
         SPACE 2                                                                
PRT      NTR1                                                                   
         LA    R1,28                                                            
         MVI   HEAD2+38,C'-'                                                    
         CLI   QOPT2,C'B'          IF REQUESTING OPEN & CLOSED                  
         BNE   PRT10                                                            
         CLI   ES,C'A'             AND END OF REQUEST                           
         BNE   PRT10                                                            
         MVC   HEAD1+38(22),=C'PRODUCTION JOB LISTING'                          
         SH    R1,=H'7'                                                         
         B     PRT20                                                            
PRT10    MVC   HEAD1+38(29),=C'CLOSED PRODUCTION JOB LISTING'                   
         CLI   OPNORCLS,C'C'                                                    
         BE    PRT20                                                            
         MVC   HEAD1+38(29),=C'OPEN PRODUCTION JOB LISTING  '                   
         CLI   OPNORCLS,C'O'                                                    
         BNE   *+12                                                             
         SH    R1,=H'2'                                                         
         B     PRT20                                                            
         MVC   HEAD1+38(29),=C'PRODUCTION BILLING INTERFACE '                   
PRT20    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD2+39(0),HEAD2+38                                             
         MVC   HEAD5(92),HEADS                                                  
         MVC   HEAD4(92),HEADS2                                                 
         MVC   HEAD4+8(47),FOR4    CLIENT/DESCRIPTION                           
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 2                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
COMPTBL  DC    X'F6',C'WE02'                                                    
         DC    X'F1',C'TB06'                                                    
         DC    X'C2',C'MC07'                                                    
         DC    X'F9',C'SJ99'                                                    
         DC    X'4A',C'YN08'                                                    
         DC    X'6B',C'HD99'       ADDED 11/92                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
TIOA     DS    CL66                                                             
CODE1REC DC    CL60' '                                                          
SVIOA    DS    CL66                                                             
         SPACE 2                                                                
ES       DC    CL1'F'                                                           
SVBILL   DS    CL6                                                              
HEADS    DC    CL92' '                                                          
HEADS2   DC    CL92' '                                                          
FSW      DC    CL1'F'                                                           
EDTW     DS    CL6                                                              
SVTRDT   DS    CL3                                                              
TOT      DC    PL6'0'                                                           
TOTAMT   DC    PL6'0'                                                           
MINOR    DC    PL6'0'                                                           
RECS     DC    PL6'0'                                                           
PREBUD   DC    CL15'*'                                                          
SVDUE    DS    CL6                                                              
SVJOB    DS    CL6                                                              
BUFREC   DS    0CL23                                                            
BUFKEY   DS    CL15                                                             
BUFC1    DS    PL8                                                              
ADBUFC   DS    A                                                                
SVCODE   DS    CL2                                                              
PREYR    DC    CL5' '                                                           
TOTYR    DC    PL6'0'                                                           
SVACC    DC    CL12' '                                                          
SVCLSTOT DC    PL6'0'                                                           
SVOPNCLS DS    C                                                                
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,2,A,62,5,A,29,15,A,16,6,A),FORMAT=CHX        
               '                                                                
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=66'                                    
SORTCRD2 DC    CL80'SORT FIELDS=(2,21,A),FORMAT=CH'                             
RECCRD2  DC    CL80'RECORD TYPE=F,LENGTH=60'                                    
         SPACE 2                                                                
ACI3TP   DCB   DDNAME=ACI3TP,DSORG=PS,RECFM=FB,MACRF=PM,LRECL=60,      X        
               BLKSIZE=32760                                                    
ACI3WK   DCB   DDNAME=ACI3WK,DSORG=PS,RECFM=FB,MACRF=(PM,GM),LRECL=60, X        
               BLKSIZE=32760,EODAD=EOWK                                         
         SPACE 2                                                                
         GETEL R9,DATADISP,ELCODE                                               
ELCODE   DS    CL1                                                              
         SPACE 1                                                                
QSTRT3   DS    CL3                                                              
QEND3    DS    CL3                                                              
RELO     DS    F                                                                
SORTER   DS    A                                                                
SVCO     DS    CL2                                                              
SVDATE3  DS    CL3                                                              
FOR4     DS    CL50                                                             
         LTORG                                                                  
         SPACE 2                                                                
         DCBD  DSORG=PS,DEVD=TA                                                 
* ACGENMODES                                                                    
* ACGENBOTH                                                                     
* ACREPWORKD                                                                    
* DDBUFFALOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
         BUFF  LINES=200,ROWS=1,COLUMNS=1,COMMENT=0,FLAVOR=PACKED,     X        
               KEYLIST=(15,A)                                                   
         EJECT                                                                  
TAPED    DSECT                                                                  
OPNORCLS DS    C                   OPEN OR CLOSED ACCOUNT IDENTIFIER            
TTYPE    DS    CL1                                                              
TESTNO   DS    CL12                                                             
TPMT     DS    CL1                                                              
TINV     DS    CL7                                                              
TAMT     DS    PL6                                                              
TBUDNO   DS    CL15                                                             
TBLANKS  DS    CL18                                                             
TSORT    DS    CL1                                                              
TYEAR    DS    CL4                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPI302 04/10/15'                                      
         END                                                                    
