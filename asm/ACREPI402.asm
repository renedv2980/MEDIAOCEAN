*          DATA SET ACREPI402  AT LEVEL 014 AS OF 04/10/15                      
*PHASE ACI402A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'ACI402 * PROD JOB INTERFACE LIST *'                             
ACI402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACI402*,RR=R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         CLI   MODE,RUNFRST                                                     
         BNE   CKRQFST                                                          
         L     R7,=V(SORTER)                                                    
         ST    R8,RELO                                                          
         A     R7,RELO                                                          
         ST    R7,SORTER                                                        
         OPEN  (ACI4WK,(OUTPUT))                                                
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   XIT                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
RELO     DS    F                                                                
         EJECT                                                                  
CKRQFST  CLI   MODE,REQFRST                                                     
         BNE   CKLEVA                                                           
         CLI   QOPT2,C' '          IF BLANK                                     
         BNE   *+8                                                              
         MVI   QOPT2,C'C'          GIVE DEFAULT-CLOSED ACCOUNT LISTING          
         LA    R6,SORTREC                                                       
         USING SORTDD,R6                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   ES,C'F'                                                          
         MVI   FSW,C'F'                                                         
         LA    R1,COMPTBL                                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   NOTP                                                             
         CLI   QOPT2,C'B'        WHEN LISTING BOTH OPEN/CLOSED-NO TAPE          
         BNE   NOTP                                                             
         CLI   FIRST,C'Y'                                                       
         BNE   NOTP                                                             
         MVI   FIRST,C'N'                                                       
         MVI   TAPESW,C'Y'                                                      
CLICO    CLC   QCOMPANY,0(R1)                                                   
         BE    OPENT                                                            
         LA    R1,5(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CLICO                                                            
         ABEND 1                                                                
OPENT    MVC   SVCO,1(R1)                                                       
         MVC   SVCD,3(R1)                                                       
         LA    R7,TAPE                                                          
         USING IHADCB,R7                                                        
         MVC   DCBDDNAM+5(2),SVCO                                               
         MVC   DDPARM+5(2),SVCO                                                 
         MVC   DSPARM+13(2),SVCO                                                
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),(0,DSPARM)                              
         OPEN  ((R7),(OUTPUT))                                                  
NOTP     GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTRT3)                                
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         B     XIT                                                              
DDPARM   DC    CL8'ACI4TXX'                                                     
DSPARM   DC    CL20'ACCTAPE.AC0I4XX1'                                           
         EJECT                                                                  
CKLEVA   CLI   MODE,PROCLEVA                                                    
         BNE   CKPRACC                                                          
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
CKPRACC  CLI   MODE,PROCACC                                                     
         BNE   CKREQLST                                                         
         LA    R6,SORTREC                                                       
         PERF  SEECLS                                                           
         BNE   XIT                                                              
         MVC   TESTNO,SPACES                                                    
         MVC   TBUDNO,SPACES                                                    
         MVC   TESTNO(7),=C'MISSING'                                            
         MVC   TBUDNO(7),=C'MISSING'                                            
         MVI   ELCODE,X'27'                                                     
         L     R9,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   BWS                                                              
         USING ACABILLD,R9                                                      
         MVC   TYEAR,ACABACNO                                                   
         MVI   TSORT,C' '        FORCE CURRENT TO BE FIRST                      
         CLI   TYEAR,C' '                                                       
         BE    CURR                                                             
         MVI   TSORT,C'A'        FORCE ACCRUALS & PRIORS TO FOLLOW              
         CLI   TYEAR,C'P'                                                       
         BNE   CL39                                                             
         MVC   TYEAR,ACABACNO+1                                                 
         B     CL39                                                             
CURR     DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(20,WORK)                                   
         MVC   TYEAR,WORK                                                       
CL39     CLI   ACABLEN,X'39'                                                    
         BNE   BWS                                                              
         MVC   TESTNO,ACABESNO                                                  
         MVC   TBUDNO,ACABBUNO                                                  
BWS      BAS   RE,WRITSORT                                                      
         B     XIT                                                              
         EJECT                                                                  
SEECLS   NTR1                                                                   
         L     R7,ADACCSTA                                                      
         USING ACSTATD,R7                                                       
*                                                                               
         MVI   OPNORCLS,C'C'       CLOSED INDICATION TO SORTREC                 
         TM    ACSTSTAT,X'40'      IS ACCOUNT CLOSED                            
         BNZ   *+8                 YES                                          
         MVI   OPNORCLS,C'O'       INDICATE OPEN                                
         CLI   QOPT2,C'B'          ARE WE LISTING BOTH                          
         BE    SE20                YES                                          
         CLI   QOPT2,C'C'          JUST CLOSED                                  
         BNE   SE10                                                             
         CLI   OPNORCLS,C'C'                                                    
         BE    SE20                                                             
         B     NO                                                               
SE10     CLI   OPNORCLS,C'O'       IT MUST BE AN OPEN ACT LIST                  
         BNE   *-8                                                              
*                                                                               
SE20     L     R9,ADACC                                                         
         MVI   ELCODE,X'26'                                                     
         PERF  GETEL                                                            
         BNE   NO                                                               
         USING ACJOBD,R9                                                        
         CLC   ACJBCLOS,QSTRT3                                                  
         BL    NO                                                               
         CLC   ACJBCLOS,QEND3                                                   
         BH    NO                                                               
         DROP  R7                                                               
         L     R7,ADACCBAL                                                      
         USING ACBALD,R7                                                        
         ZAP   BILLING,ACBLCR                                                   
         DROP  R7                                                               
         ZAP   ESTIMATE,=P'0'                                                   
*                                                                               
         BAS   RE,LOOKUP                                                        
         B     YES                                                              
         SPACE 2                                                                
WRITSORT NTR1                                                                   
         CLI   FSW,C'F'                                                         
         BNE   NF                                                               
         MVI   FSW,C'N'                                                         
         MVC   REC1+2(2),SVCD                                                   
         MVC   REC1+4(4),QEND+2                                                 
         MVC   REC1+8(2),QEND                                                   
         MVC   R1SP,SPACES                                                      
         MVC   SORTBODY,REC1                                                    
         PERF  PUTSORT                                                          
NF       MVC   SORTBODY,SPACES                                                  
         MVI   TCODE,C'2'                                                       
         CLI   QOPT2,C'A'                                                       
         BNE   NODDS                                                            
         L     R1,ADACC                                                         
         MVC   TEST,3(R1)                                                       
         B     YESDDS                                                           
NODDS    MVC   TEST,TESTNO                                                      
YESDDS   MVC   TBUD,TBUDNO                                                      
         ZAP   SESTAMT,ESTIMATE                                                 
         MVO   SESTAMT,SESTAMT(5)                                               
         MVO   SESTAMT,SESTAMT(5)                                               
         ZAP   TESTAMT,SESTAMT                                                  
         ZAP   TBILLAMT,BILLING                                                 
         ZAP   ESTIMATE,=P'0'                                                   
         ZAP   BILLING,=P'0'                                                    
         MVC   TSPACES,SPACES                                                   
         PERF  PUTSORT                                                          
         B     XIT                                                              
         SPACE 3                                                                
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     XIT                                                              
         EJECT                                                                  
CKREQLST CLI   MODE,REQLAST                                                     
         BNE   CKRUNLST                                                         
         MVI   FSW,C'F'                                                         
RS1      GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    EOS1                                                             
         CLI   TCODE,C'1'                                                       
         BNE   *+12                                                             
         PERF  PUTWK                                                            
         B     RS1                                                              
         SPACE 1                                                                
         CLI   FSW,C'F'                                                         
         BNE   NTF1                                                             
         MVI   FSW,C'N'                                                         
         MVC   SAVBUD,TBUD                                                      
         MVC   SAVEST,TEST                                                      
         MVC   SAVYR,TYEAR                                                      
         MVC   SBUD,TBUD                                                        
         MVC   SYR,TYEAR                                                        
NTF1     CLC   SAVBUD,TBUD                                                      
         BE    *+8                                                              
         PERF  BTOT                                                             
         CLC   SAVYR,TYEAR                                                      
         BE    *+8                                                              
         PERF  YTOT                                                             
         CLI   ES,C'E'                                                          
         BE    TTOT                                                             
         PERF  PUTWK                                                            
         MVC   P+3(4),SYR                                                       
         MVC   P+12(15),SBUD                                                    
         MVC   P+36(12),TEST                                                    
         MVC   SYR,SPACES                                                       
         MVC   SBUD,SPACES                                                      
         ZAP   SESTAMT,TESTAMT                                                  
         MP    SESTAMT,=P'100'                                                  
         ZAP   SBILLAMT,TBILLAMT                                                
         LA    R7,SESTAMT                                                       
         LA    R8,BE                                                            
         PERF  APE                                                              
*                                                                               
         CLC   OPNORCLS,SVDCORO    IF LISTING BOTH NEED PAGE BREAK              
         BE    NTF2                                                             
         MVI   FORCEHED,C'Y'                                                    
         ZAP   CLSYE,YE            SAVE FOR REQUEST TOTALS                      
         ZAP   CLSYB,YB                                                         
         ZAP   CLSTE,TE                                                         
         ZAP   CLSTB,TB                                                         
         ZAP   YE,=P'0'        CLEAR PRESENT EST. AND ACTUAL BILLING            
         ZAP   YB,=P'0'        NEW YEAR END TOTS FOR OPEN ACCOUNTS              
NTF2     PERF  PRT                                                              
         MVC   SVDCORO,OPNORCLS                                                 
         B     RS1                                                              
         SPACE 1                                                                
EOS1     MVI   ES,C'E'                                                          
         LA    R6,NINES                                                         
         B     NTF1                                                             
         EJECT                                                                  
BTOT     NTR1                                                                   
         PERF  PRT                                                              
         LA    R7,BE                                                            
         LA    R8,YE                                                            
         MVC   P+1(17),=C'TOTALS FOR BUDGET'                                    
         MVC   P+19(15),SAVBUD                                                  
         PERF  APE                                                              
         PERF  PRT,T=2                                                          
         MVC   SAVBUD,TBUD                                                      
         MVC   SBUD,TBUD                                                        
         MVC   SYR,TYEAR                                                        
         B     XIT                                                              
YTOT     NTR1                                                                   
         LA    R7,YE                                                            
         LA    R8,TE                                                            
         MVC   P+1(15),=C'TOTALS FOR     '                                      
         MVC   P+12(4),SAVYR                                                    
         PERF  APE                                                              
         PERF  PRT,T=2                                                          
         MVC   SAVYR,TYEAR                                                      
         MVC   SYR,TYEAR                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
TTOT     MVC   P+1(18),=C'TOTALS FOR REQUEST'                                   
         CLI   QOPT2,C'B'          IF OPEN AND CLOSED REQUEST                   
         BNE   TTOT10                                                           
         PERF  PRT                                                              
         MVC   P+3(15),=C'CLOSED ACCOUNTS'                                      
         LA    R7,CLSYE                                                         
         LA    R8,CLSYB                                                         
         PERF  APE                                                              
         PERF  PRT                                                              
*                                                                               
         MVC   P+3(13),=C'OPEN ACCOUNTS'                                        
TTOT10   LA    R7,TE                                                            
         LA    R8,DD                                                            
         PERF  APE                                                              
         PERF  PRT,T=2                                                          
         B     XIT                                                              
         EJECT                                                                  
APE      NTR1                                                                   
         EDIT  (P6,0(R7)),(13,P+54),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P6,6(R7)),(13,P+69),2,COMMAS=YES,FLOAT=-                        
         AP    0(6,R8),0(6,R7)                                                  
         AP    6(6,R8),6(6,R7)                                                  
         ZAP   SAVE12,0(6,R7)                                                   
         SP    6(6,R7),0(6,R7)                                                  
         BP    ED2                                                              
         EDIT  (P6,6(R7)),(13,P+84),2,COMMAS=YES,FLOAT=-                        
         B     ED3                                                              
ED2      EDIT  (P6,6(R7)),(13,P+84),2,COMMAS=YES,FLOAT=+                        
ED3      ZAP   WORK(12),6(6,R7)                                                 
         MP    WORK(12),=P'1000'                                                
         CP    0(6,R7),=P'0'                                                    
         BNE   DPOK                                                             
         ZAP   SAVE12(6),=P'0'                                                  
         B     NODP                                                             
DPOK     DP    WORK(12),SAVE12+6(6)                                             
         ZAP   SAVE12(6),WORK(6)                                                
NODP     EDIT  (P6,SAVE12),(7,P+99),1,FLOAT=-                                   
         ZAP   0(6,R7),=P'0'                                                    
         ZAP   6(6,R7),=P'0'                                                    
         B     XIT                                                              
         EJECT                                                                  
PRT      NTR1                                                                   
         LA    R1,28                                                            
         MVI   HEAD2+38,C'-'                                                    
         CLI   QOPT2,C'B'          IF REQUESTING OPEN & CLOSED                  
         BNE   PRT10                                                            
         CLI   ES,C'E'             AND END OF REQUEST                           
         BNE   PRT10                                                            
         MVC   HEAD1+38(22),=C'PRODUCTION JOB LISTING'                          
         SH    R1,=H'7'                                                         
         B     PRT20                                                            
PRT10    MVC   HEAD1+38(29),=C'CLOSED PRODUCTION JOB LISTING'                   
         CLI   OPNORCLS,C'C'                                                    
         BE    *+14                                                             
         MVC   HEAD1+38(29),=C'OPEN PRODUCTION JOB LISTING  '                   
         SH    R1,=H'2'                                                         
PRT20    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD2+39(0),HEAD2+38                                             
         MVC   HEAD4(47),FOR4                                                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
CKRUNLST CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         CLOSE ACI4WK                                                           
         CLI   TAPESW,C'Y'                                                      
         BNE   XIT                                                              
         CLI   QOPT2,C'B'   NO TAPE WHEN LISTING BOTH OPENED & CLOSED           
         BNE   XIT                                                              
         OPEN  (ACI4WK,(INPUT),TAPE,(OUTPUT))                                   
         GOTO1 SORTER,DMCB,SORTCD2,RECCD2                                       
GW1      GET   ACI4WK                                                           
         MVC   SORTREC,6(R1)                                                    
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     GW1                                                              
         SPACE 1                                                                
EOFWK    GOTO1 SORTER,DMCB,=C'GET'                                              
         DROP  R6                                                               
         USING SORTBODY,R6                                                      
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    CLSIT                                                            
         AP    RECS,=P'1'                                                       
         PUT   TAPE,SORTBODY                                                    
         B     EOFWK                                                            
         SPACE 1                                                                
CLSIT    CLOSE (TAPE,,ACI4WK)                                                   
         CLI   TAPESW,C'Y'                                                      
         BNE   XIT                                                              
         MVC   P+1(20),=C'TAPE RECORDS WRITTEN'                                 
         EDIT  (P6,RECS),(12,P+21),0,COMMAS=YES                                 
         MVI   FORCEHED,C'Y'                                                    
         PERF  PRT                                                              
         B     XIT                                                              
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
         USING JBCOLD,R3                                                        
         ZAP   ESTIMATE,JBCOLVAL                                                
         B     XIT                                                              
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE'                                                            
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
         SPACE 1                                                                
         ANSR                                                                   
         SPACE 1                                                                
         GETEL R9,DATADISP,ELCODE                                               
         SPACE 1                                                                
PUTWK    NTR1                                                                   
         PUT   ACI4WK,(R6)                                                      
         B     XIT                                                              
         EJECT                                                                  
TAPE     DCB   DSORG=PS,DDNAME=ACI4TXX,RECFM=FB,MACRF=PM,              X        
               LRECL=60,BLKSIZE=32760                                           
ACI4WK   DCB   DDNAME=ACI4WK,DSORG=PS,RECFM=FB,MACRF=(GL,PM),          X        
               LRECL=66,BLKSIZE=6600,EODAD=EOFWK                                
SORTCARD DC    CL80'SORT FIELDS=(1,6,CH,A,20,15,CH,A,8,12,CH,A)'                
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=66'                                    
SORTCD2  DC    CL80'SORT FIELDS=(1,13,CH,A)'                                    
RECCD2   DC    CL80'RECORD TYPE=F,LENGTH=60'                                    
ELCODE   DS    X                                                                
BILLING  DC    PL6'0'                                                           
ESTIMATE DC    PL6'0'                                                           
ES       DC    C'F'                                                             
FSW      DC    C'F'                                                             
COMPTBL  DC    X'F6',C'WE02'                                                    
         DC    X'F1',C'TB06'                                                    
         DC    X'C2',C'MC07'                                                    
         DC    X'F9',C'SJ99'                                                    
         DC    X'4A',C'YN08'                                                    
         DC    X'FF'                                                            
SVCO     DC    CL2' '                                                           
SVCD     DC    CL2' '                                                           
FIRST    DC    C'Y'                                                             
SORTER   DS    A                                                                
SORTREC  DS    CL66                                                             
TESTNO   DS    CL12                                                             
TBUDNO   DS    CL15                                                             
SBUD     DS    CL15                                                             
SYR      DS    CL4                                                              
SVDCORO  DS    C                   PG BRK INDICATOR FOR BOTH OPTION             
REC1     DC    C'12  '                                                          
         DC    CL6' '                                                           
         DC    CL23'AGENCY-CLOSED-ESTIMATES'                                    
R1SP     DC    CL27' '                                                          
**** KEEP TOGETHER THE DC'S BETWEEN THE STARS *************************         
SESTAMT  DC    PL6'0'                                                           
SBILLAMT DC    PL6'0'                                                           
BE       DC    PL6'0'                                                           
BB       DC    PL6'0'                                                           
YE       DC    PL6'0'                                                           
YB       DC    PL6'0'                                                           
TE       DC    PL6'0'                                                           
TB       DC    PL6'0'                                                           
DD       DC    PL6'0'                                                           
         DC    PL6'0'                                                           
***********************************************************************         
SAVEST   DS    CL12                                                             
SAVBUD   DS    CL15                                                             
SAVYR    DS    CL4                                                              
TAPESW   DC    C'N'                                                             
RECS     DC    PL6'0'                                                           
QSTRT3   DS    CL3                                                              
QEND3    DS    CL3                                                              
NINES    DC    60C'9'                                                           
SAVE12   DC    PL12'0'                                                          
FOR4     DC    CL50' '                                                          
PK5      DC    PL1'5'                                                           
*                                                                               
CLSYE    DC    PL6'0'              SAVES FOR REQUEST TOTALS                     
CLSYB    DC    PL6'0'                                                           
CLSTE    DC    PL6'0'                                                           
CLSTB    DC    PL6'0'                                                           
         LTORG                                                                  
         EJECT                                                                  
SORTDD   DSECT                                                                  
OPNORCLS DS    C                   C=CLOSED O=OPEN                              
TSORT    DS    AL1                                                              
TYEAR    DS    CL4                                                              
SORTBODY DS    0CL60                                                            
TCODE    DS    CL1                                                              
TEST     DS    CL12                                                             
TBUD     DS    CL15                                                             
TESTAMT  DS    PL4                                                              
TBILLAMT DS    PL6                                                              
TSPACES  DS    CL22                                                             
         PRINT OFF                                                              
         DCBD  DSORG,DEVD=TA                                                    
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREPI402 04/10/15'                                      
         END                                                                    
