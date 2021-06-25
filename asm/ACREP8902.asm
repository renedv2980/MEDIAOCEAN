*          DATA SET ACREP8902  AT LEVEL 020 AS OF 05/01/02                      
*PHASE AC8902A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE ACCEDIT                                                                
         TITLE 'BUDGET COMPARISON REPORT'                                       
AC8902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC8902,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC8802D,RC                                                       
         ST    R5,RELO                                                          
         EJECT                                                                  
*              HANDLE REQUEST DETAILS                                           
         SPACE 3                                                                
         CLI   MODE,RUNFRST                                                     
         BE    BCEXT                                                            
         CLI   MODE,RUNLAST                                                     
         BE    BCEXT                                                            
         LA    R2,HEAD6+92                                                      
         MVC   HEAD6+85(37),SPACES                                              
         CLI   QOPT1,C' '                                                       
         BE    BC2                                                              
         MVC   HEAD6+85(6),=C'DEBITS'                                           
         CLI   QOPT1,C'D'                                                       
         BE    BC2                                                              
         MVC   HEAD6+85(7),=C'CREDITS'                                          
         LA    R2,1(R2)                                                         
         CLI   QOPT2,C'C'                                                       
         BE    BC2                                                              
         MVC   HEAD6+85(7),=C'BALANCE'                                          
         SPACE 2                                                                
BC2      CLI   QOPT2,C' '                                                       
         BE    BC6                                                              
         CLI   QOPT2,C'U'                                                       
         BE    BC3                                                              
         MVC   0(4,R2),=C'OVER'                                                 
         LA    R2,5(R2)                                                         
         B     BC4                                                              
         SPACE 2                                                                
BC3      MVC   0(5,R2),=C'UNDER'                                                
         LA    R2,6(R2)                                                         
         SPACE 2                                                                
BC4      MVC   0(3,R2),QOPT3                                                    
         MVC   4(7,R2),=C'PERCENT'                                              
         CLI   0(R2),C'0'                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         EJECT                                                                  
*              SWITCH TO APPROPRIATE ROUTINE                                    
         SPACE 3                                                                
BC6      CLI   MODE,REQFRST                                                     
         BE    BC8                                                              
         CLI   MODE,PROCACC                                                     
         BE    BC50                                                             
         CLI   MODE,PROCHIST       DO WE HAVE A HISTORY RECORD ?                
         BE    BC60                YES, GO HANDLE IT                            
         CLI   MODE,ACCLAST                                                     
         BE    BC20                                                             
         LA    R3,2                                                             
         CLI   MODE,LEVCLAST                                                    
         BE    BC10                                                             
         LA    R3,3                                                             
         CLI   MODE,LEVBLAST                                                    
         BE    BC10                                                             
         LA    R3,4                                                             
         CLI   MODE,LEVALAST                                                    
         BE    BC10                                                             
         LA    R3,5                                                             
         CLI   MODE,LEDGLAST                                                    
         BE    BC10                                                             
         LA    R3,6                                                             
         CLI   MODE,UNITLAST                                                    
         BE    BC10                                                             
         LA    R3,7                                                             
         CLI   MODE,REQLAST                                                     
         BE    BC10                                                             
         SPACE 2                                                                
BCEXT    XMOD1 1                                                                
         EJECT                                                                  
*              FIRST AND LAST ROUTINES                                          
         SPACE 3                                                                
BC8      MVI   FORCEHED,C'Y'       REQUEST FIRST                                
         MVC   PAGE,=H'1'                                                       
         GOTO1 PROLLER,DMCB,0,ACCUMS,9,4                                        
         B     BCEXT                                                            
         SPACE 2                                                                
BC10     L     R2,ADLDGHIR                                                      
         USING ACHEIRD,R2                                                       
         MVC   TOTAB(15),ACHRDESC                                               
         MVC   TOTAB+15(15),ACHRDESB                                            
         MVC   TOTAB+30(15),ACHRDESA                                            
         LA    R3,1(R3)                                                         
         BAS   RE,FORMAT                                                        
         CLC   P+70(40),SPACES                                                  
         BE    BCEXT                                                            
         SH    R3,=H'2'                                                         
         MH    R3,=H'15'                                                        
         LA    R3,TOTAB-15(R3)                                                  
         MVC   P+27(10),=C'TOTALS FOR'                                          
         MVC   P+38(15),0(R3)                                                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     BCEXT                                                            
         SPACE 2                                                                
         DC    CL15'ACCOUNT'                                                    
TOTAB    DC    CL45' '                                                          
         DC    CL15'LEDGER'                                                     
         DC    CL15'UNIT'                                                       
         DC    CL15'REQUEST'                                                    
         EJECT                                                                  
*              PROCESS BUDGETS IN BLOCK AND PRINT                               
         SPACE 3                                                                
BC20     MVC   WORK,SPACES         COMBINE A/C NO AND NAME                      
         GOTO1 =V(ACCEDIT),DMCB,ADACC,ADLDGHIR,WORK,RR=RB                       
         LA    R3,WORK-2                                                        
         CLC   WORK,SPACES                                                      
         BE    BC20B                                                            
         LA    R3,WORK+20                                                       
         SPACE 2                                                                
BC20A    CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         BCT   R3,BC20A                                                         
         SPACE 2                                                                
BC20B    L     R2,ADACCNAM                                                      
         USING ACNAMED,R2                                                       
         SR    R1,R1                                                            
         IC    R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),ACNMNAME                                                 
         GOTO1 CHOPPER,DMCB,(50,WORK),(25,P+1),(C'P',2)                         
         L     R2,=A(BUDGBLOC)                                                  
         A     R2,RELO                                                          
         SPACE 2                                                                
BC22     CLI   0(R2),0                                                          
         BE    BC40                                                             
         USING ACBUDGD,R2                                                       
         CLI   QOPT1,C' '          CHECK TYPE FILTER                            
         BE    BC24                                                             
         CLC   QOPT1(1),ACBDTYPE                                                
         BNE   BC36                                                             
         SPACE 2                                                                
BC24     GOTO1 DATCON,DMCB,(1,ACBDEND),(0,WORK)                                 
         CLC   QSTART,SPACES       IF START DATE IS SPECIFIED                   
         BE    BC26                                                             
         CLC   WORK(4),QSTART      CHECK BUDGET DOES NOT                        
         BL    BC36                END BEFORE START DATE                        
         SPACE 2                                                                
BC26     GOTO1 DATCON,DMCB,(1,ACBDSTRT),(0,WORK)                                
         CLC   QEND,SPACES         IF END DATE IS SPECIFIED                     
         BE    BC28                                                             
         CLC   WORK(4),QEND        CHECK BUDGET DOES NOT                        
         BH    BC36                START BEFORE END DATE                        
         SPACE 2                                                                
BC28     LA    R4,1                                                             
         SPACE 2                                                                
BC30     GOTO1 PROLLER,DMCB,3,ACCUMS,ACBDBUDG,1,1                               
         ZAP   DUB,ACBDBUDG                                                     
         GOTO1 PROLLER,DMCB,,,ACBDORIG,,2                                       
         SP    DUB,ACBDORIG                                                     
         GOTO1 PROLLER,DMCB,,,DUB+2,,3                                          
         LA    R3,1                                                             
         LTR   R4,R4                                                            
         BE    BC35                                                             
         BAS   RE,FORMAT                                                        
         CLI   QOPT2,C' '          OPTION TO FILTER OVER/UNDER NNN PC           
         BE    BC34                                                             
         PACK  DUB,QOPT3(3)                                                     
         CVB   R5,DUB                                                           
         MH    R5,=H'100'                                                       
         CLI   QOPT2,C'U'          OVER OPTION TEST                             
         BE    BC32                                                             
         C     R5,PERCENT                                                       
         BNH   BC34                                                             
         B     BC34A                                                            
         SPACE 2                                                                
BC32     C     R5,PERCENT          UNDER OPTION TEST                            
         BL    BC34A                                                            
         SPACE 2                                                                
BC34     SR    R4,R4               PASSED TESTS - GO BACK AND REPOST            
         B     BC30                                                             
         SPACE 2                                                                
BC34A    MVC   P(132),SPACES                                                    
         B     BC36                                                             
         EJECT                                                                  
*              NOW FORMAT LINE AND PRINT AT END OF ACCOUNT                      
         SPACE 3                                                                
BC35     GOTO1 PROLLER,DMCB,6,ACCUMS                                            
         LA    R3,1                                                             
         BAS   RE,FORMAT                                                        
         LR    R4,R2                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(15),36(R2)                                                  
         L     R5,ADCMPEL                                                       
         USING ACCOMPD,R5                                                       
         L     R6,ADACC                                                         
         CLC   1(2,R6),ACMPRECV    BILLING SOURCE FOR RECEIVABLES               
         BE    *+10                                                             
         MVC   WORK(51),37(R2)     (ACCOUNT LESS COMPANY & NAME)                
         GOTO1 =V(SQUASHER),DMCB,WORK,51,RR=RB                                  
         GOTO1 CHOPPER,DMCB,(51,WORK),(19,P+27),(C'P',2)                        
         USING ACBUDGD,R2                                                       
         GOTO1 DATCON,DMCB,(1,ACBDSTRT),(9,P+54)                                
         GOTO1 DATCON,DMCB,(1,ACBDEND),(9,P+62)                                 
         MVI   P+60,C'-'                                                        
         MVC   P+47(5),=C'DEBIT'                                                
         CLI   ACBDTYPE,C'C'                                                    
         BNE   *+10                                                             
         MVC   P+47(6),=C'CREDIT'                                               
         CLI   ACBDTYPE,C'B'                                                    
         BNE   *+10                                                             
         MVC   P+47(6),=C'BALNCE'                                               
         GOTO1 ACREPORT                                                         
         MVI   ACSWITCH,C'Y'                                                    
         SPACE 2                                                                
BC36     LA    R2,88(R2)                                                        
         B     BC22                                                             
         SPACE 2                                                                
BC40     MVC   P,SPACES                                                         
         CLI   ACSWITCH,C'N'                                                    
         BE    BCEXT                                                            
         MVI   ACSWITCH,C'N'                                                    
         GOTO1 ACREPORT                                                         
         LA    R3,1                                                             
         B     BC10                                                             
         SPACE 2                                                                
ACSWITCH DC    C'N'                                                             
         EJECT                                                                  
*              FORMAT A LINE OF TOTALS                                          
         SPACE 3                                                                
FORMAT   NTR                                                                    
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R2,DMCB                                                          
         XC    PERCENT,PERCENT                                                  
         CP    0(6,R2),=P'0'       WORK OUT PERCENT OF BUDGET                   
         BE    FORMAT2                                                          
         CP    6(6,R2),=P'0'                                                    
         BE    FORMAT2                                                          
         ZAP   DIVWRK,6(6,R2)                                                   
         MP    DIVWRK,=P'10000'                                                 
         DP    DIVWRK,0(6,R2)                                                   
         ZAP   DUB,DIVWRK(8)                                                    
         CVB   R5,DUB                                                           
         ST    R5,PERCENT                                                       
         EDIT  (P8,DIVWRK),(8,P+98),2                                           
         SPACE 2                                                                
FORMAT2  LA    R3,P+67                                                          
         LA    R4,3                                                             
         SPACE 2                                                                
FORMAT4  CP    0(6,R2),=P'0'                                                    
         BE    FORMAT6                                                          
         EDIT  (P6,(R2)),(12,DMCB),FLOAT=-                                      
         MVC   0(10,R3),DMCB                                                    
         ZAP   0(6,R2),=P'0'                                                    
         SPACE 2                                                                
FORMAT6  LA    R2,6(R2)                                                         
         LA    R3,10(R3)                                                        
         BCT   R4,FORMAT4                                                       
         XIT                                                                    
         EJECT                                                                  
*              ROUTINE TO FORMAT BUDGET BLOCK                                   
         SPACE 2                                                                
BC50     MVC   LASTWORK,SPACES                                                  
         L     R2,ADACC                                                         
         AH    R2,DATADISP                                                      
         L     R3,=A(BUDGBLOC)                                                  
         A     R3,RELO                                                          
         SR    R5,R5               (BUDGET COUNT)                               
         SPACE 2                                                                
BC52     MVI   0(R3),0                                                          
         CLI   0(R2),0                                                          
         BE    BC56                                                             
         CLI   0(R2),X'34'                                                      
         BNE   BC54                                                             
         LA    R5,1(R5)                                                         
         MVC   36(52,R3),SPACES                                                 
         USING ACBUDGD,R2                                                       
         MVC   36(15,R3),ACBDSBAC                                               
         CLC   36(15,R3),SPACES                                                 
         BE    BC53                                                             
         SPACE 2                                                                
BC52A    CLI   36(R3),C' '         SLIDE LEFT                                   
         BNE   BC53                                                             
         MVC   36(15,R3),37(R3)                                                 
         B     BC52A                                                            
         SPACE 2                                                                
BC53     MVC   0(36,R3),0(R2)                                                   
         ZAP   30(6,R3),=P'0'                                                   
         LA    R3,88(R3)                                                        
         SPACE 2                                                                
BC54     SR    R4,R4                                                            
         IC    R4,1(R2)                                                         
         AR    R2,R4                                                            
         B     BC52                                                             
         SPACE 2                                                                
BC56     L     R3,=A(BUDGBLOC)                                                  
         A     R3,RELO                                                          
         GOTO1 XSORT,DMCB,(R3),(R5),88,15,13                                    
         B     BCEXT                                                            
         EJECT                                                                  
*              ROUTINE TO POST TRANSACTIONS INTO BLOCK                          
         SPACE 3                                                                
BC60     L     R2,=A(BUDGBLOC)                                                  
         A     R2,RELO                                                          
         USING ACBUDGD,R2                                                       
         L     R3,ADSUBAC                                                       
         USING TRSUBHD,R3                                                       
         L     R4,ADTRANS                                                       
         USING TRHISTD,R4                                                       
         CLI   0(R4),X'45'                                                      
         BE    BC62                                                             
         CLI   0(R4),X'42'                                                      
         BNE   BCEXT                                                            
         MVC   LASTWORK,2(R4)                                                   
         B     BCEXT                                                            
         SPACE 2                                                                
BC62     CLI   0(R2),0                                                          
         BE    BCEXT                                                            
         CLC   LASTWORK,SPACES     CHECK WORK CODE MATCH                        
         BE    BC64                                                             
         CLC   ACBDWORK,SPACES                                                  
         BE    BC66                                                             
         CLC   ACBDWORK,LASTWORK                                                
         BE    BC66                                                             
         B     BC70                                                             
         SPACE 2                                                                
BC64     CLC   ACBDSBAC,SPACES     CHECK OPTIONAL SUB-ACCOUNT MATCH             
         BE    BC66                                                             
         LA    R1,14                                                            
         LA    R5,ACBDSBAC+14                                                   
         SPACE 2                                                                
BC65     CLI   0(R5),C' '               FIND LENGTH OF SUB-ACC                  
         BNE   BC65B                    FOR VARIABLE CLC LATER                  
         BCTR  R5,0                                                             
         BCT   R1,BC65                                                          
         SPACE 2                                                                
BC65B    EX    R1,*+8              CHECK FOR MATCH ON SPECIFIED LENGTH          
         B     *+10                                                             
         CLC   ACBDSBAC(0),TRSBACNT                                             
         BNE   BC70                                                             
         MVC   52(36,R2),SPACES                                                 
         CLC   ACBDSBAC,TRSBACNT                                                
         BNE   BC66                IF MATCH IS EXACT,                           
         SR    R1,R1               USE SB-AC NAME                               
         IC    R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         MVC   WORK,SPACES                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TRSBNAME                                                 
         MVC   52(36,R2),WORK                                                   
         SPACE 2                                                                
BC66     CLI   TRHSEL,X'45'        CHECK DATES FIT                              
         BNE   BC70                                                             
         CLC   TRHSYEAR(2),ACBDSTRT                                             
         BL    BC70                                                             
         CLC   TRHSYEAR(2),ACBDEND                                              
         BH    BC70                                                             
         CLI   ACBDTYPE,C'C'                                                    
         BNE   *+10                                                             
         AP    ACBDORIG,TRHSCR                                                  
         CLI   ACBDTYPE,C'D'                                                    
         BNE   *+10                                                             
         AP    ACBDORIG,TRHSDR                                                  
         CLI   ACBDTYPE,C'B'                                                    
         BNE   BC70                                                             
         AP    ACBDORIG,TRHSDR                                                  
         SP    ACBDORIG,TRHSCR                                                  
         SPACE 2                                                                
BC70     LA    R2,88(R2)                                                        
         B     BC62                                                             
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 3                                                                
AC8802D  DSECT                                                                  
RELO     DS    F                                                                
ACCUMS   DS    CL224               9*4*6 + 8                                    
LASTWORK DS    CL2                                                              
PERCENT  DS    F                                                                
DIVWRK   DS    CL14                                                             
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         SPACE 2                                                                
BUDGBLOC CSECT                                                                  
         DS    8800C                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACREP8902 05/01/02'                                      
         END                                                                    
