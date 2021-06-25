*          DATA SET DDCULOAN   AT LEVEL 010 AS OF 05/01/02                      
*PHASE DDCULOAN,*                                                               
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE IJDFYZZZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'AMCALC - CALCULATE AMORTIZATION SCHEDULE'                       
AMCALC   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**AMCA,=V(REGSAVE)                                             
         SPACE 2                                                                
*        FORMAT OF CARD INPUT                                                   
*              01-09   AMOUNT IN CENTS                                          
*              11-17   INTEREST PER PERIOD (5 DEC)                              
*              21-25   NO. OF PERIODS                                           
*                                                                               
*                                                                               
         SPACE 2                                                                
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(51),=C'DONOVAN DATA CREDIT UNION - LOAN REPAYMENT X        
               SCHEDULE'                                                        
         MVC   MID1(14),=C'AMOUNT OF LOAN'                                      
         MVC   MID2(22),=C'INTEREST (ANNUAL RATE)'                              
         MVC   MID2+33(3),=C'PCT'                                               
         MVC   MID3(15),=C'NO. OF PAYMENTS'                                     
         MVC   SUB1(36),=C'     PYMT       BALANCE         PYMT'                
         MVC   SUB1+68(11),=C'CUMMULATIVE'                                      
         MVC   SUB2(79),=C'      NO.         DUE          AMOUNT      IX        
               NTEREST     PRINCIPAL     INTEREST '                             
         MVC   SUB3(79),=C'     ----       -------        ------      -X        
               -------     ---------   -----------'                             
AC2      DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'/*',CARD                                                      
         BE    EOJ                                                              
*                                                                               
         MVC   MID1+55(40),CARD+40                                              
         MVC   MID3+55(25),SPACES                                               
         CLI   CARD+30,C' '                                                     
         BE    *+16                                                             
         MVC   MID3+55(13),=C'FIRST PAYMENT'                                    
         MVC   MID3+69(8),CARD+30                                               
         XC    TBAL(16),TBAL                                                    
         XC    CINT,CINT                                                        
         PACK  DUB,CARD(9)         ORIGINAL AMOUNT                              
         CVB   R0,DUB                                                           
         ST    R0,AMT                                                           
         PACK  DUB,CARD+10(7)      INT 5 DEC                                    
         CVB   R0,DUB                                                           
         ST    R0,INT                                                           
         PACK  DUB,CARD+20(5)      NO. OF PERIODS                               
         CVB   R0,DUB                                                           
         ST    R0,PERS                                                          
         EDIT  (B4,AMT),(12,MID1+20),2,FLOAT=$,COMMAS=YES                       
         EDIT  (B4,INT),(8,MID2+24),5                                           
         EDIT  (B4,PERS),(5,MID3+27)                                            
*                                  DIVIDE INT BY 24 PERIODS                     
         L     R1,INT                                                           
         SR    R0,R0                                                            
         L     RF,=F'24'                                                        
         BAS   RE,DIV                                                           
         ST    R1,INT                                                           
         L     R1,PERS                                                          
         SRL   R1,1                                                             
         MVC   MID3+33(11),=C'(NN MONTHS)'                                      
         EDIT  (R1),(2,MID3+34)                                                 
*                                                                               
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         XC    PAYNO,PAYNO                                                      
*                                  CALC PAY AMOUNT                              
         L     R1,INT                                                           
         A     R1,RNDR                                                          
         LR    R2,R1                                                            
         L     R3,PERS                                                          
         L     RF,RNDR                                                          
         B     AC4B                                                             
AC4      DS    0H                                                               
         MR    R0,R2                                                            
         BAS   RE,DIV                                                           
AC4B     DS    0H                                                               
         BCT   R3,AC4                                                           
AC5      DS    0H                                                               
         S     R1,RNDR                                                          
         LR    RF,R1               (1+I)**N - 1                                 
         L     R1,AMT                                                           
         M     R0,INT                                                           
         STM   R0,R1,DUB           SAVE AMT X INT                               
         BAS   RE,DIV                                                           
         LR    R4,R1                                                            
         LM    R0,R1,DUB           AMT X INT                                    
         D     R0,RNDR             DIV WITHOUT ROUNDING                         
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)            FORCE 'ROUNDING UP'                          
         AR    R1,R4                                                            
         ST    R1,PAYAMT                                                        
*                                                                               
         MVC   BAL,AMT                                                          
AC6      DS    0H                                                               
         L     R1,BAL                                                           
         M     R0,INT                                                           
         L     RF,RNDR                                                          
         BAS   RE,DIV                                                           
         ST    R1,PINT                                                          
         L     R1,PAYAMT                                                        
         S     R1,PINT                                                          
         ST    R1,PPRIN                                                         
         L     R1,BAL                                                           
         S     R1,PPRIN                                                         
         BNM   AC7                                                              
         MVC   PPRIN,BAL                                                        
         L     R1,PPRIN            ADJUST LAST PAYMENT                          
         A     R1,PINT                                                          
         ST    R1,PAYAMT                                                        
AC7      DS    0H                                                               
         L     R1,CINT             CUMMULATIVE INT                              
         A     R1,PINT                                                          
         ST    R1,CINT                                                          
         L     R1,PAYNO                                                         
         LA    R1,1(R1)                                                         
         ST    R1,PAYNO                                                         
         EDIT  (B4,PAYNO),(4,P+5)                                               
*                                                                               
         LA    R2,BAL                                                           
         LA    R3,P+11                                                          
         LA    R4,5                                                             
AC8      DS    0H                                                               
         EDIT  (B4,0(R2)),(12,0(R3)),2,COMMAS=YES                               
*                                                                               
         LA    R2,4(R2)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,AC8                                                           
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*                                  ROLL UP TOTALS                               
         L     R1,PAYAMT                                                        
         A     R1,TPAY                                                          
         ST    R1,TPAY                                                          
         L     R1,PPRIN                                                         
         A     R1,TPRIN                                                         
         ST    R1,TPRIN                                                         
*                                                                               
         CLC   PAYNO,PERS                                                       
         BH    AC10                                                             
         L     R1,BAL                                                           
         S     R1,PPRIN                                                         
         ST    R1,BAL                                                           
         BP    AC6                                                              
*                                                                               
*                                                                               
AC10     DS    0H                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   TBAL,BAL                                                         
         MVC   TINT,CINT                                                        
         LA    R2,TBAL                                                          
         LA    R3,P+11                                                          
         LA    R4,4                                                             
AC12     DS    0H                                                               
         EDIT  (B4,0(R2)),(12,0(R3)),2,COMMAS=YES                               
         LA    R2,4(R2)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,AC12                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     AC2                                                              
         SPACE 3                                                                
DIV      DS    0H                                                               
         SLDA  R0,1                                                             
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
EOJ      DS    0H                                                               
         XBASE                                                                  
         SPACE 3                                                                
AMT      DC    F'0'                                                             
INT      DC    F'0'                                                             
PERS     DC    F'0'                                                             
PAYNO    DC    F'0'                                                             
BAL      DC    F'0'                                                             
PAYAMT   DC    F'0'                                                             
PINT     DC    F'0'                                                             
PPRIN    DC    F'0'                                                             
CINT     DC    F'0'                                                             
TBAL     DC    F'0'                                                             
TPAY     DC    F'0'                                                             
TINT     DC    F'0'                                                             
TPRIN    DC    F'0'                                                             
*                                                                               
RNDR     DC    F'10000000'                                                      
         SPACE 2                                                                
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DC    6F'0'                                                            
CARD     DS    CL80                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
       ++INCLUDE DDDPRINT                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DDCULOAN  05/01/02'                                      
         END                                                                    
