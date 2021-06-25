*          DATA SET APGHFIYN2  AT LEVEL 005 AS OF 05/01/02                      
*                                                                               
*PHASE ACHFIYN2,+0                                                              
         TITLE 'APG HOOK FOR FINANCIAL COLUMN'                                  
ACHFIYN2 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,RR=R4                                                 
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         L     R5,HOOKAREC         ADDR OF SORT REC                             
         CLI   HOOK1ST,C'Y'        1ST TIME IN INITIALIZE                       
         BNE   HOOKIT              NO - CONTINUE                                
         SPACE 1                                                                
         MVI   HOOK1ST,C'N'        SET TO NO                                    
         MVC   BUDGETN,=H'5'       DEFAULT                                      
         CLI   QAPPL+1,C' '                                                     
         BE    *+10                                                             
         MVC   BUDGETN+1(1),QAPPL+1                                             
         SPACE 1                                                                
********************************************************                        
*        SET UP TABLE OF MONTHS FROM REQUEST           *                        
********************************************************                        
         MVC   WORK(6),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,HKSTART)                                 
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,HKEND)                                   
         LA    R7,DATABL                                                        
         LA    R2,0                                                             
         MVC   STRDATE,QSTART                                                   
ACYN01   EQU   *                                                                
         MVC   STRDATE+4(2),=C'01'                                              
         GOTO1 DATCON,DMCB,(0,STRDATE),(1,HKDATE)                               
         GOTO1 ADDAY,DMCB,STRDATE,STRDATE,32                                    
         MVC   0(2,R7),HKDATE                                                   
         LA    R2,1(R2)                                                         
         STC   R2,NMNTHS                                                        
         LA    R7,2(R7)                                                         
         CLI   NMNTHS,12                                                        
         BE    ACYN08                                                           
         CLC   HKDATE(2),HKEND                                                  
         BE    ACYN08                                                           
         B     ACYN01                                                           
         SPACE 1                                                                
ACYN08   L     RE,AMNKEY                                                        
         MVC   SVAPGKEY,0(RE)                                                   
         LA    R3,MYIO                                                          
         USING ACKEYD,R3                                                        
         SPACE 1                                                                
         MVC   ACBTKACC(32),SPACES                                              
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,RCCOMPFL                                                
         MVC   ACBTKACC+1(3),=C'SR1'                                            
         MVC   ACBTKBNO,BUDGETN                                                 
         BAS   RE,MYREAD                                                        
         SPACE 1                                                                
         LA    R0,12                                                            
         LA    R1,MNTHTBL                                                       
ACYN10   EQU   *                                                                
         ZAP   0(8,R1),=PL8'0'          CLEAR OUT                               
         LA    R1,8(R1)                 BUMP TO NEXT MONTH                      
         BCT   R0,ACYN10                                                        
         SPACE 1                                                                
         USING ACBAD,R4                                                         
         LA    R4,ACRECORD                                                      
         LA    R1,MNTHTBL                                                       
         LA    R7,DATABL                                                        
ACYN14   EQU   *                                                                
         CLI   0(R4),X'1D'                                                      
         BE    ACYN20                                                           
         CLI   0(R4),0                                                          
         BE    ACYN30                   GET OUT                                 
ACYN18   EQU   *                                                                
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ACYN14                                                           
ACYN20   EQU   *                                                                
         CLC   0(2,R7),ACBAMNTH                                                 
         BNE   ACYN18                                                           
         ZAP   ANS,=PL16'0'                                                     
         ZAP   ANS+8(8),ACBABUDG                                                
         DP    ANS(16),=PL8'12'                                                 
         ZAP   0(8,R1),QUOTIENT                                                 
         LA    R1,8(R1)                 BUMP TO NEXT MONTH                      
         LA    R7,2(R7)                                                         
         B     ACYN18                                                           
         SPACE 1                                                                
ACYN30   LA    R3,MYIO                                                          
         MVC   0(49,R3),SVAPGKEY                                                
         BAS   RE,MYREAD                                                        
         EJECT                                                                  
         USING R1SORTD,R3                                                       
HOOKIT   EQU   *                                                                
         ZAP   TOTANS,=PL8'0'                                                   
         LA    R3,HKAREA                                                        
         LR    RE,R5               R5 HAS ORIG SORTREC LOCATION                 
         LR    RF,R3               R3 HAS MY SORT WORK AREA                     
         LA    R1,S1LEN            LENGTH  OF SORT REC                          
         MOVE  ((RF),(R1)),(RE)                                                 
         SPACE 1                                                                
         LA    R4,R1MNTHBK(R3)                                                  
         LA    R1,MNTHTBL                                                       
         ZIC   R0,NMNTHS                                                        
         ZAP   ANS,=PL16'0'                                                     
HOOKIT10 EQU   *                                                                
         ZAP   ANS+8(8),0(8,R4)                                                 
         MP    ANS,0(8,R1)                                                      
         AP    TOTANS,ANS+8(8)                                                  
         LA    R4,8(R4)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,HOOKIT10                                                      
         SPACE 1                                                                
         SH    R1,=H'08'                                                        
         ZAP   ANS,=PL16'0'                                                     
         ZAP   ANS+8(8),S1MTH8                                                  
         MP    ANS,0(8,R1)                                                      
         SRP   ANS(16),58,5                                                     
         ZAP   S1MTH8,ANS+8(8)                                                  
         SPACE 1                                                                
         ZAP   ANS,=PL16'0'                                                     
         ZAP   ANS(16),TOTANS                                                   
         SRP   ANS(16),58,5                                                     
         ZAP   S1MTH21,ANS+8(8)                                                 
         EJECT                                                                  
***************************************************                             
*       PUT SORTREC BACK IN APGREP LOCATION       *                             
***************************************************                             
         SPACE 1                                                                
HKRESTOR LR    RF,R5               R5 HAS ORIG SORTREC LOCATION                 
         LR    RE,R3               R3 HAS MY SORT WORK AREA                     
         LA    R1,S1LEN            LENGTH  OF SORT REC                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     XIT                                                              
         SPACE 1                                                                
         B     XITA                                                             
XIT      SR    R0,R0               THIS XIT SETS CC TO YES                      
XITA     XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
MYREAD   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'ACCOUNT',(R3),(R3)                  
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         XIT                                                                    
         DROP  R4                                                               
         EJECT                                                                  
DATABL   DS    12CL2                                                            
         SPACE 1                                                                
HOOK1ST  DC    C'Y'                FIRST TIME INTO HOOK ROUTINE INIT            
NMNTHS   DS    CL1                 NUMBER OF MONTHS USED                        
STRDATE  DS    CL6                                                              
HKSTART  DS    CL3                                                              
HKEND    DS    CL3                                                              
HKDATE   DS    CL3                                                              
HKAREA   DS    CL(S1LEN)           MY SORT RECORD WORK AREA                     
SVAPGKEY DS    CL49                                                             
         SPACE 1                                                                
QUOTIENT DS    PL8                                                              
REMANDER DS    PL8                                                              
         ORG   QUOTIENT                                                         
ANS      DS    PL16                                                             
TOTANS   DS    PL8                                                              
BUDGETN  DS    H                                                                
MNTHTBL  DS    12PL8               12 MONTHS                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYDUB    DS    PL16                                                             
MYIO     DS    CL1000                                                           
*                                                                               
         EJECT                                                                  
R1SORTD  DSECT COVERS SORT RECORD PASSED FROM ACAPGUTILS                        
S1REPORT DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1OFFICE DS    CL1                 ROW 1 BRANCH OR PROFIT CENTER (1C)           
         DS    CL13                                                             
         DS    XL2                 REPORT NUMBER/NUMBER OF COPIES               
S1ACCT   DS    CL11                ROW 2 CLIENT OFFICE (1C)                     
         DS    CL3                                                              
REPORT   EQU   *-R1SORTD           DISP TO REPORT NUMBER                        
         DS    XL1                 REPORT NUMBER                                
         DS    XL1                 NUMBER COPIES                                
         DS    XL2                 2 BLANKS                                     
         DS    CL36                OFFICE CODE NAME                             
         DS    CL36                ROW 2 NAME                                   
REP1BUK  EQU   *-R1SORTD                                                        
S1MTH1   DS    PL8                 A/R BALANCCE                                 
S1MTH2   DS    PL8                 PAST DUE                                     
S1MTH3   DS    PL8                 PERCENT DIFFERENCE                           
S1MTH4   DS    PL8                 AGE 1 - 10                                   
S1MTH5   DS    PL8                 AGE 11 - 30                                  
S1MTH6   DS    PL8                 AGE 31 - 60                                  
S1MTH7   DS    PL8                 OVER 60                                      
R1MNTHBK EQU   *-R1SORTD                                                        
S1MTH8   DS    PL8                 MONTH 1                                      
S1MTH9   DS    PL8                 MONTH 2                                      
S1MTH10  DS    PL8                 MONTH 3                                      
S1MTH11  DS    PL8                 MONTH 4                                      
S1MTH12  DS    PL8                 MONTH 5                                      
S1MTH13  DS    PL8                 MONTH 6                                      
S1MTH14  DS    PL8                 MONTH 7                                      
S1MTH15  DS    PL8                 MONTH 8                                      
S1MTH16  DS    PL8                 MONTH 9                                      
S1MTH17  DS    PL8                 MONTH 10                                     
S1MTH18  DS    PL8                 MONTH 11                                     
S1MTH19  DS    PL8                 MONTH 12                                     
S1MTH20  DS    PL8                 FINANCIAL COST - LATEST                      
S1MTH21  DS    PL8                 FINANCIAL COST - YTD                         
BUKCNT1  EQU   (*-S1MTH1)/8        NUMBER OF BUCKETS (COLUMNS)                  
S1LEN    EQU   *-R1SORTD           LENGTH OF SORT RECORD                        
         EJECT                                                                  
*        INCLUDED HERE                                                          
*        ACREPWORKD                                                             
*        ACAPGWORKD                                                             
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACAPGWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005APGHFIYN2 05/01/02'                                      
         END                                                                    
