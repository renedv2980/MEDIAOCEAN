*          DATA SET REREPCMSN4 AT LEVEL 088 AS OF 05/01/02                      
*PHASE BLRCMSNB BLRCMSN4                                                        
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
         TITLE 'MMR COMMISSION REPORT'                                          
BLRCMSN4 CSECT                                                                  
         ENTRY UTL                                                              
         PRINT NOGEN                                                            
         NBASE 0,BLRCMSN4,=V(REGSAVE),RA                                        
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         XC    OFFNUM,OFFNUM                                                    
         XC    GRPNUM,GRPNUM                                                    
         XC    MSGRATE(200),MSGRATE                                             
         XC    MSGRATE+200(100),MSGRATE+200                                     
         EJECT                                                                  
READ     GOTO1 =V(CARDS),P1,CARD,=C'RE00'                                       
         SPACE 1                                                                
         CLC   =C'/*',CARD                                                      
         BE    BC60                                                             
         CLC   =C'RATE',CARD       STATION                                      
         BE    BC10                                                             
         CLC   =C'DEFO',CARD       OFFICE                                       
         BE    BC12                                                             
         CLC   =C'DEFG',CARD       GROUP                                        
         BE    BC14                                                             
         CLC   =C'DATA',CARD                                                    
         BE    BC50                                                             
         CLC   =C'DATE',CARD                                                    
         BE    BC16                                                             
         CLC   =C'RQTR',CARD       REQUESTOR                                    
         BE    BC18                                                             
*                                                                               
         MVC   P(L'ERR1),ERR1                                                   
         GOTO1 =V(PRINTER)                                                      
         B     EOJ                                                              
         EJECT                                                                  
BC10     L     R5,STALIST+2                                                     
         LH    R7,STALIST                                                       
*                                                                               
         LA    R4,3                                                             
         MVC   1(5,R5),CARD+9                                                   
         CLC   CARD+9(5),=C'DUMMY' SAVE DUMMY RATE FOR PRINTING                 
         BNE   BC10F                                                            
         MVC   DUMMYRT(20),CARD+15                                              
*                                                                               
BC10F    LA    R8,CARD+15                                                       
*                                  POINT TO NEXT AVAILABLE ENTRY                
         LA    R6,6(R5)                                                         
         SPACE 1                                                                
BC11     PACK  DUB,0(6,R8)                                                      
         CVB   R0,DUB                                                           
         ST    R0,0(R6)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,7(R8)                                                         
         BCT   R4,BC11                                                          
*                                                                               
         LA    R5,0(R5,R7)         STORE NEW ADDRESS                            
         ST    R5,STALIST+2                                                     
         B     READ                                                             
         SPACE 2                                                                
BC12     LA    R5,OFFLIST                                                       
         LH    R3,OFFNUM                                                        
         AH    R3,=H'1'                                                         
         STH   R3,OFFNUM                                                        
         SPACE 1                                                                
         CH    R3,=H'50'           TOO MANY OFFICES - 49 ALLOWED                
         BL    BC13                                                             
         SPACE 1                                                                
* TOO MANY OFFICES - BLOW UP                                                    
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'TOO MANY OFFICES-MAX 49'                                
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
         SPACE 2                                                                
BC13     CLI   0(R5),C' '                                                       
         BE    *+12                                                             
         LA    R5,22(R5)                                                        
         B     *-12                                                             
*                                                                               
         MVC   0(2,R5),CARD+9                                                   
         MVC   2(20,R5),CARD+15                                                 
         LA    R5,22(R5)                                                        
         MVC   0(22,R5),=CL22'  TOTALS'                                         
         B     READ                                                             
         SPACE 2                                                                
BC14     LH    R3,GRPNUM           COUNT NUMBER OF GROUPS (MAX 40)              
         AH    R3,=H'1'                                                         
         STH   R3,GRPNUM                                                        
         SPACE 1                                                                
         CH    R3,=H'41'                                                        
         BL    BC15                                                             
* TOO MANY GROUPS - BLOW UP                                                     
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(22),=C'TOO MANY GROUPS-MAX 40'                                 
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
         SPACE 2                                                                
BC15     LA    R5,GRPLIST                                                       
         CLI   0(R5),C' '                                                       
         BE    *+12                                                             
         LA    R5,12(R5)                                                        
         B     *-12                                                             
*                                                                               
         MVC   0(2,R5),CARD+9                                                   
         MVC   2(10,R5),CARD+15                                                 
         B     READ                                                             
         SPACE 2                                                                
BC16     MVC   PDATE,CARD+9                                                     
*                                  DO REST OF HEADLINES                         
         MVC   TITLE+10(17),=C'COMMISSION REPORT'                               
         MVC   MID1(18),=C'MAJOR MARKET RADIO'                                  
         MVC   MID1+42(16),=C'FOR THE MONTH OF'                                 
         MVC   MID1+59(6),PDATE                                                 
         MVC   SUB1+32(LSB1),SB1                                                
         MVC   SUB2(LSB2),SB2                                                   
         MVC   SUB3(LSB3),SB3                                                   
         B     READ                                                             
         SPACE 2                                                                
BC18     MVC   PRQTR,CARD+9                                                     
         MVC   MID1+94(12),PRQTR                                                
         B     READ                                                             
         SPACE 2                                                                
BC20     CLI   FRSTIME,1                                                        
         BE    BC22                                                             
         BAS   RE,GRUPHD                                                        
         MVC   SAVECODE,CARD+5                                                  
         LA    RE,SBGTOTS                                                       
         LH    RF,ALINS                                                         
         MH    RF,=H'24'                                                        
         XCEF                                                                   
         LA    RE,GRPTOTS                                                       
         LH    RF,ALINS                                                         
         MH    RF,=H'24'                                                        
         XCEF                                                                   
         XC    STATOTS(24),STATOTS                                              
         MVI   FRSTIME,1                                                        
         B     BC40                                                             
         SPACE 1                                                                
BC22     CLC   SAVECODE,CARD+5     STATION TEST                                 
         BE    BC40                                                             
BC23     MVC   POFF(6),=C'TOTALS'                                               
         MVI   P+10,C'*'                                                        
         LA    5,STATOTS                                                        
         BAS   RE,EDIT1                                                         
         GOTO1 =V(PRINTER)                                                      
         MVC   SAVECODE+2(5),CARD+7                                             
         XC    STATOTS(24),STATOTS                                              
         MVI   STABYTE,1                                                        
         GOTO1 =V(PRINTER)                                                      
         SPACE 1                                                                
         CP    RECNT,=P'1'                                                      
         BE    BC62                ONE RECORD  GET OUT                          
         CLC   SAVECODE(2),CARD+5  SUBGROUP TEST                                
         BNE   BC24                                                             
         B     BC40                                                             
         SPACE 2                                                                
BC24     LA    R3,OFFLIST                                                       
         LH    R4,ALINS                                                         
         LA    R5,SBGTOTS                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R1,SAVECODE                                                      
         BAS   RE,GETGRPNM                                                      
         MVC   PSTA(10),0(R1)                                                   
BC26     BAS   RE,EDIT1                                                         
         MVC   POFF,2(R3)                                                       
         CLC   POFF(5),=C'TOTAL'                                                
         BNE   *+10                                                             
         MVC   P+09(2),=C'***'                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R3,22(R3)                                                        
         LA    R5,24(R5)                                                        
         BCT   R4,BC26                                                          
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         MVC   SAVECODE+1(1),CARD+6                                             
         LA    RE,SBGTOTS                                                       
         LH    RF,ALINS                                                         
         MH    RF,=H'24'                                                        
         XCEF                                                                   
         SPACE 1                                                                
         CLC   SAVECODE(1),CARD+5  SKIP A LINE                                  
         BNE   BC28                                                             
         MVI   LINE,99                                                          
         BAS   RE,GRUPHD                                                        
         B     BC40                                                             
         SPACE 2                                                                
BC28     LA    R3,OFFLIST                                                       
         LH    R4,ALINS                                                         
         LA    R5,GRPTOTS                                                       
*                                                                               
         MVC   HALF(1),SAVECODE                                                 
         LA    R1,HALF                                                          
         MVI   HALF+1,C' '                                                      
         BAS   RE,GETGRPNM                                                      
         MVC   PSTA(10),0(R1)                                                   
BC30     BAS   RE,EDIT1                                                         
         MVC   POFF,2(R3)                                                       
         CLC   POFF(5),=C'TOTAL'                                                
         BNE   *+10                                                             
         MVC   P+08(3),=C'***'                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R3,22(R3)                                                        
         LA    R5,24(R5)                                                        
         BCT   R4,BC30                                                          
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         MVC   SAVECODE(1),CARD+5                                               
         LA    RE,GRPTOTS                                                       
         LH    RF,ALINS                                                         
         MH    RF,=H'24'                                                        
         XCEF                                                                   
         MVI   LINE,99                                                          
         CLI   DONEBYTE,1                                                       
         BE    BC62                                                             
         BAS   RE,GRUPHD                                                        
         EJECT                                                                  
BC40     LA    R2,OFFLIST          FIND OFFICE NAME                             
         SR    R3,R3                                                            
         LH    R4,OFFNUM                                                        
BC40A    CLC   0(2,R2),CARD+12                                                  
         BE    BC40B                                                            
         LA    R3,1(R3)                                                         
         LA    R2,22(R2)                                                        
         BCT   R4,BC40A                                                         
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'MISSING CARD FOR OFFICE'                                
         MVC   P+24(2),CARD+12                                                  
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
BC40B    MVC   POFF,2(R2)                                                       
         CLI   STABYTE,1           TEST IF FIRST TIME FOR STATION               
         BNE   BC42                                                             
         MVI   STABYTE,0                                                        
         LA    R5,STALIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
BC40D    CLC   0(5,R5),CARD+7      DO WE HAVE RATE CARD                         
         BE    BC41                                                             
*                                                                               
         CLC   0(5,R5),=C'DUMMY'   SHOULD WE USE DEFAULT RATE                   
         BE    BC40G                                                            
*                                                                               
         BXLE  R5,R6,BC40D                                                      
*                                                                               
         MVI   P,C'*'              DON'T EVEN HAVE DUMMY RATE CARD              
         MVC   P+1(109),P          SO BLOW UP                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(35),=C'MISSING DUMMY RATE CARD FOR STATION'                    
         MVC   P+36(5),CARD+7                                                   
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
BC40G    LA    R2,60               ALLOW 60 STATIONS MISSING RATE CARDS         
         LA    R1,MSGRATE          BUILD TABLE OF STATIONS                      
BC40L    OC    0(5,R1),0(R1)       NEXT AVAILABLE SPACE                         
         BZ    BC40P                                                            
         CLC   0(5,R1),CARD+7      STATION ALREADY IN TABLE                     
         BE    BC41                                                             
         LA    R1,5(R1)                                                         
         BCT   R2,BC40L                                                         
*                                                                               
         MVI   P,C'*'              TABLE TOO BIG - BLOW UP                      
         MVC   P+1(109),P                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(27),=C'TOO MANY MISSING RATE CARDS'                            
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
BC40P    MVC   0(5,R1),CARD+7                                                   
         SPACE 1                                                                
BC41     MVC   RATE,5(R5)                                                       
         MVC   PSTA(4),CARD+7                                                   
         CLI   CARD+11,C' '                                                     
         BE    BC42                                                             
         LA    R1,PSTA+3                                                        
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),CARD+11                                                  
         MVI   2(R1),C'M'                                                       
*                                                                               
BC42     LA    R4,3                CONVERT TO BINARY                            
         LA    R5,CARD+15                                                       
         LA    R6,AMT1                                                          
BC44     PACK  WORK(8),0(10,R5)                                                 
         CVB   R0,WORK                                                          
         ST    R0,0(R6)                                                         
         LA    R6,8(R6)                                                         
         LA    R5,12(R5)                                                        
         BCT   R4,BC44                                                          
*                                  POST TO TOTALS                               
         BAS   RE,ADDUP                                                         
         LA    R8,STATOTS                                                       
         BAS   RE,ADDIN                                                         
         LA    R8,GRPTOTS                                                       
         LR    R4,R3                                                            
         MH    R4,=H'24'                                                        
         LA    R8,0(R8,R4)                                                      
         BAS   RE,ADDIN                                                         
         LH    R8,OFFNUM                                                        
         MH    R8,=H'24'                                                        
         LA    R8,GRPTOTS(R8)                                                   
         BAS   RE,ADDIN                                                         
         LA    R8,SBGTOTS                                                       
         LA    R8,0(R8,R4)                                                      
         BAS   RE,ADDIN                                                         
         LH    R8,OFFNUM                                                        
         MH    R8,=H'24'                                                        
         LA    R8,SBGTOTS(R8)                                                   
         BAS   RE,ADDIN                                                         
         LA    R5,AMT1                                                          
         BAS   RE,EDIT1                                                         
         GOTO1 =V(PRINTER)                                                      
         B     BC65                                                             
         EJECT                                                                  
         SPACE 1                                                                
BC50     CLI   CARD+8,C'R'         TEST FOR A GROUP FILTER                      
         BE    *+12                                                             
         CLI   CARD+8,C'T'                                                      
         BNE   READ                                                             
         MVC   FILTGRP,CARD+8                                                   
         B     READ                                                             
         SPACE 2                                                                
         SPACE 2                                                                
BC60     LH    R5,OFFNUM                                                        
         AH    R5,=H'1'                                                         
         STH   R5,ALINS                                                         
BC62     MVI   DONEBYTE,0                                                       
         MVC   COMMAND,=CL8'INDEX'                                              
         BAS   RE,GETWK                                                         
         TM    DMCB+8,X'80'        TEST FOR NO MORE INDICES                     
         BO    COMX                YES-ALL DONE                                 
         CLC   ID(2),=C'I1'        TEST FOR MMR                                 
         BNE   BC62                SKIP IT                                      
BC63     CLC   ID+2(3),=C'R34'     TEST FOR REP34 AS SOURCE                     
         BNE   BC62                NO-GET THE NEXT                              
         CLI   ID+7,C'S'           TEST FOR CLASS 'S'                           
         BNE   BC62                                                             
         CLI   FILTGRP,0           TEST FOR ANY GROUP FILTER                    
         BE    BC64                                                             
         CLC   FILTGRP,ID+5        TEST FOR FILTERED GROUP                      
         BNE   BC62                                                             
         SPACE 1                                                                
BC64     MVI   STABYTE,1                                                        
         MVI   FRSTIME,0                                                        
         ZAP   PAGE,=PL4'1'                                                     
         ZAP   LINE,=PL2'75'                                                    
         ZAP   RECNT,=P'0'                                                      
         MVC   COMMAND,=CL8'READ'                                               
         BAS   RE,GETWK            READ FIRST RECORD FOR DATA SET               
         SPACE 1                                                                
BC65     BAS   RE,GETWK                                                         
         TM    DMCB+8,X'80'        TEST FOR EOF ON DATA SET                     
         BNO   BC66                NO                                           
         CP    RECNT,=P'0'         TEST FOR NO DETAIL RECORDS                   
         BE    BC62                YES - READ NEXT INDEX                        
         MVI   DONEBYTE,1                                                       
         XC    CARD,CARD                                                        
         B     BC23                                                             
BC66     AP    RECNT,=P'1'         BUMP RECORD COUNT                            
         TM    DMCB+8,X'40'        TEST FOR DISK ERROR                          
         BZ    *+6                                                              
         DC    H'0'                BLOW FOR DISK ERROR                          
         B     BC20                                                             
         SPACE 2                                                                
COMX     LA    R5,MSGRATE          PRINT ANY MISSING RATE CARDS                 
         OC    0(5,R5),0(R5)                                                    
         BZ    EOJ                 NONE TO PRINT                                
*                                                                               
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(16),=C'DUMMY RATES WERE'                                       
         MVC   P+17(20),DUMMYRT                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(22),=C'MISSING RATE CARDS FOR'                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R6,4                MAX 4 LINES                                  
COMX5    LA    R4,15               OF 15 STATIONS                               
         LA    R3,P                                                             
COMX10   MVC   0(5,R3),0(R5)                                                    
         LA    R3,7(R3)                                                         
         LA    R5,5(R5)                                                         
         OC    0(5,R5),0(R5)       MORE TO PRINT                                
         BZ    COMX20              NO                                           
         BCT   R4,COMX10           YES                                          
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         BCT   R6,COMX5                                                         
         B     EOJ                                                              
*                                                                               
COMX20   GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      XBASE                                                                  
         EJECT                                                                  
ADDIN    NTR1                                                                   
         LA    R4,6                                                             
         LA    R5,AMT1                                                          
AI2      L     R0,0(R5)                                                         
         A     R0,0(R8)                                                         
         ST    R0,0(R8)                                                         
         LA    R8,4(R8)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,AI2                                                           
         XIT1                                                                   
         SPACE 2                                                                
ADDUP    LA    R5,AMT1                                                          
         LA    R4,3                                                             
         LA    R6,RATE                                                          
AU2      L     R1,0(R5)                                                         
         M     R0,0(R6)                                                         
         SLDA  R0,1                                                             
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BL    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,4(R5)                                                         
         LA    R5,8(R5)                                                         
         LA    R6,4(R6)                                                         
         BCT   R4,AU2                                                           
         BR    RE                                                               
         SPACE 2                                                                
GETGRPNM LA    R2,GRPLIST                                                       
         CLC   0(2,R1),0(R2)                                                    
         BE    *+12                                                             
         LA    R2,12(R2)                                                        
         B     *-14                                                             
         LA    R1,2(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
GRUPHD   LH    R6,GRPNUM                                                        
         LA    R2,GRPLIST                                                       
         CLI   1(R2),C' '                                                       
         BNE   *+14                                                             
         CLC   0(1,R2),CARD+5                                                   
         BE    GRUPHD10                                                         
         LA    R2,12(R2)                                                        
         BCT   R6,*-22                                                          
         B     MISSGRP                                                          
*GRUPHD10 MVC   MID2(10),2(R2)                                                  
*                                                                               
GRUPHD10 LH    R6,GRPNUM                                                        
         LA    R2,GRPLIST                                                       
         CLC   0(2,R2),CARD+5                                                   
         BE    GRPHD20                                                          
         LA    R2,12(R2)                                                        
         BCT   R6,*-14                                                          
         B     MISSGRP                                                          
*GRPHD20  MVC   MID2+11(10),2(R2)                                               
GRPHD20  BR    RE                                                               
         SPACE 2                                                                
MISSGRP  MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(22),=C'MISSING CARD FOR GROUP'                                 
         MVC   P+23(2),CARD+5                                                   
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
         SPACE 2                                                                
EDIT1    NTR1                                                                   
         LA    R4,PAMTS                                                         
         LA    R6,3                                                             
*                                                                               
ED10     L     R0,0(R5)                                                         
         EDIT  (R0),(15,0(R4)),MINUS=YES                                        
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         L     R0,0(R5)                                                         
         EDIT  (R0),(15,0(R4)),2,MINUS=YES                                      
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R6,ED10                                                          
         XIT1                                                                   
*                                                                               
GETWK    NTR1                                                                   
         GOTO1 =V(WORKER),DMCB,COMMAND,AWRKBUFF,ID,IOAR                         
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
SB1      DC    C'---PRIOR  MONTHS''  ADJUSTMENT---  -----LAST  MONTH''SX        
                 ACTUAL------  ---CURRENT  MONTH''S  ESTIMATE---'               
SB2      DC    CL32' STATION     OFFICE'                                        
         DC    3C'     GROSS         COMMISSION     '                           
SB3      DC    10C'-',C' ',20C'-',C' ',6C'---------------  '                    
LSB1     EQU   SB2-SB1                                                          
LSB2     EQU   SB3-SB2                                                          
LSB3     EQU   *-SB3                                                            
GRPTOTS  DS    300F                50 LINES X 6                                 
SBGTOTS  DS    300F                                                             
OFFNUM   DS    H                                                                
GRPNUM   DS    H                                                                
ALINS    DS    H                                                                
STATOTS  DS    6F                                                               
SAVECODE DS    CL7                                                              
DUMMYRT  DS    CL20                DUMMY RATE                                   
MSGRATE  DS    60CL5               LIST OF MISSING RATE CARD STATIONS           
IOAR     DS    0F                                                               
         DC    H'84'                                                            
         DC    H'0'                                                             
CARD     DS    CL80                                                             
DUB      DS    D                                                                
WORK     DS    CL17                                                             
AMT1     DS    F                                                                
         DS    5F                                                               
HALF     DS    CL2                                                              
ERR1     DC    C'INVALID CARD - REPORT TERMINATED'                              
OFFLIST  DC    CL22'  TOTALS'                                                   
         DS    49CL22              49 OFFICES                                   
GRPLIST  DC    40CL12' '           40 GROUPS                                    
FRSTIME  DC    X'00'                                                            
PDATE    DS    CL6                                                              
PRQTR    DS    CL12                                                             
RATE     DS    CL12                                                             
DONEBYTE DC    X'00'                                                            
STABYTE  DC    X'01'                                                            
FILTGRP  DC    X'00'               GROUP FILTER FROM 'DATA' CARD(OS)            
P1       DS    F                                                                
RECNT    DS    PL8                 RECORD COUNT                                 
*                                                                               
*                                                                               
COMMAND  DC    CL8' '                                                           
ID       DS    CL16                                                             
FILE     DC    320X'00'                                                         
LASTFILE DC    A(FILE)                                                          
AWRKBUFF DC    A(WORKBUFF)                                                      
DMCB     DC    6F'0'                                                            
UTL      DS    0D                                                               
         DC    4X'00',X'08',2X'00'                                              
*                                                                               
         DS    3F                                                               
         LTORG                                                                  
         CNOP  2,4                                                              
STALIST  DC    H'17'                                                            
         DC    A(STALISTX-1)                                                    
STALISTX EQU   *                                                                
         DS    2800CL17                                                         
         SPACE 2                                                                
         ENTRY WORKBUFF                                                         
WORKBUFF DS    0D                                                               
         DC    4096X'00'           WORK FILE BUFFER                             
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         ORG   P                                                                
PSTA     DS    CL10                                                             
         DS    CL1                                                              
POFF     DS    CL20                                                             
         DS    CL1                                                              
PAMTS    DS    CL15                                                             
         DS    CL2                                                              
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088REREPCMSN405/01/02'                                      
         END                                                                    
