*          DATA SET DMPRTQINFO AT LEVEL 002 AS OF 12/15/16                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 047421.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PRTQINFO                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
         TITLE 'PRTQINFO - PRINT DETAILS OF MULTIPLE QUEUES'                    
         PRINT NOGEN                                                            
PQINFO   CSECT                                                                  
         NBASE 0,PQINFO,RA,WORK=A(PQWORK)                                       
         L     R2,=V(CPRINT)                                                    
         USING DPRINT,R2                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(30),=CL30'PRINT QUEUE INFO'                                
         LA    R3,Q                                                             
         USING PQPLD,R3                                                         
         LA    R4,BUFF                                                          
         USING PQRECD,R4                                                        
*                                                                               
INIT     GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(6),=C'DDSIO='                                                  
         BNE   INIT1                                                            
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),C+6                                                      
*                                                                               
INIT1    MVC   PRTQID,=CL7'PRTQU '                                              
         GOTO1 =V(DATAMGR),DMCB,=C'GLIST',PRTQID,X,Q,BUFF                       
         L     RE,X+32                                                          
         SR    RF,RF                                                            
         IC    RF,0(RE)            RF=NUMBER OF PRTQ FILES                      
         STH   RF,ACTPQS                                                        
         CLC   C(6),=C'DDSIO='                                                  
         BNE   NEXT1                                                            
*                                                                               
NEXT     GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
*                                                                               
NEXT1    CLI   C,C'*'              IGNORE COMMENT CARDS                         
         BE    NEXT                                                             
         CLC   C(6),=C'COUNT='                                                  
         BE    CNT                                                              
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
* CARD FORMAT COUNT=N                                                           
*                                                                               
CNT      OI    C+6,C'0'            COUNT=N WHERE N IS REQUIRED NUM PQS          
         PACK  DUB,C+6(1)                                                       
         CVB   RF,DUB                                                           
         STH   RF,REQPQS           REQUIRED NUM OF PRTQS                        
         LTR   RF,RF                                                            
         BZ    CNTERR                                                           
         CH    RF,=H'8'                                                         
         BH    CNTERR                                                           
         XC    CURPQS,CURPQS                                                    
*                                                                               
CNT1     LH    RF,CURPQS           BUMP TO NEXT CURRENT PQ                      
         LA    RF,1(RF)                                                         
         STH   RF,CURPQS                                                        
         CH    RF,ACTPQS                                                        
         BH    CNTPRT                                                           
         STC   RF,PRTQID+4                                                      
         OI    PRTQID+4,C'0'                                                    
         XC    X,X                 CLEAR USER INDEX                             
         LA    R9,X                                                             
         USING UKINDEX,R9                                                       
*                                                                               
CNT2     GOTO1 =V(DATAMGR),DMCB,(X'00',=C'INDEX'),PRTQID,X,Q,BUFF               
         CLI   DMCB+8,0                                                         
         BE    CNT3                                                             
         TM    DMCB+8,X'40'        TEST DISK ERROR                              
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'80'        TEST EOF                                     
         BO    CNT1                                                             
         B     CNT2                                                             
*                                                                               
CNT3     MVC   USERID,UKSRCID      EXTRACT USER ID                              
         MVC   PART1,=H'1'                                                      
         MVC   PART2,=H'0'                                                      
         CLI   UKSEQ,0             TEST IF SINGLE CI                            
         BE    CNT4                                                             
         L     RE,=A(BUFF1)        COPY BUFFER                                  
         L     RF,=F'6144'                                                      
         L     R0,=A(BUFF)                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         L     R4,=A(BUFF1)                                                     
         XC    Q(4),Q              SET TO READ FILE HEADER                      
         MVC   Q+4(4),=C'PAGE'                                                  
         GOTO1 =V(DATAMGR),DMCB,(X'00',=C'RANDOM'),PRTQID,X,Q,(R4)              
         CLI   DMCB+8,0                                                         
         BNE   CNT2                                                             
         SR    RF,RF               GET TOTAL NUMBER OF CIS                      
         IC    RF,PQNCI                                                         
         SH    RF,=H'1'                                                         
         BNP   CNT4                                                             
         STH   RF,PART2                                                         
*                                                                               
CNT4     SR    RE,RE               COMPUTE TARGET PRTQ FILE                     
         LH    RF,USERID                                                        
         LH    R0,REQPQS           DIVIDE BY REQUIRED NUM OF PQS                
         DR    RE,R0                                                            
         LA    RE,1(RE)                                                         
         STH   RE,WHATPQ                                                        
         SLL   RE,5                                                             
         LA    R5,PQARRAY(RE)                                                   
*                                                                               
CNT5     L     RF,04(R5)           BUMP NUMBER OF REPORTS                       
         LA    RF,1(RF)                                                         
         ST    RF,04(R5)                                                        
*                                                                               
CNT6     OC    PART2,PART2         TEST SMALL/LARGE REPORT                      
         BNZ   CNT6A                                                            
         L     RF,08(R5)           BUMP NUMBER OF SMALL REPORTS                 
         LA    RF,1(RF)                                                         
         ST    RF,08(R5)                                                        
         B     CNT7                                                             
CNT6A    L     RF,12(R5)           BUMP NUMBER OF LARGE REPORTS                 
         LA    RF,1(RF)                                                         
         ST    RF,12(R5)                                                        
*                                                                               
CNT7     L     RF,16(R5)           BUMP NUMBER OF PART1 CIS REQUIRED            
         AH    RF,PART1                                                         
         ST    RF,16(R5)                                                        
*                                                                               
CNT8     L     RF,20(R5)           BUMP NUMBER OF PART2 CIS REQUIRED            
         AH    RF,PART2                                                         
         ST    RF,20(R5)                                                        
*                                                                               
CNT9     L     RE,0(R5)            GET A(USERID TABLE)                          
         LA    RF,2(RE)                                                         
CNT9A    CLC   0(2,RF),=X'FFFF'    END OF TABLE                                 
         BE    CNT9X                                                            
         OC    0(2,RF),0(RF)       EMPTY SLOT                                   
         BZ    CNT9B                                                            
         CLC   0(2,RF),USERID      ALREADY IN TABLE                             
         BE    CNT9X                                                            
         LA    RF,2(RF)                                                         
         B     CNT9A                                                            
CNT9B    MVC   0(2,RF),USERID      INSERT NEW USERID INTO TABLE                 
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          BUMP ENTRY COUNT                             
         LA    RF,1(RF)                                                         
         STH   RF,0(RE)                                                         
CNT9X    B     CNT2                                                             
*                                                                               
CNTERR   MVC   P(30),=CL30'INVALID PQ COUNT'                                    
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
*                                                                               
CNTPRT   MVC   PRTQID,=CL7'PRTQU '                                              
         GOTO1 =V(DATAMGR),DMCB,=C'GLIST',PRTQID,X,Q,BUFF                       
         L     RE,X+32             GET A(PRTQLST)                               
         LA    RE,8(RE)                                                         
         SR    RF,RF                                                            
         ICM   RF,7,5(RE)          GET A(FIRST PQ FILE DTF)                     
         SH    RF,=H'40'                                                        
         MVC   TRKS1,06(RF)        GET TRKS PER PART1 CI                        
         MVC   TRKS2,22(RF)        GET TRKS PER PART2 CI                        
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P,HDR                                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P,HDRUND                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R5,PQARRAY          R5=A(NEXT SLOT IN PQ INFO)                   
         LA    R6,L'PQARRAY        R6=LEN OF EACH ENTRY                         
         SR    R7,R7               R7=ENTRY NUM                                 
*                                                                               
CNTPRT1  AR    R5,R6               BUMP TO NEXT ENTRY                           
         LA    R7,1(R7)                                                         
         CH    R7,REQPQS                                                        
         BH    CNTPRTX                                                          
         MVC   LNE,SPACES                                                       
*                                                                               
         CVD   R7,DUB              PQ NUMBER                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  LPQ,DUB                                                          
*                                                                               
         L     RE,0(R5)            NUMBER OF USERS                              
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LUSERS,DUB                                                       
*                                                                               
         L     RF,04(R5)           NUMBER OF REPORTS                            
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LREPORTS,DUB                                                     
*                                                                               
         L     RF,08(R5)           NUMBER OF SMALL REPORTS                      
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSMALL,DUB                                                       
*                                                                               
         L     RF,12(R5)           NUMBER OF LARGE REPORTS                      
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LLARGE,DUB                                                       
*                                                                               
         L     RF,16(R5)           NUMBER OF PART1 CIS                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LCIS1,DUB                                                        
*                                                                               
         L     RF,16(R5)           NUMBER OF PART1 CIS                          
         MH    RF,TRKS1            TIMES TRKS FOR PART1                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LTRKS1,DUB                                                       
*                                                                               
         L     RF,20(R5)           NUMBER OF PART2 CIS                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LCIS2,DUB                                                        
*                                                                               
         L     RF,20(R5)           NUMBER OF PART2 CIS                          
         MH    RF,TRKS2            TIMES TRKS FOR PART2                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LTRKS2,DUB                                                       
*                                                                               
         MVC   P,LNE                                                            
         GOTO1 =V(PRINTER)                                                      
         B     CNTPRT1                                                          
*                                                                               
CNTPRTX  B     NEXT                                                             
         EJECT                                                                  
         DS    0D                                                               
PQARRAY  DC    XL32'00'                                                         
         DC    A(USERS1),XL28'00'                                               
         DC    A(USERS2),XL28'00'                                               
         DC    A(USERS3),XL28'00'                                               
         DC    A(USERS4),XL28'00'                                               
         DC    A(USERS5),XL28'00'                                               
         DC    A(USERS6),XL28'00'                                               
         DC    A(USERS7),XL28'00'                                               
         DC    A(USERS8),XL28'00'                                               
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FUL      DS    F                                                                
P1       DS    6F                                                               
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
*                                                                               
PRTQID   DS    CL8                                                              
*                                                                               
ACTPQS   DS    H                                                                
REQPQS   DS    H                                                                
CURPQS   DS    H                                                                
WHATPQ   DS    H                                                                
*                                                                               
USERID   DS    H                                                                
PART1    DS    H                                                                
PART2    DS    H                                                                
TRKS1    DS    H                                                                
TRKS2    DS    H                                                                
*                                                                               
C        DS    CL80                                                             
*                                                                               
SOFLAB   DC    X'0000',C'SOFSOF',X'0000'                                        
EOFLAB   DC    X'FFFF',C'EOFEOF',X'FFFF'                                        
*                                                                               
HDR      DC    CL132'PQ  #USERS  #RPRTS  #SMALL  #LARGE  CI-S#1  TRKS#1X        
                 CI-S#2  TRKS#2'                                                
HDRUND   DC    CL132'--  ------  ------  ------  ------  --------------X        
                 --------------'                                                
*                                                                               
LNE      DC    CL132' '                                                         
         ORG   LNE                                                              
LPQ      DS    CL2                                                              
         DS    CL2                                                              
LUSERS   DS    CL6                                                              
         DS    CL2                                                              
LREPORTS DS    CL6                                                              
         DS    CL2                                                              
LSMALL   DS    CL6                                                              
         DS    CL2                                                              
LLARGE   DS    CL6                                                              
         DS    CL2                                                              
LCIS1    DS    CL6                                                              
         DS    CL2                                                              
LTRKS1   DS    CL6                                                              
         DS    CL2                                                              
LCIS2    DS    CL6                                                              
         DS    CL2                                                              
LTRKS2   DS    CL6                                                              
         ORG                                                                    
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*NDXNDX*'                                                      
X        DC    XL40'00'                                                         
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*LNELNE*'                                                      
QH       DC    XL4'00'                                                          
Q        DC    CL133' '                                                         
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*SOBSOB*'                                                      
BUFF     DC    6144X'00'                                                        
         DC    C'*EOBEOB*'                                                      
BUFF1    DC    6144X'00'                                                        
         SPACE 2                                                                
         DC    C'*WRKWRK*'                                                      
PQWORK   DS    2000D                                                            
         SPACE 2                                                                
USERS1   DC    H'0',5000H'0',X'FFFF'                                            
USERS2   DC    H'0',5000H'0',X'FFFF'                                            
USERS3   DC    H'0',5000H'0',X'FFFF'                                            
USERS4   DC    H'0',5000H'0',X'FFFF'                                            
USERS5   DC    H'0',5000H'0',X'FFFF'                                            
USERS6   DC    H'0',5000H'0',X'FFFF'                                            
USERS7   DC    H'0',5000H'0',X'FFFF'                                            
USERS8   DC    H'0',5000H'0',X'FFFF'                                            
         SPACE 2                                                                
UTL      CSECT                                                                  
         DC    F'0',X'01'                                                       
SSB      CSECT                                                                  
         DC    10F'0'                                                           
         EJECT                                                                  
*DMPRTQK                                                                        
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
*DMPRTQD                                                                        
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*DDDPRINTL                                                                      
       ++INCLUDE DDDPRINTL                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMPRTQINFO12/15/16'                                      
         END                                                                    
