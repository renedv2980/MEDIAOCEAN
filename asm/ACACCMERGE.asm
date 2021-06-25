*          DATA SET ACACCMERGE AT LEVEL 017 AS OF 05/01/02                      
*PHASE ACMERGE,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'MERGE - 2 OLD STYLE ACCOUNT FILES'                              
MERGE    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE MERGEX-MERGED,MERGE,=V(REGSAVE),CLEAR=YES                        
         USING MERGED,RC                                                        
         SPACE 1                                                                
         LA    R3,DMPLIST          SET STXITER DUMP PARAMETERS                  
         LR    R2,RB                                                            
         ST    R2,0(R3)                                                         
         L     R2,=V(DUMMY)                                                     
         ST    R2,4(R3)                                                         
         OI    4(R3),X'80'                                                      
         GOTO1 =V(STXITER),DMCB,A(DMPLIST)                                      
*                                                                               
         MVI   SPACES,C' '         INITIALIZE                                   
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         GOTO1 =V(PRINT),DMCB,SPACES,=C'BC01'                                   
         EJECT                                                                  
*              READ IN CONTROL CARDS                                            
         SPACE 1                                                                
IN2      GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'                                                   
         BE    INX                                                              
         MVC   P+1(80),CARD                                                     
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
*                                                                               
         CLC   CARD(4),=C'GEN='    GEN=NO/YES                                   
         BNE   IN4                                                              
         MVC   GEN,CARD+4          BUILD GENERATION RECORD                      
         B     IN2                                                              
*                                                                               
IN4      CLC   CARD(6),=C'TRAIL='  TRAIL=NO/YES                                 
         BNE   IN5                                                              
         MVC   TRAIL,CARD+6        BUILD TRAILER RECORD                         
         B     IN2                                                              
*                                                                               
IN5      CLC   CARD(5),=C'DUMP='   DUMP=NO/YES                                  
         BNE   IN6                                                              
         MVC   DUMP,CARD+5        DUMP SOME OUTPUT RECORDS                      
         B     IN2                                                              
*                                                                               
IN6      CLC   CARD(6),=C'FIRST='  FIRST=YES                                    
         BNE   IN7                                                              
         MVC   FRST,CARD+6         CREATE FIRST ACCHST FROM RECOVERY            
         MVI   GEN,C'Y'                                                         
         MVI   TRAIL,C'Y'                                                       
         MVI   EOFB,C'Y'                                                        
         B     IN2                                                              
*                                                                               
IN7      DC    H'0'                INVALID INPUT CARD                           
         SPACE 1                                                                
INX      DS    0H                                                               
         GOTO1 =V(PRINT),DMCB,SPACES,=C'BC01'                                   
         EJECT                                                                  
*              OPEN THE FILES                                                   
         SPACE 1                                                                
         OPEN  (TINA,(INPUT))     OPEN THE FILES                                
         CLI   FRST,C'Y'                                                        
         BE    GET3                FIRST TIME ONLY ONE TAPE                     
         OPEN  (TINB,(INPUT))                                                   
*                                                                               
GET3     OPEN  (TOUT,(OUTPUT))                                                  
         CLI   GEN,C'Y'            NEED A GENERATION RECORD                     
         BNE   GET5                                                             
         L     R3,AIOA             OUTPUT AREA                                  
         LA    R3,4(R3)                                                         
         USING ACKEYD,R3                                                        
         XC    ACKEYD(255),ACKEYD                                               
         MVI   ACKEYSBR,1                                                       
         MVC   ACLENGTH,=H'69'                                                  
         LA    R4,ACRECORD                                                      
         USING ACCONTD,R4                                                       
         MVC   ACCONTEL(2),=X'040E'                                             
         ZAP   ACCONCOT,=P'1'                                                   
         GOTO1 =V(DATCON),DMCB,(5,0),(2,ACCONLOD)                               
         MVC   ACCONUPS,ACCONLOD                                                
         MVC   ACCONUND,ACCONLOD                                                
         SR    R0,R0                                                            
         IC    R0,ACCONTLN                                                      
         AR    R4,R0                                                            
         MVC   0(2,R4),=X'FE05'                                                 
         MVC   2(3,R4),=C'001'                                                  
         SR    R0,R0                                                            
         ICM   R0,3,ACLENGTH                                                    
         AH    R0,=H'4'                                                         
         SH    R3,=H'4'                                                         
         XC    0(4,R3),0(R3)                                                    
         STCM  R0,3,0(R3)                                                       
         L     R4,AIOA                                                          
         BAS   R9,PUT                                                           
*                                                                               
GET5     L     R2,AIOA             GET FIRST RECORDS                            
         GET   TINA,(R2)                                                        
         CLI   FRST,C'Y'                                                        
         BNE   GET6                                                             
         LA    RF,4(R2)            STRIPE OFF THE RCV HEADER                    
         LA    RE,28(R2)                                                        
         SR    R1,R1                                                            
         ICM   R1,3,0(R2)                                                       
         SH    R1,=H'24'                                                        
         STCM  R1,3,0(R2)                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
GET6     CLC   4(42,R2),LSTAKEY                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTAKEY,4(R2)                                                    
         AP    INACNT,=P'1'                                                     
         CLI   4(R2),X'00'         GENERATION RECORD                            
         BNE   GET7                                                             
         CLI   GEN,C'Y'            OPTION TO GENERATE ONE                       
         BNE   GET7                                                             
         SP    INACNT,=P'1'                                                     
         B     GET5                                                             
*                                                                               
GET7     CLI   FRST,C'Y'                                                        
         BE    GET9                                                             
         L     R3,AIOB             GET FIRST RECORDS                            
         GET   TINB,(R3)                                                        
         CLC   4(42,R3),LSTBKEY                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTBKEY,4(R3)                                                    
         AP    INBCNT,=P'1'                                                     
         CLI   4(R3),X'00'         GENERATION RECORD                            
         BNE   GET9                                                             
         CLI   GEN,C'Y'            OPTION TO GENERATE ONE                       
         BNE   GET9                                                             
         SP    INBCNT,=P'1'                                                     
         B     GET7                                                             
*                                                                               
GET9     CLI   EOFA,C'Y'                                                        
         BNE   GET11                                                            
         CLI   EOFB,C'Y'                                                        
         BE    PUTLAST             EOF BOTH                                     
         B     PUTB                EOF A BUT NOT B                              
*                                                                               
GET11    CLI   EOFB,C'Y'                                                        
         BE    PUTA                EOF B NOT A                                  
         CLC   4(42,R2),4(R3)      A VS. B                                      
         BL    PUTA                                                             
         BH    PUTB                                                             
         B     GETA                                                             
*                                                                               
PUTA     LR    R4,R2                                                            
         BAS   R9,PUT              PROCESS A RECORD                             
GETA     GET   TINA,(R2)                                                        
         AP    INACNT,=P'1'                                                     
         CLI   FRST,C'Y'                                                        
         BNE   GET9                                                             
         LA    RF,4(R2)            STRIPE OFF THE RCV HEADER                    
         LA    RE,28(R2)                                                        
         SR    R1,R1                                                            
         ICM   R1,3,0(R2)                                                       
         SH    R1,=H'24'                                                        
         STCM  R1,3,0(R2)                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         B     GET9                                                             
*                                                                               
PUTB     LR    R4,R3                                                            
         BAS   R9,PUT              PROCESS B RECORD                             
         GET   TINB,(R3)                                                        
         AP    INBCNT,=P'1'                                                     
         B     GET9                                                             
         EJECT                                                                  
*                                                                               
LASTA    MVI   EOFA,C'Y'                                                        
         B     GET9                                                             
*                                                                               
LASTB    MVI   EOFB,C'Y'                                                        
         B     GET9                                                             
*                                                                               
PUTLAST  CLI   4(R4),X'FF'         WAS LAST A TRAILER                           
         BE    EOJ                                                              
         MVI   4(R4),X'FF'                                                      
         MVI   TRAIL,C'Y'          FORCE A TRAILER                              
         BAS   R9,PUT                                                           
*                                                                               
EOJ      CLOSE TINA                CLOSE THE FILE                               
         CLI   FRST,C'Y'                                                        
         BE    EOJ3                                                             
         CLOSE TINB                                                             
EOJ3     CLOSE TOUT                                                             
**                                                                              
         MVC   P(15),=CL15'TAPE A IN'                                           
         EDIT  INACNT,(7,P+17)                                                  
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
*                                                                               
         MVC   P(15),=CL15'TAPE B IN'                                           
         EDIT  INBCNT,(7,P+17)                                                  
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
*                                                                               
         MVC   P(15),=CL15'TAPE OUT'                                            
         EDIT  OUTCNT,(7,P+17)                                                  
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         MVC   P,SPACES                                                         
*                                                                               
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         XBASE                                                                  
         EJECT                                                                  
*              PUT ROUTINE                                                      
*               R4=OUTPUT RECORD                                                
*                                                                               
PUT      CLI   4(R4),X'FF'         TRAILER                                      
         BNE   PUT5                                                             
         CLI   TRAIL,C'Y'          DO I WANT TO GEN A TRAILER                   
         BNE   PUT5                                                             
         BAS   RE,TRL               BUILD A TRAILER                             
*                                                                               
PUT5     CLI   DUMP,C'N'                                                        
         BE    PUT7                                                             
         CLI   4(R4),X'FF'        TRAILER RECORD                                
         BE    *+14                                                             
         CP    DMPCNT,MAXCNT                                                    
         BH    PUT7                                                             
         AP    DMPCNT,=P'1'                                                     
         EDIT  DMPCNT,(5,P),ALIGN=LEFT,WRK=CARD                                 
         SR    R0,R0                                                            
         ICM   R0,3,0(R4)                                                       
         GOTO1 =V(PRNTBL),DMCB,(5,P),(R4),C'DUMP',(R0),=C'2D',         X        
               (C'P',V(PRINT))                                                  
*                                                                               
*                                                                               
PUT7     PUT   TOUT,(R4)                                                        
         AP    OUTCNT,=P'1'                                                     
         CLC   4(42,R4),LSTOKEY                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   LSTOKEY,4(R4)                                                    
         BR    R9                                                               
         EJECT                                                                  
TRL      L     R3,AIOA                                                          
         LA    R3,4(R3)                                                         
         USING ACKEYD,R3                                                        
         XC    ACKEYACC(255),ACKEYACC                                           
         MVI   ACKEYACC,X'FF'                                                   
         MVC   ACKEYACC+1(41),ACKEYACC                                          
         MVC   ACLENGTH,=H'64'                                                  
         LA    R4,ACRECORD                                                      
         USING ACCONTD,R4                                                       
         MVC   ACCONTEL(2),=X'040E'                                             
         ZAP   ACCONCOT,=P'1'                                                   
         MVC   ACCONDTE,=C'930207'                                              
         SR    R0,R0                                                            
         ICM   R0,3,ACLENGTH                                                    
         AH    R0,=H'4'                                                         
         L     R4,AIOA                                                          
         XC    0(4,R4),0(R4)                                                    
         STCM  R0,3,0(R4)                                                       
         BR    RE                                                               
         EJECT                                                                  
*              CONSTANTS                                                        
*                                                                               
MAXCNT   DC    PL5'1000'                                                        
DMPCNT   DC    PL5'0'                                                           
INACNT   DC    PL5'0'                                                           
INBCNT   DC    PL5'0'                                                           
OUTCNT   DC    PL5'0'                                                           
*                                                                               
GEN      DC    C'Y'                                                             
TRAIL    DC    C'Y'                                                             
DUMP     DC    C'N'                                                             
FRST     DC    C'N'                                                             
*                                                                               
EOFA     DC    C'N'                                                             
EOFB     DC    C'N'                                                             
*                                                                               
AIOA     DC    A(IOA)                                                           
AIOB     DC    A(IOB)                                                           
*                                                                               
LSTAKEY  DC    XL42'00'                                                         
LSTBKEY  DC    XL42'00'                                                         
LSTOKEY  DC    XL42'00'                                                         
         SPACE 1                                                                
DMPLIST  DC    6F'0'                                                            
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*DCB*'                                                         
TINA     DCB   DDNAME=TINA,DSORG=PS,MACRF=(GM),EODAD=LASTA,RECFM=VB             
*              LRECL=2048,BLKSIZE=32760                                         
*                                                                               
TINB     DCB   DDNAME=TINB,DSORG=PS,MACRF=(GM),EODAD=LASTB,RECFM=VB             
*              LRECL=2048,BLKSIZE=32760                                         
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),RECFM=VB,               *        
               LRECL=2048,BLKSIZE=32760                                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              IO AREAS                                                         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**IOA**'                                                       
IOA      DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'**IOB**'                                                       
IOB      DS    2000C                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL STORAGE                                     
         SPACE 1                                                                
MERGED   DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
ADCB     DS    A                                                                
CARD     DS    CL80                                                             
         DS    CL1                 (P-1)                                        
P        DS    CL132                                                            
SPACES   DS    CL132                                                            
WORK     DS    CL64                                                             
MERGEX   EQU   *                                                                
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACACCMERGE05/01/02'                                      
         END                                                                    
