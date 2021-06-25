*          DATA SET FATEMPCLR  AT LEVEL 004 AS OF 02/10/03                      
*PHASE TEMPCLRA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
         TITLE 'FATEMPCLR - TEMPSTR REFORMATTER PROGRAM'                        
         PRINT NOGEN                                                            
TEMPCLR  CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE 0,TEMPCLR,VREGSAVE                                               
         L     R9,=V(CPRINT)       SET FOR REPORT                               
         USING DPRINT,R9                                                        
         MVC   TITLE(30),=CL30'TEMPSTR REFORMATTER PROGRAM'                     
*                                                                               
         LA    RE,TEMPCLR          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
         EJECT                                                                  
CARDS    GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    CARDSX                                                           
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CARDS1   CLC   C(4),=C'LEN='                                                    
         BNE   CARDS2                                                           
         GOTO1 =V(NUMVAL),DMCB,(0,C+4),(2,0)                                    
         CLI   0(R1),X'FF'                                                      
         BE    CARDSINV                                                         
         ICM   RF,15,4(R1)                                                      
         BNP   CARDSINV                                                         
         C     RF,=F'18432'                                                     
         BE    CARDS1A                                                          
         C     RF,=F'14336'                                                     
         BE    CARDS1A                                                          
         B     CARDSINV                                                         
CARDS1A  ST    RF,LENTWA                                                        
         B     CARDS                                                            
*                                                                               
CARDS2   CLC   C(4),=C'NUM='                                                    
         BNE   CARDS3                                                           
         GOTO1 =V(NUMVAL),DMCB,(0,C+4),(2,0)                                    
         CLI   0(R1),X'FF'                                                      
         BE    CARDSINV                                                         
         ICM   RF,15,4(R1)                                                      
         BNP   CARDSINV                                                         
         C     RF,=F'12'                                                        
         BE    CARDS2A                                                          
         B     CARDSINV                                                         
CARDS2A  ST    RF,NUMTWA                                                        
         B     CARDS                                                            
*                                                                               
CARDS3   EQU   *                                                                
CARDSINV MVC   P(22),=C'INVALID PARAMETER CARD'                                 
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
CARDSX   EQU   *                                                                
         EJECT                                                                  
         GOTO1 =V(DADDS),DMCB,DAOPEN,TWA,0,TMP,0,0                              
         LA    R2,TMP                                                           
         USING DTFPHD,R2                                                        
         MVC   DNEXT(4),=X'00010000'                                            
         DROP  R2                                                               
*                                                                               
         L     R5,LENTWA           COMPUTE RECORDS PER TRACK                    
         GOTO1 =V(DADDS),DMCB,DARPT,,(R5)                                       
         LH    R5,DMCB+10                                                       
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R5,RPTTWA                                                        
         MVC   P(20),=CL40'RECORDS PER TRACK = '                                
         EDIT  (R5),(3,P+20),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LR    R1,R5               DYNAMICALLY BUILD CCW CHAIN                  
         BCTR  R1,0                                                             
         LA    R2,BUF                                                           
         MVC   BUF+6(2),LENTWA+2   SET RECORD LENGTH                            
DYNCCW   MVC   L'BUF(L'BUF,R2),0(R2)                                            
         LA    R2,L'BUF(R2)                                                     
         BCT   R1,DYNCCW                                                        
*                                                                               
         LA    R2,L'BUF(R2)        COMPUTE LENGTH OF CCW CHAIN                  
         S     R2,=A(BUF)                                                       
*                                                                               
         SR    R3,R3               WRITE TRACKS UNTIL END OF FILE               
LOOP     DS    0H                                                               
         GOTO1 =V(DADDS),DMCB,WTTRK,CCW,(R2),TMP,ADDR,(X'FF',BUF)               
         TM    9(R1),X'04'                                                      
         BO    FOUNDEOF                                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AR    R3,R5               COUNT RECORDS                                
         B     LOOP                                                             
*                                                                               
FOUNDEOF SR    R2,R2               COMPUTE NUM OF TERMINALS SUPPORTED           
         D     R2,NUMTWA                                                        
         MVC   P(32),=CL32'NUMBER OF TERMINALS SUPPORTED = '                    
         EDIT  (R3),(6,P+32),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
DMCB     DS    8F                                                               
ADDR     DS    F                                                                
*                                                                               
VREGSAVE DC    V(REGSAVE)          FOR STXITER                                  
*                                                                               
LENTWA   DC    F'18432'            SET BY LEN= CARD                             
NUMTWA   DC    F'12'               SET BY NUM= CARD                             
RPTTWA   DC    F'0'                COMPUTED FROM LENTWA                         
*                                                                               
UTL      DC    F'0',255X'00'                                                    
*                                                                               
         DS    0D                                                               
CCW      DS    256X                                                             
WORK     DS    CL17                                                             
C        DS    CL80                                                             
*                                                                               
TMP      DMDA                                                                   
*                                                                               
SSB      DC    H'0',X'FF',1023X'00'                                             
*                                                                               
         DS    0D                                                               
BUF      DS    0XL12                                                            
         DC    6X'00'              COUNT FIELD                                  
         DC    AL2(6144)                                                        
         DC    A(TWA)              ADDRESS OF DATA AREA                         
         DS    200X                                                             
LBUF     DS    F                                                                
TWA      DC    50000X'00'                                                       
         EJECT                                                                  
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004FATEMPCLR 02/10/03'                                      
         END                                                                    
