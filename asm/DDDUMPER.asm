*          DATA SET DDDUMPER   AT LEVEL 005 AS OF 08/28/98                      
*PHASE DUMPER,*                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'DUMPER - DUMP A DISK DATASET'                                   
DUMPER   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE DUMPX-DUMPD,DUMPER,=V(REGSAVE),CLEAR=YES                         
         USING DUMPD,RC                                                         
         SPACE 1                                                                
         LA    R3,DMPLIST          SET STXITER DUMP PARAMETERS                  
         LR    R2,RB                                                            
         ST    R2,0(R3)                                                         
         L     R2,=V(DUMMY)                                                     
         ST    R2,4(R3)                                                         
         OI    4(R3),X'80'                                                      
         GOTO1 =V(STXITER),DMCB,A(DMPLIST)                                      
         SPACE 1                                                                
         MVI   SPACES,C' '         INITIALIZE                                   
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         ZAP   DUMPED,=P'0'                                                     
         ZAP   MAXDUMP,=P'100000'                                               
         LA    R1,DISKPS                                                        
         ST    R1,ADCB                                                          
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
         SPACE 1                                                                
         CLC   CARD(6),=C'DSORG='                                               
         BNE   IN4                                                              
         MVC   DSO,CARD+6                                                       
         LA    R1,DISKIS                                                        
         CLC   CARD+6(2),=C'IS'                                                 
         BE    *+8                                                              
         LA    R1,DISKPS                                                        
         ST    R1,ADCB                                                          
         B     IN2                                                              
         SPACE 1                                                                
IN4      CLC   CARD(8),=C'RECORDS='                                             
         BNE   IN2                                                              
         CLC   CARD+8(3),=C'ALL'   USER WANTS ALL RECORDS                       
         BNE   *+14                                                             
         MVC   MAXDUMP(3),=C'ALL'                                               
         B     IN2                                                              
         LA    R1,CARD+8                                                        
         LA    R0,6                                                             
IN4B     CLI   0(R1),C' '                                                       
         BE    IN4D                                                             
         CLI   0(R1),C'0'                                                       
         BL    IN2                 IGNORE BAD CARDS                             
         LA    R1,1(R1)                                                         
         BCT   R0,IN4B                                                          
IN4D     LA    R2,CARD+8                                                        
         SR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     IN2                                                              
         PACK  MAXDUMP,CARD+8(0)                                                
         SPACE 1                                                                
INX      DS    0H                                                               
         GOTO1 =V(PRINT),DMCB,SPACES,=C'BC01'                                   
         EJECT                                                                  
*              READ THE FILE AND PRINT SOME RECORDS                             
         SPACE 1                                                                
         USING IHADCB,R2                                                        
         L     R2,ADCB                                                          
         OPEN  ((R2),(INPUT))      OPEN THE FILE                                
         SPACE 1                                                                
GET2     GET   (R2),IO             GET A RECORD                                 
         LH    R3,DCBLRECL         FIXED RECORD LENGTH                          
         CLI   DSO,C'I'            ALWAYS FOR IS                                
         BE    GET4                                                             
         TM    DCBRECFM,DCBRECF    TEST FIXED RECORD LENGTH                     
         BO    GET4                                                             
         TM    DCBRECFM,DCBRECV    ELSE CHECK FOR VARIABLE                      
         BNO   GET4                                                             
         LH    R3,IO               VARIABLE RECORD LENGTH                       
         SPACE 1                                                                
GET4     AP    DUMPED,=P'1'                                                     
         EDIT  DUMPED,(5,P),ALIGN=LEFT,WRK=CARD                                 
         GOTO1 =V(PRNTBL),DMCB,(5,P),IO,C'DUMP',(R3),=C'2D',           X        
               (C'P',V(PRINT))                                                  
         CLC   MAXDUMP(3),=C'ALL'                                               
         BE    GET2                                                             
         CP    DUMPED,MAXDUMP                                                   
         BL    GET2                                                             
         SPACE 1                                                                
EOJ      CLOSE (R2)                CLOSE THE FILE                               
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         XBASE                                                                  
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
DMPLIST  DC    6F'0'                                                            
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**DCBPS*'                                                      
DISKPS   DCB   DDNAME=DISKPS,DSORG=PS,EODAD=EOJ,MACRF=GM                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**DCBIS*'                                                      
DISKIS   DCB   DDNAME=DISKIS,DSORG=IS,EODAD=EOJ,MACRF=GM                        
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DS    10000C                                                           
         EJECT                                                                  
*              DSECT TO COVER LOCAL STORAGE                                     
         SPACE 1                                                                
DUMPD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
ADCB     DS    A                                                                
CARD     DS    CL80                                                             
DSO      DS    CL1                                                              
MAXDUMP  DS    PL5                                                              
DUMPED   DS    PL5                                                              
         DS    CL1                 (P-1)                                        
P        DS    CL132                                                            
SPACES   DS    CL132                                                            
DUMPX    EQU   *                                                                
         EJECT                                                                  
         PRINT GEN                                                              
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT NOGEN                                                            
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDDUMPER  08/28/98'                                      
         END                                                                    
