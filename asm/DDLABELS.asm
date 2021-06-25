*          DATA SET DDLABELS   AT LEVEL 016 AS OF 05/01/02                      
*PHASE LABELS,*,NOAUTO                                                          
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE REGSAVE                                                                
         TITLE 'MODULE TO PRINT STICKY LABELS'                                  
ACLBLS   CSECT                                                                  
         NBASE 0,ACLBLS**,=V(REGSAVE),RA,R9,RR=R8                               
         ST    R8,RELO                                                          
         B     *+8                                                              
RELO     DS    F                                                                
*                                                                               
         EJECT                                                                  
*              CHECK AN INPUT CARD FOR TYPE OF FILE                             
         SPACE 3                                                                
         GOTO1 =V(CARDS),PARA,C,=C'RE00',RR=RELO                                
         LA    R8,TAPE                                                          
         LA    R7,D                                                             
         CLC   C(2),=C'/*'                                                      
         BE    OPEN                                                             
         CLC   C(10),=C'FIXED TAPE'                                             
         BE    OPEN                                                             
         GOTO1 =V(LOGIO),PARA,1,=C'INVALID FILE CARD',RR=RELO                   
         ABEND 0                                                                
         SPACE 2                                                                
OPEN     OPEN  (TAPE,(INPUT))                                                   
         SR    R2,R2                                                            
         BAS   RE,LINEUP                                                        
         EJECT                                                                  
*              READ FILE AND WRITE STICKY LABELS                                
         SPACE 3                                                                
LOOP     GET   (R8),(R7)                                                        
         CLC   D(4),=X'FFFFFFFF'                                                
         BE    EOF                                                              
         LR    R3,R2                                                            
         MH    R3,=H'41'                                                        
         LA    R3,P(R3)                                                         
         LA    R4,D                                                             
         LA    R5,7                                                             
         SPACE 2                                                                
LOOP2    MVC   1(38,R3),0(R4)                                                   
         LA    R3,132(R3)                                                       
         LA    R4,38(R4)                                                        
         BCT   R5,LOOP2                                                         
         LA    R2,1(R2)                                                         
         CH    R2,=H'3'            IS THIS THE THIRD LABEL                      
         BNE   LOOP                                                             
         SR    R2,R2                                                            
         SPACE 2                                                                
LOOP3    LA    R3,P                GO AND PRINT THREE                           
         LA    R4,6                                                             
         SPACE 2                                                                
LOOP4    GOTO1 =V(PRINT),PARA,(R3),=C'BL01',RR=RELO                             
         MVI   0(R3),C' '                                                       
         MVC   1(131,R3),0(R3)                                                  
         LA    R3,132(R3)                                                       
         BCT   R4,LOOP4                                                         
         GOTO1 =V(PRINT),PARA,(R3),=C'BL03',RR=RELO                             
         MVI   0(R3),C' '                                                       
         MVC   1(131,R3),0(R3)                                                  
         LTR   R2,R2                                                            
         BE    LOOP                                                             
         BR    R6                                                               
         EJECT                                                                  
*              LINE-UP AND LAST TIME                                            
         SPACE 3                                                                
LINEUP   NTR                                                                    
         GOTO1 =V(PRINT),PARA,P,=C'BC01',RR=RELO                                
         MVI   P+1,C'*'                                                         
         MVC   P+2(37),P+1                                                      
         MVC   P+42(38),P+1                                                     
         MVC   P+83(38),P+1                                                     
         LA    R3,P                                                             
         LA    R4,6                                                             
         SPACE 2                                                                
LINEUP2  MVC   132(132,R3),0(R3)                                                
         LA    R3,132(R3)                                                       
         BCT   R4,LINEUP2                                                       
         LA    R2,4                                                             
         BAS   R6,LOOP3                                                         
         XIT                                                                    
         SPACE 2                                                                
EOF      LTR   R2,R2                                                            
         BZ    CLOSE                                                            
         LA    R2,4                                                             
         BAS   R6,LOOP3            GO AND PRINT ANY LEFTOVERS                   
         SPACE 2                                                                
CLOSE    CLOSE  ((R8),)                                                         
         XBASE                                                                  
         EJECT                                                                  
*              DTF FOR FIXED BLOCKED INPUT (TAPE)                               
         SPACE 3                                                                
TAPE     DCB   DDNAME=TAPE,                                            X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00266,                                            X        
               BLKSIZE=02660,                                          X        
               MACRF=GM                                                         
********                                                                        
         EJECT                                                                  
*              WORK SPACE FOR PROGRAM                                           
         SPACE 3                                                                
PARA     DS    6F                                                               
C        DS    CL80                                                             
         DS    F                                                                
D        DS    300C                                                             
P        DC    7CL132' '                                                        
IOA      DS    3008C                                                            
IOB      DS    3008C                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DDLABELS  05/01/02'                                      
         END                                                                    
         EJECT                                                                  
