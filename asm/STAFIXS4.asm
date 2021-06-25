*          DATA SET STAFIXS4   AT LEVEL 008 AS OF 07/09/96                      
*PHASE STAFIXS4                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'STAFIXS4 - COPY MI X POINTERS TO S4'                            
STACOPY  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,STAFIX,=V(REGSAVE)                                             
         SPACE 2                                                                
*                                                                               
         OPEN  (IN,(INPUT))                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (OUT1,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (OUT2,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GET      DS    0H                                                               
         GET   IN,REC1-4                                                        
         AP    INCNT,=P'1'                                                      
*                                                                               
         CLI   REC1,C'X'                                                        
         BNE   PUT1                                                             
*                                                                               
         CLC   =C'XS4',REC1        DROP XS4 RECORDS                             
         BE    GET10                                                            
*                                                                               
         CLC   =C'XMI',REC1                                                     
         BNE   PUT1                                                             
*                                                                               
GET2     PUT   OUT1,REC1-4         XMI REC1 TO OUT1                             
*                                                                               
         MVC   REC1(3),=C'XS4'     CLONE TO OUT2 FOR XS4                        
         PUT   OUT2,REC1-4                                                      
         B     GET                                                              
*                                                                               
* GET HERE FOR FIRST XS4 RECORD                                                 
*                                                                               
GET10    MVC   P(20),=C'GOT FIRST XS4 RECORD'                                   
         GOTO1 =V(PRINT),DMCB,P,=C'BL01'                                        
*                                                                               
GET12    GET   IN,REC1-4                                                        
         AP    XS4CNT,=P'1'                                                     
         CLC   =C'XS4',REC1                                                     
         BE    GET12                                                            
* NOW READ THE CLONED RECORDS ON OUT2                                           
         MVC   P(20),=C'READ ALL XS4 RECORDS'                                   
         GOTO1 =V(PRINT),DMCB,P,=C'BL01'                                        
*                                                                               
         CLOSE OUT2                                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (OUT2,(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   P(20),=C'XS4 RECORDS GOING OUT'                                  
         GOTO1 =V(PRINT),DMCB,P,=C'BL01'                                        
*                                                                               
GET14    DS    0H                                                               
         GET   OUT2,REC2-4                                                      
         MVC   P(17),=C'GOT AN XS4 RECORD'                                      
         GOTO1 =V(PRINT),DMCB,P,=C'BL01'                                        
         AP    OUT2CNT,=P'1'                                                    
         PUT   OUT1,REC2-4                                                      
         B     GET14                                                            
*                                                                               
GET10X   DS    0H                                                               
         CLOSE OUT2                                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUT1     PUT   OUT1,REC1-4                                                      
         AP    OUT1CNT,=P'1'                                                    
         B     GET                                                              
         SPACE 2                                                                
EOF      CP    OUT2CNT,=P'0'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE IN                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE OUT1                                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=32760,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
OUT1     DCB   DDNAME=OUT1,            DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=24000,                                          X        
               MACRF=PM                                                         
OUT2     DCB   DDNAME=OUT2,            DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=24000,                                          X        
               MACRF=(GM,PM),                                          X        
               EODAD=GET10X                                                     
*                                                                               
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
REPSW    DC    C'Y'                                                             
FILE     DS    C                                                                
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
OUT1CNT  DC    PL5'0',CL20'FILE 1 OUT'                                          
OUT2CNT  DC    PL5'0',CL20'FILE 2 OUT'                                          
XS4CNT   DC    PL5'0',CL20'XS4 DELETES'                                         
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
*                                                                               
         DS    0D                                                               
         DC    CL8'**REC1**'                                                    
         DS    F                                                                
REC1     DS    1000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**REC2**'                                                    
         DS    F                                                                
REC2     DS    1000C                                                            
         DS    D                                                                
         SPACE 3                                                                
         ORG   REC1                                                             
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008STAFIXS4  07/09/96'                                      
         END                                                                    
