*          DATA SET NRSCPY     AT LEVEL 002 AS OF 05/01/02                      
*PHASE NRSCPY,*,NOAUTO                                                          
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'NRSCPY'                                                         
         PRINT NOGEN                                                            
NRSCPY   CSECT                                                                  
         NBASE 0,NRSCPY,=V(REGSAVE)                                             
         SPACE 2                                                                
         BAS   RE,PRNT                                                          
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
START1   DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'COPY=',CARD                                                   
         BNE   START2                                                           
         PACK  CPYCNT,CARD+5(4)                                                 
         B     START1                                                           
START2   DS    0H                                                               
         CLC   =C'DUMP=',CARD                                                   
         BNE   START3                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
         B     START1                                                           
*                                                                               
START10  DS    0H                                                               
*                                                                               
GET      DS    0H                                                               
         GET   IN,REC                                                           
         CLI   EOFSW,C'Y'                                                       
         BE    EOJ                                                              
*                                                                               
         AP    INCNT,=P'1'                                                      
         CP    INCNT,CPYCNT                                                     
         BH    EOJ                                                              
*                                                                               
         BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
EOF      DS    0H                                                               
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
         MVI   EOFSW,C'Y'                                                       
         XIT1                      RETURN TO GETREC CALL                        
*                                                                               
EOJ      DS    0H                                                               
         CLOSE (IN,,OUT,)                                                       
         XBASE                                                                  
         SPACE 2                                                                
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
         SPACE 3                                                                
DMPREC   NTR1                                                                   
         LA    R5,REC                                                           
         LA    R3,160(R5)          EOR                                          
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   XIT                                                              
         CH    R4,=H'48'                                                        
         BNH   *+8                                                              
         LA    R4,48                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+00(16),WORK+00                                                 
         MVC   P+17(16),WORK+16                                                 
         MVC   P+35(16),WORK+32                                                 
         MVC   P+53(16),WORK+48                                                 
         MVC   P+71(16),WORK+64                                                 
         MVC   P+87(16),WORK+80                                                 
         BAS   RE,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         B     XIT                                                              
         SPACE 3                                                                
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
         BAS   RE,DMPREC                                                        
PUTREC2  DS    0H                                                               
         PUT   OUT,REC                                                          
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00160,                                            X        
               BLKSIZE=6560,                                           X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00160,                                            X        
               BLKSIZE=01600,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
         DS    0F                                                               
WORK     DS    CL256                                                            
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
DMPCNT   DC    PL5'396'                                                         
CPYCNT   DC    PL5'4950'                                                        
CARD     DS    CL80                                                             
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    160C                                                             
         DS    D                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NRSCPY    05/01/02'                                      
         END                                                                    
