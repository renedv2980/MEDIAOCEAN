*          DATA SET SMRBPRT    AT LEVEL 010 AS OF 05/01/02                      
*PHASE SMRBPRT,*,NOAUTO                                                         
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CBTBL                                                                  
*INCLUDE REGSAVE                                                                
         TITLE 'SMRBPRT'                                                        
         PRINT NOGEN                                                            
SMRBPRT  CSECT                                                                  
         NBASE 0,SMRBPRT,=V(REGSAVE)                                            
         SPACE 2                                                                
         BAS   RE,PRNT                                                          
         OPEN  (IN,(INPUT))                                                     
*                                                                               
START1   DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'COUNT=',CARD                                                  
         BNE   START2                                                           
         PACK  COUNT,CARD+6(4)                                                  
         B     START9                                                           
START2   DS    0H                                                               
         CLC   =C'BINARY=',CARD                                                 
         BNE   START3                                                           
         MVC   BINSW,CARD+7                                                     
         B     START9                                                           
*                                                                               
START3   DS    0H                                                               
         B     START1                                                           
*                                                                               
START9   DS    0H                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
         B     START1                                                           
*                                                                               
START10  DS    0H                                                               
         BAS   RE,PRNT                                                          
*                                                                               
GET      DS    0H                                                               
         GET   IN,REC                                                           
*                                                                               
         AP    INCNT,=P'1'                                                      
         CP    INCNT,COUNT                                                      
         BH    EOF                                                              
*                                                                               
         MVC   P+1(3),=C'REC'                                                   
         EDIT  (P5,INCNT),(5,P+5)                                               
         MVI   REC+10,C'='                                                      
         GOTO1 CVEBC,DMCB,(80,REC),P+11                                         
         BAS   RE,PRNT                                                          
*                                                                               
         CLI   BINSW,C'Y'                                                       
         BNE   *+12                                                             
         BAS   RE,DMPREC                                                        
         BAS   RE,PRNT                                                          
*                                                                               
         B     GET                                                              
*                                                                               
EOF      DS    0H                                                               
         CLOSE (IN,)                                                            
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
         MVC   P+01(16),WORK+00                                                 
         MVC   P+18(16),WORK+16                                                 
         MVC   P+36(16),WORK+32                                                 
         MVC   P+54(16),WORK+48                                                 
         MVC   P+72(16),WORK+64                                                 
         MVC   P+90(16),WORK+80                                                 
         BAS   RE,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
         EJECT                                                                  
*                                  CONVERT 360 COLUMN BINARY TO EBCDIC          
*                                  PARAM1 =A(INPUT) - BYTE 0 = LENGTH           
*                                  PARAM2 =A(OUTPUT)                            
         SPACE 2                                                                
CVEBC    NTR1                                                                   
         SPACE 2                                                                
         LM    R2,R3,0(R1)         R2 = A(INPUT)                                
*                                  R3 = A(OUTPUT)                               
         ZICM  R0,0(R1),(1)        R0 = LENGTH OF INPUT (POSITIONS)             
         BZ    CVEX                                                             
*                                                                               
*                                                                               
CVE4     DS    0H                                                               
         ZIC   R4,0(R2)            6 BITS FROM BYTE 1                           
         SLL   R4,6                                                             
         ZIC   R5,1(R2)            6 BITS FROM BYTE 2                           
         OR    R5,R4               R5 = 12 BIT RESULT                           
*                                                                               
         A     R5,=V(CBTBL)        DISPLACE INTO TRANSLATE TABLE                
         MVC   0(1,R3),0(R5)       SET IN OUTPUT                                
*                                                                               
         LA    R2,2(R2)            NEXT POSITION                                
         LA    R3,1(R3)                                                         
         BCT   R0,CVE4                                                          
*                                                                               
CVEX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00160,                                            X        
               BLKSIZE=01600,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
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
INCNT    DC    PL5'0'                                                           
EOFSW    DC    C'N'                                                             
BINSW    DS    C                                                                
COUNT    DC    PL5'4950'                                                        
CARD     DS    CL80                                                             
*                                                                               
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    160C                                                             
         DS    D                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SMRBPRT   05/01/02'                                      
         END                                                                    
