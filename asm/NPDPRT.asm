*          DATA SET NPDPRT     AT LEVEL 025 AS OF 05/01/02                      
*PHASE NPDPRT,*,NOAUTO                                                          
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'NPDPRT'                                                         
         PRINT NOGEN                                                            
NPDPRT   CSECT                                                                  
         NBASE 0,NPDPRT,=V(REGSAVE)                                             
         SPACE 2                                                                
         BAS   RE,PRNT                                                          
         OPEN  (IN,(INPUT))                                                     
*                                                                               
START1   DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'COUNT=',CARD                                                  
         BNE   START2                                                           
         PACK  COUNT,CARD+6(5)                                                  
         B     START1                                                           
*                                                                               
START2   DS    0H                                                               
         CLC   =C'REC=',CARD                                                    
         BNE   START3                                                           
         MVC   RTYP,CARD+4                                                      
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
START10  DS    0H                                                               
         BAS   RE,PRNT                                                          
         ZAP   TOT1,=P'0'                                                       
         ZAP   TOT2,=P'0'                                                       
*                                                                               
GET      DS    0H                                                               
         CLI   EOFSW,C'Y'                                                       
         BE    PR12                                                             
*                                                                               
         GET   IN,REC                                                           
         CLI   RTYP,C' '                                                        
         BE    GET2                                                             
         CLC   RTYP,REC+114                                                     
         BNE   GET                                                              
*                                                                               
GET2     DS    0H                                                               
         AP    INCNT,=P'1'                                                      
         CP    INCNT,COUNT                                                      
         BH    EOF                                                              
*                                                                               
         MVC   P+001(03),REC+000   DMA                                          
         MVC   P+005(04),REC+089   START TIME                                   
         MVC   P+010(04),REC+093   END TIME                                     
         MVC   P+015(02),REC+097   AM/PM                                        
         MVC   P+018(04),REC+040   PROGRAM NUMBER                               
         MVC   P+023(25),REC+044   PROGRAM NAME                                 
         MVC   P+049(09),REC+099   DAYS                                         
         MVC   P+059(01),REC+108   DAY CODE                                     
         MVC   P+061(03),REC+111   QH'S                                         
         MVC   P+065(01),REC+114   RECORD TYPE                                  
         CLI   REC+114,C'3'                                                     
         BE    GET5                                                             
         MVC   P+067(07),REC+121   HH UNIV                                      
         MVC   P+077(07),REC+247   HH AUD                                       
         MVC   P+087(07),REC+366   V0611 AUD                                    
         MVC   P+095(04),REC+373   STATION                                      
         MVC   P+101(26),REC+3      MARKET NAME                                 
*                                                                               
         PACK  DUB,REC+247(7)                                                   
         AP    TOT1,DUB                                                         
         PACK  DUB,REC+366(7)                                                   
         AP    TOT2,DUB                                                         
         B     GET6                                                             
*                                                                               
GET5     DS    0H                                                               
         MVC   P+075(09),REC+177   US HH AUD                                    
         MVC   P+085(09),REC+331   US V0611 AUD                                 
         BAS   RE,PRNT                                                          
*                                                                               
         OI    TOT1+6,X'0F'                                                     
         UNPK  P+075(09),TOT1                                                   
         OI    TOT2+6,X'0F'                                                     
         UNPK  P+085(09),TOT2                                                   
         MVI   P+074,C'*'                                                       
         MVI   P+084,C'*'                                                       
*                                                                               
         ZAP   TOT1,=P'0'                                                       
         ZAP   TOT2,=P'0'                                                       
*                                                                               
GET6     DS    0H                                                               
         BAS   RE,PRNT                                                          
*                                                                               
         B     GET                                                              
*                                                                               
EOF      DS    0H                                                               
         CLOSE (IN,)                                                            
         MVI   EOFSW,C'Y'                                                       
*                                                                               
PR12     DS    0H                                                               
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
XIT      XIT1                                                                   
         SPACE 3                                                                
IN       DCB   DDNAME=NPDTAP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
         SPACE 3                                                                
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
COUNT    DC    PL5'999999'                                                      
RTYP     DC    C' '                                                             
CARD     DS    CL80                                                             
*                                                                               
TOT1     DS    PL7                                                              
TOT2     DS    PL7                                                              
*                                                                               
*                                                                               
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    5000C                                                            
         DS    D                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NPDPRT    05/01/02'                                      
         END                                                                    
