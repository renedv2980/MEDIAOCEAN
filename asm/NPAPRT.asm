*          DATA SET NPAPRT     AT LEVEL 034 AS OF 05/01/02                      
*PHASE NPAPRT,*,NOAUTO                                                          
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'NPAPRT'                                                         
         PRINT NOGEN                                                            
NPAPRT   CSECT                                                                  
         NBASE 0,NPAPRT,=V(REGSAVE)                                             
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
         ZAP   TOT3,=P'0'                                                       
*                                                                               
GET      DS    0H                                                               
         CLI   EOFSW,C'Y'                                                       
         BE    PR12                                                             
*                                                                               
         GET   IN,REC                                                           
         CLI   RTYP,C' '                                                        
         BE    GET2                                                             
         CLC   RTYP,REC+3          SKIP RTYP RECS                               
         BE    GET                                                              
*                                                                               
GET2     DS    0H                                                               
         AP    INCNT,=P'1'                                                      
         CP    INCNT,COUNT                                                      
         BH    EOF                                                              
*                                                                               
GET3     DS    0H                                                               
         CLI   REC+3,C'1'          TEST US REC                                  
         BNE   GET3B                                                            
         CP    TOT1,=P'0'                                                       
         B     GET3B               **NOOP TOTALS**                              
         EDIT  (P7,TOT1),(6,P+067)                                              
         EDIT  (P7,TOT2),(6,P+074)                                              
         EDIT  (P7,TOT3),(6,P+081)                                              
         EDIT  (P7,TOT4),(6,P+088)                                              
         EDIT  (P7,TOT5),(6,P+095)                                              
         ZAP   TOT1,=P'0'                                                       
         ZAP   TOT2,=P'0'                                                       
         ZAP   TOT3,=P'0'                                                       
         ZAP   TOT4,=P'0'                                                       
         ZAP   TOT5,=P'0'                                                       
         BAS   RE,PRNT                                                          
*                                                                               
GET3B    DS    0H                                                               
         MVC   P+001(01),REC+003   RECORD NUMBER                                
         MVC   P+003(27),REC+008   PGM NAME                                     
         MVC   P+031(04),REC+036   PGM NUMBER                                   
         MVC   P+036(03),REC+043   NETWORK                                      
         CLI   REC+3,C'1'          TEST US REC                                  
         BNE   GET4                                                             
         MVC   P+040(07),REC+103   DAYS                                         
         MVC   P+048(03),REC+111   DUR                                          
         MVC   P+052(07),REC+100   TELS                                         
         MVC   P+068(03),REC+148   US HH RTGS                                   
         MVC   P+075(03),REC+157   US W18+                                      
         MVC   P+082(03),REC+184   US W1849                                     
         MVC   P+089(03),REC+193   US W2549                                     
         MVC   P+096(03),REC+211   US W2564                                     
         B     GET6                                                             
*                                                                               
GET4     DS    0H                                                               
         MVC   P+040(03),REC+046   MARKET NUMBER                                
         MVC   P+044(10),REC+052   MKT NAME                                     
         MVC   P+057(01),REC+091   SPILL?                                       
         MVC   P+068(03),REC+100   MKT HH                                       
         MVC   P+075(03),REC+126   MKT W18+                                     
         MVC   P+082(03),REC+159   MKT W1849                                    
         MVC   P+089(03),REC+170   MKT W2549                                    
         MVC   P+096(03),REC+192   MKT W2564                                    
         CLI   REC+3,C'2'          TOTAL MKT RECS                               
         BNE   GET6                                                             
         PACK  DUB,REC+103(5)                                                   
         AP    TOT1,DUB                                                         
         PACK  DUB,REC+129(5)                                                   
         AP    TOT2,DUB                                                         
         PACK  DUB,REC+250(5)                                                   
         AP    TOT3,DUB                                                         
*                                                                               
GET6     DS    0H                                                               
         BAS   RE,PRNT                                                          
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
IN       DCB   DDNAME=NPATST,                                          X        
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
TOT3     DS    PL7                                                              
*                                                                               
*                                                                               
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    5000C                                                            
         DS    D                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034NPAPRT    05/01/02'                                      
         END                                                                    
