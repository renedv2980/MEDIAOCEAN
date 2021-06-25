*          DATA SET DDMULTOQ   AT LEVEL 013 AS OF 05/01/02                      
*PHASE MULTOQ,*,NOAUTO                                                          
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE DATCON                                                                 
         TITLE 'MULTOQ - MULTIPLE TAPES TO PRINTQ'                              
MULTOQ   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**MTPQ**,=A(MYSAVE)                                            
         LA    R8,TIN                                                           
         OPEN  ((R8),(INPUT))                                                   
         LA    R9,DUMREM                                                        
         USING REMOTED,R9                                                       
         MVC   REMOTSYS(3),=C'TAP' PRESET PRINTQ DEFAULTS                       
         MVC   REMOTFRM,=CL4'1S'                                                
         MVC   REMOTJID,=C'TAP'                                                 
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'A'                                                    
         MVC   REMOTABF,=V(PQBUFF)                                              
         MVC   REMOTADM,=V(DATAMGR)                                             
         MVC   REMOTAOP,=V(PQOPEN)                                              
         EJECT                                                                  
*              HANDLE THE CONTROL CARDS                                         
         SPACE 3                                                                
CARD     GOTO1 =V(CARDS),PARA,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    CARDEOF                                                          
         MVC   P(80),C                                                          
         GOTO1 =V(PRINT),PARA,P-1,=C'BL01'                                      
         CLC   C(3),=C'ID='                                                     
         BNE   CARD2                                                            
         PACK  DUB,C+3(5)                                                       
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   REMOTDST,DUB                                                     
         B     CARD                                                             
         SPACE 2                                                                
CARD2    CLC   C(5),=C'FORM='                                                   
         BNE   CARD4                                                            
         MVC   REMOTFRM,C+5                                                     
         MVC   BASICFRM,C+5                                                     
         B     CARD                                                             
         SPACE 2                                                                
CARD4    CLC   C(6),=C'CLASS='                                                  
         BNE   CARD6                                                            
         MVC   REMOTCLS,C+6                                                     
         MVC   BASICCLS,C+6                                                     
         B     CARD                                                             
         SPACE 2                                                                
CARD6    CLC   C(7),=C'REPORT='                                                 
         BNE   CARD8                                                            
         MVC   REMOTSYS(3),C+7                                                  
         MVC   REMOTJID,C+7                                                     
         B     CARD                                                             
         SPACE 2                                                                
CARD8    CLC   C(09),=C'PRINT=YES'                                              
         BNE   CARD10                                                           
         MVI   REMOTPRT,1                                                       
         B     CARD                                                             
         SPACE 2                                                                
CARD10   CLC   C(8),=C'QUEUE=NO'                                                
         BNE   CARD12                                                           
         XC    DUMREM,DUMREM                                                    
         B     CARD                                                             
         SPACE 2                                                                
CARD12   B     CARD                                                             
         SPACE 2                                                                
CARDEOF  MVC   P,SPACES                                                         
         L     R9,=V(REMOTEC)                                                   
         MVC   0(36,R9),DUMREM                                                  
         EJECT                                                                  
*              HANDLE THE TAPE                                                  
         SPACE 3                                                                
TAPE     GET   (R8),T              GET FIRST LABEL                              
         BAS   RE,LABEL                                                         
         SPACE 1                                                                
TAPE2    GET   (R8),T                                                           
         CLI   T,C'X'              XRECORDS ARE LABELS                          
         BE    EOF2                SO END PREVIOUS FILE                         
         BAS   RE,OUT                                                           
         B     TAPE2                                                            
         SPACE 1                                                                
EOF      MVI   EOFSW,C'Y'          REAL EOF                                     
         SPACE 1                                                                
EOF2     MVC   P,SPACES                                                         
         L     R9,=V(REMOTEC)                                                   
         MVC   DUMREM,0(R9)                                                     
         GOTO1 =V(PRINT),PARAS,=C'CLOSE'                                        
         MVC   0(36,R9),DUMREM                                                  
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         EDIT  (P6,PAGCNT),(6,P)                                                
         MVC   P+7(5),=C'PAGES'                                                 
         GOTO1 =V(PRINT),PARAS,P-1,=C'BL01'                                     
         EDIT  (P6,LINCNT),(6,P)                                                
         MVC   P+7(5),=C'LINES'                                                 
         GOTO1 =V(PRINT),PARAS,P-1,=C'BL01'                                     
         GOTO1 =V(PRINT),PARAS,=C'CLOSE'                                        
         MVC   0(36,R9),DUMREM                                                  
         CLI   EOFSW,C'Y'                                                       
         BNE   EOF6                                                             
         CLOSE ((R8))                                                           
         XBASE                                                                  
         SPACE 1                                                                
EOF6     BAS   RE,LABEL                                                         
         ZAP   PAGCNT,=P'0'                                                     
         ZAP   LINCNT,=P'0'                                                     
         B     TAPE2                                                            
         EJECT                                                                  
*              HANDLE LABELS                                                    
         SPACE 3                                                                
LABEL    NTR1                                                                   
         SPACE 1                                                                
         MVC   REPID,T+1           REPORT                                       
         MVC   DEST,T+6            OFFICE                                       
         LA    R2,DESTLIST                                                      
         SPACE 1                                                                
LABEL6   CLI   0(R2),X'FF'                                                      
         BE    LABEL8                                                           
         CLC   0(2,R2),DEST                                                     
         BE    LABEL8                                                           
         LA    R2,4(R2)                                                         
         B     LABEL6                                                           
         SPACE 2                                                                
LABEL8   MVC   DESTID,2(R2)                                                     
         OC    REMOTKEY,REMOTKEY                                                
         BNZ   LABEL10                                                          
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PARA,P-1,=C'BC01'                                      
         MVC   P,T                                                              
         GOTO1 =V(PRINT),PARAS,P-1,=C'BL02'                                     
         MVC   P,SPACES                                                         
         MVC   P(6),=C'OFFICE'                                                  
         MVC   P+10(2),DEST                                                     
         EDIT  (2,DESTID),(4,P+20)                                              
         GOTO1 =V(PRINT),PARAS,P-1,=C'BL01'                                     
         MVC   P,SPACES                                                         
         B     XIT                                                              
         SPACE 2                                                                
LABEL10  MVC   REMOTDST,DESTID                                                  
         MVC   REMOTJID,=C'NRM'                                                 
         MVC   REMOTSYS(4),REPID                                                
         MVI   REMOTLPP,68                                                      
         MVC   REMOTFRM,BASICFRM                                                
         MVC   REMOTCLS,BASICCLS                                                
         CLC   REPID(4),=C'RANK'                                                
         BNE   XIT                                                              
         MVC   REMOTJID,=C'RNK'                                                 
         MVI   REMOTLPP,51                                                      
         MVI   REMOTCLS,C'R'                                                    
         MVC   REMOTFRM,=C'RNK '                                                
         B     XIT                                                              
         SPACE 1                                                                
DESTLIST DS    0H                                                               
         DC    C'NY',AL2(80)        NEW YORK                                    
         DC    C'AT',AL2(71)        ATLANTA                                     
         DC    C'BO',AL2(72)        BOSTON                                      
         DC    C'CH',AL2(73)        CHICAGO                                     
         DC    C'DA',AL2(75)        DALLAS                                      
         DC    C'DE',AL2(76)        DETROIT                                     
         DC    C'LA',AL2(78)        LA                                          
         DC    C'MN',AL2(79)        MINNEAPOLIS                                 
         DC    C'HO',AL2(497)       HOUSTON                                     
         DC    C'PH',AL2(81)        PHILADELPHIA                                
         DC    C'SL',AL2(85)        ST LOUIS                                    
         DC    C'ST',AL2(85)        ST LOUIS                                    
         DC    C'SF',AL2(84)        SAN FRSNCISCO                               
         DC    C'SE',AL2(83)        SEATTLE                                     
         DC    X'FFFF',AL2(80)                                                  
         EJECT                                                                  
*              HANDLE PRINT LINES                                               
         SPACE 3                                                                
OUT      NTR1                                                                   
         MVC   P(132),SPACES                                                    
         CLI   T,C'1'              CHECK FIRST CHARACTER FOR CONTROL            
         BNE   OUT2                SKIP TO CHANNEL 1                            
         GOTO1 =V(PRINT),PARAS,P-1,=C'BC01'                                     
         AP    PAGCNT,=P'1'                                                     
         B     OUTEND                                                           
         SPACE 1                                                                
OUT2     CLI   T,C' '              SPACE BEFORE PRINTING                        
         BNE   OUTEND                                                           
         GOTO1 =V(PRINT),PARAS,P-1,=C'BL01'                                     
         AP    LINCNT,=P'1'                                                     
         SPACE 1                                                                
OUTEND   MVC   P,T+1                                                            
         GOTO1 =V(PRINT),PARAS,P-1,=C'BL00'                                     
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
DUB      DS    D                                                                
WORK     DS    CL32                                                             
C        DS    CL80                                                             
PFILL    DS    CL1                                                              
P        DS    CL132                                                            
PARAS    DS    6F                                                               
PARA     DS    6F                                                               
SPACES   DC    CL132' '                                                         
ANYDATA  DC    C'N'                                                             
COMMAND  DS    CL4                                                              
LABCNT   DC    PL2'0'                                                           
PAGCNT   DC    PL6'0'                                                           
LINCNT   DC    PL6'0'                                                           
DUMREM   DC    XL48'00'                                                         
REPID    DC    C'    '                                                          
DEST     DC    C'  '                                                            
DESTID   DC    H'384'                                                           
BASICFRM DC    C'1PP '                                                          
BASICCLS DC    C'A'                                                             
LABSW    DC    X'00'                                                            
SAVKEY   DS    CL32                                                             
NFIL     DC    F'0'                                                             
EOFSW    DC    C'N'                                                             
         SPACE 2                                                                
         LTORG                                                                  
T        DS    136C                                                             
         EJECT                                                                  
*        DCBS                                                                   
         SPACE 1                                                                
         DS    0D                                                               
TIN      DCB   DDNAME=TIN,                                             X        
               DSORG=PS,                                               X        
               RECFM=BF,                                               X        
               LRECL=136,                                              X        
               BLKSIZE=06800,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         SPACE 2                                                                
IOA      CSECT                                                                  
         DS    2000C                                                            
         SPACE 2                                                                
UTL      CSECT                                                                  
         DC    F'0'                                                             
         DC    X'0A'                                                            
         DC    X'000000'                                                        
MYSAVE   CSECT                                                                  
         DS    20000C                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013DDMULTOQ  05/01/02'                                      
         END                                                                    
