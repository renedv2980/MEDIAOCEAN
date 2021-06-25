*          DATA SET PUBCOPYNW  AT LEVEL 014 AS OF 05/01/02                      
*PHASE PUBCOPYN,*,NOAUTO                                                        
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE RECUP                                                                  
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'PURGEON - PRT/PUB FILE PEEL-OFF'                                
*                                                                               
*        SPECIAL PUBCOPY FOR ATT - NW                                           
*        ALSO DELETES 'FIRST TRY' AO PUBS                                       
*                                                                               
PURGEON  CSECT                                                                  
         NBASE 0,PURGEON,=V(REGSAVE)                                            
         SPACE 2                                                                
         BAS   RE,PRNT                                                          
*&&DO                                                                           
         OPEN  IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*&&                                                                             
*                                                                               
         LA    R4,AGMLST                                                        
         BAS   RE,CARDS                                                         
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
START1   DS    0H                                                               
         BAS   RE,CARDS                                                         
START2   DS    0H                                                               
         CLC   =C'/*',CARD                                                      
         BE    GET                                                              
         LA    R0,AGMLSTX                                                       
         CR    R4,R0                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(6,R4),CARD                                                     
         LA    R4,6(R4)                                                         
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
         B     START1                                                           
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
         CLI   EOFSW,C'Y'                                                       
         BE    EOJ                                                              
*                                                                               
         CLC   REC+7(2),=C'AO'   DON'T COPY 'BAD' AO PUBS                       
         BNE   GET1                                                             
         AP    DELCNT,=P'1'                                                     
         B     GET                                                              
*                                                                               
GET1     LA    R4,AGMLST                                                        
         MVC   WORK(2),REC+7                                                    
         MVC   WORK+2(1),REC                                                    
GET2     DS    0H                                                               
         CLC   WORK(3),0(R4)                                                    
         BE    GET4                                                             
         CLI   0(R4),0                                                          
         BE    PUT                                                              
         LA    R4,6(R4)                                                         
         B     GET2                                                             
GET4     DS    0H                                                               
         CLI   3(R4),C' '                                                       
         BE    GET                                                              
         BAS   RE,PUTREC                                                        
         MVC   REC(1),5(R4)                                                     
         MVC   REC+7(2),3(R4)                                                   
         CLC   0(2,R4),3(R4)       IF TO SAME AGENCY                            
         BE    GET6                COPY EVERYTHING                              
*                                                                               
         CLI   REC+9,X'81'      NO NEED TO COPY LITTLE REC                      
         BNE   GET85                                                            
         AP    COPYCNT,=P'1'                                                    
*                                                                               
         LA    R2,REC+33                                                        
GET4B    DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    GET6                                                             
         CLI   0(R2),X'08'                                                      
         BE    GET5                                                             
         CLI   0(R2),X'09'                                                      
         BE    GET5                                                             
         CLI   0(R2),X'0A'                                                      
         BE    GET5                                                             
         CLI   0(R2),X'0B'                                                      
         BE    GET5                                                             
         CLI   0(R2),X'14'      COPY CLT ELEMS                                  
         BE    GET5             FOR 'ALL' AND 'ZY '                             
         CLI   0(R2),X'80'                                                      
         BE    GET5B                                                            
GET4D    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GET4B                                                            
*                                                                               
GET5     DS    0H                                                               
         CLC   2(3,R2),=3X'FF'          ALL CLTS                                
         BE    GET4D                                                            
         CLC   2(3,R2),=C'ZY '          OR AOR CLT ZY                           
         BE    GET4D                                                            
GET5B    DS    0H                                                               
         GOTO1 =V(RECUP),DMCB,(1,REC),(R2)                                      
*                                                                               
         B     GET4B                                                            
GET6     DS    0H                                                               
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
         EJECT                                                                  
*                                                                               
GET85    DS    0H                                                               
         CLI   REC+9,X'85'           SEE IF LITTLE REC                          
         BNE   GET                                                              
         MVI   SW21,0                                                           
         LA    R2,REC+33                                                        
*                                                                               
GET85B   CLI   0(R2),0               END OF REC                                 
         BE    GET86                                                            
         CLI   0(R2),X'71'           DELETE ALL DRD ELEMS                       
         BE    GET85D                                                           
         CLI   0(R2),X'21'          SEE IF RECORD HAD SUPPLEMENTAL              
         BNE   GET85C              PRODUCTION ELEM                              
         MVI   SW21,C'Y'           SO I'LL COPY IT                              
         B     GET85C                                                           
*                                                                               
GET85C   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GET85B                                                           
*                                                                               
GET85D   DS    0H                                                               
         GOTO1 =V(RECUP),DMCB,(1,REC),(R2)                                      
         B     GET85B                                                           
*                                                                               
GET86    DS    0H                                                               
         CLI   SW21,C'Y'                                                        
         BNE   GET                                                              
         AP    LTLCNT,=P'1'                                                     
         B     PUT                                                              
*                                                                               
PUT      DS    0H                                                               
         BAS   RE,PUTREC                                                        
         B     GET                                                              
         SPACE 2                                                                
*&&DO                                                                           
EOF      CLOSE IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (IN,,OUT,)                                                       
*&&                                                                             
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
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R2,4(R2)                                                         
         LA    R3,0(R5,R2)         EOR                                          
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   XIT                                                              
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         B     XIT                                                              
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         LA    R2,KLEN                                                          
         GOTO1 =V(HEXOUT),DMCB,(R5),P+01,(R2),=C'N'                             
*                                                                               
         MVC   WORK(KLEN),0(R5)                                                 
         TR    WORK(KLEN),TRTAB                                                 
         MVC   P+75(KLEN),WORK                                                  
         B     XIT                                                              
         SPACE 3                                                                
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         AP    INCNT,=P'1'                                                      
*                                                                               
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
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*&&DO                                                                           
IN       DTFMT BLKSIZE=8400,RECFORM=VARBLK,TYPEFLE=INPUT,              X        
               IOAREA1=IN1,DEVADDR=SYS010,FILABL=STD,WORKA=YES,        X        
               EOFADDR=EOF                                                      
*&&                                                                             
*&&OS                                                                           
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*&&                                                                             
         EJECT                                                                  
*&&DO                                                                           
OUT      DTFMT BLKSIZE=8000,RECFORM=VARBLK,TYPEFLE=OUTPUT,             X        
               IOAREA1=OUT1,DEVADDR=SYS011,FILABL=STD,WORKA=YES                 
*&&                                                                             
*&&OS                                                                           
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
SW21     DC    X'00'                                                            
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
AGMLST   DC    1200X'00'                                                        
AGMLSTX  EQU   *-1                                                              
         DC    6X'00'                                                           
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
DELCNT   DC    PL5'0',CL20'DELETE COUNT'                                        
COPYCNT  DC    PL5'0',CL20'PUBS COPIED- 81'                                     
LTLCNT   DC    PL5'0',CL20'LTL RECS COPIED- 85'                                 
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4100C                                                            
         DS    D                                                                
*&&DO                                                                           
IN1      DS    8500C                                                            
*&&                                                                             
*&&DO                                                                           
OUT1     DS    8500C                                                            
*&&                                                                             
         SPACE 3                                                                
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
*                                                                               
BSTAB    CSECT                                                                  
         DS    1000C                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PUBCOPYNW 05/01/02'                                      
         END                                                                    
