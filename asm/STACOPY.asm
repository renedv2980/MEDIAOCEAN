*          DATA SET STACOPY    AT LEVEL 017 AS OF 03/20/12                      
*PHASE STACOPYA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE REGSAVE                                                                
         TITLE 'STACOPY- SPOT STATION FILE COPY PROGRAM'                        
STACOPY  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,STACOPY,=V(REGSAVE)                                            
         SPACE 2                                                                
         BAS   RE,PRNT                                                          
         OPEN  (IN,(INPUT),OUT1,(OUTPUT),OUT2,(OUTPUT))                         
*                                                                               
         SR    R0,R0                                                            
         LA    R1,AGYTAB                                                        
         SR    R2,R2                                                            
         LA    R3,6                                                             
         LA    R4,3                                                             
         LA    R5,100                                                           
         STM   R0,R5,BSPARS                                                     
*                                                                               
ST4      DS    0H                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    ST20                                                             
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
         CLC   =C'REPS=',CARD                                                   
         BNE   ST6                                                              
         MVC   REPSW,CARD+5                                                     
         B     ST4                                                              
*                                                                               
ST6      CLC   =C'CABLE=',CARD                                                  
         BNE   ST7                                                              
         MVC   CABLESW,CARD+6                                                   
         B     ST4                                                              
*                                                                               
ST7      CLC   =C'CLTS=',CARD                                                   
         BNE   ST8                                                              
         MVC   CLTSW,CARD+5                                                     
         B     ST4                                                              
*                                                                               
ST8      DS    0H                                                               
         MVC   X(6),CARD                                                        
         GOTO1 =V(BINSRCH),BSPARS,(1,X)                                         
         OC    BSPARS+1(3),BSPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   BSPARS,1                                                         
         BE    ST10                                                             
         MVC   P+1(16),=C'DUPLICATE AGENCY'                                     
         BAS   RE,PRNT                                                          
*                                                                               
ST10     DS    0H                                                               
         B     ST4                                                              
ST20     DS    0H                                                               
*                                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
         CLI   EOFSW,C'Y'                                                       
         BE    EOJ                                                              
*                                                                               
*              ADD CODE FOR THIS RUN HERE                                       
*                                                                               
         LA    R2,REC+7                                                         
         LA    R3,REC+1                                                         
         CLI   REC,C'A'            ADDR                                         
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+1                                                         
         LA    R3,REC+3                                                         
         CLI   REC,C'F'                                                         
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+1                                                         
         LA    R3,REC+3                                                         
         CLI   REC,C'L'                                                         
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+6                                                         
         LA    R3,REC+1                                                         
         CLI   REC,C'M'            MKT                                          
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+1                                                         
         LA    R3,REC+3                                                         
         CLI   REC,C'N'                                                         
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+5                                                         
         LA    R3,REC+1                                                         
         CLI   REC,C'R'            REP                                          
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+1                                                         
         LA    R3,REC+11                                                        
         CLI   REC,C'B'            REP NAME FOR BROWSE                          
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+7                                                         
         LA    R3,REC+1                                                         
         CLI   REC,C'S'            STA                                          
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+1                                                         
         LA    R3,=C'T'            X RECS HAVE NO MEDIA. USE T                  
         CLI   REC,C'X'                                                         
         BE    GET2                                                             
*                                                                               
         LA    R2,REC+7                                                         
         LA    R3,REC+1                                                         
         CLI   REC,C'Y'                                                         
         BE    GET2                                                             
*                                                                               
         MVI   FILE,C'1'                                                        
         BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
GET2     DS    0H                                                               
         MVC   WORK(2),0(R2)       AGY                                          
         MVC   WORK+2(1),0(R3)     MED                                          
         CLC   LASTIN,WORK                                                      
         BE    GET6                                                             
         MVC   LASTIN,WORK                                                      
         GOTO1 =V(BINSRCH),BSPARS,LASTIN                                        
         CLI   BSPARS,1                                                         
         BNE   GET4                                                             
         MVC   LASTOUT,=C'XXX'     NORMAL = COPY TO OUT1                        
         B     GET6                                                             
GET4     DS    0H                                                               
         L     RF,BSPARS                                                        
         MVC   LASTOUT,3(RF)                                                    
*                                                                               
GET6     DS    0H                                                               
         CLC   LASTOUT,=C'XXX'     = COPY TO OUT1                               
         BNE   GET7                                                             
         MVI   FILE,C'1'                                                        
         BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
GET7     DS    0H                                                               
         CLC   LASTOUT(2),LASTIN   IN VS OUT                                    
         BE    GET15               = TRANS TO OUT2                              
         CLC   LASTOUT,=C'   '     BLANK = DELETE                               
         BE    GET30                                                            
*                                                                               
         MVI   FILE,C'1'           IN NOT = OUT                                 
         BAS   RE,PUTREC           PRESERVE ON OUT1                             
*                                                                               
         CLI   CABLESW,C'N'        TEST TO SKIP CABLE STATIONS                  
         BNE   GET10                                                            
         CLI   REC,C'Y'            SKIP ALL CABLE RECS                          
         BE    GET                                                              
         CLI   REC,C'S'            SKIP STATION RECS                            
         BNE   GET8                                                             
         CLI   REC+2,C'Z'          SKIP IF STATION IS NUMERIC                   
         BNH   GET10               ELSE KEEP                                    
         AP    CBLCNT,=P'1'        NUMBER OF CABLE STATIONS SKIPPED             
         B     GET                                                              
GET8     CLI   REC,C'A'            AND ADDRESS RECS                             
         BNE   GET9                                                             
         CLI   REC+2,C'Z'          SKIP IF STATION IS NUMERIC                   
         BH    GET                                                              
GET9     CLI   REC,C'N'            AND N RECORDS                                
         BNE   GET10                                                            
         TM    REC+6,X'F0'         STATION IS PACKED                            
         BO    GET                                                              
*                                                                               
GET10    CLI   REPSW,C'N'          TEST TO COPY REPS                            
         BNE   GET12                                                            
         CLI   REC,C'R'            NO SKIP REP RECORDS                          
         BE    GET                                                              
         CLI   REC,C'B'            NO SKIP REP BROWSE RECORDS                   
         BE    GET                                                              
*                                                                               
GET12    DS    0H                                                               
         CLI   REC,C'S'            AND CLIENT EXCEPTION RECS                    
         BNE   GET14                                                            
         CLI   CLTSW,C'Y'                                                       
         BE    GET14                                                            
         CLC   REC+9(3),=C'000'                                                 
         BNE   GET                                                              
*                                                                               
GET14    DS    0H                                                               
         MVC   0(2,R2),LASTOUT     NEW AGY                                      
*                                                                               
         CLI   REC,C'S'            IF MASTER REC                                
         BNE   GET15                                                            
         CLI   REPSW,C'N'          AND NOT COPYING REPS                         
         BNE   GET15                                                            
         LA    RF,REC+SDATE-STAREC                                              
         XC    0(9,RF),0(RF)                                                    
         LA    RF,REC+SREP-STAREC                                               
         MVC   0(9,RF),=9C'0'                                                   
*                                                                               
GET15    CLI   REC,C'A'            STATION ADDRESS RECORD?                      
         BNE   GET16               NO                                           
         LA    RF,REC+ADDRCHDT-ADDRREC                                          
         XC    0(5,RF),0(RF)       CLEAR LAST CHANGED BY/DATE                   
*                                                                               
GET16    DS    0H                                                               
         MVI   FILE,C'2'           COPY TO OUT2                                 
         BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                  DELETE                                       
GET30    DS    0H                                                               
         B     GET                                                              
*                                                                               
*                                                                               
*                                                                               
PUT      DS    0H                                                               
         BAS   RE,PUTREC                                                        
         B     GET                                                              
         SPACE 2                                                                
EOJ      DS    0H                                                               
         XBASE                                                                  
         SPACE 2                                                                
EOF      CLOSE (IN,,OUT1,,OUT2,)                                                
*                                                                               
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
         MVI   EOFSW,C'Y'                                                       
         XIT1                                                                   
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
         LH    R2,REC-4                                                         
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
         LA    R0,REC-4                                                         
         LA    R1,OUT1                                                          
         LA    R2,OUT1CNT                                                       
         CLI   FILE,C'1'                                                        
         BE    PR4                                                              
         LA    R1,OUT2                                                          
         LA    R2,OUT2CNT                                                       
PR4      DS    0H                                                               
         AP    0(5,R2),=P'1'                                                    
         PUT   (1),(0)                                                          
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
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
OUT2     DCB   DDNAME=OUT2,            DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
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
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
REPSW    DC    C'Y'                                                             
CABLESW  DC    C'Y'                                                             
CLTSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL3'00'                                                          
LASTOUT  DC    XL3'00'                                                          
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
FILE     DS    C                                                                
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
OUT1CNT  DC    PL5'0',CL20'FILE 1 OUT'                                          
OUT2CNT  DC    PL5'0',CL20'FILE 2 OUT'                                          
CBLCNT   DC    PL5'0',CL20'CABLE STNS SKIPPED'                                  
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
*                                                                               
AGYTAB   DS    XL400                                                            
         DS    F                                                                
REC      DS    2500C                                                            
         DS    D                                                                
         SPACE 3                                                                
         ORG   REC                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
*                                                                               
BSTAB    CSECT                                                                  
         DS    1000C                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017STACOPY   03/20/12'                                      
         END                                                                    
