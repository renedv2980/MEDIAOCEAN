*          DATA SET PREPCON    AT LEVEL 004 AS OF 05/01/02                      
*PHASE PREPCONA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'PREPCON - REP COPY/PURGE'                                       
         PRINT NOGEN                                                            
*                                                                               
*              COPIES ALL RECORDS FROM TAPE ONE TO TAPE TWO                     
*              EXCEPT REPS BEING PURGED                                         
*                                                                               
PREPCON CSECT                                                                   
         NBASE 0,PREPCON,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,PREPCON+4095                                                  
         LA    R6,1(R6)                                                         
         USING PREPCON+4096,R6                                                  
*                                  NOTE USE OF R6 AS BASE REGISTER              
         USING ETABD,R8                                                         
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*&&DO                                                                           
         OPEN  IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*&&                                                                             
*                                                                               
*                                  SET LOOKUP TABLE BINSRCH PARS                
         ZAP   INCNT,=P'0'                                                      
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   COPYCNT,=P'0'                                                    
         ZAP   DELCNT,=P'0'                                                     
*                                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
*              ADD CODE FOR THIS RUN HERE                                       
         CLC   REC(3),=3X'FF'      EOF RECORD                                   
         BE    EOF                                                              
*                                                                               
*                                 THIS RUN DELETES WE REP                       
*                                 AND COPIES CA REPS TO WE                      
         CLC   REC(2),=C'WE'                                                    
         BNE   GET5                                                             
         CLI   REC+3,X'11'                                                      
         BNE   PUT                                                              
         AP    DELCNT,=P'1'                                                     
         B     GET                                                              
*                                                                               
GET5     CLC   REC(2),=C'CA'                                                    
         BNE   PUT                                                              
         CLI   REC+3,X'11'                                                      
         BNE   PUT                                                              
         BAS   RE,PUTREC            WRITE BACK CA REP                           
*                                                                               
         MVC   REC(2),=C'WE'        CREATE WE REP                               
         AP    COPYCNT,=P'1'                                                    
         CP    COPYCNT,=P'100'                                                  
         BH    PUT                                                              
         BAS   RE,DMPREC                                                        
*                                                                               
*                                                                               
*                                                                               
PUT      DS    0H                                                               
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
         SPACE 2                                                                
*&&DO                                                                           
EOF      CLOSE IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
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
*                                  PRINT ESTIMATE LIST                          
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
*        CLI   DMPSW,C'Y'                                                       
*        BNE   PUTREC2                                                          
*        MVI   DMPSW,C'N'                                                       
*        SP    DMPCNT,=P'1'                                                     
*        BNP   PUTREC2                                                          
*        BAS   RE,DMPREC                                                        
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
IN       DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=INPUT,             X        
               IOAREA1=IN1,DEVADDR=SYS010,FILABL=NO,WORKA=YES,         X        
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
OUT      DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=OUTPUT,            X        
               IOAREA1=OUT1,DEVADDR=SYS011,FILABL=NO,WORKA=YES                  
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
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
RTOTS    DS    11PL8               RUN TOTALS                                   
MTOTS    DS    11PL8               AGY MEDIA TOTALS                             
SAVAMED  DS    CL3                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT         '                                
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT        '                                
DELCNT   DC    PL5'0',CL20'WE REPS PURGED      '                                
COPYCNT  DC    PL5'0',CL20'CA REPS COPIED TO WE'                                
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4000C                                                            
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
         ORG   REC                                                              
       ++INCLUDE PBILLREC                                                       
*                                                                               
ETABD    DSECT                                                                  
ETAGY    DS    CL2                                                              
ETMED    DS    CL1                                                              
ETCLT    DS    CL3                                                              
ETPRD    DS    CL3                                                              
ETEST    DS    XL2                                                              
ETESTSW  DS    CL1                                                              
ETBUYS   DS    F                                                                
ETBILLS  DS    F                                                                
ETBUYGRS DS    F                                                                
ETBUYAC  DS    F                                                                
ETBUYCD  DS    F                                                                
ETPAYGRS DS    F                                                                
ETPAYAC  DS    F                                                                
ETPAYCD  DS    F                                                                
ETBILGRS DS    F                                                                
ETBILAC  DS    F                                                                
ETBILCD  DS    F                                                                
ETABEL   EQU   *-ETABD                                                          
*                                                                               
ESTTAB   CSECT                                                                  
         DS    6000CL56            ROOM FOR 6000 ESTS                           
         DC    X'0000'                                                          
*                                                                               
LKTAB    CSECT                                                                  
         DS    50CL11                                                           
         DC    X'0000'                                                          
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PREPCON   05/01/02'                                      
         END                                                                    
