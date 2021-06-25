*          DATA SET FILCOMP    AT LEVEL 013 AS OF 05/01/02                      
*PHASE FILCOMP,*,NOAUTO                                                         
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE IJFVZZWZ                                                               
*INCLUDE IJDFYZZZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'FILE COMPARISON PROGRAM'                                        
FILCOMP  CSECT                                                                  
         NBASE 0,**FILCOMP,=V(REGSAVE),RA                                       
         SPACE 2                                                                
         COMRG                                                                  
         MVC   UPSI,23(R1)                                                      
*                                  X'80' PRINT ONLY KEYS                        
*                                  X'40'  LIMIT OF 100 RECS                     
*                                  X'20'  PRINT ONLY MISSING                    
*                                  X'10'  PRINT ONLY F1                         
         TM    UPSI,X'40'                                                       
         BZ    *+10                                                             
         ZAP   MAX,=P'100'                                                      
         OPEN  FIL1,FIL2                                                        
         SPACE 1                                                                
         BAS   R9,PRNT                                                          
         BAS   R9,PRNT                                                          
GETBOTH  BAS   R9,GET1                                                          
         BAS   R9,GET2                                                          
COMP     CLC   REC1(25),REC2                                                    
         BL    PUT1                                                             
         BH    PUT2                                                             
         CLC   REC1(25),=25X'FF'                                                
         BE    END                                                              
         BAS   R9,CMPR                                                          
         BE    GETBOTH                                                          
         TM    UPSI,X'20'                                                       
         BNZ   COMP2                                                            
         MVC   P+1(10),=C'UNEQUAL F1'                                           
         BAS   RE,DMPREC1                                                       
         TM    UPSI,X'10'                                                       
         BNZ   *+14                                                             
         MVC   P+1(10),=C'UNEQUAL F2'                                           
         BAS   RE,DMPREC2                                                       
COMP2    DS    0H                                                               
         AP    CNT5,=P'1'                                                       
         B     GETBOTH                                                          
         SPACE 3                                                                
PUT1     AP    CNT1,=P'1'                                                       
         MVC   P+1(10),=C'ONLY ON F1'                                           
         BAS   RE,DMPREC1                                                       
         BAS   R9,GET1                                                          
         B     COMP                                                             
         SPACE 3                                                                
PUT2     AP    CNT2,=P'1'                                                       
         TM    UPSI,X'10'                                                       
         BNZ   PUT2B                                                            
         MVC   P+1(10),=C'ONLY ON F2'                                           
         BAS   RE,DMPREC2                                                       
PUT2B    DS    0H                                                               
         BAS   R9,GET2                                                          
         B     COMP                                                             
         SPACE 3                                                                
GET1     CLC   REC1(25),=25X'FF'                                                
         BER   R9                                                               
GET1A    GET   FIL1,REC1-4                                                      
         OC    REC1(24),REC1                                                    
         BNZ   *+12                                                             
         MVI   FS1,1               HEADER                                       
         B     GET1A                                                            
         CLC   REC1(25),=25X'FF'                                                
         BNE   *+10                                                             
         MVI   FS2,1                                                            
         BR    R9                                                               
         AP    CNT3,=P'1'                                                       
         BR    R9                                                               
         SPACE 3                                                                
GET2     CLC   REC2(25),=25X'FF'                                                
         BER   R9                                                               
GET2A    GET   FIL2,REC2-4                                                      
         OC    REC2(24),REC2                                                    
         BNZ   *+12                                                             
         MVI   FS3,1                                                            
         B     GET2A                                                            
         CLC   REC2(25),=25X'FF'                                                
         BNE   *+10                                                             
         MVI   FS4,1                                                            
         BR    R9                                                               
         AP    CNT4,=P'1'                                                       
         BR    R9                                                               
         SPACE 3                                                                
END      LA    R3,CNT1                                                          
         LA    R4,5                                                             
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         BAS   R9,PRNT                                                          
         BAS   R9,PRNT                                                          
         BAS   R9,PRNT                                                          
END1     MVC   P+25(20),8(R3)                                                   
         OI    7(R3),X'0F'                                                      
         UNPK  P+46(8),0(8,R3)                                                  
         BAS   R9,PRNT                                                          
         LA    R3,28(R3)                                                        
         BCT   R4,END1                                                          
         LA    R3,FS1                                                           
         LA    R4,4                                                             
END2     CLI   0(R3),0                                                          
         BE    END4                                                             
         MVC   P+25(20),1(R3)                                                   
         BAS   R9,PRNT                                                          
END4     LA    R3,21(R3)                                                        
         BCT   R4,END2                                                          
         CLOSE FIL1,FIL2                                                        
         EOJ                                                                    
         SPACE 3                                                                
PRNT     DS    0H                                                               
         GOTO1 =V(PRINT),DMCB,P,=C'BL01'                                        
         SPACE 1                                                                
         MVI   P,C' '                                                           
         MVC   P+1(131),P                                                       
         BR    R9                                                               
         SPACE 3                                                                
CMPR     DS    0H                                                               
         MVC   DUB(2),REC1+25                                                   
         LH    RF,DUB                                                           
         LA    R1,REC1                                                          
         LA    R2,REC2                                                          
CMPR2    CH    RF,=H'256'                                                       
         BL    CMPR4                                                            
         CLC   0(256,R1),0(R2)                                                  
         BNE   CMPRX                                                            
         LA    R1,256(R1)                                                       
         LA    R2,256(R2)                                                       
         SH    RF,=H'256'                                                       
         B     CMPR2                                                            
CMPR4    LTR   RF,RF                                                            
         BZ    CMPRX                                                            
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     CMPRX                                                            
         CLC   0(0,R1),0(R2)                                                    
CMPRX    BR    R9                                                               
         SPACE 3                                                                
         SPACE 3                                                                
DMPREC1  DS    0H                                                               
         LA    R5,REC1                                                          
         B     DMPREC                                                           
DMPREC2  DS    0H                                                               
         LA    R5,REC2                                                          
DMPREC   NTR1                                                                   
         SP    MAX,=P'1'                                                        
         BNP   DRX                                                              
         TM    UPSI,X'80'                                                       
         BZ    DR1                                                              
         GOTO1 =V(HEXOUT),DMCB,(R5),P+12,25,=C'N'                               
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+86(25),WORK                                                    
         BAS   R9,PRNT                                                          
         B     DRX                                                              
*                                                                               
DR1      DS    0H                                                               
         BAS   R9,PRNT                                                          
         MVC   HALF,25(R5)                                                      
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DR2      DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   DRX                                                              
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+12(8),WORK+00                                                  
         MVC   P+21(8),WORK+08                                                  
         MVC   P+30(8),WORK+16                                                  
         MVC   P+39(8),WORK+24                                                  
         MVC   P+48(8),WORK+32                                                  
         MVC   P+57(8),WORK+40                                                  
         MVC   P+66(8),WORK+48                                                  
         MVC   P+75(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+86(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   R9,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DR2                                                              
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
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
         SPACE 3                                                                
EOT1     MVC   REC1(25),=25X'FF'                                                
         B     GET1                                                             
EOT2     MVC   REC2(25),=25X'FF'                                                
         B     GET2                                                             
         SPACE 3                                                                
DMCB     DC    6F'0'                                                            
P        DC    132C' '                                                          
*                                                                               
CNT1     DC    PL8'0'                                                           
         DC    CL20'MISSING ON FILE 2'                                          
CNT2     DC    PL8'0'                                                           
         DC    CL20'MISSING ON FILE 1'                                          
CNT3     DC    PL8'0'                                                           
         DC    CL20'FILE 1 TOTAL'                                               
CNT4     DC    PL8'0'                                                           
         DC    CL20'FILE 2 TOTAL'                                               
CNT5     DC    PL8'0'                                                           
         DC    CL20'UNEQUAL RECORDS'                                            
FS1      DC    X'00'                                                            
         DC    CL20'HAVE FILE1 HEADER'                                          
FS2      DC    X'00'                                                            
         DC    CL20'HAVE FILE 1 TRAILER'                                        
FS3      DC    X'00'                                                            
         DC    CL20'HAVE FILE 2 HEADER'                                         
FS4      DC    X'00'                                                            
         DC    CL20'HAVE FILE 2 TRAILER'                                        
UPSI     DS    X                                                                
         DS    0F                                                               
WORK     DS    XL64                                                             
HALF     DS    H                                                                
DUB      DS    D                                                                
MAX      DC    PL8'9999'                                                        
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
FIL1     DTFMT DEVADDR=SYS010,BLKSIZE=8500,RECSIZE=2100,RECFORM=VARBLK,X        
               IOAREA1=IN1,WORKA=YES,TYPEFLE=INPUT,EOFADDR=EOT1,       X        
               FILABL=STD                                                       
         SPACE 3                                                                
FIL2     DTFMT DEVADDR=SYS011,BLKSIZE=8500,RECSIZE=2100,RECFORM=VARBLK,X        
               IOAREA1=IN2,WORKA=YES,TYPEFLE=INPUT,EOFADDR=EOT2,       X        
               FILABL=STD                                                       
         SPACE 3                                                                
         DC    C'**RECORD 1**'                                                  
STAT1    DC    CL8' '                                                           
         DS    F                                                                
REC1     DS    2500C                                                            
         DC    C'**RECORD 2**'                                                  
STAT2    DC    CL8' '                                                           
         DS    F                                                                
REC2     DS    2500C                                                            
IN1      DS    8500C                                                            
IN2      DS    8500C                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013FILCOMP   05/01/02'                                      
         END                                                                    
