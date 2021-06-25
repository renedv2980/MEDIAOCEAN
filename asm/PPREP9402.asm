*          DATA SET PPREP9402  AT LEVEL 013 AS OF 05/01/02                      
*PHASE PP9402A,+0,NOAUTO                                                        
*INCLUDE IJFFZZWZ                                                               
         TITLE 'PP9402 - PRINTPAK PUB LABEL EXTRACT'                            
PP9402   CSECT                                                                  
         NMOD1 0,**PP94                                                         
         SPACE 2                                                                
         L     R4,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,4095(RC)                                                      
         LA    R9,1(R9)                                                         
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP94WRKD,R8                                                      
         SPACE 2                                                                
         CLI   MODE,PROCREQ                                                     
         BE    PROCR                                                            
         CLI   MODE,RUNFRST                                                     
         BE    FRST                                                             
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
FRST     DS    0H                                                               
*&&DO                                                                           
         LA    R1,PP94OUT                                                       
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         OPEN  (PP94OUT,(OUTPUT))                                               
*&&                                                                             
*                                                                               
         MVI   OUTREC,C'X'                   LINE UP                            
         MVC   OUTREC+1(100),OUTREC                                             
         MVC   OUTREC+100(L'OUTREC-100),OUTREC+99                               
*                                                                               
         LA    R7,6                                                             
FRST2    DS    0H                                                               
         LA    R1,PP94OUT                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
         BCT   R7,FRST2                                                         
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
LAST     DS    0H                  PUT END OF FILE RECORD                       
         MVI   OUTREC,X'FF'                  LINE UP                            
         MVC   OUTREC+1(100),OUTREC                                             
         MVC   OUTREC+100(L'OUTREC-100),OUTREC+99                               
         LA    R1,PP94OUT                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*&&DO                                                                           
         LA    R1,PP94OUT                                                       
         CLOSER (1)                                                             
*&&                                                                             
*&&OS                                                                           
         CLOSE (PP94OUT,)                                                       
*&&                                                                             
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
PROCR    DS    0H                                                               
         ZAP   NLBLS,=P'0'                                                      
         CLI   QOPT1,C'A'          TEST ALL PUBS OPTION                         
         BE    AP1                                                              
         EJECT                                                                  
*                                  DO ONLY ACTIVE PUBS                          
         SPACE 2                                                                
*                                  SET PLPARS                                   
         SR    R2,R2               A(REC)                                       
         RELOC (R3)                                                             
         A     R3,=A(PLIST)        A(TAB)                                       
         SR    R4,R4               NUMBER                                       
         LA    R5,6                LENGTH                                       
         LA    R6,6                KEY LEN                                      
         L     R7,=F'10000'        MAX                                          
         STM   R2,R7,PLPARS                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKEY                                                   
         MVI   KEY+3,X'21'                                                      
RB2      DS    0H                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   RB6                                                              
         BAS   RE,PLPOST           ADD TO LIST                                  
         IC    RF,KEY+12           LAST BYTE OF PUB                             
         LA    RF,1(RF)            BUMP TO NEXT PUB                             
         STC   RF,KEY+12                                                        
         B     RB2                                                              
*                                                                               
RB6      DS    0H                                                               
*                                                                               
         L     R2,PLTAB                                                         
         L     RF,PLNUM                                                         
         M     RE,PLLEN                                                         
         AR    RF,R2                                                            
         XC    0(6,RF),0(RF)       SET END OF TABLE                             
*                                                                               
RB6A     DS    0H                                                               
         OC    0(6,R2),0(R2)       END OF TABLE                                 
         BZ    RB10                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R2)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
RB6B     DS    0H                                                               
         BAS   RE,HIPUB                                                         
         CLC   KEY(10),KEYSAVE                                                  
         BE    RB8                                                              
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
RB7      DC    H'0'                PUB NOT FOUND                                
         CLI   PAGYPROF+16,C'0'         TEST DEFAULT                            
         BE    RB7                      NO                                      
         MVC   KEYSAVE+7(2),=C'ZZ'                                              
         CLC   KEY(10),KEYSAVE                                                  
         BE    RB8                                                              
         B     RB6B                                                             
*                                                                               
RB8      DS    0H                                                               
         BAS   RE,PUTPUB                                                        
         A     R2,PLLEN                                                         
         B     RB6A                NEXT PUB                                     
*                                                                               
RB10     DS    0H                                                               
         B     TOTPRT                                                           
         EJECT                                                                  
*                                  POST TO TABLE                                
PLPOST   NTR1                                                                   
         SPACE 2                                                                
         LA    R2,KEY+7                                                         
         ST    R2,PLPARS                                                        
         MVI   PLPARS,1            SET TO ADD                                   
         GOTO1 BINSRCH,PLPARS                                                   
*                                                                               
         OC    PLPARS+1(3),PLPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                  ALL PUBS OPTION                              
         SPACE 2                                                                
AP1      DS    0H                                                               
         XC    LASTPUB,LASTPUB                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         BAS   RE,HIPUB                                                         
         B     *+8                                                              
AP4      DS    0H                                                               
         BAS   RE,SEQPUB                                                        
         CLC   KEY(1),KEYSAVE                                                   
         BNE   AP20                DONE                                         
         CLI   KEY+9,X'81'         READ ONLY 81'S                               
         BNE   AP4                                                              
         CLC   KEY+7(2),PAGYKAGY                                                
         BE    AP6                                                              
         CLI   PAGYPROF+16,C'0'    TEST DEFAULT                                 
         BE    AP4                 NO                                           
         CLC   KEY+7(2),=C'ZZ'                                                  
         BNE   AP4                                                              
         CLC   KEY+1(6),LASTPUB         SAME AS AGY REC                         
         BE    AP4                      YES - BYPASS                            
*                                                                               
AP6      DS    0H                                                               
         MVC   LASTPUB,KEY+1                                                    
         BAS   RE,PUTPUB                                                        
         B     AP4                                                              
*                                                                               
AP20     DS    0H                                                               
         B     TOTPRT                                                           
*                                                                               
         EJECT                                                                  
*                                  GENERATE OUTPUT RECORD                       
PUTPUB   NTR1                                                                   
         SPACE 2                                                                
         MVC   OUTREC(20),SPACES                                                
         MVC   OUTREC+20(L'OUTREC-20),OUTREC+19                                 
*                                                                               
         BAS   RE,GETPUB                                                        
         AP    NLBLS,=P'1'                                                      
         MVC   UNPUB,SPACES                                                     
         GOTO1 PUBEDIT,DMCB,PUBKPUB,UNPUB                                       
*                                                                               
         LA    R2,UNPUB+16                                                      
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R0,UNPUB-1                                                       
         SR    R2,R0               R2 = LENGTH                                  
         LA    R3,LIN2+37                                                       
         SR    R3,R2                                                            
         MVC   0(17,R3),UNPUB                                                   
         SH    R3,=H'2'                                                         
         MVC   0(1,R3),PAGYKMED                                                 
*                                                                               
         MVC   LIN5+2(30),PUBLINE1                                              
         MVC   LIN6+2(30),PUBLINE2                                              
         MVC   LIN4+2(20),PUBZNAME                                              
         LA    R2,LIN3+2                                                        
         CLI   PUBZNAME,C' '                                                    
         BH    *+8                                                              
         LA    R2,LIN4+2                                                        
         CLI   PAGYKMED,C'N'                                                    
         BE    PP4                                                              
*                                  MAG - ONLY NAME                              
         MVC   0(20,R2),PUBNAME                                                 
         B     PP8                                                              
*                                  NEWS - CITY, NAME                            
PP4      DS    0H                                                               
         CLI   PUBCITY,C' '                                                     
         BNH   PP5                                                              
         MVC   0(16,R2),PUBCITY                                                 
         LA    R2,16(R2)                                                        
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,3(R2)                                                         
PP5      DS    0H                                                               
         MVC   0(20,R2),PUBNAME                                                 
*                                                                               
PP8      DS    0H                                                               
         LA    R1,PP94OUT                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                  TOTALS                                       
TOTPRT   DS    0H                                                               
         MVI   LINE,0                                                           
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
*                                                                               
         EDIT  (P5,NLBLS),(6,P+10)                                              
*                                                                               
         MVC   P+17(33),=C'LABELS GENERATED FOR THIS REQUEST'                   
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                             DMGR LINKS                                        
HIPUB    MVC   KEYSAVE,KEY                                                      
         LA    R0,DMRDHI                                                        
         B     DIRPUB                                                           
*                                                                               
SEQPUB   LA    R0,DMRSEQ                                                        
*                                                                               
DIRPUB   ST    R0,DMCB                                                          
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PUBDIR,KEY,KEY                                     
*                                                                               
         B     DMCK                                                             
*                                                                               
*                                                                               
GETPUB   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,DMWORK                 
*                                                                               
*                                                                               
DMCK     DS    0H                                                               
         LR    RE,R0                                                            
         TM    DMCB+8,X'00'                                                     
         BZR   RE                                                               
         DC    H'0'                                                             
         SPACE 2                                                                
HIGH     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
*                                                                               
         B     DMCK                                                             
         EJECT                                                                  
*&&DO                                                                           
PP94OUT  DTFMT DEVADDR=SYS008,RECFORM=FIXBLK,RECSIZE=266,BLKSIZE=2660, X        
               WORKA=YES,IOAREA1=OUT1,FILABL=STD,TYPEFLE=OUTPUT                 
*&&                                                                             
*&&OS                                                                           
PP94OUT  DCB   DDNAME=PP94OUT,         DOS SYS008                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00266,                                            X        
               BLKSIZE=02660,          DOS BLKSIZE=02660               X        
               MACRF=PM                                                         
*&&                                                                             
         SPACE 3                                                                
*&&DO                                                                           
OUT1     DS    2660C                                                            
*&&                                                                             
*                                                                               
PLIST    DS    60000C              UP TO 10000 PUBS                             
         EJECT                                                                  
PP94WRKD DSECT                                                                  
PP94WRK  DS    0C                                                               
PLPARS   DS    0CL24                                                            
PLRECA   DS    F                                                                
PLTAB    DS    F                                                                
PLNUM    DS    F                                                                
PLLEN    DS    F                                                                
PLKLEN   DS    F                                                                
PLMAX    DS    F                                                                
*                                                                               
*                                                                               
NLBLS    DS    PL5                                                              
*                                                                               
OUTREC   DS    0CL266              7 LINES X 38 CHARS                           
LIN1     DS    CL38                                                             
LIN2     DS    CL38                                                             
LIN3     DS    CL38                                                             
LIN4     DS    CL38                                                             
LIN5     DS    CL38                                                             
LIN6     DS    CL38                                                             
LIN7     DS    CL38                                                             
LASTPUB  DS    CL6                                                              
UNPUB    DS    CL17                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREP9402 05/01/02'                                      
         END                                                                    
