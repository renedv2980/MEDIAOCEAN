*          DATA SET TARCVLOOK  AT LEVEL 072 AS OF 05/01/02                      
*PHASE TALOOKA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE TINVCON                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'TARCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
RCVLOOK  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RCVLOOK,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCVLOOK+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,RCVLOOK          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
*****    OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
******   L     R1,VMASTC                                                        
*******  USING MASTD,R1                                                         
*******  CLI   MCWRITE,C'N'        IF WRITE = NO                                
*******  BNE   *+8                                                              
*******  MVI   WRITSTAT,C'N'       SET WRITE =NO                                
*******  DROP  R1                                                               
*                                                                               
         MVC   TITLE(20),=C'TALENT RECOVERY LOOK'                               
         B     IN2                                                              
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
*                                                                               
         CLI   RFILTY,X'72'        TEST TALFIL                                  
         BNE   IN2                                                              
*                                                                               
         CLC   RDATE,=X'5F0C12'     TEST CORRECT DATE                           
         BNE   IN2                                                              
         NI    RTIME,X'BF'          STRIP OFF EXTENDED RECORD INDICATOR         
         CLC   RTIME(3),=X'004500'  AND BEFORE 10:50AM                          
         BH    IN2                                                              
*                                                                               
         CLI   RKEY,TLINCDQ        TEST FOR INVOICE RECORD                      
         BNE   IN2                                                              
         CLI   RRECTY,2            IF CHANGE                                    
         BNE   IN2                                                              
*                                                                               
         BAS   RE,PRNTINV                                                       
         B     IN2                                                              
         SPACE 2                                                                
PRNTINV  NTR1                                                                   
         LA    R4,RKEY                                                          
         USING TLIND,R4                                                         
         MVC   P+1(6),TLINAGY                                                   
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 =V(TINVCON),DMCB,WORK,P+10,V(DATCON)                             
*                                                                               
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TAIND,R4                                                         
         MVC   P+19(8),TAINPST                                                  
*                                                                               
         LA    R4,RKEY                                                          
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACOD,R4                                                         
         MVC   P+29(12),TACOCID                                                 
*                                                                               
         LA    R4,RKEY                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PINV20                                                           
         USING TAPDD,R4                                                         
         MVC   P+42(3),TAPDUSE                                                  
         OC    TAPDCYCS,TAPDCYCS                                                
         BZ    PINV20                                                           
         GOTO1 =V(DATCON),DMCB,(X'11',TAPDCYCS),(8,P+47)                        
*                                                                               
PINV20   GOTO1 =V(PRINTER)         AND PRINT IT                                 
*                                                                               
         AP    OUTCNT,=P'1'        ADD TO OUTPUT COUNT                          
         B     XIT                                                              
         EJECT                                                                  
PRINTIT  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,RECVHDR-4      GET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,0,RECVHDR-4,C'DUMP',(R0),=C'1D'                  
         AP    OUTCNT,=P'1'        ADD TO OUTPUT COUNT                          
         B     XIT                                                              
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
******   CLOSE RECVOUT                                                          
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         EDIT  (P4,OUTCNT),(10,P+1),ZERO=NOBLANK,COMMAS=YES                     
         MVC   P+12(16),=CL16'INVOICE RECORDS'                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               MACRF=GM,EODAD=ENDIN                                             
         SPACE 1                                                                
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2048,            X        
               MACRF=PM,BLKSIZE=8200,BUFNO=2                                    
         EJECT                                                                  
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
WRITSTAT DC    C' '                                                             
MYSEQ    DC    F'0'                                                             
OUTCNT   DC    PL4'0'                                                           
OUTFCNT  DC    PL4'0'                                                           
OUTCCNT  DC    PL4'0'                                                           
CHGCOUNT DC    PL4'0'                                                           
DUB      DS    D                                                                
SAVEKEY  DS    CL(L'RKEY)                                                       
CPYKEY   DS    CL(L'RKEY)                                                       
CHGKEY   DS    CL(L'RKEY)                                                       
WORK     DS    XL64                                                             
DATADISP DC    Y(TLRCELEM-TLRCD)                                                
ELCODE   DS    XL1                                                              
*                                                                               
         DS    0D                                                               
         DC    C'*RCVREC*'                                                      
RCVREC   DC    F'0'                VAR REC LEN                                  
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RKEY     DS    0CL32                                                            
         DS    2100C                                                            
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072TARCVLOOK 05/01/02'                                      
         END                                                                    
