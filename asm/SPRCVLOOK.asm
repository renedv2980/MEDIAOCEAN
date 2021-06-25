*          DATA SET SPRCVLOOK  AT LEVEL 165 AS OF 05/01/02                      
*PHASE SPLOOK,*                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE LOADER                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'SPRCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
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
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (RECVOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&DO                                                                           
         OPEN  (KEYOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY LOOK PROGRAM'                           
*                                                                               
         MVC   DUB,SPACES                                                       
         MVC   DUB(4),=C'T00A'                                                  
         MVI   WORK,QSTAPACK                                                    
         GOTO1 =V(HEXOUT),DMCB,WORK,DUB+4,1,0                                   
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
*                                                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,STAPACK                                                       
*                                                                               
         B     IN2                                                              
         EJECT                                                                  
*                                                                               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
         LA    RE,RCVREC                                                        
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)                                                    
*&&DO                                                                           
         CLC   RECVHDR+4(4),=X'0000AE64'                                        
         BNE   IN2                                                              
         BAS   RE,PRINTIT                                                       
         B     IN2                                                              
*                                                                               
         CLC   =C'SE',RECVHDR+24+80                                             
         BNE    IN2                                                             
         BAS    RE,PRINTIT                                                      
         B      IN2                                                             
*                                                                               
IN3      CLC   =X'2103',RECVHDR    TEST SPTFILE/ADD                             
         BNE   IN2                                                              
         LA    RE,RECVHDR+24       POINT TO KEY                                 
         CLI   0(RE),X'10'         TEST BUY                                     
         BNH   IN2                                                              
         OC    4(2,RE),4(RE)       TEST MKT 0                                   
         BNZ   IN2                                                              
         PUT   RECVOUT,RCVREC                                                   
         GOTO1 =V(HEXOUT),DMCB,RECVHDR+24,P,13,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
         B     IN2                                                              
*&&                                                                             
IN3      CLC   RECVHDR+24(3),BADKEYLO                                           
         BL    IN2                                                              
         CLC   RECVHDR+24(3),BADKEYHI                                           
         BH    IN2                                                              
* SEE IF RECORD IS IN LIST                                                      
         LA    R0,31                                                            
         LA    R1,BADKEYLO                                                      
IN3A     CLC   RECVHDR+24(13),0(R1)                                             
         BE    IN4                                                              
         LA    R1,L'BADKEYLO(R1)                                                
         BCT   R0,IN3A                                                          
         B     IN2                                                              
*                                                                               
IN4      SR    R0,R0                                                            
         ICM   R0,3,RCVREC                                                      
         AHI   R0,-24                                                           
         SLL   R0,16                                                            
         STCM  R0,15,RECVHDR+20                                                 
         LA    R0,RECVHDR+20                                                    
         PUT   RECVOUT,(0)                                                      
         B     IN2                                                              
*                                                                               
BADKEYLO DC    X'B1E4FFFF01AFF0F20713010100'                                    
         DC    X'B1E4FFFF01AFF12C0713010100'                                    
         DC    X'B1E4FFFF01AFF1498713010100'                                    
         DC    X'B1E4FFFF01AFF14A8713010100'                                    
         DC    X'B1E4FFFF01AFF14B0713010100'                                    
         DC    X'B1E4FFFF01AFF1C50713010100'                                    
         DC    X'B1E4FFFF01AFF2018713010100'                                    
         DC    X'B1E4FFFF01AFF6118713010100'                                    
         DC    X'B1E4FFFF01AFF92D0713010100'                                    
         DC    X'B1E4FFFF01AFF92D8713010100'                                    
         DC    X'B1E4FFFF01AFF92E0713010100'                                    
         DC    X'B1E4FFFF01AFF9548713010100'                                    
         DC    X'B1E4FFFF01AFF9B18713010100'                                    
         DC    X'B1E4FFFF01AFF9B20713010100'                                    
         DC    X'B1E4FFFF01AFFB278713010100'                                    
         DC    X'B1E4FFFF01AFFB380713010100'                                    
         DC    X'B1E4FFFF06C3F0428713010100'                                    
         DC    X'B1E4FFFF06C3F14B8713010100'                                    
         DC    X'B1E4FFFF06C3F14B9413010100'                                    
         DC    X'B1E4FFFF06C3F14C0713010100'                                    
         DC    X'B1E4FFFF06C3F14E0713010100'                                    
         DC    X'B1E4FFFF06C3F2020713010100'                                    
         DC    X'B1E4FFFF06C3F4B40713010100'                                    
         DC    X'B1E4FFFF06C3F4BD0713010100'                                    
         DC    X'B1E4FFFF06C3F9268713010100'                                    
         DC    X'B1E4FFFF06C3F9269413010100'                                    
         DC    X'B1E4FFFF06C3F9B40713010100'                                    
         DC    X'B1E4FFFF06C3FAED0713010100'                                    
         DC    X'B1E4FFFF06C3FB268713010100'                                    
         DC    X'B1E4FFFF06C3FB400713010100'                                    
BADKEYHI DC    X'B1E4FFFF06C3FC950713010100'                                    
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        CLC   =C'30SONTCI',RECVHDR+104                                         
*        BE    PRINTIT                                                          
*        B     IN2                                                              
*&&DO                                                                           
         GETEL R6,24,ELCODE                                                     
*&&                                                                             
PRINTIT  NTR1                                                                   
         LA    RE,RCVREC                                                        
         LA    R3,24                                                            
         GOTO1 =V(PRNTBL),DMCB,0,RCVREC+4,C'DUMP',(R3),=C'1D'                   
*                                                                               
         LA    RE,RCVREC                                                        
         LH    R3,0(RE)                                                         
         SH    R3,=H'24'                                                        
         GOTO1 =V(PRNTBL),DMCB,0,RCVREC+4+24,C'DUMP',(R3),=C'1D'                
         B     EXIT                                                             
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLOSE RECVOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&DO                                                                           
         CLOSE KEYOUT                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
EOJ      DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
PRTBUY   NTR1                                                                   
         LA    R6,RCVREC+24                                                     
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
*                                                                               
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         ZIC   RF,BYTE                                                          
         LA    RE,MEDTAB-1(RF)                                                  
         MVC   PMED,0(RE)                                                       
*                                                                               
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         XC    STAWORK,STAWORK                                                  
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,BUYALPHA                                                 
         MVC   STAPMED,PMED                                                     
         MVC   STAPCTRY,C'U'       ONLY WORKS FOR U.S.                          
         MVC   STAPSTA,BUYMSTA                                                  
         GOTO1 STAPACK,(R1)                                                     
         MVC   PSTA(4),STAPQSTA                                                 
*                                                                               
         CLI   STAPQSTA+4,C' '                                                  
         BE    *+14                                                             
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),STAPQSTA+4                                             
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 =V(PRINTER)                                                      
EXIT     DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
MEDTAB   DC    C'TRNX'                                                          
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
ELCDLO   DS    C                                                                
ELCDHI   DS    C                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,                       X        
               BLKSIZE=16000,MACRF=PM                                           
*                                                                               
KEYOUT   DCB   DDNAME=KEYOUT,DSORG=PS,RECFM=FB,RECLN=26,               X        
               BLKSIZE=26000,MACRF=PM                                           
         EJECT                                                                  
CLUNPK   DS    F                                                                
STAPACK  DS    F                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
SVPRD    DS    X                                                                
ELCODE   DS    X                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
WORK     DS    XL64                                                             
OUTCNT   DC    PL4'0'                                                           
         DS    0D                                                               
         DC    CL8'*STAWORK'                                                    
STAWORK  DS    XL32                                                             
*                                                                               
RSORTKEY DS    0XL13                                                            
RSORTAM  DS    XL1                                                              
RSORTCLT DS    XL2                                                              
RSORTMKT DS    XL2                                                              
RSORTSTA DS    XL3                                                              
RSORTPRD DS    XL1                                                              
RSORTEST DS    XL1                                                              
RSORTBUY DS    XL3                                                              
*                                                                               
RSORTSEQ DS    XL3                                                              
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVREC   DC    F'0'                                                             
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL13                                                            
         DS    4000C                                                            
*                                                                               
WORKX    DS    0X                                                               
*                                                                               
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDCOREQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165SPRCVLOOK 05/01/02'                                      
         END                                                                    
