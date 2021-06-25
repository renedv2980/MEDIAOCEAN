*          DATA SET RERCVLK    AT LEVEL 146 AS OF 05/01/02                      
*          DATA SET RERCVLK    AT LEVEL 144 AS OF 04/27/95                      
*PHASE RELOOKA                                                                  
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
         TITLE 'RERCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
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
         OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY LOOK PROGRAM'                           
*                                                                               
         B     IN2                                                              
         EJECT                                                                  
*                                                                               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
*                                                                               
*   TEST                                                                        
*        L     RF,RECCTR                                                        
*        LA    RF,1(RF)                                                         
*        ST    RF,RECCTR                                                        
*        CH    RF,=H'30'                                                        
*        BE    ENDIN                                                            
*   TEST END                                                                    
*                                                                               
*        LA    RE,RCVREC                                                        
*        AH    RE,0(RE)                                                         
*        XC    0(2,RE),0(RE)                                                    
*                                                                               
**       CLC   RECVHDR+4(4),=X'0000AE64'                                        
**       BNE   IN2                                                              
**       BAS   RE,PRINTIT                                                       
**       B     IN2                                                              
*                                                                               
**       CLC   =C'30TRRBWA',RECVHDR+24+80                                       
**       BNE    IN2                                                             
*                                                                               
*   TEST                                                                        
         L     RF,RECCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RECCTR                                                        
         CH    RF,=H'40'                                                        
         BH    IN0020                                                           
         BAS    RE,PRINTIT                                                      
IN0020   EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         BAS   RE,RESETREC                                                      
*                                                                               
         LA    RE,RKEY-1                                                        
         AH    RE,28(RE)                                                        
         XC    0(2,RE),0(RE)                                                    
*                                                                               
         LA    RF,RCVREC+4+34      A(1ST ELEMENT)                               
IN0030   EQU   *                                                                
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),0             END OF RECORD?                               
         BE    IN0038              YES                                          
         CLI   1(RF),0             INVALID LENGTH?                              
         BNE   IN0030                                                           
         XC    0(8,RF),0(RF)       CLEAR NEXT EIGHT BYTES                       
         LR    R1,RF               SAVE RF                                      
         LA    RE,RCVREC           LOAD A(RCVREC)                               
         SR    RF,RE               SUB A(NEW END)                               
         STH   RF,RCVREC           PUT LENGTH BACK                              
         LA    RE,RKEY             LOAD A(RECORD)                               
         SR    R1,RE               SUB A(NEW END)                               
         STCM  R1,3,RKEY+27                                                     
         BAS   RE,PRINTIT2                                                      
IN0038   EQU   *                                                                
         LA    R1,RECVOUT          WRITE OUTPUT RECORD                          
         PUT   (1),RCVREC                                                       
*                                                                               
*   TEST                                                                        
         L     RF,RECCTR                                                        
         CH    RF,=H'40'                                                        
         BH    IN0040                                                           
         BAS    RE,PRINTIT                                                      
IN0040   EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         B      IN2                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
RESETREC NTR1                                                                   
         LA    R2,RCVREC+4         TO-ADDRESS                                   
         LA    R3,1280             SET RECEIVING MAX LENGTH                     
         LA    R4,RCVREC+11        FROM-ADDRESS                                 
         L     R5,RCVREC           SET A(RECORD LENGTH)                         
         MVCL  R2,R4               MCVL TO DROP **REC**                         
*                                  LEAVE LENGTH AS IS                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
PRINTIT  NTR1                                                                   
*        LA    RE,RCVREC                                                        
*        LA    R3,36                                                            
*        GOTO1 =V(PRNTBL),DMCB,0,RCVREC,C'DUMP',(R3),=C'1D'                     
*                                                                               
         LA    RE,RCVREC                                                        
         LH    R3,0(RE)                                                         
*        SH    R3,=H'24'                                                        
*        GOTO1 =V(PRNTBL),DMCB,0,RCVREC+4+24,C'DUMP',(R3),=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,0,RCVREC,C'DUMP',(R3),=C'1D'                     
         B     EXIT                                                             
*                                                                               
PRINTIT2 NTR1                                                                   
         LA    RE,RCVREC                                                        
         LH    R3,0(RE)                                                         
         GOTO1 =V(PRNTBL),DMCB,0,RCVREC,C'DUMP',32,=C'1D'                       
         B     EXIT                                                             
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
         CLOSE RECVOUT                                                          
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
*        LA    R5,P+2                                                           
*        USING PLINED,R5                                                        
*                                                                               
*        MVC   BYTE,BUYKAM                                                      
*        NI    BYTE,X'0F'                                                       
*        ZIC   RF,BYTE                                                          
*        LA    RE,MEDTAB-1(RF)                                                  
*        MVC   PMED,0(RE)                                                       
*        GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*        LA    R1,STAWORK                                                       
*        USING STAPACKD,R1                                                      
*======= MANY INSTRUCTIONS MISSING HERE !  =============*                       
*        GOTO1 STAPACK,DMCB,BUYMSTA,PMKT,WORK                                   
*        MVC   PSTA(4),WORK                                                     
*        MVI   PSTA+4,C'-'                                                      
*        MVC   PSTA+5(1),WORK+4                                                 
*                                                                               
*        ZIC   R0,BUYKEST                                                       
*        CVD   R0,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  WORK(10),DUB                                                     
*        MVC   PEST,WORK+7                                                      
*        MVI   PEST+3,C'-'                                                      
*                                                                               
*        ZIC   R0,BUYKBUY                                                       
*        CVD   R0,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  WORK(10),DUB                                                     
*        MVC   PLIN,WORK+7                                                      
*                                                                               
*        GOTO1 =V(PRINT)                                                        
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
MEDTAB   DC    C'TRNX'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               BLKSIZE=2104,MACRF=GM,EODAD=ENDIN                                
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=02024,           X        
               BLKSIZE=2028,MACRF=PM                                            
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
RECCTR   DS    F                                                                
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
         DS    2100C                                                            
*                                                                               
WORKX    DS    0X                                                               
         EJECT                                                                  
RBUYRECD DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'146RERCVLK   05/01/02'                                      
         END                                                                    
