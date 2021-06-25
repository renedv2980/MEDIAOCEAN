*          DATA SET SPRCVLOOKS AT LEVEL 110 AS OF 07/26/94                      
*PHASE SPLOOK,*                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE MSUNPK                                                                 
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
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY LOOK PROGRAM'                           
         B     IN2                                                              
         EJECT                                                                  
*                                                                               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
***      LA    RE,RCVREC                                                        
***      AH    RE,0(RE)                                                         
***      XC    0(2,RE),0(RE)                                                    
***      LA    R1,RECVHDR                                                       
***      CLC   =X'2101',0(R1)      TEST SPTFILE COPY                            
***      BE    IN4                                                              
***      CLC   =X'2102',0(R1)      TEST SPTFILE CHANGE                          
***      BNE   IN2                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
         CLC   =C'30SONTCI',RECVHDR+104                                         
         BE    PRINTIT                                                          
         B     IN2                                                              
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
PRINTIT  DS    0H                                                               
         LA    RE,RCVREC                                                        
         LH    R3,0(RE)                                                         
         GOTO1 =V(PRNTBL),DMCB,0,RCVREC,C'DUMP',(R3),=C'1D'                     
         B     IN2                                                              
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
*&&UK                                                                           
PRTBUY   NTR1                                                                   
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
*                                                                               
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         ZIC   RF,BYTE                                                          
         LA    RE,MEDTAB-1(RF)                                                  
         MVC   PMED,0(RE)                                                       
         GOTO1 =V(CLUNPK),DMCB,BUYKCLT,PCLT                                     
         GOTO1 =V(MSUNPK),DMCB,BUYMSTA,PMKT,WORK                                
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
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
         GOTO1 =V(PRINT)                                                        
*&&                                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
MEDTAB   DC    C'TRNX'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2100,            X        
               BLKSIZE=16000,MACRF=PM                                           
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
SVPRD    DS    X                                                                
ELCODE   DS    X                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
WORK     DS    XL64                                                             
OUTCNT   DC    PL4'0'                                                           
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110SPRCVLOOKS07/26/94'                                      
         END                                                                    
