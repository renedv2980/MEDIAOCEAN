*          DATA SET SPRCVLOOKA AT LEVEL 141 AS OF 03/11/96                      
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
         TITLE 'SPRCVLOOKA - SEARCH RECOVERY FILE FOR 2 OC ELEMS'               
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
         L     R0,=A(STATAB)                                                    
         LA    R1,2048                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    KEYSAVE,KEYSAVE                                                  
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
*                                                                               
         CLC   RECVHDR(2),=X'2102' SPOT FILE CHANGE?                            
         BNE   IN2                 NOPE-GET NEXT                                
         CLI   RECVHDR+24,X'10'    BUYREC?                                      
         BNH   IN2                 NOPE-GET NEXT                                
*                                                                               
         LA    R6,RECVHDR+24+24    POINT TO FIRST EMELENT                       
IN4      CLI   0(R6),0                                                          
         BE    IN2                 DONE WITH REC --> GET NEXT                   
         CLI   0(R6),X'0C'         FIRST OC ELEM                                
         BNE   IN6                                                              
         TM    6(R6),X'80'         MINUS?                                       
         BZ    IN6                                                              
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'0C'         2ND OC ELEM IN A ROW?                        
         BNE   IN6                 NOPE - WE ARE OK LOOK AT NEXT ELEM           
         TM    6(R6),X'80'         2ND MINUS OC ELEM IN A ROW?                  
         BZ    IN6                                                              
*                                                                               
         MVC   P(33),=C'REC WITH 2 MINUS OC ELEMS IN ROW:'                      
         MVC   KEYSAVE,RECVHDR+24    THIS REC HAS 2 OC ELEMS IN A ROW           
         GOTO1 =V(HEXOUT),DMCB,KEYSAVE,P+35,13,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
         B     IN2                 GET NEXT REC                                 
*                                                                               
IN6      ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     IN4                                                              
*                                                                               
KEYSAVE  DS    XL13                                                             
STASAVE  DS    CL8                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
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
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*&&DO                                                                           
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
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*======= MANY INSTRUCTIONS MISSING HERE !  =============*                       
         GOTO1 STAPACK,DMCB,BUYMSTA,PMKT,WORK                                   
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
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
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
*                                                                               
STATAB   DS    2048C                                                            
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
       ++INCLUDE SPNWSDTL                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'141SPRCVLOOKA03/11/96'                                      
         END                                                                    
