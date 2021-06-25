*          DATA SET SCHORCV    AT LEVEL 182 AS OF 02/28/00                      
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
         XC    KEYSAVE,KEYSAVE                                                  
*                                                                               
         XC    COUNT,COUNT                                                      
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
*        OPEN  (KEYOUT,(OUTPUT))                                                
*        LTR   RF,RF                                                            
*        BZ    *+6                                                              
*        DC    H'0'                                                             
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
         MVC   P(13),=C'*** START ***'                                          
         GOTO1 =V(PRINTER)                                                      
         B     IN2                                                              
         EJECT                                                                  
*                                                                               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
         CLI   RCVREC+4,X'22'                                                   
         BE    IN2                                                              
         XC    THREBYTE,THREBYTE                                                
         LA    RE,RCVREC                                                        
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)                                                    
*                                                                               
* CHECK SEQUENCE OF 0B/0C ELEMENTS                                              
*                                                                               
         LA    R8,RECVHDR+24                                                    
         USING BUYREC,R8                                                        
*                                                                               
         CLI   0(R8),X'10'                                                      
         BL    IN2                                                              
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM           POINT TO FIRST EMELENT                       
IN10     BAS   RE,NEXTEL                                                        
         BNE   IN2                                                              
*                                                                               
         CLC   THREBYTE,2(R6)                                                   
         BE    IN20                DUPLICATE REC, BAD                           
         MVC   THREBYTE,2(R6)                                                   
         B     IN10                                                             
*                                                                               
*        BAD RECORDS GETS PRINTED OUT                                           
*                                                                               
IN20     MVC   KEYSAVE,BUYKEY                                                   
         BAS   RE,PRTBUY                                                        
         B     IN2                                                              
*                                                                               
KEYSAVE  DS    XL13                                                             
STASAVE  DS    CL8                                                              
THREBYTE DS    XL3                                                              
LASTBAD  DS    XL13                                                             
COUNT    DS    H                                                                
*                                                                               
         EJECT                                                                  
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
*                                                                               
EOJ      MVC   P(13),=C'--- TOTAL -->'                                          
         GOTO1 =V(HEXOUT),DMCB,COUNT,P+16,2                                     
         GOTO1 =V(PRINTER)                                                      
         DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
PRTBUY   NTR1                                                                   
*                                                                               
         CLC   LASTBAD,BUYKEY                                                   
         BE    EXIT                                                             
*                                                                               
         ZIC   R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
         GOTO1 =V(HEXOUT),DMCB,COUNT,P+90,2                                     
*                                                                               
         MVC   LASTBAD,BUYKEY                                                   
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
*        GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
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
         GOTO1 =V(HEXOUT),DMCB,BUYKEY,PKEY,13                                   
*                                                                               
         GOTO1 =V(PRINTER)                                                      
EXIT     DS    0H                                                               
         XIT1                                                                   
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
**PAN#1  DC    CL21'182SCHORCV   02/28/00'                                      
         END                                                                    
