*          DATA SET SPREPFXX4  AT LEVEL 033 AS OF 02/13/95                      
*PHASE SPFX02Q                                                                  
         TITLE 'SPFX02 - BAD CANADIAN BUYS'                                     
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)        MOVE CLT                                     
         MVC   KEY+3(3),=X'FF0000'                                              
         MVC   KEY+6(3),=X'086803'                                              
         GOTO1 HIGH                                                             
                                                                                
         MVI   PRINTIT,C'Y'                                                     
         B     FX20                                                             
*                                                                               
FX10     GOTO1 SEQ                                                              
*                                                                               
FX20     DS    0H                                                               
         CLC   KEY(9),KEYSAVE      TEST A-M/CLT/PRD/MKT 0                       
         BE    FX25                                                             
         MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
*                                                                               
* NOW CHANGE THE BUY RECORD                                                     
*                                                                               
FX25     DS    0H                                                               
         GOTO1 PRNTBL,DMCB,=C'DIRKEY',KEY,C'DUMP',18,=C'1D00'                   
                                                                                
         GOTO1 GETBUY                                                           
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'68'                                                     
         BAS   RE,GETEL                                                         
                                                                                
FX30     CLI   0(R6),X'68'                                                      
         BNE   FX10                                                             
         CLI   6(R6),X'0F'                                                      
         BNE   FX40                                                             
         MVI   6(R6),X'30'                                                      
                                                                                
         CLI   PRINTIT,C'Y'                                                     
         BNE   FX35                                                             
         GOTO1 HEXOUT,DMCB,(R6),P,11,=C'TOG'                                    
         GOTO1 REPORT                                                           
                                                                                
FX35     BAS   RE,NEXTEL                                                        
         BE    FX30                                                             
                                                                                
         GOTO1 PUTBUY                                                           
                                                                                
         MVI   PRINTIT,C'N'                                                     
         B     FX10                                                             
                                                                                
* SKIP TO NEXT STATION                                                          
                                                                                
FX40     DS    0H                                                               
         MVC   P,=C'ERROR'                                                      
         GOTO1 REPORT                                                           
         GOTO1 PRNTBL,DMCB,=C'FILREC',(R6),C'DUMP',18,=C'1D00'                  
         B     FX10                                                             
                                                                                
FXEND    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
STAWORK  DS    XL31                                                             
BAGYTAB  DS    16XL4                                                            
         DS    F                                                                
PRINTIT  DS    C                                                                
RECORD   DS    CL50                                                             
SPACE    DS    XL2000                                                           
*                                                                               
AGENCYD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPREPFXX4 02/13/95'                                      
         END                                                                    
