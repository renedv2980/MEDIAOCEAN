*          DATA SET SPLDEXTAA  AT LEVEL 002 AS OF 09/20/00                      
*          DATA SET SPLDEXTMH  AT LEVEL 002 AS OF 04/20/99                      
*PHASE SPEXTAA                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'SPEXTMH - DELETE C1DDE3 RECORDS'                                
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         L     R9,12(R1)           DMLDDEFN                                     
         USING LDDEFND,R9                                                       
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(CLUNPK)                                                    
         ST    RF,VCLUNPK                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         XC    COUNT,COUNT                                                      
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
         CLC   =X'C1DDE3',0(R6)                                                 
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
COUNTIT  L     R0,COUNT                                                         
         AHI   R0,1                                                             
         ST    R0,COUNT                                                         
         GOTO1 LHEXOUT,DMCB,(R6),P,13,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         L     R0,COUNT                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P(6),DUB                                                         
         MVC   P+7(30),=CL30'OF THE LITTLE FUCKERS'                             
         GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
COUNT    DS    F                   NUMBER OF CLIENT RECORDS CHANGED             
BYTE     DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
MYFLAG   DS    X                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
VCLUNPK  DS    A                                                                
VMSUNPK  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
*                                                                               
         DS    0D                                                               
KEYLIST  DS    0XL13                       '                                    
         DC    X'0D7652B03B0EEBC75101000000'                                    
         DC    X'0E01528AB20E240EEBC7510100'                                    
         DC    X'0E0152B02802160EEBC7510100'                                    
         DC    X'0E0152B03A02490EEBC7510100'                                    
         DC    X'0E0152B03A03150EEBC7510100'                                    
         DC    X'0E0152B03A03160EEBC7510100'                                    
         DC    X'0E0152B03B01150EEBC7510100'                                    
         DC    X'0E0152B03B01160EEBC7510100'                                    
         DC    X'0E0152B03B027E0EEBC7510100'                                    
         DC    X'0E0152B03B027F0EEBC7510100'                                    
         DC    X'528641080EEBC7510101000101'                                    
         DC    X'528641080EEBC7510101000201'                                    
         DC    X'528641080EEBC7510101000301'                                    
         DC    X'529A64FF0EEBC751014E000101'                                    
         DC    X'529A64FF0EEBC751014E000201'                                    
         DC    X'52B028020EEBC7510116000201'                                    
         DC    X'52B028020EEBC7510116000301'                                    
         DC    X'52B028020EEBC7510116000501'                                    
         DC    X'52B03A020EEBC7510149000101'                                    
         DC    X'52B03A020EEBC7510149000201'                                    
         DC    X'52B03A020EEBC7510149000301'                                    
         DC    X'52B03A020EEBC7510149000401'                                    
         DC    X'52B03A020EEBC7510149000501'                                    
         DC    X'52B03A020EEBC7510149000601'                                    
         DC    X'52B03A020EEBC7510149000701'                                    
         DC    X'52B03A020EEBC7510149000801'                                    
         DC    X'52B03A020EEBC7510149000901'                                    
         DC    X'52B03A020EEBC7510149000A01'                                    
         DC    X'52B03A030EEBC7510115000101'                                    
         DC    X'52B03A030EEBC7510115000201'                                    
         DC    X'52B03A030EEBC7510115000201'                                    
         DC    X'52B03A030EEBC7510115000301'                                    
         DC    X'52B03A030EEBC7510115000401'                                    
         DC    X'52B03A030EEBC7510115000501'                                    
         DC    X'52B03A030EEBC7510115000601'                                    
         DC    X'52B03A030EEBC7510116000101'                                    
         DC    X'52B03A030EEBC7510116000201'                                    
         DC    X'52B03A030EEBC7510116000301'                                    
         DC    X'52B03A030EEBC7510116000401'                                    
         DC    X'52B03A030EEBC7510116000501'                                    
         DC    X'52B03A030EEBC7510116000601'                                    
         DC    X'52B03A030EEBC7510116000701'                                    
         DC    X'52B03A030EEBC7510116000801'                                    
         DC    X'52B03A030EEBC7510116000901'                                    
         DC    X'52B03A030EEBC7510116000A01'                                    
         DC    X'52B03A030EEBC7510116000B01'                                    
         DC    X'52B03A030EEBC7510116000C01'                                    
         DC    X'52B03A030EEBC7510116000D01'                                    
         DC    X'52B03A030EEBC7510116000E01'                                    
         DC    X'52B03A030EEBC7510116000F01'                                    
         DC    X'52B03A030EEBC7510116001001'                                    
         DC    X'52B03A030EEBC7510116001101'                                    
         DC    X'52B03A030EEBC7510116001201'                                    
         DC    X'52B03B010EEBC7510115000101'                                    
         DC    X'52B03B010EEBC7510115000201'                                    
         DC    X'52B03B010EEBC7510115000401'                                    
         DC    X'52B03B010EEBC7510115000501'                                    
         DC    X'52B03B010EEBC7510115000601'                                    
         DC    X'52B03B010EEBC7510116000101'                                    
         DC    X'52B03B010EEBC7510116000201'                                    
         DC    X'52B03B010EEBC7510116000301'                                    
         DC    X'52B03B010EEBC7510116000401'                                    
         DC    X'52B03B010EEBC7510116000501'                                    
         DC    X'52B03B010EEBC7510116000601'                                    
         DC    X'52B03B010EEBC7510116000701'                                    
         DC    X'52B03B010EEBC7510116000801'                                    
         DC    X'52B03B010EEBC7510116000901'                                    
         DC    X'52B03B010EEBC7510116000B01'                                    
         DC    X'52B03B010EEBC7510116000C01'                                    
         DC    X'52B03B010EEBC7510116000D01'                                    
         DC    X'52B03B010EEBC7510116000E01'                                    
         DC    X'52B03B010EEBC7510116000F01'                                    
         DC    X'52B03B010EEBC7510116000F01'                                    
         DC    X'52B03B010EEBC7510116001001'                                    
         DC    X'52B03B010EEBC7510116001101'                                    
         DC    X'52B03B010EEBC7510116001201'                                    
         DC    X'52B03B010EEBC7510116001301'                                    
         DC    X'52B03B010EEBC7510116001401'                                    
         DC    X'52B03B010EEBC7510116001501'                                    
         DC    X'52B03B010EEBC7510116001601'                                    
         DC    X'52B03B010EEBC7510116001701'                                    
         DC    X'52B03B020EEBC751017E000101'                                    
         DC    X'52B03B020EEBC751017E000201'                                    
         DC    X'52B03B020EEBC751017E000301'                                    
         DC    X'52B03B020EEBC751017E000401'                                    
         DC    X'52B03B020EEBC751017E000501'                                    
         DC    X'52B03B020EEBC751017E000601'                                    
         DC    X'52B03B020EEBC751017F000101'                                    
         DC    X'52B03B020EEBC751017F000201'                                    
         DC    X'52B03B020EEBC751017F000301'                                    
         DC    X'52B03B020EEBC751017F000401'                                    
         DC    X'52B03B020EEBC751017F000501'                                    
         DC    X'52B03B020EEBC751017F000601'                                    
         DC    X'52B03B020EEBC751017F000701'                                    
         DC    X'52B03B020EEBC751017F000801'                                    
         DC    X'52B03B020EEBC751017F000901'                                    
         DC    X'52B03B020EEBC751017F000901'                                    
         DC    X'52B03B020EEBC751017F000A01'                                    
         DC    X'52B03B020EEBC751017F000B01'                                    
         DC    X'52B03B020EEBC751017F000D01'                                    
         DC    X'52B03B020EEBC751017F000F01'                                    
         DC    X'52B03B020EEBC751017F001001'                                    
         DC    X'52B03B020EEBC751017F001501'                                    
         DC    X'52B03B020EEBC751017F001601'                                    
         DC    X'52B03B020EEBC751017F001701'                                    
         DC    X'52B03B020EEBC751017F001801'                                    
         DC    X'52B03B020EEBC751017F001901'                                    
         DC    X'52B03B020EEBC751017F001A01'                                    
         DC    X'52B03B020EEBC751017F001B01'                                    
         DC    X'52B03B020EEBC751017F001C01'                                    
         DC    X'52B03B020EEBC751017F001D01'                                    
         DC    X'52B03B020EEBC751017F001E01'                                    
         DC    X'52B03B020EEBC751017F001F01'                                    
         DC    X'52B03B020EEBC751017F002001'                                    
         DC    X'52B03B020EEBC751017F002101'                                    
         DC    X'52B03B020EEBC751017F002201'                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPLDEXTAA 09/20/00'                                      
         END                                                                    
