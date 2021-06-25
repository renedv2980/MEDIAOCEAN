*          DATA SET SPLDEXTME2 AT LEVEL 007 AS OF 02/18/00                      
*PHASE SPEXTME2                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXTMEL - FIX BAD CANADIAN 6A ELEMENTS'                      
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
         CLI   BUYKAM,X'11'        SPOT BUY?                                    
         BL    DMXKEEP             NO                                           
*                                                                               
         MVI   MYFLAG,0                                                         
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'6A'                                                     
         MVI   ELCDHI,X'6A'                                                     
*                                                                               
XR2      BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
*                                                                               
         CLI   2(R6),C' '                                                       
         BH    DMXKEEP                                                          
         MVI   2(R6),C'S'                                                       
*                                                                               
XR10     DS    0H                                                               
*&&DO                                                                           
         GOTO1 VCLUNPK,DMCB,BUYKCLT,P+5                                         
         GOTO1 LHEXOUT,DMCB,BUYKPRD,P+9,1,=C'TOG'                               
         EDIT  (B1,BUYKEST),(3,P+12),FILL=0                                     
         EDIT  (B1,BUYKBUY),(3,P+30),FILL=0                                     
*                                                                               
         GOTO1 LHEXOUT,DMCB,BUYMSTA,P+60,5                                      
         GOTO1 VPRINTER                                                         
*&&                                                                             
         CLC   COUNT,=F'10'                                                     
         BH    DMXKEEP                                                          
         L     R6,AREC                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,=C'BAD 6A',AREC,C'DUMP',(R0),=C'1D00',  X        
               (C'P',LPRINT)                                                    
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
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
**PAN#1  DC    CL21'007SPLDEXTME202/18/00'                                      
         END                                                                    
