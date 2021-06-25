*          DATA SET STLDEXTKW  AT LEVEL 008 AS OF 10/20/00                      
*PHASE STEXTKW                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTAG - DELETE NON K/W STATIONS FOR FV'                     
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
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
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
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
                                                                                
         L     R6,AREC                                                          
         CLC   0(2,R6),=C'AT'      TV ADDRESS RECORD?                           
         BE    DMXREC2                                                          
         CLC   0(2,R6),=C'AR'      RADIO ADDRESS RECORD?                        
         BE    DMXREC2                                                          
*                                                                               
         CLC   0(2,R6),=C'ST'      TV MASTER RECORD ?                           
         BE    DMXREC2                                                          
         CLC   0(2,R6),=C'SR'      RADIO MASTER RECORD ?                        
         BE    DMXREC2                                                          
         B     DMXKEEP             KEEP EVERYTHING ELSE                         
*                                                                               
DMXREC2  CLC   7(2,R6),=C'FV'      IS IT THE RIGHT AGY?                         
         BNE   DMXKEEP                                                          
                                                                                
         CLI   2(R6),C'K'          STATION STARTS WITH K,W                      
         BE    DMXKEEP                                                          
         CLI   2(R6),C'W'                                                       
         BE    DMXKEEP                                                          
         B     DMXPURGE                                                         
                                                                                
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
BYTE     DS    X                                                                
ACOUNT   DS    F                                                                
MCOUNT   DS    F                                                                
SCOUNT   DS    F                                                                
ELCODE   DS    X                                                                
RECLEN   DS    H                                                                
         DC    H'0'                                                             
IO       DS    XL2000                                                           
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008STLDEXTKW 10/20/00'                                      
         END                                                                    
