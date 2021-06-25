*          DATA SET SPLDEXTSC1 AT LEVEL 075 AS OF 10/05/99                      
*PHASE SPEXTSC                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXTSC - FIX BAD BUY RECORDS'                                
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
         L     R8,12(R1)           DMLDDEFN                                     
         USING LDDEFND,R8                                                       
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
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
         L     RF,=V(CLUNPK)                                                    
         ST    RF,VCLUNPK                                                       
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R5,AREC             THIS IS FOR RECUP CALLS                      
         SR    R0,R0               MAKE SURE 2 BYTES BEYOND REC'S 00            
         ICM   R0,3,13(R5)                                                      
         AR    R5,R0                                                            
         XC    0(2,R5),0(R5)                                                    
*                                                                               
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
         CLI   BUYKAM,X'11'        SPOT BUY?                                    
         BL    DMXKEEP             NO                                           
         GOTO1 LHEXOUT,DMCB,0(R6),P+2,13,=C'TOG'                                
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'F1'        SEARCHING FOR X'F1' ELEMENT                  
         MVI   ELCDHI,X'F1'                                                     
*                                                                               
DMXREC1  BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC2  CLC   COUNT,=F'100'                                                    
         BH    DMXREC5                                                          
         GOTO1 LHEXOUT,DMCB,0(R6),P+30,20,=C'TOG'                               
         GOTO1 LPRINTER                                                         
*                                                                               
DMXREC5  L     R1,COUNT                                                         
         AHI   R1,1                                                             
         ST    R1,COUNT                                                         
*                                                                               
DMXREC10 GOTO1 =V(RECUP),DMCB,(C'S',AREC),(R6),0                                
*                                                                               
         CLC   COUNT,=F'10'                                                     
         BH    DMXREC20                                                         
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VCLUNPK,DMCB,BUYKCLT,P+5                                         
         GOTO1 LHEXOUT,DMCB,BUYKPRD,P+9,1,=C'TOG'                               
         EDIT  (B1,BUYKEST),(3,P+12),FILL=0                                     
         EDIT  (B1,BUYKBUY),(3,P+30),FILL=0                                     
*                                                                               
         GOTO1 LHEXOUT,DMCB,BUYMSTA,P+60,5,=C'TOG'                              
         GOTO1 LPRINTER                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R5)                                                      
         GOTO1 =V(PRNTBL),DMCB,C'BADREC',AREC,C'DUMP',(R0),=C'1D00',   X        
               (C'P',LPRINT)                                                    
DMXREC20 BAS   RE,NEXTEL2                                                       
         BNE   DMXKEEP                                                          
         B     DMXREC2                                                          
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
VCLUNPK  DS    A                                                                
SVEOR    DS    F                                                                
COUNT    DS    F                   NUMBER OF CLIENT RECORDS CHANGED             
BYTE     DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVBYKEY  DS    XL13                                                             
ELEM     DS    XL100                                                            
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
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
**PAN#1  DC    CL21'075SPLDEXTSC110/05/99'                                      
         END                                                                    
