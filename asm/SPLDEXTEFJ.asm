*          DATA SET SPLDEXTEFJ AT LEVEL 024 AS OF 10/28/98                      
*PHASE SPEXTEFJ                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTEFJ - FIND ALL BUY 70 ELEMS WITH TBD4'                   
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
         XR    R0,R0                                                            
         LA    R4,BDELEM                                                        
DMXR2    CLI   0(R4),0                                                          
         BE    DMXRX                                                            
         CLI   0(R4),X'70'                                                      
         BE    DMXR4                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DMXR2                                                            
*                                                                               
DMXR4    CLC   3(3,R4),=C'TBD'                                                  
         BNE   DMXRX                                                            
         MVC   P+70(12),3(R4)                                                   
*                                                                               
         MVC   P(2),BUYALPHA       AGENCY POWER CODE                            
         MVI   P+2,C'/'                                                         
*                                                                               
         MVC   BYTE,BUYKAM         MEDIA                                        
         NI    BYTE,X'0F'                                                       
         MVI   P+3,C'T'                                                         
         CLI   BYTE,X'01'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'R'                                                         
         CLI   BYTE,X'02'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'N'                                                         
         CLI   BYTE,X'03'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'X'                                                         
         CLI   BYTE,X'04'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'?'                                                         
*                                                                               
DMXR10   MVI   P+4,C'/'                                                         
         GOTO1 VCLUNPK,DMCB,BUYKCLT,P+5                                         
         MVI   P+8,C'/'                                                         
         GOTO1 VHEXOUT,DMCB,BUYKPRD,P+9,1                                       
         MVI   P+11,C'/'                                                        
         EDIT  (B1,BUYKEST),(3,P+12),FILL=0                                     
         MVI   P+15,C'/'                                                        
         MVI   P+20,C'/'                                                        
*         GOTO1 VMSUNPK,DMCB,(X'80',BUYMSTA),P+16,P+21                          
         MVI   P+29,C'/'                                                        
         EDIT  (B1,BUYKBUY),(3,P+30),FILL=0                                     
*                                                                               
         MVC   P+50(8),=CL8'HEX STA:'                                           
         GOTO1 VHEXOUT,DMCB,BUYMSTA+2,P+60,3                                    
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
DMXRX    B     DMXKEEP             KEEP IT                                      
         EJECT                                                                  
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
COUNT    DS    F                   NUMBER OF CLIENT RECORDS CHANGED             
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
VCLUNPK  DS    A                                                                
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
**PAN#1  DC    CL21'024SPLDEXTEFJ10/28/98'                                      
         END                                                                    
