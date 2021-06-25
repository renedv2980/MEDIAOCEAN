*          DATA SET PREXTPA    AT LEVEL 042 AS OF 10/12/94                      
*PHASE PREXTPA,+0                                                               
*INCLUDE BINSRCH                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
*                                                                               
***********                                                                     
****   THIS EXTERN WILL DELETE ALL RECORDS FOR PATO (PA)                        
****   FOR MEDIA 'S'                                                            
***********                                                                     
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
DMLDEXT  CSECT                                                                  
         NMOD1 80,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
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
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
         CLC   0(3,R3),=C'PAS'     ONLY CHECK AGENCY PA AND MEDIA S             
         BNE   DMXKEEP                                                          
         AP    RECDEL,=P'1'                                                     
         LA    R6,BUYCNT                                                        
         CLI   3(R3),X'20'       BUYRECS                                        
         BE    DMXREC5                                                          
         LA    R6,CLTCNT                                                        
         CLI   3(R3),X'02'       CLIENTS                                        
         BE    DMXREC5                                                          
         LA    R6,PRDCNT                                                        
         CLI   3(R3),X'06'       PRODUCTS                                       
         BE    DMXREC5                                                          
         LA    R6,ESTCNT                                                        
         CLI   3(R3),X'07'       ESTIMATESS                                     
         BE    DMXREC5                                                          
         LA    R6,BILLCNT                                                       
         CLI   3(R3),X'08'       BILLS                                          
         BE    DMXREC5                                                          
         LA    R6,OTHCNT        OTHER RECORDS                                   
DMXREC5  AP    0(4,R6),=P'1'                                                    
         B     DMXPURGE                                                         
*                                                                               
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                  EXIT WITH CC EQ                              
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT EQ                          
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         LA    R3,COUNTS                                                        
         LA    R4,NCOUNTS                                                       
*                                                                               
DMXEOF2  OI    3(R3),X'0F'                                                      
         UNPK  P(7),0(4,R3)                                                     
         MVC   P+10(20),4(R3)                                                   
         LA    R3,24(R3)                                                        
         GOTO1 VPRINTER                                                         
         BCT   R4,DMXEOF2                                                       
*                                                                               
*                                                                               
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
COUNTS   DS    0CL24                                                            
RECDEL   DC    PL4'0',CL20' RECORDS DELETED'                                    
CLTCNT   DC    PL4'0',CL20' CLIENTS DELETED'                                    
PRDCNT   DC    PL4'0',CL20'PRODUCTS DELETED'                                    
ESTCNT   DC    PL4'0',CL20'    ESTS DELETED'                                    
BILLCNT  DC    PL4'0',CL20'   BILLS DELETED'                                    
BUYCNT   DC    PL4'0',CL20' BUYRECS DELETED'                                    
OTHCNT   DC    PL4'0',CL20'   OTHER DELETED'                                    
NCOUNTS  EQU   (*-COUNTS)/L'COUNTS                                              
         DS    F                                                                
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
HEXBLOCK DS    CL500                                                            
         SPACE 2                                                                
WORKD    DSECT                                                                  
WORK     DS    CL30                                                             
ELEM     DS    CL50                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
HALF     DS    H                                                                
CHGSW    DS    CL1                                                              
ERR      DS    CL1         1=NOT BILLED     FOR ==P BUYS                        
*                          2=ALREADY PAID   FOR ==P BUYS                        
*                          3=NO EMPTY PAY ELEM FOR ==P BUYS                     
*                                                                               
*                          5=NOT PAID       FOR ==B BUYS                        
*                          6=ALREADY BILLED FOR ==B BUYS                        
*                          7=NO EMPTY BILL ELEM FOR ==B BUYS                    
*                                                                               
*                          C= CLIENT CHANGED                                    
*                                                                               
*                          TOTALED FOR ELEMS                                    
BPGRS    DS    F                                                                
BPAGYC   DS    F                                                                
BPCD     DS    F                                                                
*                                                                               
BBGRS    DS    F                                                                
BBAGYC   DS    F                                                                
BBCD     DS    F                                                                
*                                                                               
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
       ++INCLUDE PPAYELEM                                                       
       ++INCLUDE PBILELEM                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042PREXTPA   10/12/94'                                      
         END                                                                    
