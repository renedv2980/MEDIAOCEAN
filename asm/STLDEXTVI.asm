*          DATA SET STLDEXTVI  AT LEVEL 001 AS OF 06/12/95                      
*PHASE STLDVI                                                                   
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXTVI - COPY STATION FILE TBSNY -> VMNY'                    
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
         OPEN  (TOUT,OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
                                                                                
         L     R6,AREC                                                          
         CLC   0(2,R6),=C'AR'      RADIO ADDRESS RECORD?                        
         BE    DMXREC3                                                          
         CLC   0(2,R6),=C'AT'      T.V.  ADDRESS RECORD?                        
         BNE   DMXREC10                                                         
DMXREC3  CLC   7(2,R6),=C'TY'      IS IT TBSNY?                                 
         BNE   DMXKEEP                                                          
                                                                                
         CLI   2(R6),C'K'          STATION STARTS WITH K,W,OR X?                
         BE    DMXREC5                                                          
         CLI   2(R6),C'W'                                                       
         BNE   DMXKEEP                                                          
                                                                                
DMXREC5  MVC   7(2,R6),=C'VI'      CHANGE TO VMNY                               
         B     DMXREC30                                                         
                                                                                
DMXREC10 CLC   0(2,R6),=C'MR'      RADIO MARKET RECORD?                         
         BE    DMXREC15                                                         
         CLC   0(2,R6),=C'MT'      T.V. MARKET RECORD?                          
         BNE   DMXREC20                                                         
DMXREC15 CLC   6(2,R6),=C'TY'      IS IT TBSNY?                                 
         BNE   DMXKEEP                                                          
                                                                                
         MVC   6(2,R6),=C'VI'      CHANGE TO VMNY                               
         B     DMXREC30                                                         
                                                                                
DMXREC20 CLC   0(2,R6),=C'SR'      RADIO STATION RECORD?                        
         BE    DMXREC23                                                         
         CLC   0(2,R6),=C'ST'      T.V. STATION RECORD?                         
         BNE   DMXKEEP                                                          
                                                                                
DMXREC23 CLC   7(2,R6),=C'TY'      IS IT TBSNY?                                 
         BNE   DMXKEEP                                                          
                                                                                
         CLC   9(3,R6),=C'000'     CLIENT CODE 000                              
         BNE   DMXKEEP                                                          
                                                                                
         CLI   2(R6),C'K'          STATION STARTS WITH K OR W?                  
         BE    DMXREC25                                                         
         CLI   2(R6),C'W'                                                       
         BNE   DMXKEEP                                                          
                                                                                
DMXREC25 MVC   7(2,R6),=C'VI'      CHANGE TO VMNY                               
         USING STARECD,R6                                                       
         MVC   SREP,=C'000000000'  REPS = 0                                     
         DROP  R6                                                               
                                                                                
*                                                                               
DMXREC30 L     R1,AREC                                                          
         SH    R1,=H'4'                                                         
         ZICM  R2,15(R6),2         GET RECORD LENGTH                            
         LA    R2,4(R2)                                                         
         SLL   R2,16                                                            
         ST    R2,0(R1)                                                         
*                                                                               
         LR    R0,R1                                                            
         PUT   TOUT,(0)            PUT TO A TAPE                                
                                                                                
         B     DMXPURGE                                                         
                                                                                
DMXEOF   DS    0H                                                               
         CLOSE TOUT                                                             
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=4004,BLKSIZE=16000                                
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
**PAN#1  DC    CL21'001STLDEXTVI 06/12/95'                                      
         END                                                                    
