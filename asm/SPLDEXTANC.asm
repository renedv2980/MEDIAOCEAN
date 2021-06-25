*          DATA SET SPLDEXTANC AT LEVEL 101 AS OF 02/25/00                      
*PHASE SPEXTANC,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - FIND NON-POL RADIO BUYS WITH ZERO SPOT ELEMS'         
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         MVI   CHANGED,C'N'                                                     
         USING SPGENBUYD,R3                                                     
         L     R3,AREC             POINT TO RECORD                              
         CLI   0(R3),X'12'         YNR RADIO BUYS                               
         BNE   DMXKEEP                                                          
         CLI   3(R3),X'FF'         SKIP POL                                     
         BE    DMXKEEP                                                          
         CLI   10(R3),X'FF'                                                     
         BE    DMXKEEP                                                          
*                                                                               
         LA    R6,BDELEM                                                        
DMXR10   CLI   0(R6),0                                                          
         BE    DMXR30                                                           
         CLI   0(R6),6                                                          
         BE    DMXR20                                                           
DMXR15   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DMXR10                                                           
*                                                                               
DMXR20   CLI   7(R6),0                                                          
         BNE   DMXR15                                                           
*                                                                               
         MVC   P(8),=C'DEL ELEM'                                                
         GOTO1 =V(HEXOUT),DMCB,(R6),P+10,12                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(RECUP),DMCB,(C'S',(R3)),(R6),0     DELETE ELEM                
         MVI   CHANGED,C'Y'                                                     
         B     DMXR10                                                           
*                                                                               
DMXR30   CLI   CHANGED,C'Y'                                                     
         BNE   DMXKEEP                                                          
         BAS   RE,BUYPRNT                                                       
         AP    NUMBUYS,=P'1'                                                    
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
BUYPRNT  NTR1                                                                   
         CP    NUMBUYS,=P'20'                                                   
         BH    BPX                                                              
BP10     LA    R4,=CL20'BUY RECORD'                                             
BP20     SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
BPX      XIT1                                                                   
*                                                                               
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF BUYS ='                                       
         EDIT  (P10,NUMBUYS),(15,P+25)                                          
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
NEXTEST  DC    F'0'                                                             
LASTEST  DC    F'0'                                                             
DATADISP DC    H'0024'                                                          
NUMBUYS  DC    PL10'0'                                                          
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
**                                                                              
SVCLIST  DS    CL880                                                            
ESTTAB   DS    3000CL6                                                          
**                                                                              
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
CHANGED  DS    XL1                                                              
ESTOWSDY DS    CL1                                                              
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORK2    DS    CL64                                                             
NUMRECS  DS    PL6                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENBUY                                                                       
SPGENBUYD      DSECT                                                            
       ++INCLUDE SPGENBUY                                                       
*SPGENEST                                                                       
SPGENESTD      DSECT                                                            
       ++INCLUDE SPGENEST                                                       
*SPGENCLT                                                                       
SPGENCLTD      DSECT                                                            
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101SPLDEXTANC02/25/00'                                      
         END                                                                    
