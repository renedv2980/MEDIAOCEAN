*          DATA SET SXLDEXTAN2 AT LEVEL 109 AS OF 06/08/00                      
*PHASE SXEXTAN2,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - XSP FILES - DELETE E8 ELEM FROM MCT INVOICES'         
*                                                                               
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
         USING SNVKEYD,R3                                                       
         L     R3,AREC             POINT TO RECORD                              
         CLC   =X'0E03',0(R3)      INVOICE RECORD                               
         BNE   DMXKEEP                                                          
         TM    2(R3),X'10'         WILA                                         
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,42(R3)           FIRST ELEM                                   
DMX10    CLI   0(R2),0             EOR                                          
         BE    DMXKEEP                                                          
         CLI   0(R2),X'10'         STATUS ELEM                                  
         BE    DMX20                                                            
         CLI   0(R2),X'E8'         MM ELEM                                      
         BE    DMX30                                                            
DMXNXTEL ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DMX10                                                            
*                                                                               
         USING SNVHDELD,R2                                                      
DMX20    MVI   MCTINV,C'N'         PRESET TO NO                                 
         TM    SNVHDCTL,SNVHDMCQ   MCT INVOICE                                  
         BNO   *+8                                                              
         MVI   MCTINV,C'Y'                                                      
         B     DMXNXTEL                                                         
         DROP  R2                                                               
*                                                                               
DMX30    CLI   MCTINV,C'Y'         IS IT A MCT INVOICE                          
         BNE   DMXKEEP                                                          
*                                                                               
*        MVC   P(4),=C'KEY='                                                    
*        GOTO1 =V(HEXOUT),DMCB,(R3),P+5,32                                      
*        MVC   P+72(8),=C'DEL ELEM'                                             
*        GOTO1 =V(HEXOUT),DMCB,(R2),P+81,12                                     
*        GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(RECUP),DMCB,(X'FE',(R3)),(R2),0,XRECUP                        
*                                                                               
         AP    NUMRECS,=P'1'                                                    
*        BAS   RE,RECPRNT                                                       
         B     DMXKEEP                                                          
*                                                                               
RECPRNT  NTR1                                                                   
         CP    NUMRECS,=P'10'                                                   
         BH    RPX                                                              
         LA    R4,=CL20'CHANGED REC'                                            
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,32(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
RPX      XIT1                                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF RECS ='                                       
         EDIT  (P8,NUMRECS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0024'                                                          
*                                                                               
*                                                                               
NUMRECS  DC    PL8'0'                                                           
XRECUP   DC    H'42'                                                            
         DC    H'32'                                                            
         DC    H'2000'                                                          
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
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
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
BYTE     DS    XL1                                                              
NUMBUYS  DS    PL6                                                              
MCTINV   DS    CL1                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENCLT                                                                       
SPGENCLTD      DSECT                                                            
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENSNV                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109SXLDEXTAN206/08/00'                                      
         END                                                                    
