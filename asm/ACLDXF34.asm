*          DATA SET ACLDXF34   AT LEVEL 009 AS OF 02/05/02                      
*PHASE ACLDXF34                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACRECTYP                                                               
         TITLE 'FIND PARTICULAR ELEMENT'                                        
*                                                                               
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
ACLDXF34 CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         ZAP   PRNTRECS,=P'0'                                                   
         ZAP   BADLEN,=P'0'                                                     
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
         USING TRNRECD,R3                                                       
DMXREC   L     R3,AREC                                                          
         GOTO1 RECTYP,DMCB,(C'D',TRNRECD)                                       
         CLI   0(R1),ACRTTRN                                                    
         BNE   DMXKEEP                                                          
         CLC   TRNKUNT(2),=C'SJ'                                                
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   TRNKDATE,=X'980101'                                              
         BL    DMXKEEP                                                          
                                                                                
         CLC   TRNKDATE,=X'A11231'                                              
         BH    DMXKEEP                                                          
*                                                                               
         USING BSCELD,R2                                                        
         LA    R2,TRNRFST                                                       
         AP    PRNTRECS,=P'1'                                                   
*                                                                               
DMXREC0  CLI   0(R2),0                                                          
         BE    DMXKEEP                                                          
         CLI   BSCEL,BSCELQ                                                     
         BNE   DMXREC2                                                          
*                                                                               
         CLI   BSCLN,16                                                         
         BE    DMXREC2                                                          
*                                                                               
         BAS   RE,DMPGET                                                        
         AP    BADLEN,=P'1'                                                     
*                                                                               
DMXREC2  SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DMXREC0                                                          
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         ZAP   LINE,=P'99'                                                      
         MVI   P,C' '                                                           
         GOTO1 VPRINTER                                                         
         MVC   P(14),=C'BSCEL ELEMENTS'                                         
         UNPK  P+17(6),PRNTRECS                                                 
         OI    P+22,X'F0'                                                       
         GOTO1 VPRINTER                                                         
         MVI   P,C' '                                                           
         MVC   P(12),=C'WRONG LENGTH'                                           
         UNPK  P+17(6),BADLEN                                                   
         OI    P+22,X'F0'                                                       
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DMPGET   NTR1  ,                                                                
         L     R9,AREC                                                          
         LA    R7,=C'GET'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                   
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VPRNTBL  DC    V(PRNTBL)                                                        
RECTYP   DC    V(ACRECTYP)                                                      
HEXOUT   DC    V(HEXOUT)                                                        
PRNTRECS DC    PL4'0'                                                           
BADLEN   DC    PL4'0'                                                           
DATADISP DC    H'56'                                                            
         EJECT                                                                  
KEYPRT   DS    0C                                                               
         DC    C',ACT='                                                         
KACT     DS    CL12                                                             
         DC    C',WC='                                                          
KWORK    DS    CL2                                                              
KEYPRTL  EQU   *-KEYPRT                                                         
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* ACOPTEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACOPTEQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACLDXF34  02/05/02'                                      
         END                                                                    
