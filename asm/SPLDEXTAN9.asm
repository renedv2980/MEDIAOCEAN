*          DATA SET SPLDEXTAN9 AT LEVEL 093 AS OF 12/03/99                      
*PHASE SPEXTAN2,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - PEEL AGY/CLT/PRD AND 06-09 RECS OFF FILE'             
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
         L     R3,AREC             POINT TO RECORD                              
         CLC   =X'11AEA0',0(R3)                                                 
         BE    DMX10                                                            
         CLC   =X'11886D',0(R3)                                                 
         BE    DMX20                                                            
         CLC   =X'11CE2D',0(R3)                                                 
         BE    DMX30                                                            
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
DMX10    DS    0H                                                               
         LA    R4,=CL20'PURGE BUY'                                              
         AP    NUMLVAS,=P'1'                                                    
         CP    NUMLVAS,=P'10'                                                   
         BH    *+8                                                              
         BAS   RE,PRTKEY                                                        
         B     DMXPURGE                                                         
*                                                                               
DMX20    DS    0H                                                               
         LA    R4,=CL20'PURGE BUY'                                              
         AP    NUMCDNS,=P'1'                                                    
         CP    NUMCDNS,=P'10'                                                   
         BH    *+8                                                              
         BAS   RE,PRTKEY                                                        
         B     DMXPURGE                                                         
*                                                                               
DMX30    DS    0H                                                               
         LA    R4,=CL20'PURGE BUY'                                              
         AP    NUMTRNS,=P'1'                                                    
         CP    NUMTRNS,=P'10'                                                   
         BH    *+8                                                              
         BAS   RE,PRTKEY                                                        
         B     DMXPURGE                                                         
*                                                                               
*                                                                               
PRTKEY   NTR1                                                                   
*        SR    R5,R5               PRINT OUT RECORD                             
*        ICM   R5,3,13(R3)                                                      
         LA    R5,20                                                            
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
*                                                                               
PRTREC   NTR1                                                                   
         CP    NUMRECS,=P'10'                                                   
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
         MVC   P(16),=C'NUMBER OF LVA  ='                                       
         EDIT  (P6,NUMLVAS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         MVC   P(16),=C'NUMBER OF CDN  ='                                       
         EDIT  (P6,NUMCDNS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         MVC   P(16),=C'NUMBER OF TRN  ='                                       
         EDIT  (P6,NUMTRNS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0024'                                                          
*                                                                               
*                                                                               
NUMRECS  DC    PL6'0'                                                           
NUMLVAS  DC    PL6'0'                                                           
NUMCDNS  DC    PL6'0'                                                           
NUMTRNS  DC    PL6'0'                                                           
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093SPLDEXTAN912/03/99'                                      
         END                                                                    
