*          DATA SET PREXTPUC   AT LEVEL 065 AS OF 05/12/99                      
*PHASE PREXTPUC,+0                                                              
*INCLUDE BINSRCH                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PUBRECD,R3                                                       
*                                                                               
         MVI   CHGSW,0                                                          
*                                                                               
         CLI   PUBKMED,C'M'        MEDIA M?                                     
         BE    DMXR30                                                           
         CLI   PUBKMED,C'S'        MEDIA S?                                     
         BE    DMXR30                                                           
         CLI   PUBKMED,C'T'        MEDIA T?                                     
         BNE   DMXKEEP                                                          
*                                                                               
******** CLC   PUBKAGY,=C'SJ'      FOR TEST PURPOSES                            
DMXR30   CLC   PUBKAGY,=C'YN'                                                   
         BNE   DMXKEEP                                                          
         CLI   PUBKCOD,X'81'                                                    
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NEXTEL                                                        
         BE    DMXR120                                                          
*                                                                               
         XC    ELEM,ELEM           BUILDING PRODUCTION (X'20') ELEM             
         LA    R4,ELEM                                                          
         USING PUBGENEL,R4                                                      
*                                                                               
         MVI   PUBGENEL,X'20'      PRODUCTION ELEM CODE                         
         MVI   PUBGENEL+1,X'32'    PRODUCTION ELEM LENGTH                       
*                                                                               
         ZAP   PUBCD,=P'0'                                                      
         ZAP   PUBCDDAS,=P'0'                                                   
*                                                                               
         ZAP   PUBCLMO,=P'0'                                                    
         ZAP   PUBCLDA,=P'0'                                                    
*                                                                               
         ZAP   PUBOSMO,=P'-1'                                                   
         ZAP   PUBOSDA,=P'10'                                                   
         ZAP   PUBPAYMO,=P'0'                                                   
         ZAP   PUBPAYDA,=P'0'                                                   
*                                                                               
         ZAP   PUBFD,=P'0'                                                      
*                                                                               
         ZAP   PUBAC,=P'15000'                                                  
*                                                                               
         ZAP   PUBMCLMO,=P'0'                                                   
         ZAP   PUBMCLDA,=P'0'                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         LA    R7,PUBREC+33                                                     
         CLI   0(R7),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0               X'20' ELEM INSERTING POSITION                
*                                                                               
         GOTO1 =V(RECUP),DMCB,(1,PUBREC),ELEM,0(R7)                             
*                                                                               
         AP    NOX20CNT,=P'1'      PRODUCTION ELEM JUST BEEN ADDED              
         CP    NOX20CNT,=P'10'     ONLY PRINT FIRST TEN RECORDS                 
         BH    *+8                                                              
         BAS   RE,DDSTRACE                                                      
         B     DMXKEEP                                                          
*                                                                               
         USING PUBGENEL,R6                                                      
*                                                                               
DMXR120  CP    PUBOSMO,=P'0'                                                    
         BNE   DMXKEEP                                                          
         CP    PUBOSDA,=P'0'                                                    
         BNE   DMXKEEP                                                          
*                                                                               
         ZAP   PUBOSMO,=P'-1'                                                   
         ZAP   PUBOSDA,=P'10'                                                   
         MVI   CHGSW,1             RECORD HAS BEEN CHANGED                      
*                                                                               
         DROP  R6                                                               
*                                                                               
DMXR150  DS    0H                                                               
         CLI   CHGSW,1             SEE IF RECORD CHANGED                        
         BNE   DMXKEEP                                                          
         AP    CHGCOUNT,=P'1'                                                   
         CP    CHGCOUNT,=P'10'     ONLY PRINT FIRST TEN RECORDS                 
         BH    *+8                                                              
         BAS   RE,DDSTRACE                                                      
         B     DMXKEEP                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
*                                                                               
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
*                                                                               
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         BAS   R4,BLANKLN                                                       
         BAS   R4,BLANKLN                                                       
         BAS   R4,BLANKLN                                                       
*                                                                               
         MVC   P(24),=C'# OF RECORD(S) CHANGED: '                               
         EDIT  (P4,CHGCOUNT),(9,P+24),COMMAS=YES,ALIGN=LEFT                     
         GOTO1 VPRINTER                                                         
         BAS   R4,BLANKLN                                                       
         MVC   P(44),=C'# OF RECORD(S) W/ PRODUCTION ELEMENT ADDED: '           
         EDIT  (P4,NOX20CNT),(9,P+44),COMMAS=YES,ALIGN=LEFT                     
         GOTO1 VPRINTER                                                         
         BAS   R4,BLANKLN                                                       
*                                                                               
         B     DMXIT                                                            
*                                                                               
BLANKLN  MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         BR    R4                                                               
*                                                                               
*                                                                               
*                                                                               
CHGCOUNT DC    PL4'0'              NUMBER OF RECORDS CHANGED                    
NOX20CNT DC    PL4'0'              NO PRODUCTION ELEM (X'20') COUNTER           
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                    SUBROUTINE PRINTS ELEMENT TRACE                  *         
***********************************************************************         
*                                                                               
         USING  PUBRECD,R3                                                      
*                                                                               
DDSTRACE NTR1                                                                   
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,(R3),P,33                                        
         MVC   P+66(3),SPACES                                                   
*                                                                               
         LR    R6,R3                                                            
         AHI   R6,33               POINT TO ELEMENTS                            
*                                                                               
DTRNXEL  SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    DTRNXELX                                                         
         CLC   0(1,R6),ELCODE                                                   
         BE    DTR50               PRINT PORTION OF FOUND ELEMENT               
         B     DTRNXEL                                                          
*                                                                               
DTRNXELX DS    0H                                                               
         MVC   P+66+3(33),=C'ERR: PRODUCTION ELEMENT NOT FOUND'                 
         B     DTRPRTIT                                                         
*                                                                               
DTR50    GOTO1 =V(HEXOUT),DMCB,(R6),P+66+3,30                                   
*                                                                               
         CLI   CHGSW,0             SEE IF PRODUCTION ELEM IS ADDED              
         BNE   *+8                                                              
         MVI   P+131,C'*'          FLAG RECORD ON OUTPUT                        
*                                                                               
DTRPRTIT GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         DROP   R3                                                              
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
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
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBGENEL                                                       
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
**PAN#1  DC    CL21'065PREXTPUC  05/12/99'                                      
         END                                                                    
