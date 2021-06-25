*          DATA SET TSLDEXT    AT LEVEL 012 AS OF 05/01/02                      
*PHASE TSLDEXT,+0                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE MSUNPK                                                                 
*INCLUDE CLUNPK                                                                 
         TITLE 'TSLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
TSLDEXT  CSECT                                                                  
         NMOD1 20,TSLDEXT                                                       
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
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         L     R9,VPRINTER         ADDRESS PRINTER                              
*                                                                               
         CLI   0(R3),X'10'                                                      
         BL    DMXKEEP             ONLY RECS WITH AGENCY CODE                   
         XC    CURDATE,CURDATE     CLEAR DATE FIELD                             
*                                                                               
NEXTELE  BAS   RE,GETEL            FIND AN ELEMENT                              
         BNE   DMXKEEP                                                          
         CLC   CURDATE,2(R3)                                                    
         BH    PRINTREC            REC HAS ELEMENTS OUT OF SEQ                  
         MVC   CURDATE,2(R3)                                                    
         B     NEXTELE                                                          
*                                                                               
PRINTREC MVC   TITLE(L'LINE1),LINE1                                             
         L     R3,AREC                                                          
         MVC   P+34(L'LINE2),LINE2                                              
         MVC   LINE,=PL2'75'                                                    
         GOTO1 (R9)                PRINT HEADING                                
         MVC   P+38(2),20(R3)      AGENCY CODE                                  
         ZIC   R4,0(R3)                                                         
         SLL   R4,28                                                            
         SRL   R4,28                                                            
         BCTR  R4,0                                                             
         LA    R5,MEDIATAB                                                      
         AR    R4,R5                                                            
         MVC   P+46(1),0(R4)       MEDIA CODE                                   
         LA    R4,1(R3)                                                         
         GOTO1 =V(CLUNPK),DMCB,(R4),P+52   CLIENT CODE                          
         LA    R4,3(R3)                                                         
         GOTO1 =V(HEXOUT),DMCB,(R4),P+62,1,=C'MIX'   PRODUCT CODE               
         LA    R4,4(R3)                                                         
         GOTO1 =V(MSUNPK),DMCB,(R4),P+68,P+76    MARKET AND STATION             
         ZIC   R4,9(R3)                                                         
         CVD   R4,DUB                                                           
         UNPK  P+88(3),DUB         ESTIMATE NUMBER                              
         OI    P+90,X'F0'                                                       
         ZIC   R4,10(R3)                                                        
         CVD   R4,DUB                                                           
         UNPK  P+94(3),DUB         LINE NUMBER                                  
         OI    P+96,X'F0'                                                       
         GOTO1 (R9)                PRINT KEY LINE                               
         GOTO1 (R9)                                                             
*                                                                               
PRINT2   BAS   RE,GETEL            GET AN ELEMENT                               
         BNE   DMXKEEP                                                          
         LA    R4,P                                                             
         ZIC   R5,1(R3)                                                         
         LR    R6,R3                                                            
PRINT3   GOTO1 =V(HEXOUT),DMCB,(R6),(R4),1,=C'MIX'   A BYTE                     
         LA    R6,1(R6)                                                         
         LA    R4,3(R4)                                                         
         BCT   R5,PRINT3                                                        
         GOTO1 (R9)                PRINT THE ELEMENT                            
         B     PRINT2                                                           
         EJECT                                                                  
GETEL    EQU   *                                                                
*                                                                               
         C     R3,AREC                                                          
         BNE   NEXTEL                                                           
         LA    R3,24(R3)           BEGINNING OF RECORD - GET PAST KEY           
GETEL1   CLI   0(R3),0                                                          
         BNE   GETEL2                                                           
         CLI   0(R3),1             END OF ELEMENTS -- FORCE NE                  
         B     GETELX                                                           
*                                                                               
GETEL2   CLI   0(R3),6             ACCEPT ONLY 06-08 AND 0B-0D                  
         BL    NEXTEL                                                           
         CLI   0(R3),8                                                          
         BNH   GETEL3                                                           
         CLI   0(R3),11                                                         
         BL    NEXTEL                                                           
         CLI   0(R3),13                                                         
         BNH   GETEL3                                                           
*                                                                               
NEXTEL   SR    RF,RF               FIND NEXT ELEMENT                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     GETEL1                                                           
*                                                                               
GETEL3   CR    RB,RB               ELEMENT FOUND -- FORCE EQUAL                 
*                                                                               
GETELX   BR    RE                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
CURDATE  DS    CL2                                                              
ELCODE   DC    X'00'                                                            
MEDIATAB DC    CL4'TRNX'                                                        
LINE1    DC    CL50'SPTFIL2 RECORDS WITH ELEMENTS OUT OF DATE SEQUENCE'         
LINE2    DC    CL63'AGENCY  MEDIA  CLIENT  PRODUCT  MARKET  STATION  ESX        
               TIMATE  LINE'                                                    
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
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
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012TSLDEXT   05/01/02'                                      
         END                                                                    
