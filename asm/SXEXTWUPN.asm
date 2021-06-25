*          DATA SET SXEXTWUPN  AT LEVEL 012 AS OF 10/31/96                      
*PHASE SXEXTWU,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE MSPACK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
*                                                                               
* NOV  3,1996                                                                   
* CHANGE STATION CALL LETTERS FROM WUPN TO WPNY                                 
*                             AND  WGGT TO WUPN                                 
*                                                                               
* FOR ALL AGENCIES, ALL XSPOT FILES                                             
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
         NMOD1 20,DMLDEXT,R8,R9,R2                                              
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
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WUPNT',WORK                          
         CLC   WUPNP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WPNYT',WORK                          
         CLC   WPNYP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WGGTT',WORK                          
         CLC   WGGTP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         L     R4,VLDDEFN                                                       
         USING LDDEFND,R4                                                       
         L     R5,LDDDTFD1                                                      
         DROP  R4                                                               
         SPACE                                                                  
         LA    R6,=CL20'LDDEF AREA'                                             
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R4),C'DUMP',128,=C'0D'                
         SPACE                                                                  
         LA    R6,=CL20'DCB AREA'                                               
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R5),C'DUMP',128,=C'0D'                
         SPACE                                                                  
         MVC   SYSTEM,28(R5)    SAVE SYSTEM                                     
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         CLI   0(R3),X'0E'         INV/MSR                                      
         BNE   DMXKEEP              NO                                          
         SPACE                                                                  
         CLI   1(R3),X'03'         STATION INVOICE                              
         BE    STINV                YES.                                        
         SPACE                                                                  
         CLI   1(R3),X'04'         MSR RECS?                                    
         BNE   DMXKEEP              YES.                                        
         SPACE                                                                  
         EJECT                                                                  
* MSR RECORD *                                                                  
         SPACE                                                                  
         BAS   RE,CKBYP            SEE IF BYPASSING THIS AGY                    
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TMSRCT,=P'1'                                                     
         CLC   WPNYP,10(R3)        SHOULD NOT BE ANY                            
         BNE   STMSR04                                                          
         MVC   P(24),=CL24'MSR REC WITH WPNY IN KEY'                            
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         AP    TWPNY,=P'1'                                                      
         AP    MSRCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STMSR04  CLC   WUPNP,10(R3)                                                     
         BNE   STMSR10                                                          
         CLC   COMPKEY,0(R3)       PRINT 1ST OF EVERY GROUP                     
         BE    STMSR08                                                          
         MVC   COMPKEY,0(R3)                                                    
         MVC   P+60(30),=CL30'WUPN KEY - CK TO WPNY FOR SUB'                    
         GOTO1 =V(HEXOUT),DMCB,(R3),P+30,13,0,0                                 
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
STMSR08  MVC   10(3,R3),WPNYP                                                   
         AP    MSRCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'MSR RENAME WUPN'                                        
         B     PRT                                                              
STMSR10  CLC   WGGTP,10(R3)                                                     
         BNE   DMXKEEP                                                          
         MVC   10(3,R3),WUPNP                                                   
         AP    MSRCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'MSR RENAME WGGT'                                        
         B     PRT                                                              
         EJECT                                                                  
* STATION INVOICE RECORD 0E03 *                                                 
         SPACE                                                                  
STINV    DS   0H                                                                
         BAS   RE,CKBYP            SEE IF BYPASSING THIS AGY                    
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TINVCT,=P'1'                                                     
         CLC   WPNYP,5(R3)         SHOULD NOT BE ANY                            
         BNE   STINV04                                                          
         MVC   P(25),=CL25'STA INVOICE - WPNY IN KEY'                           
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,22,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    INVCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STINV04  CLC   WUPNP,5(R3)                                                      
         BNE   STINV10                                                          
         MVC   5(3,R3),WPNYP                                                    
         AP    INVCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA INV WUPN'                                           
         B     PRT                                                              
STINV10  CLC   WGGTP,5(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   5(3,R3),WUPNP                                                    
         AP    INVCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA INV WGGT'                                           
         B     PRT                                                              
         EJECT                                                                  
CKBYP    CLI   SYSTEM,C'F'         THIS INCLUDE JWNY SPT/TRF                    
         BNE   CKBYP10                                                          
         MVC   BYTE,2(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'B0'                                                       
         BR    RE                                                               
CKBYP10  CLI   SYSTEM,C'N'         THIS INCLUDE WI   SPT/TRF                    
         BNER  RE                                                               
         MVC   BYTE,2(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WILA                                         
         BER   RE                                                               
         CLI   BYTE,X'20'          WITO                                         
         BER   RE                                                               
         CLI   BYTE,X'30'          WRLA                                         
         BR    RE                                                               
         EJECT                                                                  
PRT      SR    R5,R5                                                            
         ICM   R5,3,32(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE 3                                                                
GETEL    LA    R6,24(,R6)                                                       
FIRSTEL  CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL   SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL                                                          
         EJECT                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(60),RTITLE                                                     
         GOTO1 VPRINTER                                                         
         LA    R5,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(22),4(R3)                                                    
         EDIT  (P4,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,26(,R3)                                                       
         BCT   R5,DMXEOF10                                                      
         GOTO1 VPRINTER                                                         
         LA    R5,TOTKCTRS                                                      
         LA    R3,TWPNY                                                         
         SPACE                                                                  
DMXEOF20 MVC   P+5(22),4(R3)                                                    
         EDIT  (P4,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,26(,R3)                                                       
         BCT   R5,DMXEOF20                                                      
         SPACE                                                                  
         CLI   SYSTEM,C'F'         THIS INCLUDE JWNY SPT/TRF F                  
         BNE   DMXEOF00                                                         
         MVC   P(13),=C'BYPASSED JWNY'                                          
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
DMXEOF00 CLI   SYSTEM,C'N'         THIS INCLUDE WI   SPT/TRF N                  
         BNE   DMXEOF04                                                         
         MVC   P(13),=C'BYPASSED WI  '                                          
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
DMXEOF04 DS   0H                                                                
         B     DMXIT                                                            
         EJECT                                                                  
SYSTEM   DC    C' '                SAVED SYSTEM NUMBER                          
BYTE     DC    X'00'                                                            
STBAMCLP DC    XL4'00'                                                          
WPNYT    DC    CL4'WPNY'                                                        
WPNYP    DC    XL3'C96422'                                                      
WGGTT    DC    CL4'WGGT'                                                        
WGGTP    DC    XL3'C63622'                                                      
WUPNT    DC    CL4'WUPN'                                                        
WUPNP    DC    XL3'CB2042'                                                      
TOTRD    DC    PL4'0',CL22'TOTAL RECS READ'                                     
TMSRCT   DC    PL4'0',CL22'TOT MSRS'                                            
MSRCTA   DC    PL4'0',CL22'MSR STA WUPN'                                        
MSRCTB   DC    PL4'0',CL22'MSR STA WGGT'                                        
MSRCTC   DC    PL4'0',CL22'MSR STA WPNY'                                        
MSRCTKC  DC    PL4'0',CL22'MSR STA WUPN SUB CHA'                                
TINVCT   DC    PL4'0',CL22'TOT INV        0E03 '                                
INVCTA   DC    PL4'0',CL22'TOT INVS WUPN'                                       
INVCTB   DC    PL4'0',CL22'TOT INVS WGGT'                                       
INVCTC   DC    PL4'0',CL22'TOT INVS WPNY'                                       
TOTCTRS  EQU   (*-TOTRD)/26                                                     
TWPNY    DC    PL4'0',CL22'TOT WPNY REC'                                        
TOTKCTRS EQU   (*-TWPNY)/26                                                     
WORK     DS    CL64                                                             
COMPKEY  DC    XL10'00'                                                         
ELCODE   DS    XL1                                                              
RTITLE   DC    CL60'CHANGE STATION CALL LETTERS WUPN TO WPNY AND WGGT TC        
               O WUPN'                                                          
         SPACE                                                                  
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
**PAN#1  DC    CL21'012SXEXTWUPN 10/31/96'                                      
         END                                                                    
