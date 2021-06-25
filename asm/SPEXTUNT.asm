*          DATA SET SPEXTUNT   AT LEVEL 008 AS OF 12/03/98                      
*          DATA SET SPEXTREG   AT LEVEL 025 AS OF 01/05/89                      
*PHASE SPEXTUNT,*                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* NOV 24, 1998                                                                  
* FIND BAD CML (21) ELEM IN NETPAK UNIT FILE                                    
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
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   L     R3,AREC                                                          
         AP    TOTRD,=P'1'                                                      
         CLI   0(R3),04                                                         
         BNE   DMXPURGE                                                         
         SPACE                                                                  
         AP    TOTUNT,=P'1'                                                     
         LA    R4,27(,R3)          FIND FIRST ELEM                              
DMX10    DS    0H                                                               
         CLI   0(R4),X'00'         THIS END OF RECORD                           
         BE    DMX20                                                            
         CLI   0(R4),X'21'         THIS TRAFFIC ELEM                            
         BE    DMX30                YES CHECK IT OUT                            
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         SPACE                                                                  
         B     DMX10                                                            
*                                                                               
DMX20    DS    0H                                                               
         AP    NOCMLCT,=P'1'                                                    
         B     DMXPURGE                                                         
*                                                                               
DMX30    DS    0H                                                               
         CLI   1(R4),X'50'                                                      
         BE    DMX34                                                            
         LA    R0,20                                                            
         LA    R1,ELMTAB                                                        
DMX32    DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    DMX33                                                            
         CLC   0(1,R1),1(R4)                                                    
         BE    DMX33                                                            
         LA    R1,5(,R1)                                                        
         B     DMX32                                                            
DMX33    DS    0H                                                               
         MVC   0(1,R1),1(R4)                                                    
         AP    1(4,R1),=P'1'                                                    
*                                                                               
         AP    OLDELMCT,=P'1'                                                   
         B     DMXPURGE                                                         
DMX34    DS    0H                                                               
         OC    52(18,R4),52(R4)    BAD DATA                                     
         BZ    DMX36                                                            
         CLI   52(R4),0                                                         
         BE    DMX38                                                            
DMX36    DS    0H                                                               
         OC    70(10,R4),70(R4)    BAD DATA                                     
         BZ    DMXPURGE                                                         
DMX38    DS    0H                                                               
         AP    BADRECCT,=P'1'                                                   
         LA    R6,=CL20'BAD 21 ELEM'                                            
         SPACE                                                                  
         CLC   SVAGY,1(R3)         NEW AGY                                      
         BE    DMX50                NO                                          
         CP    BADAGYCT,=P'0'                                                   
         BE    DMX50                                                            
         B     DMX40                                                            
         UNPK  DUB,BADAGYCT                                                     
         OI    DUB+7,X'F0'                                                      
         MVC   P+18(8),DUB                                                      
         MVC   P+5(11),=C'BAD 21 ELMS'                                          
         GOTO1 VPRINTER                                                         
DMX40    DS    0H                                                               
         ZAP   BADAGYCT,=P'0'                                                   
DMX50    DS    0H                                                               
         LA    R0,ELMDTACT                                                      
         LA    R1,ELMDTA                                                        
DMX52    DS    0H                                                               
         OC    0(32,R1),0(R1)                                                   
         BZ    DMX54                                                            
         CLC   0(28,R1),52(R4)                                                  
         BE    DMX56                                                            
         LA    R1,32(,R1)                                                       
         BCT   R0,DMX52                                                         
         DC    H'0'                                                             
DMX54    DS    0H                                                               
         MVC   0(28,R1),52(R4)                                                  
         B     DMX56                                                            
         ST    R1,FULL                                                          
         SPACE                                                                  
         MVC   P+5(19),=CL19'KEY FOR BAD 21 ELEM'                               
         GOTO1 =V(HEXOUT),DMCB,(R3),P+25,20,0,0                                 
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         ZIC   R5,1(R4)                                                         
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R4),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         L     R1,FULL                                                          
         SPACE                                                                  
DMX56    ICM   RE,15,28(R1)                                                     
         LA    RE,1(,RE)                                                        
         STCM  RE,15,28(R1)                                                     
         SPACE                                                                  
         MVC   SVAGY,1(R3)                                                      
         AP    BADAGYCT,=P'1'                                                   
         CP    BADAGYCT,=P'10'                                                  
         BH    DMXKEEP                                                          
         SPACE                                                                  
         CLI   68(R4),0            IS TALENT TRANSFER FLAG ON?                  
         BNE   DMX58                YES                                         
         SPACE                                                                  
         B     DMXKEEP                                                          
DMX58    DS    0H                                                               
         MVC   P+5(39),=CL39'KEY FOR BAD 21 ELEM WITH TAL TRANS FLG='           
         GOTO1 =V(HEXOUT),DMCB,68(R4),P+44,1,0,0                                
         GOTO1 =V(HEXOUT),DMCB,(R3),P+48,20,0,0                                 
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         ZIC   R5,1(R4)                                                         
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R4),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         LA    R3,TOTRD                                                         
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,TOTUNT                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,BADRECCT                                                      
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,NOCMLCT                                                       
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R3,OLDELMCT                                                      
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R2,100                                                           
         LA    R3,ELMTAB                                                        
DMXEOF20 DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    DMXEOF40                                                         
         MVC   P+7(13),=C'ELEM LENGTH ='                                        
         ZIC   R0,0(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+21(3),DUB                                                      
         EDIT  (P4,1(R3)),(10,P+27),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,5(,R3)                                                        
         BCT   R2,DMXEOF20                                                      
DMXEOF40 DS    0H                                                               
         LA    R2,ELMDTACT                                                      
         LA    R3,ELMDTA                                                        
         LR    R0,R2                                                            
         LR    R1,R3                                                            
         SR    RE,RE                                                            
DMXEOF42 OC    0(32,R1),0(R1)                                                   
         BZ    DMXEOF44                                                         
         BCTR  RE,0                                                             
         LA    R1,32(,R1)                                                       
         BCT   R0,DMXEOF42                                                      
         DC    H'0'                                                             
DMXEOF44 LPR   R4,RE                                                            
         GOTO1 =V(XSORT),DMCB,(R3),(R4),32,28,0                                 
         SR    R6,R6                                                            
DMXEOF50 DS    0H                                                               
         OC    0(32,R3),0(R3)                                                   
         BZ    DMXEOF54                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P+5,28,0,0                                  
         EDIT  (B4,28(R3)),(10,P+63),COMMAS=YES                                 
         GOTO1 VPRINTER                                                         
         ICM   R0,15,28(R3)                                                     
         AR    R6,R0                                                            
         LA    R3,32(,R3)                                                       
         BCT   R2,DMXEOF50                                                      
         DC    H'0'                                                             
DMXEOF54 DS    0H                                                               
         EDIT  (R4),(10,P+10),COMMAS=YES                                        
         EDIT  (R6),(10,P+63),COMMAS=YES                                        
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 2                                                                
TOTRD    DC    PL4'0',CL28'TOTAL RECS READ ='                                   
TOTUNT   DC    PL4'0',CL28'TOTAL UNITS READ='                                   
BADRECCT DC    PL4'0',CL28'BAD 21 ELEM RECS='                                   
NOCMLCT  DC    PL4'0',CL28'NO 21 ELEM RECS ='                                   
OLDELMCT DC    PL4'0',CL28'OLD 21 ELEM RECS='                                   
SVAGY    DS    XL3                                                              
FULL     DS    F                                                                
BADAGYCT DC    PL4'0'                                                           
WORK     DS    CL64                                                             
ELMTAB   DC    10XL5'000000000C'                                                
ELMTABCT EQU   10                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
ELMDTA   DC    2000XL32'00'                                                     
ELMDTACT EQU   2000                                                             
         SPACE 2                                                                
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
         PRINT OFF                                                              
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPEXTUNT  12/03/98'                                      
         END                                                                    
