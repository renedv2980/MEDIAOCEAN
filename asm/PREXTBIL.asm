*          DATA SET PREXTBIL   AT LEVEL 063 AS OF 08/15/00                      
*PHASE PREXTBIL                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE PNBVAL                                                                 
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
         USING PBILRECD,R3                                                      
*                                                                               
         MVI   CHGSW,0                                                          
*                                                                               
         CLC   PBILKAGY,=C'HD'     FOR TEST PURPOSES                            
         BNE   DMXKEEP                                                          
         CLI   PBILKRCD,X'08'      BILL RECORD ?                                
         BNE   DMXKEEP             NO                                           
*                                                                               
         LA    R6,PBILLREC+33                                                   
         CLI   0(R6),X'08'                                                      
         BE    *+6                                                              
         DC    H'0'                NO BILL ELEM                                 
*                                                                               
         CP    CHGCOUNT,=P'20'                                                  
         BH    DMXR010                                                          
*                                                                               
         MVC   P+1(15),=C'***  BEFORE ***'                                      
         GOTO1 VPRINTER                                                         
         BAS   RE,DMPIN                                                         
*                                                                               
DMXR010  GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PBVALOUT                         
*                                                                               
         XC    ELEM,ELEM                                                        
         USING PBILLEL,R6                                                       
*                                                                               
         MVC   ELEM+00(PBILLGRS-PBILLEL),PBILLEL                                
         MVI   ELEM+01,X'64'       NEW LENGTH                                   
         ZAP   ELEM+22(6),PBILLGRS                                              
         ZAP   ELEM+28(6),PBILLBIL                                              
         ZAP   ELEM+34(6),PBILLNET                                              
         ZAP   ELEM+40(6),PBILLRCV                                              
         MVC   ELEM+46(28),PBILLEL+42                                           
*                                                                               
         GOTO1 =V(RECUP),DMCB,(1,PBILLREC),PBILLEL   DELETE "OLD" ELEM          
*                                                                               
*                                                    INSERT "NEW" ELEM          
         GOTO1 =V(RECUP),DMCB,(1,PBILLREC),ELEM,PBILLREC+33                     
*                                                                               
         DROP  R3,R6                                                            
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PQILRECD,R3                                                      
*                                                                               
         XC    PQVALOUT,PQVALOUT                                                
*                                                                               
         GOTO1 =V(PNBVAL),DMCB,(C'B',PQILLREC),PQVALOUT                         
*                                                                               
************ COMPARE NEW FIELDS TO OLD FIELDS - MUST BE UNCHANGED               
*                                                                               
         LA    R4,PBVALOUT                                                      
         USING PPBVALD,R4                                                       
*                                                                               
         LA    R5,PQVALOUT                                                      
         USING PQBVALD,R5                                                       
*                                                                               
         CP    PQBVEBG,PPBVEBG                                                  
         BNE   DEATH                                                            
         CP    PQBVEBB,PPBVEBB                                                  
         BNE   DEATH                                                            
         CP    PQBVEBN,PPBVEBN                                                  
         BNE   DEATH                                                            
         CP    PQBVEBC,PPBVEBC                                                  
         BNE   DEATH                                                            
         CLC   PQBVETAX,PPBVETAX                                                
         BNE   DEATH                                                            
         CP    PQBVBG,PPBVBG                                                    
         BNE   DEATH                                                            
         CP    PQBVBB,PPBVBB                                                    
         BNE   DEATH                                                            
         CP    PQBVBN,PPBVBN                                                    
         BNE   DEATH                                                            
         CP    PQBVBC,PPBVBC                                                    
         BNE   DEATH                                                            
         CLC   PQBVTAX,PPBVTAX                                                  
         BNE   DEATH                                                            
         CP    PQBVBACT,PPBVBACT                                                
         BNE   DEATH                                                            
*                                                                               
         CLC   PPBVGST(12),PQBVGST                                              
         BNE   DEATH                                                            
         B     DMXR150             CHANGES OK - CONTINUE                        
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
DEATH    DC    H'0'                                                             
*                                                                               
DMXR150  DS    0H                                                               
         CP    CHGCOUNT,=P'20'                                                  
         BH    DMXR170                                                          
*                                                                               
         MVC   P+1(15),=C'***  AFTER  ***'                                      
         GOTO1 VPRINTER                                                         
         BAS   RE,DMPOUT                                                        
*                                                                               
DMXR170  DS    0H                                                               
         AP    CHGCOUNT,=P'1'                                                   
         B     DMXKEEP                                                          
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
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
         EDIT  (P4,CHGCOUNT),(9,P+24),COMMAS=YES                                
         GOTO1 VPRINTER                                                         
         BAS   R4,BLANKLN                                                       
*                                                                               
         B     DMXIT                                                            
*                                                                               
BLANKLN  MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         BR    R4                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                   SUBROUTINES PRINT RECORD DUMPS                    *         
***********************************************************************         
*                                                                               
DMPIN    NTR1                                                                   
         SPACE 1                                                                
         USING  PBILRECD,R3                                                     
*                                                                               
         MVC   P(33),PBILLREC                                                   
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PBILLREC,HEXBLOCK,33,=C'SEP'                     
         MVC   P+38(33),HEXBLOCK                                                
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+38(33),HEXBLOCK+33                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(70),PBILLEL                                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PBILLEL,HEXBLOCK,70,=C'SEP'                      
*                                                                               
         MVC   P(70),HEXBLOCK                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(70),HEXBLOCK+70                                                
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R4,PBILLEL                                                       
*                                                                               
DMPIN2   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         ZIC   R5,1(R4)            GET CURRENT ELEMENT LENGTH                   
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R4) *EXECUTED*                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         GOTO1 =V(HEXOUT),DMCB,(R4),HEXBLOCK,(R5),=C'SEP'                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HEXBLOCK *EXECUTED*                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    RE,HEXBLOCK+1(R5)                                                
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(RE) *EXECUTED*                                            
         GOTO1 VPRINTER                                                         
         B     DMPIN2                                                           
         SPACE 1                                                                
*                                                                               
DMPOUT   NTR1                                                                   
         SPACE 1                                                                
         USING  PQILRECD,R3                                                     
*                                                                               
         MVC   P(33),PQILLREC                                                   
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PQILLREC,HEXBLOCK,33,=C'SEP'                     
         MVC   P+38(33),HEXBLOCK                                                
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+38(33),HEXBLOCK+33                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(100),PQILLEL                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PQILLEL,HEXBLOCK,100,=C'SEP'                     
*                                                                               
         MVC   P(100),HEXBLOCK                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(100),HEXBLOCK+100                                              
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R4,PQILLEL                                                       
*                                                                               
DMPOUT2  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         ZIC   R5,1(R4)            GET CURRENT ELEMENT LENGTH                   
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R4) *EXECUTED*                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         GOTO1 =V(HEXOUT),DMCB,(R4),HEXBLOCK,(R5),=C'SEP'                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),HEXBLOCK *EXECUTED*                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    RE,HEXBLOCK+1(R5)                                                
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(RE) *EXECUTED*                                            
         GOTO1 VPRINTER                                                         
         B     DMPOUT2                                                          
*                                                                               
***********************************************************************         
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
CHGCOUNT DC    PL4'0'              NUMBER OF RECORDS CHANGED                    
*                                                                               
HEXBLOCK DS    CL500                                                            
         SPACE 2                                                                
WORKD    DSECT                                                                  
WORK     DS    CL64                                                             
ELEM     DS    CL100                                                            
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
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
HALF     DS    H                                                                
CHGSW    DS    CL1                                                              
PBVALOUT DS    CL124                                                            
PQVALOUT DS    CL132                                                            
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
       ++INCLUDE PPBVALD                                                        
         SPACE 2                                                                
       ++INCLUDE PQBVALD                                                        
         EJECT                                                                  
PBILRECD DSECT                                                                  
       ++INCLUDE PBILLREC                                                       
         EJECT                                                                  
PQILRECD DSECT                                                                  
       ++INCLUDE PQBILREC                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063PREXTBIL  08/15/00'                                      
         END                                                                    
