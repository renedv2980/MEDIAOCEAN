*          DATA SET PREXTAG    AT LEVEL 060 AS OF 05/01/02                      
*PHASE PREXTAG,+0                                                               
*INCLUDE BINSRCH                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE DATCON                                                                 
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
         PRINT NOGEN                                                            
         NMOD1 80,DMLDEXT                                                       
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
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PBUYRECD,R3                                                      
*                                                                               
         CLC   0(4,R3),=X'C1C7D420' AGM X'20'    ARNOLD MAGAZINE                
         BE    DMXR10                                                           
         B     DMXKEEP                                                          
*                                                                               
DMXR10   CLC   4(3,R3),=C'ACU'     CHECK CLIENTS TO NOT CHANGE                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'BGA'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'BGR'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'DSR'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'GTE'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'LOT'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'MCR'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'PHM'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'STL'                                                  
         BE    DMXKEEP                                                          
         CLC   4(3,R3),=C'VPR'                                                  
         BE    DMXKEEP                                                          
*                                                                               
DMXR20   DS    0H                  TEST RECORDS FOR BILLED                      
         LA    R6,PBDELEM                                                       
         MVI   ELCODE,X'26'        BILL ELEMENT                                 
DMXR20D  BAS   RE,NEXTEL                                                        
         BNE   DMXR30              NOT BILLED - CHANGE DATES                    
         OC    5(3,R6),5(R6)       BILLED DATE ?                                
         BZ    DMXR20D             NO - CHK FOR MORE ELEMS                      
         MVC   P+85(9),=C'BILLED ON'                                            
         GOTO1 =V(DATCON),DMCB,(3,5(R6)),(5,P+96)                               
         GOTO1 =V(DATCON),DMCB,(3,PBDBDATE),(5,P+106)                           
         BAS   RE,DDSTRACE                                                      
         B     DMXKEEP                                                          
*                                                                               
DMXR30   DS    0H                                                               
         AP    CHGCOUNT,=P'1'                                                   
         MVC   P+85(9),=C'*CHANGED*'                                            
         GOTO1 =V(DATCON),DMCB,(3,PBDBDATE),(5,P+96)                            
         MVC   PBDBDATE,PBUYKDAT   REPLACE BILL DATE WITH INS. DATE             
         ZIC   RE,PBDBDATE+1       MONTH                                        
         BCTR  RE,0                DECREMENT MONTH BY 1                         
         STC   RE,PBDBDATE+1       NEW MONTH                                    
         LTR   RE,RE               NEW MONTH DECEMBER ?                         
         BNZ   DMXR30B             NO                                           
         LA    RE,12               YES                                          
         STC   RE,PBDBDATE+1       SET NEW MONTH                                
         ZIC   RE,PBDBDATE         YEAR                                         
         BCTR  RE,0                DECREMENT YEAR BY 1                          
         STC   RE,PBDBDATE         SET NEW YEAR                                 
         B     DMXR99              DONE                                         
DMXR30B  DS    0H                                                               
         ZIC   RE,PBDBDATE+1       NEW MONTH                                    
         ZIC   RF,PBDBDATE+2       DAY                                          
         CH    RE,=H'2'            MONTH FEBRUARY ?                             
         BNE   DMXR30D             NO                                           
         CH    RF,=H'29'           YES - DAY LT 29 ?                            
         BL    DMXR99              YES - DONE                                   
         LA    RF,28               LAST DAY OF MONTH                            
         B     DMXR30X             REPLACE DAY                                  
DMXR30D  CH    RE,=H'4'            MONTH APRIL ?                                
         BE    DMXR30R             YES                                          
         CH    RE,=H'6'            MONTH JUNE ?                                 
         BE    DMXR30R             YES                                          
         CH    RE,=H'9'            MONTH SEPTEMBER                              
         BE    DMXR30R             YES                                          
         CH    RE,=H'11'           MONTH NOVENBER ?                             
         BNE   DMXR99              NO - DONE                                    
DMXR30R  CH    RF,=H'31'                                                        
         BL    DMXR99              DONE                                         
         LA    RF,30               LAST DAY OF MONTH                            
DMXR30X  STC   RF,PBDBDATE+2       NEW DAY                                      
DMXR99   GOTO1 =V(DATCON),DMCB,(3,PBDBDATE),(5,P+106)  "NEW" BUY DATE           
         BAS   RE,DDSTRACE                                                      
         B     DMXKEEP                                                          
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
*=================================*                                             
* SUBROUTINE PRINTS ELEMENT TRACE *                                             
*=================================*                                             
         SPACE 1                                                                
         USING  PBUYRECD,R3                                                     
DDSTRACE NTR1                                                                   
*                                                                               
DDSTR1   DS    0H                                                               
         MVC   P(25),PBUYREC                                                    
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PBUYREC,P+30,25,0                                
DDSTR1X  GOTO1 VPRINTER                                                         
*                                                                               
*****DDSTR1F  EDIT  (B4,0(R7)),(14,0(R9)),2,COMMAS=YES,MINUS=YES                
*                                                                               
*****    GOTO1 =V(HEXOUT),DMCB,PBUYREC,HEXBLOCK,25,=C'SEP'                      
*****    MVC   P+30(25),HEXBLOCK                                                
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    MVC   P+30(25),HEXBLOCK+28                                             
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    MVC   P(116),PBDELEM                                                   
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    GOTO1 =V(HEXOUT),DMCB,PBDELEM,HEXBLOCK,116,=C'SEP'                     
*                                                                               
*****    MVC   P(116),HEXBLOCK                                                  
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    MVC   P(116),HEXBLOCK+116                                              
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    LA    R4,PBDELEM                                                       
*                                                                               
*****DDSTR2   ZIC   R0,1(R4)                                                    
*****    AR    R4,R0                                                            
*****    CLI   0(R4),0                                                          
*****    BE    EXIT                                                             
*****    ZIC   R5,1(R4)            GET CURRENT ELEMENT LENGTH                   
*****    BCTR  R5,0                SET FOR EX                                   
*****    EX    R5,*+8                                                           
*****    B     *+10                                                             
*****    MVC   P(0),0(R4) *EXECUTED*                                            
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    LA    R5,1(R5)                                                         
*****    GOTO1 =V(HEXOUT),DMCB,(R4),HEXBLOCK,(R5),=C'SEP'                       
*****    BCTR  R5,0                                                             
*****    EX    R5,*+8                                                           
*****    B     *+10                                                             
*****    MVC   P(0),HEXBLOCK *EXECUTED*                                         
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    LA    RE,HEXBLOCK+1(R5)                                                
*****    EX    R5,*+8                                                           
*****    B     *+10                                                             
*****    MVC   P(0),0(RE) *EXECUTED*                                            
*****    GOTO1 VPRINTER                                                         
*****    B     DDSTR2                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
****************************************************************                
*****    EJECT                                                                  
*****    LOAD  EPLOC=JWCONS        LOAD CLIENT KEEP LIST                        
*****    LR    RE,R0                                                            
*****    MVC   MYCONPRM(24),0(RE)                                               
*****    B     DMXIT                                                            
*****JWCONS   DC    CL8'JWCONS'                                                 
*****MYCONPRM DC    6F'0'                                                       
*****DMXNOP   BC    0,DMXKEEP                                                   
*****    CLC   0(2,R3),=C'JW'                                                   
*****    BNH   DMXREC2                                                          
*****    MVI   DMXNOP+1,X'F0'                                                   
*****    B     DMXKEEP                                                          
*                                                                               
*****DMXREC2  DS    0H                                                          
*****    CLC   0(2,R3),=C'JW'                                                   
*****    BNE   DMXKEEP                                                          
*****    CLI   3(R3),X'10'         TEST CONTRACT                                
*****    BNE   DMXKEEP                                                          
*****    GOTO1 =V(BINSRCH),MYCONPRM,4(R3)                                       
*****    CLI   0(R1),1             TEST CLIENT FOUND IN LIST                    
*****    BE    DMXPURGE            NO - PURGE RECORD                            
*****    GOTO1 =V(HEXOUT),DMCB,(R3),P,7,=C'TOG'                                 
*****    GOTO1 VPRINTER                                                         
*****    B     DMXKEEP                                                          
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         LA    R3,COUNTS                                                        
         LA    R4,NCOUNTS                                                       
*                                                                               
DMXEOF2  OI    3(R3),X'0F'                                                      
         UNPK  P(7),0(4,R3)                                                     
         MVC   P+10(20),4(R3)                                                   
         LA    R3,24(R3)                                                        
         GOTO1 VPRINTER                                                         
         BCT   R4,DMXEOF2                                                       
*                                                                               
*****    LA    R3,PGRS                                                          
*****    LA    R5,TITLES                                                        
*****    LA    R4,6                                                             
*                                                                               
*****DMXEOF5  EDIT  (B4,0(R3)),(14,P+20),2,COMMAS=YES,MINUS=YES                 
*****    MVC   P+1(15),0(R5)                                                    
*****    LA    R3,4(R3)                                                         
*****    LA    R5,15(R5)                                                        
*****    GOTO1 VPRINTER                                                         
*****    BCT   R4,DMXEOF5                                                       
*                                                                               
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
COUNTS   DS    0CL24                                                            
*****PCHGCNT  DC    PL4'0',CL20'MARKED PAID'                                    
*****BCHGCNT  DC    PL4'0',CL20'MARKED BILLED'                                  
*****CLTCHG   DC    PL4'0',CL20'CLT CHANGES'                                    
CHGCOUNT DC    PL4'0',CL20'CHANGED RECORDS'                                     
NCOUNTS  EQU   (*-COUNTS)/L'COUNTS                                              
         DS    F                                                                
*                                                                               
TITLES   DC    CL15'PAID GROSS'                                                 
         DC    CL15'PAID AGY COM'                                               
         DC    CL15'PAID CD'                                                    
         DC    CL15'BILLED GROSS'                                               
         DC    CL15'BILLED AGY COM'                                             
         DC    CL15'BILLED CD'                                                  
*                                                                               
PGRS     DC    F'0'          PAID TOTALS                                        
PAGYC    DC    F'0'                                                             
PCD      DC    F'0'                                                             
*                                                                               
BGRS     DC    F'0'         BILLED TOTALS                                       
BAGYC    DC    F'0'                                                             
BCD      DC    F'0'                                                             
         SPACE 2                                                                
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
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
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
**PAN#1  DC    CL21'060PREXTAG   05/01/02'                                      
         END                                                                    
