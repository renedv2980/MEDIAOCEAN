*          DATA SET PREXTOM    AT LEVEL 048 AS OF 05/01/02                      
*PHASE PREXTOM,+0                                                               
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
         CLC   0(3,R3),=C'OMM'     CHECK AGENCY O & M (M, N OR T)               
         BE    DMXR20                                                           
         CLC   0(3,R3),=C'OMN'     CHECK AGENCY O & M (M, N OR T)               
         BE    DMXR20                                                           
         CLC   0(3,R3),=C'OMT'     CHECK AGENCY O & M (M, N OR T)               
         BNE   DMXKEEP                                                          
*                                                                               
DMXR20   CLC   4(3,R3),=C'IME'     CHECK CLIENT                                 
         BNE   DMXKEEP                                                          
         CLC   7(3,R3),=C'TIV'     CHECK PRODUCT                                
         BNE   DMXKEEP                                                          
*                                                                               
         CLI   3(R3),X'20'         SEE IF BUYREC                                
         BE    DMXR30                                                           
         CLI   3(R3),X'09'         OR BUCKET REC                                
         BNE   DMXKEEP                                                          
*                                                                               
*            BUCKET REC                                                         
         CLC   10(2,R3),=X'0123'   CHECK ESTIMATE (291)                         
         BE    DMXR99                                                           
         B     DMXKEEP                                                          
*                                                                               
*            BUY REC                                                            
DMXR30   CLC   19(2,R3),=X'0123'   CHECK ESTIMATE (291)                         
         BNE   DMXKEEP                                                          
*                                                                               
DMXR99   AP    CHGCOUNT,=P'1'                                                   
         MVC   4(3,R3),=C'TSI'     CHANGE CLIENT TO TSI                         
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
         CLI   3(R3),X'20'         SEE IF BUY REC                               
         BNE   DDSTR1B             MUST BE BUCKET REC                           
         MVC   P+85(6),PBDJOB      AD CODE FOR BUY                              
         B     DDSTR1X                                                          
DDSTR1B  MVC   P+100(10),=C'* BUCKET *'                                         
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
**PAN#1  DC    CL21'048PREXTOM   05/01/02'                                      
         END                                                                    
