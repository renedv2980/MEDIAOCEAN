*          DATA SET PREXTNE    AT LEVEL 011 AS OF 01/11/90                      
*          DATA SET PREXTJW    AT LEVEL 038 AS OF 12/09/88                      
*PHASE PREXTNEC,+0                                                              
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
         USING PJOBRECD,R3                                                      
*                                                                               
         CLC   0(2,R3),=C'NE'      NEEDHAM CK AGENCY                            
         BNE   DMXKEEP                                                          
         CLI   3(R3),X'15'         SEE IF JOB RECORD                            
         BNE   DMXKEEP                                                          
         OC    PJOBKPUB,PJOBKPUB                                                
         BNZ   DMXKEEP                                                          
*                                                                               
*      THIS PGM WILL VERIFY THAT ALL ORIGIN CODES ARE VALID                     
*        IF THE ORIGIN CODE DOESN'T EXIST, SLAM IN 1412                         
*                                                                               
         LA    R6,ORIGTAB                                                       
CLCORIG  CLC   0(2,R6),PJOBORIG                                                 
         BE    DMXKEEP                                                          
         CLI   0(R6),255                                                        
         LA    R6,2(R6)                                                         
         BNE   CLCORIG                                                          
*                                                                               
         XR    RF,RF                                                            
         MVC   DMCB(2),PJOBORIG                                                 
         LH    RF,DMCB                                                          
         CVD   RF,DMCB                                                          
         OI    DMCB+7,15                                                        
         UNPK  P(5),DMCB+5(3)                                                   
         MVI   P,C' '                                                           
         MVC   P+8(3),PJOBKCLT                                                  
         LA    RF,TABLE                                                         
LOOKAGN  CLI   0(RF),255                                                        
         BE    MOVETBL                                                          
         CLC   0(3,RF),PJOBKCLT                                                 
         BNE   BBMP                                                             
         CLC   3(2,RF),PJOBORIG                                                 
         BE    BYPASS                                                           
BBMP     LA    RF,5(RF)                                                         
         B     LOOKAGN                                                          
MOVETBL  MVC   0(3,RF),PJOBKCLT                                                 
         MVC   3(2,RF),PJOBORIG                                                 
         MVI   5(RF),255                                                        
BYPASS   AP    CHGCOUNT,=P'1'                                                   
         LA    RF,REPLTAB                                                       
REPTEND  CLI   0(RF),255                                                        
         BE    PRINTIT                                                          
         CLC   0(2,RF),PJOBORIG                                                 
         BE    MOVENEW                                                          
         LA    RF,4(RF)                                                         
         B     REPTEND                                                          
MOVENEW  DS    0H                                                               
         MVC   PJOBORIG,2(RF)                                                   
         B     DMXKEEP                                                          
*                                                                               
PRINTIT  DS   0H                                                                
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*=================================*                                             
* SUBROUTINE PRINTS ELEMENT TRACE *                                             
*=================================*                                             
         SPACE 1                                                                
         USING  PBUYRECD,R3                                                     
DDSTRACE NTR1                                                                   
*                                                                               
         CLI   ERR,C' '                                                         
         BNE   DDSTR1                                                           
         MVI   ERR,C'X'                                                         
         B     DDSTR1                                                           
** BY-PASSED                                                                    
         ZAP   DUB,CHGCOUNT                                                     
         CVB   RE,DUB                                                           
         SRDL  RE,32                                                            
         D     RE,=F'50'                                                        
         C     RE,=F'1'                                                         
         BNE   EXIT                                                             
*                                                                               
DDSTR1   MVC   P(25),PBUYREC                                                    
         MVC   P+35(11),=C'ERROR CODE='                                         
         MVC   P+48(1),ERR                                                      
         CLI   ERR,C'1'                                                         
         BL    DDSTR1X                                                          
         CLI   ERR,C'3'                                                         
         BH    DDSTR1B                                                          
         MVC   P+50(4),=C'PAID'                                                 
         LA    R7,BPGRS                                                         
         B     DDSTR1C                                                          
*                                                                               
*        FOR ERROR CODES 5-7   BILLING                                          
*                                                                               
DDSTR1B  MVC   P+50(6),=C'BILLED'                                               
         LA    R7,BBGRS                                                         
*                                                                               
DDSTR1C  LA    R8,3                                                             
         LA    R9,P+60                                                          
DDSTR1F  EDIT  (B4,0(R7)),(14,0(R9)),2,COMMAS=YES,MINUS=YES                     
         LA    R7,4(R7)                                                         
         LA    R9,20(R9)                                                        
         BCT   R8,DDSTR1F                                                       
DDSTR1X  GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PBUYREC,HEXBLOCK,27,=C'SEP'                      
         MVC   P(27),HEXBLOCK                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(27),HEXBLOCK+27                                                
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(116),PBDELEM                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,PBDELEM,HEXBLOCK,116,=C'SEP'                     
*                                                                               
         MVC   P(116),HEXBLOCK                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(116),HEXBLOCK+116                                              
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R4,PBDELEM                                                       
*                                                                               
DDSTR2   ZIC   R0,1(R4)                                                         
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
         B     DDSTR2                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
****************************************************************                
         EJECT                                                                  
         LOAD  EPLOC=JWCONS        LOAD CLIENT KEEP LIST                        
         LR    RE,R0                                                            
         MVC   MYCONPRM(24),0(RE)                                               
         B     DMXIT                                                            
JWCONS   DC    CL8'JWCONS'                                                      
MYCONPRM DC    6F'0'                                                            
DMXNOP   BC    0,DMXKEEP                                                        
         CLC   0(2,R3),=C'JW'                                                   
         BNH   DMXREC2                                                          
         MVI   DMXNOP+1,X'F0'                                                   
         B     DMXKEEP                                                          
*                                                                               
DMXREC2  DS    0H                                                               
         CLC   0(2,R3),=C'JW'                                                   
         BNE   DMXKEEP                                                          
         CLI   3(R3),X'10'         TEST CONTRACT                                
         BNE   DMXKEEP                                                          
         GOTO1 =V(BINSRCH),MYCONPRM,4(R3)                                       
         CLI   0(R1),1             TEST CLIENT FOUND IN LIST                    
         BE    DMXPURGE            NO - PURGE RECORD                            
         GOTO1 =V(HEXOUT),DMCB,(R3),P,7,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
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
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
COUNTS   DS    0CL24                                                            
CHGCOUNT DC    PL4'0',CL20'CHANGED RECORDS'                                     
NCOUNTS  EQU   (*-COUNTS)/L'COUNTS                                              
         DS    F                                                                
*                                                                               
TITLES   DC    CL15'TOTAL CHANGED'                                              
*                                                                               
ORIGTAB  DC    H'32'                                                            
         DC    H'0428'                                                          
         DC    H'1247'                                                          
         DC    H'1248'                                                          
         DC    H'1280'                                                          
         DC    H'1281'                                                          
         DC    H'1312'                                                          
         DC    H'1313'                                                          
         DC    H'1314'                                                          
         DC    H'1315'                                                          
         DC    H'1348'                                                          
         DC    H'1354'                                                          
         DC    H'1355'                                                          
         DC    H'1388'                                                          
         DC    H'1412'                                                          
         DC    H'1444'                                                          
         DC    H'2124'                                                          
         DC    X'FFFF'                                                          
REPLTAB  DC    H'414',H'1354'                                                   
         DC    H'415',H'1348'                                                   
         DC    H'319',H'1354'                                                   
         DC    H'416',H'1280'                                                   
         DC    H'1004',H'1348'                                                  
         DC    H'1025',H'1280'                                                  
         DC    X'FFFF'                                                          
PCD      DC    F'0'                                                             
*                                                                               
BGRS     DC    F'0'         BILLED TOTALS                                       
BAGYC    DC    F'0'                                                             
BCD      DC    F'0'                                                             
         SPACE 2                                                                
         LTORG                                                                  
TABLE    DC    X'FF'                                                            
         ORG   TABLE                                                            
         DS    300CL5                                                           
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
PJOBRECD DSECT                                                                  
       ++INCLUDE PJOBREC                                                        
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PREXTNE   01/11/90'                                      
         END                                                                    
