*          DATA SET PREXTJID   AT LEVEL 015 AS OF 04/17/90                      
*PHASE PREXTJID,+0                                                              
*INCLUDE BINSRCH                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
*                                                                               
*        THIS PROGRAM CONVERTS TERMINAL IDS IN PJOBRECS                         
*                                                                               
*        NEEDED WHEN AN AGENCY WANTS TO DELETE IDS                              
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
         CLC   0(2,R3),=C'CE'      AGENCY CE FOR THIS RUN                       
         BNE   DMXKEEP                                                          
         CLI   3(R3),X'15'         SEE IF JOB RECORD                            
         BNE   DMXKEEP                                                          
         OC    PJOBKPUB,PJOBKPUB    SEE IF INSTRUCTION RECORD                   
         BNZ   DMXKEEP              YES - SKIP                                  
*                                                                               
*      THIS PGM CONVERT ORIGIN CODES                                            
*      IF THE ORIGIN CODE ISN'T IN ORIGTAB (TABLE OF VALID IDS)                 
*      ADD IT TO TABLE                                                          
*      THEN SEE IF IT IS REPLTAB (TABLE OF IDS TO CONVERT)                      
*      IF NOT - FLAG AS INVALID ID                                              
*      IF IT IS  - SET CONVERTED ID IN PJOBREC'S PJOBORIG                       
*                                                                               
         LA    R6,ORIGTAB                                                       
CLCORIG  CLC   0(2,R6),PJOBORIG                                                 
         BE    DMXKEEP                                                          
         CLI   0(R6),255                                                        
         LA    R6,2(R6)                                                         
         BNE   CLCORIG                                                          
*                                                                               
         MVI   PSW,0                                                            
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
         MVI   PSW,C'Y'                                                         
*                                                                               
BYPASS   AP    CHGCOUNT,=P'1'                                                   
         LA    RF,REPLTAB                                                       
REPTEND  CLI   0(RF),255                                                        
         BE    NOTFND                                                           
         CLC   0(2,RF),PJOBORIG                                                 
         BE    MOVENEW                                                          
         LA    RF,4(RF)                                                         
         B     REPTEND                                                          
MOVENEW  DS    0H                                                               
         MVC   PJOBORIG,2(RF)                                                   
         CLI   PSW,C'Y'             SEE IF PRINTING                             
         BNE   DMXKEEP                                                          
         MVC   DMCB(2),PJOBORIG                                                 
         LH    RF,DMCB                                                          
         CVD   RF,DMCB                                                          
         OI    DMCB+7,X'0F'                                                     
         UNPK  P+15(5),DMCB+5(3)                                                
         B     PRINTIT                ALSO GOES TO DMXKEEP                      
*                                                                               
NOTFND   MVC   P+25(23),=C'NOT IN CONVERSION TABLE'                             
         MVC   P+50(25),PJOBKEY                                                 
         B     PRINTIT                                                          
*                                                                               
PRINTIT  DS   0H                                                                
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
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
*                                                                               
*        TABLE OF VALID IDS                                                     
*                                                                               
ORIGTAB  DC    H'1974'        CEMN                                              
         DC    H'1977'        CECH                                              
         DC    H'1978'        CEDE                                              
         DC    H'1979'        CENY                                              
         DC    H'1980'        CECOR                                             
         DC    H'2076'        CEMNA                                             
         DC    H'2077'        CEOR                                              
         DC    H'2148'        CECHNS                                            
         DC    X'FFFF'        END OF TABLE                                      
*                                                                               
*                                                                               
*        TABLE OF IDS TO CONVERT                                                
*                                                                               
*              OLD    NEW                                                       
*              ---    ---                                                       
REPLTAB  DC    H'0232',H'1974'     CMMN TO CEMN                                 
         DC    H'0233',H'1977'     CMCH TO CECH                                 
         DC    H'1694',H'2076'     CMMNA TO CEMNA                               
         DC    H'1913',H'1980'     CMCOR TO CECOR                               
         DC    H'2094',H'2148'     CMCHNS TO CECHNS                             
         DC    H'2110',H'1978'     CEDEP TO CEDE                                
         DC    H'2132',H'1979'     CENYP TO CENY                                
         DC    H'2228',H'2077'     CEORP TO CEOR                                
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
         DS    400CL5                                                           
*                                                                               
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
PSW      DS    CL1                                                              
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
**PAN#1  DC    CL21'015PREXTJID  04/17/90'                                      
         END                                                                    
