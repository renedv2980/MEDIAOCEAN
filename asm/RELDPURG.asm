*          DATA SET RELDPURG   AT LEVEL 012 AS OF 06/05/84                      
*PHASE RELDPURG,+0                                                              
*INCLUDE CARDS                                                                  
         TITLE 'RELDPURG - LOAD/DUMP ROUTINE TO PURGE CLOSED RECORDS'           
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
         NMOD1 40,DMLDEXT,RR=R5                                                 
         USING WORKD,RC                                                         
         ST    R5,RELO                                                          
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
         AP    PURGE,=P'1'                                                      
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
         L     RE,=V(CARDS)                                                     
         A     RE,RELO                                                          
         ST    RE,CARDS                                                         
         SPACE 1                                                                
*&&OS                                                                           
* UNDER MVS, CANNOT OPEN SYSIN FROM TWO MODULES.  SYSIN MUST BE                 
* RE-NAMED SO IT DOES NOT CONFLICT WITH THE SYSIN DCB OPENED                    
* BY CALL FROM DMLDMOD.                                                         
*                                                                               
         L     RE,=V(SYSIN)        ADDRESS THE DCB                              
         MVC   40(8,RE),=C'EXTRNRDR' RE-NAME THE READER                         
*&&                                                                             
         L     R2,=A(TABLE)                                                     
         A     R2,RELO                                                          
         SPACE 1                                                                
* CARD FORMAT IS RR RECORD NAME OR ALLRECORD NAME WHERE RR=REP CODE             
* AND RECORD NAME IS A KEYWORD IN CODE TABLE                                    
*                                                                               
NEXTCARD GOTO1 CARDS,DMCB,(R2),=C'RE00'                                         
         CLC   0(2,R2),=C'/*'                                                   
         BE    DMXIT                                                            
         SPACE 1                                                                
         LA    R5,CODES                                                         
         CLC   3(8,R2),0(R5)       RECORD NAME VS. CODE TABLE                   
         BE    NEXTC2                                                           
         LA    R5,10(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
NEXTC2   MVC   P+3(15),0(R2)       PRINT CONTROL CARD                           
         GOTO1 VPRINTER                                                         
         CLC   0(3,R2),=C'ALL'     TEST FOR DELETE ACROSS REPS                  
         BNE   *+10                NO-SELECTIVE DELETE FOR ONE REP              
         MVC   0(2,R2),ALL         YES-SET ALL MARKER                           
         MVC   2(2,R2),8(R5)       RECORD TYPE/DISPLACEMENT TO REP CODE         
         LA    R2,4(R2)                                                         
         B     NEXTCARD                                                         
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         TM    29(R3),X'01'        IS IT MARKED FOR POSSIBLE DELETE             
         BNO   DMXKEEP                                                          
         SPACE 1                                                                
         L     R2,=A(TABLE)                                                     
         A     R2,RELO                                                          
         SPACE 1                                                                
DMXTAB   CLC   0(2,R2),=C'/*'                                                   
         BE    DMXKEEP             END OF TABLE KEEP IT                         
         CLC   0(1,R3),2(R2)       MATCH RECORD TYPE                            
         BNE   DMXTAB1                                                          
         CLC   ALL,0(R2)           TEST FOR DELETE ON ALL REPS                  
         BE    DMXPURGE                                                         
         ZIC   R7,3(R2)            DISPLACEMENT TO REP CODE                     
         AR    R7,R3               R7 TO REP CODE IN RECORD                     
         CLC   0(2,R7),0(R2)       DOES IT MATCH TABLE                          
         BE    DMXPURGE            YES, SO PURGE IT                             
         SPACE 1                                                                
DMXTAB1  LA    R2,4(R2)            NEXT TABLE ENTRY                             
         B     DMXTAB                                                           
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
         EJECT                                                                  
CODES    DC    CL8'REP     ',AL1(01),AL1(RREPKREP-RREPREC)                      
         DC    CL8'OFFICE  ',AL1(04),AL1(ROFFKREP-ROFFREC)                      
         DC    CL8'DIVISION',AL1(05),AL1(RTEMKREP-RTEMREC)                      
         DC    CL8'MAN     ',AL1(06),AL1(RSALKREP-RSALREC)                      
         DC    CL8'GROUP   ',AL1(07),AL1(RGRPKREP-RGRPREC)                      
         DC    CL8'ADVERTIS',AL1(08),AL1(RADVKREP-RADVREC)                      
         DC    CL8'PRODUCT ',AL1(09),AL1(RPRDKREP-RPRDREC)                      
         DC    CL8'AGENCY  ',AL1(10),AL1(RAGYKREP-RAGYREC)                      
         DC    CL8'BUY     ',AL1(11),AL1(RBUYKREP-RBUYREC)                      
         DC    CL8'CONTRACT',AL1(12),AL1(RCONKREP-RCONREC)                      
         DC    CL8'CATEGORY',AL1(15),AL1(RCTGKREP-RCTGREC)                      
         DC    CL8'CLASS   ',AL1(13),AL1(RCLSKREP-RCLSREC)                      
         DC    CL8'STATION ',AL1(02),AL1(RSTAKREP-RSTAREC)                      
         DC    CL8'INVENTRY',AL1(18),AL1(RINVKREP-RINVREC)                      
         DC    CL8'AVAIL   ',AL1(20),AL1(RAVLKREP-RAVLREC)                      
         DC    CL8'PROPOSAL',AL1(22),AL1(RPRPKREP-RPRPREC)                      
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ALL      DC    X'FFFF'             MARKER TO DELETE FOR ALL REPS                
         SPACE 1                                                                
TABLE    DS    0D                                                               
         DC    1000X'00'                                                        
         EJECT                                                                  
WORKD    DSECT                                                                  
RELO     DS    A                                                                
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
         SPACE 1                                                                
CARDS    DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
AD       DSECT                                                                  
       ++INCLUDE REGENREP                                                       
BD       DSECT                                                                  
       ++INCLUDE REGENOFF                                                       
CD       DSECT                                                                  
       ++INCLUDE REGENTEM                                                       
DD       DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
ED       DSECT                                                                  
       ++INCLUDE REGENGRP                                                       
FD       DSECT                                                                  
       ++INCLUDE REGENADV                                                       
GD       DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
HD       DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
ID       DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
JD       DSECT                                                                  
       ++INCLUDE REGENCON                                                       
KD       DSECT                                                                  
       ++INCLUDE REGENCTG                                                       
LD       DSECT                                                                  
       ++INCLUDE REGENCLS                                                       
MD       DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
ND       DSECT                                                                  
       ++INCLUDE REGENINV                                                       
OD       DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
PD       DSECT                                                                  
       ++INCLUDE REGENPRP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012RELDPURG  06/05/84'                                      
         END                                                                    
