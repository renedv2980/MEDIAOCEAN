*          DATA SET RECPEXT    AT LEVEL 015 AS OF 05/01/02                      
*          DATA SET RECPEXT    AT LEVEL 013 AS OF 06/12/90                      
*PHASE RECPEXTA                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RECPEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
DMCPEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMCPEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         MVC   VPRINTER,16(R1)                                                  
         MVC   VCPRINT,20(R1)                                                   
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            DMXKEEP RECORD EXIT                          
         MVI   0(R1),0                                                          
         AP    KEEP,=P'1'                                                       
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            DMXPURGE RECORD EXIT                         
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         SPACE 1                                                                
         L     R3,AREC             POINT TO RECORD                              
         LA    R5,CODES                                                         
DR10     DS    0H                                                               
         CLC   0(1,R3),8(R5)       RECORD TYPE TO TBL ENTRY                     
         BE    DR20                                                             
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'         END OF TBL                                   
         BNE   DR10                                                             
         B     DMXPURGE                                                         
         SPACE 1                                                                
DR20     DS    0H                                                               
         ZIC   R7,9(R5)            DISP TO REP CODE                             
         AR    R7,R3                                                            
         LA    R4,KEEPLST                                                       
DR30     DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   0(2,R4),0(R7)                                                    
         BE    DR40                                                             
         LA    R4,L'KEEPLST(R4)                                                 
         B     DR30                                                             
DR40     DS    0H                                                               
         CLI   0(R3),12            IS THIS A CONTRACT?                          
         BNE   DR50                                                             
         CLC   =C'TI',RCONKGRP-RCONKEY(R3)                                      
         BNE   DMXPURGE            ONLY KEEP CONS FOR SUBGRP 'TI'               
         AP    10(6,R5),=P'1'                                                   
         B     DMXKEEP                                                          
DR50     DS    0H                                                               
         AP    10(6,R5),=P'1'                                                   
         B     DMXKEEP                                                          
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
         SPACE 2                                                                
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE 1                                                                
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P6,PURGE),(14,P+11)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(4),=C'KEPT'                                                  
         EDIT  (P6,KEEP),(14,P+9)                                               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         LA    R5,CODES                                                         
DMC10    DS    0H                                                               
         MVC   P(8),0(R5)                                                       
         EDIT  (P6,10(R5)),(14,P+11)                                            
         GOTO1 VPRINTER                                                         
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   DMC10                                                            
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
PURGE    DC    PL6'0'                                                           
KEEP     DC    PL6'0'                                                           
         EJECT                                                                  
CODES    DS    0CL16                                                            
         DC    CL8'REP     ',AL1(01),AL1(RREPKREP-RREPREC),PL6'0'               
         DC    CL8'STATION ',AL1(02),AL1(RSTAKREP-RSTAREC),PL6'0'               
         DC    CL8'REGION  ',AL1(03),AL1(RREGKREP-RREGREC),PL6'0'               
         DC    CL8'OFFICE  ',AL1(04),AL1(ROFFKREP-ROFFREC),PL6'0'               
         DC    CL8'DIVISION',AL1(05),AL1(RTEMKREP-RTEMREC),PL6'0'               
         DC    CL8'MAN     ',AL1(06),AL1(RSALKREP-RSALREC),PL6'0'               
         DC    CL8'GROUP   ',AL1(07),AL1(RGRPKREP-RGRPREC),PL6'0'               
         DC    CL8'ADVERTIS',AL1(08),AL1(RADVKREP-RADVREC),PL6'0'               
         DC    CL8'PRODUCT ',AL1(09),AL1(RPRDKREP-RPRDREC),PL6'0'               
         DC    CL8'AGENCY  ',AL1(10),AL1(RAGYKREP-RAGYREC),PL6'0'               
******         DC    CL8'BUY     ',AL1(11),AL1(RBUYKREP-RBUYREC),PL6'0'         
         DC    CL8'CONTRACT',AL1(12),AL1(RCONKREP-RCONREC),PL6'0'               
         DC    CL8'CLASS   ',AL1(13),AL1(RCLSKREP-RCLSREC),PL6'0'               
         DC    CL8'CATEGORY',AL1(15),AL1(RCTGKREP-RCTGREC),PL6'0'               
******         DC    CL8'INVENTRY',AL1(18),AL1(RINVKREP-RINVREC),PL6'0'         
         DC    CL8'BUDGET  ',AL1(19),AL1(RBUDKREP-RBUDREC),PL6'0'               
******         DC    CL8'AVAIL   ',AL1(20),AL1(RAVLKREP-RAVLREC),PL6'0'         
******         DC    CL8'PROPOSAL',AL1(22),AL1(RPRPKREP-RPRPREC),PL6'0'         
         DC    CL8'EOM     ',AL1(24),AL1(REOMKREP-REOMREC),PL6'0'               
         DC    CL8'OVRNIGHT',AL1(34),AL1(13),PL6'0'                             
         DC    CL8'DEMOMENU',AL1(35),AL1(23),PL6'0'                             
         DC    CL8'DAYPART ',AL1(36),AL1(24),PL6'0'                             
         DC    CL8'PRG TYPE',AL1(37),AL1(24),PL6'0'                             
         DC    CL8'SDD     ',AL1(38),AL1(20),PL6'0'                             
******         DC    CL8'ATHENA  ',AL1(39),AL1(01),PL6'0'                       
******         DC    CL8'SWITCH  ',AL1(40),AL1(13),PL6'0'                       
         DC    CL8'CMISSION',AL1(41),AL1(11),PL6'0'                             
         DC    CL8'OWNRSHIP',AL1(42),AL1(22),PL6'0'                             
         DC    CL8'MARKET  ',AL1(43),AL1(21),PL6'0'                             
         DC    CL8'SBB     ',AL1(45),AL1(12),PL6'0'                             
         DC    CL8'COMMENT ',AL1(46),AL1(11),PL6'0'                             
         DC    CL8'TYPE    ',AL1(48),AL1(17),PL6'0'                             
         DC    X'FF'                                                            
         EJECT                                                                  
KEEPLST  DS    0CL2                                                             
*         DC    CL2'B1'                                                         
*         DC    CL2'GP'                                                         
         DC    CL2'BL'                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015RECPEXT   05/01/02'                                      
         END                                                                    
