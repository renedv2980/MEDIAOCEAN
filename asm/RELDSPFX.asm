*          DATA SET RELDSPFX   AT LEVEL 254 AS OF 05/01/02                      
*PHASE RELDSPFX,*                                                               
*INCLUDE RECUP                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RELDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*******************************************************************             
*  COMBINE DELETED COMPETITIVE SPL INFO WITH 'SWITCHED' TO STATION              
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
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
         L     R6,AREC             POINT TO RECORD                              
         USING RSTAREC,R6                                                       
                                                                                
         CLI   RSTAKTYP,X'02'      IF STATION                                   
         BNE   DMXR100                                                          
         CLC   RSTAKREP,=C'UV'     ONLY FOR UNIVISION                           
         BNE   DMXKEEP                                                          
                                                                                
         LA    R3,COMPLST                                                       
                                                                                
DMXR10   DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   RSTAKSTA,0(R3)                                                   
         BE    DMXR20                                                           
         LA    R3,L'COMPLST(R3)                                                 
         B     DMXR10                                                           
         DROP  R6                                                               
                                                                                
DMXR20   DS    0H                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING RSTAMKEL,R6                                                      
*                                                                               
* CHECK AGAINST TABLE OF COMPETING STATIONS TO DELETE FROM STATION              
*  RECORD                                                                       
*                                                                               
DMXR25   DS    0H                                                               
         CLC   RSTAMKST,5(R3)                                                   
         BE    DMXR30                                                           
                                                                                
* SPECIAL FOR WMDO                                                              
         CLC   =C'WMDO ',0(R3)     WMDO HAS WAJ AND WTTG TO BE DELETED          
         BNE   DMXR28                                                           
         CLC   RSTAMKST,=C'WTTG '                                               
         BE    DMXR30                                                           
                                                                                
DMXR28   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DMXR25                                                           
         B     DMXKEEP                                                          
                                                                                
DMXR30   DS    0H                                                               
         MVC   P(7),=C'STA DEL:'                                                
         MVC   P+9(5),0(R3)                                                     
         MVC   P+16(9),=C'MINI DEL:'                                            
         MVC   P+27(5),RSTAMKST                                                 
         GOTO1 VPRINTER                                                         
         AP    STACHG,=P'1'                                                     
                                                                                
         GOTO1 =V(RECUP),DMCB,(2,AREC),(R6)                                     
         CLC   =C'WMDO ',0(R3)     WMDO HAS WAJ AND WTTG TO BE DELETED          
         BNE   DMXKEEP                                                          
         L     R6,AREC                                                          
         B     DMXREC                                                           
         DROP  R6                                                               
                                                                                
DMXR100  DS    0H                                                               
         L     R6,AREC                                                          
         USING RCONREC,R6                                                       
         CLI   RCONKTYP,X'0C'      IF CONTRACT                                  
         BNE   DMXR500                                                          
         CLC   RCONKREP,=C'UV'                                                  
         BNE   DMXKEEP                                                          
         CLC   RCONKGRP,=C'TS'                                                  
         BNE   DMXKEEP                                                          
                                                                                
         LA    R3,COMPLST                                                       
DMXR110  DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   RCONKSTA,0(R3)                                                   
         BE    DMXR120                                                          
         LA    R3,L'COMPLST(R3)                                                 
         B     DMXR110                                                          
                                                                                
DMXR120  DS    0H                                                               
         MVC   P(5),=C'CONT:'                                                   
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,P+6)                                                
         MVC   P+16(5),RCONKSTA                                                 
         GOTO1 VPRINTER                                                         
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'06'        SPL ELEMENT                                  
         BAS   RE,GETEL                                                         
         BE    DMXR125                                                          
         MVC   P+22(12),=C'NO SPL FOUND'                                        
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
                                                                                
DMXR125  DS    0H                                                               
         MVC   P(16),=C'CHECKING MINI...'                                       
         GOTO1 VPRINTER                                                         
                                                                                
         USING RCONSPEL,R6                                                      
         CLI   RCONSPNU,0          SKIP IF NO MINI-ELEMENTS                     
         BNE   DMXR127                                                          
         MVC   P+22(17),=C'NO MINI ELT FOUND'                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
                                                                                
DMXR127  DS    0H                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,RCONSPLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONSPEL                                                 
         DROP  R6                                                               
                                                                                
         MVC   P(18),=C'CHECKING SWITCH...'                                     
         GOTO1 VPRINTER                                                         
                                                                                
         LA    R6,ELEM                                                          
         USING RCONSPEL,R6                                                      
         ZIC   R2,RCONSPNU         NUMBER OF MINI-ELEMENTS                      
         LA    R4,RCONSPST         START OF MINI-ELEMENT                        
                                                                                
DMXR130  DS    0H                  CHECK AGAINST TABLE                          
         CLC   5(5,R3),0(R4)                                                    
         BE    DMXR140                                                          
                                                                                
* SPECIAL FOR WMDO                                                              
         CLC   =C'WMDO ',0(R3)     WMDO HAS WAJ AND WTTG TO BE DELETED          
         BNE   DMXR135                                                          
         CLC   =C'WTTG ',0(R4)                                                  
         BE    DMXR140                                                          
                                                                                
DMXR135  DS    0H                                                               
         LA    R4,9(R4)                                                         
         BCT   R2,DMXR130                                                       
         MVC   P+22(17),=C'NO MINI ELT MATCH'                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP             NO MATCH                                     
                                                                                
DMXR140  DS    0H                  GOT A MATCH                                  
         MVC   P+22(5),=C'MINI:'                                                
         MVC   P+28(5),0(R4)                                                    
         GOTO1 VPRINTER                                                         
                                                                                
         ZIC   RE,RCONSPNU                                                      
         LA    RF,RCONSPST         START OF MINI-ELEMENT                        
                                                                                
DMXR150  DS    0H                  GOT A MATCH                                  
         CLC   10(5,R3),0(RF)                                                   
         BE    DMXR160                                                          
         LA    RF,9(RF)                                                         
         BCT   RE,DMXR150                                                       
         MVC   P+34(16),=C'NO SWITCH TO FND'                                    
         GOTO1 VPRINTER                                                         
         MVC   0(5,R4),10(R3)      NO SWITCH TO STATION FOUND, CHG IT           
         B     DMXR200             GO DELETE OLD AND ADD NEW                    
                                                                                
DMXR160  DS    0H                  AFFILIATE FOUND                              
         MVC   P+34(10),=C'SWITCH TO:'                                          
         MVC   P+45(5),0(RF)                                                    
         MVC   P+51(4),=C'AMT:'                                                 
         EDIT  (4,5(R4)),(8,P+56)                                               
                                                                                
         OC    5(4,R4),5(R4)                                                    
         BZ    DMXR170             ANY AMOUNTS TO 'SMUSH'?                      
         L     R0,5(R4)                                                         
         L     R1,5(RF)                                                         
         AR    R0,R1               YES, SMUSH DATA                              
         ST    R0,5(RF)                                                         
         MVC   P+66(4),=C'TOT:'                                                 
         EDIT  (4,5(RF)),(8,P+71)                                               
         B     DMXR170                                                          
                                                                                
DMXR170  DS    0H                  REMOVE 'DELETED' STATION                     
         GOTO1 VPRINTER                                                         
         ZIC   RE,RCONSPNU                                                      
         BCTR  RE,0                                                             
         STC   RE,RCONSPNU                                                      
         CH    R2,=H'1'                                                         
         BNH   DMXR180                                                          
         BCTR  R2,0                                                             
         MH    R2,=H'9'                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),9(R4)                                                    
                                                                                
DMXR180  DS    0H                                                               
         ZIC   RE,RCONSPLN         ADJUST ELEMENT LENGTH                        
         SH    RE,=H'9'                                                         
         STC   RE,RCONSPLN                                                      
         DROP  R6                                                               
                                                                                
DMXR200  DS    0H                                                               
         L     R6,AREC                                                          
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         LR    R5,R6                                                            
                                                                                
* DELETE OLD SPL ELEMENT                                                        
         GOTO1 =V(RECUP),DMCB,(2,AREC),(R6)                                     
                                                                                
* ADD NEW SPL ELEMENT                                                           
         GOTO1 =V(RECUP),DMCB,(2,AREC),ELEM,(R5)                                
         L     R6,AREC                                                          
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    DMXR210                                                          
         MVC   P(17),=C'*** ERROR !!! ***'                                      
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
                                                                                
DMXR210  DS    0H                                                               
         MVC   P(6),=C'ADDED:'                                                  
         USING RCONSPEL,R6                                                      
         ZIC   R1,RCONSPLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+8(0),0(R6)                                                     
         GOTO1 VPRINTER                                                         
         AP    CONCHG,=P'1'                                                     
                                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DMXR220                                                          
         MVC   P(27),=C'*** ERROR - DUP SPL !!! ***'                            
         GOTO1 VPRINTER                                                         
                                                                                
DMXR220  DS    0H                                                               
         CLC   =C'WMDO ',0(R3)     WMDO HAS WAJ AND WTTG TO BE DELETED          
         BNE   DMXKEEP                                                          
         L     R6,AREC                                                          
         B     DMXR100                                                          
         DROP  R6                                                               
         EJECT                                                                  
********************************************************************            
* ALSO PURGE BAD X'1A' AGENCY 2 RECORDS                                         
********************************************************************            
DMXR500  DS    0H                                                               
         L     R6,AREC             POINT TO RECORD                              
         USING RAGY2REC,R6                                                      
         CLI   RAGK2TYP,X'1A'      AGENCY 2 REC                                 
         BNE   DMXKEEP                                                          
         CLI   RAGY2FXL,0          IF ELEMENT HAS NO LENGTH                     
         BNE   DMXKEEP             DELETE IT!                                   
         MVC   P(9),=C'AGY2 DEL:'                                               
         MVC   P+11(4),RAGK2AGY                                                 
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
         DROP  R6                                                               
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
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'STA CHG'                                               
         EDIT  (P5,STACHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CON CHG'                                               
         EDIT  (P5,CONCHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(33),=C'DETAILS OF PURGED RECORDS FOLLOWS'                    
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,CODES                                                         
DC10     MVC   P+3(8),0(R5)                                                     
         EDIT  (P5,10(R5)),(7,P+13)                                             
         GOTO1 VPRINTER                                                         
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   DC10                                                             
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
STACHG   DC    PL5'0'                                                           
CONCHG   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CODES    DS    0CL15                                                            
         DC    CL8'REP     ',AL1(01),AL1(RREPKREP-RREPREC),PL5'0'               
         DC    CL8'STATION ',AL1(02),AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    CL8'REGION  ',AL1(03),AL1(RREGKREP-RREGREC),PL5'0'               
         DC    CL8'OFFICE  ',AL1(04),AL1(ROFFKREP-ROFFREC),PL5'0'               
         DC    CL8'DIVISION',AL1(05),AL1(RTEMKREP-RTEMREC),PL5'0'               
         DC    CL8'MAN     ',AL1(06),AL1(RSALKREP-RSALREC),PL5'0'               
         DC    CL8'GROUP   ',AL1(07),AL1(RGRPKREP-RGRPREC),PL5'0'               
         DC    CL8'ADVERTIS',AL1(08),AL1(RADVKREP-RADVREC),PL5'0'               
         DC    CL8'PRODUCT ',AL1(09),AL1(RPRDKREP-RPRDREC),PL5'0'               
         DC    CL8'AGENCY  ',AL1(10),AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    CL8'BUY     ',AL1(11),AL1(RBUYKREP-RBUYREC),PL5'0'               
         DC    CL8'CONTRACT',AL1(12),AL1(RCONKREP-RCONREC),PL5'0'               
         DC    CL8'CLASS   ',AL1(13),AL1(RCLSKREP-RCLSREC),PL5'0'               
         DC    CL8'CATEGORY',AL1(15),AL1(RCTGKREP-RCTGREC),PL5'0'               
         DC    CL8'INVENTRY',AL1(18),AL1(RINVKREP-RINVREC),PL5'0'               
         DC    CL8'BUDGET  ',AL1(19),AL1(RBUDKREP-RBUDREC),PL5'0'               
         DC    CL8'AVAIL   ',AL1(20),AL1(RAVLKREP-RAVLREC),PL5'0'               
         DC    CL8'PROPOSAL',AL1(22),AL1(RPRPKREP-RPRPREC),PL5'0'               
         DC    CL8'EOM     ',AL1(24),AL1(REOMKREP-REOMREC),PL5'0'               
         DC    CL8'DEMOMENU',AL1(35),AL1(RDEMKREP-RDEMREC),PL5'0'               
         DC    CL8'DAYPART ',AL1(36),AL1(24),PL5'0'                             
         DC    CL8'PRG TYPE',AL1(37),AL1(24),PL5'0'                             
         DC    CL8'SDD     ',AL1(38),AL1(20),PL5'0'                             
         DC    CL8'ATHENA  ',AL1(39),AL1(01),PL5'0'                             
         DC    CL8'CMISSION',AL1(41),AL1(11),PL5'0'                             
         DC    CL8'OWNRSHIP',AL1(42),AL1(22),PL5'0'                             
         DC    CL8'MARKET  ',AL1(43),AL1(21),PL5'0'                             
         DC    CL8'SBB     ',AL1(45),AL1(12),PL5'0'                             
         DC    CL8'COMMENT ',AL1(46),AL1(11),PL5'0'                             
         DC    CL8'TYPE    ',AL1(48),AL1(17),PL5'0'                             
         DC    X'FF'                                                            
         EJECT                                                                  
COMPLST  DS    0CL15                                                            
         DC    CL5'KORO ',CL5'XHRI ',CL5'KAJA '                                 
         DC    CL5'KREN ',CL5'KOLO ',CL5'KREN '                                 
         DC    CL5'KTA  ',CL5'KADY ',CL5'KTA  '                                 
         DC    CL5'KUVN ',CL5'KTMD ',CL5'KFWD '                                 
         DC    CL5'KVER ',CL5'KESQ ',CL5'KVER '                                 
         DC    CL5'KWEX ',CL5'KBG  ',CL5'KVDA '                                 
         DC    CL5'KXLN ',CL5'KTFH ',CL5'KTMD '                                 
         DC    CL5'WBL  ',CL5'WTMV ',CL5'WBL  '                                 
         DC    CL5'WBV  ',CL5'WGPR ',CL5'WBV  '                                 
         DC    CL5'WMDO ',CL5'WAJ  ',CL5'WMDO '                                 
         DC    CL5'WMDO ',CL5'WTTG ',CL5'WMDO '                                 
         DC    CL5'KBNT ',CL5'XETV ',CL5'XHAS '                                 
         DC    CL5'KBX  ',CL5'KAN  ',CL5'KBX  '                                 
         DC    X'FF'                                                            
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
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
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
ELEM     DS    CL256                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENAGY2                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'254RELDSPFX  05/01/02'                                      
         END                                                                    
