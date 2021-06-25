*          DATA SET RELDPET    AT LEVEL 024 AS OF 05/01/02                      
*PHASE RELDPET,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'RELDPET - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*                                                                 *             
*  FIX PETRY SPL ELEMENT                                          *             
*                                                                 *             
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
         AP    10(5,R5),=P'1'                                                   
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
         L     R5,AREC             POINT TO RECORD                              
         USING RCONREC,R5                                                       
*                                                                               
         CLI   RCONKTYP,X'0C'                                                   
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   =C'PV',RCONKREP                                                  
         BNE   DMXKEEP                                                          
*                                                                               
         AP    KCOUNT,=P'1'                                                     
         CLC   KCOUNT,=PL5'20000'                                               
         BL    DMXREC05                                                         
         MVC   P+3(7),=C'CON:   '                                               
         EDIT  (P5,KCOUNT),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         ZAP   KCOUNT,=P'0'                                                     
*                                                                               
DMXREC05 DS    0H                                                               
         CLC   =C'WKYC ',RCONKSTA                                               
         BNE   DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
*                                                                               
         MVI   MATCH,C'N'                                                       
         MVC   P(10),=C'FOUND ONE!'                                             
         AP    COUNT,=P'1'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         XC    ELEM2,ELEM2                                                      
         USING RCONSPEL,R5                                                      
         ZIC   R2,RCONSPLN                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONSPEL                                                 
         DROP  R5                                                               
*                                                                               
         LA    R4,ELEM+9                                                        
*                                                                               
DMXREC09 DS    0H                                                               
         LA    R3,9(R4)                                                         
*                                                                               
DMXREC10 DS    0H                                                               
         CLI   0(R3),X'00'                                                      
         BE    DMXREC20                                                         
         CLI   0(R3),C'*'                                                       
         BE    DMXREC13                                                         
         CLC   0(5,R4),0(R3)                                                    
         BE    DMXREC15                                                         
*                                                                               
DMXREC13 DS    0H                                                               
         LA    R3,9(R3)                                                         
         B     DMXREC10                                                         
*                                                                               
DMXREC15 DS    0H                                                               
         ZICM  R7,5(R4),4                                                       
         ZICM  R8,5(R3),4                                                       
         AR    R7,R8                                                            
         STCM  R7,15,5(R4)                                                      
         MVI   0(R3),C'*'                                                       
         MVI   MATCH,C'Y'                                                       
         B     DMXREC10                                                         
*                                                                               
DMXREC20 DS    0H                                                               
         LA    R4,9(R4)                                                         
         CLI   0(R4),C'*'                                                       
         BE    DMXREC20                                                         
         CLI   0(R4),X'00'                                                      
         BNE   DMXREC09                                                         
*                                                                               
* NOW COLLAPSE THE ELEMENT                                                      
*                                                                               
         CLI   MATCH,C'N'                                                       
         BNE   DMXREC23                                                         
*                                                                               
         L     R5,AREC                                                          
         USING RCONREC,R5                                                       
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+4,4,=C'TOG'                           
         MVC   P+16(9),=C'**SKIPPED'                                            
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
DMXREC23 DS    0H                                                               
         MVC   ELEM2(18),ELEM                                                   
         LA    R3,ELEM2+18                                                      
         LA    R4,ELEM+18                                                       
DMXREC25 CLI   0(R4),X'00'                                                      
         BE    DMXREC40                                                         
         CLI   0(R4),C'*'                                                       
         BNE   DMXREC30                                                         
         LA    R4,9(R4)                                                         
         B     DMXREC25                                                         
*                                                                               
DMXREC30 DS    0H                                                               
         MVC   0(9,R3),0(R4)                                                    
         LA    R3,9(R3)                                                         
         LA    R4,9(R4)                                                         
         B     DMXREC25                                                         
*                                                                               
* NOW CALCULATE NUMBER OF MINI-ELEMENTS                                         
*                                                                               
DMXREC40 DS    0H                                                               
         LA    R4,ELEM2+9                                                       
         SR    R3,R3                                                            
DMXREC45 CLI   0(R4),X'00'                                                      
         BE    DMXREC50                                                         
         LA    R4,9(R4)                                                         
         LA    R3,1(R3)                                                         
         B     DMXREC45                                                         
*                                                                               
DMXREC50 DS    0H                                                               
         STC   R3,ELEM2+8                                                       
*                                                                               
         MH    R3,=H'9'            NUMBER OF MINI-ELEMENT * LEN OF EACH         
         LA    R3,9(R3)            PLUS NINE FOR OVERHEAD                       
         STC   R3,ELEM2+1          EQUAL LENGTH OF ELEMENT                      
*                                                                               
         GOTO1 =V(RECUP),DMCB,(2,AREC),(R5),0                                   
         GOTO1 =V(RECUP),DMCB,(2,AREC),ELEM2,(R5)                               
*                                                                               
         AP    DCOUNT,=P'1'                                                     
*                                                                               
         L     R5,AREC                                                          
         USING RCONREC,R5                                                       
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+4,4,=C'TOG'                           
         GOTO1 VPRINTER                                                         
         DROP  R5                                                               
*                                                                               
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(8),=C'ELEMENT:'                                                
         GOTO1 VPRINTER                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
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
         MVC   P+3(6),=C'TOTAL:'                                                
         EDIT  (P5,COUNT),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'DUPES:'                                                
         EDIT  (P5,DCOUNT),(7,P+11)                                             
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
KCOUNT   DC    PL5'0'                                                           
SCOUNT   DC    PL5'0'                                                           
COUNT    DC    PL5'0'                                                           
DCOUNT   DC    PL5'0'                                                           
MATCH    DC    C'N'                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CODES    DS    0CL15                                                            
         DC    CL8'REP     ',XL1'01',AL1(RREPKREP-RREPREC),PL5'0'               
         DC    CL8'STATION ',XL1'02',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    CL8'STATION2',XL1'42',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    CL8'REGION  ',XL1'03',AL1(RREGKREP-RREGREC),PL5'0'               
         DC    CL8'OFFICE  ',XL1'04',AL1(ROFFKREP-ROFFREC),PL5'0'               
         DC    CL8'OFFICE2 ',XL1'44',AL1(22),PL5'0'                             
         DC    CL8'DIVISION',XL1'05',AL1(RTEMKREP-RTEMREC),PL5'0'               
         DC    CL8'MAN     ',XL1'06',AL1(RSALKREP-RSALREC),PL5'0'               
         DC    CL8'GROUP   ',XL1'07',AL1(RGRPKREP-RGRPREC),PL5'0'               
         DC    CL8'ADVERTIS',XL1'08',AL1(RADVKREP-RADVREC),PL5'0'               
         DC    CL8'PRODUCT ',XL1'09',AL1(RPRDKREP-RPRDREC),PL5'0'               
         DC    CL8'AGENCY  ',XL1'0A',AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    CL8'AGENCY2 ',XL1'1A',AL1(25),PL5'0'                             
         DC    CL8'BUY     ',XL1'0B',AL1(RBUYKREP-RBUYREC),PL5'0'               
         DC    CL8'CONTRACT',XL1'0C',AL1(RCONKREP-RCONREC),PL5'0'               
         DC    CL8'CLASS   ',XL1'0D',AL1(RCLSKREP-RCLSREC),PL5'0'               
         DC    CL8'CATEGORY',XL1'0F',AL1(RCTGKREP-RCTGREC),PL5'0'               
         DC    CL8'INVENTRY',XL1'12',AL1(RINVKREP-RINVREC),PL5'0'               
         DC    CL8'BUDGET  ',XL1'13',AL1(RBUDKREP-RBUDREC),PL5'0'               
         DC    CL8'AVAIL   ',XL1'14',AL1(RAVLKREP-RAVLREC),PL5'0'               
         DC    CL8'PROPOSAL',XL1'16',AL1(RPRPKREP-RPRPREC),PL5'0'               
         DC    CL8'EOM     ',XL1'18',AL1(REOMKREP-REOMREC),PL5'0'               
         DC    CL8'OFF BUD ',XL1'19',AL1(17),PL5'0'                             
         DC    CL8'EOP ADV ',XL1'1B',AL1(15),PL5'0'                             
         DC    CL8'EOP AGY ',XL1'1C',AL1(13),PL5'0'                             
         DC    CL8'EOP OFF ',XL1'1D',AL1(17),PL5'0'                             
         DC    CL8'EOP SAL ',XL1'1E',AL1(16),PL5'0'                             
         DC    CL8'OVR UPLD',XL1'22',AL1(13),PL5'0'                             
         DC    CL8'DEMOMENU',XL1'23',AL1(RDEMKREP-RDEMREC),PL5'0'               
         DC    CL8'DAYPART ',XL1'24',AL1(24),PL5'0'                             
         DC    CL8'PRG TYPE',XL1'25',AL1(24),PL5'0'                             
         DC    CL8'SDD     ',XL1'26',AL1(20),PL5'0'                             
         DC    CL8'ATHENA  ',XL1'27',AL1(01),PL5'0'                             
         DC    CL8'CMISSION',XL1'29',AL1(11),PL5'0'                             
         DC    CL8'OWNRSHIP',XL1'2A',AL1(22),PL5'0'                             
         DC    CL8'MARKET  ',XL1'2B',AL1(21),PL5'0'                             
         DC    CL8'AUR     ',XL1'2C',AL1(04),PL5'0'                             
         DC    CL8'SBB     ',XL1'2D',AL1(12),PL5'0'                             
         DC    CL8'COMMENT ',XL1'2E',AL1(15),PL5'0'                             
         DC    CL8'TYPE    ',XL1'30',AL1(17),PL5'0'                             
         DC    CL8'PT PRSN ',XL1'31',AL1(22),PL5'0'                             
         DC    CL8'K TYPE  ',XL1'32',AL1(24),PL5'0'                             
         DC    CL8'RADAR   ',XL1'33',AL1(17),PL5'0'                             
         DC    CL8'OCM     ',XL1'34',AL1(20),PL5'0'                             
         DC    CL8'DIRESPON',XL1'35',AL1(13),PL5'0'                             
         DC    CL8'LABEL   ',XL1'36',AL1(17),PL5'0'                             
         DC    CL8'GOAL    ',XL1'37',AL1(13),PL5'0'                             
         DC    CL8'SET     ',XL1'38',AL1(19),PL5'0'                             
         DC    CL8'STRATEGY',XL1'39',AL1(13),PL5'0'                             
         DC    CL8'DEV SAL ',XL1'3A',AL1(22),PL5'0'                             
         DC    CL8'DEV K TP',XL1'3B',AL1(23),PL5'0'                             
         DC    X'FF'                                                            
         EJECT                                                                  
* TABLE OF REP CODES TO PURGE                                                   
*                                                                               
PRGLST   DS    0CL2                                                             
         DC    C'YH'                                                            
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
ELEM2    DS    CL256                                                            
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
**PAN#1  DC    CL21'024RELDPET   05/01/02'                                      
         END                                                                    
