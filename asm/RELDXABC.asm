*          DATA SET RELDXABC   AT LEVEL 017 AS OF 05/01/02                      
*PHASE RELDXABC,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RELDXABC - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
*******************************************************************             
* RELDXABC:  PURGES ALL RECORDS FOR SELECTED REP(S)                             
*                                                                               
*******************************************************************             
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
*******************************************************************             
*                                                                 *             
*   REP  FILE PURGE                                                             
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
         BE    PROCREC             PROCESS                                      
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
DMXPURGE EQU   *                                                                
*                                                                               
*   DISPLAY OUTPUT                                                              
         L     RF,PCOUNT3                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PCOUNT3                                                       
         L     RF,PCOUNT4                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PCOUNT4                                                       
         CLC   PCOUNT4,=F'100'     100TH RECORD?                                
         BNE   TEST0040            NO                                           
         MVC   P+1(07),=C'DROP:  '                                              
         EDIT  PCOUNT3,(8,P+12)                                                 
         MVC   P+24(27),0(R3)       MOVE KEY TO PRINT                           
         GOTO1 VPRINTER                                                         
         XC    PCOUNT4,PCOUNT4                                                  
TEST0040 EQU   *                                                                
*   END DISPLAY OUTPUT                                                          
*                                                                               
         L     R1,APARM            PURGE RECORD EXIT                            
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
PROCREC  DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
*   DISPLAY OUTPUT                                                              
         L     RF,PCOUNT1                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PCOUNT1                                                       
         L     RF,PCOUNT2                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PCOUNT2                                                       
         CLC   PCOUNT2,=F'10000'   10000TH RECORD?                              
         BNE   TEST0020            NO                                           
         MVC   P+1(07),=C'RECORD '                                              
         EDIT  PCOUNT1,(8,P+12)                                                 
         MVC   P+24(27),0(R3)       MOVE KEY TO PRINT                           
         GOTO1 VPRINTER                                                         
         XC    PCOUNT2,PCOUNT2                                                  
TEST0020 EQU   *                                                                
*   END DISPLAY OUTPUT                                                          
*                                                                               
PRRE0020 EQU   *                                                                
         LA    R5,PRGLST           REP PURGE LIST                               
         CLI   0(R5),X'FF'                                                      
         BE    DMXKEEP                                                          
                                                                                
*  PURGE ALL RECORDS FOR REPS IN PRGLST                                         
         LA    R5,CODES                                                         
*                                                                               
* GET DISPLACEMENT TO REP CODE IN RECORD                                        
PRRE0040 CLC   0(1,R3),8(R5)       RECORD TYPE TO TABLE                         
         BE    PRRE0080                                                         
PRRE0060 EQU   *                                                                
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   PRRE0040                                                         
         B     DMXKEEP             UNKNOWN REC - DON'T PURGE                    
                                                                                
PRRE0080 EQU   *                                                                
         CLI   15(R5),X'00'        REC ID SINGLE CHARACTER?                     
         BE    PRRE0100            YES -                                        
         CLC   1(1,R3),15(R5)      NO  - 2ND BYTE OF ID MATCH TABLE?            
         BNE   PRRE0060            NO  - GO TO NEXT TABLE SLOT                  
PRRE0100 EQU   *                                                                
         ZIC   R7,9(R5)            DISP TO REPCODE FROM TABLE                   
         AR    R7,R3               R7 TO REP CODE IN RECORD                     
*                                                                               
* SEE IF REP IS IN PURGE LIST                                                   
         LA    R4,PRGLST                                                        
PRRE0120 EQU   *                                                                
         CLI   0(R4),X'FF'         TEST FOR EOT                                 
***>>>   BE    PRRE0140            NOT IN PURGE LIST: CHECK CV                  
         BE    DMXKEEP             NOT IN PURGE LIST, KEEP REC                  
         CLC   0(2,R4),0(R7)                                                    
         BE    DMXPURGE            PURGE FOR THIS REP                           
         LA    R4,L'PRGLST(R4)                                                  
         B     PRRE0120                                                         
PRRE0140 EQU   *                                                                
         USING RCONREC,R3                                                       
         CLI   RCONREC,X'0C'       CONTRACT RECORD?                             
         BNE   DMXKEEP             NO                                           
         CLC   RCONKREP,=C'CV'     WABC TEST FILE?                              
         BNE   DMXKEEP             NO                                           
         MVC   P+1(16),=C'WABC CONTRACT : '                                     
         MVC   P+20(27),RCONREC                                                 
*                                                                               
*        CLI   RCONREC,X'01'       REP      RECORD?                             
*        BNE   DMXKEEP             NO                                           
*        MVC   P+1(16),=C'REP  RECORD   : '                                     
*        MVC   P+20(27),RCONREC                                                 
*                                                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+1(16),=C'REP  AFTER    : '                                     
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
PCOUNT1  DS    F'0'                                                             
PCOUNT2  DS    F'0'                                                             
PCOUNT3  DS    F'0'                                                             
PCOUNT4  DS    F'0'                                                             
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
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
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
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CODES    DS    0CL16                                                            
         DC    CL8'REP     ',XL1'01',AL1(RREPKREP-RREPREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'STATION ',XL1'02',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'STATION2',XL1'42',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'REGION  ',XL1'03',AL1(RREGKREP-RREGREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'OFFICE  ',XL1'04',AL1(ROFFKREP-ROFFREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'OFFICE2 ',XL1'44',AL1(ROFFKREP-ROFFREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'DIVISION',XL1'05',AL1(RTEMKREP-RTEMREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'MAN     ',XL1'06',AL1(RSALKREP-RSALREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'GROUP   ',XL1'07',AL1(RGRPKREP-RGRPREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'ADVERTIS',XL1'08',AL1(RADVKREP-RADVREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'PRODUCT ',XL1'09',AL1(RPRDKREP-RPRDREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'AGENCY  ',XL1'0A',AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'AGENCY2 ',XL1'1A',AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'BUY     ',XL1'0B',AL1(RBUYKREP-RBUYREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'CONTRACT',XL1'0C',AL1(RCONKREP-RCONREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'CLASS   ',XL1'0D',AL1(RCLSKREP-RCLSREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'EDI     ',XL1'0E',AL1(14),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'CATEGORY',XL1'0F',AL1(RCTGKREP-RCTGREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'MKGD OFF',XL1'11',AL1(06),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'INVENTRY',XL1'12',AL1(RINVKREP-RINVREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'BUDGET  ',XL1'13',AL1(RBUDKREP-RBUDREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'AVAIL   ',XL1'14',AL1(RAVLKREP-RAVLREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'1501 REC',XL1'15',AL1(23),PL5'0'                             
         DC    XL1'01'                                                          
         DC    CL8'1502 REC',XL1'15',AL1(23),PL5'0'                             
         DC    XL1'02'                                                          
         DC    CL8'1503 REC',XL1'15',AL1(23),PL5'0'                             
         DC    XL1'03'                                                          
         DC    CL8'1504 REC',XL1'15',AL1(23),PL5'0'                             
         DC    XL1'04'                                                          
         DC    CL8'1505 REC',XL1'15',AL1(23),PL5'0'                             
         DC    XL1'05'                                                          
         DC    CL8'1506 REC',XL1'15',AL1(23),PL5'0'                             
         DC    XL1'06'                                                          
         DC    CL8'PROPOSAL',XL1'16',AL1(RPRPKREP-RPRPREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'EOM     ',XL1'18',AL1(REOMKREP-REOMREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'OFF BUD ',XL1'19',AL1(17),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'EOP ADV ',XL1'1B',AL1(15),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'EOP AGY ',XL1'1C',AL1(13),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'EOP OFF ',XL1'1D',AL1(17),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'EOP SAL ',XL1'1E',AL1(16),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'TKO     ',XL1'1F',AL1(16),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'ALT CAL ',XL1'20',AL1(01),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'STN CTRL',XL1'21',AL1(15),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'OVR UPLD',XL1'22',AL1(13),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'DEMOMENU',XL1'23',AL1(RDEMKREP-RDEMREC),PL5'0'               
         DC    XL1'00'                                                          
         DC    CL8'DAYPART ',XL1'24',AL1(24),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'PRG TYPE',XL1'25',AL1(24),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'SDD     ',XL1'26',AL1(20),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'ATHENA  ',XL1'27',AL1(01),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'SWITCH  ',XL1'28',AL1(13),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'CMISSION',XL1'29',AL1(11),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'OWNRSHIP',XL1'2A',AL1(22),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'MARKET  ',XL1'2B',AL1(21),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'AUR     ',XL1'2C',AL1(04),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'SBB     ',XL1'2D',AL1(12),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'COMMENT ',XL1'2E',AL1(15),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'TYPE    ',XL1'30',AL1(17),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'PT PRSN ',XL1'31',AL1(22),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'K TYPE  ',XL1'32',AL1(24),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'RADAR   ',XL1'33',AL1(17),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'OCM     ',XL1'34',AL1(20),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'DIRESPON',XL1'35',AL1(13),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'LABEL   ',XL1'36',AL1(17),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'GOAL    ',XL1'37',AL1(13),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'SET     ',XL1'38',AL1(19),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'STRATEGY',XL1'39',AL1(13),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'DEV SAL ',XL1'3A',AL1(22),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'DEV K TP',XL1'3B',AL1(23),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'RSRCH DP',XL1'3C',AL1(24),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'TERRITOR',XL1'3D',AL1(23),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'GEN AVAL',XL1'3E',AL1(10),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'DARE    ',XL1'41',AL1(07),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'PROPOSAL',XL1'43',AL1(07),PL5'0'                             
         DC    XL1'01'                                                          
         DC    CL8'PROPOSAL',XL1'43',AL1(07),PL5'0'                             
         DC    XL1'02'                                                          
         DC    CL8'SCRIBE  ',XL1'45',AL1(03),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'CFC     ',XL1'47',AL1(21),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'COVRSHT ',XL1'49',AL1(16),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'BUS ACT ',XL1'4A',AL1(17),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'DARE II ',XL1'51',AL1(07),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'EDI PERF',XL1'52',AL1(20),PL5'0'                             
         DC    XL1'00'                                                          
         DC    CL8'DR NOTIF',XL1'53',AL1(20),PL5'0'                             
         DC    XL1'00'                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
* TABLE OF REP CODES TO PURGE                                                   
*                                                                               
PRGLST   DS    0CL2                                                             
         DC    C'I8'                                                            
         DC    C'S1'                                                            
         DC    C'RM'                                                            
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
**PAN#1  DC    CL21'017RELDXABC  05/01/02'                                      
         END                                                                    
