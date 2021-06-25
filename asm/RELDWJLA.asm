*          DATA SET RELDWJLA   AT LEVEL 033 AS OF 05/01/02                      
*PHASE RELDWJLA,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'RELDWJLA - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
***********************************************************************         
*    EXTERN TO DELETE KATZ AMERICAN WJLA $0 INV BUCKETS GENERATED               
*        BY ERROR WHEN ACTUALIZATION ENCOUNTERED FUTURE DATE                    
***********************************************************************         
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
         L     RF,COUNT1                                                        
         LA    RF,1(RF)                                                         
         ST    RF,COUNT1                                                        
         L     RF,COUNT2                                                        
         LA    RF,1(RF)                                                         
         ST    RF,COUNT2                                                        
         CLC   COUNT2,=F'5000'     NTH RECORD BREAK?                            
         BNE   DMX10                                                            
         XC    COUNT2,COUNT2                                                    
         MVC   P+1(07),=C'RECORD '                                              
         EDIT  COUNT1,(8,P+12)                                                  
         MVC   P+24(27),0(R5)       MOVE KEY TO PRINT                           
         GOTO1 VPRINTER                                                         
DMX10    EQU   *                                                                
         CLI   RCONKTYP,X'0C'      CONTRACT RECORD?                             
         BNE   DMXKEEP             NO                                           
         CLC   RCONKREP,=C'AM'     KATZ AMERICAN?                               
         BNE   DMXKEEP             NO                                           
         CLC   RCONKSTA(4),=C'WJLA'  STATION = WJLA?                            
         BNE   DMXKEEP             NO                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVI   DISPFLAG,0          CLEAR DISPLAY FLAG                           
         L     R3,AREC             SET A(RECORD) FOR PRNTBL                     
DMX20    EQU   *                                                                
         L     R5,AREC             SET A(RECORD IN PROGRESS)                    
         MVI   ELCODE,X'04'        FIND INVOICE BUCKETS                         
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     DMX30                                                            
DMX25    EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
DMX30    EQU   *                                                                
         BNE   DMX100              FINISHED                                     
         USING RCONSTEL,R5                                                      
         CLC   RCONSTYR(2),=X'6304'                                             
*                                  PRE APRIL 99?                                
         BL    DMX25               YES - GO BACK FOR NEXT                       
*                                  NO                                           
         CLC   RCONSTYR(2),=X'6309'                                             
*                                  POST SEP 99?                                 
         BH    DMX25               YES - GO BACK FOR NEXT                       
         OC    RCONSTAM,RCONSTAM   ANY DOLLARS?                                 
         BNZ   DMX25               YES - GO BACK FOR NEXT                       
*                                                                               
         DROP  R5                                                               
*                                                                               
         CLI   DISPFLAG,0          NO  - ALREADY PRINTED?                       
         BNE   DMX40               YES - DON'T REPRINT                          
         LA    R6,=C'PRE  ='                                                    
         MVC   HALF,RCONLEN-RCONREC(R3)                                         
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(6,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         MVC   DISPFLAG,1          SET 'PRINTED' FLAG                           
DMX40    EQU   *                                                                
         MVI   0(R5),X'FF'         RESET ELEMENT CODE                           
*        GOTO1 RECUP,DMCB,(C'R',AREC),(R5),0                                    
*                                  DELETE THE ELEMENT                           
         B     DMX25               GO BACK FOR NEXT ELEMENT                     
DMX100   EQU   *                                                                
         CLI   DISPFLAG,0          SOMETHING TO PRINT?                          
         BE    DMXKEEP             NO                                           
         GOTO1 =V(HELLO),DMCB,(C'D',=CL8'REPFILE'),(X'FF',AREC),0,0             
         LA    R6,=C'POST ='       YES                                          
         MVC   HALF,RCONLEN-RCONREC(R3)                                         
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(6,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
*                                                                               
*                                                                               
         AP    CHANGE,=P'1'                                                     
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
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CONFIRM'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'STA:   '                                               
         EDIT  (P5,STACTR),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CON:   '                                               
         EDIT  (P5,CONCTR),(7,P+12)                                             
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
STACTR   DC    PL5'0'                                                           
CONCTR   DC    PL5'0'                                                           
PRTCTR   DC    PL5'0'                                                           
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
DISPFLAG DS    XL1                                                              
COUNT1   DS    F                                                                
COUNT2   DS    F                                                                
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
RECUP    DS    A                                                                
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
**PAN#1  DC    CL21'033RELDWJLA  05/01/02'                                      
         END                                                                    
