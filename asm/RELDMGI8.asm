*          DATA SET RELDMGI8   AT LEVEL 014 AS OF 05/01/02                      
*PHASE RELDMGI8,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'RELDMGI8 - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
**********************************************************************          
* EXTERN:  INTEREP MCGAVERN/CABALLERO MERGE                          *          
*          STRIP OUT STATIONS/BUDGETS/COMMISSIONS/                   *          
*          THESE RECORDS WILL BE READDED FROM THE CONVERSION         *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
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
*  KRG NEW REP CODE CHANGE                                        *             
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
***      AP    10(5,R5),=P'1'                                                   
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
         USING RAURREC,R5                                                       
*                                                                               
*******************************************************************             
*   CODE TO DROP ALL X'2C' RECORDS FROM FILE FOR INTEREP, AND LEAVE             
*        '2C'S FOR OTHER REPS.                                                  
*   REMOVE THESE LINES WHEN RUNNING CONVERSION!!                                
*                                                                               
         CLI   RAURKTYP,X'2C'      AUR RECORD?                                  
         BNE   DMX0060             NO  - KEEP IT                                
         CLC   RAURKREP,=C'B1'     YES - KEEP THIS REP?                         
         BE    DMX0050             YES - KEEP IT                                
         CLC   RAURKREP,=C'EE'     NO  - KEEP THIS REP?                         
         BE    DMX0050             YES - KEEP IT                                
         CLC   RAURKREP,=C'NN'     NO  - KEEP THIS REP?                         
         BE    DMX0050             YES - KEEP IT                                
         CLC   RAURKREP,=C'UV'     NO  - KEEP THIS REP?                         
         BE    DMX0050             YES - KEEP IT                                
         L     RF,PURG2C                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PURG2C                                                        
         CLC   PURG2C,=F'50'       TEST DISPLAY                                 
         BH    DMX0040                                                          
         EDIT  PURG2C,(4,P+1)                                                   
         MVC   P+10(06),=C'PURG2C'                                              
         MVC   P+18(27),RAURKEY                                                 
         GOTO1 VPRINTER                                                         
DMX0040  EQU   *                                                                
         B     DMXPURGE            NO  - DROP THE RECORD!!                      
*                                                                               
*   REMOVE THESE LINES WHEN RUNNING PRODUCTION!!                                
*******************************************************************             
*                                                                               
DMX0050  EQU   *                                                                
         L     RF,KEEP2C                                                        
         LA    RF,1(RF)                                                         
         ST    RF,KEEP2C                                                        
         CLC   KEEP2C,=F'50'       TEST DISPLAY                                 
         BH    DMX0060                                                          
         EDIT  KEEP2C,(4,P+1)                                                   
         MVC   P+10(06),=C'KEEP2C'                                              
         MVC   P+18(27),RAURKEY                                                 
         GOTO1 VPRINTER                                                         
         DROP  R5                                                               
DMX0060  EQU   *                                                                
         L     R5,AREC             POINT TO RECORD                              
         USING RSTAREC,R5                                                       
         CLI   RSTAKTYP,X'02'      STATION RECORD?                              
         BNE   DMX0100             NO                                           
         CLC   =C'MG',RSTAKREP     MCGAVERN?                                    
         BNE   DMXKEEP             NO  - KEEP IT                                
         MVC   P+2(2),RSTAKREP                                                  
         MVC   P+6(5),RSTAKSTA                                                  
         MVC   P+25(15),=C'STATION DROPPED'                                     
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXPURGE                                                         
DMX0100  EQU   *                                                                
         L     R5,AREC             POINT TO RECORD                              
         USING RBUDREC,R5                                                       
*                                                                               
         CLI   RBUDKTYP,X'13'      BUDGET  RECORD?                              
         BNE   DMX0200             NO                                           
         CLC   =C'MG',RBUDKREP     MCGAVERN?                                    
         BNE   DMXKEEP             NO  - KEEP IT                                
         MVC   P+2(2),RBUDKREP                                                  
         MVC   P+6(9),RBUDKYR                                                   
         MVC   P+25(15),=C'BUDGET  DROPPED'                                     
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXPURGE                                                         
         DROP  R5                                                               
DMX0200  EQU   *                                                                
         L     R5,AREC             POINT TO RECORD                              
         USING RCOMREC,R5                                                       
*                                                                               
         CLI   RCOMKTYP,X'29'      COMMISSION RECORD?                           
         BNE   DMXKEEP             NO  - LAST TYPE BEING CHECKED                
         CLC   =C'MG',RCOMKREP     MCGAVERN?                                    
         BNE   DMXKEEP             NO  - KEEP IT                                
         MVC   P+2(2),RCOMKREP                                                  
         MVC   P+6(16),RCOMKSTA                                                 
         MVC   P+25(15),=C'COMMISH DROPPED'                                     
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXPURGE                                                         
         DROP  R5                                                               
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
PURG2C   DS    F                                                                
KEEP2C   DS    F                                                                
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
*         RINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE REGENCOM                                                       
       ++INCLUDE REGENAUR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014RELDMGI8  05/01/02'                                      
         END                                                                    
