*          DATA SET RELDIGRP2  AT LEVEL 055 AS OF 05/01/02                      
*PHASE RELDIGR2,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RELDTEAM - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
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
* REARRANGE INTEREP GROUP/SUBGROUP CODES                          *             
*     VERSION TO CREATE A TESTFILE ONLY                           *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDIGRP CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDIGRP,RR=R5                                       
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
         L     RF,TOTRECS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TOTRECS                                                       
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
*        AP    10(5,R5),=P'1'                                                   
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
DMXREC   DS    0H                                                               
         L     R3,AREC             SET A(RECORD)                                
*                                                                               
*   TO ONLY STRIP OUT A SINGLE REP FROM THE TOTAL FILE, REACTIVATE              
*        THE ROUTINE BELOW, AND IN IT DETAIL THE REP DESIRED.                   
*                                                                               
***      BAS   RE,CHKREP           PROCESS THIS REP?                            
***      BNZ   DMXPURGE            NO  - SKIP IT                                
         LA    R2,RECCODES         SET A(RECORD TABLE)                          
         L     RF,PROCCTR1                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR1                                                      
         L     RF,PROCCTR2                                                      
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR2                                                      
         CLC   PROCCTR2,=F'5000'   DISPLAY EVERY 5000 RECORDS                   
         BNE   DMXR0020            NO                                           
         XC    PROCCTR2,PROCCTR2                                                
         MVC   P+1(06),=C'DMXREC'                                               
         MVC   P+10(27),0(R3)                                                   
         EDIT  PROCCTR1,(9,P+42)                                                
         GOTO1 VPRINTER                                                         
DMXR0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    DMXKEEP             YES - NOT IN TABLE: KEEP IT                  
         CLC   0(1,R3),DRECTYPE(R2) NO - RECORD TYPE FOUND IN TABLE?            
         BNE   DMXR0060            NO  -                                        
DMXR0040 EQU   *                   YES - PROCESS FURTHER                        
         CLI   0(R3),X'43'         PROPOSAL RECORD?                             
         BNE   DMXR0080            NO  - PROCESS FURTHER                        
         CLI   1(R3),X'01'         YES - SUBREC 'PROPOSAL'?                     
         BNE   DMXKEEP             NO  - KEEP THE RECORD                        
         B     DMXR0080            YES - PROCESS FURTHER                        
DMXR0060 EQU   *                                                                
         LA    R2,L'RECCODES(R2)   NO  - BUMP TO NEXT ENTRY                     
         B     DMXR0020            GO BACK FOR NEXT                             
DMXR0080 EQU   *                                                                
         LR    R4,R3               TEST REP CODE IN RECORD                      
         ZIC   R5,DDISPLAC(R2)     SET DISPLACEMENT TO CODE                     
         AR    R4,R5               SET A(REP CODE IN RECORD)                    
         LA    R5,REPTABLE         SET REP CODE TABLE                           
DMXR0100 EQU   *                                                                
         CLI   0(R5),0             END OF TABLE?                                
         BE    DMXKEEP             YES - REP NOT IN TABLE: KEEP                 
         CLC   0(2,R4),0(R5)       REP IN REC = REP IN TABLE?                   
         BE    DMXR0200            YES - PROCESS FURTHER                        
         LA    R5,L'REPTABLE(R5)   NO  - BUMP TO NEXT ENTRY                     
         B     DMXR0100            GO BACK FOR NEXT                             
DMXR0200 EQU   *                                                                
         MVC   NEWGROUP,2(R5)      SET UP NEW GROUP/SUBGRP CODE                 
         L     RF,DROUTINE(R2)     GET A(ROUTINE)                               
         BASR  RE,RF               GO TO ROUTINE                                
         EJECT                                                                  
*******************************************************************             
* CHKREP:  ACCESS CODE TABLE                                      *             
*          FIRST FIND RECORD TYPE IN TABLE                        *             
*          THEN CHECK REP CODE IN RECORD                          *             
*          IF NOT IR (INTEREP MASTER) AND NOT UO (CUMULUS), SKIP  *             
*             THE RECORD.                                         *             
*******************************************************************             
CHKREP   NTR1                                                                   
         LA    R2,CODES                                                         
CREP0020 EQU   *                                                                
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    CREP0800            YES - EXIT NOT ZERO                          
         CLC   0(1,R3),8(R2)       RECORD INPUT = TABLE RECTYPE?                
         BE    CREP0040            YES - PROCESS THIS RECORD                    
         LA    R2,L'CODES(R2)      BUMP TO NEXT ENTRY                           
         B     CREP0020                                                         
CREP0040 EQU   *                                                                
         LR    R4,R3               SET A(RECORD)                                
         ZIC   RF,9(R2)            GET DISPLACEMENT TO REP                      
         AR    R4,RF                                                            
         CLC   =C'IR',0(R4)        INTEREP RECORD?                              
         BNE   CREP0060            NO  - CHECK FOR CUMULUS                      
         CLI   8(R2),2             YES - STATION RECORD?                        
         BE    CREP0800            YES - DON'T ACCEPT IT                        
         B     CREP0820            NO  - ACCEPT IT                              
CREP0060 EQU   *                                                                
         CLC   =C'UO',0(R4)        CUMULUS RECORD?                              
         BE    CREP0820            YES - EXIT CC ZERO                           
CREP0800 EQU   *                                                                
***      MVC   P+1(09),=C'REJECTED:'                                            
***      MVC   P+10(27),0(R3)                                                   
***      GOTO1 VPRINTER                                                         
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CREP0900                                                         
CREP0820 EQU   *                                                                
***      MVC   P+1(09),=C'ACCEPTED:'                                            
***      MVC   P+10(27),0(R3)                                                   
***      GOTO1 VPRINTER                                                         
         MVC   0(2,R4),=C'4S'      INSERT REPLACEMENT REP CODE                  
         SR    R0,R0               SET CC ZERO                                  
CREP0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* RECORD PROCESSING ROUTINES                                      *             
*******************************************************************             
STARTN   EQU   *                                                                
         USING RSTAREC,R3                                                       
**       CLI   RSTAGRUP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'STATION1    '                                          
         L     RF,STACTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
         GOTO1 DISREC,DMCB,STACTR,0                                             
         MVC   RSTAGRUP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,STACTR,1                                             
         B     DMXKEEP                                                          
         DROP  R3                                                               
ST2RTN   EQU   *                                                                
         USING RSTAREC,R3                                                       
**       CLI   RSTAGRUP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'STATION2    '                                          
         L     RF,ST2CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ST2CTR                                                        
         GOTO1 DISREC,DMCB,ST2CTR,0                                             
         MVC   RSTAGRUP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,ST2CTR,1                                             
         B     DMXKEEP                                                          
         DROP  R3                                                               
CONRTN   EQU   *                                                                
         USING RCONREC,R3                                                       
**       CLI   RCONKGRP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'CONTRACT    '                                          
         L     RF,CONCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         GOTO1 DISREC,DMCB,CONCTR,0                                             
         MVC   RCONKGRP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,CONCTR,1                                             
         B     DMXKEEP                                                          
         DROP  R3                                                               
MKGRTN   EQU   *                                                                
         USING RMKGREC,R3                                                       
         LA    R5,34(R3)           SET A(01 ELT IN RECORD)                      
         MVI   ELCODE,X'0A'        SET ELEMENT CODE                             
         BAS   RE,GETEL                                                         
         BNZ   DMXKEEP             NO ELEMENT: KEEP RECORD                      
         USING RMKGXELQ,R5                                                      
**       CLI   RMKGXGRP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'MAKEGOOD    '                                          
         L     RF,MKGCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,MKGCTR                                                        
         GOTO1 DISREC,DMCB,MKGCTR,0                                             
         MVC   RMKGXGRP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,MKGCTR,1                                             
         B     DMXKEEP             KEEP RECORD                                  
         DROP  R3,R5                                                            
ATHRTN   EQU   *                                                                
         USING RATNREC,R3                                                       
**       CLI   RATNKGRP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'ATHENA      '                                          
         L     RF,ATHCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ATHCTR                                                        
         GOTO1 DISREC,DMCB,ATHCTR,0                                             
         MVC   RATNKGRP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,ATHCTR,1                                             
         B     DMXKEEP                                                          
         DROP  R3                                                               
AURRTN   EQU   *                                                                
         USING RAURREC,R3                                                       
**       CLI   RAURKGRP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'AUR         '                                          
         L     RF,AURCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AURCTR                                                        
         GOTO1 DISREC,DMCB,AURCTR,0                                             
         MVC   RAURKGRP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,AURCTR,1                                             
         B     DMXKEEP                                                          
         DROP  R3                                                               
STRRTN   EQU   *                                                                
         USING RSTRREC,R3                                                       
**       CLI   RSTRKGRP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'STRATEGY    '                                          
         L     RF,STRCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,STRCTR                                                        
         GOTO1 DISREC,DMCB,STRCTR,0                                             
         MVC   RSTRKGRP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,STRCTR,1                                             
         B     DMXKEEP                                                          
         DROP  R3                                                               
PRORTN   EQU   *                                                                
         USING RPROHDRD,R3                                                      
         LA    R5,34(R3)           SET A(01 ELT IN RECORD)                      
         MVI   ELCODE,X'02'        SET ELEMENT CODE                             
         BAS   RE,GETEL                                                         
         BNZ   DMXKEEP             NO ELEMENT: KEEP RECORD                      
         USING RPRSWELD,R5                                                      
**       CLI   RPRSWGRP,C'R'       RADIO GROUP CODE?                            
**       BNE   DMXKEEP             NO  - DON'T PROCESS                          
         MVC   RECTYP,=C'PROPOSAL    '                                          
         L     RF,PROCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PROCTR                                                        
         GOTO1 DISREC,DMCB,PROCTR,0                                             
         MVC   RPRSWGRP,NEWGROUP   INSERT NEW GROUP CODE                        
         GOTO1 DISREC,DMCB,PROCTR,1                                             
         B     DMXKEEP             KEEP RECORD                                  
         DROP  R3,R5                                                            
         EJECT                                                                  
DISREC   NTR1                                                                   
         L     R2,0(R1)            SET A(COUNTER)                               
         L     R4,4(R1)            LOAD FLAG                                    
***      MVC   P+1(04),=C'CTR='                                                 
***      MVC   P+5(4),0(R2)                                                     
***      GOTO1 VPRINTER                                                         
         CLC   0(4,R2),=F'10'      DISPLAY FIRST 10 CHANGES                     
         BH    DSRC0900            EXIT                                         
         GOTO1 VPRINTER            BLANK LINE                                   
         MVC   P+1(6),=C'PRE  ='                                                
         LTR   R4,R4                                                            
         BZ    DSRC0020                                                         
         MVC   P+1(6),=C'POST ='                                                
DSRC0020 EQU   *                                                                
         MVC   P+20(12),RECTYP                                                  
         EDIT  (4,(R2)),(6,P+8),                                                
         GOTO1 VPRINTER                                                         
         L     R3,AREC                                                          
         USING RCONREC,R3                                                       
         MVC   HALF,RCONLEN                                                     
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(0,(R3)),(R3),C'DUMP',(R8),=C'2D'                    
DSRC0900 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
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
         MVC   P+3(6),=C'KEPT  '                                                
         EDIT  TOTRECS,(7,P+11)                                                 
         GOTO1 VPRINTER                                                         
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
TOTRECS  DS    F                                                                
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
         DC    CL8'MAN2    ',XL1'46',AL1(RSA2KREP-RSA2REC),PL5'0'               
         DC    CL8'GROUP   ',XL1'07',AL1(RGRPKREP-RGRPREC),PL5'0'               
         DC    CL8'ADVERTIS',XL1'08',AL1(RADVKREP-RADVREC),PL5'0'               
         DC    CL8'PRODUCT ',XL1'09',AL1(RPRDKREP-RPRDREC),PL5'0'               
         DC    CL8'AGENCY  ',XL1'0A',AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    CL8'AGENCY2 ',XL1'1A',AL1(25),PL5'0'                             
         DC    CL8'BUY     ',XL1'0B',AL1(RBUYKREP-RBUYREC),PL5'0'               
         DC    CL8'CONTRACT',XL1'0C',AL1(RCONKREP-RCONREC),PL5'0'               
         DC    CL8'CLASS   ',XL1'0D',AL1(RCLSKREP-RCLSREC),PL5'0'               
         DC    CL8'CATEGORY',XL1'0F',AL1(RCTGKREP-RCTGREC),PL5'0'               
         DC    CL8'MAKEGOOD',XL1'11',AL1(RMKGKREP-RMKGREC),PL5'0'               
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
         DC    CL8'TKO EQUV',XL1'1F',AL1(16),PL5'0'                             
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
         DC    CL8'STRATEGY',XL1'39',AL1(RSTRKREP-RSTRREC),PL5'0'               
         DC    CL8'DEV SAL ',XL1'3A',AL1(22),PL5'0'                             
         DC    CL8'DEV K TP',XL1'3B',AL1(23),PL5'0'                             
         DC    CL8'TERRITOR',XL1'3D',AL1(RTERKREP-RTERREC),PL5'0'               
         DC    X'FF'                                                            
         EJECT                                                                  
         DS    0F                  SET FULL-WORD ALIGNMENT                      
DRECNAME EQU   0                                                                
DROUTINE EQU   8                                                                
DRECTYPE EQU   12                                                               
DDISPLAC EQU   13                                                               
RECCODES DS    0CL16                                                            
         DC    CL8'STATION ',A(STARTN),XL1'02',AL1(RSTAKREP-RSTAREC)            
         DC    XL2'00'                                                          
         DC    CL8'STATION2',A(ST2RTN),XL1'42',AL1(RSTAKREP-RSTAREC)            
         DC    XL2'00'                                                          
         DC    CL8'CONTRACT',A(CONRTN),XL1'0C',AL1(RCONKREP-RCONREC)            
         DC    XL2'00'                                                          
         DC    CL8'MAKEGOOD',A(MKGRTN),XL1'11',AL1(RMKGKREP-RMKGREC)            
         DC    XL2'00'                                                          
         DC    CL8'ATHENA  ',A(ATHRTN),XL1'27',AL1(01)                          
         DC    XL2'00'                                                          
         DC    CL8'AUR     ',A(AURRTN),XL1'2C',AL1(04)                          
         DC    XL2'00'                                                          
         DC    CL8'STRATEGY',A(STRRTN),XL1'39',AL1(RSTRKREP-RSTRREC)            
         DC    XL2'00'                                                          
         DC    CL8'PROPOSAL',A(PRORTN),XL1'43',AL1(RPROKRCD-RPROKEY)            
         DC    XL2'00'                                                          
*              REQUIRES SPECIAL CONSIDERATION: SUBKEY TYPE = 1                  
         DC    X'00'                                                            
         DC    H'00'                                                            
CTRLABEL DS    2F                                                               
PROCCTR1 DS    F                                                                
PROCCTR2 DS    F                                                                
STACTR   DS    F                                                                
ST2CTR   DS    F                                                                
CONCTR   DS    F                                                                
MKGCTR   DS    F                                                                
ATHCTR   DS    F                                                                
AURCTR   DS    F                                                                
STRCTR   DS    F                                                                
PROCTR   DS    F                                                                
RECTYP   DS    CL12                                                             
REPTABLE DC    C'AQRL'             ALLIED = RL                                  
         DC    C'MGRM'             MCGAVERN = RM                                
         DC    C'IFRI'             INFINITY = RI                                
         DC    C'I2RN'             NON-REP = RN                                 
         DC    C'I8RC'             CABALLERO = RC                               
         DC    C'D4RD'             D&R = RD                                     
         DC    C'IBRA'             ABC RADIO SALES = RA                         
         DC    C'NXRX'             PUBLIC NETWORK RADIO = ??                    
         DC    C'UORU'             CUMULUS = RU                                 
         DC    C'4SRU'                                                          
         DC    X'00'                                                            
         DC    H'00'                                                            
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
NEWGROUP DS    CL2                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE REGENALLD                                                      
         ORG RECORD                                                             
       ++INCLUDE REGENSTR                                                       
         ORG RECORD                                                             
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE REGENATNA                                                      
       ++INCLUDE REGENAUR                                                       
       ++INCLUDE REGENPRO                                                       
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENTER                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055RELDIGRP2 05/01/02'                                      
         END                                                                    
