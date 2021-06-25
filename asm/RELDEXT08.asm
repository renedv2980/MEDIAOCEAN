*          DATA SET RELDEXT08  AT LEVEL 220 AS OF 05/01/02                      
*PHASE RELDEXT,*                                                                
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
*                                                                 *             
*   NOTE - TO DELETE A COMPETITIVE TV STATION, SEE RESPLFIX       *             
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
         L     R3,AREC             POINT TO RECORD                              
*        SPACE 1                                                                
* PURGE OVERNIGHT RECORDS FOR SPECIFIC MARKETS --TEMPORARY                      
*        CLI   0(R3),X'22'                                                      
*        BNE   DMXRECX                                                          
*        CLC   13(2,R3),=C'BL'                                                  
*        BNE   DMXRECX                                                          
*        CLC   20(3,R3),=C'CHI'                                                 
*        BE    DMXPURGE                                                         
*        CLC   20(3,R3),=C'DET'                                                 
*        BE    DMXPURGE                                                         
*        CLC   20(3,R3),=C'HOU'                                                 
*        BE    DMXPURGE                                                         
*        CLC   20(3,R3),=C'MIA'                                                 
*        BE    DMXPURGE                                                         
*        CLC   20(3,R3),=C'NEW'                                                 
*        BE    DMXPURGE                                                         
*        SPACE 1                                                                
*  ANY REPS TO BE PURGED?                                                       
*         SPACE 1                                                               
*DMXRECX  LA    R5,PRGLST           REP PURGE LIST                              
*         CLI   0(R5),X'FF'                                                     
*         BE    DMXREC9                                                         
         SPACE 1                                                                
*  PURGE ALL RECORDS FOR REPS IN PRGLST                                         
         LA    R5,CODES                                                         
*                                                                               
* GET DISPLACEMENT TO REP CODE IN RECORD                                        
DMXREC1  CLC   0(1,R3),8(R5)       RECORD TYPE TO TABLE                         
         BE    DMXREC2                                                          
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   DMXREC1                                                          
         B     DMXKEEP             UNKNOWN REC - DON'T PURGE                    
         SPACE 1                                                                
DMXREC2  ZIC   R7,9(R5)            DISP TO REPCODE                              
         AR    R7,R3               R7 TO REP CODE IN RECORD                     
*                                                                               
* SEE IF REP IS IN PURGE LIST                                                   
         LA    R4,PRGLST                                                        
DMXREC3  CLI   0(R4),X'FF'         TEST FOR EOT                                 
         BE    DMXKEEP             NOT IN PURGE LIST, KEEP REC                  
         CLC   0(2,R4),0(R7)                                                    
         BE    DMXPURGE            PURGE FOR THIS REP                           
         LA    R4,L'PRGLST(R4)                                                  
         B     DMXREC3                                                          
*                                                                               
*DMXREC3A DS    0H                                                              
*         CLI   0(R3),12            IS THIS A CONTRACT RECORD?                  
*         BNE   DMXKEEP             NO, LEAVE IT ALONE                          
*                                                                               
* SEE IF REP IS IN CHANGE LIST                                                  
*         LA    R5,CNGLST                                                       
*DMXREC4  CLI   0(R5),X'FF'         TEST FOR EOT                                
*         BE    DMXKEEP             NOT IN CHANGE LIST, LEAVE ALONE             
*         CLC   0(2,R5),0(R7)                                                   
*         BE    DMXREC5             HIT                                         
*         LA    R5,L'CNGLST(R5)                                                 
*         B     DMXREC4                                                         
*                                                                               
* SEE IF CONTRACT IS TYPE 'N' - IF SO, CHANGE TO 'C'                            
*DMXREC5  DS    0H                                                              
*         USING RCONREC,R3                                                      
*         CLI   RCONTYPE,C'N'                                                   
*         BNE   DMXKEEP                                                         
*         MVI   RCONTYPE,C'C'                                                   
*         AP    CHANGE,=P'1'                                                    
*         B     DMXKEEP                                                         
*         DROP  R3                                                              
*         SPACE 4                                                               
* CODE TO CHANGE FROM ONE REP TO ANOTHER, BASED ON CNGLST TABLE                 
*                                                                               
*MXREC2  ZIC   R7,9(R5)            DISP TO REPCODE                              
*        AR    R7,R3                                                            
*        LA    R5,CNGLST                                                        
*MXREC3  CLI   0(R5),X'FF'         TEST FOR EOT                                 
*        BE    DMXREC9                                                          
*        CLC   0(2,R5),0(R7)                                                    
*        BNE   DMXREC4                                                          
*        MVC   0(2,R7),2(R5)                                                    
*        B     DMXREC9                                                          
*MXREC4  LA    R5,L'CNGLST(R5)                                                  
*        B     DMXREC3                                                          
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORDS                                                 *             
*******************************************************************             
         SPACE 2                                                                
*DMXREC9  DS    0H                                                              
*         CLI   0(R3),12                                                        
*         BNE   DMXKEEP                                                         
*         USING RCONREC,R3                                                      
*         CLC   RCONKREP,=C'TO'     MATCH ON REP                                
*         BE    DMR10                                                           
*         CLC   RCONKREP,=C'I1'     MATCH ON REP                                
*         BE    DMR10                                                           
*         CLC   RCONKREP,=C'I2'     MATCH ON REP                                
*         BE    DMR10                                                           
*         CLC   RCONKREP,=C'DI'     MATCH ON REP                                
*         BE    DMR10                                                           
*         CLC   RCONKREP,=C'GP'     MATCH ON REP                                
*         BE    DMR10                                                           
*         CLC   RCONKREP,=C'HN'     MATCH ON REP                                
*         BE    DMR10                                                           
*         CLC   RCONKREP,=C'MG'     MATCH ON REP                                
*         BNE   DMXKEEP                                                         
*DMR10    DS    0H                                                              
*         CLI   RCONTYPE,C' '                                                   
*         BE    DMR20                                                           
*         CLI   RCONTYPE,X'00'                                                  
*         BNE   DMXKEEP                                                         
*DMR20    DS    0H                                                              
*         MVI   RCONTYPE,C'S'                                                   
*         B     DMXKEEP                                                         
*         BE    CONTRACT                                                        
**       CLI   0(R3),2                                                          
**       BE    STATION                                                          
*         B     DMXKEEP                                                         
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
* PROCESS CONTRACT RECORDS                                        *             
*                                                                 *             
*    ACE CONTRACT - RCONMODR+1=X'80'                              *             
*    GRAPHNET CONTRACT - RCONMODR+1=X'40'                         *             
*    NEITHER ACE OR GRAPHNET CONTRACT - RCONMODR+1=X'00'          *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
*         USING RECD,R3                                                         
         SPACE 1                                                                
*      FIX TO CHANGE GRAPHNET CONTRACTS TO ACE                                  
         SPACE 1                                                                
*CONTRACT EQU   *                                                               
**       CLC   RCONKREP,=C'BL'     MATCH ON REP                                 
**       BNE   CON100                                                           
**       CLC   RCONKSTA,=C'WNAC '  MATCH ON STATION                             
**       BNE   CON20                                                            
**       TM    RCONMODR+1,X'40'    IF TWX                                       
**       BZ    DMXKEEP                                                          
**       MVI   RCONMODR+1,X'80'    CHANGE TO ACE                                
**       B     DMXKEEP                                                          
*                                                                               
*CON20    EQU   *                                                               
**       CLC   RCONKSTA,=C'KGET '  MATCH ON STATION                             
**       BNE   DMXKEEP                                                          
**       TM    RCONMODR+1,X'40'    IF TWX                                       
**       BZ    DMXKEEP                                                          
**       MVI   RCONMODR+1,X'80'    CHANGE TO ACE                                
**       B     DMXKEEP                                                          
*                                                                               
*CON100   EQU   *                                                               
**       CLC   RCONKREP,=C'UV'     MATCH ON REP                                 
**       BNE   CON200                                                           
**       CLC   RCONKSTA,=C'KUVN '  MATCH ON STATION                             
**       BNE   DMXKEEP                                                          
**       TM    RCONMODR+1,X'40'    IF TWX                                       
**       BZ    DMXKEEP                                                          
**       MVI   RCONMODR+1,X'80'    CHANGE TO ACE                                
**       B     DMXKEEP                                                          
*                                                                               
*CON200   EQU   *                                                               
*         B     DMXKEEP                                                         
         EJECT                                                                  
*******************************************************************             
* PROCESS STATION RECORDS                                         *             
*                                                                 *             
*  RECLAIM RSTAOPT4 - SET OPTION TO 'N' SO CAN BE USED TO LATER   *             
*  IDENTIFY EASI STATION                                          *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
*         USING RECD,R3                                                         
         SPACE 1                                                                
*STATION  EQU   *                                                               
*                                                                               
**       L     R5,AREC                                                          
**       MVI   ELCODE,X'08'                                                     
**       BAS   RE,GETEL                                                         
**       BNE   DMXKEEP                                                          
**       USING RSTAXXEL,R5                                                      
**       MVI   RSTAOPT4,C'N'                                                    
*         B     DMXKEEP                                                         
**       DROP  R5                                                               
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
* TABLE OF REP CODES TO PURGE                                                   
*                                                                               
PRGLST   DS    0CL2                                                             
         DC    C'XF'                                                            
         DC    C'X6'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF REP CODES TO CHANGE                                                  
*                                                                               
*CNGLST   DS    0CL2                                                            
*         DC    C'B1'                                                           
*         DC    X'FF'                                                           
*         SPACE 2                                                               
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
**PAN#1  DC    CL21'220RELDEXT08 05/01/02'                                      
         END                                                                    
