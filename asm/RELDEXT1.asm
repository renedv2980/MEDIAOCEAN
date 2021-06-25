*          DATA SET RELDEXT1   AT LEVEL 006 AS OF 05/01/02                      
*PHASE RELDEXT,+0                                                               
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
*        PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 40,DMLDEXT,RR=R5                                                 
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
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
         B     DMXIT                                                            
         SPACE 2                                                                
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
*        TM    29(R3),X'01'                                                     
*        BO    DMXPURGE                                                         
*        B     DMXREC9                                                          
         LA    R5,CODES                                                         
         SPACE 1                                                                
DMXREC1  CLC   0(1,R3),8(R5)       RECORD TYPE TO TABLE                         
         BE    DMXREC2                                                          
         LA    R5,10(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BNE   DMXREC1                                                          
         B     DMXREC9                                                          
         SPACE 1                                                                
DMXREC2  ZIC   R7,9(R5)            DISP TO REPCODE                              
         AR    R7,R3                                                            
         LA    R5,PRGLST                                                        
DMXREC3  CLC   0(2,R5),0(R7)                                                    
         BNE   *+10                                                             
         MVC   0(2,R7),=C'TO'      CHANGE TL TO TO                              
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   DMXREC3                                                          
         SPACE 1                                                                
DMXREC9  CLI   0(R3),12                                                         
         BE    CONTRACT                                                         
         CLI   0(R3),2                                                          
         BE    STATION                                                          
         CLI   0(R3),18                                                         
         BE    INVNTRY                                                          
         CLI   0(R3),X'13'                                                      
         BE    BUDGET                                                           
         B     DMXKEEP                                                          
         SPACE 1                                                                
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*              CONTRACT RECORDS                                                 
         USING RECD,R3                                                          
         USING FIXD,R4                                                          
CONTRACT LA    R4,FIXT                                                          
CON1     CLI   0(R4),X'FF'                                                      
         BE    DMXKEEP                                                          
         SPACE 1                                                                
         CLC   RCONKREP,FREP       FILTER ON REP                                
         BNE   CON2                                                             
         CLC   FSTA,SPACES                                                      
         BE    *+14                                                             
         CLC   RCONKSTA,FSTA       STATION                                      
         BNE   CON2                                                             
         CLC   FOFF,SPACES                                                      
         BE    *+14                                                             
         CLC   RCONKOFF,FOFF       OFFICE                                       
         BNE   CON2                                                             
         CLC   FGRP,SPACES                                                      
         BE    *+14                                                             
         CLC   RCONKGRP,FGRP       GROUP                                        
         BNE   CON2                                                             
         CLC   FTEAM,SPACES                                                     
         BE    *+14                                                             
         CLC   RCONTEM,FTEAM       TEAM                                         
         BNE   CON2                                                             
         CLC   FMAN,SPACES                                                      
         BE    *+14                                                             
         CLC   RCONSAL,FMAN        SALESMAN                                     
         BNE   CON2                                                             
         SPACE 1                                                                
         LA    R5,CONCNT                                                        
         CLI   CONCHA,12                                                        
         BE    CON3                                                             
         SPACE 1                                                                
CON2     LA    R4,L'FIXT(R4)                                                    
         B     CON1                                                             
         SPACE 1                                                                
CON3     BAS   RE,DMPGET           DUMP                                         
         CLI   ACTION,C'P'                                                      
         BE    DMXPURGE                                                         
         CLC   NSTA,SPACES                                                      
         BE    *+10                NOT A REP STATION CHANGE                     
         MVC   RCONKSTA,NSTA                                                    
         CLC   NOFF,SPACES                                                      
         BE    *+10                                                             
         MVC   RCONKOFF,NOFF                                                    
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RCONKGRP,NGRP                                                    
         CLC   NTEAM,SPACES                                                     
         BE    *+10                                                             
         MVC   RCONTEM,NTEAM                                                    
         CLC   NMAN,SPACES                                                      
         BE    *+10                                                             
         MVC   RCONSAL,NMAN                                                     
         SPACE 1                                                                
CON5     LA    R5,RCONELEM                                                      
         CLC   CSTA,SPACES                                                      
         BE    CON8                                                             
         MVI   ELCODE,6                                                         
         BAS   RE,NEXTEL                                                        
         BNE   CON8                                                             
         SPACE 1                                                                
         USING RCONSPEL,R5                                                      
         SR    R1,R1                                                            
         IC    R1,RCONSPNU         NUMBER OF MINI ELEMENTS                      
         LA    R6,RCONSPST                                                      
         SPACE 1                                                                
CON6     CLC   0(5,R6),CSTA        COMPETING STATION                            
         BNE   CON7                                                             
         CLC   NCSTA,SPACES                                                     
         BE    *+10                                                             
         MVC   0(5,R6),NCSTA       NEW COMPETING                                
         B     CON8                                                             
         SPACE 1                                                                
CON7     LA    R6,9(R6)                                                         
         BCT   R1,CON6                                                          
         SPACE 1                                                                
CON8     LA    R5,CONCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
*              STATION RECORDS                                                  
         SPACE 1                                                                
STATION  LA    R4,FIXT                                                          
STA1     CLI   0(R4),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   RSTAKREP,FREP       FILTER REP                                   
         BNE   STA2                                                             
         CLC   RSTAKSTA,FSTA       STATION                                      
         BNE   STA2                                                             
         LA    R5,STACNT                                                        
         CLI   STACHA,2                                                         
         BE    STA3                                                             
         SPACE 1                                                                
STA2     LA    R4,L'FIXT(R4)                                                    
         B     STA1                                                             
         SPACE 1                                                                
STA3     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    DMXPURGE                                                         
         CLC   NSTA,SPACES                                                      
         BE    *+10                NOT A REP STATION CHANGE                     
         MVC   RSTAKSTA,NSTA                                                    
         CLC   NGRP,SPACES                                                      
         BE    *+10                                                             
         MVC   RSTAGRUP,NGRP                                                    
         CLC   NAFF,SPACES                                                      
         BE    *+10                                                             
         MVC   RSTAAFFL,NAFF                                                    
         SPACE 1                                                                
STA5     LA    R5,RSTAELEM                                                      
         CLC   CSTA,SPACES                                                      
         BE    STA9                                                             
         MVI   ELCODE,2                                                         
         BAS   RE,NEXTEL                                                        
         BNE   STA9                                                             
         SPACE 1                                                                
         USING RSTAMKEL,R5                                                      
         CLC   RSTAMKST,CSTA                                                    
         BNE   *-14                                                             
         CLC   NCSTA,SPACES                                                     
         BE    *+10                                                             
         MVC   RSTAMKST,NCSTA                                                   
         CLC   NCAFF,SPACES                                                     
         BE    *+10                                                             
         MVC   RSTAMKAF,NCAFF                                                   
         SPACE 1                                                                
STA9     LA    R5,STACNT                                                        
         BAS   RE,DMPPUT                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
*              INVENTORY RECORDS                                                
         SPACE 1                                                                
INVNTRY  LA    R4,FIXT                                                          
INV1     CLI   0(R4),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   RINVKREP,FREP                                                    
         BNE   INV2                                                             
         CLC   RINVKSTA(4),FSTA                                                 
         BNE   INV2                                                             
         LA    R5,INVCNT                                                        
         CLI   INVCHA,18                                                        
         BE    INV4                                                             
         SPACE 1                                                                
INV2     LA    R4,L'FIXT(R4)                                                    
         B     INV1                                                             
         SPACE 1                                                                
INV4     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    DMXPURGE                                                         
         CLC   NSTA,SPACES                                                      
         BE    INV9                                                             
         MVC   RINVKSTA(4),NSTA                                                 
         SPACE 1                                                                
INV9     LA    R5,INVCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
*              BUDGET RECORDS                                                   
         SPACE 1                                                                
BUDGET   LA    R4,FIXT                                                          
BUD1     CLI   0(R4),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   RBUDKREP,FREP       FILTER REP                                   
         BNE   BUD2                                                             
         CLC   RBUDKSTA,FSTA       STATION                                      
         BNE   BUD2                                                             
         LA    R5,BUDCNT                                                        
         CLI   BUDCHA,19                                                        
         BE    BUD3                                                             
         SPACE 1                                                                
BUD2     LA    R4,L'FIXT(R4)                                                    
         B     BUD1                                                             
         SPACE 1                                                                
BUD3     BAS   RE,DMPGET                                                        
         CLI   ACTION,C'P'                                                      
         BE    DMXPURGE                                                         
         CLC   NSTA,SPACES                                                      
         BE    BUD9                NOT A REP STATION CHANGE                     
         MVC   RBUDKSTA,NSTA                                                    
         SPACE 1                                                                
BUD9     LA    R5,BUDCNT                                                        
         BAS   RE,DMPPUT                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
*              END OF FILE                                                      
         SPACE 1                                                                
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+10(29),=C'STATION     COMPETING STATION'                       
         MVC   P+46(15),=C'CHANGED RECORDS'                                     
         GOTO1 VPRINTER                                                         
         MVC   P+3(34),=C'REP  OLD    NEW     OLD        NEW'                   
         MVC   P+41(36),=C'STATION  CONTRACT  INVENTORY  BUDGET'                
         GOTO1 VPRINTER                                                         
         MVC   P+3(34),=C'---  -----  -----  -----     -----'                   
         MVC   P+41(36),=C'-------  --------  ---------  ------'                
         GOTO1 VPRINTER                                                         
         LA    R4,FIXT                                                          
         SPACE 1                                                                
DMCNT1   CLI   0(R4),X'FF'                                                      
         BE    DMCNT2                                                           
         MVC   P+3(2),FREP                                                      
         MVC   P+8(5),FSTA                                                      
         MVC   P+15(5),NSTA                                                     
         MVC   P+22(5),CSTA                                                     
         MVC   P+33(5),NCSTA                                                    
         EDIT  (P4,STACNT),(7,P+41)                                             
         EDIT  (P4,CONCNT),(7,P+50)                                             
         EDIT  (P4,INVCNT),(7,P+60)                                             
         EDIT  (P4,BUDCNT),(6,P+71)                                             
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,L'FIXT(R4)                                                    
         B     DMCNT1                                                           
*                                                                               
DMCNT2   GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 1                                                                
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
         EJECT                                                                  
DMPGET   NTR1                                                                   
         AP    0(4,R5),=P'1'                                                    
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMXIT                                                            
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         ZAP   DUB,0(4,R5)                                                      
         DP    DUB,4(4,R5)                                                      
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMXIT                                                            
         LA    R6,=C'PUT'                                                       
         SPACE 1                                                                
DUMP     MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CODES    DC    CL8'REP     ',AL1(01),AL1(RREPKREP-RREPREC)                      
         DC    CL8'REGION  ',AL1(03),AL1(RREGKREP-RREGREC)                      
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
         DC    CL8'BUDGET  ',AL1(19),AL1(RBUDKREP-RBUDREC)                      
         DC    CL8'EOM     ',AL1(24),AL1(REOMKREP-REOMREC)                      
*        DC    CL8'ERROR   ',AL1(14),AL1(1)                                     
*        DC    CL8'REPORT  ',AL1(112),AL1(1)                                    
         DC    X'FF'                                                            
         EJECT                                                                  
PRGLST   DC    C'TL'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
FIXT     DC    X'FF'                                                            
*IXT     DS    0CL89                                                            
         DC    C'BL'               REP                                          
         DC    C'WDMA '            STATION                                      
         DC    C'  '               OFFICE                                       
         DC    C'  '               GROUP                                        
         DC    C'  '               DIV                                          
         DC    C'   '              MAN                                          
         SPACE 1                                                                
         DC    C'WUPW '            STATION                                      
         DC    C'  '               OFFICE                                       
         DC    C'  '               GROUP                                        
         DC    C'  '               DIV                                          
         DC    C'   '              MAN                                          
         DC    C'   '              AFF                                          
         SPACE 1                                                                
         DC    C'WDMA '            COMP. STA                                    
         DC    C'WUPW '            NEW COMP. STA                                
         DC    C'   '              NEW AFF                                      
         SPACE 1                                                                
         DC    X'02'                                                            
         DC    PL4'0'                                                           
         DC    PL4'1'                                                           
         DC    X'0C'                                                            
         DC    PL4'0'                                                           
         DC    PL4'100'                                                         
         DC    X'12'                                                            
         DC    PL4'0'                                                           
         DC    PL4'200'                                                         
         DC    X'13'                                                            
         DC    PL4'0'                                                           
         DC    PL4'1'                                                           
         DC    C'C'                                                             
         DS    CL6                                                              
         DC    X'FF'                                                            
         EJECT                                                                  
FIXD     DSECT                     DSECT FOR CHANGE LIST                        
FILTER   DS    0C                  FILTER FIELDS                                
FREP     DS    CL2       1         REP CODE                                     
FSTA     DS    CL5       3         STATION                                      
FOFF     DS    CL2       8         OFFICE                                       
FGRP     DS    CL2                 GROUP/SUB-GROUP                              
FTEAM    DS    CL2                 DIVISION/TEAM                                
FMAN     DS    CL3                 SALESMAN                                     
         SPACE 1                                                                
NSTA     DS    CL5                 NEW STATION                                  
NOFF     DS    CL2                 NEW OFFICE                                   
NGRP     DS    CL2                 NEW GROUP                                    
NTEAM    DS    CL2                 NEW TEAM                                     
NMAN     DS    CL3                 NEW MAN                                      
NAFF     DS    CL3                 NEW AFFILIATION                              
         SPACE 1                                                                
CSTA     DS    CL5                 COMPETING STATION                            
NCSTA    DS    CL5                 NEW COMPETING STATION                        
NCAFF    DS    CL3                 NEW COMPETING AFFILIATION                    
         SPACE 1                                                                
STACHA   DS    CL1                 02 = CHANGE STATION RECORD                   
STACNT   DS    PL4                 COUNT STATION RECORDS CHANGED                
         DS    PL4                 DUMP EVERY NTH RECORD                        
CONCHA   DS    CL1                 0C = CHANGE CONTRACT RECORD                  
CONCNT   DS    PL4                 COUNT CONTRACTS CHANGED                      
         DS    PL4                 DUMP EVERY NTH RECORD                        
INVCHA   DS    CL1                 12 = CHANGE INVENTORY RECORD                 
INVCNT   DS    PL4                 COUNT INVENTORY CHANGED                      
         DS    PL4                 DUMP EVERY NTH RECORD                        
BUDCHA   DS    CL1                 13 = CHANGE BUDGET RECORD                    
BUDCNT   DS    PL4                 COUNT BUDGET CHANGED                         
         DS    PL4                 DUMP EVERY NTH RECORD                        
ACTION   DS    C                   P=PURGE,  C=CHANGE                           
         EJECT                                                                  
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
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006RELDEXT1  05/01/02'                                      
         END                                                                    
