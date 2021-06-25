*          DATA SET RELDMPI    AT LEVEL 153 AS OF 05/01/02                      
*PHASE RELDMPI,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'RELDMPI - MPI ASOF DATE FILE FIX'                               
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
         CLI   0(R3),12                                                         
         BE    CONTRACT                                                         
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
CONTRACT LA    R4,FIXT                                                          
         USING FIXD,R4                                                          
         CLC   RCONKREP,=C'BL'     FILTER ON REP                                
         BNE   DMXKEEP                                                          
         CLI   RCONKGRP,C'R'       RADIO                                        
         BNE   MPI5                                                             
         SPACE 1                                                                
* MPI RADIO - CONTRACT START DATE IS BTWN JAN30/84 & FEB26/84                   
         CLC   RCONDATE(3),=X'54011E'                                           
         BL    DMXKEEP                                                          
         CLC   RCONDATE(3),=X'54021A'                                           
         BH    DMXKEEP                                                          
         SPACE 1                                                                
         LA    R5,CONCNT                                                        
         BAS   RE,DMPGET                                                        
         SPACE 1                                                                
*  FOR RADIO, USE 7 DAYS BEFORE CONTRACT START DATE FOR AS OF DATE              
*                                                                               
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE),(0,WORK)   TO YYMMDD                
         LA    R2,7                                                             
         LCR   R2,R2               MAKE IT NEG.                                 
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,(R2)         SUBTRACT 7               
         SPACE 1                                                                
         GOTO1 =V(DATCON),DMCB,(0,WORK+6),(2,ASOF)     TO COMPRESSED            
         B     MPI23                                                            
         SPACE 1                                                                
* MPI TV - CONTRACT NUMBERS BTWN 01500000 & 01600000                            
MPI5     CLC   RCONKCON,=X'01500000' IS IT MPI CONTRACT                         
         BL    DMXKEEP                                                          
         CLC   RCONKCON,=X'01600000'                                            
         BNL   DMXKEEP                                                          
         SPACE 1                                                                
         LA    R5,CONCNT                                                        
         BAS   RE,DMPGET                                                        
         SPACE 1                                                                
         SR    R0,R0                                                            
         LA    R1,RCONELEM                                                      
MPI10    CLI   0(R1),0             FIND X'1F' ELEMENT                           
         BNE   *+12                                                             
         BAS   RE,NOASOF           PRINT RECORD IF NO X'1F'                     
         B     DMXKEEP                                                          
         CLI   0(R1),X'1F'                                                      
         BE    MPI20                                                            
         SPACE 1                                                                
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MPI10                                                            
         SPACE 1                                                                
MPI20    OC    4(2,R1),4(R1)                                                    
         BNZ   *+12                                                             
         BAS   RE,NOASOF           PRINT RECORD IF NO AS OF DATE                
         B     DMXKEEP                                                          
         SPACE 1                                                                
         MVC   ASOF,4(R1)          SAVE AS OF DATE                              
         SPACE 1                                                                
MPI23    SR    R0,R0                                                            
         LA    R1,RCONELEM                                                      
MPI25    CLI   0(R1),0             FIND X'03' ELEMENTS                          
         BE    MPI40                                                            
         CLI   0(R1),X'03'                                                      
         BE    MPI30                                                            
         SPACE 1                                                                
MPI27    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MPI25                                                            
         SPACE 1                                                                
MPI30    MVC   4(2,R1),ASOF        PUT AS OF INTO BUCKET                        
         B     MPI27                                                            
         SPACE 1                                                                
MPI40    LA    R5,CONCNT                                                        
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
         MVC   P+41(28),=C'STATION  CONTRACT  INVENTORY'                        
         GOTO1 VPRINTER                                                         
         MVC   P+3(34),=C'---  -----  -----  -----     -----'                   
         MVC   P+41(28),=C'-------  --------  ---------'                        
         GOTO1 VPRINTER                                                         
         LA    R4,FIXT                                                          
         SPACE 1                                                                
DMCNT1   MVC   P+3(2),FREP                                                      
         MVC   P+8(5),FSTA                                                      
         MVC   P+15(5),NSTA                                                     
         MVC   P+22(5),CSTA                                                     
         MVC   P+33(5),NCSTA                                                    
         EDIT  (P4,STACNT),(7,P+41)                                             
         EDIT  (P4,CONCNT),(7,P+50)                                             
         EDIT  (P4,INVCNT),(7,P+60)                                             
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,L'FIXT(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   DMCNT1                                                           
         GOTO1 VPRINTER                                                         
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
         SPACE 1                                                                
NOASOF   NTR1                                                                   
         MVC   HALF,27(R3)                                                      
         LH    R8,HALF                                                          
         LA    R6,=C'NO ASOF'                                                   
         GOTO1 PRNTBL,DMCB,(7,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ASOF     DS    H                   AS OF DATE                                   
FIXT     DS    0CL80                                                            
         DC    C'BL'               REP                                          
         DC    C'     '            STATION                                      
         DC    C'  '               OFFICE                                       
         DC    C'  '               GROUP                                        
         DC    C'  '               DIV                                          
         DC    C'   '              MAN                                          
         SPACE 1                                                                
         DC    C'     '            STATION                                      
         DC    C'  '               OFFICE                                       
         DC    C'  '               GROUP                                        
         DC    C'  '               DIV                                          
         DC    C'   '              MAN                                          
         DC    C'   '              AFF                                          
         SPACE 1                                                                
         DC    C'     '            COMP. STA                                    
         DC    C'     '            NEW COMP. STA                                
         DC    C'   '              NEW AFF                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         DC    PL4'0'                                                           
         DC    PL4'1'                                                           
         DC    X'0C'                                                            
         DC    PL4'0'                                                           
         DC    PL4'100'                                                         
         DC    X'00'                                                            
         DC    PL4'0'                                                           
         DC    PL4'200'                                                         
         DC    C'C'                                                             
         DS    CL6                                                              
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
         DS    0CL80                                                            
         DC    C'BL'               REP                                          
         DC    C'ANSNA'            STATION                                      
         DC    C'  '               OFFICE                                       
         DC    C'  '               GROUP                                        
         DC    C'  '               DIV                                          
         DC    C'   '              MAN                                          
         SPACE 1                                                                
         DC    C'     '            STATION                                      
         DC    C'  '               OFFICE                                       
         DC    C'RX'               GROUP                                        
         DC    C'  '               DIV                                          
         DC    C'   '              MAN                                          
         DC    C'   '              AFF                                          
         SPACE 1                                                                
         DC    C'     '            COMP. STA                                    
         DC    C'     '            NEW COMP. STA                                
         DC    C'   '              NEW AFF                                      
         SPACE 1                                                                
         DC    X'02'                                                            
         DC    PL4'0'                                                           
         DC    PL4'1'                                                           
         DC    X'0C'                                                            
         DC    PL4'0'                                                           
         DC    PL4'100'                                                         
         DC    X'00'                                                            
         DC    PL4'0'                                                           
         DC    PL4'200'                                                         
         DC    C'C'                                                             
         DS    CL6                                                              
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
**PAN#1  DC    CL21'153RELDMPI   05/01/02'                                      
         END                                                                    
