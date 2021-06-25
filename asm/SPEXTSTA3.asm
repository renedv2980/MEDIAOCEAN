*          DATA SET SPEXTSTA3  AT LEVEL 003 AS OF 06/24/92                      
*          DATA SET SPEXTSTA1  AT LEVEL 046 AS OF 06/24/92                      
*PHASE SPEXTSTT,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE MSPACK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
*                                                                               
*     N O T E ------------- NEXT TIME THIS IS USED                              
*     FIX BUGS AS FOLLOWS                                                       
*     1. WHEN ACCESSING PACKAGE ELEMS (05) IN BUYS, FIX SUBLINE THERE           
*         IF UPDATING SUBLINE IN BUY REC IN FIXS RTN                            
*     2. IF WOOD IS FOUND IN NWS HDR REC, MUST FIX DETAILS IF THERE             
*         WAS ALSO A WOTV                                                       
*                                                                               
* JUN 26,1992                                                                   
* CHANGE STATION CALL LETTERS FROM WOTV TO WOOD                                 
*                             AND  WUHQ TO WOTV                                 
*                                                                               
* FOR WUNDERMAN ONLY                                                            
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
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT,R8,R9,R2                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
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
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WOTVT',WORK                          
         CLC   WOTVP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WOODT',WORK                          
         CLC   WOODP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WUHQT',WORK                          
         CLC   WUHQP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         CLC   =X'0E15',0(R3)      WBS CELL?                                    
         BE    WBSC                 YES.                                        
         SPACE                                                                  
         CLC   =X'0E18',0(R3)      WBS LOG?                                     
         BE    WBSL                 YES.                                        
         SPACE                                                                  
         CLC   =X'0E19',0(R3)      WBS INVOICE?                                 
         BE    WBSI                 YES.                                        
         SPACE                                                                  
         CLC   =X'0E1A',0(R3)      WBS ORDER?                                   
         BE    WBSO                 YES.                                        
         SPACE                                                                  
         B     DMXKEEP                                                          
         EJECT                                                                  
* WBS CELL           0E15 *                                                     
         SPACE                                                                  
WBSC     AP    TWBSCCT,=P'1'                                                    
         LA    R4,24(,R3)                                                       
         SR    R5,R5                                                            
WBSC10   CLI   0(R4),X'05'                                                      
         BE    WBSC20                                                           
WBSC14   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   WBSC10               NO                                          
         B     WBSC30                                                           
         SPACE                                                                  
WBSC20   CLC   WOODP,4(R4)         SHOULD NOT BE ANY                            
         BNE   WBSC24                                                           
         AP    TWOOD,=P'1'                                                      
         AP    WBSCCTC,=P'1'                                                    
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'WBS-CEL-WOOD IN REC'                                    
         B     PRT                                                              
WBSC24   CLC   WOTVP,6(R4)                                                      
         BNE   WBSC26                                                           
         MVC   6(3,R4),WOODP                                                    
         AP    WBSCCTA,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSC14                                                           
WBSC26   CLC   WUHQP,6(R4)                                                      
         BNE   DMXKEEP                                                          
         MVC   6(3,R4),WOTVP                                                    
         AP    WBSCCTB,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSC14                                                           
WBSC30   LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS CELL CHGE'                                          
         B     PRT                                                              
         EJECT                                                                  
* WBS LOG            0E18 *                                                     
         SPACE                                                                  
WBSL     AP    TWBSLCT,=P'1'                                                    
         LA    R4,24(,R3)                                                       
         SR    R5,R5                                                            
WBSL10   CLI   0(R4),X'80'         PASSIVE POINTER ELEM                         
         BE    WBSL20                                                           
WBSL14   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   WBSL10               NO                                          
         B     WBSL30                                                           
         SPACE                                                                  
WBSL20   CLC   WOODP,9(R4)         SHOULD NOT BE ANY                            
         BNE   WBSL24                                                           
         AP    TWOOD,=P'1'                                                      
         AP    WBSLCTC,=P'1'                                                    
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'WBS-CEL-WOOD IN REC'                                    
         B     PRT                                                              
WBSL24   CLC   WOTVP,9(R4)                                                      
         BNE   WBSL26                                                           
         MVC   9(3,R4),WOODP                                                    
         AP    WBSLCTA,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSL14                                                           
WBSL26   CLC   WUHQP,9(R4)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R4),WOTVP                                                    
         AP    WBSLCTB,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSL14                                                           
WBSL30   LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS LOG CHGE'                                           
         B     PRT                                                              
         EJECT                                                                  
* WBS INVOICE        0E19 *                                                     
         SPACE                                                                  
WBSI     AP    TWBSICT,=P'1'                                                    
         LA    R4,24(,R3)                                                       
         SR    R5,R5                                                            
WBSI10   CLI   0(R4),X'80'         PASSIVE POINTER ELEM                         
         BE    WBSI20                                                           
WBSI14   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   WBSI10               NO                                          
         B     WBSI30                                                           
         SPACE                                                                  
WBSI20   CLC   WOODP,9(R4)         SHOULD NOT BE ANY                            
         BNE   WBSI24                                                           
         AP    TWOOD,=P'1'                                                      
         AP    WBSICTC,=P'1'                                                    
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'WBS-CEL-WOOD IN REC'                                    
         B     PRT                                                              
WBSI24   CLC   WOTVP,9(R4)                                                      
         BNE   WBSI26                                                           
         MVC   9(3,R4),WOODP                                                    
         AP    WBSICTA,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSI14                                                           
WBSI26   CLC   WUHQP,9(R4)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R4),WOTVP                                                    
         AP    WBSICTB,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSI14                                                           
WBSI30   LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS INVOICE CHGE'                                       
         B     PRT                                                              
         EJECT                                                                  
* WBS ORDER          0E1A *                                                     
         SPACE                                                                  
WBSO     AP    TWBSOCT,=P'1'                                                    
         LA    R4,24(,R3)                                                       
         SR    R5,R5                                                            
WBSO10   CLI   0(R4),X'80'         PASSIVE POINTER ELEM                         
         BE    WBSO20                                                           
WBSO14   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   WBSO10               NO                                          
         B     WBSO30                                                           
         SPACE                                                                  
WBSO20   CLC   WOODP,9(R4)         SHOULD NOT BE ANY                            
         BNE   WBSO24                                                           
         AP    TWOOD,=P'1'                                                      
         AP    WBSOCTC,=P'1'                                                    
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'WBS-CEL-WOOD IN REC'                                    
         B     PRT                                                              
WBSO24   CLC   WOTVP,9(R4)                                                      
         BNE   WBSO26                                                           
         MVC   9(3,R4),WOODP                                                    
         AP    WBSOCTA,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSO14                                                           
WBSO26   CLC   WUHQP,9(R4)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R4),WOTVP                                                    
         AP    WBSOCTB,=P'1'                                                    
         BCTR  R5,0                                                             
         B     WBSO14                                                           
WBSO30   LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS ORDER CHGE'                                         
         B     PRT                                                              
         EJECT                                                                  
PRT      SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE 3                                                                
GETEL    LA    R6,24(,R6)                                                       
FIRSTEL  CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL   SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL                                                          
         EJECT                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         LA    R5,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(22),4(R3)                                                    
         EDIT  (P4,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,26(,R3)                                                       
         BCT   R5,DMXEOF10                                                      
         GOTO1 VPRINTER                                                         
         LA    R5,TOTKCTRS                                                      
         LA    R3,TWOOD                                                         
         SPACE                                                                  
DMXEOF20 MVC   P+5(22),4(R3)                                                    
         EDIT  (P4,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,26(,R3)                                                       
         BCT   R5,DMXEOF20                                                      
         B     DMXIT                                                            
         EJECT                                                                  
STBAMCLP DC    XL4'00'                                                          
LBLLMSG  DC    CL20'LBL LIST XXXX YYYY'                                         
STALMSG  DC    CL20'STA LIST '                                                  
PATLMSG  DC    CL20'PATTERN '                                                   
WOODT    DC    CL4'WOOD'                                                        
WOODP    DC    XL3'C90D22'                                                      
WUHQT    DC    CL4'WUHQ'                                                        
WUHQP    DC    XL3'CB05A2'                                                      
WOTVT    DC    CL4'WOTV'                                                        
WOTVP    DC    XL3'C92042'                                                      
TOTRD    DC    PL4'0',CL22'TOTAL RECS READ'                                     
TBUYCT   DC    PL4'0',CL22'TOT BUYS'                                            
BUYCTA   DC    PL4'0',CL22'BUYS STA WOTV'                                       
BUYCTB   DC    PL4'0',CL22'BUYS STA WUHQ'                                       
BUYCTC   DC    PL4'0',CL22'BUYS STA WOOD'                                       
BUYCTKC  DC    PL4'0',CL22'BUYS STA WOTV SUB CHA'                               
TINVCT   DC    PL4'0',CL22'TOT INV         0B  '                                
INVCTA   DC    PL4'0',CL22'TOT INVS WOTV'                                       
INVCTB   DC    PL4'0',CL22'TOT INVS WUHQ'                                       
INVCTC   DC    PL4'0',CL22'TOT INVS WOOD'                                       
TSIRCT   DC    PL4'0',CL22'TOT SIR         0C  '                                
SIRCTA   DC    PL4'0',CL22'TOT SIRS WOTV'                                       
SIRCTB   DC    PL4'0',CL22'TOT SIRS WUHQ'                                       
SIRCTC   DC    PL4'0',CL22'TOT SIRS WOOD'                                       
TSGRCT   DC    PL4'0',CL22'TOT STA GRP     0D05'                                
SGRCTA   DC    PL4'0',CL22'TOT STA GRP WOTV'                                    
SGRCTB   DC    PL4'0',CL22'TOT STA GRP WUHQ'                                    
SGRCTC   DC    PL4'0',CL22'TOT STA GRP WOOD'                                    
TFLTCT   DC    PL4'0',CL22'TOT FLT         0D0D'                                
FLTCTA   DC    PL4'0',CL22'TOT FLTS WOTV'                                       
FLTCTB   DC    PL4'0',CL22'TOT FLTS WUHQ'                                       
FLTCTC   DC    PL4'0',CL22'TOT SPT FLT WOOD'                                    
TCSPCT   DC    PL4'0',CL22'TOT CSP         0D0D'                                
CSPCTA   DC    PL4'0',CL22'TOT CSPS WOTV'                                       
CSPCTB   DC    PL4'0',CL22'TOT CSPS WUHQ'                                       
CSPCTC   DC    PL4'0',CL22'TOT CAN SPL WOOD'                                    
TPCTCT   DC    PL4'0',CL22'TOT STA PCT     0D42'                                
PCTCTA   DC    PL4'0',CL22'TOT STA PCT WOTV'                                    
PCTCTB   DC    PL4'0',CL22'TOT STA PCT WUHQ'                                    
PCTCTC   DC    PL4'0',CL22'TOT STA PCT WOOD'                                    
TEQUCT   DC    PL4'0',CL22'TOT STA EQU     0D44'                                
EQUCTA   DC    PL4'0',CL22'TOT STA EQU WOTV'                                    
EQUCTB   DC    PL4'0',CL22'TOT STA EQU WUHQ'                                    
EQUCTC   DC    PL4'0',CL22'TOT STA EQU WOOD'                                    
TCSTCT   DC    PL4'0',CL22'TOT COMPET STA  0D47'                                
CSTCTA   DC    PL4'0',CL22'TOT CMP STA WOTV'                                    
CSTCTB   DC    PL4'0',CL22'TOT CMP STA WUHQ'                                    
CSTCTC   DC    PL4'0',CL22'TOT CMP STA WOOD'                                    
TCSOCT   DC    PL4'0',CL22'TOT CSO PROGRA  0D49'                                
CSOCTA   DC    PL4'0',CL22'TOT CSO PRG WOTV'                                    
CSOCTB   DC    PL4'0',CL22'TOT CMP PRG WUHQ'                                    
CSOCTC   DC    PL4'0',CL22'TOT CMP PRG WOOD'                                    
TCDOCT   DC    PL4'0',CL22'TOT CSO DEM OVR 0D57'                                
CDOCTA   DC    PL4'0',CL22'TOT CSO DEM WOTV'                                    
CDOCTB   DC    PL4'0',CL22'TOT CSO DEM WUHQ'                                    
CDOCTC   DC    PL4'0',CL22'TOT CMP DEM WOOD'                                    
TCSCCT   DC    PL4'0',CL22'TOT CSO COMMNTS 0D58'                                
CSCCTA   DC    PL4'0',CL22'TOT CSO CMT WOTV'                                    
CSCCTB   DC    PL4'0',CL22'TOT CMP CMT WUHQ'                                    
CSCCTC   DC    PL4'0',CL22'TOT CMP CMT WOOD'                                    
TSLSCT   DC    PL4'0',CL22'TOT STA LIST    0D5B'                                
SLSCTA   DC    PL4'0',CL22'TOT STA LST WOTV'                                    
SLSCTB   DC    PL4'0',CL22'TOT STA LST WUHQ'                                    
SLSCTC   DC    PL4'0',CL22'TOT STA LST WOOD'                                    
TNWSHCT  DC    PL4'0',CL22'TOT NWS HDR     0D67'                                
NWSHCTA  DC    PL4'0',CL22'TOT NWS HDR WOTV'                                    
NWSHCTB  DC    PL4'0',CL22'TOT NWS HDR WUHQ'                                    
NWSHCTC  DC    PL4'0',CL22'TOT NWS HDR WOOD'                                    
TNWSDCT  DC    PL4'0',CL22'TOT NWS DTL     0D68'                                
NWSDCTA  DC    PL4'0',CL22'TOT NWS DTL WOTV'                                    
NWSDCTB  DC    PL4'0',CL22'TOT NWS DTL WUHQ'                                    
NWSDCTC  DC    PL4'0',CL22'TOT NWS DTL WOOD'                                    
TPRDEXCT DC    PL4'0',CL22'TOT PRD EXCL    0D70'                                
PRDEXCTA DC    PL4'0',CL22'TOT PRD EXCL WOTV'                                   
PRDEXCTB DC    PL4'0',CL22'TOT PRD EXCL WUHQ'                                   
PRDEXCTC DC    PL4'0',CL22'TOT PRD EXCL WOOD'                                   
TSTATUCT DC    PL4'0',CL22'TOT STATUS      0D71'                                
STATUCTA DC    PL4'0',CL22'TOT STATUS   WOTV'                                   
STATUCTB DC    PL4'0',CL22'TOT STATUS   WUHQ'                                   
STATUCTC DC    PL4'0',CL22'TOT STATUS   WOOD'                                   
TSLHCT   DC    PL4'0',CL22'TOT STA LOC HDR 0D72'                                
SLHCTA   DC    PL4'0',CL22'TOT STA LOCK WOTV'                                   
SLHCTB   DC    PL4'0',CL22'TOT STA LOCK WUHQ'                                   
SLHCTC   DC    PL4'0',CL22'TOT STA LOCK WOOD'                                   
TSLTCT   DC    PL4'0',CL22'TOT STATION LST 0D75'                                
SLTCTA   DC    PL4'0',CL22'TOT STA LST  WOTV'                                   
SLTCTB   DC    PL4'0',CL22'TOT STA LST  WUHQ'                                   
SLTCTC   DC    PL4'0',CL22'TOT STA LST  WOOD'                                   
SLTCTD   DC    PL4'0',CL22'TOT STA LST  WOTV ELE'                               
SLTCTE   DC    PL4'0',CL22'TOT STA LST  WUHQ ELE'                               
SLTCTF   DC    PL4'0',CL22'TOT STA LST  WOOD ELE'                               
TCLSTACT DC    PL4'0',CL22'TOT CLEARED STA 0D76'                                
CLSTACTA DC    PL4'0',CL22'TOT CLST WOTV'                                       
CLSTACTB DC    PL4'0',CL22'TOT CLST WUHQ'                                       
CLSTACTC DC    PL4'0',CL22'TOT CLST WOOD'                                       
TBILLCT  DC    PL4'0',CL22'TOT BILLS       0E01'                                
BILLCTA  DC    PL4'0',CL22'STA BILLS WOTV'                                      
BILLCTM  DC    PL4'0',CL22'STA BILLS WOTV MERGED'                               
BILLCTB  DC    PL4'0',CL22'STA BILLS WUHQ'                                      
BILLCTC  DC    PL4'0',CL22'STA BILLS WOOD'                                      
TWBSRCT  DC    PL4'0',CL22'WBS REG         0E14'                                
WBSRCTA  DC    PL4'0',CL22'WBS REG   WOTV'                                      
WBSRCTB  DC    PL4'0',CL22'WBS REG   WUHQ'                                      
WBSRCTC  DC    PL4'0',CL22'WBS REG   WOOD'                                      
TWBSCCT  DC    PL4'0',CL22'WBS CELL       0E15'                                 
WBSCCTA  DC    PL4'0',CL22'WBS CELL WOTV'                                       
WBSCCTB  DC    PL4'0',CL22'WBS CELL WUHQ'                                       
WBSCCTC  DC    PL4'0',CL22'WBS CELL WOOD'                                       
TWBSGCT  DC    PL4'0',CL22'WBS GRADE ASS   0E16'                                
WBSGCTA  DC    PL4'0',CL22'WBS GRADE WOTV'                                      
WBSGCTB  DC    PL4'0',CL22'WBS GRADE WUHQ'                                      
WBSGCTC  DC    PL4'0',CL22'WBS GRADE WOOD'                                      
TWBSLCT  DC    PL4'0',CL22'WBS LOG       0E18'                                  
WBSLCTA  DC    PL4'0',CL22'WBS LOG WOTV'                                        
WBSLCTB  DC    PL4'0',CL22'WBS LOG WUHQ'                                        
WBSLCTC  DC    PL4'0',CL22'WBS LOG WOOD'                                        
TWBSICT  DC    PL4'0',CL22'WBS INVOICE    0E19'                                 
WBSICTA  DC    PL4'0',CL22'WBS INVOICE WOTV'                                    
WBSICTB  DC    PL4'0',CL22'WBS INVOICE WUHQ'                                    
WBSICTC  DC    PL4'0',CL22'WBS INVOICE WOOD'                                    
TWBSOCT  DC    PL4'0',CL22'WBS ORDER      0E1A'                                 
WBSOCTA  DC    PL4'0',CL22'WBS ORDER WOTV'                                      
WBSOCTB  DC    PL4'0',CL22'WBS ORDER WUHQ'                                      
WBSOCTC  DC    PL4'0',CL22'WBS ORDER WOOD'                                      
TESTCT   DC    PL4'0',CL22'TOT EST STA BK'                                      
ESTCTA   DC    PL4'0',CL22'TOT EST STA BK WOTV'                                 
ESTCTB   DC    PL4'0',CL22'TOT EST STA BK WUHQ'                                 
ESTCTC   DC    PL4'0',CL22'TOT EST STA BK WOOD'                                 
TOTCTRS  EQU   (*-TOTRD)/26                                                     
TWOOD    DC    PL4'0',CL22'TOT WOOD REC'                                        
TPSTABLS DC    PL4'0',CL22'STAT BILLS ON TAPE'                                  
TOTKCTRS EQU   (*-TWOOD)/26                                                     
WORK     DS    CL64                                                             
COMPKEY  DC    XL10'00'                                                         
ELCODE   DS    XL1                                                              
RTITLE   DC    CL60'CHANGE STATION CALL LETTERS WOTV TO WOOD AND WUHQ TC        
               O WOTV'                                                          
         SPACE                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         SPACE                                                                  
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
         EJECT                                                                  
       ++INCLUDE SPGENCLRST                                                     
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
       ++INCLUDE SPGENFLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSHR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLST                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSYN                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWBS                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPTRSTAL                                                       
         EJECT                                                                  
       ++INCLUDE SPTRTBAE                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMLTXT                                                     
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSDTL                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPEXTSTA3 06/24/92'                                      
         END                                                                    
