*          DATA SET SPEXTWUPN  AT LEVEL 011 AS OF 05/01/02                      
*PHASE SPEXTWU,*                                                                
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
* NOV  3,1996                                                                   
* CHANGE STATION CALL LETTERS FROM WUPN TO WPNY                                 
*                             AND  WGGT TO WUPN                                 
*                                                                               
* JUN 12,1992                                                                   
* CHANGE STATION CALL LETTERS FROM WOTV TO WOOD                                 
*                             AND  WUHQ TO WOTV                                 
*                                                                               
* FOR ALL AGENCIES, ALL SPOT FILES                                              
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
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WUPNT',WORK                          
         CLC   WUPNP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WPNYT',WORK                          
         CLC   WPNYP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(MSPACK),DMCB,=C'0000',=C'WGGTT',WORK                          
         CLC   WGGTP,WORK+2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,VLDDEFN                                                       
         USING LDDEFND,R4                                                       
         L     R5,LDDDTFD1                                                      
         DROP  R4                                                               
         SPACE                                                                  
         LA    R6,=CL20'LDDEF AREA'                                             
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R4),C'DUMP',128,=C'0D'                
         SPACE                                                                  
         LA    R6,=CL20'DCB AREA'                                               
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R5),C'DUMP',128,=C'0D'                
         SPACE                                                                  
         MVC   SYSTEM,28(R5)    SAVE SYSTEM                                     
         SPACE                                                                  
* CLEAR HOLD AREA FOR WPNY MERGED STATION BILLS *                               
         SPACE                                                                  
         LA    R0,255                                                           
         L     RE,=A(STBLTBL)                                                   
DMXINT10 LA    RF,8                                                             
         SPACE                                                                  
DMXINT14 XC    0(248,RE),0(RE)                                                  
         LA    RE,248(,RE)                                                      
         BCT   RF,DMXINT14                                                      
         BCT   R0,DMXINT10                                                      
         SPACE                                                                  
         B     DMXIT                                                            
         SPACE                                                                  
* OPEN OUTPUT TAPE FOR MERGED STATION BILLS *                                   
         SPACE                                                                  
         OPEN  (STBILLS,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    DMXIT                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         CLI   0(R3),X'0A'         TRAFFIC                                      
         BL    DMXKEEP              NO                                          
         BE    TRA                                                              
         SPACE                                                                  
         CLI   0(R3),X'0B'         STATION INVOICE                              
         BE    STINV                YES.                                        
         SPACE                                                                  
         CLI   0(R3),X'0C'         SIR RECS?                                    
         BE    STSIR                YES.                                        
         SPACE                                                                  
         CLC   =X'0D05',0(R3)      STATION GROUP DEFINITION REC?                
         BH    DMXKEEP              NO                                          
         BE    STGRP                YES.                                        
         SPACE                                                                  
         CLC   =X'0D0D',0(R3)      SPOT FLIGHT REC                              
         BH    DMXKEEP              NO                                          
         BE    STFLT                YES.                                        
         SPACE                                                                  
         CLC   =X'0D13',0(R3)      CANADIAN SPILL                               
         BH    DMXKEEP              NO                                          
         BE    CASPL                YES.                                        
         SPACE                                                                  
         CLC   =X'0D42',0(R3)      IS THIS A STATION PCT?                       
         BH    DMXKEEP              NO                                          
         BE    STPCT                YES.                                        
         SPACE                                                                  
         CLC   =X'0D44',0(R3)      IS THIS A STATION EQU?                       
         BE    STEQU                YES.                                        
         SPACE                                                                  
         CLC   =X'0D47',0(R3)      IS THIS A COMPETITIVE STATION REC?           
         BE    STCST                YES.                                        
         SPACE                                                                  
         CLC   =X'0D49',0(R3)      IS THIS A CSO PROGRAM REC?                   
         BE    STCSO                YES.                                        
         SPACE                                                                  
         CLC   =X'0D57',0(R3)      IS THIS A CSO DEMO OVRREC?                   
         BE    STCDO                YES.                                        
         SPACE                                                                  
         CLC   =X'0D58',0(R3)      IS THIS A CSO COMMENTS REC?                  
         BE    STCSC                YES.                                        
         SPACE                                                                  
         CLC   =X'0D59',0(R3)      EST STA BOOK?                                
         BE    ESTST                YES.                                        
         SPACE                                                                  
         CLC   =X'0D5B',0(R3)      STATION LIST?                                
         BE    STLST                YES.                                        
         SPACE                                                                  
         CLC   =X'0D67',0(R3)      NWS HEADER?                                  
         BE    STNWSH               YES.                                        
         SPACE                                                                  
         CLC   =X'0D68',0(R3)      NWS DETAIL?                                  
         BE    STNWSD               YES.                                        
         SPACE                                                                  
         CLC   =X'0D70',0(R3)      PRD EXCLUSION?                               
         BE    PRDEX                YES.                                        
         SPACE                                                                  
         CLC   =X'0D71',0(R3)      STATUS?                                      
         BE    STATUS               YES.                                        
         SPACE                                                                  
         CLC   =X'0D75',0(R3)      STATION LIST?                                
         BE    STSLT                YES.                                        
         SPACE                                                                  
         CLC   =X'0D76',0(R3)      CLEARED STATUS?                              
         BE    CLSTA                YES.                                        
         SPACE                                                                  
         CLC   =X'0E01',0(R3)      STATION BILL?                                
         BH    DMXKEEP              NO                                          
         BE    STBILL               YES.                                        
         CLI   STBILLSW,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,WTSTBL           WRITE OUT ANY STATION BILLS                  
         SPACE                                                                  
         CLI   0(R3),X'11'         LESS THAN BUY                                
         BL    DMXKEEP                                                          
         EJECT                                                                  
* BUY RECORD *                                                                  
         SPACE                                                                  
         BAS   RE,CKBUYS                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TBUYCT,=P'1'                                                     
         CLC   WPNYP,6(R3)         SHOULD NOT BE ANY                            
         BNE   STBUY04                                                          
         MVC   P(24),=CL24'BUY REC WITH WPNY IN KEY'                            
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         AP    TWPNY,=P'1'                                                      
         AP    BUYCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STBUY04  CLC   WUPNP,6(R3)                                                      
         BNE   STBUY10                                                          
         CLC   COMPKEY,0(R3)       PRINT 1ST OF EVERY GROUP                     
         BE    STBUY08                                                          
         MVC   COMPKEY,0(R3)                                                    
         MVC   P+60(30),=CL30'WUPN KEY - CK TO WPNY FOR SUB'                    
         GOTO1 =V(HEXOUT),DMCB,(R3),P+30,13,0,0                                 
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
STBUY08  BAS   RE,FIXS             FIX SUBLINE IF NEEDED                        
         SPACE                                                                  
         MVC   6(3,R3),WPNYP                                                    
         AP    BUYCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'BUY RENAME WUPN'                                        
         B     PRT                                                              
STBUY10  CLC   WGGTP,6(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   6(3,R3),WUPNP                                                    
         AP    BUYCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'BUY RENAME WGGT'                                        
         B     PRT                                                              
         EJECT                                                                  
* STATION INVOICE RECORD 0B   *                                                 
         SPACE                                                                  
STINV    DS   0H                                                                
         SPACE                                                                  
         BAS   RE,CKBYT1                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TINVCT,=P'1'                                                     
         CLC   WPNYP,2(R3)         SHOULD NOT BE ANY                            
         BNE   STINV04                                                          
         MVC   P(25),=CL25'STA INVOICE - WPNY IN KEY'                           
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    INVCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STINV04  CLC   WUPNP,2(R3)                                                      
         BNE   STINV10                                                          
         MVC   2(3,R3),WPNYP                                                    
         AP    INVCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA INV WUPN'                                           
         B     PRT                                                              
STINV10  CLC   WGGTP,2(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   2(3,R3),WUPNP                                                    
         AP    INVCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA INV WGGT'                                           
         B     PRT                                                              
         EJECT                                                                  
* S I R  RECORD     0C   *                                                      
         SPACE                                                                  
STSIR    CLI   1(R3),01                                                         
         BE    DMXKEEP                                                          
         SPACE                                                                  
         BAS   RE,CKBYT1                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TSIRCT,=P'1'                                                     
         CLC   WPNYP,6(R3)         SHOULD NOT BE ANY                            
         BNE   STSIR04                                                          
         MVC   P(23),=CL23'S I R REC - WPNY IN KEY'                             
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    SIRCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STSIR04  CLC   WUPNP,6(R3)                                                      
         BNE   STSIR10                                                          
         MVC   6(3,R3),WPNYP                                                    
         AP    SIRCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'SIR REC WUPN'                                           
         B     PRT                                                              
STSIR10  CLC   WGGTP,6(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   6(3,R3),WUPNP                                                    
         AP    SIRCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'SIR REC WGGT'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATION GROUP RECORD 0D05 *                                                   
         SPACE                                                                  
STGRP    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TSGRCT,=P'1'                                                     
         LA    R4,24(,R3)                                                       
         SR    R5,R5                                                            
STGRP10  CLI   0(R4),X'30'                                                      
         BE    STGRP20                                                          
STGRP14  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   STGRP10              NO                                          
         B     STGRP30                                                          
         SPACE                                                                  
STGRP20  CLC   WPNYT,2(R4)         SHOULD NOT BE ANY                            
         BNE   STGRP24                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWPNY,=P'1'                                                      
         AP    SGRCTC,=P'1'                                                     
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STATN GRP-WPNY IN REC'                                  
         B     PRT                                                              
STGRP24  CLC   WUPNT,2(R4)                                                      
         BNE   STGRP26                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   2(4,R4),WPNYT                                                    
         AP    SGRCTA,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STGRP14                                                          
STGRP26  CLC   WGGTT,2(R4)                                                      
         BNE   DMXKEEP                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   2(4,R4),WUPNT                                                    
         AP    SGRCTB,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STGRP14                                                          
STGRP30  LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA GRP CHGE'                                           
         B     PRT                                                              
         EJECT                                                                  
* SPOT FLIGHT RECORD 0D0D *                                                     
         SPACE                                                                  
STFLT    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TFLTCT,=P'1'                                                     
         SPACE                                                                  
         CLC   WPNYP,9(R3)         SHOULD NOT BE ANY                            
         BNE   STFLT10                                                          
         MVC   P(24),=CL24'FLIGHT REC - WPNY IN KEY'                            
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    FLTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STFLT10  CLC   WUPNP,9(R3)                                                      
         BNE   STFLT20                                                          
         MVC   9(3,R4),WPNYP                                                    
         AP    FLTCTA,=P'1'                                                     
         B     STFLT30                                                          
STFLT20  CLC   WGGTP,9(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R3),WUPNP                                                    
         AP    FLTCTB,=P'1'                                                     
         SPACE                                                                  
STFLT30  BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'SPT FLT CHGE'                                           
         B     PRT                                                              
         EJECT                                                                  
* CANADIAN SPILL RECORD 0D13 *                                                  
         SPACE                                                                  
CASPL    DS    0H                                                               
         CLC   =C'JW',2(R3)       BYPASS JWNY                                   
         BE    DMXKEEP                                                          
         CLC   =C'WI',2(R3)       BYPASS WILA                                   
         BE    DMXKEEP                                                          
         CLC   =C'WR',2(R3)       BYPASS WRLA                                   
         BE    DMXKEEP                                                          
         CLC   =C'WT',2(R3)       BYPASS WITO                                   
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TCSPCT,=P'1'                                                     
         SPACE                                                                  
         CLC   WPNYT,5(R3)         SHOULD NOT BE ANY                            
         BNE   CASPL10                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(25),=CL25'CAN SPL REC - WPNY IN KEY'                           
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    SGRCTC,=P'1'                                                     
         B     DMXKEEP                                                          
CASPL10  CLC   WUPNT,5(R3)                                                      
         BNE   CASPL20                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   5(4,R3),WPNYT                                                    
         AP    CSPCTA,=P'1'                                                     
         B     CASPL30                                                          
CASPL20  CLC   WGGTT,5(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   5(4,R3),WUPNT                                                    
         AP    CSPCTB,=P'1'                                                     
         SPACE                                                                  
CASPL30  BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CAN SPILL CHGE'                                         
         B     PRT                                                              
         EJECT                                                                  
* STATION PCT RECORD 0D42 *                                                     
         SPACE                                                                  
STPCT    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TPCTCT,=P'1'                                                     
         CLC   WPNYT,5(R3)         SHOULD NOT BE ANY                            
         BNE   STPCT04                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(29),=CL29'STATION PCT REC - WPNY IN KEY'                       
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    PCTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STPCT04  CLC   WUPNT,5(R3)                                                      
         BNE   STPCT10                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   5(4,R3),WPNYT                                                    
         AP    PCTCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA PCT WUPN'                                           
         B     PRT                                                              
STPCT10  CLC   WGGTT,5(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   5(4,R3),WUPNT                                                    
         AP    PCTCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA PCT WGGT'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATION EQU RECORD 0D44 *                                                     
         SPACE                                                                  
STEQU    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TEQUCT,=P'1'                                                     
         CLC   WPNYT,5(R3)         SHOULD NOT BE ANY                            
         BNE   STEQU04                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(28),=CL28'STATION EQ REC - WPNY IN KEY'                        
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    EQUCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STEQU04  CLC   WUPNT,5(R3)                                                      
         BNE   STEQU10                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   5(4,R3),WPNYT                                                    
         AP    EQUCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EQU WUPN'                                           
         B     PRT                                                              
STEQU10  CLC   WGGTT,5(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   5(4,R3),WUPNT                                                    
         AP    EQUCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EQU WGGT'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATION COMPETITION RECORD 0D47 *                                             
         SPACE                                                                  
STCST    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TCSTCT,=P'1'                                                     
         LA    R4,24(,R3)                                                       
         SR    R5,R5                                                            
STCST10  CLI   0(R4),X'01'                                                      
         BE    STCST20                                                          
         CLI   0(R4),X'02'                                                      
         BE    STCST20                                                          
STCST14  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BE    DMXKEEP                                                          
         B     STCST10                                                          
         SPACE                                                                  
STCST20  ZIC   R6,1(R4)                                                         
         BCTR  R6,0                                                             
         BCTR  R6,0                                                             
         LA    R7,2(,R4)                                                        
         SPACE                                                                  
STCST22  CLC   WPNYT,0(R7)         SHOULD NOT BE ANY                            
         BNE   STCST24                                                          
         CLI   4(R7),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R7),C'F'                                                       
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TWPNY,=P'1'                                                      
         AP    CSTCTC,=P'1'                                                     
         SR    R7,R3               FIND DISP INTO REC                           
         ST    R7,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'CMPET STA-WPNY IN REC'                                  
         B     PRT                                                              
STCST24  CLC   WUPNT,0(R7)                                                      
         BNE   STCST26                                                          
         CLI   4(R7),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R7),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   0(4,R7),WPNYT                                                    
         AP    CSTCTA,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STCST28                                                          
STCST26  CLC   WGGTT,0(R7)                                                      
         BNE   DMXKEEP                                                          
         CLI   4(R7),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R7),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   0(4,R7),WUPNT                                                    
         AP    CSTCTB,=P'1'                                                     
         BCTR  R5,0                                                             
STCST28  LA    R7,5(,R7)                                                        
         SH    R6,=H'5'                                                         
         BP    STCST22                                                          
         B     STCST14                                                          
STCST30  LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'COM STA LIST CH 0D47'                                   
         B     PRT                                                              
         EJECT                                                                  
* CSO PROGRAM RECORD 0D49 *                                                     
         SPACE                                                                  
STCSO    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TCSOCT,=P'1'                                                     
         CLC   WPNYP,7(R3)         SHOULD NOT BE ANY                            
         BNE   STCSO04                                                          
         MVC   P(26),=CL26'CSO PROG REC - WPNY IN KEY'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    CSOCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STCSO04  CLC   WUPNP,7(R3)                                                      
         BNE   STCSO10                                                          
         MVC   7(3,R3),WPNYP                                                    
         AP    CSOCTA,=P'1'                                                     
         LA    R5,WPNYT                                                         
*                                                                               
         B     STCSO20                                                          
STCSO10  CLC   WGGTP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WUPNP                                                    
         AP    CSOCTB,=P'1'                                                     
         LA    R5,WUPNT                                                         
STCSO20  LA    R4,24(,R3)                                                       
         SPACE                                                                  
STCSO30  CLI   0(R4),X'02'                                                      
         BE    STCSO40                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   STCSO30                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CSO PROG REC'                                           
         B     PRT                                                              
STCSO40  MVC   2(4,R4),0(R5)                                                    
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CSO PROG REC'                                           
         B     PRT                                                              
         EJECT                                                                  
* CSO DEMO OVR REC   0D57 *                                                     
         SPACE                                                                  
STCDO    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TCDOCT,=P'1'                                                     
         CLC   WPNYP,7(R3)         SHOULD NOT BE ANY                            
         BNE   STCDO04                                                          
         MVC   P(29),=CL29'CSO DEM OVR REC - WPNY IN KEY'                       
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    CDOCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STCDO04  CLC   WUPNP,7(R3)                                                      
         BNE   STCDO10                                                          
         MVC   7(3,R3),WPNYP                                                    
         AP    CDOCTA,=P'1'                                                     
*                                                                               
         B     STCDO20                                                          
STCDO10  CLC   WGGTP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WUPNP                                                    
         AP    CDOCTB,=P'1'                                                     
*                                                                               
STCDO20  BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CSO DEM OVR REC'                                        
         B     PRT                                                              
         EJECT                                                                  
* CSO COMMENTS REC   0D58 *                                                     
         SPACE                                                                  
STCSC    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TCSCCT,=P'1'                                                     
         CLC   WPNYP,7(R3)         SHOULD NOT BE ANY                            
         BNE   STCSC04                                                          
         MVC   P(26),=CL26'CSO CMTS REC - WPNY IN KEY'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    CSCCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STCSC04  CLC   WUPNP,7(R3)                                                      
         BNE   STCSC10                                                          
         MVC   7(3,R3),WPNYP                                                    
         AP    CSCCTA,=P'1'                                                     
*                                                                               
         B     STCSC20                                                          
STCSC10  CLC   WGGTP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WUPNP                                                    
         AP    CSCCTB,=P'1'                                                     
*                                                                               
STCSC20  BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CSO COMMENTS REC'                                       
         B     PRT                                                              
         EJECT                                                                  
* EST STATION BOOK RECORD 0D59 *                                                
         SPACE                                                                  
ESTST    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TESTCT,=P'1'                                                     
         CLC   WPNYP,5(R3)         SHOULD NOT BE ANY                            
         BNE   ESTST04                                                          
         MVC   P(24),=CL24'STA BK REC - WPNY IN KEY'                            
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    ESTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
ESTST04  CLC   WUPNP,5(R3)                                                      
         BNE   ESTST10                                                          
         MVC   5(3,R3),WPNYP                                                    
         AP    ESTCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EST BK WUPN'                                        
         B     PRT                                                              
ESTST10  CLC   WGGTP,5(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   5(3,R3),WUPNP                                                    
         AP    ESTCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EST BK WGGT'                                        
         B     PRT                                                              
         EJECT                                                                  
* STATION LIST  RECORD 0D5B *                                                   
         SPACE                                                                  
STLST    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TSLSCT,=P'1'                                                     
         LA    R4,24(,R3)                                                       
         SPACE                                                                  
STLST10  CLI   0(R4),X'05'                                                      
         BE    STLST20                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BE    DMXKEEP                                                          
         B     STLST10                                                          
         SPACE                                                                  
STLST20  ZIC   R5,1(R4)                                                         
         LA    R6,6(,R4)                                                        
         SH    R5,=H'6'                                                         
         SR    R7,R7                                                            
STLST30  CLC   WPNYT,0(R6)         SHOULD NOT BE ANY                            
         BNE   STLST34                                                          
         CLI   4(R6),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R6),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWPNY,=P'1'                                                      
         AP    SLSCTC,=P'1'                                                     
         SR    R6,R3               FIND DISP INTO REC                           
         ST    R6,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA LIST-WPNY IN REC'                                   
         B     PRT                                                              
STLST34  CLC   WUPNT,0(R6)                                                      
         BNE   STLST36                                                          
         CLI   4(R6),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R6),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   0(4,R6),WPNYT                                                    
         AP    SLSCTA,=P'1'                                                     
         BCTR  R7,0                                                             
         B     STLST38                                                          
STLST36  CLC   WGGTT,0(R6)                                                      
         BNE   STLST38                                                          
         CLI   4(R6),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R6),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   0(4,R6),WUPNT                                                    
         AP    SLSCTB,=P'1'                                                     
         BCTR  R7,0                                                             
STLST38  LA    R6,5(,R6)                                                        
         SH    R5,=H'5'                                                         
         BP    STLST30                                                          
STLST40  LTR   R7,R7                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA LST CHGE 0D5B'                                      
         B     PRT                                                              
         EJECT                                                                  
* NWS HEADER RECORD 0D67 *  SPNWSHDR                                            
         SPACE                                                                  
STNWSH   DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TNWSHCT,=P'1'                                                    
         LA    R4,24(,R3)                                                       
         SR    R5,R5                                                            
         CLI   0(R4),0             END OF REC                                   
         BE    DMXKEEP                                                          
         SPACE                                                                  
STNWSH10 CLI   0(R4),X'02'                                                      
         BE    STNWSH20                                                         
         CLI   0(R4),X'04'         SPILL                                        
         BE    STNWSH40                                                         
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   STNWSH10                                                         
         DC    H'0'                                                             
         SPACE                                                                  
STNWSH20 LA    R6,3(R4)                                                         
         CLC   WPNYT,0(R6)         SHOULD NOT BE ANY                            
         BNE   STNWSH24                                                         
         CLI   4(R6),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R6),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWPNY,=P'1'                                                      
         AP    NWSHCTC,=P'1'                                                    
         SPACE                                                                  
         LA    R4,=CL20'NWS HDR-WPNY IN REC'                                    
         ST    R5,DUB+4                                                         
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         L     R5,DUB+4                                                         
         LR    RF,R6                                                            
         SR    RF,R3               FIND DISP INTO REC                           
         ST    RF,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'MUST FIX NWS DETAIL REC'                              
         GOTO1 VPRINTER                                                         
         B     STNWSH50                                                         
         SPACE                                                                  
STNWSH24 CLC   WUPNT,0(R6)                                                      
         BNE   STNWSH26                                                         
         CLI   4(R6),C'A'                                                       
         BE    STNWSH50                                                         
         CLI   4(R6),C'F'                                                       
         BE    STNWSH50                                                         
         MVC   0(4,R6),WPNYT                                                    
         AP    NWSHCTA,=P'1'                                                    
         BCTR  R5,0                                                             
         B     STNWSH50                                                         
STNWSH26 CLC   WGGTT,0(R6)                                                      
         BNE   STNWSH50                                                         
         CLI   4(R6),C'A'                                                       
         BE    STNWSH50                                                         
         CLI   4(R6),C'F'                                                       
         BE    STNWSH50                                                         
         MVC   0(4,R6),WUPNT                                                    
         AP    NWSHCTB,=P'1'                                                    
         BCTR  R5,0                                                             
         B     STNWSH50                                                         
         SPACE                                                                  
STNWSH40 LA    R6,4(R4)                                                         
         CLC   WPNYT,0(R6)         SHOULD NOT BE ANY                            
         BNE   STNWSH44                                                         
         CLI   4(R6),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R6),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWPNY,=P'1'                                                      
         AP    NWSHCTC,=P'1'                                                    
         SPACE                                                                  
         LA    R4,=CL20'NWS HDR-WPNY IN REC'                                    
         ST    R5,DUB+4                                                         
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         L     R5,DUB+4                                                         
         LR    RF,R6                                                            
         SR    RF,R3               FIND DISP INTO REC                           
         ST    RF,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'MUST FIX NWS DETAIL REC'                              
         GOTO1 VPRINTER                                                         
         B     STNWSH50                                                         
         SPACE                                                                  
STNWSH44 CLC   WUPNT,0(R6)                                                      
         BNE   STNWSH46                                                         
         CLI   4(R6),C'A'                                                       
         BE    STNWSH50                                                         
         CLI   4(R6),C'F'                                                       
         BE    STNWSH50                                                         
         MVC   0(4,R6),WPNYT                                                    
         AP    NWSHCTA,=P'1'                                                    
         BCTR  R5,0                                                             
         B     STNWSH50                                                         
STNWSH46 CLC   WGGTT,0(R6)                                                      
         BNE   STNWSH50                                                         
         CLI   4(R6),C'A'                                                       
         BE    STNWSH50                                                         
         CLI   4(R6),C'F'                                                       
         BE    STNWSH50                                                         
         MVC   0(4,R6),WUPNT                                                    
         AP    NWSHCTB,=P'1'                                                    
         BCTR  R5,0                                                             
         B     STNWSH50                                                         
         SPACE                                                                  
STNWSH50 DS   0H                                                                
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'02'                                                      
         BE    STNWSH20                                                         
         CLI   0(R4),X'04'         SPILL                                        
         BE    STNWSH40                                                         
         CLI   0(R4),0             END OF REC                                   
         BNE   STNWSH50                                                         
         B     STNWSH60                                                         
         SPACE                                                                  
STNWSH60 LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS HDR CHGE'                                           
         B     PRT                                                              
         SPACE                                                                  
         EJECT                                                                  
* NWS DETAIL RECORD 0D68 *  SPNWSDTL                                            
         SPACE                                                                  
STNWSD   DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TNWSDCT,=P'1'                                                    
         LA    R4,24(,R3)                                                       
         CLI   0(R4),0             END OF REC                                   
         BE    DMXKEEP                                                          
         SPACE                                                                  
STNWSD10 CLI   0(R4),X'01'                                                      
         BE    STNWSD20                                                         
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   STNWSD10                                                         
         DC    H'0'                                                             
         SPACE                                                                  
STNWSD20 CLC   WPNYT,BWDSTA-BWDEL(R6)     SHOULD NOT BE ANY                     
         BNE   STNWSD24                                                         
         CLI   BWDSTA-BWDEL+4(R6),C'A'                                          
         BE    DMXKEEP                                                          
         CLI   BWDSTA-BWDEL+4(R6),C'F'                                          
         BE    DMXKEEP                                                          
         AP    TWPNY,=P'1'                                                      
         AP    NWSDCTC,=P'1'                                                    
         SPACE                                                                  
         LA    R4,=CL20'NWS DTL-WPNY IN REC'                                    
         ST    R5,DUB+4                                                         
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         L     R5,DUB+4                                                         
         LR    RF,R6                                                            
         SR    RF,R3               FIND DISP INTO REC                           
         ST    RF,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         MVC   P+1(23),=C'MUST FIX NWS DETAIL REC'                              
         GOTO1 VPRINTER                                                         
         B     STNWSD30                                                         
         SPACE                                                                  
STNWSD24 CLC   WUPNT,BWDSTA-BWDEL(R6)                                           
         BNE   STNWSD26                                                         
         CLI   BWDSTA-BWDEL+4(R6),C'A'                                          
         BE    DMXKEEP                                                          
         CLI   BWDSTA-BWDEL+4(R6),C'F'                                          
         BE    DMXKEEP                                                          
         MVC   BWDSTA-BWDEL(4,R6),WPNYT                                         
         AP    NWSDCTA,=P'1'                                                    
         B     STNWSD30                                                         
STNWSD26 CLC   WGGTT,BWDSTA-BWDEL(R6)                                           
         BNE   DMXKEEP                                                          
         CLI   BWDSTA-BWDEL+4(R6),C'A'                                          
         BE    DMXKEEP                                                          
         CLI   BWDSTA-BWDEL+4(R6),C'F'                                          
         BE    DMXKEEP                                                          
         MVC   BWDSTA-BWDEL(4,R6),WUPNT                                         
         AP    NWSDCTB,=P'1'                                                    
         SPACE                                                                  
STNWSD30 LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS DTL CHGE'                                           
         B     PRT                                                              
         EJECT                                                                  
* PRD EXCLUSION RECORD 0D70 *                                                   
         SPACE                                                                  
PRDEX    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TPRDEXCT,=P'1'                                                   
         CLC   WPNYP,5(R3)         SHOULD NOT BE ANY                            
         BNE   PRDEX04                                                          
         MVC   P(27),=CL27'PRD EXCLU REC - WPNY IN KEY'                         
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    PRDEXCTC,=P'1'                                                   
         B     DMXKEEP                                                          
PRDEX04  CLC   WUPNP,5(R3)                                                      
         BNE   PRDEX10                                                          
         MVC   5(3,R3),WPNYP                                                    
         AP    PRDEXCTA,=P'1'                                                   
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PRD EXC WUPN'                                           
         B     PRT                                                              
PRDEX10  CLC   WGGTP,5(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   5(3,R3),WUPNP                                                    
         AP    PRDEXCTB,=P'1'                                                   
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PRD EXC WGGT'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATUS        RECORD 0D71 *                                                   
         SPACE                                                                  
STATUS   DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TSTATUCT,=P'1'                                                   
         CLC   WPNYP,10(R3)        SHOULD NOT BE ANY                            
         BNE   STATUS04                                                         
         MVC   P(24),=CL24'STATUS REC - WPNY IN KEY'                            
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    STATUCTC,=P'1'                                                   
         B     DMXKEEP                                                          
STATUS04 CLC   WUPNP,10(R3)                                                     
         BNE   STATUS10                                                         
         MVC   10(3,R3),WPNYP                                                   
         AP    STATUCTA,=P'1'                                                   
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STATUS WUPN'                                            
         B     PRT                                                              
STATUS10 CLC   WGGTP,10(R3)                                                     
         BNE   DMXKEEP                                                          
         MVC   10(3,R3),WUPNP                                                   
         AP    STATUCTB,=P'1'                                                   
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STATUS WGGT'                                            
         B     PRT                                                              
         EJECT                                                                  
* STATION LOCK HDR RECORD 0D72 *                                                
         SPACE                                                                  
STSLH    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TSLHCT,=P'1'                                                     
         CLC   WPNYP,7(R3)        SHOULD NOT BE ANY                             
         BNE   STSLH04                                                          
         MVC   P(24),=CL24'STA LK HDR - WPNY IN KEY'                            
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    SLHCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STSLH04  CLC   WUPNP,7(R3)                                                      
         BNE   STSLH10                                                          
         MVC   7(3,R3),WPNYP                                                    
         AP    SLHCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA LOC HDR WUPN'                                       
         B     PRT                                                              
STSLH10  CLC   WGGTP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WUPNP                                                    
         AP    SLHCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA LOC HDR WGGT'                                       
         B     PRT                                                              
         EJECT                                                                  
* STATION LIST RECORD 0D75 *                                                    
         SPACE                                                                  
STSLT    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TSLTCT,=P'1'                                                     
         SPACE                                                                  
         SR    R5,R5                                                            
         CLC   WPNYT,3(R3)         SHOULD NOT BE ANY                            
         BNE   STSLT10                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(25),=CL25'NTI STA LST - WPNY IN KEY'                           
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    SLTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STSLT10  CLC   WUPNT,3(R3)                                                      
         BNE   STSLT20                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   3(4,R3),WPNYT                                                    
         AP    SLTCTA,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STSLT30                                                          
STSLT20  CLC   WGGTT,3(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   3(4,R3),WUPNT                                                    
         AP    SLTCTB,=P'1'                                                     
         BCTR  R5,0                                                             
         SPACE                                                                  
STSLT30  LA    R4,24(,R3)                                                       
         SPACE                                                                  
STSLT40  CLI   0(R4),X'01'                                                      
         BE    STSLT60                                                          
STSLT44  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   STSLT40                                                          
         B     STSLT70                                                          
         SPACE                                                                  
STSLT60  CLC   WPNYT,2(R4)         SHOULD NOT BE ANY                            
         BNE   STSLT64                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWPNY,=P'1'                                                      
         AP    SLTCTF,=P'1'                                                     
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA LST-WPNY IN REC'                                    
         B     PRT                                                              
STSLT64  CLC   WUPNT,2(R4)                                                      
         BNE   STSLT66                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   2(4,R4),WPNYT                                                    
         AP    SLTCTD,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STSLT44                                                          
STSLT66  CLC   WGGTT,2(R4)                                                      
         BNE   STSLT44                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   2(4,R4),WUPNT                                                    
         AP    SLTCTE,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STSLT44                                                          
STSLT70  LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NTI STAT LIST 0D75'                                     
         B     PRT                                                              
         EJECT                                                                  
* CLEARED STATUS RECORD 0D76 *                                                  
         SPACE                                                                  
CLSTA    DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TCLSTACT,=P'1'                                                   
         CLC   WPNYP,7(R3)         SHOULD NOT BE ANY                            
         BNE   CLSTA04                                                          
         MVC   P(29),=CL29'CLRD STATUS REC - WPNY IN KEY'                       
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    CLSTACTC,=P'1'                                                   
         B     DMXKEEP                                                          
CLSTA04  CLC   WUPNP,7(R3)                                                      
         BNE   CLSTA10                                                          
         MVC   7(3,R3),WPNYP                                                    
         AP    CLSTACTA,=P'1'                                                   
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CLRD STAT WUPN'                                         
         B     PRT                                                              
CLSTA10  CLC   WGGTP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WUPNP                                                    
         AP    CLSTACTB,=P'1'                                                   
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CLRD STAT WGGT'                                         
         B     PRT                                                              
         EJECT                                                                  
* STATION BILL RECORDS 0E01 *                                                   
         SPACE                                                                  
STBILL   DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TBILLCT,=P'1'                                                    
         SPACE                                                                  
         CLC   STBAMCLP,2(R3)                                                   
         BE    STBILL10                                                         
         SPACE                                                                  
         CLI   STBILLSW,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,WTSTBL           WRITE OUT ANY STA BILLS                      
         MVC   STBAMCLP,2(R3)                                                   
         SPACE                                                                  
STBILL10 CLC   WPNYP,9(R3)         SHOULD NOT BE ANY                            
         BNE   STBILL20                                                         
         MVC   P(22),=CL22'STA BILL - WPNY IN KEY'                              
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    BILLCTC,=P'1'                                                    
         MVI   STBILLSW,C'Y'                                                    
         LA    RF,255                                                           
         L     RE,=A(STBLTBL)                                                   
STBILL12 OC    0(2,RE),0(RE)                                                    
         BZ    STBILL14                                                         
         AH    RE,=H'1984'                                                      
         BCT   RF,STBILL12                                                      
         DC    H'0'                                                             
STBILL14 SR    R1,R1                                                            
         ICM   R1,3,13(R3)         SET TAPE RECORD LENGTH                       
         LA    R1,4(,R1)                                                        
         STCM  R1,3,0(RE)                                                       
         SR    RF,RF                                                            
         ICM   RF,3,13(R3)                                                      
         LA    RE,4(,RE)                                                        
         LR    R1,RF                                                            
         LR    R0,R3                                                            
         MVCL  RE,R0                                                            
         MVI   STBILLSW,C'Y'                                                    
         B     DMXPURGE                                                         
         SPACE                                                                  
STBILL20 CLC   WUPNP,9(R3)                                                      
         BNE   STBILL30                                                         
         MVC   9(3,R3),WPNYP                                                    
         SPACE                                                                  
* SEE IF EQUAL EXISTS IN TABLE *                                                
         SPACE                                                                  
         LA    RF,255                                                           
         L     RE,=A(STBLTBL)                                                   
STBILL21 OC    0(2,RE),0(RE)                                                    
         BZ    STBILL28                                                         
         CLC   4(13,RE),0(R3)      EQUAL KEY                                    
         BE    STBILL22                                                         
         AH    RE,=H'1984'                                                      
         BCT   RF,STBILL21                                                      
         DC    H'0'                                                             
STBILL22 LR    R6,R3                                                            
         LA    R4,4(,RE)                                                        
         MVI   ELCODE,X'0E'                                                     
         BAS   RE,GETEL                                                         
         BE    STBILL24                                                         
         DC    H'0'                                                             
STBILL24 DS    0H                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'SPTFILE'),(R4),(R6)                      
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,NEXTEL                                                        
         BE    STBILL24                                                         
         SPACE                                                                  
         SR    R1,R1                                                            
         ICM   R1,3,13(R4)         RESET NEW TAPE RECORD LENGTH                 
         SH    R4,=H'4'                                                         
         LA    R1,4(,R1)                                                        
         STCM  R1,3,0(R4)                                                       
         SPACE                                                                  
         LA    R6,=CL20'NEW WPNY STA BILL'                                      
         SR    R5,R5                                                            
         ICM   R5,3,0(R4)                                                       
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R4),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         LA    R4,=CL20'WUPN MERGED STA BILL'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         AP    BILLCTM,=P'1'       ADD TO MERGED CT                             
         B     DMXPURGE                                                         
STBILL28 AP    BILLCTA,=P'1'                                                    
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA BILL WUPN'                                          
         B     PRT                                                              
STBILL30 CLC   WGGTP,9(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R3),WUPNP                                                    
         AP    BILLCTB,=P'1'                                                    
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA BILL WGGT'                                          
         B     PRT                                                              
         EJECT                                                                  
* CHECK OUT TRAFFIC RECORDS *                                                   
         SPACE                                                                  
TRA      DS    0H                                                               
         SPACE                                                                  
         BAS   RE,CKBYT2                                                        
         BE    DMXKEEP                                                          
         SPACE                                                                  
         CLI   1(R3),X'22'         PATTERN                                      
         BE    TRA22                                                            
         CLI   1(R3),X'24'         INSTR REC                                    
         BE    TRA24                                                            
         CLI   1(R3),X'25'         SHIP REC                                     
         BE    TRA25                                                            
         CLI   1(R3),X'28'         STA ADDR                                     
         BE    TRA28                                                            
         CLI   1(R3),X'2E'         BUYACT                                       
         BE    TRA2E                                                            
         CLI   1(R3),X'2F'         STATION LABEL LIST                           
         BE    TRA2F                                                            
         CLI   1(R3),X'31'         TBUY STA LIST                                
         BE    TRA31                                                            
         CLI   1(R3),X'32'         TRAFFIC BUYS                                 
         BE    TRA32                                                            
         CLI   1(R3),X'35'         TRAFFIC COMML TEXT                           
         BE    TRA35                                                            
         B     DMXKEEP                                                          
         EJECT                                                                  
* TRAFFIC PATTERN RECORD (STATION LIST) *                                       
         SPACE                                                                  
TRA22    AP    TRA22CT,=P'1'                                                    
         MVI   ELCODE,X'20'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    PATLMSG+9(10),PATLMSG+9                                          
         CLI   2(R6),C'S'          STATION LIST                                 
         BNE   DMXKEEP                                                          
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         LA    R5,3(R6)                                                         
TRA2210  CLC   WPNYT,0(R5)         SHOULD NOT BE ANY                            
         BNE   TRA2214                                                          
         CLI   4(R5),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R5),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P+30(3),=C'KEY'                                                  
         AP    TWPNY,=P'1'                                                      
         AP    TRA22C,=P'1'                                                     
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'TRA PAT-WPNY IN REC'                                    
         B     PRT                                                              
TRA2214  CLC   WUPNT,0(R5)                                                      
         BNE   TRA2220                                                          
         CLI   4(R5),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R5),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   0(4,R5),WPNYT                                                    
         AP    TRA22A,=P'1'                                                     
         MVI   TRA2240+1,0                                                      
         OC    PATLMSG+9(4),PATLMSG+9                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PATLMSG+9(4),WUPNT                                               
         B     TRA2230                                                          
TRA2220  CLC   WGGTT,0(R5)                                                      
         BNE   TRA2230                                                          
         CLI   4(R5),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R5),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   0(4,R5),WUPNT                                                    
         AP    TRA22B,=P'1'                                                     
         MVI   TRA2240+1,0                                                      
         OC    PATLMSG+14(4),PATLMSG+14                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PATLMSG+14(4),WGGTT                                              
*                                                                               
TRA2230  LA    R5,5(,R5)                                                        
         SH    R1,=H'5'                                                         
         BP    TRA2210                                                          
TRA2240  B     DMXKEEP                                                          
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,PATLMSG                                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC INSTRUCTION RECAP RECORDS *                                           
         SPACE                                                                  
TRA24    AP    TRA24CT,=P'1'                                                    
         CLC   WPNYP,8(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2404                                                          
         MVC   P(22),=CL22'TRAF INS - WPNY IN KEY'                              
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA24C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2404  CLC   WUPNP,8(R3)                                                      
         BNE   TRA2410                                                          
         MVC   8(3,R3),WPNYP                                                    
         AP    TRA24A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR INST REC WUPN'                                       
         B     PRT                                                              
TRA2410  CLC   WGGTP,8(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   8(3,R3),WUPNP                                                    
         AP    TRA24B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR INST REC WGGT'                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC SHIPPING RECAP RECORDS *                                              
         SPACE                                                                  
TRA25    AP    TRA25CT,=P'1'                                                    
         CLC   WPNYP,7(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2504                                                          
         MVC   P(22),=CL22'TRAF SHP - WPNY IN KEY'                              
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA25C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2504  CLC   WUPNP,7(R3)                                                      
         BNE   TRA2510                                                          
         MVC   7(3,R3),WPNYP                                                    
         AP    TRA25A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR SHIP REC WUPN'                                       
         B     PRT                                                              
TRA2510  CLC   WGGTP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WUPNP                                                    
         AP    TRA25B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR SHIP REC WGGT'                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC STATION ADDRESS RECORDS *                                             
         SPACE                                                                  
TRA28    AP    TRA28CT,=P'1'                                                    
         CLC   WPNYT,3(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2804                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(26),=CL26'TRAF STA ADR - WPNY IN KEY'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA28C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2804  CLC   WUPNT,3(R3)                                                      
         BNE   TRA2810                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   3(4,R3),WPNYT                                                    
         AP    TRA28A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR STA ADDR WUPN'                                       
         B     PRT                                                              
TRA2810  CLC   WGGTT,3(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   3(4,R3),WUPNT                                                    
         AP    TRA28B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR STA ADDR WGGT'                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC BUY ACTIVITY RECORDS *                                                
         SPACE                                                                  
TRA2E    AP    TRA2ECT,=P'1'                                                    
         CLC   WPNYP,7(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2E04                                                          
         MVC   P(26),=CL26'TRAF BUY ACT - WPNY IN KEY'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA2EC,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2E04  CLC   WUPNP,7(R3)                                                      
         BNE   TRA2E10                                                          
         MVC   7(3,R3),WPNYP                                                    
         AP    TRA2EA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY ACT WUPN'                                        
         B     PRT                                                              
TRA2E10  CLC   WGGTP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WUPNP                                                    
         AP    TRA2EB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY ACT WGGT'                                        
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC STATION LABEL LIST RECORDS *                                          
         SPACE                                                                  
TRA2F    AP    TRA2FCT,=P'1'                                                    
         XC    LBLLMSG+9(10),LBLLMSG+9                                          
         MVI   ELCODE,X'10'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
TRA2F10  CLC   WPNYP,2(R6)         SHOULD NOT BE ANY                            
         BNE   TRA2F14                                                          
         MVC   P(26),=CL26'TRAF LAB LST - WPNY IN REC'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 =V(HEXOUT),DMCB,(R6),P+50,5,0,0                                  
         MVC   P+60(7),=C'ELEMENT'                                              
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA2FC,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2F14  CLC   WUPNP,2(R6)                                                      
         BNE   TRA2F20                                                          
         MVC   2(3,R6),WPNYP                                                    
         AP    TRA2FA,=P'1'                                                     
         MVI   TRA2F40+1,0                                                      
         OC    LBLLMSG+9(4),LBLLMSG+9                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   LBLLMSG+9(4),WUPNT                                               
         B     TRA2F30                                                          
TRA2F20  CLC   WGGTP,2(R6)                                                      
         BNE   TRA2F30                                                          
         MVC   2(3,R6),WUPNP                                                    
         AP    TRA2FB,=P'1'                                                     
         MVI   TRA2F40+1,0                                                      
         OC    LBLLMSG+14(4),LBLLMSG+14                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   LBLLMSG+14(4),WGGTT                                              
*                                                                               
TRA2F30  BAS   RE,NEXTEL                                                        
         BE    TRA2F10                                                          
TRA2F40  B     DMXKEEP                                                          
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,LBLLMSG                                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC STATION LIST *                                                        
         SPACE                                                                  
TRA31    AP    TRA31CT,=P'1'                                                    
         XC    STALMSG+9(10),STALMSG+9                                          
         MVI   ELCODE,X'10'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
TRA3110  CLC   WPNYP,2(R6)         SHOULD NOT BE ANY                            
         BNE   TRA3114                                                          
         MVC   P(26),=CL26'TRAF STA LST - WPNY IN REC'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 =V(HEXOUT),DMCB,(R6),P+50,5,0,0                                  
         MVC   P+60(7),=C'ELEMENT'                                              
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA31C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA3114  CLC   WUPNP,2(R6)                                                      
         BNE   TRA3120                                                          
         MVC   2(3,R6),WPNYP                                                    
         AP    TRA31A,=P'1'                                                     
         MVI   TRA3140+1,0                                                      
         OC    STALMSG+9(4),STALMSG+9                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   STALMSG+9(4),WUPNT                                               
         B     TRA3130                                                          
TRA3120  CLC   WGGTP,2(R6)                                                      
         BNE   TRA3130                                                          
         MVC   2(3,R6),WUPNP                                                    
         AP    TRA31B,=P'1'                                                     
         MVI   TRA3140+1,0                                                      
         OC    STALMSG+14(4),STALMSG+14                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   STALMSG+14(4),WGGTT                                              
*                                                                               
TRA3130  BAS   RE,NEXTEL                                                        
         BE    TRA3110                                                          
TRA3140  B     DMXKEEP                                                          
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,STALMSG                                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC BUY RECORDS *                                                         
         SPACE                                                                  
TRA32    AP    TRA32CT,=P'1'                                                    
         CLC   WPNYP,8(R3)         SHOULD NOT BE ANY                            
         BNE   TRA3204                                                          
         MVC   P(22),=CL22'TRAF BUY - WPNY IN KEY'                              
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA32C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA3204  CLC   WUPNP,8(R3)                                                      
         BNE   TRA3210                                                          
         MVC   8(3,R3),WPNYP                                                    
         AP    TRA32A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY WUPN'                                            
         B     PRT                                                              
TRA3210  CLC   WGGTP,8(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   8(3,R3),WUPNP                                                    
         AP    TRA32B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY WGGT'                                            
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC COMML TEXT RECORDS *                                                  
         SPACE                                                                  
TRA35    AP    TRA35CT,=P'1'                                                    
         CLC   WPNYP,8(R3)         SHOULD NOT BE ANY                            
         BNE   TRA3504                                                          
         MVC   P(26),=CL26'TRAF COM TXT - WPNY IN KEY'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 VPRINTER                                                         
         AP    TWPNY,=P'1'                                                      
         AP    TRA35C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA3504  CLC   WUPNP,8(R3)                                                      
         BNE   TRA3510                                                          
         MVC   8(3,R3),WPNYP                                                    
         AP    TRA35A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR C TXT WUPN'                                          
         B     PRT                                                              
TRA3510  CLC   WGGTP,8(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   8(3,R3),WUPNP                                                    
         AP    TRA35B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR C TXT WGGT'                                          
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
* CHECK BUYS FOR JW, WI, WR, WT                                                 
         SPACE                                                                  
CKBUYS   CLI   SYSTEM,C'F'         THIS INCLUDE JWNY SPT/TRF F                  
         BNE   CKBU10                                                           
         MVC   BYTE,0(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'B0'                                                       
         BR    RE                                                               
CKBU10   CLI   SYSTEM,C'N'         THIS INCLUDE WI   SPT/TRF N                  
         BNER  RE                                                               
         MVC   BYTE,0(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WILA                                         
         BER   RE                                                               
         CLI   BYTE,X'20'          WITO                                         
         BER   RE                                                               
         CLI   BYTE,X'30'          WRLA                                         
         BR    RE                                                               
         SPACE                                                                  
* CK ALL RECS WITH A/M IN BYTE 1                                                
         SPACE                                                                  
CKBYT1   CLI   SYSTEM,C'F'         THIS INCLUDE JWNY SPT/TRF F                  
         BNE   CKBYT16                                                          
         MVC   BYTE,1(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'B0'                                                       
         BR    RE                                                               
CKBYT16  CLI   SYSTEM,C'N'         THIS INCLUDE WI   SPT/TRF N                  
         BNER  RE                                                               
         MVC   BYTE,1(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'                                                       
         BER   RE                                                               
         CLI   BYTE,X'20'                                                       
         BER   RE                                                               
         CLI   BYTE,X'30'                                                       
         BR    RE                                                               
         SPACE                                                                  
* CK ALL RECS WITH A/M IN BYTE 2                                                
         SPACE                                                                  
CKBYT2   CLI   SYSTEM,C'F'         THIS INCLUDE JWNY SPT/TRF F                  
         BNE   CKBYT26                                                          
         MVC   BYTE,2(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'B0'                                                       
         BR    RE                                                               
CKBYT26  CLI   SYSTEM,C'N'         THIS INCLUDE WI   SPT/TRF N                  
         BNER  RE                                                               
         MVC   BYTE,2(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'                                                       
         BER   RE                                                               
         CLI   BYTE,X'20'                                                       
         BER   RE                                                               
         CLI   BYTE,X'30'                                                       
         BR    RE                                                               
         EJECT                                                                  
* FIX SUBLINE FOR BUYS *                                                        
         SPACE                                                                  
FIXS     NTR1                                                                   
         LA    R0,NBUYS                                                         
         LA    R1,BUYKEYS                                                       
FIXS10   CLC   0(10,R1),0(R3)                                                   
         BE    FIXS20                                                           
         LA    R1,14(,R1)                                                       
         BCT   R0,FIXS10                                                        
FIXSX    XIT1                                                                   
         SPACE                                                                  
FIXS20   ZIC   RE,13(R1)           GET SUBLINE ADD #                            
         ZIC   RF,10(R3)           GET SUBLINE                                  
         AR    RF,RE                                                            
         STC   RF,10(,R3)                                                       
         AP    BUYCTKC,=P'1'                                                    
         MVC   P+60(30),=CL30'WUPN KEY - UPDATED SUBLINE'                       
         GOTO1 =V(HEXOUT),DMCB,(R3),P+30,13,0,0                                 
         GOTO1 VPRINTER                                                         
         B     FIXSX                                                            
         SPACE 3                                                                
* WRITE WPNY STA BILLS TO TAPE *                                                
         SPACE                                                                  
WTSTBL   NTR1                                                                   
         DC    H'0'                TEMP CK                                      
         LA    R5,255                                                           
         L     R6,=A(STBLTBL)                                                   
         OC    0(2,R6),0(R6)                                                    
         BNZ   WTSTBL10                                                         
         DC    H'0'                                                             
WTSTBL10 OC    0(2,R6),0(R6)                                                    
         BZ    WTSTBL20                                                         
         SR    R1,R1                                                            
         ICM   R1,3,0(R6)                                                       
         SH    R1,=H'4'                                                         
         CLM   R1,3,17(R6)                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         PUT   STBILLS,(R6)                                                     
         AP    TPSTABLS,=P'1'                                                   
         SR    R5,R5                                                            
         ICM   R5,3,0(R6)                                                       
         LA    R4,=CL23'MERGED STA BILL TO TAPE'                                
         GOTO1 =V(PRNTBL),DMCB,(23,(R4)),(R6),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
* CLEAR AREA                                                                    
         SPACE                                                                  
         LA    R0,8                                                             
         LR    R1,R6                                                            
WTSTBL14 XC    0(248,R1),0(R1)                                                  
         LA    R1,248(,R1)                                                      
         BCT   R0,WTSTBL14                                                      
         SPACE                                                                  
         AH    R6,=H'1984'                                                      
         BCT   R5,WTSTBL10                                                      
         DC    H'0'                                                             
WTSTBL20 MVI   STBILLSW,C'N'       RESET STA BILLS PRESENT                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(60),RTITLE                                                     
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         CLI   SYSTEM,C'F'         THIS INCLUDE JWNY SPT/TRF F                  
         BNE   DMXEOF00                                                         
         MVC   P(13),=C'BYPASSED JWNY'                                          
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
DMXEOF00 CLI   SYSTEM,C'N'         THIS INCLUDE WI   SPT/TRF N                  
         BNE   DMXEOF04                                                         
         MVC   P(13),=C'BYPASSED WI  '                                          
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
DMXEOF04 LA    R5,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(22),4(R3)                                                    
         EDIT  (P4,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,26(,R3)                                                       
         BCT   R5,DMXEOF10                                                      
         GOTO1 VPRINTER                                                         
         LA    R5,TOTKCTRS                                                      
         LA    R3,TWPNY                                                         
         SPACE                                                                  
DMXEOF20 MVC   P+5(22),4(R3)                                                    
         EDIT  (P4,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,26(,R3)                                                       
         BCT   R5,DMXEOF20                                                      
         B     DMXIT                                                            
         CLOSE (STBILLS)                                                        
         B     DMXIT                                                            
         EJECT                                                                  
SYSTEM   DC    C' '                SYSTEM DUMP/RESTORING                        
STBILLSW DC    C' '                                                             
STBAMCLP DC    XL4'00'                                                          
LBLLMSG  DC    CL20'LBL LIST XXXX YYYY'                                         
STALMSG  DC    CL20'STA LIST '                                                  
PATLMSG  DC    CL20'PATTERN '                                                   
WPNYT    DC    CL4'WPNY'                                                        
WPNYP    DC    XL3'C96422'                                                      
WGGTT    DC    CL4'WGGT'                                                        
WGGTP    DC    XL3'C63622'                                                      
WUPNT    DC    CL4'WUPN'                                                        
WUPNP    DC    XL3'CB2042'                                                      
TOTRD    DC    PL4'0',CL22'TOTAL RECS READ'                                     
TBUYCT   DC    PL4'0',CL22'TOT BUYS'                                            
BUYCTA   DC    PL4'0',CL22'BUYS STA WUPN'                                       
BUYCTB   DC    PL4'0',CL22'BUYS STA WGGT'                                       
BUYCTC   DC    PL4'0',CL22'BUYS STA WPNY'                                       
BUYCTKC  DC    PL4'0',CL22'BUYS STA WUPN SUB CHA'                               
TINVCT   DC    PL4'0',CL22'TOT INV         0B  '                                
INVCTA   DC    PL4'0',CL22'TOT INVS WUPN'                                       
INVCTB   DC    PL4'0',CL22'TOT INVS WGGT'                                       
INVCTC   DC    PL4'0',CL22'TOT INVS WPNY'                                       
TSIRCT   DC    PL4'0',CL22'TOT SIR         0C  '                                
SIRCTA   DC    PL4'0',CL22'TOT SIRS WUPN'                                       
SIRCTB   DC    PL4'0',CL22'TOT SIRS WGGT'                                       
SIRCTC   DC    PL4'0',CL22'TOT SIRS WPNY'                                       
TSGRCT   DC    PL4'0',CL22'TOT STA GRP     0D05'                                
SGRCTA   DC    PL4'0',CL22'TOT STA GRP WUPN'                                    
SGRCTB   DC    PL4'0',CL22'TOT STA GRP WGGT'                                    
SGRCTC   DC    PL4'0',CL22'TOT STA GRP WPNY'                                    
TFLTCT   DC    PL4'0',CL22'TOT FLT         0D0D'                                
FLTCTA   DC    PL4'0',CL22'TOT FLTS WUPN'                                       
FLTCTB   DC    PL4'0',CL22'TOT FLTS WGGT'                                       
FLTCTC   DC    PL4'0',CL22'TOT SPT FLT WPNY'                                    
TCSPCT   DC    PL4'0',CL22'TOT CSP         0D0D'                                
CSPCTA   DC    PL4'0',CL22'TOT CSPS WUPN'                                       
CSPCTB   DC    PL4'0',CL22'TOT CSPS WGGT'                                       
CSPCTC   DC    PL4'0',CL22'TOT CAN SPL WPNY'                                    
TPCTCT   DC    PL4'0',CL22'TOT STA PCT     0D42'                                
PCTCTA   DC    PL4'0',CL22'TOT STA PCT WUPN'                                    
PCTCTB   DC    PL4'0',CL22'TOT STA PCT WGGT'                                    
PCTCTC   DC    PL4'0',CL22'TOT STA PCT WPNY'                                    
TEQUCT   DC    PL4'0',CL22'TOT STA EQU     0D44'                                
EQUCTA   DC    PL4'0',CL22'TOT STA EQU WUPN'                                    
EQUCTB   DC    PL4'0',CL22'TOT STA EQU WGGT'                                    
EQUCTC   DC    PL4'0',CL22'TOT STA EQU WPNY'                                    
TCSTCT   DC    PL4'0',CL22'TOT COMPET STA  0D47'                                
CSTCTA   DC    PL4'0',CL22'TOT CMP STA WUPN'                                    
CSTCTB   DC    PL4'0',CL22'TOT CMP STA WGGT'                                    
CSTCTC   DC    PL4'0',CL22'TOT CMP STA WPNY'                                    
TCSOCT   DC    PL4'0',CL22'TOT CSO PROGRA  0D49'                                
CSOCTA   DC    PL4'0',CL22'TOT CSO PRG WUPN'                                    
CSOCTB   DC    PL4'0',CL22'TOT CMP PRG WGGT'                                    
CSOCTC   DC    PL4'0',CL22'TOT CMP PRG WPNY'                                    
TCDOCT   DC    PL4'0',CL22'TOT CSO DEM OVR 0D57'                                
CDOCTA   DC    PL4'0',CL22'TOT CSO DEM WUPN'                                    
CDOCTB   DC    PL4'0',CL22'TOT CSO DEM WGGT'                                    
CDOCTC   DC    PL4'0',CL22'TOT CMP DEM WPNY'                                    
TCSCCT   DC    PL4'0',CL22'TOT CSO COMMNTS 0D58'                                
CSCCTA   DC    PL4'0',CL22'TOT CSO CMT WUPN'                                    
CSCCTB   DC    PL4'0',CL22'TOT CMP CMT WGGT'                                    
CSCCTC   DC    PL4'0',CL22'TOT CMP CMT WPNY'                                    
TSLSCT   DC    PL4'0',CL22'TOT STA LIST    0D5B'                                
SLSCTA   DC    PL4'0',CL22'TOT STA LST WUPN'                                    
SLSCTB   DC    PL4'0',CL22'TOT STA LST WGGT'                                    
SLSCTC   DC    PL4'0',CL22'TOT STA LST WPNY'                                    
TNWSHCT  DC    PL4'0',CL22'TOT NWS HDR     0D67'                                
NWSHCTA  DC    PL4'0',CL22'TOT NWS HDR WUPN'                                    
NWSHCTB  DC    PL4'0',CL22'TOT NWS HDR WGGT'                                    
NWSHCTC  DC    PL4'0',CL22'TOT NWS HDR WPNY'                                    
TNWSDCT  DC    PL4'0',CL22'TOT NWS DTL     0D68'                                
NWSDCTA  DC    PL4'0',CL22'TOT NWS DTL WUPN'                                    
NWSDCTB  DC    PL4'0',CL22'TOT NWS DTL WGGT'                                    
NWSDCTC  DC    PL4'0',CL22'TOT NWS DTL WPNY'                                    
TPRDEXCT DC    PL4'0',CL22'TOT PRD EXCL    0D70'                                
PRDEXCTA DC    PL4'0',CL22'TOT PRD EXCL WUPN'                                   
PRDEXCTB DC    PL4'0',CL22'TOT PRD EXCL WGGT'                                   
PRDEXCTC DC    PL4'0',CL22'TOT PRD EXCL WPNY'                                   
TSTATUCT DC    PL4'0',CL22'TOT STATUS      0D71'                                
STATUCTA DC    PL4'0',CL22'TOT STATUS   WUPN'                                   
STATUCTB DC    PL4'0',CL22'TOT STATUS   WGGT'                                   
STATUCTC DC    PL4'0',CL22'TOT STATUS   WPNY'                                   
TSLHCT   DC    PL4'0',CL22'TOT STA LOC HDR 0D72'                                
SLHCTA   DC    PL4'0',CL22'TOT STA LOCK WUPN'                                   
SLHCTB   DC    PL4'0',CL22'TOT STA LOCK WGGT'                                   
SLHCTC   DC    PL4'0',CL22'TOT STA LOCK WPNY'                                   
TSLTCT   DC    PL4'0',CL22'TOT STATION LST 0D75'                                
SLTCTA   DC    PL4'0',CL22'TOT STA LST  WUPN'                                   
SLTCTB   DC    PL4'0',CL22'TOT STA LST  WGGT'                                   
SLTCTC   DC    PL4'0',CL22'TOT STA LST  WPNY'                                   
SLTCTD   DC    PL4'0',CL22'TOT STA LST  WUPN ELE'                               
SLTCTE   DC    PL4'0',CL22'TOT STA LST  WGGT ELE'                               
SLTCTF   DC    PL4'0',CL22'TOT STA LST  WPNY ELE'                               
TCLSTACT DC    PL4'0',CL22'TOT CLEARED STA 0D76'                                
CLSTACTA DC    PL4'0',CL22'TOT CLST WUPN'                                       
CLSTACTB DC    PL4'0',CL22'TOT CLST WGGT'                                       
CLSTACTC DC    PL4'0',CL22'TOT CLST WPNY'                                       
TBILLCT  DC    PL4'0',CL22'TOT BILLS       0E01'                                
BILLCTA  DC    PL4'0',CL22'STA BILLS WUPN'                                      
BILLCTM  DC    PL4'0',CL22'STA BILLS WUPN MERGED'                               
BILLCTB  DC    PL4'0',CL22'STA BILLS WGGT'                                      
BILLCTC  DC    PL4'0',CL22'STA BILLS WPNY'                                      
TESTCT   DC    PL4'0',CL22'TOT EST STA BK'                                      
ESTCTA   DC    PL4'0',CL22'TOT EST STA BK WUPN'                                 
ESTCTB   DC    PL4'0',CL22'TOT EST STA BK WGGT'                                 
ESTCTC   DC    PL4'0',CL22'TOT EST STA BK WPNY'                                 
TRA22CT  DC    PL4'0',CL22'TOT TRAF PATTERNS 0A22'                              
TRA22A   DC    PL4'0',CL22'TOT TRAF PAT WUPN'                                   
TRA22B   DC    PL4'0',CL22'TOT TRAF PAT WGGT'                                   
TRA22C   DC    PL4'0',CL22'TOT TRAF PAT WPNY'                                   
TRA24CT  DC    PL4'0',CL22'TOT TRAF INST REC 0A24'                              
TRA24A   DC    PL4'0',CL22'TOT TRAF INST REC WUPN'                              
TRA24B   DC    PL4'0',CL22'TOT TRAF INST REC WGGT'                              
TRA24C   DC    PL4'0',CL22'TOT TRAF INST REC WPNY'                              
TRA25CT  DC    PL4'0',CL22'TOT TRAF SHIP REC'                                   
TRA25A   DC    PL4'0',CL22'TOT TRAF SHIP REC WUPN'                              
TRA25B   DC    PL4'0',CL22'TOT TRAF SHIP REC WGGT'                              
TRA25C   DC    PL4'0',CL22'TOT TRAF SHIP REC WPNY'                              
TRA28CT  DC    PL4'0',CL22'TOT TRAF STA ADDR RECS'                              
TRA28A   DC    PL4'0',CL22'TOT TRAF STA ADDR WUPN'                              
TRA28B   DC    PL4'0',CL22'TOT TRAF STA ADDR WGGT'                              
TRA28C   DC    PL4'0',CL22'TOT TRAF STA ADDR WPNY'                              
TRA2ECT  DC    PL4'0',CL22'TOT TRAF BUY ACT'                                    
TRA2EA   DC    PL4'0',CL22'TOT TRAF BUY ACT WUPN'                               
TRA2EB   DC    PL4'0',CL22'TOT TRAF BUY ACT WGGT'                               
TRA2EC   DC    PL4'0',CL22'TOT TRAF BUY ACT WPNY'                               
TRA2FCT  DC    PL4'0',CL22'TOT TR LABEL LIST ACT'                               
TRA2FA   DC    PL4'0',CL22'TOT TR LABEL LIST WUPN'                              
TRA2FB   DC    PL4'0',CL22'TOT TR LABEL LIST WGGT'                              
TRA2FC   DC    PL4'0',CL22'TOT TR LABEL LIST WPNY'                              
TRA31CT  DC    PL4'0',CL22'TOT TRAF STA LIST'                                   
TRA31A   DC    PL4'0',CL22'TOT TRAF STA LIST WUPN'                              
TRA31B   DC    PL4'0',CL22'TOT TRAF STA LIST WGGT'                              
TRA31C   DC    PL4'0',CL22'TOT TRAF STA LIST WPNY'                              
TRA32CT  DC    PL4'0',CL22'TOT TRAF BUYS'                                       
TRA32A   DC    PL4'0',CL22'TOT TRAF BUYS WUPN'                                  
TRA32B   DC    PL4'0',CL22'TOT TRAF BUYS WGGT'                                  
TRA32C   DC    PL4'0',CL22'TOT TRAF BUYS WPNY'                                  
TRA35CT  DC    PL4'0',CL22'TOT TRAF C TXT 0A35'                                 
TRA35A   DC    PL4'0',CL22'TOT TRAF C TX WUPN'                                  
TRA35B   DC    PL4'0',CL22'TOT TRAF C TX WGGT'                                  
TRA35C   DC    PL4'0',CL22'TOT TRAF C TX WPNY'                                  
TOTCTRS  EQU   (*-TOTRD)/26                                                     
TWPNY    DC    PL4'0',CL22'TOT WPNY REC'                                        
TPSTABLS DC    PL4'0',CL22'STAT BILLS ON TAPE'                                  
TOTKCTRS EQU   (*-TWPNY)/26                                                     
WORK     DS    CL64                                                             
COMPKEY  DC    XL10'00'                                                         
ELCODE   DS    XL1                                                              
BYTE     DC    XL1'00'                                                          
RTITLE   DC    CL60'CHANGE STATION CALL LETTERS WUPN TO WPNY AND WGGT TC        
               O WUPN'                                                          
         SPACE                                                                  
BUYKEYS  DC    XL13'FFC58EFF058DBD661201010100',X'09' SPOT 1                    
NBUYS    EQU   (*-BUYKEYS)/14                                                   
         SPACE 2                                                                
         LTORG                                                                  
         SPACE                                                                  
* OUTPUT TAPE - TO BE USED AS LOAD TAPE TO SPOT FILE                            
         SPACE                                                                  
STBILLS  DCB   BLKSIZE=8000,                                           C        
               BUFNO=2,                                                C        
               DDNAME=STBILLS,                                         C        
               DEVD=DA,                                                C        
               DSORG=PS,                                               C        
               LRECL=1984,                                             C        
               MACRF=(PM),                                             C        
               RECFM=VB                                                         
         SPACE 2                                                                
STBLTBL  DS    255CL1984                                                        
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
**PAN#1  DC    CL21'011SPEXTWUPN 05/01/02'                                      
         END                                                                    
