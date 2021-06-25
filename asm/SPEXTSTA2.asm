*          DATA SET SPEXTSTA2  AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET SPEXTSTA1  AT LEVEL 039 AS OF 06/16/92                      
*PHASE SPEXTSTG,*                                                               
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
*                                                                               
* JUN 20,1992 SPOT G ONLY - SPECIAL FOR WILA                                    
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
* CLEAR HOLD AREA FOR WOOD MERGED STATION BILLS *                               
         SPACE                                                                  
         LA    R0,255                                                           
         LA    RE,STBLTBL                                                       
DMXINT10 LA    RF,8                                                             
         SPACE                                                                  
DMXINT14 XC    0(248,RE),0(RE)                                                  
         LA    RE,248(,RE)                                                      
         BCT   RF,DMXINT14                                                      
         BCT   R0,DMXINT10                                                      
         SPACE                                                                  
* OPEN OUTPUT TAPE FOR MERGED STATION BILLS *                                   
         SPACE                                                                  
         OPEN  (STBILLS,(OUTPUT))                                               
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
         BE    STNWS                YES.                                        
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
         CLC   =X'0E14',0(R3)      WBS REGULAR?                                 
         BE    WBSR                 YES.                                        
         SPACE                                                                  
         CLC   =X'0E16',0(R3)      WBS GRADE ASS?                               
         BE    WBSG                 YES.                                        
         SPACE                                                                  
         CLI   0(R3),X'11'         LESS THAN BUY                                
         BL    DMXKEEP                                                          
         BE    DMXKEEP             WILA                                         
         EJECT                                                                  
* BUY RECORD *                                                                  
         SPACE                                                                  
         AP    TBUYCT,=P'1'                                                     
         CLC   WOODP,6(R3)         SHOULD NOT BE ANY                            
         BNE   STBUY04                                                          
         MVC   P(24),=CL24'BUY REC WITH WOOD IN KEY'                            
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         AP    TWOOD,=P'1'                                                      
         AP    BUYCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STBUY04  CLC   WOTVP,6(R3)                                                      
         BNE   STBUY10                                                          
         CLC   COMPKEY,0(R3)       PRINT 1ST OF EVERY GROUP                     
         BE    STBUY08                                                          
         MVC   COMPKEY,0(R3)                                                    
         MVC   P+60(30),=CL30'WOTV KEY - CK TO WOOD FOR SUB'                    
         GOTO1 =V(HEXOUT),DMCB,(R3),P+30,13,0,0                                 
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
STBUY08  BAS   RE,FIXS             FIX SUBLINE IF NEEDED                        
         SPACE                                                                  
         MVC   6(3,R3),WOODP                                                    
         AP    BUYCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'BUY RENAME WOTV'                                        
         B     PRT                                                              
STBUY10  CLC   WUHQP,6(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   6(3,R3),WOTVP                                                    
         AP    BUYCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'BUY RENAME WUHQ'                                        
         B     PRT                                                              
         EJECT                                                                  
* STATION INVOICE RECORD 0B   *                                                 
         SPACE                                                                  
STINV    AP    TINVCT,=P'1'                                                     
         SPACE                                                                  
         CLI   1(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,2(R3)         SHOULD NOT BE ANY                            
         BNE   STINV04                                                          
         MVC   P(50),=CL50'STATION INVOICE WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    INVCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STINV04  CLC   WOTVP,2(R3)                                                      
         BNE   STINV10                                                          
         MVC   2(3,R3),WOODP                                                    
         AP    INVCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA INV WOTV'                                           
         B     PRT                                                              
STINV10  CLC   WUHQP,2(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   2(3,R3),WOTVP                                                    
         AP    INVCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA INV WUHQ'                                           
         B     PRT                                                              
         EJECT                                                                  
* S I R  RECORD     0C   *                                                      
         SPACE                                                                  
STSIR    CLI   1(R3),01                                                         
         BE    DMXKEEP                                                          
         AP    TSIRCT,=P'1'                                                     
         SPACE                                                                  
         CLI   1(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,6(R3)         SHOULD NOT BE ANY                            
         BNE   STSIR04                                                          
         MVC   P(50),=CL50'S I R REC WITH WOOD IN KEY'                          
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    SIRCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STSIR04  CLC   WOTVP,6(R3)                                                      
         BNE   STSIR10                                                          
         MVC   6(3,R3),WOODP                                                    
         AP    SIRCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'SIR REC WOTV'                                           
         B     PRT                                                              
STSIR10  CLC   WUHQP,6(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   6(3,R3),WOTVP                                                    
         AP    SIRCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'SIR REC WUHQ'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATION GROUP RECORD 0D05 *                                                   
         SPACE                                                                  
STGRP    AP    TSGRCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
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
STGRP20  CLC   WOODT,2(R4)         SHOULD NOT BE ANY                            
         BNE   STGRP24                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWOOD,=P'1'                                                      
         AP    SGRCTC,=P'1'                                                     
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STATN GRP-WOOD IN REC'                                  
         B     PRT                                                              
STGRP24  CLC   WOTVT,2(R4)                                                      
         BNE   STGRP26                                                          
         CLI   6(R4),C' '                                                       
         BE    *+14                                                             
         CLI   6(R4),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(4,R4),WOODT                                                    
         AP    SGRCTA,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STGRP14                                                          
STGRP26  CLC   WUHQT,2(R4)                                                      
         BNE   DMXKEEP                                                          
         MVC   2(4,R4),WOTVT                                                    
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
STFLT    AP    TFLTCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,9(R3)         SHOULD NOT BE ANY                            
         BNE   STFLT10                                                          
         MVC   P(50),=CL50'FLIGHT REC WITH WOOD IN KEY'                         
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    FLTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STFLT10  CLC   WOTVP,9(R3)                                                      
         BNE   STFLT20                                                          
         MVC   9(3,R4),WOODP                                                    
         AP    FLTCTA,=P'1'                                                     
         B     STFLT30                                                          
STFLT20  CLC   WUHQP,9(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R3),WOTVP                                                    
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
CASPL    AP    TCSPCT,=P'1'                                                     
         SPACE                                                                  
         CLC   =C'WI',2(R3)        WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODT,5(R3)         SHOULD NOT BE ANY                            
         BNE   CASPL10                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(50),=CL50'CAN SPL REC WITH WOOD IN KEY'                        
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    SGRCTC,=P'1'                                                     
         B     DMXKEEP                                                          
CASPL10  CLC   WOTVT,5(R3)                                                      
         BNE   CASPL20                                                          
         CLI   9(R3),C' '                                                       
         BE    *+14                                                             
         CLI   9(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   5(4,R3),WOODT                                                    
         AP    CSPCTA,=P'1'                                                     
         B     CASPL30                                                          
CASPL20  CLC   WUHQT,5(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   9(R3),C' '                                                       
         BE    *+14                                                             
         CLI   9(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   5(4,R3),WOTVT                                                    
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
STPCT    AP    TPCTCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODT,5(R3)         SHOULD NOT BE ANY                            
         BNE   STPCT04                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(50),=CL50'STATION PCT REC WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    PCTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STPCT04  CLC   WOTVT,5(R3)                                                      
         BNE   STPCT10                                                          
         CLI   9(R3),C' '                                                       
         BE    *+14                                                             
         CLI   9(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   5(4,R3),WOODT                                                    
         AP    PCTCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA PCT WOTV'                                           
         B     PRT                                                              
STPCT10  CLC   WUHQT,5(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   9(R3),C' '                                                       
         BE    *+14                                                             
         CLI   9(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   5(4,R3),WOTVT                                                    
         AP    PCTCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA PCT WUHQ'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATION EQU RECORD 0D44 *                                                     
         SPACE                                                                  
STEQU    AP    TEQUCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODT,5(R3)         SHOULD NOT BE ANY                            
         BNE   STEQU04                                                          
         CLI   9(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   9(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(50),=CL50'STATION EQU REC WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    EQUCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STEQU04  CLC   WOTVT,5(R3)                                                      
         BNE   STEQU10                                                          
         CLI   9(R3),C' '                                                       
         BE    *+14                                                             
         CLI   9(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   5(4,R3),WOODT                                                    
         AP    EQUCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EQU WOTV'                                           
         B     PRT                                                              
STEQU10  CLC   WUHQT,5(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   9(R3),C' '                                                       
         BE    *+14                                                             
         CLI   9(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   5(4,R3),WOTVT                                                    
         AP    EQUCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EQU WUHQ'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATION COMPETITION RECORD 0D47 *                                             
         SPACE                                                                  
STCST    AP    TCSTCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
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
STCST22  CLC   WOODT,0(R7)         SHOULD NOT BE ANY                            
         BNE   STCST24                                                          
         CLI   4(R7),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R7),C'F'                                                       
         BE    DMXKEEP                                                          
         SPACE                                                                  
         AP    TWOOD,=P'1'                                                      
         AP    CSTCTC,=P'1'                                                     
         SR    R7,R3               FIND DISP INTO REC                           
         ST    R7,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'CMPET STA-WOOD IN REC'                                  
         B     PRT                                                              
STCST24  CLC   WOTVT,0(R7)                                                      
         BNE   STCST26                                                          
         CLI   4(R7),C' '                                                       
         BE    *+14                                                             
         CLI   4(R7),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R7),WOODT                                                    
         AP    CSTCTA,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STCST28                                                          
STCST26  CLC   WUHQT,0(R7)                                                      
         BNE   DMXKEEP                                                          
         CLI   4(R7),C' '                                                       
         BE    *+14                                                             
         CLI   4(R7),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R7),WOTVT                                                    
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
STCSO    AP    TCSOCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,7(R3)         SHOULD NOT BE ANY                            
         BNE   STCSO04                                                          
         MVC   P(50),=CL50'CSO PROGRAM REC WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    CSOCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STCSO04  CLC   WOTVP,7(R3)                                                      
         BNE   STCSO10                                                          
         MVC   7(3,R3),WOODP                                                    
         AP    CSOCTA,=P'1'                                                     
         LA    R5,WOODT                                                         
*                                                                               
         B     STCSO20                                                          
STCSO10  CLC   WUHQP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WOTVP                                                    
         AP    CSOCTB,=P'1'                                                     
         LA    R5,WOTVT                                                         
STCSO20  LA    R4,24(,R3)                                                       
         SPACE                                                                  
STCSO30  CLI   0(R4),X'02'                                                      
         BE    STCSO40                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   STCSO30                                                          
         DC    H'0'                                                             
STCSO40  MVC   2(4,R4),0(R5)                                                    
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CSO PROG REC'                                           
         B     PRT                                                              
         EJECT                                                                  
* CSO DEMO OVR REC   0D57 *                                                     
         SPACE                                                                  
STCDO    AP    TCDOCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,7(R3)         SHOULD NOT BE ANY                            
         BNE   STCDO04                                                          
         MVC   P(50),=CL50'CSO DEM OVR REC WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    CDOCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STCDO04  CLC   WOTVP,7(R3)                                                      
         BNE   STCDO10                                                          
         MVC   7(3,R3),WOODP                                                    
         AP    CDOCTA,=P'1'                                                     
*                                                                               
         B     STCDO20                                                          
STCDO10  CLC   WUHQP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WOTVP                                                    
         AP    CDOCTB,=P'1'                                                     
*                                                                               
STCDO20  BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CSO DEM OVR REC'                                        
         B     PRT                                                              
         EJECT                                                                  
* CSO COMMENTS REC   0D58 *                                                     
         SPACE                                                                  
STCSC    AP    TCSCCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,7(R3)         SHOULD NOT BE ANY                            
         BNE   STCSC04                                                          
         MVC   P(50),=CL50'CSO COMMENTS REC WITH WOOD IN KEY'                   
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    CSCCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STCSC04  CLC   WOTVP,7(R3)                                                      
         BNE   STCSC10                                                          
         MVC   7(3,R3),WOODP                                                    
         AP    CSCCTA,=P'1'                                                     
*                                                                               
         B     STCSC20                                                          
STCSC10  CLC   WUHQP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WOTVP                                                    
         AP    CSCCTB,=P'1'                                                     
*                                                                               
STCSC20  BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CSO COMMENTS REC'                                       
         B     PRT                                                              
         EJECT                                                                  
* EST STATION BOOK RECORD 0D59 *                                                
         SPACE                                                                  
ESTST    AP    TESTCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,5(R3)         SHOULD NOT BE ANY                            
         BNE   ESTST04                                                          
         MVC   P(50),=CL50'STATION BOOK REC WITH WOOD IN KEY'                   
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    ESTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
ESTST04  CLC   WOTVP,5(R3)                                                      
         BNE   ESTST10                                                          
         MVC   5(3,R3),WOODP                                                    
         AP    ESTCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EST BK WOTV'                                        
         B     PRT                                                              
ESTST10  CLC   WUHQP,5(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   5(3,R3),WOTVP                                                    
         AP    ESTCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA EST BK WUHQ'                                        
         B     PRT                                                              
         EJECT                                                                  
* STATION LIST  RECORD 0D5B *                                                   
         SPACE                                                                  
STLST    AP    TSLSCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
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
STLST30  CLC   WOODT,0(R6)         SHOULD NOT BE ANY                            
         BNE   STLST34                                                          
         CLI   4(R6),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R6),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWOOD,=P'1'                                                      
         AP    SLSCTC,=P'1'                                                     
         SR    R6,R3               FIND DISP INTO REC                           
         ST    R6,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA LIST-WOOD IN REC'                                   
         B     PRT                                                              
STLST34  CLC   WOTVT,0(R6)                                                      
         BNE   STLST36                                                          
         CLI   4(R6),C' '                                                       
         BE    *+14                                                             
         CLI   4(R6),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R6),WOODT                                                    
         AP    SLSCTA,=P'1'                                                     
         BCTR  R7,0                                                             
         B     STLST38                                                          
STLST36  CLC   WUHQT,0(R6)                                                      
         BNE   STLST38                                                          
         CLI   4(R6),C' '                                                       
         BE    *+14                                                             
         CLI   4(R6),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R6),WOTVT                                                    
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
* NWX HEADER RECORD 0D67 *  SPNWXHDR                                            
         SPACE                                                                  
STNWS    AP    TNWSCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         LA    R4,24(,R3)                                                       
         SPACE                                                                  
STNWS10  CLI   0(R4),X'01'                                                      
         BE    STNWS14                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   STNWS10                                                          
         DC    H'0'                                                             
         SPACE                                                                  
STNWS14  ZIC   R7,1(R4)                                                         
         BCTR  R7,0                                                             
         BCTR  R7,0                                                             
         LA    R6,2(,R4)                                                        
         SR    R5,R5                                                            
STNWS20  CLC   WOODT,0(R6)         SHOULD NOT BE ANY                            
         BNE   STNWS24                                                          
         CLI   4(R6),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R6),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWOOD,=P'1'                                                      
         AP    NWSCTC,=P'1'                                                     
         SR    R6,R3               FIND DISP INTO REC                           
         ST    R6,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'NWS HDR-WOOD IN REC'                                    
         B     PRT                                                              
STNWS24  CLC   WOTVT,0(R6)                                                      
         BNE   STNWS26                                                          
         CLI   4(R6),C' '                                                       
         BE    *+14                                                             
         CLI   4(R6),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R6),WOODT                                                    
         AP    NWSCTA,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STNWS28                                                          
STNWS26  CLC   WUHQT,0(R6)                                                      
         BNE   STNWS28                                                          
         CLI   4(R6),C' '                                                       
         BE    *+14                                                             
         CLI   4(R6),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R6),WOTVT                                                    
         AP    NWSCTB,=P'1'                                                     
         BCTR  R5,0                                                             
STNWS28  LA    R6,5(,R6)                                                        
         SH    R7,=H'5'                                                         
         BP    STNWS20                                                          
STNWS30  LTR   R5,R5                                                            
         BZ    DMXKEEP                                                          
         SPACE                                                                  
         BC    0,DMXKEEP                                                        
         SPACE                                                                  
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'NWS HDR CHGE'                                           
         B     PRT                                                              
         EJECT                                                                  
* PRD EXCLUSION RECORD 0D70 *                                                   
         SPACE                                                                  
PRDEX    AP    TPRDEXCT,=P'1'                                                   
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,5(R3)         SHOULD NOT BE ANY                            
         BNE   PRDEX04                                                          
         MVC   P(50),=CL50'PRODUCT EXCLU REC WITH WOOD IN KEY'                  
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    PRDEXCTC,=P'1'                                                   
         B     DMXKEEP                                                          
PRDEX04  CLC   WOTVP,5(R3)                                                      
         BNE   PRDEX10                                                          
         MVC   5(3,R3),WOODP                                                    
         AP    PRDEXCTA,=P'1'                                                   
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PRD EXC WOTV'                                           
         B     PRT                                                              
PRDEX10  CLC   WUHQP,5(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   5(3,R3),WOTVP                                                    
         AP    PRDEXCTB,=P'1'                                                   
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'PRD EXC WUHQ'                                           
         B     PRT                                                              
         EJECT                                                                  
* STATUS        RECORD 0D71 *                                                   
         SPACE                                                                  
STATUS   AP    TSTATUCT,=P'1'                                                   
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,10(R3)        SHOULD NOT BE ANY                            
         BNE   STATUS04                                                         
         MVC   P(50),=CL50'STATUS REC WITH WOOD IN KEY'                         
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    STATUCTC,=P'1'                                                   
         B     DMXKEEP                                                          
STATUS04 CLC   WOTVP,10(R3)                                                     
         BNE   STATUS10                                                         
         MVC   10(3,R3),WOODP                                                   
         AP    STATUCTA,=P'1'                                                   
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STATUS WOTV'                                            
         B     PRT                                                              
STATUS10 CLC   WUHQP,10(R3)                                                     
         BNE   DMXKEEP                                                          
         MVC   10(3,R3),WOTVP                                                   
         AP    STATUCTB,=P'1'                                                   
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STATUS WUHQ'                                            
         B     PRT                                                              
         EJECT                                                                  
* STATION LOCK HDR RECORD 0D72 *                                                
         SPACE                                                                  
STSLH    AP    TSLHCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,7(R3)        SHOULD NOT BE ANY                             
         BNE   STSLH04                                                          
         MVC   P(50),=CL50'STATION LOCK HDR WITH WOOD IN KEY'                   
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    SLHCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STSLH04  CLC   WOTVP,7(R3)                                                      
         BNE   STSLH10                                                          
         MVC   7(3,R3),WOODP                                                    
         AP    SLHCTA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA LOC HDR WOTV'                                       
         B     PRT                                                              
STSLH10  CLC   WUHQP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WOTVP                                                    
         AP    SLHCTB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA LOC HDR WUHQ'                                       
         B     PRT                                                              
         EJECT                                                                  
* STATION LIST RECORD 0D75 *                                                    
         SPACE                                                                  
STSLT    AP    TSLTCT,=P'1'                                                     
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         SR    R5,R5                                                            
         CLC   WOODT,3(R3)         SHOULD NOT BE ANY                            
         BNE   STSLT10                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(50),=CL50'NTI STA LIST WITH WOOD IN KEY'                       
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    SLTCTC,=P'1'                                                     
         B     DMXKEEP                                                          
STSLT10  CLC   WOTVT,3(R3)                                                      
         BNE   STSLT20                                                          
         CLI   7(R3),C' '                                                       
         BE    *+14                                                             
         CLI   7(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   3(4,R3),WOODT                                                    
         AP    SLTCTA,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STSLT30                                                          
STSLT20  CLC   WUHQT,3(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   7(R3),C' '                                                       
         BE    *+14                                                             
         CLI   7(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   3(4,R3),WOTVT                                                    
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
STSLT60  CLC   WOODT,2(R4)         SHOULD NOT BE ANY                            
         BNE   STSLT64                                                          
         CLI   6(R4),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   6(R4),C'F'                                                       
         BE    DMXKEEP                                                          
         AP    TWOOD,=P'1'                                                      
         AP    SLTCTF,=P'1'                                                     
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA LST-WOOD IN REC'                                    
         B     PRT                                                              
STSLT64  CLC   WOTVT,2(R4)                                                      
         BNE   STSLT66                                                          
         CLI   6(R4),C' '                                                       
         BE    *+14                                                             
         CLI   6(R4),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(4,R4),WOODT                                                    
         AP    SLTCTD,=P'1'                                                     
         BCTR  R5,0                                                             
         B     STSLT44                                                          
STSLT66  CLC   WUHQT,2(R4)                                                      
         BNE   STSLT44                                                          
         CLI   6(R4),C' '                                                       
         BE    *+14                                                             
         CLI   6(R4),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(4,R4),WOTVT                                                    
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
CLSTA    AP    TCLSTACT,=P'1'                                                   
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,7(R3)         SHOULD NOT BE ANY                            
         BNE   CLSTA04                                                          
         MVC   P(50),=CL50'CLEARED STATUS REC WITH WOOD IN KEY'                 
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    CLSTACTC,=P'1'                                                   
         B     DMXKEEP                                                          
CLSTA04  CLC   WOTVP,7(R3)                                                      
         BNE   CLSTA10                                                          
         MVC   7(3,R3),WOODP                                                    
         AP    CLSTACTA,=P'1'                                                   
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CLRD STAT WOTV'                                         
         B     PRT                                                              
CLSTA10  CLC   WUHQP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WOTVP                                                    
         AP    CLSTACTB,=P'1'                                                   
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'CLRD STAT WUHQ'                                         
         B     PRT                                                              
         EJECT                                                                  
* STATION BILL RECORDS 0E01 *                                                   
         SPACE                                                                  
STBILL   AP    TBILLCT,=P'1'                                                    
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   STBAMCLP,2(R3)                                                   
         BE    STBILL10                                                         
         SPACE                                                                  
         CLI   STBILLSW,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,WTSTBL           WRITE OUT ANY STA BILLS                      
         MVC   STBAMCLP,2(R3)                                                   
         SPACE                                                                  
STBILL10 CLC   WOODP,9(R3)         SHOULD NOT BE ANY                            
         BNE   STBILL20                                                         
         MVC   P(50),=CL50'STATION BILL REC WITH WOOD IN KEY'                   
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    BILLCTC,=P'1'                                                    
         MVI   STBILLSW,C'Y'                                                    
         LA    RF,255                                                           
         LA    RE,STBLTBL                                                       
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
STBILL20 CLC   WOTVP,9(R3)                                                      
         BNE   STBILL30                                                         
         MVC   9(3,R3),WOODP                                                    
         SPACE                                                                  
* SEE IF EQUAL EXISTS IN TABLE *                                                
         SPACE                                                                  
         LA    RF,255                                                           
         LA    RE,STBLTBL                                                       
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
         LA    R6,=CL20'NEW WOOD STA BILL'                                      
         SR    R5,R5                                                            
         ICM   R5,3,0(R4)                                                       
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R4),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         LA    R4,=CL20'WOTV MERGED STA BILL'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         AP    BILLCTM,=P'1'       ADD TO MERGED CT                             
         B     DMXPURGE                                                         
STBILL28 AP    BILLCTA,=P'1'                                                    
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA BILL WOTV'                                          
         B     PRT                                                              
STBILL30 CLC   WUHQP,9(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R3),WOTVP                                                    
         AP    BILLCTB,=P'1'                                                    
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA BILL WUHQ'                                          
         B     PRT                                                              
         EJECT                                                                  
* WBS REGULAR        0E14 *                                                     
         SPACE                                                                  
WBSR     AP    TWBSRCT,=P'1'                                                    
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,9(R3)         SHOULD NOT BE ANY                            
         BNE   WBSR04                                                           
         MVC   P(50),=CL50'WBS REGULAR REC WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    WBSRCTC,=P'1'                                                    
         B     DMXKEEP                                                          
WBSR04   CLC   WOTVP,9(R3)                                                      
         BNE   WBSR10                                                           
         MVC   9(3,R3),WOODP                                                    
         AP    WBSRCTA,=P'1'                                                    
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS REG WOTV'                                           
         B     PRT                                                              
WBSR10   CLC   WUHQP,9(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   9(3,R3),WOTVP                                                    
         AP    WBSRCTB,=P'1'                                                    
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS REG WUHQ'                                           
         B     PRT                                                              
         EJECT                                                                  
* WBS GRADE ASS      0E16 *                                                     
         SPACE                                                                  
WBSG     AP    TWBSGCT,=P'1'                                                    
         SPACE                                                                  
         CLI   2(R3),X'11'         WILA                                         
         BE    DMXKEEP              YES                                         
         SPACE                                                                  
         CLC   WOODP,6(R3)         SHOULD NOT BE ANY                            
         BNE   WBSG04                                                           
         MVC   P(50),=CL50'WBS GRADE ASS WITH WOOD IN KEY'                      
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    WBSGCTC,=P'1'                                                    
         B     DMXKEEP                                                          
WBSG04   CLC   WOTVP,6(R3)                                                      
         BNE   WBSR10                                                           
         MVC   6(3,R3),WOODP                                                    
         AP    WBSGCTA,=P'1'                                                    
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS GRADE WOTV'                                         
         B     PRT                                                              
WBSG10   CLC   WUHQP,6(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   6(3,R3),WOTVP                                                    
         AP    WBSGCTB,=P'1'                                                    
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'WBS GRADE WUHQ'                                         
         B     PRT                                                              
         EJECT                                                                  
* CHECK OUT TRAFFIC RECORDS *                                                   
         SPACE                                                                  
TRA      CLI   2(R3),X'11'         WILA THAN BUY                                
         BE    DMXKEEP              YES                                         
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
TRA2210  CLC   WOODT,0(R5)         SHOULD NOT BE ANY                            
         BNE   TRA2214                                                          
         CLI   4(R5),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   4(R5),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P+30(3),=C'KEY'                                                  
         AP    TWOOD,=P'1'                                                      
         AP    TRA22C,=P'1'                                                     
         SR    R4,R3               FIND DISP INTO REC                           
         ST    R4,DUB                                                           
         GOTO1 =V(HEXOUT),DMCB,DUB,P+4,4,0,0                                    
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'TRA PAT-WOOD IN REC'                                    
         B     PRT                                                              
TRA2214  CLC   WOTVT,0(R5)                                                      
         BNE   TRA2220                                                          
         CLI   4(R5),C' '                                                       
         BE    *+14                                                             
         CLI   4(R5),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R5),WOODT                                                    
         AP    TRA22A,=P'1'                                                     
         MVI   TRA2240+1,0                                                      
         OC    PATLMSG+9(4),PATLMSG+9                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PATLMSG+9(4),WOTVT                                               
         B     TRA2230                                                          
TRA2220  CLC   WUHQT,0(R5)                                                      
         BNE   TRA2230                                                          
         CLI   4(R5),C' '                                                       
         BE    *+14                                                             
         CLI   4(R5),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R5),WOTVT                                                    
         AP    TRA22B,=P'1'                                                     
         MVI   TRA2240+1,0                                                      
         OC    PATLMSG+14(4),PATLMSG+14                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PATLMSG+14(4),WUHQT                                              
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
         CLC   WOODP,8(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2404                                                          
         MVC   P(50),=CL50'TRAFFIC INS REC WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA24C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2404  CLC   WOTVP,8(R3)                                                      
         BNE   TRA2410                                                          
         MVC   8(3,R3),WOODP                                                    
         AP    TRA24A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR INST REC WOTV'                                       
         B     PRT                                                              
TRA2410  CLC   WUHQP,8(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   8(3,R3),WOTVP                                                    
         AP    TRA24B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR INST REC WUHQ'                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC SHIPPING RECAP RECORDS *                                              
         SPACE                                                                  
TRA25    AP    TRA25CT,=P'1'                                                    
         CLC   WOODP,7(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2504                                                          
         MVC   P(50),=CL50'TRAFFIC SHP REC WITH WOOD IN KEY'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA25C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2504  CLC   WOTVP,7(R3)                                                      
         BNE   TRA2510                                                          
         MVC   7(3,R3),WOODP                                                    
         AP    TRA25A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR SHIP REC WOTV'                                       
         B     PRT                                                              
TRA2510  CLC   WUHQP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WOTVP                                                    
         AP    TRA25B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR SHIP REC WUHQ'                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC STATION ADDRESS RECORDS *                                             
         SPACE                                                                  
TRA28    AP    TRA28CT,=P'1'                                                    
         CLC   WOODT,3(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2804                                                          
         CLI   7(R3),C'A'                                                       
         BE    DMXKEEP                                                          
         CLI   7(R3),C'F'                                                       
         BE    DMXKEEP                                                          
         MVC   P(50),=CL50'TRAFFIC STA ADDR REC WITH WOOD'                      
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA28C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2804  CLC   WOTVT,3(R3)                                                      
         BNE   TRA2810                                                          
         CLI   7(R3),C' '                                                       
         BE    *+14                                                             
         CLI   7(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   3(4,R3),WOODT                                                    
         AP    TRA28A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR STA ADDR WOTV'                                       
         B     PRT                                                              
TRA2810  CLC   WUHQT,3(R3)                                                      
         BNE   DMXKEEP                                                          
         CLI   7(R3),C' '                                                       
         BE    *+14                                                             
         CLI   7(R3),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   3(4,R3),WOTVT                                                    
         AP    TRA28B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR STA ADDR WUHQ'                                       
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC BUY ACTIVITY RECORDS *                                                
         SPACE                                                                  
TRA2E    AP    TRA2ECT,=P'1'                                                    
         CLC   WOODP,7(R3)         SHOULD NOT BE ANY                            
         BNE   TRA2E04                                                          
         MVC   P(50),=CL50'TRAFFIC BUY ACT REC WITH WOOD'                       
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA2EC,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2E04  CLC   WOTVP,7(R3)                                                      
         BNE   TRA2E10                                                          
         MVC   7(3,R3),WOODP                                                    
         AP    TRA2EA,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY ACT WOTV'                                        
         B     PRT                                                              
TRA2E10  CLC   WUHQP,7(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   7(3,R3),WOTVP                                                    
         AP    TRA2EB,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY ACT WUHQ'                                        
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
TRA2F10  CLC   WOODP,2(R6)         SHOULD NOT BE ANY                            
         BNE   TRA2F14                                                          
         MVC   P(50),=CL50'TRAFFIC LABEL LIST REC WITH WOOD'                    
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 =V(HEXOUT),DMCB,(R6),P+50,5,0,0                                  
         MVC   P+60(7),=C'ELEMENT'                                              
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA2FC,=P'1'                                                     
         B     DMXKEEP                                                          
TRA2F14  CLC   WOTVP,2(R6)                                                      
         BNE   TRA2F20                                                          
         MVC   2(3,R6),WOODP                                                    
         AP    TRA2FA,=P'1'                                                     
         MVI   TRA2F40+1,0                                                      
         OC    LBLLMSG+9(4),LBLLMSG+9                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   LBLLMSG+9(4),WOTVT                                               
         B     TRA2F30                                                          
TRA2F20  CLC   WUHQP,2(R6)                                                      
         BNE   TRA2F30                                                          
         MVC   2(3,R6),WOTVP                                                    
         AP    TRA2FB,=P'1'                                                     
         MVI   TRA2F40+1,0                                                      
         OC    LBLLMSG+14(4),LBLLMSG+14                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   LBLLMSG+14(4),WUHQT                                              
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
TRA3110  CLC   WOODP,2(R6)         SHOULD NOT BE ANY                            
         BNE   TRA3114                                                          
         MVC   P(50),=CL50'TRAFFIC STA LIST REC WITH WOOD'                      
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 =V(HEXOUT),DMCB,(R6),P+50,5,0,0                                  
         MVC   P+60(7),=C'ELEMENT'                                              
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA31C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA3114  CLC   WOTVP,2(R6)                                                      
         BNE   TRA3120                                                          
         MVC   2(3,R6),WOODP                                                    
         AP    TRA31A,=P'1'                                                     
         MVI   TRA3140+1,0                                                      
         OC    STALMSG+9(4),STALMSG+9                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   STALMSG+9(4),WOTVT                                               
         B     TRA3130                                                          
TRA3120  CLC   WUHQP,2(R6)                                                      
         BNE   TRA3130                                                          
         MVC   2(3,R6),WOTVP                                                    
         AP    TRA31B,=P'1'                                                     
         MVI   TRA3140+1,0                                                      
         OC    STALMSG+14(4),STALMSG+14                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   STALMSG+14(4),WUHQT                                              
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
         CLC   WOODP,8(R3)         SHOULD NOT BE ANY                            
         BNE   TRA3204                                                          
         MVC   P(50),=CL50'TRAFFIC BUY REC WITH WOOD'                           
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA32C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA3204  CLC   WOTVP,8(R3)                                                      
         BNE   TRA3210                                                          
         MVC   8(3,R3),WOODP                                                    
         AP    TRA32A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY WOTV'                                            
         B     PRT                                                              
TRA3210  CLC   WUHQP,8(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   8(3,R3),WOTVP                                                    
         AP    TRA32B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR BUY WUHQ'                                            
         B     PRT                                                              
         EJECT                                                                  
* TRAFFIC COMML TEXT RECORDS *                                                  
         SPACE                                                                  
TRA35    AP    TRA35CT,=P'1'                                                    
         CLC   WOODP,8(R3)         SHOULD NOT BE ANY                            
         BNE   TRA3504                                                          
         MVC   P(50),=CL50'TRAFFIC COM TXT REC WITH WOOD IN KEY'                
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R3),P,13,0,0                                    
         MVC   P+30(3),=C'KEY'                                                  
         GOTO1 VPRINTER                                                         
         AP    TWOOD,=P'1'                                                      
         AP    TRA35C,=P'1'                                                     
         B     DMXKEEP                                                          
TRA3504  CLC   WOTVP,8(R3)                                                      
         BNE   TRA3510                                                          
         MVC   8(3,R3),WOODP                                                    
         AP    TRA35A,=P'1'                                                     
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR C TXT WOTV'                                          
         B     PRT                                                              
TRA3510  CLC   WUHQP,8(R3)                                                      
         BNE   DMXKEEP                                                          
         MVC   8(3,R3),WOTVP                                                    
         AP    TRA35B,=P'1'                                                     
         BC    0,DMXKEEP                                                        
*                                                                               
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'TR C TXT WUHQ'                                          
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
         MVC   P+60(30),=CL30'WOTV KEY - UPDATED SUBLINE'                       
         GOTO1 =V(HEXOUT),DMCB,(R3),P+30,13,0,0                                 
         GOTO1 VPRINTER                                                         
         B     FIXSX                                                            
         SPACE 3                                                                
* WRITE WOOD STA BILLS TO TAPE *                                                
         SPACE                                                                  
WTSTBL   NTR1                                                                   
         LA    R5,255                                                           
         LA    R6,STBLTBL                                                       
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
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         LA    R5,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(22),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,27(,R3)                                                       
         BCT   R5,DMXEOF10                                                      
         GOTO1 VPRINTER                                                         
         LA    R5,TOTKCTRS                                                      
         LA    R3,TWOOD                                                         
         B     DMXEOF22                                                         
DMXEOF20 CP    0(5,R3),=P'0'                                                    
*        BE    DMXEOF24                                                         
DMXEOF22 MVC   P+5(22),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+27)                                              
         GOTO1 VPRINTER                                                         
DMXEOF24 LA    R3,27(,R3)                                                       
         BCT   R5,DMXEOF20                                                      
         CLOSE (STBILLS)                                                        
         B     DMXIT                                                            
         EJECT                                                                  
STBILLSW DC    C' '                                                             
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
TOTRD    DC    PL5'0',CL22'TOTAL RECS READ'                                     
TBUYCT   DC    PL5'0',CL22'TOT BUYS'                                            
BUYCTA   DC    PL5'0',CL22'BUYS STA WOTV'                                       
BUYCTB   DC    PL5'0',CL22'BUYS STA WUHQ'                                       
BUYCTC   DC    PL5'0',CL22'BUYS STA WOOD'                                       
BUYCTKC  DC    PL5'0',CL22'BUYS STA WOTV SUB CHA'                               
TINVCT   DC    PL5'0',CL22'TOT INV         0B  '                                
INVCTA   DC    PL5'0',CL22'TOT INVS WOTV'                                       
INVCTB   DC    PL5'0',CL22'TOT INVS WUHQ'                                       
INVCTC   DC    PL5'0',CL22'TOT INVS WOOD'                                       
TSIRCT   DC    PL5'0',CL22'TOT SIR         0C  '                                
SIRCTA   DC    PL5'0',CL22'TOT SIRS WOTV'                                       
SIRCTB   DC    PL5'0',CL22'TOT SIRS WUHQ'                                       
SIRCTC   DC    PL5'0',CL22'TOT SIRS WOOD'                                       
TSGRCT   DC    PL5'0',CL22'TOT STA GRP     0D05'                                
SGRCTA   DC    PL5'0',CL22'TOT STA GRP WOTV'                                    
SGRCTB   DC    PL5'0',CL22'TOT STA GRP WUHQ'                                    
SGRCTC   DC    PL5'0',CL22'TOT STA GRP WOOD'                                    
TFLTCT   DC    PL5'0',CL22'TOT FLT         0D0D'                                
FLTCTA   DC    PL5'0',CL22'TOT FLTS WOTV'                                       
FLTCTB   DC    PL5'0',CL22'TOT FLTS WUHQ'                                       
FLTCTC   DC    PL5'0',CL22'TOT SPT FLT WOOD'                                    
TCSPCT   DC    PL5'0',CL22'TOT CSP         0D0D'                                
CSPCTA   DC    PL5'0',CL22'TOT CSPS WOTV'                                       
CSPCTB   DC    PL5'0',CL22'TOT CSPS WUHQ'                                       
CSPCTC   DC    PL5'0',CL22'TOT CAN SPL WOOD'                                    
TPCTCT   DC    PL5'0',CL22'TOT STA PCT     0D42'                                
PCTCTA   DC    PL5'0',CL22'TOT STA PCT WOTV'                                    
PCTCTB   DC    PL5'0',CL22'TOT STA PCT WUHQ'                                    
PCTCTC   DC    PL5'0',CL22'TOT STA PCT WOOD'                                    
TEQUCT   DC    PL5'0',CL22'TOT STA EQU     0D44'                                
EQUCTA   DC    PL5'0',CL22'TOT STA EQU WOTV'                                    
EQUCTB   DC    PL5'0',CL22'TOT STA EQU WUHQ'                                    
EQUCTC   DC    PL5'0',CL22'TOT STA EQU WOOD'                                    
TCSTCT   DC    PL5'0',CL22'TOT COMPET STA  0D47'                                
CSTCTA   DC    PL5'0',CL22'TOT CMP STA WOTV'                                    
CSTCTB   DC    PL5'0',CL22'TOT CMP STA WUHQ'                                    
CSTCTC   DC    PL5'0',CL22'TOT CMP STA WOOD'                                    
TCSOCT   DC    PL5'0',CL22'TOT CSO PROGRA  0D49'                                
CSOCTA   DC    PL5'0',CL22'TOT CSO PRG WOTV'                                    
CSOCTB   DC    PL5'0',CL22'TOT CMP PRG WUHQ'                                    
CSOCTC   DC    PL5'0',CL22'TOT CMP PRG WOOD'                                    
TCDOCT   DC    PL5'0',CL22'TOT CSO DEM OVR 0D57'                                
CDOCTA   DC    PL5'0',CL22'TOT CSO DEM WOTV'                                    
CDOCTB   DC    PL5'0',CL22'TOT CSO DEM WUHQ'                                    
CDOCTC   DC    PL5'0',CL22'TOT CMP DEM WOOD'                                    
TCSCCT   DC    PL5'0',CL22'TOT CSO COMMNTS 0D58'                                
CSCCTA   DC    PL5'0',CL22'TOT CSO CMT WOTV'                                    
CSCCTB   DC    PL5'0',CL22'TOT CMP CMT WUHQ'                                    
CSCCTC   DC    PL5'0',CL22'TOT CMP CMT WOOD'                                    
TSLSCT   DC    PL5'0',CL22'TOT STA LIST    0D5B'                                
SLSCTA   DC    PL5'0',CL22'TOT STA LST WOTV'                                    
SLSCTB   DC    PL5'0',CL22'TOT STA LST WUHQ'                                    
SLSCTC   DC    PL5'0',CL22'TOT STA LST WOOD'                                    
TNWSCT   DC    PL5'0',CL22'TOT NWS HDR     0D67'                                
NWSCTA   DC    PL5'0',CL22'TOT NWS HDR WOTV'                                    
NWSCTB   DC    PL5'0',CL22'TOT NWS HDR WUHQ'                                    
NWSCTC   DC    PL5'0',CL22'TOT NWS HDR WOOD'                                    
TPRDEXCT DC    PL5'0',CL22'TOT PRD EXCL    0D70'                                
PRDEXCTA DC    PL5'0',CL22'TOT PRD EXCL WOTV'                                   
PRDEXCTB DC    PL5'0',CL22'TOT PRD EXCL WUHQ'                                   
PRDEXCTC DC    PL5'0',CL22'TOT PRD EXCL WOOD'                                   
TSTATUCT DC    PL5'0',CL22'TOT STATUS      0D71'                                
STATUCTA DC    PL5'0',CL22'TOT STATUS   WOTV'                                   
STATUCTB DC    PL5'0',CL22'TOT STATUS   WUHQ'                                   
STATUCTC DC    PL5'0',CL22'TOT STATUS   WOOD'                                   
TSLHCT   DC    PL5'0',CL22'TOT STA LOC HDR 0D72'                                
SLHCTA   DC    PL5'0',CL22'TOT STA LOCK WOTV'                                   
SLHCTB   DC    PL5'0',CL22'TOT STA LOCK WUHQ'                                   
SLHCTC   DC    PL5'0',CL22'TOT STA LOCK WOOD'                                   
TSLTCT   DC    PL5'0',CL22'TOT STATION LST 0D75'                                
SLTCTA   DC    PL5'0',CL22'TOT STA LST  WOTV'                                   
SLTCTB   DC    PL5'0',CL22'TOT STA LST  WUHQ'                                   
SLTCTC   DC    PL5'0',CL22'TOT STA LST  WOOD'                                   
SLTCTD   DC    PL5'0',CL22'TOT STA LST  WOTV ELE'                               
SLTCTE   DC    PL5'0',CL22'TOT STA LST  WUHQ ELE'                               
SLTCTF   DC    PL5'0',CL22'TOT STA LST  WOOD ELE'                               
TCLSTACT DC    PL5'0',CL22'TOT CLEARED STA 0D76'                                
CLSTACTA DC    PL5'0',CL22'TOT CLST WOTV'                                       
CLSTACTB DC    PL5'0',CL22'TOT CLST WUHQ'                                       
CLSTACTC DC    PL5'0',CL22'TOT CLST WOOD'                                       
TBILLCT  DC    PL5'0',CL22'TOT BILLS       0E01'                                
BILLCTA  DC    PL5'0',CL22'STA BILLS WOTV'                                      
BILLCTM  DC    PL5'0',CL22'STA BILLS WOTV MERGED'                               
BILLCTB  DC    PL5'0',CL22'STA BILLS WUHQ'                                      
BILLCTC  DC    PL5'0',CL22'STA BILLS WOOD'                                      
TWBSRCT  DC    PL5'0',CL22'WBS REG         0E14'                                
WBSRCTA  DC    PL5'0',CL22'WBS REG   WOTV'                                      
WBSRCTB  DC    PL5'0',CL22'WBS REG   WUHQ'                                      
WBSRCTC  DC    PL5'0',CL22'WBS REG   WOOD'                                      
TWBSGCT  DC    PL5'0',CL22'WBS GRADE ASS   0E16'                                
WBSGCTA  DC    PL5'0',CL22'WBS GRADE WOTV'                                      
WBSGCTB  DC    PL5'0',CL22'WBS GRADE WUHQ'                                      
WBSGCTC  DC    PL5'0',CL22'WBS GRADE WOOD'                                      
TESTCT   DC    PL5'0',CL22'TOT EST STA BK'                                      
ESTCTA   DC    PL5'0',CL22'TOT EST STA BK WOTV'                                 
ESTCTB   DC    PL5'0',CL22'TOT EST STA BK WUHQ'                                 
ESTCTC   DC    PL5'0',CL22'TOT EST STA BK WOOD'                                 
TRA22CT  DC    PL5'0',CL22'TOT TRAF PATTERNS 0A22'                              
TRA22A   DC    PL5'0',CL22'TOT TRAF PAT WOTV'                                   
TRA22B   DC    PL5'0',CL22'TOT TRAF PAT WUHQ'                                   
TRA22C   DC    PL5'0',CL22'TOT TRAF PAT WOOD'                                   
TRA24CT  DC    PL5'0',CL22'TOT TRAF INST REC 0A24'                              
TRA24A   DC    PL5'0',CL22'TOT TRAF INST REC WOTV'                              
TRA24B   DC    PL5'0',CL22'TOT TRAF INST REC WUHQ'                              
TRA24C   DC    PL5'0',CL22'TOT TRAF INST REC WOOD'                              
TRA25CT  DC    PL5'0',CL22'TOT TRAF SHIP REC'                                   
TRA25A   DC    PL5'0',CL22'TOT TRAF SHIP REC WOTV'                              
TRA25B   DC    PL5'0',CL22'TOT TRAF SHIP REC WUHQ'                              
TRA25C   DC    PL5'0',CL22'TOT TRAF SHIP REC WOOD'                              
TRA28CT  DC    PL5'0',CL22'TOT TRAF STA ADDR RECS'                              
TRA28A   DC    PL5'0',CL22'TOT TRAF STA ADDR WOTV'                              
TRA28B   DC    PL5'0',CL22'TOT TRAF STA ADDR WUHQ'                              
TRA28C   DC    PL5'0',CL22'TOT TRAF STA ADDR WOOD'                              
TRA2ECT  DC    PL5'0',CL22'TOT TRAF BUY ACT'                                    
TRA2EA   DC    PL5'0',CL22'TOT TRAF BUY ACT WOTV'                               
TRA2EB   DC    PL5'0',CL22'TOT TRAF BUY ACT WUHQ'                               
TRA2EC   DC    PL5'0',CL22'TOT TRAF BUY ACT WOOD'                               
TRA2FCT  DC    PL5'0',CL22'TOT TR LABEL LIST ACT'                               
TRA2FA   DC    PL5'0',CL22'TOT TR LABEL LIST WOTV'                              
TRA2FB   DC    PL5'0',CL22'TOT TR LABEL LIST WUHQ'                              
TRA2FC   DC    PL5'0',CL22'TOT TR LABEL LIST WOOD'                              
TRA31CT  DC    PL5'0',CL22'TOT TRAF STA LIST'                                   
TRA31A   DC    PL5'0',CL22'TOT TRAF STA LIST WOTV'                              
TRA31B   DC    PL5'0',CL22'TOT TRAF STA LIST WUHQ'                              
TRA31C   DC    PL5'0',CL22'TOT TRAF STA LIST WOOD'                              
TRA32CT  DC    PL5'0',CL22'TOT TRAF BUYS'                                       
TRA32A   DC    PL5'0',CL22'TOT TRAF BUYS WOTV'                                  
TRA32B   DC    PL5'0',CL22'TOT TRAF BUYS WUHQ'                                  
TRA32C   DC    PL5'0',CL22'TOT TRAF BUYS WOOD'                                  
TRA35CT  DC    PL5'0',CL22'TOT TRAF C TXT 0A35'                                 
TRA35A   DC    PL5'0',CL22'TOT TRAF C TX WOTV'                                  
TRA35B   DC    PL5'0',CL22'TOT TRAF C TX WUHQ'                                  
TRA35C   DC    PL5'0',CL22'TOT TRAF C TX WOOD'                                  
TOTCTRS  EQU   (*-TOTRD)/27                                                     
TWOOD    DC    PL5'0',CL22'TOT WOOD REC'                                        
TPSTABLS DC    PL5'0',CL22'STAT BILLS ON TAPE'                                  
TOTKCTRS EQU   (*-TWOOD)/27                                                     
WORK     DS    CL64                                                             
COMPKEY  DC    XL10'00'                                                         
ELCODE   DS    XL1                                                              
RTITLE   DC    CL100'CHANGE STATION CALL LETTERS WOTV TO WOOD AND WUHQ T        
               TO WOTV'                                                         
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
BUYKEYS  DC    XL13'FFC58EFF058DBD661201010100',X'09' SPOT 1                    
NBUYS    EQU   (*-BUYKEYS)/14                                                   
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
       ++INCLUDE SPGENCLST                                                      
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
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPEXTSTA2 05/01/02'                                      
         END                                                                    
