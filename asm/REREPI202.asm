*          DATA SET REREPI202  AT LEVEL 125 AS OF 05/01/02                      
*PHASE REI202A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPI202A (REI202A) --- KATZ STATION MOVER'                    
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPI202A -- KATZ STATION MOVER                          *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JAN06/96 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = VALUE IN BYTE:)     *            
*     QUESTOR+0   =   Y  -  SUPPRESS RECORD DETAILS OUTPUT         *            
*     QUESTOR+1   =                                                *            
*     QUESTOR+2   =                                                *            
*     QUESTOR+3   =                                                *            
*     QUESTOR+4   =                                                *            
*     QUESTOR+5   =                                                *            
*     QUESTOR+6   =                                                *            
*     QUESTOR+7   =                                                *            
*     QUESTOR+8   =                                                *            
*     QUESTOR+9   =                                                *            
*     QUESTOR+10  =                                                *            
*     QUESTOR+11  =                                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REI202   CSECT                                                                  
         NMOD1 0,**REI2**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAINEXIT                                                         
         DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         MVC   P+1(23),=C'BEGINNING STATION MOVER'                              
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,STALIST          SET A(RECORD TYPE TABLE)                     
MAIN0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    MAIN1000            YES - DO BUY RECORDS                         
         GOTO1 RECPROC,DMCB,(R2)                                                
*                                  PROCESS THE RECORD TYPE                      
         LA    R2,LSTALIST(R2)     BUMP TO NEXT ENTRY                           
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN1000 EQU   *                                                                
         BAS   RE,BUYPROC          PROCESS THE BUY RECORDS                      
*                                     FOR ORDERS TRANSFERRED                    
         BAS   RE,DISPTOTS         DISPLAY RUN TOTALS                           
         CLOSE FILOUTA                                                          
         CLOSE FILOUTB                                                          
MAINEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         DS    0F                                                               
*                                                                               
*   STALIST:  TABLE OF STATIONS TO BE COPIED TO NEW FILE                        
*        POS 0-4    =    STATION CALL LETTERS + MEDIA                           
*        POS 5-7    =    SPARE (ALIGNMENT FILLER)                               
*        POS 8-9    =    OLD REP CODE                                           
*        POS 10-11  =    NEW REP CODE                                           
*        POS 12-15  =    RECORD COUNTER FOR STATION                             
*        POS 16-19  =    RECORD COUNTER (SPARE)                                 
*                                                                               
*                                                                               
STALIST  EQU   *                                                                
*               +0       +5    +8   +10   +12  +16                              
STALIST1 DC    C'WQBAF',AL3(0),C'KU',C'KF',F'0',F'0'                            
STALIST2 DC    C'WQBAA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'WINDA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'WOJOF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KSABF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KUNOA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KESSA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KBNAA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KBNAF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KHOTA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KXMXF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KLATA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KJBZF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KGBTA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KIWWF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KELFF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KTROA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KCLBA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'WTELA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KCORA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KDIFA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'XHKYF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KBRGF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KDBKF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KLOKA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KIDIF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'KTAPA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'WQBNA',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'XLTNF',AL3(0),C'KU',C'KF',F'0',F'0'                            
         DC    C'TKEYA',AL3(0),C'KU',C'K4',F'0',F'0'                            
         DC    C'POPRA',AL3(0),C'KU',C'K4',F'0',F'0'                            
         DC    X'0000'                                                          
LSTALIST EQU   STALIST2-STALIST1                                                
         EJECT                                                                  
*                                                                               
*   RECPROC:  CYCLE THROUGH STATION FOR ALL CONTRACTS.                          
*        CHECK IF CONTRACT EXISTS ON TARGET REP.  IF IT DOES,                   
*           INDICATE IN REPORT, AND SKIP ORDER.  THESE WILL HAVE                
*           TO BE ADDED AS 'NEW' ORDERS, AND DELETED MANUALLY.                  
*        IF CONTRACT DOES NOT EXIST ON THE TARGET REP,                          
*            WRITE THE CONTRACT TO THE FILE, DELETE THE CONTRACT                
*            FROM THE OLD REP.                                                  
*                                                                               
RECPROC  NTR1                                                                   
         L     R2,0(R1)            RESET A(STATION IN PROGRESS)                 
         XC    KEY,KEY                                                          
         MVI   KEY,X'CC'           SET UP X'CC' KEY: STATION HIGH               
         MVC   KEY+3(5),0(R2)      INSERT STATION CALL LETTERS                  
         MVC   KEY+1(2),8(R2)      INSERT ORIGINAL REP CODE                     
         GOTO1 HIGH                                                             
         B     RECP0040                                                         
RECP0020 EQU   *                                                                
         GOTO1 SEQ                                                              
RECP0040 EQU   *                                                                
         CLC   KEY(8),KEYSAVE      KEY FOUND? THRU STATION                      
         BNE   RECP1000            KEYS NOT EQUAL THRU REP:                     
         BAS   RE,CHECKNEW         CHECK FOR EXISTENCE ON NEW REP               
         BNZ   RECP0020            KEY EXISTS:  SKIP PAST IT                    
*                                     STATION FINISHED                          
         BAS   RE,GETRECRD         RETRIEVE RECORD                              
         L     RF,12(R2)           INCREMENT STATION COUNTER                    
         LA    RF,1(RF)                                                         
         ST    RF,12(R2)           RESTORE                                      
         L     RF,CONXFER          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONXFER          RESTORE                                      
         L     RF,GRANDTOT         INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,GRANDTOT         RESTORE                                      
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C'TRANSFERRED:'                                         
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
*                                  INSERT CONTRACT NUMBER                       
         MVC   P+25(5),RCONKSTA    INSERT STATION LETTERS                       
         MVC   P+32(2),RCONKREP    INSERT OLD REP CODE                          
         MVC   P+36(2),=C'TO'                                                   
         MVC   P+40(2),10(R2)      INSERT NEW REP CODE                          
         GOTO1 REPORT                                                           
         XC    WORK,WORK                                                        
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+0(4),WORK+15                                                
*                                      REVERSE THE COMPLIMENT                   
         PACK  WORK+0(1),WORK+18(1)                                             
         PACK  WORK+1(1),WORK+17(1)                                             
         PACK  WORK+2(1),WORK+16(1)                                             
         PACK  WORK+3(1),WORK+15(1)                                             
*                                                                               
*   TEST                                                                        
*        MVC   P+1(16),=C'RECP0040: OLD/NEW'                                    
*        MVC   P+20(4),RCONKCON    ORIGINAL NUMBER                              
*        MVC   P+26(4),WORK        REVERSED NUMBER                              
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         L     RF,ANXTAREA         LOAD A(NEXT AVAILABLE SLOT)                  
         L     R3,ANXTAREA         SAVE FOR DISPLAY                             
         MVC   0(4,RF),WORK        INSERT CONTRACT INTO TABLE                   
         MVC   4(2,RF),8(R2)       INSERT ORIGINAL   REP CODE                   
         MVC   6(2,RF),10(R2)      INSERT NEW TARGET REP CODE                   
         LA    RF,08(RF)           BUMP TO NEXT AVAILABLE SLOT                  
         ST    RF,ANXTAREA         SAVE NEXT AVAILABLE SLOT                     
         L     RF,TABBUYCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TABBUYCT         INCREMENT TABLE COUNTER                      
*        GOTO1 HEXOUT,DMCB,RCONKCON,P+2,4,=C'TOG'                               
*        GOTO1 HEXOUT,DMCB,WORK,P+12,4,=C'TOG'                                  
*        MVC   P+24(6),0(R3)       INSERT TABLE ENTRY                           
*        GOTO1 REPORT                                                           
         CLI   QUESTOR+0,C'Y'      DISPLAY  DETAILS?                            
         BNE   RECP0280            NO  - SKIP THEM                              
         CLC   CONXFER,=F'050'     ONLY PRINT FIRST 050 CONTRACTS               
         BH    RECP0280                                                         
RECP0240 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C'INPUT RECORD'                                         
         EDIT  CONXFER,(4,P+25)                                                 
         GOTO1 REPORT                                                           
         LA    R4,RECORD           SET RECORD ADDR                              
         ZICM  RF,RECORD+27,2      SET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
RECP0280 EQU   *                                                                
         MVC   RECORD+2(2),10(R2)  INSERT REPLACEMENT REP ID                    
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         LA    RE,RECORD                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   REC-4(2),RECORD+27  INSERT LENGTH                                
         GOTO1 OUTRECS             ADD TO OUTPUT FILE                           
         MVC   RECORD+2(2),8(R2)   RE-INSERT ORIGINAL REP ID                    
         OI    RECORD+29,X'80'     TURN ON 'DELETE' BIT                         
***>>>   OI    RECORD+29,X'01'     TURN ON 'CLOSED' BIT                         
         L     RF,REWRCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,REWRCTR                                                       
         CLI   QUESTOR+0,C'Y'      DISPLAY  DETAILS?                            
         BNE   RECP0360            NO  - SKIP THEM                              
         CLC   CONXFER,=F'050'     ONLY PRINT FIRST 050 CONTRACTS               
         BH    RECP0360                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(23),=C'CONTRACT RECORD REWRITE'                              
         EDIT  CONXFER,(4,P+25)                                                 
         GOTO1 REPORT                                                           
         LA    R4,RECORD           SET RECORD ADDR                              
         ZICM  RF,RECORD+27,2      SET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
RECP0360 EQU   *                                                                
         CLI   QOPTION1,C'U'       UPDATE RUN?                                  
         BNE   RECP0480            NO                                           
         BAS   RE,PUTRECRD         REWRITE THE RECORD 'CLOSED'                  
RECP0480 EQU   *                                                                
         B     RECP0020            GO BACK FOR NEXT                             
RECP1000 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHECKNEW:  ACCESSES TARGET REP TO SEE IF ORDER NUMBER IN USE.               
*       IF IT IS, RETURNS CC NOT ZERO AFTER PUTTING MESSAGE TO                  
*       REPORT, AND ORDER IS SKIPPED (NOT TRANSFERRED).                         
*       IN ALL CASES, KEY IS RE-READ FOR RESTART                                
*                                                                               
CHECKNEW NTR1                                                                   
         MVC   KEYSTART,KEY        SAVE KEY FOR RESTART                         
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY,X'8C'           INSERT RECORD KEY TYPE                       
         MVC   KEY+21(2),10(R2)    INSERT TARGET REP CODE                       
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),KEYSTART+23(4)                                        
*                                  INSERT CON# FROM ORIGINAL KEY                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+0(4),WORK+15                                                
*                                      REVERSE THE COMPLIMENT                   
         PACK  WORK+0(1),WORK+18(1)                                             
         PACK  WORK+1(1),WORK+17(1)                                             
         PACK  WORK+2(1),WORK+16(1)                                             
         PACK  WORK+3(1),WORK+15(1)                                             
         MVC   KEY+23(4),WORK      INSERT CONTRACT NUMBER                       
*                                                                               
*   TEST                                                                        
*        MVC   P+1(16),=C'CHECKNEW: OLD/NEW'                                    
*        MVC   P+20(4),KEYSTART+23 ORIGINAL NUMBER                              
*        MVC   P+26(4),KEY+23      REVERSED NUMBER                              
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 HIGH                ACCESS THE KEY ON TARGET FILE                
         CLC   KEY,KEYSAVE         KEY ON FILE?                                 
         BNE   CHCK0200            NO  - PROCEED                                
         MVC   P+1(12),=C'NOT XFERRED:'                                         
         MVC   P+16(2),KEYSTART+1  INSERT REP CODE                              
         GOTO1 HEXOUT,DMCB,KEYSTART+23,P+20,4,=C'TOG'                           
*                                  INSERT CONTRACT NUMBER                       
         GOTO1 REPORT                                                           
         MVC   KEY,KEYSTART        RESTART KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CHCK0400            EXIT                                         
CHCK0200 EQU   *                                                                
         MVC   KEY,KEYSTART        RESTART KEY                                  
         GOTO1 HIGH                                                             
         SR    R0,R0               SET CC = ZERO                                
CHCK0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  BUYPROC:  ACCESS THE CONTRACT NUMBER TABLE, RETURN AND PROCESS               
*        ALL BUYS FOR CONTRACTS INVOLVED.                                       
*                                                                               
BUYPROC  NTR1                                                                   
         L     R2,ACONAREA                                                      
BUYP0010 EQU   *                                                                
         OC    0(4,R2),0(R2)       END OF TABLE?                                
         BZ    BUYP2000            YES - FINISHED                               
         L     RF,BUYCONTS         INCREMENT BUY CONTRACTS                      
         LA    RF,1(RF)                                                         
         ST    RF,BUYCONTS                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           INSERT RECORD TYPE                           
         MVC   KEY+16(2),4(R2)     INSERT ORIGINAL REP CODE                     
         MVC   KEY+18(4),0(R2)     INSERT CONTRACT NUMBER AS                    
*                                     9'S COMP, REVERSED                        
         GOTO1 HIGH                                                             
         B     BUYP0040                                                         
BUYP0020 EQU   *                                                                
         GOTO1 SEQ                                                              
BUYP0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH CONTRACT NUMBER?            
         BE    BUYP0080            YES - PROCESS IT                             
         LA    R2,8(R2)            NO  - BUMP TO NEXT SLOT                      
*                                     GO BACK FOR NEXT CONTRACT                 
         B     BUYP0010                                                         
BUYP0080 EQU   *                                                                
         BAS   RE,GETRECRD         RETRIEVE RECORD                              
         L     RF,GRANDTOT         INCREMENT TYPE COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,GRANDTOT         RESTORE                                      
         L     RF,BUYXFER          INCREMENT BUY COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYXFER                                                       
         CLI   QUESTOR+0,C'Y'      DISPLAY  DETAILS?                            
         BNE   BUYP0120            NO  - SKIP THEM                              
         CLC   BUYXFER,=F'250'     ONLY PRINT FIRST 250 BUYS                    
         BH    BUYP0120            DON'T DISPLAY - GO BACK                      
         GOTO1 REPORT                                                           
         MVC   P+1(14),=C'BUY I/P RECORD'                                       
         EDIT  BUYXFER,(4,P+25)                                                 
         GOTO1 REPORT                                                           
         LA    R4,RECORD           SET RECORD ADDR                              
         ZICM  RF,RECORD+27,2      SET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
BUYP0120 EQU   *                                                                
         MVC   RECORD+16(2),6(R2)  INSERT REPLACEMENT REP ID                    
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         LA    RE,RECORD                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   REC-4(2),RECORD+27  INSERT LENGTH                                
         GOTO1 OUTRECS2            ADD TO OUTPUT FILE                           
         MVC   RECORD+16(2),4(R2)  INSERT ORIGINAL    REP ID                    
         OI    RECORD+29,X'80'     TURN ON 'DELETE' BIT                         
***>>>   OI    RECORD+29,X'01'     TURN ON 'CLOSED' BIT                         
         L     RF,REWRCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,REWRCTR                                                       
         CLI   QUESTOR+0,C'Y'      DISPLAY  DETAILS?                            
         BNE   BUYP0180            NO  - SKIP THEM                              
         CLC   BUYXFER,=F'250'     ONLY PRINT FIRST 250 BUYS                    
         BH    BUYP0180            DON'T DISPLAY - GO BACK                      
         GOTO1 REPORT                                                           
         MVC   P+1(13),=C'CLOSED BUY   '                                        
         EDIT  BUYXFER,(4,P+25)                                                 
         GOTO1 REPORT                                                           
         LA    R4,RECORD           SET RECORD ADDR                              
         ZICM  RF,RECORD+27,2      SET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
BUYP0180 EQU   *                                                                
         CLI   QOPTION1,C'U'       UPDATE RUN?                                  
         BNE   BUYP0200            NO                                           
         BAS   RE,PUTRECRD         REWRITE THE RECORD 'CLOSED'                  
BUYP0200 EQU   *                                                                
         B     BUYP0020            GO BACK FOR NEXT BUY                         
BUYP2000 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
******************************************************************              
*  OUTRECS :  GENERATE OUTFILE ENTRIES FOR CONTRACTS             *              
******************************************************************              
*                                                                               
OUTRECS  NTR1                                                                   
*                                                                               
*   THIS ENTRY ADDS LENGTH OF CONTROL BYTE TO RECORD CONTROL                    
*                                                                               
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
*                                                                               
         CLI   QUESTOR+0,C'Y'      DISPLAY  DETAILS?                            
         BNE   OUTR0280            NO  - SKIP THEM                              
         CLC   CONXFER,=F'050'     ONLY PRINT FIRST 050 CONTRACTS               
         BH    OUTR0280                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(23),=C'CONTRACT RECORD TO TAPE'                              
         EDIT  CONXFER,(4,P+25)                                                 
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
OUTR0280 EQU   *                                                                
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           SAVE IT                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
******************************************************************              
*  OUTRECS2:  GENERATE OUTFILE ENTRIES FOR BUYS                  *              
******************************************************************              
*                                                                               
OUTRECS2 NTR1                                                                   
*                                                                               
*   THIS ENTRY ADDS LENGTH OF CONTROL BYTE TO RECORD CONTROL                    
*                                                                               
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
*                                                                               
         CLI   QUESTOR+0,C'Y'      DISPLAY  DETAILS?                            
         BNE   OREC0160            NO  - SKIP THEM                              
         CLC   BUYXFER,=F'250'     ONLY PRINT FIRST 250 BUYS                    
                                                                                
         BH    OREC0160            DON'T DISPLAY - GO BACK                      
         GOTO1 REPORT                                                           
         MVC   P+1(21),=C'BUY    RECORD TO TAPE'                                
         EDIT  BUYXFER,(4,P+25)                                                 
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
OREC0160 EQU   *                                                                
*                                                                               
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           SAVE IT                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
*  INITIAL:   SET UP ORIGINAL WORKAREAS, ETC                                    
**********************************************************************          
*                                                                               
INITIAL  NTR1                                                                   
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',500000,500000                                   
*                                  GET 500K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ACONAREA,P2         A(CONTRACT NUMBER STORAGE AREA)              
         MVC   ANXTAREA,P2         A(NEXT AVAILABLE SLOT)                       
*                                     AT THIS TIME SIZE IS UNKNOWN              
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA          SET A(IOAREA)                                
         XIT1                                                                   
         EJECT                                                                  
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         EJECT                                                                  
*                                                                               
HIGH     NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQ      NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
PUTRECRD LA    R6,PUTREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               RECORD,(0,DMWORK)                                                
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     MAINEXIT                                                         
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     MAINEXIT                                                         
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
******************************************************************              
*                                                                               
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         LA    R2,STALIST                                                       
DTOT0020 EQU   *                                                                
         CLI   0(R2),0             END OF LIST?                                 
         BZ    DTOT0100            YES - FINISHED DISPLAY                       
         MVC   P+1(5),0(R2)        INSERT STATION COUNTER                       
         L     R3,12(R2)           LOAD COUNTER VALUE                           
         EDIT  (R3),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         LA    R2,LSTALIST(R2)     BUMP TO NEXT ENTRY                           
         B     DTOT0020            GO BACK FOR NEXT                             
DTOT0100 EQU   *                                                                
         MVC   P+1(24),=C'TOTAL RECORDS PROCESSED:'                             
         EDIT  GRANDTOT,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS TRANSFERRED  :'                             
         EDIT  CONXFER,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS TABLED       :'                             
         EDIT  TABBUYCT,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUY CONTRACTS SCANNED  :'                             
         EDIT  BUYCONTS,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUYS      TRANSFERRED  :'                             
         EDIT  BUYXFER,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL RECORDS WRITTEN  :'                             
         EDIT  PUTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL RECORDS "CLOSED" :'                             
         EDIT  REWRCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*    WORK SPACE, ETC.                                                           
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
ACONAREA DS    A                   CONTRACT NUMBER STORAGE AREA                 
ANXTAREA DS    A                   NEXT AVAILABLE SLOT                          
LBLDAREA DS    F                                                                
PUTCTR   DS    F                                                                
CONXFER  DS    F                                                                
BUYXFER  DS    F                                                                
TABBUYCT DS    F                                                                
BUYCONTS DS    F                                                                
REWRCTR  DS    F                                                                
GRANDTOT DS    F                                                                
AIOAREA  DS    A                                                                
KEYSTART DS    CL27                KEY SAVE FOR RESTART                         
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT    RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY         RECORD                           
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              RECORD FOR OUTPUT                            
*                                                                               
ELCODE   DS    CL1                                                              
         EJECT                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
*  INCLUDE REGENCON                CONTRACT    RECORD                           
*  INCLUDE REGENBUY                BUY         RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125REREPI202 05/01/02'                                      
         END                                                                    
