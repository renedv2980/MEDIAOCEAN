*          DATA SET REROM03    AT LEVEL 073 AS OF 05/27/03                      
*PHASE T83F03C                                                                  
*&&      SET   TT=N,T2=N                                                        
*                                                                               
         TITLE 'T83F03 - REROM03 - DARE UNTOUCHED ORDER PRINT OUT'              
***********************************************************************         
*                                                                     *         
*  REROM03 (T83F03) --- DARE INBOX UNTOUCHED ORDER PRINT OUT          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
* 14JAN03  HQ   HAPPY BIRTHDAY                                        *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T83F03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLQ,*T83F03*,R7,CLEAR=YES                                     
         LR    R5,RC                                                            
         USING MYAREAD,R5                                                       
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RD,BASERD                                                        
*                                                                               
         XC    MYCOUNT,MYCOUNT                                                  
         XC    MYFLAG,MYFLAG                                                    
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BNO   MAIN0040            NO  -                                        
         LA    RF,SBRPCDSX         SET A(FIRST SUBREP CODE)                     
         ST    RF,ASUBREPX         SAVE AS SUBREP IN PROGRESS                   
         OC    SBRPCDSX(16),SBRPCDSX     SUBREP LIST FILLED IN?                 
         BNZ   MAIN0040            YES - DON'T DO IT AGAIN.                     
         GOTOR ISMAST,DMCB,1                                                    
         DROP  R2                                                               
*                                                                               
MAIN0040 EQU   *                                                                
         CLC   =C'K*Y',DE7RQTR     SPECIAL REQUESTOR OPTION?                    
         BNE   MAIN0060                                                         
         CLI   MODE,LISTRECS       ONLY DO ON 'LISTRECS'                        
         BNE   EXIT                                                             
         GOTOR RESETKEY                                                         
         BZ    KEYRESET            NO ERROR                                     
         L     R2,DUB              SET A(CURSOR ADDRESS)                        
         CLI   DUB+4,1             ERROR TRANSFER: MISSING REC?                 
         BE    MISSREC                                                          
         CLI   DUB+4,2             ERROR TRANSFER: NO AGY HEADER                
         BE    MISSHDR                                                          
         DC    H'0'                UNRECOGNIZED RETURN                          
         B     EXIT                                                             
MAIN0060 EQU   *                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       REPORT?                                      
         BE    VKEY                                                             
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
*                                                                               
         GOTOR SUBVKEY                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         B     LR                  ONLINE                                       
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE                                      
         BE    LR                  ONLY GO LR WHEN IT IS PRINTREP MODE          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
         GOTOR LRADDR                                                           
LR650010 EQU   *                                                                
         CLI   DE7HDLNH+5,0        CONTRACT # ENTERED?                          
         BE    LR90                NO                                           
*                                                                               
         LR    RF,RA               YES                                          
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         CLC   =C'O=',DE7HDLN      AGENCY ORDER SOUGHT?                         
         BE    LR90                YES - REGULAR PROCESSING                     
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BNO   LR90                NO  - REGULAR PROCESSING                     
*                                                                               
         DROP  RF                                                               
*                                                                               
LR650020 EQU   *                                                                
         GOTOR CYCLCON,DMCB,(RC)                                                
*                                                                               
         CLI   HALF,X'FF'          FINISHED?                                    
         BE    LRX                 YES - END OF DISPLAY                         
         CLI   HALF,X'00'          AGENCY ORDER FOUND?                          
         BE    LR210               YES - DISPLAY IT                             
*                                                                               
*   LR210    MAY OR MAY NOT BE THE CORRECT ENTRY POINT FOR DISPLAY              
*                                                                               
         DC    H'0'                UNIDENTIFIED RETURN                          
*&&DO                                                                           
LR80     DS    0H                                                               
         CLC   =C'BRAND',CONREC                                                 
         BE    LR90                                                             
         TM    CTLRFLG1,CF1BRDQ                                                 
         BO    LR90                                                             
         OC    SELCTKEY(27),SELCTKEY                                            
         BZ    LR90                                                             
         MVC   KEY,SELCTKEY                                                     
         MVC   REDITYPE,KEY+1      RESET SUBKEY TYPE                            
         LA    R3,KEY              RESET R3 FOR ADDRESSABILITY IN LR100         
         XC    SELCTKEY,SELCTKEY                                                
         MVI   DUB+1,1             TEST INDICATOR                               
         B     LR100                                                            
*&&                                                                             
*                                                                               
LR90     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
*                                                                               
         NI    CTLRFLG1,X'FF'-CF1BRDQ                                           
         CLI   KEY,X'41'           KEY ALREADY SET TO DARE REC?                 
         BE    LR900020            YES                                          
         CLI   KEY,X'51'           KEY ALREADY SET TO DARE REC?                 
         BNE   LR900040            NO                                           
LR900020 EQU   *                                                                
         MVC   KEY(27),SAVRADKY    YES - RESET IT TO RADKEY                     
         MVC   KEYLOOK,KEY         SAVE CHARACTERISTICS OF KEY                  
         LA    R3,KEY              RESET A(KEY) FOR USING                       
         GOTO1 HIGH                READ (ACTUALLY, REREAD) THIS KEY             
         MVI   DMINBTS,0                                                        
         MVI   RDUPDATE,C'N'                                                    
***      GOTO1 SEQ                 READ THE NEXT KEY FOR RESTART                
         B     LR110                                                            
LR900040 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
         MVI   RED01TYP,X'D0'      SET TO 'MASTER/UNWIRED'                      
*                                                                               
*   FOR THE TIME BEING, MASTER IDENTIFICATION IS HARD-CODED.  WE CAN            
*        MAKE IT NEEDLESSLY COMPLICATED AT A LATER DATE.                        
*                                                                               
         CLC   =C'MR',AGENCY       KATZ TV?                                     
         BE    LR98                YES                                          
         CLC   =C'K3',AGENCY       KATZ RADIO?                                  
         BE    LR98                YES                                          
         CLC   =C'IR',AGENCY       INTEREP?                                     
         BE    LR98                YES                                          
         CLC   =C'MS',AGENCY       TEST?                                        
         BE    LR98                YES                                          
         MVI   RED01TYP,X'D1'      NO  - SET TO 'SPOT'                          
         ZIC   RF,REDITYPE         BUMP KEY TYPE BY 1                           
         LA    RF,1(RF)                                                         
         STC   RF,REDITYPE                                                      
         CLI   DE7UNW,C'Y'         SPECIAL OVERRIDE TO UNWIRED?                 
         BNE   LR98                NO  - PROCEED WITH KEY AS SET                
         MVI   RED01TYP,X'D0'      NO  - SET TO 'UNWIRED' - NOW IT'S            
*                                     'UNWIRED/NON-MASTER'                      
LR98     EQU   *                                                                
         MVC   RED01MRP,AGENCY     INSERT AGENCY OF SIGNON                      
         CLI   RED01TYP,X'D0'      MASTER REP IN PROGRESS?                      
         BNE   LR100               NO  - DON'T OVERRIDE SUB REP                 
*                                                                               
         OC    COMPREP,COMPREP     YES - COMPANY CODE ENTERED?                  
         BZ    LR100               NO  - LEAVE ALONE                            
         MVC   RED01MRP,COMPREP    YES - INSERT COMPANY REP INTO KEY            
         CLC   =C'UNW',DE7RQTR     SPECIAL OVERRIDE TO UNWIRED?                 
         BE    LR100               YES - REDITYPE ALREADY ADVANCED              
         ZIC   RF,REDITYPE         BUMP KEY TYPE BY 1 TO SUB REP                
         LA    RF,1(RF)                                                         
         STC   RF,REDITYPE                                                      
LR100    DS    0H                                                               
         MVC   RED01TYP+1(1),REDITYPE                                           
*                                  SET SUBKEY TYPE                              
         MVC   KEYLOOK,RED01KEY    SAVE CHARACTERISTICS OF KEY                  
LR100020 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'A'                                                       
*   TEST END                                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
LR110    DS    0H                                                               
         CLI   CONFILT,C'Y'        WAS CON# REQUESTED FOUND?                    
         BE    LRX                 YES - NO FURTHER SEARCH                      
         CLC   KEYLOOK,KEY         SAME KEY TYPE/REP?                           
         BNE   LRX                 NO  - FINISHED SCAN                          
*                                                                               
         GOTOR RECDATCK                                                         
         BNZ   LRSEQ               FILTERED OUT BY RECV DATE                    
*                                                                               
         GOTOR INBOXCHK,DMCB,KEYDEFTB                                           
         BNZ   LRSEQ               FILTERED OUT BY INBOX CHECK                  
*                                                                               
         GOTOR STATNCHK,DMCB,KEYDEFTB                                           
         BNZ   LRSEQ               FILTERED OUT BY STATION CHECK                
* TESTING                                                                       
*        CLC   0(4,RF),=X'23460016'                                             
*        BNE   *+6                                                              
*        DC    H'0'                                                             
* TESTING                                                                       
*                                                                               
         MVI   PENDCF,0            CLEAR FLAG                                   
*                                                                               
         CLI   DE7HDLNH+5,0        ANY CON # ENTERED?                           
         BE    LR115100            NO  -                                        
         CLC   =C'O=',DE7HDLN      AGENCY ORDER SOUGHT?                         
         BE    LR115060            YES - REGULAR PROCESSING                     
         OC    CDARNUM,CDARNUM     SKIP IF THERE ARE NONE                       
         BZ    LRX                                                              
         LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
LR115020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    LR115040            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     LR115020            GO BACK FOR NEXT                             
LR115040 EQU   *                                                                
         ZIC   RE,DORDER#(RF)      GET DISPLACEMENT TO ORDER NUMBER             
         LA    RF,KEY              SET A(KEY IN PROGRESS)                       
         AR    RF,RE               DISPLACE TO ORDER NUMBER                     
* TESTING                                                                       
*        CLC   0(4,RF),=X'23460016'                                             
*        BNE   *+6                                                              
*        DC    H'0'                                                             
* TESTING                                                                       
         CLC   CDARNUM,0(RF)       LOOKING FOR THIS CONTRACT NUMBER?            
         BNE   LRSEQ               NO  - SKIP IT                                
         B     LR270               YES - ACCEPT ORDER                           
*                                                                               
*   FILTER FOR A SINGLE AGENCY ORDER                                            
*                                                                               
LR115060 EQU   *                                                                
         LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
LR115080 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    LR115090            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     LR115080            GO BACK FOR NEXT                             
LR115090 EQU   *                                                                
         ZIC   RE,DORDER#(RF)      GET DISPLACEMENT TO ORDER NUMBER             
         LA    RF,KEY              SET A(KEY IN PROGRESS)                       
         AR    RF,RE               DISPLACE TO ORDER NUMBER                     
         CLC   HAGYORDR,0(RF)      LOOKING FOR THIS AGENCY ORDER #?             
         BNE   LRSEQ               NO  - SKIP IT                                
         B     LR270               YES - ACCEPT ORDER                           
LR115100 EQU   *                                                                
*                                                                               
         CLI   STATFILT,C'F'       FILTER ON CONFIRMED?                         
         BNE   LR115160            NO  -                                        
         TM    KEY+27,X'04'        YES - 'CONFIRMED' SET IN STATUS?             
         BNO   LR115140            NO  - SET TO CHECK PENDCF                    
         L     RF,AIO              YES - SEE IF RECORD READ                     
         CLC   KEY(27),0(RF)       RECORD IN IO AREA?                           
         BE    LR115120            YES - DON'T REREAD                           
*                                  NO  - READ IN THE RECORD                     
         GOTOR MYGETREC                                                         
         BNZ   LRSEQ               NOT FOUND OR DELETED                         
*                                                                               
LR115120 DS    0H                                                               
         GOTOR CHKCAMPS                                                         
         BNZ   LRSEQ                                                            
         B     LR270                                                            
LR115140 EQU   *                                                                
         MVI   PENDCF,1            NO  - SET 'F-FILTER, NOT CF'                 
         B     LR213B10                                                         
*                                                                               
LR115160 EQU   *                                                                
*                                                                               
*   NOT 'FILTER ON CONFIRMED': REJECT ALL CONFIRMED FOR DISPLAY                 
*                                                                               
DIE      DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
*                                                                               
         CLI   DE7TYPEH+5,0        FILTER ON TYPE REQUESTED?                    
         BNE   LR115180            YES - SKIP CAMPAIGN FOR CF TEST              
*                                  NO TYPE FILTER:                              
         OC    CAMPFLD1(12),CAMPFLD1                                            
*                                  ANY CAMPAIGN FILTER?                         
         BNZ   LR120               YES - INCLUDE 'CONFIRMED' ORDERS             
*                                                                               
LR115180 EQU   *                                                                
         DROP  RF                                                               
*                                                                               
         TM    KEY+27,X'04'        YES - 'CONFIRMED' SET IN STATUS?             
         BO    LRSEQ               YES - REJECT THIS ORDER                      
         B     LR120                                                            
         EJECT                                                                  
********************************************************************            
*   KEY DEFINITION FOR FIELDS:                                                  
*        BYTE  1  =  RECORD SUBKEY TYPE                                         
*        BYTE  2  =  DISPLACEMENT TO STATION IN KEY: X'FF' = READ               
*                                  RECORD FOR DATA                              
*        BYTE  3  =  DISPLACEMENT TO                                            
*        BYTE  4  =  DISPLACEMENT TO                                            
*                                                                               
*   TYPE 1 RECORD                                                               
KEYDEFTB DC    AL1(1,255)                                                       
         DC    AL1(RED01RAG-RED01TYP)                                           
         DC    AL1(RED01ORD-RED01TYP)                                           
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
KEYDEFLN EQU   *-KEYDEFTB                                                       
*   TYPE 2 RECORD                                                               
         DC    AL1(2,255)                                                       
         DC    AL1(RED02RAG-RED02TYP)                                           
         DC    AL1(RED02ORD-RED02TYP)                                           
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
*   TYPE 3 RECORD                                                               
         DC    AL1(3,RED03STA-RED03TYP)                                         
         DC    AL1(RED03RAG-RED03TYP)                                           
         DC    AL1(RED03ORD-RED03TYP)                                           
         DC    AL1(RED03PP-RED03TYP)                                            
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
*   TYPE 4 RECORD                                                               
         DC    AL1(4,RED04STA-RED04TYP)                                         
         DC    AL1(RED04RAG-RED04TYP)                                           
         DC    AL1(RED04ORD-RED04TYP)                                           
         DC    AL1(RED04PP-RED04TYP)                                            
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
*   TYPE 5 RECORD                                                               
         DC    AL1(5,RED05STA-RED05TYP)                                         
         DC    AL1(RED05RAG-RED05TYP)                                           
         DC    AL1(RED05ORD-RED05TYP)                                           
         DC    AL1(RED05PP-RED05TYP)                                            
         DC    AL1(RED05BYR-RED05TYP)                                           
*   TYPE 6 RECORD                                                               
         DC    AL1(6,RED06STA-RED06TYP)                                         
         DC    AL1(RED06RAG-RED06TYP)                                           
         DC    AL1(RED06ORD-RED06TYP)                                           
         DC    AL1(RED06PP-RED06TYP)                                            
         DC    AL1(RED06BYR-RED06TYP)                                           
*   TYPE 7 RECORD                                                               
         DC    AL1(7,RED07STA-RED07TYP)                                         
         DC    AL1(RED07RAG-RED07TYP)                                           
         DC    AL1(RED07ORD-RED07TYP)                                           
         DC    AL1(RED07PP-RED07TYP)                                            
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
*   TYPE 8 RECORD                                                               
         DC    AL1(8,RED08STA-RED08TYP)                                         
         DC    AL1(RED08RAG-RED08TYP)                                           
         DC    AL1(RED08ORD-RED08TYP)                                           
         DC    AL1(RED08PP-RED08TYP)                                            
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
*   TYPE 9 RECORD                                                               
         DC    AL1(9,RED09STA-RED09TYP)                                         
         DC    AL1(RED09RAG-RED09TYP)                                           
         DC    AL1(RED09ORD-RED09TYP)                                           
         DC    AL1(RED09PP-RED09TYP)                                            
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
*   TYPE 10 RECORD                                                              
         DC    AL1(10,RED0ASTA-RED0ATYP)                                        
         DC    AL1(RED0ARAG-RED0ATYP)                                           
         DC    AL1(RED0AORD-RED0ATYP)                                           
         DC    AL1(RED0APP-RED0ATYP)                                            
         DC    AL1(254)            DUMMY: NO DATA FOR THIS RECORD TYPE          
*   DELIMITER                                                                   
         DC    X'FF'                                                            
         DS    0H                                                               
*                                                                               
*                                                                               
DREDTYPE EQU   0                                                                
DSTATION EQU   1                                                                
DAGENCY  EQU   2                                                                
DORDER#  EQU   3                                                                
DPOINTP  EQU   4                                                                
DBUYER   EQU   5                                                                
*                                                                               
LR120    DS    0H                                                               
         CLI   DE7AGYH+5,0         FILTER ON AGENCY                             
         BE    LR140                                                            
         LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
LR120020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    LR120040            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     LR120020            GO BACK FOR NEXT                             
LR120040 EQU   *                                                                
         ZIC   RE,DAGENCY(RF)      GET DISPLACEMENT TO AGENCY CODE              
         LA    RF,KEY              SET A(KEY IN PROGRESS)                       
         AR    RF,RE               DISPLACE TO AGENCY CODE                      
         LA    RE,AGYFILT          LOAD A(EQUIV AGENCY CODE FILTERS)            
         LA    R1,4                                                             
LR130    DS    0H                                                               
         OC    0(5,RE),0(RE)                                                    
         BZ    LRSEQ                                                            
         CLC   0(5,RE),SPACES                                                   
         BE    LRSEQ                                                            
         CLC   0(5,RE),0(RF)       COMPARE VS AGENCY IN KEY                     
         BE    LR140                                                            
         LA    RE,5(RE)                                                         
         BCT   R1,LR130                                                         
         B     LRSEQ                                                            
*                                                                               
LR140    DS    0H                                                               
*                                                                               
         MVI   ERRFLAG,C'N'                                                     
         CLI   DE7HDLNH+5,0        IF CON# SPECIFIED, LIST ONLY THE             
         BE    LR150                AGENCY ORDER LINKED TO THIS CON#            
         MVI   CONFILT,C'N'        SET 'CONTRACT NOT FOUND' FLAG                
         OC    CDARNUM,CDARNUM     SKIP IF THERE ARE NONE                       
         BZ    LRX                                                              
         LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
LR140020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    LR140040            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     LR140020            GO BACK FOR NEXT                             
LR140040 EQU   *                                                                
         ZIC   RE,DORDER#(RF)      GET DISPLACEMENT TO ORDER NUMBER             
         LA    RF,KEY              SET A(KEY IN PROGRESS)                       
         AR    RF,RE               DISPLACE TO ORDER NUMBER                     
         CLC   CDARNUM,0(RF)       LOOKING FOR THIS CONTRACT NUMBER?            
         BNE   LRSEQ               NO  - SKIP IT                                
         MVI   CONFILT,C'Y'        SET 'CONTRACT FOUND' FLAG                    
         DROP  R3                                                               
                                                                                
LR150    DS    0H                                                               
*                                                                               
*   RECEIVED DATE FILTER WAS AT THIS POINT.  UNDER CERTAIN CONDITIONS,          
*        IT WASN'T BEING TESTED.  IT HAS BEEN MOVED TO EARLIER IN THE           
*        CYCLE.  IT HAS BEEN SET AS A LABEL=* FOR ADDRESSABILITY.               
*                                                                               
         CLI   DE7GRPH+5,0         IF EITHER GROUP OR TEAM FILTER               
         BNE   LR160               REQUESTED, READ STATION RECORD               
         CLI   DE7TEAMH+5,0        IN ORDER TO VALIDATE                         
         BE    LR205                                                            
                                                                                
LR160    DS    0H                  GET GROUP AND TEAM CODES                     
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
         XC    MYWORK,MYWORK                                                    
         LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
LR160020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    LR160040            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     LR160020            GO BACK FOR NEXT                             
LR160040 EQU   *                                                                
*                                                                               
*   FOR D001/D002/D102 KEYS, STATION IS NOT IN THE KEY. RECORD                  
*        MUST BE READ TO GET THE STATION.  CAN THEN PROCEED.                    
*                                                                               
         CLI   DSTATION(RF),X'FF'  READ STATION INDICATOR?                      
         BNE   LR160060            NO                                           
         GOTOR READAGYO                                                         
*                                  YES - READ,TEST D001/D002/D102 KEYS          
         BNZ   LRSEQ               NOT EQUAL - SKIP THIS ORDER                  
         L     R6,AIO                                                           
U        USING RDARREC,R6                                                       
         LA    RE,U.RDARKSTA       SET A(STATION IN AGY ORDER REC)              
         B     LR160080                                                         
*                                                                               
         DROP  U                                                                
*                                                                               
*   POINT RF TO STATION CALLS IN AGY ORD REC, AND PROCEED.                      
*                                                                               
LR160060 EQU   *                                                                
         LA    RE,KEY              SET A(KEY IN PROGRESS)                       
LR160070 EQU   *                                                                
         ZIC   R1,DSTATION(RF)     GET DISPLACEMENT TO STA CALLS                
         AR    RE,R1               DISPLACE TO STATION CALL LETTERS             
LR160080 EQU   *                                                                
         MVC   MYWORK+8(4),0(RE)   TAKE STATION FROM KEY                        
         MVI   MYWORK+5,4                                                       
         CLI   3(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   MYWORK+5,3          IF 3 LETTER CALL LETTERS                     
         MVI   MYWORK+2,X'40'      SET ALPHA                                    
         CLI   4(RE),C'T'          NO NEED FOR TV BAND                          
         BE    LR170                                                            
         MVI   MYWORK+12,C'-'                                                   
         MVC   MYWORK+13(1),4(RE)                                               
         MVI   MYWORK+5,6                                                       
         CLI   3(RE),C' '                                                       
         BNE   LR170                                                            
         MVC   MYWORK+11(2),MYWORK+12                                           
         MVI   MYWORK+13,0         IF 3 LETTER CALL LETTERS                     
         MVI   MYWORK+5,5                                                       
                                                                                
LR170    DS    0H                                                               
         MVC   SVKEY,KEY                                                        
         LA    R2,MYWORK                                                        
         GOTO1 VALISTA                                                          
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'C'                                                       
*   TEST END                                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
                                                                                
         CLI   DE7GRPH+5,0         GROUP FILTER?                                
         BE    LR180                                                            
         ZIC   RF,DE7GRPH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   GRPFILT(0),WORK+31                                               
         BNE   LRSEQ                                                            
                                                                                
LR180    DS    0H                                                               
         CLI   DE7TEAMH+5,0        TEAM FILTER?                                 
         BE    LR205               NO                                           
                                                                                
         L     R6,AIO3             STATION RECORD IS IN IO3                     
         MVI   ELCODE,X'04'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRSEQ                                                            
                                                                                
         USING RSTAOTEL,R6                                                      
LR190    CLC   RSTAOTOF,RDARKAOF   LOOK FOR MATCHING OFFICE                     
         BE    LR200                                                            
         BRAS  RE,NEXTEL                                                        
         BE    LR190                                                            
         B     LRSEQ                                                            
                                                                                
LR200    DS    0H                                                               
         ZIC   RF,DE7TEAMH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TEAMFILT(0),RSTAOTTM                                             
         BNE   LRSEQ                                                            
         DROP  R3,R6                                                            
                                                                                
LR205    DS    0H                                                               
LR210    DS    0H                                                               
         MVC   ORDLSTDA,KEY+28                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'D'                                                       
*   TEST END                                                                    
*                                                                               
         GOTOR MYGETREC                                                         
         BZ    LR210010            NOT DELETED                                  
         CLI   KEY+27,X'FF'        DELETED BY REGENDTR?                         
         BE    LRSEQ               YES - SKIP THIS RECORD                       
LR210010 EQU   *                                                                
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARCNTL,X'80'      !!!NEVER GET A DELETED RECORD!!!             
         BZ    LR210A                                                           
         TM    RDARDELS,X'20'      FOR DELETES, PASS ONLY IF RECALL             
         BZ    LRSEQ               CANCELLED                                    
         DROP  R6                                                               
*                                                                               
LR210A   DS    0H                                                               
*&&DO                                                                           
*   CODE MOVED TO A LATER SPOT                                                  
*                                                                               
         NI    BITFLAG2,X'FF'-B2SENT                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR210A10                                                         
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'40'      SENT TO STATION (-S)?                        
         BZ    LR210A10                                                         
         OI    BITFLAG2,B2SENT                                                  
         DROP  R6                                                               
*&&                                                                             
*                                                                               
LR210A10 EQU   *                                                                
         GOTOR CHKCAMPS                                                         
         BNZ   LRSEQ               MISMATCH:  SKIP ORDER                        
LR210A20 EQU   *                                                                
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    LR210AA                                                          
         DROP  R1                                                               
*                                                                               
         GOTOR CHKLOCAL                                                         
         BNZ   LRSEQ               CHECK IF LOCAL ORDER                         
*                                                                               
LR210AA  DS    0H                                                               
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    LR210AB                                                          
         DROP  R1                                                               
*                                                                               
         GOTOR CHKDIREC                                                         
         BNZ   LRSEQ               CHECK IF DIRECT ORDER IN USE                 
*                                                                               
LR210AB  DS    0H                                                               
*                                                                               
*  RESPECT OFFICE CHECK EVEN WHEN FILTERING OF SPECIFIC CONTRACT                
*                                                                               
**       OC    CDARNUM,CDARNUM     FILTERING ON SPECIFIC CONTRACT?              
**       BNZ   LR210B              YES - NO OFFICE CHECK                        
         CLI   DE7OFFH+5,0         FILTER ON OFFICE?                            
         BE    LR210B                                                           
**       CLC   =C'C=',DE7OFF                                                    
**       BE    LR210B                                                           
         GOTOR CHECKOFF                                                         
         BNZ   LRSEQ                                                            
*                                  CHECK IF LOCAL ORDER IN USE                  
LR210B   DS    0H                                                               
         CLC   =C'BRAND',CONREC                                                 
         BE    LR211                                                            
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'08'      DON'T SHOW BRAND ORDERS IN                   
         BZ    LR211                                                            
         CLI   RDARKTYP,X'51'      UNLESS IT'S CONFIRMED                        
         BNE   LRSEQ               NO                                           
         DROP  R6                                                               
*                                                                               
LR211    DS    0H                                                               
         CLI   DE7TYPEH+5,0        FILTER ON TYPE?                              
         BNE   LR211040            YES - CHECK TYPE FILTERS                     
         L     R6,AIO              NO  - CAN'T DISPLAY A PENDCF                 
         MVI   ELCODE,X'0F'        MUST SKIP PENDCF'S                           
         BRAS  RE,GETEL                                                         
         BNE   LR211020            NO X'0F' ELEMENT                             
         USING RDARFLEM,R6                                                      
*                                                                               
         TM    RDARFLG1,X'20'      PENDCF?                                      
         BO    LRSEQ               YES - SKIP IT                                
LR211020 EQU   *                                                                
         B     LR270                                                            
LR211040 DS    0H                                                               
*                                                                               
         CLI   STATFILT,C'F'       CONFIRM                                      
         BE    LR270                                                            
*                                                                               
         CLI   STATFILT,C'I'       UNTOUCHED/INCOMING                           
         BNE   LR212                                                            
         CLI   OFFLINE,C'Y'        OFFLINE SHOULD IGNORE I FILTER               
         BE    LR270                                                            
         TM    MYFLAG,MFGUNDOP     UNDO SHOULD IGNORE I FILTER                  
         BO    LR270                                                            
*                                                                               
         MVI   ELCODE,X'30'        PRESENCE OF FIRST TOUCHED ELEMENT            
         BRAS  RE,GETEL            MEANS THE RECORD HAS BEEN LOOKED AT          
         BE    LRSEQ                                                            
         B     LR270                                                            
LR211050 DS    0H                                                               
*                                                                               
LR212    DS    0H                                                               
         CLI   STATFILT,C'*'       TOUCHED/PRINTED BY USER                      
         BNE   LR213                                                            
         MVI   ELCODE,X'30'        PRESENCE OF FIRST TOUCHED ELEMENT            
         BRAS  RE,GETEL            MEANS THE RECORD HAS BEEN PRINTED            
         BNE   LRSEQ                                                            
         CLI   STATFILT+1,C' '                                                  
         BE    LR270                                                            
*                                                                               
LR213    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   STATFILT+1,C'V'     VARIOUS/CORPORATE                            
         BNE   LR213AA                                                          
         CLI   STATFILT,C' '       COMBINATION??                                
         BE    LR213A                                                           
         TM    CTLRFLG1,CF1BRDQ    YES, VARIOUS AND SOME OTHER STATUS           
         BO    LR214                                                            
         TM    RDARMISC,X'10'                                                   
         BO    LR214                                                            
         CLC   =C'*CORPORATE*',RDARPRN1                                         
         BE    LR214                                                            
         B     LRSEQ                                                            
*                                                                               
LR213A   DS    0H                  NO, SHOW ME ALL VARIOUS ORDERS               
         TM    CTLRFLG1,CF1BRDQ                                                 
         BO    LR270                                                            
         TM    RDARMISC,X'10'                                                   
         BO    LR270                                                            
         CLC   =C'*CORPORATE*',RDARPRN1                                         
         BE    LR270                                                            
         B     LRSEQ                                                            
                                                                                
LR213AA  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         GOTOR TSTFLTR             NEW FILTERS?                                 
         BL    LR213AAA            NO                                           
         BE    LR270               YES,AND ORDER DOES SATISFY FILTER            
         BH    LRSEQ               YES,BUT ORDER DOES NOT SATISFY FILTR         
*                                                                               
LR213AAA DS    0H                                                               
         CLI   STATFILT,C'M'       FILTER ON AMEND?                             
         BNE   LR213B              NO                                           
         TM    RDARBSTS,C'M'       YES - AMEND STATUS?                          
         BNO   LRSEQ               NO  - SKIP IT                                
*                                                                               
         L     R6,AIO              YES - CHECK STACF/PENDCF                     
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRSEQ                                                            
         USING RDARFLEM,R6                                                      
*                                                                               
         TM    RDARFLG1,X'10'      STACF?                                       
         BO    LRSEQ               YES - NOT AMEND: SKIP                        
         TM    RDARFLG1,X'20'      PENDCF?                                      
         BO    LRSEQ               YES - NOT AMEND: SKIP                        
         B     LR270               NO  - REALLY AN AMEND                        
*                                                                               
LR213B   DS    0H                                                               
         CLI   STATFILT,C'W'       STACF?                                       
         BNE   LR213B10                                                         
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRSEQ                                                            
         USING RDARFLEM,R6                                                      
*                                                                               
         TM    RDARFLG1,X'10'      STACF?                                       
         BO    LR270               YES                                          
         B     LRSEQ                                                            
*                                                                               
LR213B10 DS    0H                                                               
***>>>                                                                          
         CLI   STATFILT,C'P'       FILTER PENDCF ONLY?                          
         BE    LR213B40            YES -                                        
         CLI   STATFILT,C'F'       FILTER CF/PENDCF?                            
         BNE   LR213B50            NO  -                                        
*                                                                               
*   FILTER = 'F':  WASN'T A 'CONFIRMED' ORDER.  BRANCH TO HERE                  
*        DIRECTLY FROM THE 'CONFIRMED' TEST SKIPS THE READ OF                   
*        THE CURRENT KEY.  THEREFORE, THE RECORD IS READ AT THIS                
*        STAGE TO PERMIT THE NEXT TEST TO BE AGAINST THE CORRECT                
*        RECORD.                                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   SET FLAG                                                                    
         MVI   KEY-8,C'K'                                                       
*   SET FLAG                                                                    
*                                                                               
         GOTOR MYGETREC                                                         
         BNZ   LRSEQ                                                            
*                                                                               
LR213B40 DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRSEQ                                                            
         USING RDARFLEM,R6                                                      
*                                                                               
         TM    RDARFLG1,X'20'      PENDCF?                                      
         BO    LR270               YES - ACCEPT                                 
         B     LRSEQ               NO  - SKIP THIS ORDER                        
*                                                                               
LR213B50 DS    0H                                                               
         L     R6,AIO              NOT PENDCF OR PENDCF/CF FILTER               
         MVI   ELCODE,X'0F'        MUST SKIP PENDCF'S                           
         BRAS  RE,GETEL                                                         
         BNE   LR213B60            NO X'0F' ELEMENT                             
         USING RDARFLEM,R6                                                      
*                                                                               
         TM    RDARFLG1,X'20'      PENDCF?                                      
         BO    LRSEQ               YES - SKIP IT                                
LR213B60 EQU   *                                                                
         L     R6,AIO              RESET A(DARE RECORD IN PROGRESS)             
         USING RDARREC,R6                                                       
         CLI   STATFILT+1,C'E'     REVISION?                                    
         BNE   LR214                                                            
         CLI   STATFILT,C' '       COMBINATION??                                
         BE    LR213AB                                                          
         OC    RDARRNUM,RDARRNUM                                                
         BZ    LRSEQ                                                            
         CLI   STATFILT,C'*'       COMBINATION??                                
         BE    LR270                                                            
         B     LR214                                                            
*                                                                               
LR213AB  DS    0H                  NO, SHOW ME ALL REVISION ORDERS              
         OC    RDARRNUM,RDARRNUM                                                
         BNZ   LR270                                                            
         B     LRSEQ                                                            
                                                                                
LR214    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   STATFILT,C'N'       NOTDARE                                      
         BNE   LR215                                                            
         TM    RDARMISC,X'20'                                                   
         BO    LR270                                                            
         B     LRSEQ                                                            
                                                                                
LR215    DS    0H                                                               
         CLI   STATFILT,C'S'       RESENT?                                      
         BNE   LR220                                                            
         TM    RDARMISC,X'80'      YES                                          
         BZ    LRSEQ                                                            
         OC    RDARBSTS,RDARBSTS   MAKE SURE WE DIDN'T APP/REJ                  
         BZ    LR270                                                            
         CLI   RDARBSTS,C' '                                                    
         BE    LR270                                                            
         B     LRSEQ                                                            
                                                                                
LR220    DS    0H                                                               
         CLI   STATFILT,C'C'       RECALL?                                      
         BNE   LR225                                                            
         CLI   RDARBSTS,C'C'       YES                                          
         BNE   LRSEQ                                                            
         B     LR270                                                            
                                                                                
LR225    DS    0H                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    LR240                                                            
                                                                                
         CLI   STATFILT,C'L'       LINKED                                       
         BE    LR230                                                            
         CLI   STATFILT,C'B'       BOTH                                         
         BE    LR230                                                            
         CLC   RDARBSTS,STATFILT   OPENED/REJECTED                              
         BE    LR270                                                            
         B     LRSEQ                                                            
                                                                                
LR230    DS    0H                  FOR LINKED/BOTH                              
         TM    RDARMISC,X'80'      IGNORE RESENT FOR ALL OTHER FILTERS          
         BO    LRSEQ                                                            
         OC    RDARBSTS,RDARBSTS   BUT NOT OPENED NOR REJECTED                  
         BZ    LR270                                                            
         CLI   RDARBSTS,C' '                                                    
         BE    LR270                                                            
         B     LRSEQ                                                            
                                                                                
LR240    DS    0H                  NOT LINKED                                   
         CLI   STATFILT,C'R'       REJECTED                                     
         BNE   LR250                                                            
         CLI   RDARBSTS,C'R'                                                    
         BE    LR270                                                            
         B     LRSEQ                                                            
                                                                                
LR250    DS    0H                                                               
         CLI   STATFILT,C'U'       UNLINKED                                     
         BE    LR260                                                            
         CLI   STATFILT,C'B'       BOTH                                         
         BNE   LRSEQ                                                            
                                                                                
LR260    DS    0H                                                               
         TM    RDARMISC,X'80'      IGNORE RESENT FOR ALL OTHER FILTERS          
         BO    LRSEQ                                                            
         OC    RDARBSTS,RDARBSTS   STATUS IF NEITHER OPENED                     
         BZ    LR270                 NOR REJECTED                               
         CLI   RDARBSTS,C' '                                                    
         BNE   LRSEQ                                                            
                                                                                
LR270    DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
*   AT THIS POINT, THE AGENCY ORDER SHOULD BE IN THE IO AREA.                   
*        IF IT ISN'T, IT IS RETRIEVED INTO THAT SPACE.                          
*                                                                               
         L     RF,AIO              YES - SEE IF RECORD READ                     
         CLC   KEY(27),0(RF)       RECORD IN IO AREA?                           
         BE    LR270010            YES - DON'T REREAD                           
*                                  NO  - READ IN THE RECORD                     
         GOTOR MYGETREC                                                         
         BNZ   LRSEQ               NOT FOUND OR DELETED                         
***TESTING                                                                      
*&&DO                                                                           
         L     R6,AIO                                                           
*        USING RDARREC,R6                                                       
         CLC   RDARKORD,=X'23460016'                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*        DROP  R6                                                               
*&&                                                                             
***TESTING                                                                      
*                                                                               
LR270010 DS    0H                                                               
         CLI   DE7BUYRH+5,0        ANY BUYER  FILTER?                           
         BE    LR270012            NO  -                                        
         OC    DE7BUYR(3),SPACES   YES - SET BINARY ZERO TO SPACES              
         CLC   RDARBUYC,DE7BUYR    YES - BUYER CODE FOUND?                      
         BNE   LRSEQ               NO  - SKIP IT                                
LR270012 DS    0H                                                               
         CLI   DE7HDLNH+5,0        CONTRACT # ENTERED?                          
         BE    LR270013            NO                                           
*                                  YES - LAST CHECK FOR COMPANY CODE            
         CLI   DE7REPH+5,0         ANY COMPANY FILTER?                          
         BE    LR270013            NO  -                                        
         CLC   RDARKREP,DE7REP     YES - ORDER FOR THIS COMPANY?                
         BNE   LRSEQ               NO  - SKIP IT                                
LR270013 DS    0H                                                               
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR270015            NO                                           
         CLI   DE7INBXH+5,0        ANY IN-BOX FILTER?                           
         BE    LR270015            NO  -                                        
         L     R6,AIO              YES - FIND X'0A' ELEMENT                     
         MVI   ELCODE,X'0A'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                 MUST BE PRESENT                              
         DC    H'0'                                                             
         CLC   RDARPPSP-RDARPPEL(3,R6),DE7INBX                                  
*                                  COMPARE FOR P/P-S/P OF ORDER                 
         BNE   LRSEQ               NOT SAME: SKIP                               
LR270015 DS    0H                                                               
         NI    BITFLAG2,X'FF'-B2SENT                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR270018                                                         
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'40'      SENT TO STATION (-S)?                        
         BZ    LR270018                                                         
         OI    BITFLAG2,B2SENT                                                  
         DROP  R6                                                               
LR270018 DS    0H                                                               
         L     R6,AIO              RESET A(DARE RECORD)                         
         USING RDARREC,R6                                                       
*                                                                               
         CLI   STATFILT,C'F'       CONFIRMED?                                   
         BNE   LR275               NO  -                                        
*                                                                               
         CLI   DE7OFFH+5,0         ANY OFFICE FILTER?                           
         BE    LR270020            NO  - DON'T FILTER                           
***      CLC   =C'C=',DE7OFF       YES - SPECIAL OVERRIDE?                      
***      BE    LR270020            YES - DONT CHECK OFFICE                      
         GOTOR CHECKOFF                                                         
*                                  NO  - CHECK OFFICE                           
         BNZ   LRSEQ               NO MATCH: SKIP THIS ORDER                    
*                                                                               
LR270020 EQU   *                                                                
         CLI   TWAOFFC,C'*'        DDS TERMINAL, SHOW EVERYTHING                
         BE    LR275                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRSEQ                                                            
         GOTO1 DATCON,DMCB,(5,0),(2,HALF)                                       
         PUSH  USING                                                            
         USING RDARELEM,R6                                                      
         CLI   RDARRNUM,0                                                       
         BNE   LR275               FOR ORIGINAL ORDERS, DON'T LIST IF           
         CLC   HALF,RDARESEN       PAST END OF FLIGHT?                          
         BH    LRSEQ               YES - SKIP IT                                
         DROP  R6                                                               
         POP   USING                                                            
                                                                                
LR275    DS    0H                                                               
         L     R6,AIO              POINT R6 TO BEGINNING OF REC AGAIN           
         XC    AGYOFF,AGYOFF                                                    
         XC    AGYSALP,AGYSALP                                                  
                                                                                
LR280    DS    0H                                                               
         OC    RDARREP#,RDARREP#   ONLY LINKED ORDERS ARE PASSED FOR            
         BNZ   LR290                SALESPERSON FILTERS                         
         CLI   DE7SALPH+5,0        FILTER ON SALESPERSON?                       
         BNE   LRSEQ               IF YES, LINKED ORDERS ONLY                   
         B     LR340                                                            
                                                                                
LR290    DS    0H                  GET CONTRACT RECORD                          
         MVC   SVKEY,KEY                                                        
*                                                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RDARREP#                                              
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
*                                                                               
         XC    KEY,KEY                                                          
CONKEYD  USING RCONKEY,KEY                                                      
         MVI   CONKEYD.RCONPTYP,X'8C'                                           
         MVC   CONKEYD.RCONPREP,AGENCY                                          
         MVC   CONKEYD.RCONPCON,WORK                                            
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'                                                   
         BZ    *+10                MASTER REP SIGNED ON?                        
         MVC   CONKEYD.RCONPREP,RDARKREP   YES, USE SUB REP                     
*                                                                               
         DROP  CONKEYD,R2,R6                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   ERRFLAG,C'N'                                                     
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    LR295                                                            
*        CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*        BNE   LR320                                                            
         MVI   ERRFLAG,C'Y'        SHOW THE ORDER IN ERROR STAT                 
         B     LR330               CONTRACT RECORD DELETED                      
*                                  THIS SHOULD NEVER HAPPEN...BUT IF IT         
*                                  DOES, SKIP ORDER INSTEAD OF DUMPING          
LR295    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'E'                                                       
*   TEST END                                                                    
*                                                                               
         GOTOR MYGETREC                                                         
*                                  CONTRACT RECORD READ                         
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         MVC   AGYOFF,RCONKOFF                                                  
         MVC   AGYSALP,RCONSAL                                                  
         DROP  R6                                                               
*&&DO                                                                           
*                                                                               
* FOR OPENED ORDERS, CHECK IF CONTRACT SENT TO STATION                          
*                                                                               
         NI    BITFLAG2,X'FF'-B2SENT                                            
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR320               CONTRACT NOT A DARE ORDER ANYMORE            
         LR    R3,R6                                                            
DARELEMD USING RCONDREL,R3                                                      
         TM    DARELEMD.RCONDRFG,X'40'      OPENED?                             
         BZ    LR300                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*        TM    RCONSENF,X'80'      LAST SENT BY REP?                            
*        BZ    LR300                                                            
*                                                                               
* CHECK IF DARE APPROVAL DATE IS LATER THAN LAST SENT TO STATION DATE           
*                                                                               
         CLC   DARELEMD.RCONDRDA,RCONSRDT                                       
         BH    LR300                                                            
         BL    LR298                                                            
         GOTO1 HEXIN,DMCB,RCONSRTI,WORK,4                                       
         CLC   DARELEMD.RCONDRTA,WORK                                           
         BH    LR300                                                            
*                                                                               
         DROP  DARELEMD                                                         
*                                                                               
LR298    DS    0H                                                               
         OI    BITFLAG2,B2SENT                                                  
         DROP  R6                                                               
*&&                                                                             
*                                                                               
LR300    DS    0H                                                               
*&&DO                                                                           
*                                                                               
*   HERE'S SOME SWEET CODING, WHICH DOES EXACTLY NOTHING, BUT TAKES             
*        SPACE.  IT IS HEREBY NOOPED.                                           
*                                                                               
         CLI   DE7OFFH+5,0         FILTER ON OFFICE OR                          
         BE    LR310                                                            
         CLC   =C'C=',DE7OFF                                                    
         BNE   LR310                                                            
         CLC   AGYOFF,DE7OFF+2                                                  
         BE    LR310                                                            
*&&                                                                             
LR310    DS    0H                                                               
         CLI   DE7SALPH+5,0         ON SALESPERSON?                             
         BE    LR330                                                            
         CLC   SALFILT,AGYSALP                                                  
         BE    LR330                                                            
                                                                                
LR320    DS    0H                  NO MATCH, RESTORE KEY AND SEQ NEXT           
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTO1 HIGH                                                             
         B     LRSEQ                                                            
*                                                                               
* DON'T LIST CONFIRMED VARIOUS ORDERS IF:                                       
* - NO BRAND ORDER HAS BEEN RECEIVED OR                                         
* - ALL BRAND ORDERS HAVE BEEN CONFIRMED                                        
*                                                                               
LR330    DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT LIST DARE RECORD             
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTO1 HIGH                                                             
*        CLC   KEY(L'RDARKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTOR MYGETREC                                                         
         BNZ   LRSEQ                                                            
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'04'      COFIRMED VARIOUS ORDER?                      
         BZ    LR340                                                            
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVKEY           YES, GET BRAND RECORD                        
         MVI   KEY+24,X'35'                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   LR335                                                            
*                                                                               
LR333    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'G'                                                       
*   TEST END                                                                    
*                                                                               
         GOTOR MYGETREC                                                         
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARPDFG,X'40'      BRAND ORDER RECEIVED                         
         BZ    LR334                                                            
         TM    RDARPDFG,X'80'      BRAND ORDER NOT CONFIRMED                    
         BZ    LR335                                                            
*                                                                               
LR334    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    LR333                                                            
         DROP  R6                                                               
*                                  NO MATCH, RESTORE KEY AND SEQ NEXT           
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'G'                                                       
*   TEST END                                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTO1 HIGH                                                             
         B     LRSEQ                                                            
*                                                                               
LR335    DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT LIST DARE RECORD             
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'H'                                                       
*   TEST END                                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTO1 HIGH                                                             
*        CLC   KEY(L'RDARKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'H'                                                       
*   TEST END                                                                    
*                                                                               
         GOTOR MYGETREC                                                         
         BNZ   LRSEQ                                                            
                                                                                
LR340    DS    0H                                                               
***TESTING                                                                      
*&&DO                                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   RDARKORD,=X'00000000'                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*&&                                                                             
***TESTING                                                                      
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    LR340020            DO OFFLINE PROCESSING                        
*                                                                               
         MVC   SAVRADKY,KEY                                                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        FIRST PRINTED ALREADY?                       
         BRAS  RE,GETEL                                                         
         BE    LR340A              YES, SKIP                                    
*                                                                               
* TEST TO SEE IF UNDO                                                           
         TM    MYFLAG,MFGUNDOP     UNDO + NO 30 = SKIP                          
         BO    LRSEQ                                                            
         B     LR340B              NORMAL + NO 30 = PROCESS                     
LR340A   DS    0H                  AM I UNDOING?                                
         TM    MYFLAG,MFGUNDOP                                                  
         BZ    LRSEQ               NORAML + 30 = SKIP (ALREADY PRINTED          
         GOTOR UNDO                                                             
         BE    LR340C              SUCCESSFULLY UNDO THIS RECORD                
         B     LR340D              RECORD IS NOT UNDO                           
LR340B   DS    0H                                                               
*                                                                               
         XC    ELEM,ELEM           BUILD FIRST VIEW/PRINTED ELEMENT             
K        USING RDARVPEM,ELEM                                                    
         MVI   K.RDARVPCD,X'30'                                                 
         MVI   K.RDARVPLN,RDARVP2Q                                              
         MVC   K.RDARVPDT,SVPDATE    SAVE DATE/TIME                             
         MVC   K.RDARVPT2,SVPTIME                                               
         OI    K.RDARVPFG,X'80'    PRINTED VIA SOON                             
         DROP  K                                                                
         GOTO1 ADDELEM                                                          
*                                                                               
LR340C   DS    0H                                                               
         GOTO1 PUTREC                                                           
         LH    RF,MYCOUNT                                                       
         AHI   RF,1                                                             
         STH   RF,MYCOUNT                                                       
*                                                                               
LR340D   DS    0H                                                               
         MVC   KEY(27),SAVRADKY                                                 
         GOTO1 HIGH                                                             
         B     LRSEQ               PROCESS NEXT RECORD                          
*                                                                               
LR340020 DS    0H                  OFFLINE PROCESSING                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        ELEMENT SHOULD BE ADDED ONLINE               
         BRAS  RE,GETEL                                                         
         BNE   LRSEQ               NOT THERE, SKIP                              
*                                                                               
         USING RDARVPEM,R6                                                      
*                                                                               
* NEED TO MATCH THE DATE/TIME IN ELEMENT WITH DATE/TIME ON SCREEN NOW           
*                                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,RDARVPDT),(5,WORK)                                
         CLC   DE7PDAT(8),WORK     SAME DATE?                                   
         BNE   LRSEQ               NO, SKIP                                     
         GOTO1 HEXOUT,DMCB,RDARVPT2,WORK,3                                      
         MVC   MYWORK(2),WORK                                                   
         MVI   MYWORK+2,C':'                                                    
         MVC   MYWORK+3(2),WORK+2                                               
         MVI   MYWORK+5,C':'                                                    
         MVC   MYWORK+6(2),WORK+4                                               
*                                                                               
         CLC   MYWORK(8),DE7PTIM   SAME TIME?                                   
         BNE   LRSEQ               NO SKIP                                      
*                                                                               
*                                                                               
*   NOW WE CAN PRINT THE REPORT SAFELY                                          
                                                                                
*                                                                               
         L     R6,AIO                                                           
         MVC   SAVRADKY,KEY                                                     
         MVC   SELECTKY(27),0(R6)                                               
         XC    BYTE,BYTE                                                        
         OI    BYTE,QPRNEWPG+PRTDAB                                             
*        GOTO1 VREROM30,DMCB,(RC),(BYTE,0)                                      
         GOTO1 VLOAD,DMCB,(X'30',0),(BYTE,0)                                    
*                                                                               
         LH    RF,MYCOUNT                                                       
         AHI   RF,1                                                             
         STH   RF,MYCOUNT                                                       
*                                                                               
         MVC   KEY,SAVRADKY                                                     
         MVI   RDUPDATE,C'N'                                                    
         MVI   DMINBTS,0                                                        
         GOTO1 HIGH                                                             
*                                                                               
         B     LRSEQ                                                            
LRSEQ    DS    0H                                                               
***TESTING                                                                      
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   RDARKORD,=X'FFFFFFFF'                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
***TESTING                                                                      
LRSEQ20  DS    0H                                                               
         MVI   DMINBTS,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         LA    R3,KEY              RESET A(KEY FOR FILTERING)                   
         B     LR110                                                            
*                                                                               
LRX      DS    0H                                                               
*   LOOK FOR CONFIRM VERSION OF THE CONTRACT IF NO CON IS DISPLAYED             
*                                                                               
LRXA     DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    LRXA10                                                           
         XC    DE7PNUM,DE7PNUM                                                  
         EDIT  MYCOUNT,DE7PNUM,ZERO=NOBLANK,ALIGN=LEFT                          
         OI    DE7PNUMH+6,X'80'                                                 
         MVI   DE7PNUMH+5,5                                                     
         B     LRXA20                                                           
LRXA10   DS    0H                                                               
*&&DO                                                                           
         XC    WORK,WORK                                                        
         EDIT  MYCOUNT,(L'DE7PNUM,WORK),ZERO=NOBLANK,ALIGN=LEFT                 
         CLC   DE7PNUM,WORK                                                     
         BE    LRXA20                                                           
         MVC   P(40),=C'PRINT ERROR', JOB                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*&&                                                                             
LRXA20   DS    0H                                                               
         MVC   DARLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
MISSREC  MVC   RERROR,=AL2(NOTFOUND)                                            
         B     ERREND                                                           
*                                                                               
MISSHDR  MVC   RERROR,=AL2(952)                                                 
         B     ERREND                                                           
*                                                                               
KEYRESET MVC   RERROR,=AL2(200)                                                 
         B     INFEND                                                           
*                                                                               
ERSTAJDT MVC   RERROR,=AL2(287)    START DATE MUST BE ON OR AFTER               
         B     ERREND               STATION JOIN DATE                           
*                                                                               
ERDARSTA MVC   RERROR,=AL2(426)    STATIONS DO NOT MATCH                        
         B     ERREND                                                           
*                                                                               
ERLINKED MVC   RERROR,=AL2(421)    ORDER ALREADY LINKED                         
         B     ERREND                                                           
*                                                                               
ORDNOTLK MVC   RERROR,=AL2(423)    ORDER IS NOT LINKED TO ANY CONTRACT          
         B     ERREND                                                           
*                                                                               
ERUNLNKD MVC   RERROR,=AL2(427)    ORDER ALREADY UNLINKED                       
         B     ERREND                                                           
*                                                                               
ERCONLNK MVC   RERROR,=AL2(428)    CONTRACT ALREADY LINKED TO ANOTHER           
         B     ERREND                                                           
*                                                                               
ERDARAGY MVC   RERROR,=AL2(429)    AGENCY/OFF CODES DO NOT MATCH                
         B     ERREND                                                           
*                                                                               
MISDARAG MVC   RERROR,=AL2(430)    MISSING AGY EQUIVALENCY CODE                 
         B     ERREND                                                           
*                                                                               
ERBUYFND MVC   RERROR,=AL2(432)    BUY(S) FOUND FOR THIS CONTRACT               
         B     ERREND                                                           
*                                                                               
ERAPVLNK MVC   RERROR,=AL2(436)    CANNOT UNLINK AN APPROVED ORDER              
         B     ERREND                                                           
*                                                                               
ERNOTPEN MVC   RERROR,=AL2(438)    CANNOT LINK A NON-PENDING CONTRACT           
         B     ERREND                                                           
*                                                                               
INVLGRP  MVC   RERROR,=AL2(445)    INVALID GROUP                                
         B     ERREND                                                           
*                                                                               
INVLTEAM MVC   RERROR,=AL2(446)    INVALID TEAM                                 
         B     ERREND                                                           
*                                                                               
INVLOFF  MVC   RERROR,=AL2(447)    INVALID OFFICE                               
         B     ERREND                                                           
*                                                                               
INVLMKT  MVC   RERROR,=AL2(353)    INVALID MARKET                               
         B     ERREND                                                           
*                                                                               
INVLREP  MVC   RERROR,=AL2(925)    INVALID COMPANY                              
         B     ERREND                                                           
*                                                                               
NOAGYORD MVC   RERROR,=AL2(928)    NO AGENCY ORDER FOR CAMPAIGN                 
         B     ERREND                                                           
*                                                                               
BADEST#  MVC   RERROR,=AL2(929)    INVALID ESTIMATE # IN CAMPAIGN               
         B     ERREND                                                           
*                                                                               
TEMPLOCK MVC   RERROR,=AL2(570)    TEMPORARY LOCK                               
         B     ERREND                                                           
*                                                                               
ERTKOVER MVC   RERROR,=AL2(755)    MUST GENERATE TAKEOVER CONTRACT              
         B     ERREND                                                           
*                                                                               
BADERROR MVC   RERROR,=AL2(440)    UNEXPECTED ERROR ENCOUNTERED,                
         LA    R2,DE7HDLNH          CALL DDS                                    
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
SENDED   MVC   RERROR,=AL2(924)    SENDED AFTER LINKED, CAN'T BE UNLINK         
         B     ERREND                                                           
*                                                                               
NEXTREQ  MVC   RERROR,=AL2(3)      ENTER NEXT REQUEST                           
         B     INFEND                                                           
*                                                                               
LINKED   MVC   RERROR,=AL2(108)    AGENCY ORDER HAS BEEN LINKED                 
         B     INFEND                                                           
*                                                                               
UNLINKED MVC   RERROR,=AL2(109)    AGENCY ORDER HAS BEEN UNLINKED               
         B     INFEND                                                           
*                                                                               
*                                                                               
*                                                                               
ERREND   MVI   RMSGTYPE,C'E'                                                    
         B     *+8                                                              
INFEND   MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
***>>>                                                                          
*                                                                               
*   UNDO:  FOR PRINTREP MODE, RESET WORKAREA, SET HOOKS                         
*                                                                               
UNDO     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* NEED TO MATCH THE DATE/TIME IN ELEMENT WITH DATE/TIME ON SCREEN NOW           
* TO UNDO                                                                       
         GOTOR MATCH                                                            
         BNE   UNDONO                                                           
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM                                                          
UNDOYES  SR    RC,RC                                                            
UNDONO   LTR   RC,RC                                                            
         XIT1                                                                   
***>>>                                                                          
*                                                                               
*   MATCH: FOR PRINTREP MODE, RESET WORKAREA, SET HOOKS                         
*                                                                               
MATCH    NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         XC    MYWORK,MYWORK                                                    
*                                                                               
*        IF REPORT, RESTORE MYAREAD FROM STORAGE                                
*                                                                               
         USING RDARVPEM,R6                                                      
*                                                                               
* NEED TO MATCH THE DATE/TIME IN ELEMENT WITH DATE/TIME ON SCREEN NOW           
* TO UNDO                                                                       
*        GOTO1 DATCON,DMCB,(2,RDARVPDT),(5,WORK)                                
*        CLC   DE7PDAT(8),WORK     SAME DATE?                                   
*        BNE   MTCHNO              NO, SKIP                                     
         CLC   SVPDATE,RDARVPDT                                                 
         BNE   MTCHNO                                                           
*                                                                               
         MVC   MYWORK(2),DE7PTIM                                                
         MVC   MYWORK+2(2),DE7PTIM+3                                            
         MVC   MYWORK+4(2),DE7PTIM+6                                            
         GOTO1 HEXIN,DMCB,MYWORK,DUB,6                                          
*                                                                               
         CLC   DUB(3),RDARVPTM     SAME TIME?                                   
         BNE   MTCHNO              NO SKIP                                      
MTCHYES  SR    RC,RC                                                            
MTCHNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE CAMPAIGN FILTER FIELDS TO ORDER                                       
***********************************************************************         
CHKCAMPS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
*                                                                               
         OC    CAMPFLD1(12),CAMPFLD1                                            
*                                  ANY CAMPAIGN FILTER?                         
         BZ    CCAM0080            NO                                           
         CLC   CAMPFLD3+1(3),RDAREST#                                           
*                                  ESTIMATE NUMBER MATCH?                       
         BNE   CCAM0100            NO  - SKIP THIS ORDER                        
         LA    RF,RDARELEM                                                      
         ZIC   RE,1(RF)            BUMP TO X'02' ELEMENT                        
         AR    RF,RE                                                            
         CLI   0(RF),2             2ND DESCRIPTION ELEMENT?                     
         BNE   CCAM0100            NO  - SKIP THIS ORDER                        
         CLC   CAMPFLD1(4),RDARCLI-RDARCLEM(RF)                                 
         BNE   CCAM0100            NO CLIENT MATCH                              
         CLC   CAMPFLD2(4),RDARPRD1-RDARCLEM(RF)                                
         BNE   CCAM0100            NO PRODUCT MATCH                             
*                                                                               
CCAM0080 EQU   *                                                                
         SR    R0,R0               ACCEPTED: SET CC = ZERO                      
         B     CCAM0120                                                         
CCAM0100 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  ERROR RETURN               
CCAM0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R1,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***>>>                                                                          
*   STATION CHECK:  IF FIELD ENTERED, VALIDATE VS THIS FILTER                   
*                                                                               
STATNCHK NTR1  LABEL=*,BASE=*                                                   
         L     RF,0(R1)            SET A(KEYDEFTB KEY DEFINITION TABLE)         
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
*                                                                               
         CLI   DE7STATH+5,0        FILTER ON STATION?                           
         BE    STAT0200            NO  - EXIT CC ZERO                           
         CLC   =C'M=',DE7STAT      YES - MARKET FILTER?                         
         BE    STAT0080            YES - PROCESS THAT                           
*                                  NO  - REGULAR STATION FILTER                 
****     LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
STAT0020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    STAT0040            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     STAT0020            GO BACK FOR NEXT                             
STAT0040 EQU   *                                                                
*                                                                               
*   FOR D001/D002/D102 KEYS, STATION IS NOT IN THE KEY. RECORD                  
*        MUST BE READ TO GET THE STATION.  CAN THEN PROCEED.                    
*                                                                               
         CLI   DSTATION(RF),X'FF'  READ STATION INDICATOR?                      
         BNE   STAT0060            NO                                           
         GOTOR READAGYO                                                         
*                                  YES - READ,TEST D001/D002/D102 KEYS          
         BNZ   STAT0220            NOT EQUAL - EXIT CC NOT ZERO                 
         B     STAT0200            EQUAL     - EXIT CC ZERO                     
STAT0060 EQU   *                                                                
         LA    RE,KEY              SET A(KEY IN PROGRESS)                       
         ZIC   R1,DSTATION(RF)     GET DISPLACEMENT TO STA CALLS                
         AR    RE,R1               DISPLACE TO STATION CALL LETTERS             
         CLC   STAFILT(5),0(RE)    SAME STATION?                                
         BNE   STAT0220            NOT EQUAL - EXIT CC NOT ZERO                 
         B     STAT0200            EQUAL     - EXIT CC ZERO                     
*                                                                               
*   MARKET FILTER IS REQUESTED.  STA/MKT FIELD CONTAINS "M=AAAA"                
*        WHERE AAAA IS AN ALPHANUMERIC CODE IN WHICH THE RIGHT-                 
*        MOST POSITIONS MAY BE BINARY ZERO.                                     
*        1.   THE STATION MUST BE DETERMINED IF KEY IS CAMPAIGN KEY             
*        2.   PASSIVE KEY X'8306' MUST BE CHECKED TO DETERMINE                  
*             MARKET OF STATION                                                 
*                                                                               
STAT0080 EQU   *                                                                
*                                                                               
*   FOR ALL KEYS, MUST READ THE DETAIL RECORD TO DETERMINE THE                  
*        SUBSIDIARY REP OF THE STATION. THIS COULD BE REFINED                   
*        TO ELIMINATE SOME OVERHEAD HERE, BUT AT THIS TIME,                     
*        THERE AIN'T NO TIME.  SO NEXT TIME....                                 
*                                                                               
*   CURRENT KEY POINTS AT THE APPROPRIATE X'41' OR '51' RECORD                  
*                                                                               
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'B'                                                       
*   TEST END                                                                    
*                                                                               
         GOTOR MYGETREC                                                         
         BNZ   STAT0220            EXIT CC NOT ZERO                             
*                                                                               
         L     R6,AIO                                                           
U        USING RDARREC,R6                                                       
*                                                                               
         MVC   CURRKEY,KEY         SAVE CURRENT KEY FOR RESTART                 
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                                                        
         MVI   KEY+1,X'06'                                                      
         MVC   KEY+RST8KREP-RST8KEY(2),U.RDARKREP                               
*                                  INSERT REP INTO KEY                          
         MVC   KEY+RST8KMKT-RST8KEY(4),STAFILT+2                                
*                                  INSERT FILTER FROM SCREEN                    
         MVC   KEY+RST8KSTA-RST8KEY(5),U.RDARKSTA                               
*                                  INSERT STATION INTO KEY                      
         CLI   KEY+RST8KSTA+4-RST8KEY,C'T'                                      
*                                  BAND = 'T'?                                  
         BNE   STAT0100            NO                                           
         MVI   KEY+RST8KSTA+4-RST8KEY,C' '                                      
*                                  YES - SET TO SPACE                           
STAT0100 EQU   *                                                                
         DROP  U                                                                
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BE    STAT0120            YES - CONTINUE WITH RECORD                   
         MVC   KEY,CURRKEY         NO  - NEED TO RESTART AND SKIP               
         GOTO1 HIGH                                                             
         B     STAT0220            EXIT CC NOT ZERO                             
STAT0120 EQU   *                                                                
         MVC   KEY,CURRKEY         NO  - NEED TO RESTART AND SKIP               
         GOTO1 HIGH                                                             
STAT0200 EQU   *                                                                
* TESTING                                                                       
*&&DO                                                                           
                                                                                
         CLC   KEY+20(4),=X'23460016'                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
* TESTING                                                                       
         SR    R0,R0               SET CC ZERO                                  
         B     STAT0240            EXIT                                         
STAT0220 EQU   *                                                                
* TESTING                                                                       
*&&DO                                                                           
                                                                                
         CLC   KEY+20(4),=X'23460016'                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
* TESTING                                                                       
         LTR   RB,RB               SET CC NOT ZERO                              
STAT0240 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
*                                                                               
*   INBOX CHECK:  IF FIELD ENTERED, VALIDATE VS THIS FILTER                     
*                                                                               
INBOXCHK NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     RF,0(R1)            SET A(KEYDEFTB KEY DEFINITION TABLE)         
*                                                                               
         CLI   DE7INBXH+5,0        ANY IN-BOX FILTER?                           
         BE    INBX0200            NO  - EXIT CC ZERO                           
*                                                                               
*   IN-BOX S/P-S/P FILTERING:  CAMPAIGN KEYS DON'T CONTAIN THIS                 
*        COMPONENT, AND WON'T FILTER ON IT.                                     
*                                                                               
         LA    R3,KEY              SET A(KEY)                                   
         USING RDARKEY,R3                                                       
*                                                                               
****>>>  LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
INBX0020 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    INBX0040            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     INBX0020            GO BACK FOR NEXT                             
INBX0040 EQU   *                                                                
         CLI   DPOINTP(RF),254     P/P DATA IN THIS KEY?                        
         BE    INBX0200            NO  -  ACCEPT AS IS                          
         ZIC   RE,DPOINTP(RF)      GET DISPLACEMENT TO POINT PERSON             
         LA    RF,KEY              SET A(KEY IN PROGRESS)                       
         AR    RF,RE               DISPLACE TO IN-BOX FILTER                    
         CLC   DE7INBX(3),0(RF)    KEY FILTER = IN-BOX FILTER?                  
         BE    INBX0200            YES - EXIT CC ZERO                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     INBX0240                                                         
*                                                                               
INBX0200 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
INBX0240 EQU   *                                                                
         DROP  R3                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***>>>                                                                          
*                                                                               
*   RECEIVE DATE CHECK:  IF DATE(S) ENTERED, VALIDATE VS THIS FILTER            
*                                                                               
RECDATCK NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         CLI   DE7RDATH+5,0        RECD DATES ARE OPTIONAL                      
         BE    RECD0120                                                         
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'C'                                                       
*   TEST END                                                                    
*                                                                               
         GOTOR MYGETREC            AGENCY HEADER RECORD                         
         BZ    RECD0020            NOT DELETED                                  
         CLI   KEY+27,X'FF'        DELETED BY REGENDTR?                         
         BE    RECD0750            YES - SKIP THIS RECORD                       
RECD0020 EQU   *                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARCNTL,X'80'                                                   
         BZ    RECD0040                                                         
         TM    RDARDELS,X'20'      FOR DELETES, PASS ONLY IF RECALL             
         BZ    RECD0750            CANCELLED                                    
         DROP  R6                                                               
*                                                                               
RECD0040 DS    0H                                                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   RECD0750                                                         
         USING RDARELEM,R6                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    RECD0060                                                         
         DROP  R1                                                               
*                                                                               
         GOTOR CHKLOCAL                                                         
         BNZ   RECD0750            CHECK IF LOCAL ORDER IN USE                  
*                                                                               
RECD0060 DS    0H                                                               
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    RECD0080                                                         
         DROP  R1                                                               
*                                                                               
         GOTOR CHKDIREC                                                         
         BNZ   RECD0750            CHECK IF DIRECT ORDER IN USE                 
*                                                                               
RECD0080 DS    0H                                                               
         CLC   FILTDAT1,FILTDAT2   ONE DATE INPUT?                              
         BNE   RECD0100            NO                                           
         CLC   FILTDAT1,RDARDATE                                                
         BNE   RECD0750                                                         
         B     RECD0120                                                         
*                                                                               
RECD0100 DS    0H                                                               
         CLC   RDARDATE,FILTDAT1                                                
         BL    RECD0750                                                         
         CLC   RDARDATE,FILTDAT2                                                
         BH    RECD0750                                                         
         DROP  R6                                                               
*                                                                               
RECD0120 DS    0H                                                               
         SR    R0,R0                                                            
         B     RECD0800            EXIT CC ZERO                                 
RECD0750 DS    0H                                                               
         LTR   RB,RB               SET CC NOT ZERO                              
RECD0800 DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
*                                                                               
*   LRADDR:  KICK CODE OUT FOR ADDRESSABILITY                                   
*                                                                               
LRADDR   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         CLI   MODE,PRINTREP       REPORT REQUEST?                              
         BNE   LR000010            NO                                           
*                                                                               
LR000010 EQU   *                                                                
         MVI   REDITYPE,3          SET SCREEN TO 'BASIC SEQ'                    
         ZIC   R1,CONRECH+5        EXCEPT FOR MAKEGOOD                          
         LTR   R1,R1               NO INPUT, ORDER (DEFAULT) SEQUENCE           
         BZ    LIST0020                                                         
         BCTR  R1,0                                                             
         EX    R1,DAMN0060         LOOK FOR ORDER                               
         BE    LIST0020                                                         
         MVI   REDITYPE,1          SET SCREEN TO 'CAMPAIGN SEQ'                 
         EX    R1,DAMN0061         LOOK FOR CAMPAIGN                            
         BE    LIST0020                                                         
         MVI   REDITYPE,5          SET SCREEN TO '*BUYER SEQ'                   
         EX    R1,DAMN0062         LOOK FOR *BUYER                              
         BE    LIST0020                                                         
         MVI   REDITYPE,7          SET SCREEN TO 'FLIGHT SEQ'                   
         EX    R1,DAMN0063         LOOK FOR FLIGHT                              
         BE    LIST0020                                                         
         MVI   REDITYPE,9          SET SCREEN TO 'NEWDATE  SEQ'                 
         EX    R1,DAMN0064         LOOK FOR NEWDATE                             
         BE    LIST0020                                                         
         MVI   REDITYPE,3          SET SCREEN TO 'TODO     SEQ'                 
         EX    R1,DAMN0065         LOOK FOR NEWDATE                             
         BE    LIST0020                                                         
         MVI   REDITYPE,3          SET SCREEN TO 'TODO     SEQ'                 
         EX    R1,DAMN0066         LOOK FOR NEWDATE                             
         BE    LIST0020                                                         
         DC    H'0'                UNRECOGNIZED TYPE                            
DAMN0060 CLC   CONREC(0),=C'ORDER   '       SUBTYPE = 3                         
DAMN0061 CLC   CONREC(0),=C'CAMPAIGN'       SUBTYPE = 1                         
DAMN0062 CLC   CONREC(0),=C'*BUYER  '       SUBTYPE = 5                         
DAMN0063 CLC   CONREC(0),=C'FLIGHT  '       SUBTYPE = 7                         
DAMN0064 CLC   CONREC(0),=C'NEWDATE '       SUBTYPE = 9                         
DAMN0065 CLC   CONREC(0),=C'TODO    '       SUBTYPE = 3                         
DAMN0066 CLC   CONREC(0),=C'UNDO    '       SUBTYPE = 3                         
*                                                                               
LIST0020 EQU   *                                                                
         MVC   SCRNHDRS,REDITYPE   SAVE DATA TYPE INDICATOR                     
*                                                                               
         GOTOR SETFILTR             SET UP FILTER TABLE                         
*                                                                               
* TEST IF PRINT FROM LIST SELECT COLUMN                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***>>>                                                                          
*                                                                               
*   MYGETREC:  PERFORM MY OWN GETREC FOR DELETED X'41' RECORDS                  
*                                                                               
MYGETREC NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         TM    KEY,X'D0'                                                        
         BNO   MYGREC10                                                         
*                                                                               
         CLC   =C'D*D',DE7RQTR                                                  
***      BNE   MYGREC10                                                         
         BE    MYGREC10                                                         
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
*                                                                               
MYGREC10 DS    0H                                                               
         GOTO1 GETREC              GET THE AGENCY RECORD                        
*                                                                               
         TM    KEY,X'D0'                                                        
         BNO   MYGRYES                                                          
*                                                                               
         CLC   =C'D*D',DE7RQTR                                                  
***      BNE   MYGRYES                                                          
         BE    MYGRYES                                                          
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARCNTL,X'80'      REALLY DELETED?                              
         BO    MYGRNO                                                           
*                                                                               
MYGRYES  SR    RC,RC                                                            
MYGRNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE MARKET CODE                                                          
***********************************************************************         
VALIMKT  NTR1  LABEL=*,BASE=*                                                   
         XC    WORK,WORK                                                        
         ZIC   RF,DE7STATH+5       L(STATION FIELD)                             
         CLI   DE7STATH+5,6        FIELD CAN ONLY CONTAIN 6 CHARS MAX           
         BH    VMKT0900            ERROR                                        
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R3,KEY                                                           
         USING RMKTKEY,R3                                                       
         MVI   KEY,X'2B'                                                        
         MVC   RMKTKREP,AGENCY     INSERT REP CODE                              
         MVC   RMKTKMKT,DE7STAT+2  INSERT FOUR CHARS MKT CODE                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VMKT0900            NO  - SEND ERROR MESSAGE                     
         MVC   STAFILT(6),DE7STAT  SAVE MKT FILTER AS ENTERED                   
VMKT0800 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     VMKT0990                                                         
VMKT0900 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
VMKT0990 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE REP                                                                  
*        THIS IS NO LONGER REP CODE.  IT IS NOW ALPHA ID/POWER CODE.            
*                                                                               
***********************************************************************         
VALIREP  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
*                                                                               
*   IF SIGNING ON FROM A SUBSIDIARY, THIS FIELD IS PLUGGED IN FROM              
*        T83F00, AND CANNOT BE CHANGED.  IF SIGNON IS NOT MASTER,               
*        WHATEVER IS IN THIS FIELD DOESN'T REQUIRE VALIDATION.                  
*                                                                               
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BNO   VREP0030            NO  - ACCEPT VALUE AS ENTERED                
*                                                                               
         CLC   =C'K3',AGENCY       SIGNON TO KATZ RADIO GROUP?                  
         BNE   VREP0010            NO                                           
         CLC   =C'NU',DE7REP       YES - CLEAR CHANNEL AS SUB?                  
         BE    VREP0030            YES - ACCEPT THIS INPUT                      
VREP0010 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,1               SET REP RECORD TYPE                          
         MVC   KEY+25(2),AGENCY    INSERT ALPHA ID INTO KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                REP MUST BE ON FILE                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RREPREC,R6                                                       
         LA    RF,RREPELEM         SET A(01 ELEMENT)                            
         ZIC   RE,1(RF)                                                         
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         CLI   0(RF),2             MUST BE SUBSIDIARY REP LIST ELT              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         LR    R6,RF                                                            
         USING RREPSUB,R6                                                       
         ZIC   R0,RREPSCNT         SET LOOP CONTROL                             
         LA    RF,RREPSCOD         SET A(CODES IN LIST)                         
VREP0020 EQU   *                                                                
         CLI   0(RF),0             END OF LIST?                                 
         BE    VREP0080            YES - EXIT CC NOT ZERO                       
         CLC   DE7REP(2),0(RF)     REQUESTED ALPHA IN LIST?                     
         BE    VREP0030            YES - ACCEPT IT                              
         LA    RF,2(RF)            NO  - BUMP TO NEXT ENTRY IN TABLE            
         BCT   R0,VREP0020         GO BACK FOR NEXT                             
         B     VREP0080            SHOULDN'T HAPPEN, BUT...                     
VREP0030 EQU   *                                                                
         MVC   COMPREP,DE7REP      SAVE 2-CHAR REP ID                           
         B     VREP0060            BUMP TO NEXT ELEMENT                         
VREP0040 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   VREP0020            NO                                           
         B     VREP0080            NO X'21' - ERROR MESSAGE                     
VREP0060 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     VREP0100                                                         
VREP0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  ERROR                      
VREP0100 EQU   *                                                                
         XIT1                                                                   
VREP0800 MVC   WORK+15(0),DE7REP   LOAD SOURCE REP BY LENGTH                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R2                                                               
***>>>                                                                          
*                                                                               
******************************************************************              
* EXITH IF REC DOESN'T SATISFY NEW FILTER                                       
* EXITOKIF REC SATISFY NEW FILTER                                               
* EXITL IF NOT DEALING WITH NEW FILTER                                          
******************************************************************              
TSTFLTR  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         TM    RDARMISC,X'20'+X'10'+X'08'                                       
         BNZ   EXITH1              EXCLUDE VARIOUS, BRAND ORDER                 
*                                                                               
TST10    DS    0H                                                               
         CLI   STATFILT+1,C'E'     REVISION?                                    
         BNE   TST20                                                            
         CLI   STATFILT,1                                                       
         BL    EXITL1              NOT NEW FILTER                               
*                                                                               
         MVI   FTERFLG1,FTERBYE#   FILTER BY E1,E2,E3                           
         B     TST40                                                            
*                                                                               
TST20    DS    0H                                                               
         CLI   STATFILT+1,C'O'     ORIGINAL ORDER FILTER?                       
         BNE   TST20A                                                           
         MVI   FTERFLG1,FTERBYO                                                 
         CLI   STATFILT,C' '                                                    
         BE    TST40               DONE WITH SETTING UP FILTER                  
*                                                                               
TST20A   DS    0H                                                               
         CLI   STATFILT,1                                                       
         BL    TST20AB                                                          
         CLI   FTERFLG1,FTERBYO    O ONLY                                       
         BNE   *+12                                                             
         MVI   FTERFLG1,FTERBYO#   O1,O2,O3                                     
         B     *+8                                                              
         MVI   FTERFLG1,FTERBY#    1,2,3                                        
         B     TST40               DONE WITH SETTING UP FILTER                  
TST20AB  DS    0H                                                               
         CLI   STATFILT,C'W'                                                    
         BNE   EXITL1              NOT NEW FILTER                               
*                                                                               
TST40    DS    0H                                                               
                                                                                
TST50    DS    0H                  SET UP RECORD STATS                          
         XC    RECSTATS,RECSTATS                                                
*                                                                               
         OC    RDARRNUM,RDARRNUM   REVISION?                                    
         BZ    *+8                                                              
         OI    RECSTATS,FTERREV                                                 
*                                                                               
         CLI   RDARBSTS,C'R'       REJECT?                                      
         BNE   *+12                                                             
         OI    RECSTATS,FTERRJCT                                                
         B     TST60                                                            
*                                                                               
         CLI   RDARBSTS,C'C'       RECALL?                                      
         BNE   *+12                                                             
         OI    RECSTATS,FTERRCAL                                                
         B     TST60                                                            
*                                                                               
* WIP    CLI   RDARBSTS,C'P'       CFAPP?                                       
*        BNE   *+12                                                             
*        OI    RECSTATS,FTERRCAL                                                
* WIP    B     TST60                                                            
*                                                                               
         TM    RDARMISC,X'80'      RESENT?                                      
         BZ    *+8                                                              
         OI    RECSTATS,FTERRSNT                                                
*        B     TST70               RESENT COULD BE OVERRIDE BY OPEN             
*                                                                               
         CLI   RDARBSTS,C'A'       OPEN?                                        
         BNE   TST53                                                            
         OI    RECSTATS,FTEROPEN                                                
         B     TST55                                                            
                                                                                
TST53    DS    0H                                                               
         CLI   RDARBSTS,C'M'       AMEND?                                       
         BNE   TST53A                                                           
         OI    RECSTATS,FTERAMD                                                 
         B     TST55                                                            
TST53A   DS    0H                                                               
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   TST60                                                            
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'80'      MATCH?                                       
         BZ    TST53B                                                           
         OI    RECSTATS+1,FTERMCH                                               
         B     TST55                                                            
                                                                                
TST53B   DS    0H                  STACF?                                       
         TM    RDARFLG1,X'10'                                                   
         BZ    TST60               NO                                           
         OI    RECSTATS+1,FTERSTCF                                              
         B     TST60                                                            
*                                                                               
TST55    DS    0H                                                               
         GOTOR SSTACHK             ONLY FOR AMEND, OPEN, WE CHECK THIS          
*                                                                               
* NEED TEST FOR CFAPP, AMEND-S,AMEND-A,AMEND                                    
*                                                                               
TST60    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
TST70    DS    0H                  DONE WITH SETTING UP REC STATS               
         MVC   WORK(L'RECSTATS),RECSTATS                                        
         NI    WORK,X'FF'-FTERREV                                               
         OC    WORK(L'RECSTATS),WORK                                            
         BNZ   TST70A                                                           
         OI    RECSTATS,FTERNEW    STATUS DEFAULT TO UNLINKED                   
*&&T2                                                                           
         TM    RECSTATS,FTERREV                                                 
         BZ    *+14                                                             
         TM    RECSTATS,FTERRJCT                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
TST70A   DS    0H                                                               
*                                                                               
         XC    COMBFLTR,COMBFLTR                                                
         OC    COM2FLTR,COM2FLTR                                                
         CLI   FTERFLG1,FTERBYO    1 = O1 + O2 +O3                              
         BNE   TST80                                                            
         OC    COMBFLTR,O1                                                      
         OC    COMBFLTR,O2                                                      
         OC    COMBFLTR,O3                                                      
         B     TST100                                                           
*                                                                               
TST80    DS    0H                  FILTER EITHER BY NUMBER, OR NUMBER           
         LA    R4,ONE              FOLLOWED BY E/O                              
FLTERLP  DS    0H                                                               
         CLI   0(R4),X'00'                                                      
         BE    EXITL1                                                           
*        DC    H'0'                                                             
         CLC   0(1,R4),STATFILT                                                 
         BE    FLTERLPX                                                         
         LA    R4,FTABROWQ(R4)                                                  
         B     FLTERLP                                                          
FLTERLPX DS    0H                                                               
*                                                                               
         OC    COMBFLTR,L'ONE(R4)                                               
         CLI   FTERFLG1,FTERBYO#                                                
         BE    TST100                                                           
         CLI   FTERFLG1,FTERBYE#                                                
         BNE   TST90                                                            
         MVC   COMBFLTR,L'O1+L'ONE(R4)                                          
         B     TST100                                                           
*                                                                               
TST90    DS    0H                                                               
         MVC   COM2FLTR,L'O1+L'ONE(R4)                                          
         B     TST100                                                           
*                                                                               
TST100   DS    0H                                                               
*&&TT                                                                           
         CLC   RDARKORD,=XL4'00540001'                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         TM    RECSTATS,X'80'      IS THIS A REVISION?                          
         BO    TST200                                                           
         TM    COMBFLTR,X'80'      NO, ARE WE USING REV FILTER?                 
         BO    EXITH1              YES, EXIT                                    
*                                                                               
TST200   DS    0H                                                               
         MVC   WORK(L'COMBFLTR),COMBFLTR                                        
         NC    WORK(L'RECSTATS),RECSTATS   ARE ALL THE BITS IN RECSTATS         
         CLC   WORK(L'RECSTATS),RECSTATS   ALSO IN COMBFLTR?                    
         BE    EXITOK1             RECORD SATISFY FILTER                        
         CLI   FTERFLG1,FTERBY#                                                 
         BNE   EXITH1                                                           
         TM    RECSTATS,X'80'                                                   
         BZ    EXITH1              THIS IS NOT A REVISION                       
         MVC   WORK(L'COMBFLTR),COM2FLTR                                        
         NC    WORK(L'RECSTATS),RECSTATS                                        
         CLC   RECSTATS,WORK                                                    
         BE    EXITOK1                                                          
         B     EXITH1              RECORD DOESN'T SATISFY FILTER                
EXITOK1  CR    RB,RB                                                            
         B     TSTFLTRX                                                         
EXITL1   CLI   *,X'FF'                                                          
         B     TSTFLTRX                                                         
EXITH1   CLI   *,0                                                              
TSTFLTRX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* CHECK TO SEE IF ORDER HAS BEEN SENT TO STATION                                
* EXITOK IF GET STATUS ARE SUCCESFUL                                            
********************************************************************            
SSTACHK  NTR1  BASE=*,LABEL=*      GET CONTRACT RECORD                          
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
                                                                                
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SSTACHKX                                                         
                                                                                
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'40'      -S?                                          
         BZ    SSTACHKX                                                         
         DROP  R6                                                               
                                                                                
ST70     DS    0H                                                               
         TM    RECSTATS+1,FTERMCH    MATCH?                                     
         BZ    ST80                NO                                           
         NI    RECSTATS,X'FF'-FTERRSNT                                          
         NI    RECSTATS+1,X'FF'-FTERMCH                                         
         OI    RECSTATS+1,FTERMCHS   OPEN AND SENT TO STATION                   
         B     SSTACHKX                                                         
                                                                                
ST80     DS    0H                                                               
         TM    RECSTATS,FTEROPEN   OPEN?                                        
         BZ    ST90                NO                                           
         NI    RECSTATS,X'FF'-FTEROPEN-FTERRSNT                                 
         OI    RECSTATS,FTEROPS    OPEN AND SENT TO STATION                     
         B     SSTACHKX                                                         
                                                                                
ST90     DS    0H                                                               
         TM    RECSTATS,FTERAMD                                                 
         BZ    SSTACHKX                                                         
         NI    RECSTATS,X'FF'-FTERRSNT                                          
         NI    RECSTATS+1,X'FF'-FTERAMD                                         
         OI    RECSTATS+1,FTERAMDS AMEND AND SENT TO STATION                    
SSTACHKX DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* RETRIEVE THE RECORD FOR THE KEY.  TEST THE STATION WITHIN THAT                
*    RECORD.  IF STATION MATCH, CC = ZERO, ELSE CC NOT ZERO                     
*                                                                               
READAGYO NTR1  BASE=*,LABEL=*                                                   
         GOTO1 GETREC RETRIEVE AGENCY ORDER RECORD                              
         L     R6,AIO                                                           
U        USING RDARREC,R6                                                       
*                                                                               
         CLC   STAFILT(5),RDARKSTA SAME STATION?                                
*                                                                               
         DROP  R6                                                               
*                                                                               
         BE    RAGO0040            YES                                          
         LTR   RB,RB               NO  - SET CC NOT ZERO                        
         B     RAGO0100                                                         
RAGO0040 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
RAGO0100 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK IF LOCAL SIGNON HOME MARKET IN USE                                      
* IF LOCAL SIGNON HOME MARKET IN USE, ONLY PASS LOCAL ORDERS BY                 
* FILTERING SIGNON ID AGAINST RECEIVER ID IN DARE HEADER RECORD                 
*                                                                               
* ASSUMES AIO HAS DARE HEADER RECORD                                            
*                                                                               
CHKLOCAL NTR1  BASE=*,LABEL=*                                                   
         MVI   HALF,C'N'                                                        
         LR    R4,RA               USE R2                                       
         AH    R4,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    CHKLYES                                                          
         DROP  R4                                                               
                                                                                
         CLI   SIGNONID+4,C'L'                                                  
         BNE   CHKLOC10                                                         
*                                                                               
* LOCAL SIGNON HOME MARKET IN USE, ONLY PASS LOCAL ORDERS BY FILTERING          
* SIGNON ID AGAINST RECEIVER ID IN DARE HEADER RECORD                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKLYES                                                          
         USING RDARELEM,R6                                                      
         CLC   RDARRCVR,SIGNONID                                                
         BNE   CHKLNO                                                           
         B     CHKLYES                                                          
         DROP  R6                                                               
*                                                                               
* HOME MARKET IS IN USE BUT SIGNON IS NATIONAL. WE NEED TO BYPASS               
* ANY ORDERS THAT IS HOME MARKET                                                
*                                                                               
CHKLOC10 DS    0H                                                               
         L     R4,AIO                                                           
DARKEYD  USING RDARKEY,R4                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKLYES                                                          
         USING RDARELEM,R6                                                      
         CLI   RDARRCVR+4,C'L'                                                  
         BNE   CHKLYES                                                          
         CLC   RDARRCVR(4),DARKEYD.RDARKSTA                                     
         BE    CHKLNO              ORDER IS HOME MARKET, SKIP                   
         DROP  R6,DARKEYD                                                       
*                                                                               
CHKLYES  EQU   *                                                                
         MVI   HALF,C'Y'                                                        
         SR    RC,RC                                                            
CHKLNO   LTR   RC,RC                                                            
CHKLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF STATION SIGNON DIRECT ORDER PROCESSING IN USE                        
* IF DIRECT ORDER PROCESSING, ONLY PASS ORDERS BY                               
* FILTERING SIGNON ID AGAINST RECEIVER ID IN DARE HEADER RECORD                 
*                                                                               
* ASSUMES AIO HAS DARE HEADER RECORD                                            
***********************************************************************         
CHKDIREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* LOCAL SIGNON HOME MARKET IN USE, ONLY PASS LOCAL ORDERS BY FILTERING          
* SIGNON ID AGAINST RECEIVER ID IN DARE HEADER RECORD                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKDYES                                                          
         USING RDARELEM,R6                                                      
         CLC   RDARRCVR,SIGNONID                                                
         BNE   CHKDNO                                                           
         DROP  R6                                                               
*                                                                               
CHKDYES  SR    RC,RC                                                            
CHKDNO   LTR   RC,RC                                                            
CHKDX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALKEY                                                                        
***********************************************************************         
SUBVKEY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'UNDO',CONREC      UNDO?                                       
         BNE   VK00                                                             
*                                                                               
         LA    R2,DE7PDATH                                                      
         CLI   DE7PDATH+5,0                                                     
         BE    INVLFLD2                                                         
         GOTO1 PERVAL,DMCB,(DE7PDATH+5,DE7PDATH+8),DATEBLCK                     
         CLI   DMCB+4,0            FIELDS 1 & 2 VALID                           
         BE    VK00A                                                            
         CLI   DMCB+4,4            ONLY ONE DATE INPUT                          
         BNE   INVRDATE                                                         
         MVC   SVPDATE,DATEBLCK+34                                              
*                                                                               
VK00A    DS    0H                                                               
         LA    R2,DE7PTIMH                                                      
         CLI   DE7PTIM+5,0                                                      
         BE    INVLFLD2                                                         
*                                                                               
*        GOTOR VALTIME                                                          
*                                                                               
*                                                                               
         OI    MYFLAG,MFGUNDOP                                                  
*                                                                               
         CLI   OFFLINE,C'Y'        DO NOT GENERATE ANY REPORT                   
         BNE   *+12                FOR OFFLINE UNDO                             
         L     RD,BASERD                                                        
         B     EXIT2                                                            
*                                                                               
VK00     DS    0H                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   VK01                NO                                           
         CLC   DE7PNUM,C'0'        NO RECORDS WERE PROCESSED ONLINE?            
         BNE   VK01                                                             
         L     RD,BASERD           YES, THEN WE DO NOT NEED TO                  
         B     EXIT2               PROCESS ANY RECORD OFFLINE                   
*                                                                               
                                                                                
VK01     DS    0H                                                               
         LA    R2,DE7RQTRH                                                      
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BZ    VK03                                                             
         CLI   5(R2),0                                                          
         BNE   VK02                                                             
         XC    REQINIT,REQINIT                                                  
         B     VK05                                                             
*                                                                               
VK02     DS    0H                                                               
         CLI   5(R2),3                                                          
         BNE   BADREQTR                                                         
         MVC   REQINIT,DE7RQTR                                                  
         B     VK05                                                             
*                                                                               
VK03     DS    0H                                                               
         MVC   DE7RQTR,REQINIT                                                  
*                                                                               
VK05     DS    0H                                                               
                                                                                
         MVI   NLISTS,7            LONG LIST                                    
                                                                                
         LA    R2,DE7HDLNH                                                      
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         XC    HAGYORDR,HAGYORDR   CLEAR AGENCY ORDER FILTER                    
         CLC   =C'O=',DE1HDLN      AGENCY ORDER FILTER?                         
         BNE   VK07                NO  -                                        
         CLI   DE1AGYH+5,0         AGENCY FILTER ENTERED?                       
         BE    NEEDAGY2            NO AGENCY FILTER: ERROR                      
         GOTO1 HEXIN,DMCB,DE1HDLN+2,HAGYORDR,8                                  
         B     VK50                                                             
VK07     EQU   *                                                                
*                                                                               
*   AS THERE IS NO CONTRACT AT THE MASTER LEVEL, THE USE OF THE                 
*        CAMPAIGN FILTER IS IGNORED IF THE SIGNON IS 'MASTER'.                  
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BO    VK50                YES - DON'T PROCESS SINGLE                   
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   NLISTS,4            SHORT LIST W/CONTRACT HEADER                 
                                                                                
*        TM    4(R2),X'20'         PREVIOUSLY VALID?                            
*        BNZ   VK50                                                             
                                                                                
         GOTO1 VALICON,DMCB,(R2)                                                
         BNZ   MISSREC2                                                         
         OI    4(R2),X'20'                                                      
         XC    SELCTKEY,SELCTKEY                                                
                                                                                
         CLI   ACTNUM,ACTLIST      FOR LIST WITH CONTRACT HEADER                
         BNE   VK50                SPECIFIED, STUFF FILTER WITH                 
         CLC   =C'C=',DE7CMP1      CAMPAIGN FILTER?                             
         BE    VK50                YES - DON'T STUFF ANYTHING                   
VK08     EQU   *                                                                
         MVC   DE7STAT(6),ESTATION INSERT FILTERS FROM CONTRACT                 
         OI    DE7STATH+4,X'20'                                                 
         MVI   DE7STATH+5,6                                                     
         OI    DE7STATH+6,X'80'    XMIT                                         
         CLC   DE7STAT+4(2),SPACES                                              
         BNE   *+10                                                             
         MVC   DE7STAT+4(2),=C'-T'                                              
                                                                                
         MVC   STAFILT,ESTATION    SAVE FOR LIST FILTERING                      
         CLI   STAFILT+4,C' '                                                   
         BNE   VK10                                                             
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
         B     VK15                                                             
VK10     DS    0H                                                               
         CLI   STAFILT+4,C'-'                                                   
         BNE   VK15                                                             
         MVC   STAFILT+4(1),STAFILT+5     SLIDE MEDIA DOWN                      
         MVI   STAFILT+5,C' '             CLEAR OLD MEDIA                       
         B     VK15                                                             
VK15     DS    0H                                                               
         OI    DE7AGYH+4,X'20'                                                  
         XC    DE7AGY,DE7AGY                                                    
         MVC   DE7AGY(4),CCONKAGY  AGENCY                                       
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    VK20                                                             
         MVI   DE7AGY+4,C' '                                                    
         LA    RE,DE7AGY                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
VK20     DS    0H                                                               
         MVC   AGYFILT,CDARAGY     SAVE FOR LIST FILTERING                      
         OI    DE7AGYH+6,X'80'     XMIT                                         
                                                                                
VK50     DS    0H                                                               
*        MVI   MYRECTYP,X'41'      DEFAULT                                      
         LA    R2,DE7TYPEH                                                      
         TM    4(R2),X'20'                                                      
         BO    VK90                                                             
*                                                                               
         XC    SELCTKEY,SELCTKEY                                                
         MVC   STATFILT,DE7TYPE                                                 
         OC    STATFILT,SPACES                                                  
         LA    R3,STATLIST                                                      
VK55     CLC   STATFILT(1),0(R3)                                                
         BE    VK60                                                             
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    INVLFLD2                                                         
         B     VK55                                                             
*                                                                               
VK60     DS    0H                                                               
         LA    R3,STATLIST                                                      
VK70     CLC   STATFILT+1(1),0(R3)                                              
         BE    VK80                                                             
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    INVLFLD2                                                         
         B     VK70                                                             
*                                                                               
* VARIOUS FILTER WILL ALWAYS BE PLACE IN STATFILT+1, ALL OTHER REGULAR          
* FILTERS WILL GO IN THE FIRST BYTE OF STATFILT                                 
*                                                                               
* REVISION FILTER WILL ALWAYS BE PLACE IN STATFILT+1, ALL OTHER REGULAR         
* FILTERS WILL GO IN THE FIRST BYTE OF STATFILT                                 
*                                                                               
VK80     DS    0H                                                               
         CLI   STATFILT,C'V'                                                    
         BNE   VK85                                                             
         MVC   STATFILT(1),9(R2)                                                
         MVI   STATFILT+1,C'V'                                                  
         OC    STATFILT,SPACES                                                  
         B     VK88                                                             
*                                                                               
VK85     DS    0H                                                               
         CLI   STATFILT,C'E'                                                    
         BNE   VK88                                                             
         MVC   STATFILT(1),9(R2)                                                
         MVI   STATFILT+1,C'E'                                                  
         OC    STATFILT,SPACES                                                  
         B     VK88                                                             
*                                                                               
VK88     DS    0H                                                               
         CLI   STATFILT,C'O'                                                    
         BNE   VK88A                                                            
         MVC   STATFILT(1),9(R2)                                                
         MVI   STATFILT+1,C'O'                                                  
         OC    STATFILT,SPACES                                                  
         B     VK88A                                                            
*                                                                               
VK88A    DS    0H                                                               
*        CLI   STATFILT,C'F'       CONFIRMED FILTER?                            
*        BNE   *+8                 NO                                           
*        MVI   MYRECTYP,X'51'      YES - USE 51 RECORDS                         
         B     VK90                                                             
*                                                                               
*                                                                               
* LIST OF VALID FILTERS                                                         
*                                                                               
STATLIST DC    C' ULBARNCSVWIE*FO123PM'                                         
         DC    X'FF'                                                            
*                                                                               
VK90     DS    0H                  GROUP/SUBGROUP FILTER                        
         LA    R2,DE7GRPH                                                       
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK100                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    GRPFILT,GRPFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         GOTO1 VALIGRP                                                          
         BNE   INVLGRP2                                                         
         MVC   GRPFILT,8(R2)                                                    
         OC    GRPFILT,SPACES                                                   
                                                                                
VK100    DS    0H                  DIV/TEAM FILTER                              
         LA    R2,DE7TEAMH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK110                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    TEAMFILT,TEAMFILT                                                
         CLI   5(R2),0                                                          
         BE    VK110                                                            
         GOTO1 VALITEAM                                                         
         BNE   INVTEAM                                                          
         MVC   TEAMFILT,8(R2)                                                   
         OC    TEAMFILT,SPACES                                                  
                                                                                
VK110    DS    0H                                                               
         LA    R2,DE7UNWH          ANY FLAG IN UNWIRED?                         
         TM    DE7UNWH+4,X'20'     PREVIOUSLY VALID?                            
         BO    VK115020            YES - DON'T CHECK AGAIN                      
         NI    DE7REPH+4,X'FF'-X'20'                                            
*                                  NO  - FORCE REVALID OF REP FIELD             
         CLI   5(R2),0                                                          
         BE    VK115020            NO                                           
         CLC   =C'MR',AGENCY       KATZ TV?                                     
         BE    VK112               YES                                          
         CLC   =C'K3',AGENCY       KATZ RADIO?                                  
         BE    VK112               YES                                          
         CLC   =C'IR',AGENCY       INTEREP?                                     
         BE    VK112               YES                                          
         CLC   =C'MS',AGENCY       TEST?                                        
         BNE   VK115010            NO                                           
VK112    EQU   *                                                                
         CLI   8(R2),C'Y'          MASTER: CAN ONLY ASK FOR UNWIRED             
         BNE   MASUNWNG            ERROR                                        
VK115010 EQU   *                                                                
         CLI   8(R2),C'Y'          YES ENTERED?                                 
         BE    VK115020            YES                                          
         CLI   8(R2),C'N'          NO  ENTERED?                                 
         BNE   INVLUNW             NO                                           
VK115020 EQU   *                                                                
*                                                                               
*   NEED TO ADD M= MARKET VALIDATION HERE                                       
*                                                                               
         LA    R2,DE7STATH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK120                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    STAFILT,STAFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK120                                                            
         CLC   =C'M=',DE7STAT      REQUEST FOR MARKET FILTER?                   
         BNE   VK115               NO                                           
         GOTOR VALIMKT                                                          
         BNZ   INVLMKT2                                                         
         B     VK120                                                            
VK115    EQU   *                                                                
         GOTO1 VALISTA                                                          
         MVC   STAFILT,WORK                                                     
         OC    STAFILT,SPACES                                                   
         CLI   STAFILT+4,C' '                                                   
         BNE   VK120                                                            
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
                                                                                
VK120    DS    0H                                                               
         LA    R2,DE7AGYH                                                       
                                                                                
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK160                                                            
                                                                                
         XC    SELCTKEY,SELCTKEY                                                
         XC    AGYFILT,AGYFILT                                                  
                                                                                
         CLI   5(R2),0                                                          
         BE    VK160                                                            
                                                                                
         LA    R6,KEY              NEED TO GO OUT AND RETRIEVE THE              
         XC    KEY,KEY             AGENCY EQUIVALENCY CODE                      
         USING RAGY2KEY,R6                                                      
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,SPACES                                                  
         MVC   RAGK2REP,AGENCY                                                  
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(L'DE7AGY),DE7AGY                                            
         OC    WORK,SPACES                                                      
         LA    RE,WORK                                                          
                                                                                
* CHECK FOR AGENCY OFFICE                                                       
         CLI   0(RE),C'-'                                                       
         BE    VK130                                                            
         CLI   0(RE),C' '                                                       
         BE    VK140                                                            
         LA    RE,1(RE)                                                         
         B     *-20                                                             
                                                                                
VK130    MVC   RAGK2AOF,1(RE)      AGENCY OFFICE                                
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,MOVEAGY                                                       
         B     VK150                                                            
*                                                                               
MOVEAGY  MVC   RAGK2AGY(0),WORK                                                 
*                                                                               
VK140    MVC   RAGK2AGY(4),WORK                                                 
         OC    RAGK2AOF,SPACES     SPACE PAD OFFICE                             
         DROP  R6                                                               
*                                                                               
VK150    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   MISDARA2            CHECK IF EQUIVALENCY CODE PRESENT            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RAGY2REC,R6                                                      
         MVC   AGYFILT,RAGY2DAR                                                 
         DROP  R6                                                               
                                                                                
         OC    AGYFILT,AGYFILT     CHECK IF EQUIVALENCY CODE PRESENT            
         BZ    MISDARA2                                                         
         CLC   AGYFILT,SPACES                                                   
         BE    MISDARA2                                                         
*                                                                               
VK160    DS    0H                                                               
         LA    R2,DE7OFFH                                                       
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK200                                                            
         XC    SELCTKEY,SELCTKEY                                                
*                                                                               
* HOME MARKET SIGNON DO NOT NEED TO SET OFFICE RESTRICTION FILTER               
* AND NEITHER DOES DIRECT ORDER PROCESSING (CONTRACT PROFILE 51)                
* WHEN SIGNONID IS 4 CHARS OR LESS                                              
*                                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    VK162                                                            
         CLI   SIGNONID+4,C' '                                                  
         BE    VK200                                                            
*                                                                               
VK162    DS    0H                                                               
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    VK163                                                            
         DROP  R4                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    VK200                                                            
*                                                                               
VK163    DS    0H                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VK180                                                            
         CLC   =X'800F',TWAAUTH    CHECK ACCESS                                 
         BE    VK180                                                            
         CLC   =C'O=',TWAACCS                                                   
         BNE   VK180                                                            
*                                                                               
         CLI   DE7HDLNH+5,0                                                     
         BNE   VK165                                                            
*                                                                               
         CLI   5(R2),0             DEFAULT OFFICE RESTRICTION                   
         BNE   VK170                                                            
         MVC   DE7OFF(2),TWAACCS+2                                              
         MVI   DE7OFFH+5,2                                                      
         B     VK190                                                            
*                                                                               
VK165    DS    0H                                                               
         CLC   =C'BRAND',CONREC                                                 
         BE    VK190                                                            
         MVC   DE7OFF(2),=C'C='                                                 
         MVC   DE7OFF+2(2),TWAACCS+2                                            
         MVI   DE7OFFH+5,4                                                      
         B     VK190                                                            
*                                                                               
VK170    DS    0H                                                               
*********                                                                       
         CLC   =C'BL',AGENCY                                                    
         BNE   VK175                                                            
         CLC   =C'SA',TWAACCS+2                                                 
         BNE   VK178                                                            
         CLC   =C'PO',DE7OFF                                                    
         BE    VK190                                                            
         CLC   =C'C=PO',DE7OFF                                                  
         BE    VK190                                                            
         B     VK178                                                            
VK175    DS    0H                                                               
         CLC   =C'PV',AGENCY                                                    
         BNE   VK176                                                            
         CLC   =C'SE',TWAACCS+2                                                 
         BNE   VK178                                                            
         CLC   =C'PO',DE7OFF                                                    
         BE    VK190                                                            
         CLC   =C'C=PO',DE7OFF                                                  
         BE    VK190                                                            
VK176    DS    0H                                                               
         CLC   =C'CQ',AGENCY                                                    
         BNE   VK177                                                            
         CLC   =C'LA',TWAACCS+2                                                 
         BNE   VK177A                                                           
         CLC   =C'SF',DE7OFF                                                    
         BE    VK190                                                            
         CLC   =C'C=SF',DE7OFF                                                  
         BE    VK190                                                            
*                                                                               
* SPECIAL CODING UNTIL C-FILE RESOLVES THIS OFFICE CODE MAPPING PROB.           
*                                                                               
VK177    DS    0H                                                               
         CLC   =C'SZ',AGENCY                                                    
         BE    VK177A                                                           
         CLC   =C'AM',AGENCY                                                    
         BNE   VK178                                                            
*                                                                               
VK177A   DS    0H                                                               
         CLC   =C'DV',TWAACCS+2                                                 
         BNE   VK178                                                            
         CLC   =C'DN',DE7OFF                                                    
         BE    VK190                                                            
         CLC   =C'DV',DE7OFF                                                    
         BNE   VK178                                                            
         MVC   DE7OFF(2),=C'DN'                                                 
         B     VK190                                                            
*********                                                                       
VK178    DS    0H                                                               
         CLC   DE7OFF(2),TWAACCS+2                                              
         BE    VK180                                                            
         CLC   =C'C=',DE7OFF                                                    
         BNE   NOACCESS                                                         
         CLC   DE7OFF+2(2),TWAACCS+2                                            
         BNE   NOACCESS                                                         
*                                                                               
VK180    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VK200                                                            
*                                                                               
VK190    DS    0H                                                               
         MVC   OFFFILT,8(R2)                                                    
         CLC   =C'C=',DE7OFF                                                    
         BNE   VK200                                                            
*                                                                               
VK195    DS    0H                                                               
         GOTO1 VALIOFF             VALIDATE ONLY IF FILTERING ON                
         BNE   INVLOFF2            CONTRACT OFFICE                              
         MVC   OFFFILT,10(R2)                                                   
                                                                                
VK200    DS    0H                                                               
         LA    R2,DE7SALPH                                                      
         TM    DE7SALPH+4,X'20'                                                 
         BO    VK212                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    SALFILT,SALFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK212                                                            
         CLI   DE7OFFH+5,0         SALESPERSON FILTER NEEDS OFFICE              
         BNE   VK210                                                            
         LA    R2,DE7OFFH                                                       
         B     NEEDOFF                                                          
                                                                                
VK210    DS    0H                                                               
         GOTO1 VALISAL             REVALIDATE EVERYTIME                         
         BNE   INVLSAL                                                          
         CLC   OFFFILT,WORK+20     SALESPERSON OFFICE MUST MATCH OFFICE         
         BNE   INVLSAL             FILTER                                       
         MVC   SALFILT,8(R2)                                                    
         OC    SALFILT,SPACES                                                   
*                                                                               
VK212    DS    0H                                                               
         LA    R2,DE7REPH                                                       
***      TM    DE7REPH+4,X'20'     VALIDATE REP EVERY TIME.                     
***      BO    VK220                                                            
         XC    COMPREP,COMPREP                                                  
         CLI   5(R2),0             ANY INPUT IN FIELD?                          
         BE    VK220               NO                                           
         GOTOR VALIREP             REVALIDATE EVERYTIME                         
         BNZ   INVLREP2            REP NOT FOUND                                
*                                                                               
VK220    DS    0H                                                               
         LA    R2,DE7RDATH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK250                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    FILTDAT1(4),FILTDAT1                                             
         CLI   5(R2),0                                                          
         BE    VK250                                                            
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),DATEBLCK                               
         CLI   DMCB+4,0            FIELDS 1 & 2 VALID                           
         BE    VK230                                                            
         CLI   DMCB+4,4            ONLY ONE DATE INPUT                          
         BNE   INVRDATE                                                         
*                                                                               
VK230    MVC   FILTDAT1,DATEBLCK+34 COMPRESSED FIRST DATE                       
         MVC   FILTDAT2,DATEBLCK+36 COMPRESSED LAST DATE                        
*                                                                               
VK250    EQU   *                                                                
         LA    R2,DE7CMP1H                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK300                                                            
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         XC    CAMPFLD1,CAMPFLD1   CLEAR CAMPAIGN FILTER FIELDS                 
         XC    CAMPFLD2,CAMPFLD2   CLEAR CAMPAIGN FILTER FIELDS                 
         XC    CAMPFLD3,CAMPFLD3   CLEAR CAMPAIGN FILTER FIELDS                 
*                                                                               
         DROP  R4                                                               
*                                                                               
         CLI   DE7CMP1H+5,0        CAMPAIGN FILTER REQUESTED?                   
         BE    VK300               NO  - FIRST FIELD SPEAKS FOR ALL             
         CLC   =C'C=',DE7CMP1      YES - PULL FILTER FROM CONTRACT?             
         BE    VK260               YES                        ,                 
VK255    EQU   *                                                                
         CLI   DE7AGYH+5,0         AGENCY FILTER ENTERED?                       
         BE    NEEDAGY             NO AGENCY FILTER: ERROR                      
         CLI   DE7CMP2H+5,0        NO  - ALL 3 FIELDS MUST BE FILLED            
         BE    INVLCAMP            EMPTY:  ERROR                                
         CLI   DE7CMP3H+5,0                                                     
         BE    INVLCAMP            EMPTY:  ERROR                                
*                                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
*                                                                               
*   VALIDATION FOR THESE FIELDS IS UNCERTAIN.  MUST BE TESTED.                  
*                                                                               
         MVC   CAMPFLD1,DE7CMP1    USE AS ENTERED                               
         OC    CAMPFLD1,SPACES                                                  
         MVC   CAMPFLD2,DE7CMP2    USE AS ENTERED                               
         OC    CAMPFLD2,SPACES                                                  
         LA    R2,DE7CMP3H         SET A(ESTIMATE FIELD)                        
         GOTO1 VPACK               PACK ESTIMATE NUMBER                         
         LTR   R0,R0                                                            
         BZ    BADEST#2            ERROR IN INPUT                               
         SPACE 1                                                                
         STCM  R0,15,CAMPFLD3      STORE CAMPAIGN ESTIMATE #                    
*                                                                               
         DROP  R4                                                               
         B     VK300                                                            
VK260    EQU   *                                                                
         CLI   DE7HDLNH+5,0        CONTRACT NUMBER ENTERED?                     
         BE    NEEDCON             NO CONTRACT NUMBER: ERROR                    
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),CCONDKAD  INSERT D/A OF CON                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A2'        RETRIEVE EASI CODE ELEMENT                   
         BRAS  RE,GETEL                                                         
         BNE   NOCMPCD             NO ELEMENT - NO FILTER                       
*                                                                               
         XC    DE7HDLN,DE7HDLN     CLEAR CONTRACT NUMBER                        
         MVI   DE7HDLNH+5,0        SET FIELD LENGTH TO ZERO                     
         FOUT  DE7HDLNH                                                         
         XC    CDARNUM,CDARNUM                                                  
*                                  STUFF AGENCY CODE INTO SCREEN                
         OI    DE7AGYH+4,X'20'                                                  
         XC    DE7AGY,DE7AGY                                                    
         MVC   DE7AGY(4),CCONKAGY  AGENCY                                       
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    VK265                                                            
         MVI   DE7AGY+4,C' '                                                    
         LA    RE,DE7AGY                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
VK265    DS    0H                                                               
         MVC   AGYFILT,CDARAGY     SAVE FOR LIST FILTERING                      
         OI    DE7AGYH+6,X'80'     XMIT                                         
                                                                                
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
*                                                                               
*   VALIDATION FOR THESE FIELDS IS UNCERTAIN.  MUST BE TESTED.                  
*                                                                               
         MVC   CAMPFLD1,RCONIADV-RCONIEL(R6)                                    
         MVC   CAMPFLD2,RCONIPRD-RCONIEL(R6)                                    
         MVC   CAMPFLD3,RCONIEST-RCONIEL(R6)                                    
*                                  EST# IS DISP FLD IN REP CONTRACT             
         MVC   DE7CMP1,CAMPFLD1                                                 
         MVC   DE7CMP2,CAMPFLD2                                                 
         MVC   DE7CMP3,CAMPFLD3                                                 
*                                  INSERT ORIG FORMAT INTO SCREEN               
         LA    R0,4                SET LOOP CONTROL                             
*                                  SCAN BACKWARD FOR COUNT                      
         LA    R1,CAMPFLD3+3       SET A(LAST CHARACTER IN FIELD)               
VK270    EQU   *                                                                
         CLI   0(R1),C' '          VALID CHARACTER IN FIELD?                    
         BH    VK275               YES - FINISHED                               
         BCTR  R1,0                BACK UP ONE CHARACTER                        
         BCT   R0,VK270            DECREMENT ONE COUNT                          
*                                  DROP THROUGH WILL RETURN ERROR               
VK275    EQU   *                                                                
         STC   R0,DE7CMP3H+5       SET FIELD LENGTH TO WHAT'S LEFT              
VK277    EQU   *                                                                
         CLI   0(R1),C'0'                                                       
         BL    VK279               LESS THAN X'F0': NON-NUMERIC                 
         CLI   0(R1),C'9'                                                       
         BH    VK279               MORE THAN X'F9': NON-NUMERIC                 
         BCTR  R1,0                BACK UP ONE CHARACTER                        
         BCT   R0,VK277            DECREMENT ONE COUNT                          
*                                  DROP-THROUGH:  ALL NUMERIC                   
         OI    DE7CMP3H+4,X'08'    TURN ON VALID NUMERIC                        
VK279    EQU   *                                                                
*                                                                               
         FOUT  DE7CMP1H                                                         
         FOUT  DE7CMP2H                                                         
         FOUT  DE7CMP3H                                                         
*                                  FOUT THE ORIGINAL FORMAT, THEN               
*                                     CONVERT ORIGINAL TO BINARY, WITH          
*                                     ERROR IF NOT NUMERIC                      
*                                                                               
         LA    R2,DE7CMP3H         SET A(ESTIMATE FIELD)                        
         GOTO1 VPACK               PACK ESTIMATE NUMBER                         
         LTR   R0,R0                                                            
         BZ    BADEST#2            ERROR IN INPUT                               
         SPACE 1                                                                
         STCM  R0,15,CAMPFLD3      STORE CAMPAIGN ESTIMATE #                    
*                                                                               
         DROP  R4                                                               
         LTR   R0,R0               A PLACE TO HANG A DUMP                       
*                                                                               
*                                                                               
*   NEED TO CHECK IF CONTRACT ENTERED, PULL CAMPAIGN FROM CONTRACT,             
*        STORE IN TWA, AND REMOVE CONTRACT FILTER.                              
*                                                                               
VK300    DS    0H                                                               
         OI    DE7STATH+4,X'20'                                                 
         OI    DE7GRPH+4,X'20'                                                  
         OI    DE7TEAMH+4,X'20'                                                 
         OI    DE7STATH+4,X'20'                                                 
         OI    DE7AGYH+4,X'20'                                                  
         OI    DE7OFFH+4,X'20'                                                  
         OI    DE7SALPH+4,X'20'                                                 
         OI    DE7UNWH+4,X'20'                                                  
         OI    DE7REPH+4,X'20'                                                  
         OI    DE7RDATH+4,X'20'                                                 
         OI    DE7CMP1H+4,X'20'                                                 
         OI    DE7CMP2H+4,X'20'                                                 
         OI    DE7CMP3H+4,X'20'                                                 
*                                                                               
VKX      DS    0H                                                               
         CLI   OFFLINE,C'Y'        AM I OFFLINE?                                
         BE    VKX10               YES                                          
         TM    MYFLAG,MFGUNDOP     AM I UNDOING?                                
         BO    VKX10                                                            
* IF ONLINE, THEN MARK THE PRINTING DATE/TIME, LATER ON IN LR, WE               
* WILL MARK THE RECORD WITH PRINTING DATE/TIME, SO THAT WHEN OFFLINE            
* WE CAN MATCH THE RECORDS.                                                     
         GOTO1 DATCON,DMCB,(5,0),(5,DE7PDAT)                                    
         GOTO1 DATCON,DMCB,(5,0),(2,SVPDATE)                                    
         MVI   DE7PDATH+5,8                                                     
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,DUB                                                         
         MVC   SVPTIME,DUB                                                      
         GOTO1 HEXOUT,DMCB,DUB,WORK,3                                           
         MVC   DE7PTIM(2),WORK                                                  
         MVI   DE7PTIM+2,C':'                                                   
         MVC   DE7PTIM+3(2),WORK+2                                              
         MVI   DE7PTIM+5,C':'                                                   
         MVC   DE7PTIM+6(2),WORK+4                                              
         MVI   DE7PTIMH+5,8                                                     
*                                                                               
         MVC   DE7LAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
*                                                                               
VKX10    DS    0H                                                               
EXIT2    DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***************************************************************                 
* VALTIME: VALIDATE TIME                                                        
***************************************************************                 
VALTIME  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK(2),DE7PTIM                                                
         MVC   MYWORK+2(2),DE7PTIM+3                                            
         MVC   MYWORK+4(2),DE7PTIM+6                                            
         GOTO1 HEXIN,DMCB,WORK,MYWORK,6                                         
         OC    DMCB,DMCB                                                        
         BZ    INVLFLD2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,WORK                                                        
         CHI   R1,24                                                            
         BH    INVLFLD2                                                         
         CHI   R1,0                                                             
         BL    INVLFLD2                                                         
         ICM   R1,3,WORK+2                                                      
         CHI   R1,60                                                            
         BH    INVLFLD2                                                         
         CHI   R1,0                                                             
         BL    INVLFLD2                                                         
         ICM   R1,3,WORK+4                                                      
         CHI   R1,60                                                            
         BH    INVLFLD2                                                         
         CHI   R1,0                                                             
         BL    INVLFLD2                                                         
VALTX    DS    0H                                                               
         XIT1                                                                   
*&&                                                                             
MISSREC2 MVC   RERROR,=AL2(NOTFOUND)                                            
         B     ERREND3                                                          
*                                                                               
INVLFLD2 MVC   RERROR,=AL2(2)                                                   
         B     ERREND3                                                          
*                                                                               
INVLGRP2 MVC   RERROR,=AL2(445)    INVALID GROUP                                
         B     ERREND3                                                          
*                                                                               
INVTEAM  MVC   RERROR,=AL2(446)    INVALID TEAM                                 
         B     ERREND3                                                          
*                                                                               
MISDARA2 MVC   RERROR,=AL2(430)    MISSING AGY EQUIVALENCY CODE                 
         B     ERREND3                                                          
*                                                                               
INVLOFF2 MVC   RERROR,=AL2(447)    INVALID OFFICE                               
         B     ERREND3                                                          
*                                                                               
INVLMKT2 MVC   RERROR,=AL2(353)    INVALID MARKET                               
         B     ERREND3                                                          
*                                                                               
INVLREP2 MVC   RERROR,=AL2(925)    INVALID COMPANY                              
         B     ERREND3                                                          
*                                                                               
BADEST#2 MVC   RERROR,=AL2(929)    INVALID ESTIMATE # IN CAMPAIGN               
         B     ERREND3                                                          
*                                                                               
INVLSAL  MVC   RERROR,=AL2(448)    INVALID SALESPERSON                          
         B     ERREND3                                                          
*                                                                               
NEEDAGY  MVC   RERROR,=AL2(927)    CAMPAIGN FILTER NEEDS AGENCY                 
         B     ERREND3                                                          
*                                                                               
NEEDAGY2 MVC   RERROR,=AL2(955)    CAMPAIGN FILTER NEEDS AGENCY                 
         B     ERREND3                                                          
*                                                                               
NEEDCON  MVC   RERROR,=AL2(938)    CAMPAIGN FILTER NEEDS CONTRACT #             
         B     ERREND3                                                          
*                                                                               
NOCMPCD  MVC   RERROR,=AL2(939)    CONTRACT SELECTED CONTAINS NO                
         B     ERREND3                CLIENT/PROD/ESTIMATE CODES                
*                                                                               
INVLCAMP MVC   RERROR,=AL2(926)    EMPTY CAMPAIGN FIELD                         
         B     ERREND3                                                          
*                                                                               
INVLUNW  MVC   RERROR,=AL2(945)    UNWIRED MUST BE Y OR N                       
         B     ERREND3                                                          
*                                                                               
MASUNWNG MVC   RERROR,=AL2(946)    UNWIRED MUST BE Y FOR MASTER                 
         B     ERREND3                                                          
*                                                                               
INVRDATE MVC   RERROR,=AL2(INVDATE) INVALID RCVD DATE FILTER                    
         B     ERREND3                                                          
*                                                                               
NEEDOFF  MVC   RERROR,=AL2(449)    SALESPERSON FILTER NEEDS OFFICE FILT         
         B     ERREND3                                                          
*                                                                               
BADREQTR MVC   RERROR,=AL2(871)    MUST BE 3 CHARACTERS                         
         B     ERREND3                                                          
*                                                                               
NOACCESS MVI   GERROR1,55          SECURITY LOCKOUT                             
         B     ERREND3                                                          
ERREND3  MVI   RMSGTYPE,C'E'                                                    
         B     *+8                                                              
INFEND3  MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
***********************************************************************         
* CHECK TO SEE IF CONTRACT IS PENDING                                           
***********************************************************************         
CHKPDING NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OC    RDARREP#,RDARREP#                                                
         BZ    CHKPDNO                                                          
*                                                                               
         ZAP   MYWORK+5(5),=P'99999999'                                         
         ZAP   MYWORK+10(5),=P'0'                                               
         MVO   MYWORK+10(5),RDARREP#                                            
         SP    MYWORK+5(5),MYWORK+10(5)                                         
         MVO   MYWORK(5),MYWORK+5(5)                                            
         DROP  R6                                                               
*                                                                               
         MVC   WORK(L'KEY),KEY                                                  
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,MYWORK                                                  
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   CHKPDNO                                                          
*        DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
                                                                                
* CHECK IF CONTRACT IS PENDING                                                  
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
         TM    RCONMODR,X'10'      IS THIS A PENDING CONTRACT?                  
         BO    CHKPDNO             NO                                           
         DROP  R6                                                               
CHKPDYES CR    RC,RC               YES                                          
         B     CHKPDX                                                           
CHKPDNO  LTR   RC,RC                                                            
CHKPDX   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,WORK                                                         
         XIT1                                                                   
***********************************************************************         
* SET UP NEW FITLER TALBE                                                       
***********************************************************************         
SETFILTR NTR1  BASE=*,LABEL=*                                                   
         XC    FILTRTAB(FILTRTBQ),FILTRTAB                                      
         OI    O1,FTERNEW+FTERRSNT+FTEROPEN+FTERCFAP                            
         OI    O1+1,FTERMCH+FTERSTCF                                            
         OI    E1,FTERREV+FTERNEW+FTEROPEN+FTERRSNT+FTERCFAP                    
         OI    O2,FTERRJCT+FTERRCAL                                             
         OI    O2+1,FTERAMD+FTERAMDS                                            
         OI    E2,FTERREV+FTERRJCT+FTERRCAL                                     
         OI    E2+1,FTERAMD+FTERAMDS                                            
         OI    O3,FTEROPS                                                       
         OI    O3+1,FTERMCHS+FTERSTCF                                           
         OI    E3,FTERREV+FTEROPS                                               
         OI    E3+1,FTERMCHS                                                    
         MVI   ONE,C'1'                                                         
         MVI   TWO,C'2'                                                         
         MVI   THREE,C'3'                                                       
         MVI   FLTEREND,X'00'                                                   
SETFILTX DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
* VARIOUS SUBROUTINES                                                           
***********************************************************************         
CYCLCON  NTR1  LABEL=*,BASE=*                                                   
         XC    HALF,HALF                                                        
         L     RC,0(R1)                                                         
         LA    R2,DE7HDLNH                                                      
         MVC   SVTWAAGY,TWAAGY                                                  
*                                                                               
         LR    R4,RA               YES                                          
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
*                                                                               
CYCL0020 EQU   *                                                                
         L     R3,ASUBREPX         SET A(SUBREP IN PROGRESS)                    
*                                                                               
         OC    0(2,R3),0(R3)       ANY VALUE IN FIELD?                          
         BZ    CYCL0900            NO  - ALL PROCESSED                          
         MVC   TWAAGY,0(R3)        REPLACE TWAAGY W/REP IN PROGRESS             
         GOTO1 VALICON,DMCB,(R2)                                                
         BZ    CYCL0040            FOUND: CHECK FURTHER                         
CYCL0030 EQU   *                                                                
         LA    R3,2(R3)            NOT FOUND: BUMP TO NEXT SLOT                 
         ST    R3,ASUBREPX                                                      
         B     CYCL0020            GO BACK FOR NEXT                             
*                                                                               
CYCL0040 EQU   *                                                                
         MVI   SVKEYTYP,X'41'                                                   
CYCL0060 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(01),SVKEYTYP    SET KEY TYPE FOR AGY ORD                     
         MVC   KEY+7(02),TWAAGY    INSERT KEY TYPE                              
         MVC   KEY+9(05),CCONKSTA  INSERT STATION CALL LETTERS                  
         MVI   KEY+14,C' '         CLEAR LAST CHARACTER TO SPACE                
         MVC   KEY+20(04),CDARNUM  INSERT AGY HDR LINKED                        
         MVI   KEY+24,X'10'        SET AGENCY HEADER CODE                       
         LA    R6,CDARAGY          SET A(1ST AGY EQUIV CODE)                    
         LA    R0,4                SET MAX LOOP FOR CONTROL                     
CYCL0080 EQU   *                                                                
         OC    0(5,R6),0(R6)       ANYTHING IN AGY EQUIV SLOT?                  
         BZ    CYCL0120            NO  - END OF LOOP                            
         MVC   KEY+15(05),0(R6)    YES - INSERT INTO KEY                        
         GOTO1 HIGH                READ AGENCY ORDER RECORD                     
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
         BE    CYCL0140            YES - INDICATE FOUND                         
         LA    R6,5(R6)            BUMP TO NEXT SLOT                            
         MVC   KEY(27),KEYSAVE     RESET KEY SOUGHT                             
         BCT   R0,CYCL0080         GO BACK FOR NEXT                             
*                                  ALL CHECKED: END OF LOOP                     
CYCL0120 EQU   *                                                                
         CLI   SVKEYTYP,X'51'      51'S PROCESSED?                              
         BE    CYCL0030            YES - AGENCY ORDER NOT FOUND                 
*                                  GO BACK AND CHECK NEXT REP                   
         MVI   SVKEYTYP,X'51'      NO  - PROCESS 51'S                           
         B     CYCL0060            GO BACK FOR NEXT                             
CYCL0140 EQU   *                                                                
         GOTO1 GETREC RETRIEVE THE AGENCY ORDER                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'        LOCATE MISC FLAGS ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   CYCL0030            NO ELEMENT: NOT UNWIRED ORDER                
*                                     GO BACK AND LOOK IN NEXT REP              
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRED ORDER?                               
         BNO   CYCL0030            NO                                           
*                                     GO BACK AND LOOK IN NEXT REP              
         LA    R3,2(R3)            BUMP TO NEXT SLOT - LEAVE                    
         ST    R3,ASUBREPX            FOR NEXT LOOP                             
         MVI   HALF,0              SET 'ORDER FOUND'                            
         B     CYCL0999                                                         
CYCL0900 EQU   *                                                                
         MVI   HALF,X'FF'          SET 'FINISHED'                               
CYCL0999 EQU   *                                                                
         MVC   TWAAGY,SVTWAAGY     RESET REP OF RUN                             
*                                                                               
         XIT1                                                                   
         DROP  R4,R6                                                            
         DS    0H                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ISMAST: IS THIS A MASTER SIGN ON? FLAG SHOULD BE SET IN DAR00                 
*        P1  -  0  =  READ AND TEST MASTER                                      
*               1  =  READ AND LOAD SUBSIDIARY REPS                             
***********************************************************************         
         PRINT GEN                                                              
ISMAST   NTR1  BASE=*,WORK=(R4,IMWORKQ),LABEL=*                                 
         PRINT NOGEN                                                            
         USING IMWORKD,R4                                                       
         L     R3,0(R1)            SAVE P1 VALUE                                
*                                                                               
         LR    RF,RA               USE R2 TO COVER THE ENTRY                    
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
*                                                                               
         MVC   IMSVKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
K        USING RREPKEY,KEY                                                      
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,MSTREPCX                                              
         DROP  K,RF                                                             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IMSVIO,AIO                                                       
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   KEY,IMSVKEY         RESTORE KEY AND AIO                          
*        GOTO1 HIGH                                                             
         MVC   AIO,IMSVIO                                                       
*                                                                               
         LTR   R3,R3               TEST OR LOAD?                                
         BNZ   ISMSTLOD            1  =  LOAD                                   
*                                                                               
         LA    R6,IMIO                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RREPCODE,R6                                                      
         CLC   RREPMAST,=X'FFFF'                                                
         BE    ISMSTY              MASTER                                       
         CLC   RREPMAST,=X'4040'                                                
         BNH   ISMSTH              STAND ALONE REP                              
ISMSTL   CLI   *,X'FF'             OR IS A SUBREP                               
         B     ISMASTX                                                          
ISMSTY   CR    RB,RB                                                            
         B     ISMASTX                                                          
ISMSTH   CLI   *,0                                                              
ISMASTX  XMOD1                                                                  
ISMSTLOD EQU   *                                                                
*                                                                               
         LA    R6,IMIO                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
ISMSL020 EQU   *                                                                
         USING RREPSUB,R6                                                       
         ZIC   RF,RREPSCNT         COUNT OF NUMBER OF SUBS                      
         SLL   RF,1                DOUBLE SIZE OF COUNT                         
         BCTR  RF,0                BACK OFF 1 FOR EX MOVE                       
         LR    RE,RA               USE R2 TO COVER THE ENTRY                    
         AHI   RE,DARPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
*                                                                               
         EX    RF,ISMSL100         MOVE DATA BY LENGTH                          
         B     ISMSL120                                                         
ISMSL100 EQU   *                                                                
         MVC   SBRPCDSX(0),RREPSCOD                                             
*                                                                               
ISMSL120 EQU   *                                                                
         LA    R6,IMIO                                                          
         USING RREPREC,R6                                                       
         CLC   =C'K3',RREPKREP     KRG?                                         
         BNE   ISMASTX             NO                                           
         LA    R1,SBRPCDSX         SET A(REP CODES)                             
         LA    RF,24               CHECK 24 SLOTS MAX                           
ISMSL140 EQU   *                                                                
         CLI   0(R1),0             OPEN SLOT REACHED?                           
         BE    ISMSL160            YES                                          
         LA    R1,2(R1)            BUMP TO NEXT SLOT                            
         BCT   RF,ISMSL140         GO BACK FOR NEXT                             
         DC    H'0'                NO OPEN SLOTS                                
ISMSL160 EQU   *                                                                
         MVC   0(2,R1),=C'NU'      INSERT CLEAR CHANNEL                         
         DROP  R4,R6,RE                                                         
         B     ISMASTX                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RESET KEYS OF AN AGENCY ORDER                                       *         
*       THIS IS A DDS-USED ROUTINE ONLY.  AS SUCH, THESE RULES APPLY: *         
*       1.  AGENCY ORDER MUST BE 8 DIGITS                             *         
*       2.  ROUTING AGENCY IS USED AS ENTERED.  USER RESPONSIBILITY   *         
*           TO DETERMINE CORRECT CODE.                                *         
*                                                                     *         
***********************************************************************         
RESETKEY NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO3             CLEAR WORK AREA                              
         LHI   RF,1600                                                          
         XCEF                                                                   
*                                                                               
         CLI   DE7HDLNH+5,0        AGENCY ORDER # ENTERED?                      
         BE    RKEY0900            NO  - NEED #                                 
         CLI   DE7HDLNH+5,8        EIGHT CHARACTERS ENTERED?                    
         BNE   RKEY0900            NO  - IN ERROR                               
         CLI   DE7STATH+5,0        STATION ENTERED?                             
         BE    RKEY0901            NO  - NEED STATION                           
         CLI   DE7AGYH+5,0         AGENCY ENTERED?                              
         BE    RKEY0902            NO  - NEED AGENCY                            
         LA    R2,DE7STATH                                                      
         GOTO1 VALISTA                                                          
         MVC   STAFILT,WORK                                                     
         OC    STAFILT,SPACES                                                   
         CLI   STAFILT+4,C' '                                                   
         BNE   RKEY0020                                                         
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
RKEY0020 EQU   *                                                                
*                                                                               
         BAS   RE,GETAGYHR         FIND THE AGENCY HEADER                       
         BNZ   RKEY0903            NOT FOUND                                    
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
*   CLEAR BUFFER AREA.                                                          
*        IF AGENCY ORDER IS DELETED, ALL OLD KEYS WILL BE DELETED.              
*        IF AGENCY ORDER IS ACTIVE,  ALL KEYS WILL BE RESET AS ACTIVE           
*                                                                               
         L     RE,AIO3             CLEAR WORK AREA                              
         LHI   RF,1600                                                          
         XCEF                                                                   
*                                                                               
         TM    RDARCNTL,X'80'      AGENCY ORDER DELETED?                        
         BNO   RKEY0040            NO  - ACTIVE                                 
         DROP  R6                                                               
*                                                                               
*   DELETED ORDER: MAKE SURE ALL KEYS ARE !! TURNED OFF !!                      
*   PULL OLD KEYS:                                                              
*        AIO3:  KEY BUILD AREA                                                  
*        AIO :  CURRENT LOCATION OF AGENCY ORDER RECORD (AIO1)                  
*        AIO2:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'81',ACOMFACS),AIO3,AIO,      X        
               AIO2                                                             
*                                                                               
*   NOW SEE IF THERE IS A 51 RECORD, AND THIS IS A !! CONFIRMED !!              
*        ORDER.  IF SO, NEED TO ACTIVATE THE !! CONFIRMED !! SIDE.              
*                                                                               
         MVI   KEY,X'51'           LOOK FOR SAME KEY AS CONFIRMED               
*                                  READ KEY: IGNORE DELETED!                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   RKEY0030            NOT FOUND - PROCESS OLD ONLY                 
*                                  FOUND:  USE D/A FROM 51 RECORD               
         MVC   HDRDA2,KEY+28       SAVE D/A FOR LATER USE                       
         GOTOR MYGETREC                                                         
*                                  RETRIEVE THE 51 AGY HDR                      
         L     R4,AIO3             SET A(KEY BUILD AREA)                        
         AH    R4,=H'800'          BUMP TO 'NEW' KEY AREA                       
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'81',ACOMFACS),(R4),AIO,      X        
               AIO2                                                             
*                                  SET UP KEYS FOR 51 RECORD                    
RKEY0030 EQU   *                                                                
         B     RKEY0060            GO TO 'ADDPTRS' CALL                         
*                                                                               
RKEY0040 EQU   *                                                                
*                                                                               
*   ACTIVE  ORDER: MAKE SURE ALL KEYS ARE !! TURNED ON  !!                      
*                                                                               
*   PULL NEW KEYS:                                                              
*        AIO3:  KEY BUILD AREA                                                  
*        R6  :  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         L     R4,AIO3             PULL NEW PASSIVE POINTER                     
         LA    R4,800(R4)                                                       
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'81',ACOMFACS),(R4),AIO,      X        
               AIO2                                                             
*                                                                               
RKEY0060 EQU   *                                                                
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         L     R4,AIO3             A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),AIO3,(R4),     X        
               HDRDA2                                                           
*                                                                               
         B     RKEY0800            EXIT CC ZERO                                 
RKEY0900 EQU   *                                                                
         MVI   DUB+4,1             SET ERROR TRANSFER                           
         LA    RF,DE7HDLNH                                                      
         ST    RF,DUB                                                           
         B     RKEY0910                                                         
RKEY0800 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     RKEY0920                                                         
RKEY0901 EQU   *                                                                
         MVI   DUB+4,1             SET ERROR TRANSFER                           
         LA    RF,DE7STATH                                                      
         ST    RF,DUB                                                           
         B     RKEY0910                                                         
RKEY0902 EQU   *                                                                
         MVI   DUB+4,1             SET ERROR TRANSFER                           
         LA    RF,DE7AGYH                                                       
         ST    RF,DUB                                                           
         B     RKEY0910                                                         
RKEY0903 EQU   *                                                                
         MVI   DUB+4,2             SET ERROR TRANSFER                           
         LA    RF,DE7HDLNH                                                      
         ST    RF,DUB                                                           
         B     RKEY0910                                                         
RKEY0910 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO, EXIT                        
RKEY0920 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
GETAGYHR NTR1                                                                   
         MVC   AIO,AIO1            SET AGENCY ORDER AREA                        
*                                                                               
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   RDARKTYP,=X'41'     SET DARE TYPE RECORD                         
         MVC   RDARKREP,AGENCY     INSERT ORIGINAL REP INTO KEY                 
         MVC   RDARKSTA(5),STAFILT                                              
         OC    RDARKSTA(6),SPACES                                               
         ZIC   RE,DE7AGYH+5        GET L(ROUTING AGENCY CODE)                   
         BCTR  RE,0                BACK OFF 1 FOR EX                            
*                                                                               
*   BECAUSE THE ROUTING AGENCY CODE IS USED AS ENTERED, THIS CODE               
*        MAY BE ENTERED AS GREATER THAN FIVE CHARACTERS.  THE                   
*        AGENCY ORDER NUMBER WOULD THEN OVERLAY THE LOWER POSITIONS,            
*        AND THIS SITUATION WOULD RESULT IN A NO-FIND OF THE AGENCY             
*        ORDER (X'41') RECORD ITSELF.                                           
*                                                                               
         EX    RE,GAGY0800         MOVE ROUTING AGY CODE BY LENGTH              
         OC    RDARKAGY(5),SPACES  SET TO SPACES                                
*                                                                               
         GOTO1 HEXIN,DMCB,DE7HDLN,RDARKORD,8                                    
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
*                                                                               
*                                                                               
GAGY0040 DS    0H                                                               
         OI    DMINBTS,X'08'       RETURN DELETED KEY FOR 41 REC                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
*                                  41 RECORD FOUND?                             
         BE    GAGY0060            YES - PROCESS IT                             
         MVC   KEY(27),KEYSAVE     NO  - RESET KEY TO 41 NOT FOUND              
         MVI   KEY,X'51'           SET TO 51 KEY                                
         GOTO1 HIGH                READ FOR AN UNDELETED 51 KEY                 
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
*                                  51 RECORD FOUND?                             
         BNE   GAGY0200            NO  - NEITHER 41 NOR 51 FOUND                
*                                                                               
*  FOUND:  41 (UNDELETED OR DELETED) OR 51 (UNDELETED)                          
*                                                                               
GAGY0060 DS    0H                                                               
         MVC   HDRDA2,KEY+28       SAVE D/A FOR LATER USE                       
         OI    DMINBTS,X'08'       ALWAYS RETURN DELETED RECORD                 
         GOTOR MYGETREC                                                         
         SR    R0,R0               SET CC ZERO                                  
         B     GAGY0220            EXIT CC ZERO                                 
GAGY0200 DS    0H                                                               
         LTR   RB,RB               SET CC NOT ZERO                              
GAGY0220 EQU   *                                                                
         XIT1                                                                   
GAGY0800 MVC   RDARKAGY(0),DE7AGY                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* CHECK IF REP OFFICE LIMIT ACCESS MATCH THE OFFICE OF THE DARE                 
* RECEIVING ID OFFICE                                                           
*                                                                               
CHECKOFF NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'J0',RDARKREP     DARE ORDER FOR KATZ DEDICATED?               
         BNE   COFF0003            NO                                           
         MVC   KATZRAD+15(2),=X'9999'                                           
*                                  YES - TURN OFF KATZ RADIO IN REPIDS          
COFF0003 EQU   *                                                                
         CLC   =C'IF',RDARKREP     DARE ORDER FOR CBS/IF?                       
         BNE   COFF0005            NO                                           
         MVC   CBSTV+15(2),=X'9999'                                             
*                                  YES - TURN OFF CBS TV IN REPIDS              
COFF0005 EQU   *                                                                
         CLC   =C'IB',RDARKREP     DARE ORDER FOR ABC/IB?                       
         BNE   COFF0006            NO                                           
         MVC   ABCTV+15(2),=X'9999'                                             
*                                  YES - TURN OFF ABC TV IN REPIDS              
COFF0006 EQU   *                                                                
         LA    R3,REPIDS                                                        
*                                                                               
COFF10   DS    0H                                                               
         ZIC   R1,14(R3)                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RDARRCVR(0),15(R3)                                               
         BE    COFF30                                                           
         AHI   R3,L'REPIDS                                                      
         CLI   0(R3),X'FF'                                                      
         BNE   COFF10                                                           
         B     COFF40                                                           
*                                                                               
COFF30   DS    0H                                                               
         TM    13(R3),X'80'        NO OFFICE APPENDED                           
         BO    COFF40              USE AGENCY OFFICE INSTEAD                    
         LA    R4,RDARRCVR                                                      
         LA    R4,1(R1,R4)         BUMP PAST REP PREFIX                         
         CLC   DE7OFF(2),0(R4)                                                  
         BNE   COFFNO                                                           
         B     COFFYES                                                          
*                                                                               
COFF40   DS    0H                                                               
         CLC   RDARKAOF,DE7OFF     NOT FOUND, USE AGENCY OFFICE                 
         BE    COFFYES             INSTEAD                                      
         B     COFFNO                                                           
*                                                                               
COFFYES  SR    RC,RC                                                            
COFFNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETELN R6,DATADISP,ELCODE,4                                            
         LTORG                                                                  
*****************************                                                   
       ++INCLUDE DDDARETAB          DARE 3 CHARACTER REP CODES                  
         EJECT                                                                  
***********************************************************************         
*    VALID DAYPARTS                                                             
***********************************************************************         
*                                  BYTE 1 = AGENCY BUY NUMBER                   
*                                  BYTE 2 = CONTRACT BUY NUMBER                 
*                                  BYTE 3-6 = CONTRACT BUY D/A                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENSTAD                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLCON                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE REPFACSQ                                                       
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE REROMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REROME7D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REROME1D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REROME5D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REROME3D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REROME4D                                                       
       ++INCLUDE REDARTW2                                                       
       ++INCLUDE REDARWORKD                                                     
RECS     DSECT                                                                  
       ++INCLUDE REDARDSECT                                                     
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENPTP                                                       
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE DDPERVALD                                                      
IMWORKD  DSECT                                                                  
IMSVIO   DS    A                                                                
IMSVKEY  DS    XL27                                                             
IMIO     DS    XL2000                                                           
IMWORKQ  EQU   *-IMWORKD                                                        
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
MYWORK   DS    CL80                                                             
SVKEY    DS    CL32                                                             
*                                                                               
AGYOFF   DS    CL2                                                              
AGYSALP  DS    CL3                                                              
ERRFLAG  DS    C                                                                
DATEBLCK DS    CL56                PERVAL OUTPUT BLOCK                          
ORDLSTDA DS    XL4                 CURRENT LIST RECORD DISK ADDRESS             
*                                                                               
MYRECTYP DS    X                   RECORD TYPE FOR LISTING 41/51                
*                                                                               
HDRDA    DS    CL4                 DISK ADDRESS OF REASSIGN RECORD              
HAGYORDR DS    XL4                 HEX AGENCY ORDER NUMBER                      
REDITYPE DS    XL1                 REDI ORDER SCREEN TYPE                       
*                                  0  =  ORDER    (DEFAULT SEQUENCE)            
*                                  1  =  CAMPAIGN                               
*                                  2  =  *BUYER                                 
*                                  3  =  FLIGHT                                 
*                                  4  =  NEWDATE                                
*                                                                               
SCRNHDRS DS    XL1                 SAVE ORIGINAL REDITYPE TO SWITCH             
*                                  HEADERS, IF NECESSARY                        
COMPREP  DS    CL2                 RADIO INBOX COMPANY REP                      
KEYLOOK  DS    CL4                                                              
CONFILT  DS    CL1                                                              
CURRKEY  DS    CL27                CURRENT KEY: TEMP SAVE AREA                  
*                                                                               
FILTRTAB DS    0H                                                               
ONE      DS    CL1                                                              
O1       DS    XL2                                                              
E1       DS    XL2                                                              
TWO      DS    CL1                                                              
O2       DS    XL2                                                              
E2       DS    XL2                                                              
THREE    DS    CL1                                                              
O3       DS    XL2                                                              
E3       DS    XL2                                                              
FLTEREND DS    XL1                                                              
FILTRTBQ EQU   *-FILTRTAB                                                       
FTABROWQ EQU   TWO-ONE                                                          
FTABCOLQ EQU   E1-ONE                                                           
*                                                                               
RECSTATS DS    CL2                 RECORD STATUS                                
COMBFLTR DS    CL2                 COMBINE FILTER DEDUCE FROM TABLE             
COM2FLTR DS    CL2                 COMBINE FILTER DEDUCE FROM TABLE             
*                                                                               
FTERREV  EQU   X'80'               REVISION?                                    
*                                                                               
FTERNEW  EQU   X'40'               NEW ( UNLINK & LINK )                        
FTERCFAP EQU   X'20'               CONFIRM APPROVE                              
FTEROPS  EQU   X'10'               OPEN-SENT TO STATION                         
FTERRSNT EQU   X'08'               RESENT                                       
FTERRJCT EQU   X'04'               REJECT                                       
FTERRCAL EQU   X'02'               RECALL                                       
FTEROPEN EQU   X'01'               OPEN?                                        
*                                                                               
* BYTE 2                                                                        
FTERMCH  EQU   X'80'               MATCH                                        
FTERCFHD EQU   X'40'               CONFIRM HOLD                                 
FTERMCHS EQU   X'20'               MATCH-S                                      
FTERSTCF EQU   X'10'               STACF                                        
*                                                                               
FTERAMDS EQU   X'08'               AMEND-S                                      
FTERAMD  EQU   X'04'               AMEND                                        
*                                                                               
FTERFLG1 DS    XL1                 NEW FILTER FLAG                              
FTERBYO  EQU   1                   FILTER BY 'O'                                
FTERBY#  EQU   2                   FILTER BY 1,2,3                              
FTERBYO# EQU   3                   FILTER BY O1,O2,O3                           
FTERBYE# EQU   4                   FILTER BY E1,E2,E3                           
*                                                                               
HDRDA2   DS    XL4                                                              
PENDCF   DS    X                                                                
*                                                                               
SVPDATE  DS    XL2                 SAVED PRINTED DATE                           
SVPTIME  DS    XL3                 SAVED PRINTED TIME                           
MYCOUNT  DS    H                   PRINTED ORDER COUNT                          
MYFLAG   DS    X                                                                
MFGUNDOP EQU   X'80'                                                            
PRTDAB   EQU   X'01'               PRINT THE DAB FORMAT                         
SVTWAAGY DS    CL2                                                              
SVKEYTYP DS    XL1                                                              
WORKLQ   EQU   *-MYAREAD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073REROM03   05/27/03'                                      
         END                                                                    
