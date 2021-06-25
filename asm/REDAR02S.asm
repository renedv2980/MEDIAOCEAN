*          DATA SET REDAR02S   AT LEVEL 171 AS OF 01/02/03                      
*PHASE T80F02A                                                                  
*INCLUDE REREPDIF                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE REDARTKO                                                               
*&&      SET   TT=N,T2=N                                                        
*                                                                               
         TITLE 'T80F02 - REDAR02 - DARE HEADER DISPLAY/LIST - RADIO'            
***********************************************************************         
*                                                                     *         
*  REDAR02 (T80F02) --- DARE HEADER DISPLAY/LIST RADIO INBOX          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
* 17OCT02 (BU ) INITIAL RELEASE                                       *         
* 25OCT02 (BU ) SCREEN 'F0' REPLACED BY SCREEN 'E3'                   *         
* 05NOV02 (HQ ) NEW FILTERS INCOPORATED                               *         
* 14NOV02 (HQ ) DISALLOW REJECT ON CONFIRM ORDER                      *         
*               FIX BUY COMMENT RECORD SOFT/HARD DELETE BUG           *         
*               COMPARE DIFFERENCE B/T PREVIOUS AND CURRENT ORDER     *         
*               RADIO EDI LINK/UNLINK RULE                            *         
* 18NOV02 (HQ ) CONFIRM CONTRACT                                      *         
* 20NOV02 (HQ ) DISPLAY NEW FOR UNLINK/LINK ORDER FOR RADIO EDI       *         
* 22NOV02 (HQ ) COMPARING ORDER VS CONTRACT AT LINKING TIME           *         
* 16DEC02 (BU ) CONTAINS FIXES FOR:                                   *         
*               1.  REMOVING LINES REPEATED ON PAGE BREAK             *         
*               2.  ELAPSED TIME IN FLIGHT SEQUENCE SCREEN            *         
*               3.  REPIDS TABLE FIX:  KR VS KRD                      *         
*               4.  GROUP/SUBGROUP FILTER                             *         
* 19DEC02 (BU)  CONTAINS FIXES FOR:                                   *         
*               1.  CONTRACT NUMBER FILTER: OTHER FILTERS IGNORED     *         
*               2.  CF FILTER, WITH RECVD DATE AND INBOX FILTER       *         
*               3.  REPORT FEATURE WORKING                            *         
*               4.  REPIDS TABLE FIX:  ABC VS ABCR, CBS VS CBSR       *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T80F02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLQ,*T80F02*,R7,RR=R3                                         
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
         ST    R3,RELO                                                          
*                                                                               
         XC    BUYKEY,BUYKEY       THIS FIX THE DAR10 ORDER CMT BUG             
*                                                                               
         OI    GLSTSTAT,APPLCDSP+RETEXTRA   SET I DISPLAY                       
         MVC   LLIST,=Y(LISTLEN)   SET L'DATE LINE                              
*                                  SETUP THE PFKEYS                             
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
         GOTO1 =A(ISMAST),DMCB,1,RR=RELO                                        
         DROP  R2                                                               
*                                                                               
*   TEST                                                                        
         LTR   RB,RB                                                            
*   TEST END                                                                    
*                                                                               
MAIN0040 EQU   *                                                                
*                                                                               
         GOTO1 =A(SUBROUT),DMCB,(RC),('QSETPFKY',0),RR=RELO                     
                                                                                
         CLC   =C'K*Y',DE1RQTR     SPECIAL REQUESTOR OPTION?                    
         BNE   MAIN0060                                                         
         CLI   MODE,LISTRECS       ONLY DO ON 'LISTRECS'                        
         BNE   EXIT                                                             
         GOTO1 =A(RESETKEY),RR=RELO                                             
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
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       REPORT?                                      
         BE    LR                                                               
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VKEY     DS    0H                                                               
*                                                                               
         GOTO1 =A(SUBVKEY),RR=RELO                                              
*                                                                               
*        TEMPORARILY SAVE MYAREAD IN EVENT THIS IS A REPORT PASS                
*                                                                               
         MVC   FULL(2),=Y(WORKLQ)                                               
         ZICM  R1,FULL,2                                                        
         PRINT GEN                                                              
         L     RF,AIO3             SET A(TEMP STORAGE SPACE)                    
         LA    RE,MYAREAD          SET A(DATA TO BE SAVED)                      
         MOVE  ((RF),(R1)),(RE)                                                 
         PRINT NOGEN                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD, MOVE TO SUBROUTINE FOR ADDRESSIBILITY                        
***********************************************************************         
VR       DS    0H                                                               
         GOTO1 =A(VREC),RR=RELO                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
         GOTO1 =A(LRADDR),RR=RELO                                               
LR10     DS    0H                                                               
         CLI   DE1HDLNH+5,0                                                     
         BE    LR40                                                             
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BO    LR40                YES - USE LONG SCREEN                        
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(SWAPHDRS),DMCB,DE5LHEDH,RR=RELO                               
         CLI   MYSCRNUM,X'E5'      SHORT LIST SCREEN ALREADY LOADED?            
         BNE   LR20                                                             
         TWAXC DE5AGYCH,DE5ESTH    YES, CLEAR IT                                
         TWAXC DE5SELH,DE5LLINH,PROT=Y                                          
         B     LR30                                                             
                                                                                
* LOAD LOWER SCREEN                                                             
LR20     DS    0H                                                               
         GOTO1 CALLOV,DMCB,DE1TAGH,X'D9080FE5'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCRNUM,X'E5'                                                   
LR30     DS    0H                                                               
         LA    R2,DE5SELH          SET FOR GENCON CURSOR POSITION               
         ST    R2,AFRSTREC                                                      
         LA    R2,DE5STATH                                                      
         ST    R2,ATHISLST                                                      
         OI    DE5SELH+6,X'40'     FORCE CURSOR HERE                            
                                                                                
*                                  DISPLAY CONTRACT INFORMATION                 
         GOTO1 =A(SUBROUT),DMCB,(RC),('QDISCON',0),RR=RELO                      
         MVC   DE5LAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         B     LR70                                                             
                                                                                
LR40     DS    0H                                                               
         CLI   MYSCRNUM,X'E3'      LONG LIST SCREEN ALREADY LOADED?             
         BNE   LR50                                                             
         CLC   =C'Act',DARHDR1                                                  
         BNE   LR50                ADDITIONAL CHECK TO INSURE SCREEN IS         
*                                  LOADED                                       
         TWAXC DARSELH,DARLLIN,PROT=Y YES, CLEAR IT                             
**       MVC   DARPFLN(17),=C'PF2 Makegood List'                                
         B     LR60                                                             
                                                                                
* LOAD LOWER SCREEN                                                             
LR50     DS    0H                                                               
         GOTO1 CALLOV,DMCB,DE1TAGH,X'D9080FE3'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCRNUM,X'E3'                                                   
*                                                                               
LR60     DS    0H                                                               
***      CLI   SCRNHDRS,3          DEFAULT: HEADERS UP                          
***      BE    LR65                YES - NO CHANGE                              
         GOTO1 =A(SWAPHDRS),DMCB,DARHDR1H,RR=RELO                               
LR65     DS    0H                                                               
         LA    R2,DARSELH          SET FOR GENCON CURSOR POSITION               
         ST    R2,AFRSTREC                                                      
         LA    R2,DARSTATH                                                      
         ST    R2,ATHISLST                                                      
         L     R3,ATHISLST                                                      
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR650010            NO                                           
         LA    R3,PRNTWORK         SET A(PRINT LINE)                            
LR650010 EQU   *                                                                
         OI    DARSELH+6,X'40'     FORCE CURSOR HERE                            
*                                                                               
         MVC   DARLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
*                                                                               
         CLI   DE1HDLNH+5,0        CONTRACT # ENTERED?                          
         BE    LR70                NO                                           
*                                                                               
         LR    RF,RA               YES                                          
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BNO   LR70                NO  - REGULAR PROCESSING                     
*                                                                               
         DROP  RF                                                               
*                                                                               
LR650020 EQU   *                                                                
         GOTO1 =A(CYCLCON),DMCB,(RC),RR=RELO                                    
*                                                                               
         CLI   HALF,X'FF'          FINISHED?                                    
         BE    LRX                 YES - END OF DISPLAY                         
         CLI   HALF,X'00'          AGENCY ORDER FOUND?                          
         BE    LR210               YES - DISPLAY IT                             
*                                                                               
*   LR210    MAY OR MAY NOT BE THE CORRECT ENTRY POINT FOR DISPLAY              
*                                                                               
         DC    H'0'                UNIDENTIFIED RETURN                          
LR70     DS    0H                                                               
         XC    MYLISTDA,MYLISTDA   BUILD LIST OF DISK ADDRESSES                 
         LA    R4,MYLISTDA         FOR ACTION PRINT                             
                                                                                
         TM    CTLRFLG1,CF11STKY   REDISPLAY PAGE?                              
         BZ    LR80                                                             
         NI    CTLRFLG1,X'FF'-CF11STKY                                          
         MVC   SELCTKEY,FIRSTKEY                                                
*                                                                               
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
*                                                                               
LR90     DS    0H                                                               
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
         GOTO1 SEQ                 READ THE NEXT KEY FOR RESTART                
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
         CLI   DE1UNW,C'Y'         SPECIAL OVERRIDE TO UNWIRED?                 
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
         CLC   =C'UNW',DE1RQTR     SPECIAL OVERRIDE TO UNWIRED?                 
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
         GOTO1 =A(RECDATCK),RR=RELO                                             
         BNZ   LRSEQ               FILTERED OUT BY RECV DATE                    
*                                                                               
         GOTO1 =A(INBOXCHK),DMCB,KEYDEFTB,RR=RELO                               
         BNZ   LRSEQ               FILTERED OUT BY INBOX CHECK                  
*                                                                               
         GOTO1 =A(STATNCHK),DMCB,KEYDEFTB,RR=RELO                               
         BNZ   LRSEQ               FILTERED OUT BY STATION CHECK                
*                                                                               
*   TEST DUMP                                                                   
*&&DO                                                                           
         LA    RF,KEY-8                                                         
         CLI   KEY+27,X'FF'        DELETED KEY?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =X'23250021',KEY+20 PROCESS ONLY A SINGLE ORDER                  
         BNE   LRSEQ                                                            
****     MVC   DIE(2),=X'0000'                                                  
TEST0020 EQU   *                                                                
*&&                                                                             
         MVI   PENDCF,0            CLEAR FLAG                                   
*                                                                               
         CLI   DE1HDLNH+5,0        ANY CON # ENTERED?                           
         BE    LR115010            NO  -                                        
         OC    CDARNUM,CDARNUM     SKIP IF THERE ARE NONE                       
         BZ    LRX                                                              
         LA    RF,KEYDEFTB         SET A(KEY DEFINITION TABLE)                  
LR115005 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                MUST BE IN TABLE                             
         CLC   RED01TYP+1(1),DREDTYPE(RF)                                       
*                                  RECORD TYPE FOUND IN TABLE?                  
         BE    LR115006            YES                                          
         LA    RF,KEYDEFLN(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     LR115005            GO BACK FOR NEXT                             
LR115006 EQU   *                                                                
         ZIC   RE,DORDER#(RF)      GET DISPLACEMENT TO ORDER NUMBER             
         LA    RF,KEY              SET A(KEY IN PROGRESS)                       
         AR    RF,RE               DISPLACE TO ORDER NUMBER                     
         CLC   CDARNUM,0(RF)       LOOKING FOR THIS CONTRACT NUMBER?            
         BNE   LRSEQ               NO  - SKIP IT                                
         B     LR270               YES - ACCEPT ORDER                           
*                                                                               
LR115010 EQU   *                                                                
         CLI   STATFILT,C'F'       FILTER ON CONFIRMED?                         
         BNE   LR115020            NO  -                                        
         TM    KEY+27,X'04'        YES - 'CONFIRMED' SET IN STATUS?             
         BO    LR270               YES - ACCEPT THIS ORDER                      
         MVI   PENDCF,1            NO  - SET 'F-FILTER, NOT CF'                 
         B     LR213B10                                                         
*                                                                               
LR115020 EQU   *                                                                
*                                                                               
*   NOT 'FILTER ON CONFIRMED': REJECT ALL CONFIRMED FOR DISPLAY                 
*                                                                               
DIE      DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         OC    CAMPFLD1(12),CAMPFLD1                                            
*                                  ANY CAMPAIGN FILTER?                         
         BNZ   LR120               YES - INCLUDE 'CONFIRMED' ORDERS             
*                                                                               
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
         CLI   DE1AGYH+5,0         FILTER ON AGENCY                             
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
         CLI   DE1HDLNH+5,0        IF CON# SPECIFIED, LIST ONLY THE             
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
         CLI   DE1GRPH+5,0         IF EITHER GROUP OR TEAM FILTER               
         BNE   LR160               REQUESTED, READ STATION RECORD               
         CLI   DE1TEAMH+5,0        IN ORDER TO VALIDATE                         
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
         GOTO1 =A(READAGYO),RR=RELO                                             
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
                                                                                
         CLI   DE1GRPH+5,0         GROUP FILTER?                                
         BE    LR180                                                            
         ZIC   RF,DE1GRPH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   GRPFILT(0),WORK+31                                               
         BNE   LRSEQ                                                            
                                                                                
LR180    DS    0H                                                               
         CLI   DE1TEAMH+5,0        TEAM FILTER?                                 
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
         ZIC   RF,DE1TEAMH+5                                                    
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
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
*                                                                               
LR210A10 EQU   *                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
*                                                                               
         OC    CAMPFLD1(12),CAMPFLD1                                            
*                                  ANY CAMPAIGN FILTER?                         
         BZ    LR210A20            NO                                           
         CLC   CAMPFLD3+1(3),RDAREST#                                           
*                                  ESTIMATE NUMBER MATCH?                       
         BNE   LRSEQ               NO  - SKIP THIS ORDER                        
         LA    RF,RDARELEM                                                      
         ZIC   RE,1(RF)            BUMP TO X'02' ELEMENT                        
         AR    RF,RE                                                            
         CLI   0(RF),2             2ND DESCRIPTION ELEMENT?                     
         BNE   LRSEQ               NO  - SKIP THIS ORDER                        
         CLC   CAMPFLD1(4),RDARCLI-RDARCLEM(RF)                                 
         BNE   LRSEQ               NO CLIENT MATCH                              
         CLC   CAMPFLD2(4),RDARPRD1-RDARCLEM(RF)                                
         BNE   LRSEQ               NO PRODUCT MATCH                             
*                                                                               
         DROP  R1,R6                                                            
*                                                                               
LR210A20 EQU   *                                                                
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    LR210AA                                                          
         DROP  R1                                                               
*                                                                               
         GOTO1 =A(CHKLOCAL),RR=RELO                                             
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
         GOTO1 =A(CHKDIREC),RR=RELO                                             
         BNZ   LRSEQ               CHECK IF DIRECT ORDER IN USE                 
*                                                                               
LR210AB  DS    0H                                                               
         OC    CDARNUM,CDARNUM     FILTERING ON SPECIFIC CONTRACT?              
         BNZ   LR210B              YES - NO OFFICE CHECK                        
         CLI   DE1OFFH+5,0         FILTER ON OFFICE?                            
         BE    LR210B                                                           
         CLC   =C'C=',DE1OFF                                                    
         BE    LR210B                                                           
         GOTO1 =A(CHECKOFF),RR=RELO                                             
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
         CLI   DE1TYPEH+5,0        FILTER ON TYPE                               
         BE    LR270                                                            
*                                                                               
         CLI   STATFILT,C'F'       CONFIR                                       
         BE    LR270                                                            
*                                                                               
         CLI   STATFILT,C'I'       UNTOUCHED/INCOMING                           
         BNE   LR212                                                            
         MVI   ELCODE,X'30'        PRESENCE OF FIRST TOUCHED ELEMENT            
         BRAS  RE,GETEL            MEANS THE RECORD HAS BEEN LOOKED AT          
         BE    LRSEQ                                                            
         B     LR270                                                            
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
         GOTO1 =A(TSTFLTR),RR=RELO NEW FILTERS?                                 
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
         BNE   LR213BB             NO  -                                        
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
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
LR213BB  DS    0H                                                               
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
*                                                                               
*   AT THIS POINT, THE AGENCY ORDER SHOULD BE IN THE IO AREA.                   
*        IF IT ISN'T, IT IS RETRIEVED INTO THAT SPACE.                          
*                                                                               
         L     RF,AIO              YES - SEE IF RECORD READ                     
         CLC   KEY(27),0(RF)       RECORD IN IO AREA?                           
         BE    LR270010            YES - DON'T REREAD                           
*                                  NO  - READ IN THE RECORD                     
         GOTO1 =A(MYGETREC),RR=RELO                                             
         BNZ   LRSEQ               NOT FOUND OR DELETED                         
*                                                                               
LR270010 DS    0H                                                               
         CLI   STATFILT,C'F'       CONFIRMED?                                   
         BNE   LR275               NO  -                                        
*                                                                               
         CLI   DE1OFFH+5,0         ANY OFFICE FILTER?                           
         BE    LR270020            NO  - DON'T FILTER                           
         CLC   =C'C=',DE1OFF       YES - SPECIAL OVERRIDE?                      
         BE    LR270020            YES - DONT CHECK OFFICE                      
         GOTO1 =A(CHECKOFF),RR=RELO                                             
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
         CLI   DE1SALPH+5,0        FILTER ON SALESPERSON?                       
         BNE   LRSEQ               IF YES, LINKED ORDERS ONLY                   
         B     LR340                                                            
*&&DO                                                                           
         CLI   DE1OFFH+5,0         FILTER ON OFFICE?                            
         BE    LR340                                                            
         CLC   DE1OFF(2),RDARKAOF                                               
         BE    LR340                                                            
         CLC   =C'C=',DE1OFF                                                    
         BNE   *+14                                                             
         CLC   DE1OFF+2(2),RDARKAOF                                             
         BE    LR340                                                            
*                                                                               
*                                                                               
* SPECIAL FOR SELTEL (SZ) TO HANDLE OFFICE FILTER                               
*                                                                               
         CLC   =C'SZ',AGENCY                                                    
         BNE   LRSEQ                                                            
         CLC   =C'LA',OFFFILT      LA --> LO                                    
         BNE   SZ10                                                             
         CLC   =C'LO',RDARKAOF                                                  
         BE    LR340                                                            
         B     LRSEQ                                                            
SZ10     CLC   =C'SL',OFFFILT      SL --> ST                                    
         BNE   SZ20                                                             
         CLC   =C'ST',RDARKAOF                                                  
         BE    LR340                                                            
         B     LRSEQ                                                            
SZ20     CLC   =C'NY',OFFFILT      NY --> NE/TN                                 
         BNE   LRSEQ                                                            
         CLC   =C'NE',RDARKAOF                                                  
         BE    LR340                                                            
         CLC   =C'TN',RDARKAOF                                                  
         BE    LR340                                                            
*                                                                               
* END SPECIAL CODING                                                            
*                                                                               
         B     LRSEQ                                                            
*&&                                                                             
                                                                                
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
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
         CLI   DE1OFFH+5,0         FILTER ON OFFICE OR                          
         BE    LR310                                                            
         CLC   =C'C=',DE1OFF                                                    
         BNE   LR310                                                            
         CLC   AGYOFF,DE1OFF+2                                                  
         BE    LR310                                                            
*&&DO                                                                           
*                                                                               
* SPECIAL FOR SELTEL (SZ) TO HANDLE OFFICE FILTER                               
*                                                                               
LSZ00    DS    0H                                                               
         CLC   =C'SZ',AGENCY                                                    
         BNE   LR320                                                            
         CLC   =C'LA',OFFFILT      LA --> LO                                    
         BNE   LSZ10                                                            
         CLC   =C'LO',AGYOFF                                                    
         BE    LR310                                                            
         B     LR320                                                            
LSZ10    CLC   =C'SL',OFFFILT      SL --> ST                                    
         BNE   LSZ20                                                            
         CLC   =C'ST',AGYOFF                                                    
         BE    LR310                                                            
         B     LR320                                                            
LSZ20    CLC   =C'NY',OFFFILT      NY --> NE/TN                                 
         BNE   LR320                                                            
         CLC   =C'NE',AGYOFF                                                    
         BE    LR310                                                            
         CLC   =C'TN',AGYOFF                                                    
         BNE   LR320                                                            
*                                                                               
* END SPECIAL CODING                                                            
*                                                                               
*&&                                                                             
LR310    DS    0H                                                               
         CLI   DE1SALPH+5,0         ON SALESPERSON?                             
         BE    LR330                                                            
         CLC   SALFILT,AGYSALP                                                  
         BE    LR330                                                            
                                                                                
LR320    DS    0H                  NO MATCH, RESTORE KEY AND SEQ NEXT           
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'D'                                                       
*   TEST END                                                                    
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
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'F'                                                       
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
         MVI   KEY-8,C'F'                                                       
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(MYGETREC),RR=RELO                                             
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
         BNZ   LRSEQ                                                            
                                                                                
LR340    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   LISTNUM,NLISTS      IF LISTING AT MAX, GOTO LSTMON               
         BE    LR440                                                            
                                                                                
         MVC   0(4,R4),KEY+28      SAVE DISK ADDRESS TO LIST                    
                                                                                
         L     R3,ATHISLST         =A(CURRENT LIST LINE)                        
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR342               NO                                           
         LA    R3,PRNTWORK         SET A(PRINT LINE)                            
LR342    EQU   *                                                                
         USING LISTD,R3                                                         
         CLI   MYSCRNUM,X'E5'                                                   
         BE    *+12                                                             
         LA    R0,DARSTATH         IF FIRST RECORD ON LIST                      
         B     *+8                                                              
         LA    R0,DE5STATH                                                      
         CR    R3,R0                                                            
         BH    LR342020                                                         
         TM    KEY,X'D0'           RADIO IN-BOX KEY?                            
         BNO   LR342020            NO  - CAN'T RESTART THIS KEY                 
         MVC   FIRSTKEY,KEY        SAVE FIRST KEY OF LIST                       
LR342020 EQU   *                                                                
         DROP  R6                                                               
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR343               DEFAULT GREEN                                
         NI    LSTSTATH+1,X'FF'-X'0C'                                           
         MVI   LSTCOLOR,C'G'       AND REVNEW                                   
                                                                                
* STATUS                                                                        
LR343    DS    0H                                                               
*                                                                               
         CLI   ERRFLAG,C'Y'                                                     
         BNE   LR344                                                            
         MVI   LSTCOLOR,C'>'                                                    
*                                                                               
LR344    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         TM    RDARMISC,X'04'      VARIOUS W/BRANDS                             
         BZ    LR344A                                                           
         MVC   LSTSTAT,=C'BRAND '                                               
         B     LR380                                                            
*                                                                               
LR344A   DS    0H                                                               
*        GOTO1 =A(FLEMSET),DMCB,(R3),RR=RELO                                    
*        BNZ   LR380               VALUE SET IN ROUTINE                         
*                                                                               
         GOTO1 =A(GENLMOVS),DMCB,(R3),(R6),RR=RELO                              
*                                  DISPLAY CODE MOVED OUT FOR ADDRESS           
         BNZ   LR383               ALTERNATE RETURN                             
*                                                                               
* REVISION FROM AGENCY??                                                        
*                                                                               
LR380    DS    0H                                                               
* FLAG IF ORDER HAS BEEN PRINTED                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        PRESENCE OF FIRST TOUCHED ELEMENT            
         BRAS  RE,GETEL            MEANS THE RECORD HAS BEEN PRINTED            
         BNE   *+8                                                              
         MVI   LSTPRNTD,C'*'                                                    
                                                                                
LR380AA  DS    0H                                                               
         L     R6,AIO                                                           
         OC    RDARRNUM,RDARRNUM                                                
         BZ    LR383                                                            
         CLC   =C'MAT',LSTSTAT     SHOW ONLY MATCH EVEN IF REV MATCH            
         BE    LR381                                                            
         CLC   =C'PEN',LSTSTAT     SHOW ONLY PENDCF                             
         BE    LR381                                                            
         CLC   =C'STA',LSTSTAT     SHOW ONLY STACF                              
         BE    LR381                                                            
         MVC   WORK(L'LSTSTAT),LSTSTAT                                          
         MVC   LSTSTAT(3),=C'REV'                                               
         MVC   LSTSTAT+3(3),WORK                                                
         CLC   =C'OPE',LSTSTAT+3                                                
         BNE   *+8                                                              
         MVI   LSTSTAT+5,C'N'      CHANGE 'REVOPE' TO REVOPN'                   
         CLC   =C'UNL',WORK                                                     
         BE    LR380A                                                           
         CLC   =C'LIN',WORK                                                     
         BNE   LR381                                                            
*                                                                               
LR380A   DS    0H                                                               
         MVC   LSTSTAT+3(3),=C'NEW'                                             
         MVC   LSTSTAT2,SPACES                                                  
*                                                                               
LR381    EQU   *                                                                
*                                  UNLINK REVISION CAN ONLY OCCUR               
         OC    RDARREP#,RDARREP#   IF STATION IS A TAKEOVER STATION             
         BNZ   LR383               AND USER HAD NOT HAVE A CHANCE TO DO         
         MVC   LSTCON#,=C'*MISSING' A TKO FOR THE CONTRACT                      
         B     LR390                                                            
*                                                                               
* CONTRACT NUMBER                                                               
*                                                                               
LR383    DS    0H                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    LR390                                                            
         CLI   ERRFLAG,C'Y'                                                     
         BNE   LR385                                                            
         MVC   LSTCON#,=C'*MISSING'                                             
*        GOTO1 HEXOUT,DMCB,ORDLSTDA,LSTCON#,L'ORDLSTDA,0                        
         MVI   ERRFLAG,C'N'        RESET                                        
         B     LR390                                                            
*                                                                               
LR385    DS    0H                                                               
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,LSTCON#),ALIGN=LEFT                              
                                                                                
* AGENCY ORDER NUMBER                                                           
LR390    DS    0H                                                               
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARKORD                                              
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR390010            NO                                           
         EDIT  (P5,WORK+17),(8,LSTAGY#),ALIGN=LEFT,FILL=0                       
         B     LR390100                                                         
LR390010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR390020            NO                                           
         EDIT  (P5,WORK+17),(8,LIN2STRT+9),ALIGN=LEFT,FILL=0                    
         B     LR390100                                                         
LR390020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR390030            NO                                           
         EDIT  (P5,WORK+17),(8,LIN2STRT+9),ALIGN=LEFT,FILL=0                    
         B     LR390100                                                         
LR390030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR390040            NO                                           
         EDIT  (P5,WORK+17),(8,LIN2STRT+9),ALIGN=LEFT,FILL=0                    
         B     LR390100                                                         
LR390040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR390100            NO                                           
         EDIT  (P5,WORK+17),(8,LIN2STRT+9),ALIGN=LEFT,FILL=0                    
         B     LR390100                                                         
LR390100 EQU   *                                                                
*                                                                               
         CLI   RDARCORT,C'T'                                                    
         BNE   *+8                                                              
         MVI   LSTTFLAG,C'T'                                                    
*                                                                               
* STATION                                                                       
         MVC   WORK(4),RDARKSTA                                                 
         CLI   RDARKSTA+4,C' '                                                  
         BE    LR395                                                            
         MVI   WORK+4,C'-'                                                      
         MVC   WORK+5(1),RDARKSTA+4                                             
         CLI   RDARKSTA+3,C' '                                                  
         BNE   LR395                                                            
         MVC   WORK+3(2),WORK+4                                                 
         MVI   WORK+5,C' '                                                      
LR395    DS    0H                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR395010            NO                                           
         MVC   LSTSTA(6),WORK                                                   
         B     LR395100                                                         
LR395010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR395020            NO                                           
         MVC   LIN2STRT+55(6),WORK                                              
         B     LR395100                                                         
LR395020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR395030            NO                                           
         MVC   LIN2STRT+36(6),WORK                                              
         B     LR395100                                                         
LR395030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR395040            NO                                           
         MVC   LIN2STRT+27(6),WORK                                              
         B     LR395100                                                         
LR395040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR395100            NO                                           
         MVC   LIN2STRT+27(6),WORK                                              
         B     LR395100                                                         
LR395100 EQU   *                                                                
                                                                                
* AGENCY                                                                        
         XC    WORK(12),WORK                                                    
         MVC   WORK(3),RDARKAGY                                                 
         CLC   RDARKAGY+3(2),SPACES                                             
         BE    LR397                                                            
         LA    RE,WORK                                                          
         MVI   WORK+3,C' '                                                      
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RDARKAOF    AGENCY OFFICE                                
LR397    EQU   *                                                                
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR397010            NO                                           
         MVC   LSTAGY(6),WORK                                                   
         B     LR397100                                                         
LR397010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR397020            NO                                           
         MVC   LIN1STRT+09(6),WORK                                              
         B     LR397100                                                         
LR397020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR397030            NO                                           
         MVC   LIN2STRT+18(6),WORK                                              
         B     LR397100                                                         
LR397030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR397040            NO                                           
         MVC   LIN1STRT+26(6),WORK                                              
         B     LR397100                                                         
LR397040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR397100            NO                                           
         MVC   LIN1STRT+26(6),WORK                                              
         B     LR397100                                                         
LR397100 EQU   *                                                                
                                                                                
* OFFICE                                                                        
LR400    DS    0H                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR400010            NO                                           
         MVC   LSTOFF,AGYOFF                                                    
         B     LR400020                                                         
LR400010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR400020            NO                                           
         MVC   LIN2STRT+69(2),AGYOFF                                            
LR400020 EQU   *                                                                
                                                                                
* SALESPERSON                                                                   
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR401010            NO                                           
         MVC   LSTSALP,AGYSALP                                                  
         B     LR401020                                                         
LR401010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR401020            NO                                           
         MVC   LIN2STRT+64(3),AGYSALP                                           
LR401020 EQU   *                                                                
                                                                                
* FLIGHT DATES                                                                  
         GOTO1 DATCON,DMCB,(2,RDARESST),(5,WORK)                                
         MVI   WORK+8,C'-'                                                      
         GOTO1 DATCON,DMCB,(2,RDARESEN),(5,WORK+09)                             
*                                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR402010            NO                                           
         MVC   LSTSTDT(17),WORK                                                 
         B     LR402040                                                         
LR402010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR402020            NO                                           
         MVC   LIN1STRT+44(17),WORK                                             
         B     LR402040                                                         
LR402020 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR402040            NO                                           
         MVC   LIN1STRT+08(17),WORK                                             
LR402040 EQU   *                                                                
                                                                                
* ESTIMATE NUMBER                                                               
*                                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR40A010            NO                                           
         EDIT  RDAREST#,(4,LSTEST#),ALIGN=LEFT                                  
         B     LR40A100                                                         
LR40A010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR40A020            NO                                           
         EDIT  RDAREST#,(4,LIN2STRT+43),ALIGN=LEFT                              
         B     LR40A100                                                         
LR40A020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR40A030            NO                                           
         EDIT  RDAREST#,(4,LIN2STRT+60),ALIGN=LEFT                              
         B     LR40A100                                                         
LR40A030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR40A040            NO                                           
         EDIT  RDAREST#,(4,LIN2STRT+60),ALIGN=LEFT                              
         B     LR40A100                                                         
LR40A040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR40A100            NO                                           
         EDIT  RDAREST#,(4,LIN2STRT+60),ALIGN=LEFT                              
         B     LR40A100                                                         
LR40A100 EQU   *                                                                
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR410                                                            
         USING RDARCLEM,R6                                                      
                                                                                
* ADVERTISER                                                                    
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR40B010            NO                                           
         MVC   LSTADV,RDARCLNM                                                  
         B     LR40B100                                                         
LR40B010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR40B020            NO                                           
         MVC   LIN1STRT+18(12),RDARCLNM                                         
         B     LR40B100                                                         
LR40B020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR40B030            NO                                           
         MVC   LIN1STRT+35(12),RDARCLNM                                         
         B     LR40B100                                                         
LR40B030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR40B040            NO                                           
         MVC   LIN1STRT+35(12),RDARCLNM                                         
         B     LR40B100                                                         
LR40B040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR40B100            NO                                           
         MVC   LIN1STRT+35(12),RDARCLNM                                         
         B     LR40B100                                                         
LR40B100 EQU   *                                                                
                                                                                
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR40C010            NO                                           
* PRODUCT 1                                                                     
         MVC   LSTPRD1,RDARPRN1                                                 
* PRODUCT 2                                                                     
         MVC   LSTPRD2,RDARPRN2                                                 
         B     LR40C100                                                         
LR40C010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR40C020            NO                                           
         MVC   LIN2STRT+27(12),RDARPRN1                                         
         B     LR40C100                                                         
LR40C020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR40C030            NO                                           
         MVC   LIN2STRT+45(12),RDARPRN1                                         
         B     LR40C100                                                         
LR40C030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR40C040            NO                                           
         MVC   LIN2STRT+45(12),RDARPRN1                                         
         B     LR40C100                                                         
LR40C040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR40C100            NO                                           
         MVC   LIN2STRT+45(12),RDARPRN1                                         
         B     LR40C100                                                         
LR40C100 EQU   *                                                                
                                                                                
         DROP  R6                                                               
*                                                                               
LR403    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   LR410               REMOVE REJECT PFKEY OPTION                   
         XC    LSTPRD2,LSTPRD2                                                  
*                                                                               
LR410    DS    0H                                                               
**++**++**                                                                      
* BUYER                                                                         
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BE    LR40D100            YES - NOT ON SCREEN                          
LR40D010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR40D020            NO                                           
         MVC   LIN2STRT+19(08),RDARBUYR                                         
         B     LR40D100                                                         
LR40D020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR40D030            NO                                           
         MVC   LIN1STRT+08(08),RDARBUYR                                         
         B     LR40D100                                                         
LR40D030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR40D040            NO                                           
         MVC   LIN2STRT+36(08),RDARBUYR                                         
         B     LR40D100                                                         
LR40D040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR40D100            NO                                           
         MVC   LIN2STRT+36(08),RDARBUYR                                         
         B     LR40D100                                                         
LR40D100 EQU   *                                                                
         DROP  R6                                                               
                                                                                
**++**++**                                                                      
**&&&&&&&&                                                                      
* NEWDATE/ELAPSED TIME                                                          
                                                                                
         LA    R6,KEY              SET A(KEY IN PROGRESS)                       
         USING RDARREC,R6          OVERLAY WITH PASSIVE KEYS                    
*                                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BE    LR40E100            YES - NOT ON SCREEN                          
***>>>   CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
***>>>   BE    LR40E100            YES - NOT ON SCREEN                          
*                                                                               
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR40E030            NO                                           
         GOTO1 =A(PERVERT1),DMCB,(R3),RR=RELO                                   
         B     LR40E100                                                         
*                                                                               
LR40E030 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR40E037            NO                                           
         L     R6,AIO                                                           
         GOTO1 DATCON,DMCB,(2,RDARDATE-RDARREC(R6)),(5,WORK)                    
         MVC   LIN1STRT+17(08),WORK                                             
         GOTO1 DATCON,DMCB,(2,RDARDATE-RDARREC(R6)),(0,WORK)                    
*                                  CONVERT DATE TO YYMMDD EBCDIC                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)                                     
*                                  CONVERT TODAY'S DATE SIMILARLY               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZICM  R1,DMCB+8,4         GET DAYS INCLUSIVE - INSERT WHOLE            
*                                     WORD FOR SIGN PURPOSES                    
         SRA   R1,16               SHIFT BACK DOWN, PROPAGATE SIGN              
         LTR   R1,R1                                                            
         BNP   LR40E035            NOT POSITIVE                                 
         BCTR  R1,0                POS:  SUB 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=+,ZERO=NOBLANK,ALIGN=LEFT              
         B     LR40E100                                                         
LR40E035 EQU   *                                                                
         AHI   R1,1                NEG: ADD 1 FOR 'NOT INCLUSIVE'               
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT              
         B     LR40E100                                                         
*>>>>>>>>>>>>>                                                                  
LR40E037 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR40E040            NO                                           
         GOTO1 =A(PERVERT2),DMCB,(R3),RR=RELO                                   
         B     LR40E100                                                         
*>>>>>>>>>>>>>                                                                  
LR40E040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR40E100            NO                                           
         XC    WORK(24),WORK                                                    
         GOTO1 DATCON,DMCB,(2,RED07FST),(5,WORK)                                
*                                                                               
         MVC   LIN1STRT+08(08),WORK                                             
         GOTO1 DATCON,DMCB,(2,RED07FST),(0,WORK)                                
*                                  CONVERT DATE TO YYMMDD EBCDIC                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)                                     
*                                  CONVERT TODAY'S DATE SIMILARLY               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZICM  R1,DMCB+8,4         GET DAYS INCLUSIVE - INSERT WHOLE            
*                                     WORD FOR SIGN PURPOSES                    
         SRA   R1,16               SHIFT BACK DOWN, PROPAGATE SIGN              
         LTR   R1,R1                                                            
         BNP   LR40E050            NOT POSITIVE                                 
         BCTR  R1,0                POS:  SUB 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=+,ZERO=NOBLANK,ALIGN=LEFT              
         B     LR40E100                                                         
LR40E050 EQU   *                                                                
         AHI   R1,1                NEG: ADD 1 FOR 'NOT INCLUSIVE'               
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT              
         B     LR40E100                                                         
LR40E100 EQU   *                                                                
         DROP  R6                                                               
                                                                                
**&&&&&&&&                                                                      
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   SVKEY,KEY                                                        
         MVC   KEY(27),RDARKEY     KEY FROM A(IOAREA)                           
         DROP  R6                                                               
*                                                                               
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'50'       GET TRAILER RECORD                           
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   LR430                                                            
                                                                                
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'I'                                                       
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(MYGETREC),RR=RELO                                             
*                                  DARE TRAILER RECORD READ                     
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR420                                                            
         USING RDARELE9,R6                                                      
                                                                                
* TOTAL DOLLARS                                                                 
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR412010            NO                                           
         EDIT  (P6,RDARTDOL),(14,LSTTOTL),2,COMMAS=YES                          
         B     LR412100                                                         
LR412010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR412020            NO                                           
         EDIT  (P6,RDARTDOL),(14,LIN1STRT+29),2,COMMAS=YES                      
         B     LR412100                                                         
LR412020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR412030            NO                                           
         EDIT  (P6,RDARTDOL),(14,LIN1STRT+47),2,COMMAS=YES                      
         B     LR412100                                                         
LR412030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR412040            NO                                           
         EDIT  (P6,RDARTDOL),(14,LIN1STRT+47),2,COMMAS=YES                      
         B     LR412100                                                         
LR412040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR412100            NO                                           
         EDIT  (P6,RDARTDOL),(14,LIN1STRT+47),2,COMMAS=YES                      
         B     LR412100                                                         
LR412100 EQU   *                                                                
*                                                                               
         CLI   RDARCORT,C'T'                                                    
         BNE   LR415                                                            
         MVI   LSTTOTL,C'T'                                                     
                                                                                
* TOTAL SPOTS                                                                   
LR415    DS    0H                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR415010            NO                                           
         EDIT  RDARTSPT,(5,LSTTSPT)                                             
         B     LR415100                                                         
LR415010 EQU   *                                                                
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR415020            NO                                           
         EDIT  RDARTSPT,(5,LIN2STRT+49)                                         
         B     LR415100                                                         
LR415020 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR415030            NO                                           
         EDIT  RDARTSPT,(5,LIN2STRT+66)                                         
         B     LR415100                                                         
LR415030 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR415040            NO                                           
         EDIT  RDARTSPT,(5,LIN2STRT+66)                                         
         B     LR415100                                                         
LR415040 EQU   *                                                                
         CLI   SCRNHDRS,9          'NEWDATE ' SCREEN?                           
         BNE   LR415100            NO                                           
         EDIT  RDARTSPT,(5,LIN2STRT+66)                                         
         B     LR415100                                                         
LR415100 EQU   *                                                                
         DROP  R3,R6                                                            
                                                                                
LR420    DS    0H                  RESTORE LIST KEY, IO AREA                    
         MVC   KEY,SVKEY           BEFORE GOING TO LSTMON                       
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'I'                                                       
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
         MVI   KEY-8,C'J'                                                       
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(MYGETREC),RR=RELO                                             
         BNZ   LRSEQ                                                            
         MVC   SELCTKEY,KEY                                                     
                                                                                
LR430    DS    0H                                                               
         LA    R4,4(R4)            SAVE OFF DISK ADD                            
                                                                                
LR440    DS    0H                                                               
         L     R2,ATHISLST         SET A(SCREEN LINE)                           
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR435               NO                                           
         LA    R2,PRNTWORK         SET A(PRINT LINE)                            
         USING LISTD,R2                                                         
LR435    DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BNO   LR450               NO  -                                        
         DROP  RF                                                               
         L     RF,AIO                                                           
         USING RDARREC,RF                                                       
         MVC   LSTCOMP+2(2),RDARKREP                                            
         DROP  RF                                                               
LR450    EQU   *                                                                
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR460                                                            
         MVC   LINE1CMP+1(LLINE2),LSTCON#                                       
*                                  MOVE 2ND PART OF LINE UP 7 CHARS             
         MVC   LSTADV+13(7),SPACES CLEAR END OF MOVED PORTION                   
         MVC   P+4(LLINE1),LSTCOLOR                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+0(LLINE2),LIN2PRNT                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    PRNTWORK,PRNTWORK   CLEAR PRINTLINE SETUP AREA                   
         DROP  R2                                                               
         B     LR480                                                            
LR460    DS    0H                                                               
         GOTO1 LISTMON                                                          
LR480    DS    0H                                                               
         MVC   SAVRADKY,KEY        SAVE RADIO EDI KEY FOR NEXT PAGE             
*                                                                               
LRSEQ    DS    0H                                                               
*&&DO                                                                           
         CLC   =C'BRAND',CONREC                                                 
         BNE   LRSEQ10                                                          
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    LRSEQ10                                                          
         MVC   KEY(L'RDARKEY),AGYVKEY                                           
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
         MVI   RDARKRT,X'35'       BRAND RECORDS ONLY                           
         MVC   RDARKSEQ,BRANDSEQ                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     BLR10                                                            
         DROP  R3                                                               
*&&                                                                             
*                                                                               
LRSEQ10  DS    0H                                                               
*                                                                               
         CLI   DE1HDLNH+5,0        CONTRACT # ENTERED?                          
         BE    LRSEQ20             NO                                           
*                                                                               
         LR    RF,RA               YES                                          
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BO    LR650020            YES - USE CYCLCON FOR I/O                    
*                                                                               
         DROP  RF                                                               
*                                                                               
LRSEQ20  DS    0H                                                               
*                                                                               
*   TEST                                                                        
*        MVC   KEY-7(1),DMINBTS                                                 
*        MVC   KEY-6(1),GENSTAT1                                                
*        MVI   KEY-5,C'J'                                                       
*   TEST END                                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         LA    R3,KEY              RESET A(KEY FOR FILTERING)                   
         B     LR110                                                            
*                                                                               
LRX      DS    0H                                                               
*   LOOK FOR CONFIRM VERSION OF THE CONTRACT IF NO CON IS DISPLAYED             
         CLI   MYSCRNUM,X'E5'      SHORT LIST SCREEN?                           
         BNE   LRXA                NO                                           
         CLI   LISTNUM,0           IS THERE ANY REC BEEN DISPLAYED?             
         BNE   LRXA                NO                                           
         CLI   DE1HDLNH+5,0        CONTRACT# SPECIFIED?                         
         BE    LRXA                NO                                           
         CLI   STATFILT,C'F'       CONFIRMED?                                   
         BE    LRXA                YES                                          
*                                                                               
         MVI   STATFILT,C'F'       CHEAT BY STICK IN CONFIRM FILTER             
         XC    KEY,KEY                                                          
         B     LR90                AND START THE LOOP AGAIN                     
*                                                                               
LRXA     DS    0H                                                               
         XC    SELCTKEY,SELCTKEY                                                
****     MVC   DARPFLN(17),=C'PF2 billuhr  List'                                
****     FOUT  DARPFLNH                                                         
         MVC   DARLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         ZIC   R0,LISTNUM                                                       
         ZIC   R1,NLISTS                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*   SETHEAD:  FOR PRINTREP MODE, RESET WORKAREA, SET HOOKS                      
*                                                                               
SETHEAD  NTR1                                                                   
*                                                                               
*        IF REPORT, RESTORE MYAREAD FROM STORAGE                                
*                                                                               
         MVC   FULL(2),=Y(WORKLQ)                                               
         ZICM  R1,FULL,2                                                        
         PRINT GEN                                                              
         LA    RF,MYAREAD          SET A(DATA TO BE RESTORED)                   
         L     RE,AIO3             SET A(TEMPORARY STORAGE)                     
         MOVE  ((RF),(R1)),(RE)                                                 
         PRINT NOGEN                                                            
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK2                                                         
         ST    R1,HEADHOOK                                                      
         XIT1                                                                   
         EJECT                                                                  
HOOK2    NTR1                                                                   
         GOTO1 =A(SWAPRHDR),RR=RELO                                             
         XIT1                                                                   
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
INVLSAL  MVC   RERROR,=AL2(448)    INVALID SALESPERSON                          
         B     ERREND                                                           
*                                                                               
NEEDAGY  MVC   RERROR,=AL2(927)    CAMPAIGN FILTER NEEDS AGENCY                 
         B     ERREND                                                           
*                                                                               
NEEDCON  MVC   RERROR,=AL2(938)    CAMPAIGN FILTER NEEDS CONTRACT #             
         B     ERREND                                                           
*                                                                               
NOCMPCD  MVC   RERROR,=AL2(939)    CONTRACT SELECTED CONTAINS NO                
         B     ERREND                 CLIENT/PROD/ESTIMATE CODES                
*                                                                               
INVLCAMP MVC   RERROR,=AL2(926)    EMPTY CAMPAIGN FIELD                         
         B     ERREND                                                           
*                                                                               
INVLUNW  MVC   RERROR,=AL2(945)    UNWIRED MUST BE Y OR N                       
         B     ERREND                                                           
*                                                                               
MASUNWNG MVC   RERROR,=AL2(946)    UNWIRED MUST BE Y FOR MASTER                 
         B     ERREND                                                           
*                                                                               
INVRDATE MVC   RERROR,=AL2(INVDATE) INVALID RCVD DATE FILTER                    
         B     ERREND                                                           
*                                                                               
NEEDOFF  MVC   RERROR,=AL2(449)    SALESPERSON FILTER NEEDS OFFICE FILT         
         B     ERREND                                                           
*                                                                               
TEMPLOCK MVC   RERROR,=AL2(570)    TEMPORARY LOCK                               
         B     ERREND                                                           
*                                                                               
ERTKOVER MVC   RERROR,=AL2(755)    MUST GENERATE TAKEOVER CONTRACT              
         B     ERREND                                                           
*                                                                               
BADREQTR MVC   RERROR,=AL2(871)    MUST BE 3 CHARACTERS                         
         B     ERREND                                                           
*                                                                               
BADERROR MVC   RERROR,=AL2(440)    UNEXPECTED ERROR ENCOUNTERED,                
         LA    R2,DE1HDLNH          CALL DDS                                    
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
NOACCESS MVI   GERROR1,55          SECURITY LOCKOUT                             
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
ERUWLK   MVC   RERROR,=AL2(948)    UNWIRE ORD CAN'T LINK TO NON-UW CON          
         B     ERREND                                                           
*                                                                               
ERLKUW   MVC   RERROR,=AL2(949)    NON-UW CON CAN'T LINK TO UNWIRE ORD          
         B     ERREND                                                           
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
*   STATION CHECK:  IF FIELD ENTERED, VALIDATE VS THIS FILTER                   
*                                                                               
STATNCHK NTR1  LABEL=*,BASE=*                                                   
         L     RF,0(R1)            SET A(KEYDEFTB KEY DEFINITION TABLE)         
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
*                                                                               
         CLI   DE1STATH+5,0        FILTER ON STATION?                           
         BE    STAT0200            NO  - EXIT CC ZERO                           
         CLC   =C'M=',DE1STAT      YES - MARKET FILTER?                         
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
         GOTO1 =A(READAGYO),RR=RELO                                             
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
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
         SR    R0,R0               SET CC ZERO                                  
         B     STAT0240            EXIT                                         
STAT0220 EQU   *                                                                
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
         CLI   DE1INBXH+5,0        ANY IN-BOX FILTER?                           
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
         CLC   DE1INBX(3),0(RF)    KEY FILTER = IN-BOX FILTER?                  
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
         CLI   DE1RDATH+5,0        RECD DATES ARE OPTIONAL                      
         BE    RECD0120                                                         
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*   TEST                                                                        
         MVI   KEY-8,C'C'                                                       
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(MYGETREC),RR=RELO AGENCY HEADER RECORD                        
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
         GOTO1 =A(CHKLOCAL),RR=RELO                                             
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
         GOTO1 =A(CHKDIREC),RR=RELO                                             
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
         GOTO1 SETHEAD                                                          
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
         DC    H'0'                UNRECOGNIZED TYPE                            
DAMN0060 CLC   CONREC(0),=C'ORDER   '       SUBTYPE = 3                         
DAMN0061 CLC   CONREC(0),=C'CAMPAIGN'       SUBTYPE = 1                         
DAMN0062 CLC   CONREC(0),=C'*BUYER  '       SUBTYPE = 5                         
DAMN0063 CLC   CONREC(0),=C'FLIGHT  '       SUBTYPE = 7                         
DAMN0064 CLC   CONREC(0),=C'NEWDATE '       SUBTYPE = 9                         
*                                                                               
LIST0020 EQU   *                                                                
         MVC   SCRNHDRS,REDITYPE   SAVE DATA TYPE INDICATOR                     
*                                                                               
         GOTO1 =A(SETFILTR),RR=RELO  set up filter table                        
*                                                                               
* TEST IF PRINT FROM LIST SELECT COLUMN                                         
         GOTO1 =A(PR),RR=RELO                                                   
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
         CLC   =C'D*D',DE1RQTR                                                  
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
         CLC   =C'D*D',DE1RQTR                                                  
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
*   GENLMOVS:  CODE MOVED OUT OF LINE FOR ADDRESSABILITY                        
*                                                                               
GENLMOVS NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R3,0(R1)            SET A(LISTD)                                 
         USING LISTD,R3                                                         
         L     R6,4(R1)            SET A(DARE RECORD)                           
         USING RDARREC,R6                                                       
*                                                                               
         TM    RDARMISC,X'20'      ORDER NOTDARED FROM AGENCY SIDE?             
         BZ    LR345                                                            
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 GREEN FOR NOTDARE                            
         MVI   LSTCOLOR,C'G'                                                    
                                                                                
         MVC   LSTSTAT,=C'NTDARE'                                               
         B     GMOV0380                                                         
                                                                                
LR345    DS    0H                                                               
         CLI   RDARBSTS,C'C'       ORDER RECALLED?                              
         BNE   LR350                                                            
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 RED FOR RECALL/CANCEL                        
         MVI   LSTCOLOR,C'R'                                                    
                                                                                
         MVC   LSTSTAT,=C'RECALL'                                               
         TM    RDARDELS,X'20'      REVISION CANCELLED??                         
         BZ    GMOV0380                                                         
         MVC   LSTSTAT,=C'CANCEL'                                               
         B     GMOV0380                                                         
                                                                                
LR350    DS    0H                                                               
         CLI   RDARKTYP,X'51'      LOOKING AT CONFIRMED ORDERS?                 
         BNE   LR351               NO                                           
         MVC   LSTSTAT,=C'CNFRMD'                                               
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    GMOV0390                                                         
*        OI    LSTSTATH+1,X'08'                                                 
         MVI   LSTCOLOR,C'!'                                                    
         B     GMOV0390            WILL TURN THIS TO BLUE                       
                                                                                
LR351    DS    0H                                                               
         TM    RDARMISC,X'80'      ORDER RESENT?                                
         BZ    LR354                                                            
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 GREEN FOR RESEND                             
         MVI   LSTCOLOR,C'G'                                                    
                                                                                
         MVC   LSTSTAT,=C'RESENT'                                               
         B     LR360                                                            
*                                                                               
LR354    DS    0H                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    LR355                                                            
         MVC   LSTSTAT,=C'VARNEW'                                               
         B     LR360                                                            
                                                                                
LR355    DS    0H                                                               
         CLI   RDARMEDI,C'R'                                                    
         BNE   LR355A                                                           
         MVC   LSTSTAT,=C'NEW   '  STATUS DEFAULT TO NEW FOR RADIO EDI          
         B     LR360                                                            
*                                                                               
LR355A   DS    0H                                                               
         MVC   LSTSTAT,=C'UNLNKD'  STATUS DEFAULT TO UNLINKED                   
         OC    RDARREP#,RDARREP#                                                
         BZ    LR370                                                            
         MVC   LSTSTAT,=C'LINKED ' ORDER IS LINKED                              
                                                                                
LR360    DS    0H                                                               
         CLI   RDARBSTS,C'A'                                                    
         BNE   LR363                                                            
         MVC   LSTSTAT,=C'OPENED'                                               
*                                                                               
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR365               GREEN FOR OPENED                             
         MVI   LSTCOLOR,C'G'                                                    
         B     LR365                                                            
*                                                                               
LR363    DS    0H                                                               
         CLI   RDARBSTS,C'M'                                                    
         BNE   LR370                                                            
         MVC   LSTSTAT,=C'AMNDED'                                               
                                                                                
         TM    BITFLAG2,B2SENT                                                  
         BZ    GMOV0375                                                         
         MVC   LSTSTAT2,=C'-S'                                                  
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 RED FOR REJECT                               
         MVI   LSTCOLOR,C'R'                                                    
*                                                                               
LR365    DS    0H                                                               
         TM    BITFLAG2,B2SENT                                                  
         BZ    GMOV0375                                                         
         MVC   LSTSTAT2,=C'-S'                                                  
         CLC   =C'AMNDED',LSTSTAT  AMEND-S STAYS RED                            
         BE    GMOV0375                                                         
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    GMOV0375                                                         
         MVI   LSTCOLOR,C'!'                                                    
         B     GMOV0375            WILL TURN THIS TO BLUE                       
*                                                                               
LR370    DS    0H                  LINKED OR UNLINKED, ORDER CAN                
         CLI   RDARBSTS,C'R'         BE REJECTED                                
         BNE   GMOV0375                                                         
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 RED FOR REJECT                               
         MVI   LSTCOLOR,C'R'                                                    
                                                                                
         MVC   LSTSTAT,=C'REJCTD'                                               
         B     GMOV0380                                                         
*                                                                               
GMOV0375 EQU   *                                                                
         GOTO1 =A(FLEMSET),DMCB,(R3),RR=RELO                                    
*                                                                               
GMOV0380 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     GMOV0400                                                         
GMOV0390 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
GMOV0400 EQU   *                                                                
*                                                                               
         DROP  R3,R6                                                            
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*   SET FIELDS IN SCREEN                                                        
*                                                                               
FLEMSET  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R3,0(R1)            RESET A(LISTD)                               
         USING LISTD,R3                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'        MISC. FLAG ELEMENT                           
         BRAS  RE,GETEL                                                         
         BNE   LR381100                                                         
U        USING RDARFLEM,R6                                                      
*                                                                               
LR381040 EQU   *                                                                
         TM    U.RDARFLG1,X'20'    PENDCF?                                      
         BNO   LR381060            NO                                           
         MVC   LSTSTAT(6),=C'PENDCF'                                            
         MVC   LSTSTAT2,SPACES                                                  
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR381080                                                         
         MVI   LSTCOLOR,C'R'                                                    
         B     LR381080                                                         
*                                                                               
LR381060 EQU   *                                                                
         TM    U.RDARFLG1,X'10'    STACF?                                       
         BNO   LR381070            NO  - NO FLAGS SET: NO SETTING               
         MVC   LSTSTAT(6),=C'STACF '                                            
         MVC   LSTSTAT2,SPACES                                                  
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR381080                                                         
         MVI   LSTCOLOR,C'G'                                                    
         B     LR381080            WILL TURN THIS TO BLUE                       
*                                                                               
LR381070 EQU   *                                                                
         TM    U.RDARFLG1,X'80'    MATCH?                                       
         BNO   LR381100            NO                                           
         TM    U.RDARFLG1,X'40'    SENT TO STATION?                             
         BNO   LR381100            NO                                           
         MVC   LSTSTAT(6),=C'MATCH '                                            
         MVC   LSTSTAT2,=C'-S'                                                  
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR381080                                                         
         MVI   LSTCOLOR,C'G'                                                    
         B     LR381080                                                         
*&&DO                                                                           
LR381020 EQU   *                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR381080                                                         
         MVI   LSTCOLOR,C'R'                                                    
         CLC   LSTSTAT2(2),=C'-S'  SENT TO STATION?                             
         BE    LR381080                                                         
         MVI   LSTCOLOR,C'B'                                                    
         B     LR381080                                                         
*&&                                                                             
LR381080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     LR381120                                                         
LR381100 EQU   *                                                                
         SR    R0,R0                                                            
LR381120 EQU   *                                                                
         DROP  U                                                                
         DROP  R3                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* PERVERT1:  DATE CALCULATION MOVED OUT FOR ADDRESSABILITY           *          
**********************************************************************          
PERVERT1 NTR1  LABEL=*,BASE=*                                                   
         L     R3,0(R1)            RESET A(LISTD)                               
         USING LISTD,R3                                                         
*                                                                               
         L     R6,AIO                                                           
         GOTO1 DATCON,DMCB,(2,RDARESST-RDARREC(R6)),(0,WORK+6)                  
*                                  CONVERT DATE TO YYMMDD EBCDIC                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
*                                  CONVERT TODAY'S DATE SIMILARLY               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZICM  R1,DMCB+8,4         GET DAYS INCLUSIVE - INSERT WHOLE            
*                                     WORD FOR SIGN PURPOSES                    
         SRA   R1,16               SHIFT BACK DOWN, PROPAGATE SIGN              
         LTR   R1,R1                                                            
         BNP   PERV1010            NOT POSITIVE                                 
         BCTR  R1,0                POS:  SUB 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=+,ZERO=NOBLANK,ALIGN=LEFT              
         B     PERV1100                                                         
PERV1010 EQU   *                                                                
         AHI   R1,1                NEG: ADD 1 FOR 'NOT INCLUSIVE'               
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT              
PERV1100 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PERVERT2:  DATE CALCULATION MOVED OUT FOR ADDRESSABILITY           *          
**********************************************************************          
PERVERT2 NTR1  LABEL=*,BASE=*                                                   
         L     R3,0(R1)            RESET A(LISTD)                               
         USING LISTD,R3                                                         
*                                                                               
         L     R6,AIO                                                           
         GOTO1 DATCON,DMCB,(2,KEY+7),(0,WORK)                                   
*                                  CONVERT DATE TO YYMMDD EBCDIC                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)                                     
*                                  CONVERT TODAY'S DATE SIMILARLY               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZICM  R1,DMCB+8,4         GET DAYS INCLUSIVE - INSERT WHOLE            
*                                     WORD FOR SIGN PURPOSES                    
         SRA   R1,16               SHIFT BACK DOWN, PROPAGATE SIGN              
         LTR   R1,R1                                                            
         BNP   PER20020            NOT POSITIVE                                 
         BCTR  R1,0                POS:  SUB 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=+,ZERO=NOBLANK,ALIGN=LEFT              
         B     PER20040                                                         
PER20020 EQU   *                                                                
         AHI   R1,1                NEG: ADD 1 FOR 'NOT INCLUSIVE'               
         EDIT  (R1),(8,LIN2STRT+1),FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT              
PER20040 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SWAP COLUMN HEADERS BASED ON REQUEST                                          
***********************************************************************         
SWAPHDRS NTR1  LABEL=*,BASE=*                                                   
         L     R2,0(R1)            SET A(OUTPUT LINE)                           
         LA    R1,CAMPHDR          SET A(CAMPAIGN HEADER)                       
         CLI   SCRNHDRS,1          CAMPAIGN REQUEST?                            
         BE    SHDR0040            YES                                          
         LA    R1,ORDRHDR          SET A(ORDER    HEADER)                       
         CLI   SCRNHDRS,3          ORDER    REQUEST?                            
         BE    SHDR0040            YES                                          
         LA    R1,BUYRHDR          SET A(*BUYER   HEADER)                       
         CLI   SCRNHDRS,5          *BUYER   REQUEST?                            
         BE    SHDR0040            YES                                          
         LA    R1,FLITHDR          SET A(FLIGHT   HEADER)                       
         CLI   SCRNHDRS,7          FLIGHT   REQUEST?                            
         BE    SHDR0040            YES                                          
         LA    R1,SENTHDR          SET A(NEWDATE  HEADER)                       
         CLI   SCRNHDRS,9          NEWDATE  REQUEST?                            
         BNE   SHDR0060            THIS SHOULDN'T HAPPEN                        
*                                                                               
SHDR0040 DS    0H                                                               
         LA    R0,2                SET LOOP FOR TWO GO-ROUNDS                   
SHDR0050 DS    0H                                                               
         XC    8(78,R2),8(R2)      CLEAR FIRST LINE OF HEADING                  
         MVC   8(LNEWHDR,R2),0(R1)                                              
*                                  INSERT FIRST NEW HEADER                      
         FOUT  (R2)                                                             
         ZIC   R4,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,R4                                                            
         LA    R1,LNEWHDR(R1)      BUMP TO NEXT NEW FIELD                       
         BCT   R0,SHDR0050         GO BACK AND DO 2ND FIELD                     
SHDR0060 DS    0H                                                               
         XIT1                                                                   
*                                                                               
*   REPLACEMENT SCREEN HEADERS FOR DIFFERING VIEWS.                             
*        KEEP ALL ENTRIES THE SAME SIZE.  LOOP DEPENDS ON IT                    
*                                                                               
CAMPHDR  DC    C'Act  Status   Con#     Agy-Off  Advertiser           '         
         DC    C'Total$ Flight          '                                       
LNEWHDR  EQU   *-CAMPHDR                                                        
CAMPHDR2 DC    C'     Starts   EDI#     Buyer    Prod1/Prod2  Est#    '         
         DC    C'  Spts Station  Sal Off'                                       
ORDRHDR  DC    C'Act  Status   Con#     Agy Ord# Stn    Agency Off Sal'         
         DC    C' Advertiser            '                                       
ORDRHDR2 DC    C'     Product 1    Product 2   Flight Dates       Est#'         
         DC    C'         Total$  Spts  '                                       
BUYRHDR  DC    C'Act  Status   Con#     Buyer    1st Sent w/Status Adv'         
         DC    C'ertiser          Total$'                                       
BUYRHDR2 DC    C'     Elapse   EDI#     Agy-Off           Station  Pro'         
         DC    C'd1/Prod2  Est#     Spts'                                       
FLITHDR  DC    C'Act  Status   Con#     Flight            Agy-Off  Adv'         
         DC    C'ertiser          Total$'                                       
FLITHDR2 DC    C'     Starts   EDI#              Station  Buyer    Pro'         
         DC    C'd1/Prod2  Est#     Spts'                                       
SENTHDR  DC    C'Act  Status   Con#     1st Sent w/Status Agy-Off  Adv'         
         DC    C'ertiser          Total$'                                       
SENTHDR2 DC    C'     Elapse   EDI#              Station  Buyer    Pro'         
         DC    C'd1/Prod2  Est#     Spts'                                       
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SWAP COLUMN HEADERS BASED ON REQUEST ON REPORT                                
***********************************************************************         
SWAPRHDR NTR1  LABEL=*,BASE=*                                                   
         LA    R1,RCAMPHDR         SET A(CAMPAIGN HEADER)                       
         CLI   SCRNHDRS,1          CAMPAIGN REQUEST?                            
         BE    SRHD0040            YES                                          
         LA    R1,RORDRHDR         SET A(ORDER    HEADER)                       
         CLI   SCRNHDRS,3          ORDER    REQUEST?                            
         BE    SRHD0040            YES                                          
         LA    R1,RBUYRHDR         SET A(*BUYER   HEADER)                       
         CLI   SCRNHDRS,5          *BUYER   REQUEST?                            
         BE    SRHD0040            YES                                          
         LA    R1,RFLITHDR         SET A(FLIGHT   HEADER)                       
         CLI   SCRNHDRS,7          FLIGHT   REQUEST?                            
         BE    SRHD0040            YES                                          
         LA    R1,RSENTHDR         SET A(NEWDATE  HEADER)                       
         CLI   SCRNHDRS,9          NEWDATE  REQUEST?                            
         BNE   SRHD0060            THIS SHOULDN'T HAPPEN                        
*                                                                               
SRHD0040 DS    0H                                                               
         XC    H1,H1               CLEAR FIRST LINE OF HEADING                  
         XC    H2,H2               CLEAR 2ND   LINE OF HEADING                  
         MVC   H1(LRNEWHDR),0(R1)   MOVE TO HEAD1                               
         LA    R1,LRNEWHDR(R1)      BUMP TO SEND HALF                           
         MVC   H2(LRNEWHDR),0(R1)   MOVE TO HEAD2                               
*                                                                               
SRHD0060 DS    0H                                                               
         XIT1                                                                   
*                                                                               
*   REPLACEMENT SCREEN HEADERS FOR DIFFERING VIEWS.                             
*        KEEP ALL ENTRIES THE SAME SIZE.  LOOP DEPENDS ON IT                    
*                                                                               
RCAMPHDR DC    C'Act  Status   Con#     Agy-Off  Advertiser           '         
         DC    C'Total$ Flight          '                                       
LRNEWHDR EQU   *-RCAMPHDR                                                       
RCAMPHD2 DC    C'     STARTS   EDI#     BUYER    PROD1/PROD2  EST#    '         
         DC    C'  Spts Station  Sal Off'                                       
RORDRHDR DC    C'Act  Status   Con#     Agy Ord# Stn    Agency Off Sal'         
         DC    C' Advertiser            '                                       
RORDRHD2 DC    C'     PRODUCT 1    PRODUCT 2   FLIGHT DATES       EST#'         
         DC    C'         Total$  Spts  '                                       
RBUYRHDR DC    C'Act  Status   Con#     Buyer    1st Sent w/Status Adv'         
         DC    C'ertiser          Total$'                                       
RBUYRHD2 DC    C'     ELAPSE   EDI#     AGY-OFF           STATION  PRO'         
         DC    C'd1/Prod2  Est#     Spts'                                       
RFLITHDR DC    C'Act  Status   Con#     Flight            Agy-Off  Adv'         
         DC    C'ertiser          Total$'                                       
RFLITHD2 DC    C'     STARTS   EDI#              STATION  BUYER    PRO'         
         DC    C'd1/Prod2  Est#     Spts'                                       
RSENTHDR DC    C'Act  Status   Con#     1st Sent w/Status Agy-Off  Adv'         
         DC    C'ertiser          Total$'                                       
RSENTHD2 DC    C'     ELAPSE   EDI#              STATION  BUYER    PRO'         
         DC    C'd1/Prod2  Est#     Spts'                                       
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE MARKET CODE                                                          
***********************************************************************         
VALIMKT  NTR1  LABEL=*,BASE=*                                                   
         XC    WORK,WORK                                                        
         ZIC   RF,DE1STATH+5       L(STATION FIELD)                             
         CLI   DE1STATH+5,6        FIELD CAN ONLY CONTAIN 6 CHARS MAX           
         BH    VMKT0900            ERROR                                        
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R3,KEY                                                           
         USING RMKTKEY,R3                                                       
         MVI   KEY,X'2B'                                                        
         MVC   RMKTKREP,AGENCY     INSERT REP CODE                              
         MVC   RMKTKMKT,DE1STAT+2  INSERT FOUR CHARS MKT CODE                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VMKT0900            NO  - SEND ERROR MESSAGE                     
         MVC   STAFILT(6),DE1STAT  SAVE MKT FILTER AS ENTERED                   
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
         CLC   =C'K3',AGENCY       SIGNON TO KATZ RADIO GROUP?                  
         BNE   VREP0010            NO                                           
         CLC   =C'NU',DE1REP       YES - CLEAR CHANNEL AS SUB?                  
         BE    VREP0030            YES - ACCEPT THIS INPUT                      
VREP0010 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,1               SET REP RECORD TYPE                          
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BZ    VREP0015                                                         
         MVC   KEY+25(2),MSTREPCX  INSERT MASTER ALPHA ID INTO KEY              
         B     VREP0018                                                         
         DROP  R2                                                               
*                                                                               
VREP0015 EQU   *                                                                
         MVC   KEY+25(2),AGENCY    INSERT ALPHA ID INTO KEY                     
*                                                                               
VREP0018 EQU   *                                                                
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
         CLC   DE1REP(2),0(RF)     REQUESTED ALPHA IN LIST?                     
         BE    VREP0030            YES - ACCEPT IT                              
         LA    RF,2(RF)            NO  - BUMP TO NEXT ENTRY IN TABLE            
         BCT   R0,VREP0020         GO BACK FOR NEXT                             
         B     VREP0080            SHOULDN'T HAPPEN, BUT...                     
VREP0030 EQU   *                                                                
         MVC   COMPREP,DE1REP      SAVE 2-CHAR REP ID                           
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
VREP0800 MVC   WORK+15(0),DE1REP   LOAD SOURCE REP BY LENGTH                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
VREC     NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'REMOVE',DE1HDLN  REMOVE REQUEST?                              
         BNE   VR01                NO                                           
*                                                                               
         GOTO1 =A(VALREM),RR=RELO                                               
         BNE   INVLACT2                                                         
*                                                                               
         GOTO1 =A(DELORD),RR=RELO                                               
*                                                                               
         GOTO1 =A(MARKCON),RR=RELO                                              
*                                                                               
         XC    DE1HDLN,DE1HDLN                                                  
         MVI   DE1HDLNH+5,0                                                     
         B     EXIT2                                                            
*                                                                               
VR01     DS    0H                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   VR05                                                             
         CLC   =C'CONFIRM',DE1HDLN                                              
         BE    VR03                                                             
         CLC   =C'KILLEM',DE1HDLN                                               
         BE    VR03                                                             
*                                                                               
         B     VR05                                                             
*&&DO                                                                           
         CLC   =C'DELETE',DE1HDLN                                               
         BNE   VR05                                                             
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'$EDI$',RDARAGAD  CAN'T DELETE UNLESS EDI ORDER                
         BNE   INVLACT                                                          
         DROP  R6                                                               
*&&                                                                             
*                                                                               
VR03     DS    0H                                                               
         GOTO1 =A(DELORD),RR=RELO                                               
         CLC   =C'KILLEM',DE1HDLN                                               
         BNE   VR04                                                             
         GOTO1 =A(MARKCON),RR=RELO                                              
         GOTO1 =A(PROCMKG),RR=RELO                                              
*                                                                               
VR04     DS    0H                                                               
         XC    DE1HDLN,DE1HDLN                                                  
         MVI   DE1HDLNH+5,0                                                     
         B     EXIT2                                                            
                                                                                
VR05     DS    0H                                                               
         CLI   ACTNUM,MYACTSEL     ONLY FOR MY SELECT ACTION                    
         BNE   VRX                                                              
         GOTOR GETFLAG             SAVE OFF DARE AND CONTRACT FLAGS             
*                                                                               
         TM    BITFLAG2,B2GOCON    WAS PF2 PRESSED? SWAP TO CONTRACT            
         BZ    VR06                                                             
         GOTO1 =A(SUBROUT),DMCB,(RC),('QSWAPCON',0),RR=RELO                     
         NI    BITFLAG2,X'FF'-B2GOCON                                           
         B     EXIT2                                                            
*                                                                               
VR06     DS    0H                                                               
         TM    BITFLAG3,B3CFCON    WAS PF10 PRESSED? SWAP TO CONTRACT           
         BZ    VR08                                                             
         GOTO1 =A(SUBROUT),DMCB,(RC),('QCFRMCON',0),RR=RELO                     
         NI    BITFLAG3,X'FF'-B3CFCON                                           
         B     EXIT2                                                            
*                                                                               
VR08     DS    0H                                                               
         TM    BITFLAG,BFPRINT     WAS PF5 PRESSED? (REPORT)                    
         BZ    VR10                                                             
*        GOTO1 =A(SUBROUT),DMCB,(RC),('QPR',0),RR=RELO                          
         GOTO1 =A(PR),RR=RELO                                                   
         NI    BITFLAG,X'FF'-BFPRINT                                            
         B     EXIT2                                                            
*                                                                               
VR10     DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BNZ   VR20                                                             
         LA    R2,CONACTH                                                       
         CLI   CALLSP,0                                                         
         BE    INVLACT                                                          
         MVC   CONACT,=CL8'LIST'                                                
         B     EXIT2                                                            
                                                                                
VR20     DS    0H                                                               
         CLI   MYSCRNUM,X'E4'      AGENCY ORDER SCREEN ALREADY LOADED?          
         BE    VR30                                                             
* LOAD LOWER SCREEN                                                             
         GOTO1 CALLOV,DMCB,DE1TAGH,X'D9080FE4'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCRNUM,X'E4'                                                   
*                                                                               
* SPECIAL FOR STEREO. WE NEED TO SET IN THE OUT TRANSLATOR BLOCK THE            
* SCREEN NUMBER THAT WE HAVE RESTORED SO STEREO IS PASSED THE CORRECT           
* SCREEN IN USE.                                                                
*                                                                               
         TM    STEREOFG,STFINUSE   STEREO IS IN USE                             
         BZ    VR30                                                             
*                                                                               
         L     R4,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,R4                                                         
         OI    TIOBINDS,TIOBSCRN   SET SCREEN NUM IN TIOBCNT(1)                 
         MVC   TIOBCNT(1),MYSCRNUM                                              
         DROP  R4                                                               
*                                                                               
VR30     DS    0H                                                               
         MVC   DRVLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
                                                                                
*        OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'SELECTKY),SELECTKY                                         
         BNE   MISSREC                                                          
*        OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   DAREDKAD,KEY+28                                                  
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         GOTO1 =A(CHGPFLN),RR=RELO                                              
*&&DO                                                                           
* CHECK IF CONFIRMED VARIOUS                                                    
*                                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         TM    RDARMISC,X'04'                                                   
         BZ    VR30AA                                                           
         MVC   DRVPFKL(69),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 History  12 Return'                                    
         B     VR50                                                             
*                                                                               
*                                                                               
* SETUP THE PFKEY LINE                                                          
* DEFAULT IF UNLINKED                                                           
*                                                                               
VR30AA   DS    0H                                                               
         MVC   DRVPFKL(74),=C'PF2 Contract  3 Buy List  4 Contract ListX        
                 5 Print  7 Hist  11 Rej  12 Ret'                               
*                                  VARIOUS UNLINK                               
         TM    RDARMISC,X'10'                                                   
         BZ    VR30BB                                                           
         MVC   DRVPFKL(75),=C'PF2 Con  3 Buy Lst  4 Con Lst  5 Print  6X        
                Brand Lst  7 Hist  11 Rej  12 Ret'                              
*                                                                               
VR30BB   DS    0H                  BRAND UNLINK                                 
         TM    RDARMISC,X'08'                                                   
         BZ    VR30A                                                            
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  4 Contract ListX        
                 5 Print  7 History  12 Return'                                 
*                                                                               
VR30A    DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   VR31                REMOVE REJECT PFKEY OPTION                   
         MVC   DRVPFKL+61(5),DRVPFKL+69                                         
         XC    DRVPFKL+66(8),DRVPFKL+66                                         
*                                                                               
VR31     DS    0H                                                               
         TM    RDARMISC,X'20'      CHECK IF NOTDARE ORDER                       
         BZ    VR33                                                             
* IF NOTDARE                                                                    
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(66),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  11 Undare  12 Return'                                       
*                                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    VR32                                                             
         MVC   DRVPFKL+35(39),=C'6 Brand List  7 Hist  11 UnDare  12 Rex        
               t'                                                               
*                                                                               
VR32     DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   VR50                REMOVE REJECT PFKEY OPTION                   
         MVC   DRVPFKL+47(8),DRVPFKL+58                                         
         XC    DRVPFKL+55(11),DRVPFKL+55                                        
         B     VR50                                                             
                                                                                
VR33     DS    0H                                                               
         CLI   RDARBSTS,C'R'                                                    
         BE    VR34                                                             
         CLI   RDARBSTS,C'C'       RECALLED                                     
         BNE   VR35                                                             
                                                                                
* IF REJECTED (LINKED OR UNLINKED) OR RECALLED                                  
VR34     DS    0H                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(55),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  12 Return'                                                  
         TM    RDARMISC,X'10'                                                   
         BZ    VR50                                                             
         MVC   DRVPFKL(69),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 History  12 Return'                                    
         B     VR50                                                             
                                                                                
VR35     DS    0H                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    VR50                                                             
*                                                                               
* IF LINKED                                                                     
*                                                                               
* IF LINKED AND RADIO                                                           
*                                                                               
                                                                                
         CLI   RDARMEDI,C'R'                                                    
         BNE   VR35A                                                            
         XC    DRVPFKL,DRVPFKL                                                  
*                                                                               
         TM    MISCFLG4,MF4PDING                                                
         BO    VR35AA                                                           
         MVC   DRVPFKL(75),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Differences 11 Reject  12 Ret'                              
         B     VR40                                                             
*                                                                               
VR35AA   DS    0H                                                               
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Open  11 Reject  12 Return'                                 
         B     VR40                                                             
*                                                                               
VR35A    DS    0H                                                               
*                                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Open  11 Reject  12 Return'                                 
*                                                                               
* IF LINKED AND REVISION                                                        
*                                                                               
         CLI   RDARRNUM,0                                                       
         BE    VR36                                                             
         MVC   DRVPFKL(75),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Differences 11 Reject  12 Ret'                              
*                                                                               
* IF LINKED AND VARIOUS                                                         
*                                                                               
VR36     DS    0H                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    VR37                                                             
         MVC   DRVPFKL(75),=C'PF2 Con  3 Buy List  5 Print  6 Brand LisX        
               t  7 Hist  10 Open  11 Rej  12 Ret'                              
*                                                                               
* IF LINKED AND BRAND                                                           
*                                                                               
VR37     DS    0H                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    VR38                                                             
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(64),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Open  12 Return'                                         
         B     VR40                                                             
*                                                                               
VR38     DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   VR40                REMOVE REJECT PFKEY OPTION                   
         MVC   DRVPFKL+62(5),DRVPFKL+70                                         
         XC    DRVPFKL+67(8),DRVPFKL+67                                         
*                                                                               
VR40     DS    0H                                                               
         CLI   RDARBSTS,C'A'                                                    
         BNE   VR50                                                             
*                                                                               
* IF OPENED (LINKED IMPLIED)                                                    
*                                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(66),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  11 Reject  12 Return'                                       
*                                                                               
* IF OPENED (LINKED IMPLIED) AND VARIOUS                                        
*                                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    VR45                                                             
         MVC   DRVPFKL(74),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 Hist  11 Reject  12 Ret'                               
*                                                                               
* IF OPENED (LINKED IMPLIED) AND BRAND                                          
*                                                                               
VR45     DS    0H                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    VR47                                                             
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(55),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  12 Return'                                                  
*                                                                               
VR47     DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BE    VR47A               REMOVE REJECT PFKEY OPTION                   
         CLI   RDARKTYP,X'51'      AND CONFIRM ORDER                            
         BNE   VR50                                                             
VR47A    DS    0H                                                               
         MVC   DRVPFKL+47(8),DRVPFKL+58                                         
         XC    DRVPFKL+55(11),DRVPFKL+55                                        
*&&                                                                             
*                                                                               
VR50     DS    0H                                                               
         MVC   SELCTKEY,0(R6)      SAVE FOR WHEN WE RETURN TO LIST              
         MVC   AORDNUM,RDARKORD                                                 
         OC    RDARREP#,RDARREP#                                                
         BZ    VR60                                                             
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,DE1HDLN),ALIGN=LEFT                              
         STC   R0,DE1HDLNH+5       SET LENGTH OF DESTINATION                    
         MVI   DE1HDLNH+4,X'08'    SET VALID NUMERIC                            
         DROP  R6                                                               
*                                                                               
VR60     DS    0H                                                               
         LA    R2,DE1HDLNH                                                      
         CLI   5(R2),0                                                          
         BNE   VR70                                                             
*        BAS   RE,CKGLOB           CHECK IF ANYTHING IN GLOBBER AREA            
         GOTO1 =A(SUBROUT),DMCB,(RC),('QCKGLOB',0),RR=RELO                      
         BNZ   VR80                                                             
*                                                                               
VR70     DS    0H                                                               
         GOTO1 VALICON,DMCB,(R2)                                                
         BNZ   VR80                                                             
*                                  DISPLAY CONTRACT INFORMATION                 
         GOTO1 =A(SUBROUT),DMCB,(RC),('QDISCON',0),RR=RELO                      
                                                                                
VR80     DS    0H                  DISPLAY AGENCY ORDER INFORMATION             
*        GOTO1 =A(SUBROUT),DMCB,(RC),('QDISAORD',0),RR=RELO                     
         GOTO1 =A(DISAORD),RR=RELO                                              
         GOTO1 =A(COMPORD),DMCB,(RC),RR=Y                                       
                                                                                
VR90     DS    0H                                                               
*                                  CHECK REASSIGN FIELD                         
         LA    R2,DRVNPSNH         SET A(CURSOR POSITION)                       
         GOTO1 =A(REASSIGN),RR=RELO                                             
         BZ    VR92                NO ERROR                                     
         CLI   DUB,4               TEST ORDER FOUND?                            
         BE    TESTORDR            ERROR                                        
         CLI   DUB,3               LEAVE DATE?                                  
         BE    SPLEFT              ERROR                                        
         CLI   DUB,2               KEEP FROM EDI?                               
         BE    KEEPOFF             ERROR                                        
         CLI   DUB,1               NOT ON FILE?                                 
         BE    BADOFFC             ERROR ON OFFICE MATCH                        
         B     MISSREC2            ERROR ON REASSIGN CODE                       
VR92     DS    0H                  VALIDATE LINK/UNLINK                         
         LA    R2,DRVLINKH                                                      
         CLI   5(R2),0                                                          
         BNE   VR110                                                            
*                                                                               
* CHECK IF LINK/UNLINK FROM LIST SELECT COLUMN                                  
* WE ARE USING CTLRFLG1 FOR UN/LINKING BRAND ORDERS SINCE BITFLAG GETS          
* CLEARED GOING FROM BRAND LIST TO BRAND SELECT                                 
*                                                                               
         TM    CTLRFLG1,CF1BRDLK                                                
         BZ    VR95                IF BRAND LINK FLAG SET                       
         OI    BITFLAG,BFLINK      SET FLAG TO LINK                             
         NI    CTLRFLG1,X'FF'-CF1BRDLK  AND FLAG LINK FLAG                      
         B     VR98                AND LINK FLAG LINK BLAH BLAH BLAH            
*                                                                               
VR95     DS    0H                                                               
         TM    CTLRFLG1,CF1BRDUL                                                
         BZ    VR98                IF BRAND UNLINK FLAG SET                     
         OI    BITFLAG,BFUNLINK    SET FLAG TO UNLINK                           
         NI    CTLRFLG1,X'FF'-CF1BRDUL                                          
*                                                                               
VR98     DS    0H                                                               
         TM    BITFLAG,BFLINK+BFUNLINK                                          
         BNZ   VR100                                                            
         TM    1(R2),X'20'         NO ACTION FOUND. IF FIELD IS                 
         BZ    VRX                 PROTECTED, PUT CURSOR AT CON#                
         LA    R2,DE1HDLNH                                                      
         B     VRX                                                              
                                                                                
VR100    DS    0H                                                               
         MVI   5(R2),1             YES, AUTO FILL IN LINK/UNLINK ACTION         
         MVI   8(R2),C'L'                                                       
         TM    BITFLAG,BFUNLINK                                                 
         BZ    *+8                                                              
         MVI   8(R2),C'U'                                                       
         NI    BITFLAG,X'FF'-BFLINK-BFUNLINK                                    
                                                                                
VR110    DS    0H                                                               
         CLI   8(R2),C'U'                                                       
         BE    VR200                                                            
         CLI   8(R2),C'L'                                                       
         BNE   INVLFLD2                                                         
         EJECT                                                                  
*                                                                               
* ACTION IS LINK                                                                
*                                                                               
VR120    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         OC    RDARREP#,RDARREP#                                                
         BNZ   ERLINKED            ORDER IS ALREADY LINKED                      
*                                                                               
         OC    RDARRNUM,RDARRNUM   IF REVISION AND IS MISSING CONTRACT          
         BZ    VR123               ORDER MUST BE A TAKEOVER                     
         OC    RDARREP#,RDARREP#   WARN USER TO ENTER TAKEOVER CONTRACT         
         BNZ   VR123               MUST GENERATE TAKEOVER CONTRACT              
         XC    DRVLINK,DRVLINK                                                  
         LA    R2,DE1HDLNH                                                      
         B     ERTKOVER                                                         
*                                                                               
VR123    DS    0H                  IF LINK IS REQUESTED                         
         LA    R2,DE1HDLNH         AND NO CONTRACT NUMBER SPECIFIED             
         CLI   5(R2),0             JUMP TO THE CONTRACT PROGRAM                 
         BNE   VR125                                                            
         GOTO1 =A(SUBROUT),DMCB,(RC),('QGOCON',0),RR=RELO                       
*                                                                               
VR125    DS    0H                                                               
         CLC   CCONKSTA(4),RDARKSTA   DO STATIONS MATCH?                        
         BNE   ERDARSTA                                                         
                                                                                
         CLI   CCONKSTA+4,C' '     CHECK IF TV                                  
         BNE   VR130                                                            
         CLI   RDARKSTA+4,C'T'                                                  
         BE    VR140                                                            
                                                                                
VR130    DS    0H                  CHECK BAND                                   
         CLC   CCONKSTA+4(1),RDARKSTA+4                                         
         BNE   ERDARSTA                                                         
                                                                                
VR140    DS    0H                                                               
         CLC   CDARAGY(20),SPACES  MISSING DARE AGENCY EQUIVALENCY CODE         
         BE    MISDARAG                                                         
                                                                                
         LA    RE,CDARAGY                                                       
         LA    RF,4                                                             
                                                                                
VR145    DS    0H                                                               
         CLC   RDARKAGY(5),0(RE)   AGENCY+OFF MATCH?                            
         BE    VR150                                                            
         LA    RE,5(RE)                                                         
         BCT   RF,VR145                                                         
*                                                                               
* SPECIAL EQUIVALENCY CHECK FOR SELTEL ED2-DE ORDERS THAT WERE                  
* CONVERTED 2/20/96                                                             
*                                                                               
         CLC   =C'SZ',AGENCY       FOR SELTEL ONLY                              
         BNE   ERDARAGY                                                         
         CLC   =C'ED2DE',RDARKAGY                                               
         BNE   ERDARAGY                                                         
*                                                                               
* START DATE MUST BE ON OR AFTER STATION JOIN DATE                              
*                                                                               
VR150    DS    0H                                                               
         LA    R2,FAKEHDR          SETUP FAKE FIELD HEADER                      
         MVI   0(R2),14            FIELD HEADER LENGTH                          
         MVI   5(R2),6             SET FIELD LENGTH                             
         MVC   8(6,R2),DRVASTA                                                  
         GOTO1 VALISTA             GET STATION JOIN DATE -> WORK+43             
         MVC   STAJONDT,WORK+33                                                 
         LA    R2,DE1HDLNH         POINT HERE IF ERROR                          
         GOTO1 DATCON,DMCB,(2,RDARESST),(3,FLTSTART)                            
*                                                                               
         CLC   RTKODATE,FLTSTART   TAKEOVER DATE CHOPPING REQUIRED??            
         BL    VR150A                                                           
         GOTO1 DATCON,DMCB,(3,RTKODATE),(3,FLTSTART)                            
*                                                                               
VR150A   DS    0H                                                               
         CLC   FLTSTART,STAJONDT                                                
         BL    ERSTAJDT                                                         
*                                                                               
         NI    BITFLAG,X'FF'-BFKTZEDI                                           
         CLC   =C'$EDI$',RDARAGAD                                               
         BNE   VR151                                                            
         OI    BITFLAG,BFKTZEDI    FLAG IF KATZ EDI ORDER                       
*                                                                               
VR151    DS    0H                                                               
         NI    BITFLAG2,X'FF'-B2POLORD                                          
         CLC   =C'***',RDARPRD1                                                 
         BNE   VR152                                                            
         OI    BITFLAG2,B2POLORD    FLAG IF TRUE POOL                           
*                                                                               
VR152    DS    0H                                                               
         NI    BITFLAG2,X'FF'-B2VARORD                                          
         TM    RDARMISC,X'10'                                                   
         BZ    VR152A                                                           
         OI    BITFLAG2,B2VARORD    FLAG IF VARIOUS ORDER                       
         NI    BITFLAG2,X'FF'-B2POLORD                                          
*                                                                               
VR152A   DS    0H                                                               
         NI    BITFLAG2,X'FF'-B2BRDORD                                          
         TM    RDARMISC,X'08'                                                   
         BZ    VR153                                                            
         OI    BITFLAG2,B2BRDORD    FLAG IF BRAND ORDER                         
         DROP  R6                                                               
*                                                                               
* ALSO MAKE SURE NO BUYS ARE ATTACHED                                           
*                                                                               
VR153    DS    0H                                                               
         PACK  WORK(1),CCONNUM+3(1) REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),CCONNUM+2(1)                                           
         PACK  WORK+2(1),CCONNUM+1(1)                                           
         PACK  WORK+3(1),CCONNUM(1)                                             
         LA    R6,KEY                                                           
         USING RBUYKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'      BUY RECORDS                                  
         MVC   RBUYKREP,AGENCY                                                  
         MVC   RBUYKCON,WORK                                                    
         DROP  R6                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED AS WELL                    
         GOTO1 HIGH                                                             
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   VR153A              BUY NOT FOUND IN CONTRACT                    
*                                                                               
         L     R6,AIO              BUY FOUND IN CONTRACT                        
         USING RDARREC,R6                                                       
         CLI   RDARMEDI,C'R'       RADIO?                                       
         BNE   ERBUYFND            NO,ERROR                                     
         OI    MISCFLG4,MF4RADIO   YES, RADIO EDI COULD LINK CON/BUY            
         DROP  R6                                                               
*                                                                               
VR153A   DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   INVLFLD2                                                         
         GOTO1 GETREC                                                           
                                                                                
* CHECK IF CONTRACT IS PENDING                                                  
         GOTO1 =A(VALLNK),RR=RELO  LINKING VALIDATION                           
                                                                                
         NI    MISCFLG4,X'FF'-MF4CONCF-MF4CNWIP                                 
         L     R6,AIO                                                           
* SET UP MISC FLAGS HERE FOR LATER USE                                          
         MVI   ELCODE,X'1F'        EXPANPDED SAR ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   VR154                                                            
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRM NOW?                                 
         BZ    VR154                                                            
         OI    MISCFLG4,MF4CONCF   CONTRACT IS CONFIRM                          
         DROP  R6                                                               
*                                                                               
VR154    DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'20'        MISC FLAG ELT                                
         BRAS  RE,GETEL                                                         
         BNE   VR154A                                                           
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'10'+X'20' WIP?                                        
         BO    VR154A              NO                                           
         OI    MISCFLG4,MF4CNWIP   YES,CONTRACT IS WIP                          
         DROP  R6                                                               
*                                                                               
VR154A   DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         TM    RCONMODR,X'10'      IS THIS A PENDING CONTRACT?                  
         BO    *+8                                                              
         OI    MISCFLG4,MF4PDING   YES, PENDING                                 
*                                                                               
* CHECK FOR PREBOOK CONTRACT HERE                                               
         TM    MISCFLG4,MF4RADIO   SKIP SOME CHECK FOR RADIO EDI                
         BO    VR155A                                                           
*                                                                               
         TM    RCONMODR,X'10'      IS THIS A PENDING CONTRACT?                  
         BO    ERNOTPEN                                                         
         DROP  R6                                                               
                                                                                
         L     R6,AIO              YES, BUT IS IT A FORECAST CONTRACT?          
         MVI   ELCODE,X'12'        EXPANPDED SAR ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   VR155                                                            
                                                                                
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS FORECAST FLAG          
         BL    VR155                                                            
         TM    RSARXFLG,X'10'      FLAGGED AS FORECAST?                         
         BO    ERNOTPEN                                                         
         DROP  R6                                                               
                                                                                
VR155    DS    0H                                                               
         L     R6,AIO              PENDING K HAS                                
         MVI   ELCODE,3            NO ESTIMATE BUCKET                           
         BRAS  RE,GETEL                                                         
         BE    ERNOTPEN                                                         
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,4            NO INVOICE BUCKET                            
         BRAS  RE,GETEL                                                         
         BE    ERNOTPEN                                                         
                                                                                
         L     R6,AIO              NO SPL/EPL DATA                              
         MVI   ELCODE,6                                                         
         BRAS  RE,GETEL                                                         
         BE    ERNOTPEN                                                         
                                                                                
VR155A   DS    0H                                                               
         NI    MISCFLG4,X'FF'-MF4RADIO    TURN OFF RADIO EDI                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'1D'                                                     
         USING RCONDREL,R6                                                      
         BRAS  RE,GETEL            IF THERE A DARE ELEMENT?                     
         BE    VR160                                                            
         XC    ELEM,ELEM           NO, THEN ADD ONE                             
         LA    R6,ELEM                                                          
         MVI   RCONDRCD,X'1D'                                                   
         MVI   RCONDRLN,RCONDL2Q                                                
         MVI   RCONDRFG,X'80'                                                   
         MVC   RCONDRLK,AORDNUM                                                 
*                                                                               
         TM    BITFLAG,BFKTZEDI    FLAG IF KATZ EDI ORDER                       
         BZ    VR156                                                            
         OI    RCONDRFG,X'04'                                                   
         NI    BITFLAG,X'FF'-BFKTZEDI                                           
*                                                                               
VR156    DS    0H                                                               
         TM    BITFLAG2,B2POLORD   FLAG IF TRUE POOL ORDER                      
         BZ    VR156A                                                           
         OI    RCONDRF2,X'10'                                                   
         NI    BITFLAG2,X'FF'-B2POLORD                                          
*                                                                               
VR156A   DS    0H                                                               
         TM    BITFLAG2,B2VARORD   FLAG IF VARIOUS ORDER                        
         BZ    VR157                                                            
         OI    RCONDRF2,X'80'                                                   
         NI    BITFLAG2,X'FF'-B2VARORD                                          
*                                                                               
VR157    DS    0H                                                               
         TM    BITFLAG2,B2BRDORD   FLAG IF BRAND ORDER                          
         BZ    VR158                                                            
         OI    RCONDRF2,X'40'                                                   
         NI    BITFLAG2,X'FF'-B2BRDORD                                          
         MVC   RCONDRVN,VARNUM     SAVE OFF VAR NUM FOR THIS BRAND              
         MVC   RCONDRCN,VARKNUM    AND CON NUM FOR THIS VAR                     
*                                                                               
VR158    DS    0H                                                               
         GOTO1 ADDELEM                                                          
         B     VR170                                                            
*                                                                               
* REP LINKS THEN AGENCY RECALLS AND RESENDS. THE CONTRACT NUMBER IS             
* NOT PRESERVED ON THE RESEND. HOWEVER, THE CONTRACT IS STILL LINKED            
* TO RESENT ORDER. CHECK THERE TO ALLOW LINK OF RESEND ORDER                    
*                                                                               
VR160    DS    0H                  YES, WE FOUND A DARE ELEMENT                 
         TM    RCONDRFG,X'80'      IS CONTRACT LINKED ALREADY?                  
         BZ    VR163                                                            
         CLC   RCONDRLK,AORDNUM                                                 
         BNE   ERCONLNK                                                         
         TM    RCONDRFG,X'10'      RECALLED AND RESEND??                        
         BZ    ERCONLNK            NO, ERROR                                    
*                                                                               
VR163    DS    0H                                                               
         OI    RCONDRFG,X'80'      NO, IT IS NOW                                
         MVC   RCONDRLK,AORDNUM                                                 
*                                                                               
         TM    BITFLAG,BFKTZEDI    FLAG IF KATZ EDI ORDER                       
         BZ    VR165                                                            
         OI    RCONDRFG,X'04'                                                   
         NI    BITFLAG,X'FF'-BFKTZEDI                                           
         DROP  R6                                                               
*                                                                               
VR165    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'1D'                                                     
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING RCONDREL,R6                                                      
         MVI   RCONDRLN,RCONDL2Q   INSERT NEW LENGTH IF NOT ALREADY             
*                                                                               
         TM    BITFLAG2,B2VARORD    FLAG IF VARIOUS ORDER                       
         BZ    VR168                                                            
         OI    RCONDRF2,X'80'                                                   
         NI    BITFLAG2,X'FF'-B2VARORD                                          
*                                                                               
VR168    DS    0H                                                               
         TM    BITFLAG2,B2BRDORD    FLAG IF BRAND ORDER                         
         BZ    VR169                                                            
         OI    RCONDRF2,X'40'                                                   
         NI    BITFLAG2,X'FF'-B2BRDORD                                          
*                                                                               
VR169    DS    0H                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR170    DS    0H                                                               
         GOTO1 PUTREC              WRITE OUT THE CONTRACT RECORD                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'1D'                                                     
         USING RCONDREL,R6                                                      
         BRAS  RE,GETEL            IF THERE A DARE ELEMENT?                     
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONDRFG,X'04'+X'02'                                             
         BNZ   VR175               SKIP BUILDING X'BD' FOR EDI ORDER            
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(SUBROUT),DMCB,(RC),('QBDKEY',0),RR=RELO                       
*                                                                               
VR175    DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         MVC   HDRDA2,KEY+28       SAVE D/A OF DARE REC                         
*                                  RE-READ FOR PUTREC                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
*                                                                               
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3:  KEY BUILD AREA                                                  
*        AIO :  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),AIO3,AIO,      X        
               AIO2                                                             
*                                                                               
         GOTO1 =A(REDILNK),RR=RELO   RADIO EDI LINKING(AIO IS MODIFIED          
*                                    IN THIS ROUTINE)                           
         GOTO1 =A(DTTMSTMP),RR=RELO  STAMP LINK DATE/TIME                       
         GOTO1 =A(PREBOOK),RR=RELO   MARK PREBOOK                               
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   RDARREP#,CCONKNUM                                                
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3:  KEY BUILD AREA                                                  
*        R6  :  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         L     R4,AIO3             PULL NEW PASSIVE POINTER                     
         LA    R4,800(R4)                                                       
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R4),AIO,      X        
               AIO2                                                             
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         L     R4,AIO3             A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),AIO3,(R4),     X        
               HDRDA2                                                           
*                                                                               
         GOTO1 PUTREC                                                           
         MVC   DRVSTAT(40),=C'*** ORDER IS LINKED TO CON#          ***'         
*                                                                               
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         LA    R2,DRVSTATH                                                      
         EDIT  (P5,WORK+17),(8,36(R2)),ALIGN=LEFT                               
         OI    6(R2),X'80'         XMIT                                         
         OC    RDARRNUM,RDARRNUM   CANNOT UNLINK FOR REVISIONS                  
         BNZ   VR179                                                            
         MVC   DRVQLNK(16),=C'(U)nlink Order ?'                                 
         OI    DRVQLNKH+6,X'80'                                                 
         XC    DRVLINK,DRVLINK                                                  
         MVI   DRVLINKH+5,0                                                     
         OI    DRVLINKH+6,X'80'+X'40' XMIT AND PLACE CURSOR                     
                                                                                
* SHOW NEW PFKEY ACTIONS ALLOWED FOR LINKED ORDER                               
VR179    DS    0H                                                               
         GOTO1 =A(CHGPFLN),RR=RELO                                              
*&&DO                                                                           
* IF LINK=A(CHGPFLN),RR=RELO                                                    
*                                                                               
                                                                                
         CLI   RDARMEDI,C'R'                                                    
         BNE   VR179A                                                           
         TM    MISCFLG4,MF4PDING                                                
         BO    VR179AA                                                          
         MVC   DRVPFKL(75),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Differences 11 Reject  12 Ret'                              
         B     VR190                                                            
VR179AA  DS    0H                                                               
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Open  11 Reject  12 Ret'                                 
         B     VR190                                                            
                                                                                
*                                                                               
VR179A   DS    0H                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Open  11 Reject  12 Ret'                                 
*                                                                               
* IF LINKED AND BRAND                                                           
*                                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    VR180                                                            
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(64),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Open  12 Return'                                         
*                                                                               
VR180    DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   VR190               REMOVE REJECT PFKEY OPTION                   
         MVC   DRVPFKL+59(5),DRVPFKL+70                                         
         XC    DRVPFKL+64(11),DRVPFKL+64                                        
*                                                                               
VR190    DS    0H                                                               
         OI    DRVPFKLH+6,X'80'    XMIT                                         
*&&                                                                             
                                                                                
         B     LINKED                                                           
         DROP  R6                                                               
         EJECT                                                                  
* ACTION IS UNLINK                                                              
VR200    DS    0H                  IF UNLINK IS REQUESTED                       
         CLI   DRVLINK,C'U'                                                     
         BNE   VRX                                                              
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OC    RDARREP#,RDARREP#                                                
         BZ    ORDNOTLK            ORDER IS NOT LINKED TO ANY CONTRACT          
         CLI   RDARBSTS,C'A'                                                    
         BE    ERAPVLNK            CANNOT UNLINK AN OPENED ORDER                
*                                                                               
         CLI   RDARMEDI,C'R'       EXTRA VALIDATION FOR RADIO UNLINK            
         BNE   VR200A                                                           
         GOTO1 =A(VALUNLNK),RR=RELO                                             
         BNE   SENDED              ERROR, CONTRACT SEND AFTER LINK              
         DROP  R6                                                               
*                                                                               
VR200A   DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   RECNTFND                                                         
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'1D'                                                     
         USING RCONDREL,R6                                                      
         BRAS  RE,GETEL            IF THERE A DARE ELEMENT?                     
         BNE   VR210               DON'T DIE IF NOT FOUND, JUST SKIP IT         
                                                                                
         TM    RCONDRFG,X'80'      IS CONTRACT UNLINKED ALREADY?                
         BZ    ERUNLNKD            YES, ERROR                                   
         NI    RCONDRFG,X'FF'-X'80' NO, IT IS NOW                               
         NI    RCONDRFG,X'FF'-X'04' CLEAR KATZ EDI FLAG, TOO...JIC              
         XC    RCONDRLK,RCONDRLK   CLEAR AGENCY NUMBER                          
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC              WRITE OUT THE CONTRACT RECORD                
*                                                                               
         GOTO1 =A(SUBROUT),DMCB,(RC),('QBDKEY',0),RR=RELO                       
*                                                                               
VR210    DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         MVC   HDRDA2,KEY+28       SAVE D/A OF DARE REC                         
*                                  RE-READ FOR PUTREC                           
         GOTO1 GETREC                                                           
*                                                                               
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3:  KEY BUILD AREA                                                  
*        AIO :  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),AIO3,AIO,      X        
               AIO2                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         XC    RDARREP#,RDARREP#                                                
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         USING RDARFLEM,R6                                                      
         NI    RDARFLG1,X'FF'-X'10'-X'C0'-X'20'-X'04'                           
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
                                                                                
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3:  KEY BUILD AREA                                                  
*        R6  :  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         L     R4,AIO3             PULL NEW PASSIVE POINTER                     
         LA    R4,800(R4)                                                       
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R4),AIO,      X        
               AIO2                                                             
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         L     R4,AIO3             A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),AIO3,(R4),     X        
               HDRDA2                                                           
*                                                                               
* UPDATE RECORD                                                                 
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         XC    DRVSTAT,DRVSTAT                                                  
         MVC   DRVSTAT(27),=C'*** ORDER IS NOT LINKED ***'                      
         OI    6(R2),X'80'         XMIT                                         
         MVC   DRVQLNK(16),=C'  (L)ink Order ?'                                 
         OI    DRVQLNKH+6,X'80'                                                 
         XC    DRVLINK,DRVLINK                                                  
         MVI   DRVLINKH+5,0                                                     
         OI    DRVLINKH+6,X'80'+X'40' XMIT AND PLACE CURSOR                     
         GOTO1 =A(CHGPFLN),RR=RELO                                              
*                                                                               
* RESET PFKEY AFTER UNLINKED                                                    
*                                                                               
*&&DO                                                                           
VR210A   DS    0H                                                               
         MVC   DRVPFKL(74),=C'PF2 Contract  3 Buy List  4 Contract ListX        
                 5 Print  7 Hist  11 Rej  12 Ret'                               
*                                  VARIOUS UNLINK                               
         TM    RDARMISC,X'10'                                                   
         BZ    VR210B                                                           
         MVC   DRVPFKL(75),=C'PF2 Con  3 Buy Lst  4 Con Lst  5 Print  6X        
                Brand Lst  7 Hist  11 Rej  12 Ret'                              
*                                                                               
VR210B   DS    0H                  BRAND UNLINK                                 
         TM    RDARMISC,X'08'                                                   
         BZ    VR210C                                                           
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  4 Contract ListX        
                 5 Print  7 History  12 Return'                                 
*&&                                                                             
VR210C   DS    0H                                                               
         B     UNLINKED                                                         
         DROP  R6                                                               
                                                                                
VRX      DS    0H                                                               
         B     NEXTREQ                                                          
EXIT2    XIT1                                                                   
*                                                                               
INVLACT2 MVC   RERROR,=AL2(12)                                                  
         B     ERREND                                                           
*                                                                               
INVLACT  MVC   RERROR,=AL2(INVACT)                                              
         B     ERREND                                                           
*                                                                               
MISSREC2 MVC   RERROR,=AL2(NOTFOUND)                                            
         B     ERREND                                                           
*                                                                               
BADOFFC  MVC   RERROR,=AL2(931)                                                 
         B     ERREND                                                           
*                                                                               
TESTORDR MVC   RERROR,=AL2(941)                                                 
         B     ERREND                                                           
*                                                                               
SPLEFT   MVC   RERROR,=AL2(937)                                                 
         B     ERREND                                                           
*                                                                               
KEEPOFF  MVC   RERROR,=AL2(936)                                                 
         B     ERREND                                                           
*                                                                               
INVLFLD2 MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETFLAG SAVE OFF DARE AND CONTRACT FLAGS                                      
***********************************************************************         
GETFLAG  NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         USING IMWORKD,R4                                                       
         XC    SVDARFLG,SVDARFLG                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   REVNUM,RDARRNUM                                                  
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GF10                                                             
         USING RDARFLEM,R6                                                      
         MVC   SVDARFLG,RDARFLG1   SAVE OFF FLAG                                
         DROP  R6                                                               
*                                                                               
GF10     EQU   *                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OC    RDARREP#,RDARREP#   LINK TO CONTRACT?                            
         BZ    GFX                 NO, EXIT                                     
*                                                                               
         MVC   IMSVKEY,KEY         GET CONTRACT FLAG                            
K        USING RCONKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   K.RCONPTYP,X'8C'                                                 
         MVC   K.RCONPREP,AGENCY                                                
         GOTO1 (RFCONNUM,REPFACS),DMCB,(1,RDARREP#),(2,K.RCONPCON)              
         DROP  K                                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   GFX                                                              
*                                                                               
         MVC   IMSVIO,AIO                                                       
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         NI    MISCFLG4,X'FF'-MF4CONCF                                          
         L     R6,AIO                                                           
* SET UP MISC FLAGS HERE FOR LATER USE                                          
         MVI   ELCODE,X'1F'        EXPANPDED SAR ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   GF50                                                             
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRM NOW?                                 
         BZ    GF50                                                             
         OI    MISCFLG4,MF4CONCF   CONTRACT IS CONFIRM                          
         DROP  R6                                                               
*                                                                               
GF50     EQU   *                                                                
         MVC   AIO,IMSVIO                                                       
GFX      EQU   *                                                                
         MVC   KEY,IMSVKEY                                                      
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
* CHANGE PF KEYLINE                                                             
******************************************************************              
CHGPFLN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARKTYP,X'51'                                                   
         BNE   CP03                                                             
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(55),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  12 Return'                                                  
         B     CP50                                                             
*                                                                               
CP03     DS    0H                                                               
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CP10                                                             
         LR    R4,R6                                                            
         USING RDARFLEM,R4                                                      
         TM    RDARFLG1,X'20'      PEND-CF?                                     
         BZ    CP04                                                             
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(60),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 11 Reject  12 Ret'                                             
         B     CP50                                                             
*                                                                               
CP04     DS    0H                                                               
         TM    RDARFLG1,X'C0'      MATCH-S?                                     
         BO    CP05                YES                                          
         TM    RDARFLG1,X'10'      STA-CF?                                      
         BZ    CP10                                                             
CP05     DS    0H                                                               
         XC    DRVPFKL,DRVPFKL                                                  
*                                                                               
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Confirm  11 Reject  12 Ret'                                 
         B     CP50                                                             
*                                                                               
CP10     DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
* CHECK IF CONFIRMED VARIOUS                                                    
*                                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         TM    RDARMISC,X'04'                                                   
         BZ    CP30AA                                                           
         MVC   DRVPFKL(69),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 History  12 Return'                                    
         B     CP50                                                             
*                                                                               
*                                                                               
* SETUP THE PFKEY LINE                                                          
* DEFAULT IF UNLINKED                                                           
*                                                                               
CP30AA   DS    0H                                                               
         MVC   DRVPFKL(74),=C'PF2 Contract  3 Buy List  4 Contract ListX        
                 5 Print  7 Hist  11 Rej  12 Ret'                               
*                                  VARIOUS UNLINK                               
         TM    RDARMISC,X'10'                                                   
         BZ    CP30BB                                                           
         MVC   DRVPFKL(75),=C'PF2 Con  3 Buy Lst  4 Con Lst  5 Print  6X        
                Brand Lst  7 Hist  11 Rej  12 Ret'                              
*                                                                               
CP30BB   DS    0H                  BRAND UNLINK                                 
         TM    RDARMISC,X'08'                                                   
         BZ    CP30A                                                            
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  4 Contract ListX        
                 5 Print  7 History  12 Return'                                 
*                                                                               
CP30A    DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   CP31                REMOVE REJECT PFKEY OPTION                   
         MVC   DRVPFKL+61(5),DRVPFKL+69                                         
         XC    DRVPFKL+66(8),DRVPFKL+66                                         
*                                                                               
CP31     DS    0H                                                               
         TM    RDARMISC,X'20'      CHECK IF NOTDARE ORDER                       
         BZ    CP33                                                             
* IF NOTDARE                                                                    
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(66),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  11 Undare  12 Return'                                       
*                                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    CP32                                                             
         MVC   DRVPFKL+35(39),=C'6 Brand List  7 Hist  11 UnDare  12 Rex        
               t'                                                               
*                                                                               
CP32     DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   CP50                REMOVE REJECT PFKEY OPTION                   
         MVC   DRVPFKL+47(8),DRVPFKL+58                                         
         XC    DRVPFKL+55(11),DRVPFKL+55                                        
         B     CP50                                                             
                                                                                
CP33     DS    0H                                                               
         CLI   RDARBSTS,C'R'       REJECT                                       
         BE    CP34                                                             
         CLI   RDARBSTS,C'M'       AMEND                                        
         BE    CP33A                                                            
         CLI   RDARBSTS,C'C'       RECALLED                                     
         BE    CP34                                                             
         B     CP35                                                             
                                                                                
CP33A    DS    0H                  AMEND-S, REVAMD-S                            
         TM    SVDARFLG,SF1SENT    -S                                           
         BZ    CP33AA                                                           
*                                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(67),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Confirm  12 Return'                                      
         TM    RDARMISC,X'10'                                                   
         BZ    CP50                                                             
         MVC   DRVPFKL(78),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 Hist  10 Confirm  12 Return'                           
         B     CP50                                                             
CP33AA   DS    0H                  AMEND, REVAMD                                
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(69),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Difference 12 Return'                                    
         TM    RDARMISC,X'10'                                                   
         BZ    CP50                                                             
         MVC   DRVPFKL(75),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 Hist  10 Diff  12 Return'                              
         B     CP50                                                             
                                                                                
* IF REJECTED (LINKED OR UNLINKED) OR RECALLED                                  
CP34     DS    0H                                                               
                                                                                
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(55),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  12 Return'                                                  
         TM    RDARMISC,X'10'                                                   
         BZ    CP50                                                             
         MVC   DRVPFKL(69),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 History  12 Return'                                    
         B     CP50                                                             
                                                                                
CP35     DS    0H                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    CP50                                                             
*                                                                               
* IF LINKED                                                                     
*                                                                               
* IF LINKED AND RADIO                                                           
*                                                                               
         CLI   RDARMEDI,C'R'                                                    
         BNE   CP35A                                                            
         XC    DRVPFKL,DRVPFKL                                                  
*                                                                               
         TM    MISCFLG4,MF4PDING                                                
         BO    CP35AA                                                           
         MVC   DRVPFKL(75),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Differences 11 Reject  12 Ret'                              
         B     CP40                                                             
*                                                                               
CP35AA   DS    0H                                                               
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Open  11 Reject  12 Return'                                 
         B     CP40                                                             
*                                                                               
CP35A    DS    0H                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Open  11 Reject  12 Return'                                 
*                                                                               
* IF LINKED AND REVISION                                                        
*                                                                               
         CLI   RDARRNUM,0                                                       
         BE    CP36                                                             
         MVC   DRVPFKL(75),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Differences 11 Reject  12 Ret'                              
*                                                                               
* IF LINKED AND VARIOUS                                                         
*                                                                               
CP36     DS    0H                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    CP37                                                             
         MVC   DRVPFKL(75),=C'PF2 Con  3 Buy List  5 Print  6 Brand LisX        
               t  7 Hist  10 Open  11 Rej  12 Ret'                              
*                                                                               
* IF LINKED AND BRAND                                                           
*                                                                               
CP37     DS    0H                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    CP38                                                             
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(64),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Open  12 Return'                                         
         B     CP40                                                             
*                                                                               
CP38     DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   CP40                REMOVE REJECT PFKEY OPTION                   
         MVC   DRVPFKL+62(5),DRVPFKL+70                                         
         XC    DRVPFKL+67(8),DRVPFKL+67                                         
*                                                                               
CP40     DS    0H                                                               
         CLI   RDARBSTS,C'A'                                                    
         BNE   CP50                CHECK FOR AMEND                              
         CLI   RDARKTYP,X'51'                                                   
         BE    *+12                                                             
         TM    SVDARFLG,SF1SENT    CONFIRM CONTRACT?                            
         BO    CP40CF              USE CONFIRM PFKEY                            
*                                                                               
* IF OPENED   (LINKED IMPLIED)                                                  
*                                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(66),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  11 Reject  12 Return'                                       
*                                                                               
* IF OPENED (LINKED IMPLIED) AND VARIOUS                                        
*                                                                               
         TM    RDARMISC,X'10'                                                   
         BZ    CP45                                                             
         MVC   DRVPFKL(74),=C'PF2 Contract  3 Buy List  5 Print  6 BranX        
               d List  7 Hist  11 Reject  12 Ret'                               
*                                                                               
* IF OPENED (LINKED IMPLIED) AND BRAND                                          
*                                                                               
CP45     DS    0H                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    CP47                                                             
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(55),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  12 Return'                                                  
         B     CP47                                                             
*                                                                               
CP47     DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BE    CP47A               REMOVE REJECT PFKEY OPTION                   
         CLI   RDARKTYP,X'51'      AND CONFIRM ORDER                            
         BE    CP47A                                                            
         B     CP50                                                             
*                                                                               
CP47A    DS    0H                                                               
         MVC   DRVPFKL+47(8),DRVPFKL+58                                         
         XC    DRVPFKL+55(11),DRVPFKL+55                                        
         B     CP50                                                             
*                                                                               
CP40CF   DS    0H                                                               
*                                                                               
* IF OPENED-S (LINKED IMPLIED)                                                  
*                                                                               
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(75),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Confirm  11 Reject  12 Return'                              
*                                                                               
* IF OPENED-S (LINKED IMPLIED) AND VARIOUS                                      
*                                                                               
CP45CF   DS    0H                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    CP47CF                                                           
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(67),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
               ory  10 Confirm  12 Return'                                      
         B     CP50                                                             
*                                                                               
CP47CF   DS    0H                                                               
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BE    CP47ACF             REMOVE REJECT PFKEY OPTION                   
         CLI   RDARKTYP,X'51'      AND CONFIRM ORDER                            
         BE    CP47ACF                                                          
         B     CP50                                                             
*                                                                               
CP47ACF  DS    0H                                                               
*        MVC   DRVPFKL+56(8),DRVPFKL+70                                         
*        XC    DRVPFKL+70(11),DRVPFKL+65                                        
         B     CP50                                                             
CP50     DS    0H                                                               
         MVC   DRVLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         DROP  R4,R6                                                            
CHGPFLNX XIT1                                                                   
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
         GOTO1 =A(SSTACHK),RR=RELO ONLY FOR AMEND, OPEN, WE CHECK THIS          
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
* VALIDATE UNLINK                                                               
* UNLINK IS NOT ALLOWED IF IT WAS LINKED AND THEN SEND TO THE STATION           
*                                                                               
***********************************************************************         
VALUNLNK NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'15'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VALUYES                                                          
         USING RDARLKEM,R6                                                      
         MVC   MYWORK2(L'RDARLKDT+L'RDARLKTM),RDARLKDT                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   SVKEY,KEY                                                        
*                                                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RDARREP#                                              
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING RCONKEY,RF                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  RF                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   RECNTFND                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        CHECK IF SENT TO STATION                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                                                               
* CHECK IF DARE LINK DATE IS EARLIER THAN LAST SENT TO STATION DATE             
*                                                                               
         CLC   MYWORK2(L'RCONSRDT),RCONSRDT                                     
         BH    VALUYES             LINK AFTER SEND                              
         GOTO1 HEXIN,DMCB,RCONSRTI,WORK+12,6                                    
         CLC   MYWORK2+L'RDARLKDT(L'RDARLKTM),WORK+12                           
         BL    VALUNO                                                           
         DROP  R6                                                               
*                                                                               
VALUYES  SR    RC,RC                                                            
VALUNO   LTR   RC,RC                                                            
VALUX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE UNWIRE ORDER LINKING                                                 
***********************************************************************         
VALLNK   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
*                                                                               
         TM    MISCFLG4,MF4RADIO   SKIP VALIDATION IF NOT RADIO                 
         BZ    VALX                                                             
*                                                                               
         TM    SVDARFLG,SF1UW      UNWIRED?                                     
         BO    UWLK                                                             
*                                                                               
         CLI   RCONTYPE,C'N'       NON-UNWIRED ORDER CAN'T BE LINKED            
         BE    ERLKUW              TO UNWIRED CONTRACT                          
         CLI   RCONTYPE,C'X'                                                    
         BE    ERLKUW                                                           
         CLI   RCONTYPE,C'D'                                                    
         BE    ERLKUW                                                           
         B     VALX                                                             
*                                                                               
UWLK     DS    0H                                                               
         CLI   RCONTYPE,C'N'                                                    
         BE    VALX                                                             
         CLI   RCONTYPE,C'X'                                                    
         BE    VALX                                                             
         CLI   RCONTYPE,C'D'       UNWIRED ORDER CAN'T BE LINKED TO             
         BNE   ERUWLK              NON-UNWIRED CONTRACT                         
*                                                                               
VALX     XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* LINK DATE/TIME STAMP                                                          
***********************************************************************         
DTTMSTMP NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'        LINK/DATE ELEMENT                            
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM           NO, THEN ADD ONE                             
         LA    R6,ELEM                                                          
         USING RDARLKEM,R6                                                      
         MVI   RDARLKCD,X'15'                                                   
         MVI   RDARLKEN,RDARLKLQ                                                
         GOTO1 DATCON,DMCB,(5,0),(2,RDARLKDT) LINK DATE                         
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,7,RDARLKTM                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         XIT1                                                                   
***********************************************************************         
* MARK ORDER AS PREBOOK IF LINK TO A PREBOOK-CONTRACT                           
***********************************************************************         
PREBOOK  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'        MISC FLAG ELEMENT                            
*                                                                               
         BRAS  RE,GETEL                                                         
         BNE   PREBOOKX                                                         
         USING RDARFLEM,R6                                                      
         TM    MISCFLG4,MF4PDING                                                
         BO    PREBOOKX                                                         
         OI    RDARFLG1,X'04'      MARK PREBOOK FLAG IN DARE REC                
         DROP  R6                                                               
PREBOOKX DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
* RADIO EDI LINKING NEW RULE                                                    
* THIS ROUTINE DEALS WITH LINKING ORDER WITH CONTRACT/ BUYLINES                 
* LINKING NEW/RESENT ORDER WITH MATCHING:                                       
* CONFIRM CONTRACT NOT IN WIP ->    STACF                                       
* CONFIRM CONTRACT IN WIP ->        NEW/RESENT (DO NOTHING)                     
* UN-CONFIRM CONTRACT NOT IN WIP -> MATCH-S                                     
* UN-CONFIRM CONTRACT IN WIP ->     NEW/RESENT (DO NOTHING)                     
* LINKING NEW/RESENT ORDER WITH NON-MATCHING:                                   
* NEW/RESENT -> NEW/RESENT (DO NOTHING)                                         
*                                                                               
***********************************************************************         
REDILNK  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         TM    MISCFLG4,MF4PDING                                                
         BO    REDILNKX                                                         
         TM    MISCFLG4,MF4CNWIP   CONTRACT IS WIP?                             
         BO    REDILNKX            YES, DO NOTHING                              
* NOT PENDING - HAS BUYLINES ON IT, CHECK TO SEE IF CONFIRM                     
*                                                                               
         MVC   KEYSAVE,0(R6)                                                    
         XC    WORK(20),WORK                                                    
         MVC   WORK(4),ACOMFACS                                                 
         L     RE,=V(REDARTKO)                                                  
         A     RE,RELO                                                          
         STCM  RE,15,WORK+4                                                     
         L     RE,=V(GETBROAD)                                                  
         A     RE,RELO                                                          
         STCM  RE,15,WORK+8                                                     
         GOTO1 =V(REREPDIF),DMCB,(X'40',KEYSAVE),WORK,CCONNUM,RR=RELO           
         BE    LKYES                                                            
LKNO     DS    0H                  CONTRACT <> ORDER                            
         B     REDILNKX            DO NOTHING                                   
*                                  STATUS STAY AS THEY WERE                     
LKYES    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BE    LK100                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'0F0A'    BUILD A 0F ELT                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO,ELEM                           
         B     LKYES                                                            
*                                                                               
LK100    DS    0H                                                               
         LR    R4,R6                                                            
         L     R6,AIO                                                           
         USING RDARFLEM,R4                                                      
*                                                                               
         TM    MISCFLG4,MF4CONCF                                                
         BZ    LK300               NOT CONFIRM                                  
*                                                                               
LK200    DS    0H                  CONFIRM                                      
         OI    RDARFLG1,X'10'      -> STACF                                     
         B     REDILNKX                                                         
LK300    DS    0H                                                               
         OI    RDARFLG1,X'C0'      -> MATCH-S                                   
*                                                                               
REDILNKX DS    0H                                                               
         DROP  R4                                                               
         XIT1                                                                   
*                                                                               
***********************************************************************         
* VALKEY                                                                        
***********************************************************************         
SUBVKEY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,DE1RQTRH                                                      
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
         MVC   REQINIT,DE1RQTR                                                  
         B     VK05                                                             
*                                                                               
VK03     DS    0H                                                               
         MVC   DE1RQTR,REQINIT                                                  
*                                                                               
VK05     DS    0H                                                               
         CLI   ACTNUM,ACTLIST      ONLY FOR ACTION LIST                         
         BNE   VKX                                                              
                                                                                
         MVI   NLISTS,7            LONG LIST                                    
                                                                                
         LA    R2,DE1HDLNH                                                      
         CLI   5(R2),0                                                          
         BE    VK50                                                             
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
         BNZ   MISSREC                                                          
         OI    4(R2),X'20'                                                      
         XC    SELCTKEY,SELCTKEY                                                
                                                                                
         CLI   ACTNUM,ACTLIST      FOR LIST WITH CONTRACT HEADER                
         BNE   VK50                SPECIFIED, STUFF FILTER WITH                 
         CLC   =C'C=',DE1CMP1      CAMPAIGN FILTER?                             
         BE    VK50                YES - DON'T STUFF ANYTHING                   
VK08     EQU   *                                                                
         MVC   DE1STAT(6),ESTATION INSERT FILTERS FROM CONTRACT                 
         OI    DE1STATH+4,X'20'                                                 
         MVI   DE1STATH+5,6                                                     
         OI    DE1STATH+6,X'80'    XMIT                                         
         CLC   DE1STAT+4(2),SPACES                                              
         BNE   *+10                                                             
         MVC   DE1STAT+4(2),=C'-T'                                              
                                                                                
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
                                                                                
         OI    DE1AGYH+4,X'20'                                                  
         XC    DE1AGY,DE1AGY                                                    
         MVC   DE1AGY(4),CCONKAGY  AGENCY                                       
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    VK20                                                             
         MVI   DE1AGY+4,C' '                                                    
         LA    RE,DE1AGY                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
VK20     DS    0H                                                               
         MVC   AGYFILT,CDARAGY     SAVE FOR LIST FILTERING                      
         OI    DE1AGYH+6,X'80'     XMIT                                         
                                                                                
VK50     DS    0H                                                               
*        MVI   MYRECTYP,X'41'      DEFAULT                                      
         LA    R2,DE1TYPEH                                                      
         TM    4(R2),X'20'                                                      
         BO    VK90                                                             
*                                                                               
         XC    SELCTKEY,SELCTKEY                                                
         MVC   STATFILT,DE1TYPE                                                 
         OC    STATFILT,SPACES                                                  
         LA    R3,STATLIST                                                      
VK55     CLC   STATFILT(1),0(R3)                                                
         BE    VK60                                                             
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    INVLFLD                                                          
         B     VK55                                                             
*                                                                               
VK60     DS    0H                                                               
         LA    R3,STATLIST                                                      
VK70     CLC   STATFILT+1(1),0(R3)                                              
         BE    VK80                                                             
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    INVLFLD                                                          
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
         LA    R2,DE1GRPH                                                       
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK100                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    GRPFILT,GRPFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         GOTO1 VALIGRP                                                          
         BNE   INVLGRP                                                          
         MVC   GRPFILT,8(R2)                                                    
         OC    GRPFILT,SPACES                                                   
                                                                                
VK100    DS    0H                  DIV/TEAM FILTER                              
         LA    R2,DE1TEAMH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK110                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    TEAMFILT,TEAMFILT                                                
         CLI   5(R2),0                                                          
         BE    VK110                                                            
         GOTO1 VALITEAM                                                         
         BNE   INVLTEAM                                                         
         MVC   TEAMFILT,8(R2)                                                   
         OC    TEAMFILT,SPACES                                                  
                                                                                
VK110    DS    0H                                                               
         LA    R2,DE1UNWH          ANY FLAG IN UNWIRED?                         
         TM    DE1UNWH+4,X'20'     PREVIOUSLY VALID?                            
         BO    VK115020            YES - DON'T CHECK AGAIN                      
         NI    DE1REPH+4,X'FF'-X'20'                                            
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
         LA    R2,DE1STATH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK120                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    STAFILT,STAFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK120                                                            
         CLC   =C'M=',DE1STAT      REQUEST FOR MARKET FILTER?                   
         BNE   VK115               NO                                           
         GOTO1 =A(VALIMKT),RR=RELO                                              
         BNZ   INVLMKT                                                          
         B     VK120                                                            
VK115    EQU   *                                                                
         GOTO1 VALISTA                                                          
         MVC   STAFILT,WORK                                                     
         OC    STAFILT,SPACES                                                   
         CLI   STAFILT+4,C' '                                                   
         BNE   VK120                                                            
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
                                                                                
VK120    DS    0H                                                               
         LA    R2,DE1AGYH                                                       
                                                                                
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
         MVC   WORK(L'DE1AGY),DE1AGY                                            
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
         BNE   MISDARAG            CHECK IF EQUIVALENCY CODE PRESENT            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RAGY2REC,R6                                                      
         MVC   AGYFILT,RAGY2DAR                                                 
         DROP  R6                                                               
                                                                                
         OC    AGYFILT,AGYFILT     CHECK IF EQUIVALENCY CODE PRESENT            
         BZ    MISDARAG                                                         
         CLC   AGYFILT,SPACES                                                   
         BE    MISDARAG                                                         
*                                                                               
VK160    DS    0H                                                               
         LA    R2,DE1OFFH                                                       
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
         CLI   DE1HDLNH+5,0                                                     
         BNE   VK165                                                            
*                                                                               
         CLI   5(R2),0             DEFAULT OFFICE RESTRICTION                   
         BNE   VK170                                                            
         MVC   DE1OFF(2),TWAACCS+2                                              
         MVI   DE1OFFH+5,2                                                      
         B     VK190                                                            
*                                                                               
VK165    DS    0H                                                               
         CLC   =C'BRAND',CONREC                                                 
         BE    VK190                                                            
         MVC   DE1OFF(2),=C'C='                                                 
         MVC   DE1OFF+2(2),TWAACCS+2                                            
         MVI   DE1OFFH+5,4                                                      
         B     VK190                                                            
*                                                                               
VK170    DS    0H                                                               
*********                                                                       
         CLC   =C'BL',AGENCY                                                    
         BNE   VK175                                                            
         CLC   =C'SA',TWAACCS+2                                                 
         BNE   VK178                                                            
         CLC   =C'PO',DE1OFF                                                    
         BE    VK190                                                            
         CLC   =C'C=PO',DE1OFF                                                  
         BE    VK190                                                            
         B     VK178                                                            
VK175    DS    0H                                                               
         CLC   =C'PV',AGENCY                                                    
         BNE   VK176                                                            
         CLC   =C'SE',TWAACCS+2                                                 
         BNE   VK178                                                            
         CLC   =C'PO',DE1OFF                                                    
         BE    VK190                                                            
         CLC   =C'C=PO',DE1OFF                                                  
         BE    VK190                                                            
VK176    DS    0H                                                               
         CLC   =C'CQ',AGENCY                                                    
         BNE   VK177                                                            
         CLC   =C'LA',TWAACCS+2                                                 
         BNE   VK177A                                                           
         CLC   =C'SF',DE1OFF                                                    
         BE    VK190                                                            
         CLC   =C'C=SF',DE1OFF                                                  
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
         CLC   =C'DN',DE1OFF                                                    
         BE    VK190                                                            
         CLC   =C'DV',DE1OFF                                                    
         BNE   VK178                                                            
         MVC   DE1OFF(2),=C'DN'                                                 
         B     VK190                                                            
*********                                                                       
VK178    DS    0H                                                               
         CLC   DE1OFF(2),TWAACCS+2                                              
         BE    VK180                                                            
         CLC   =C'C=',DE1OFF                                                    
         BNE   NOACCESS                                                         
         CLC   DE1OFF+2(2),TWAACCS+2                                            
         BNE   NOACCESS                                                         
*                                                                               
VK180    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VK200                                                            
*                                                                               
VK190    DS    0H                                                               
         MVC   OFFFILT,8(R2)                                                    
         CLC   =C'C=',DE1OFF                                                    
         BNE   VK200                                                            
*                                                                               
VK195    DS    0H                                                               
         GOTO1 VALIOFF             VALIDATE ONLY IF FILTERING ON                
         BNE   INVLOFF             CONTRACT OFFICE                              
         MVC   OFFFILT,10(R2)                                                   
                                                                                
VK200    DS    0H                                                               
         LA    R2,DE1SALPH                                                      
         TM    DE1SALPH+4,X'20'                                                 
         BO    VK212                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    SALFILT,SALFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK212                                                            
         CLI   DE1OFFH+5,0         SALESPERSON FILTER NEEDS OFFICE              
         BNE   VK210                                                            
         LA    R2,DE1OFFH                                                       
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
         LA    R2,DE1REPH                                                       
***      TM    DE1REPH+4,X'20'     VALIDATE REP EVERY TIME.                     
***      BO    VK220                                                            
         XC    COMPREP,COMPREP                                                  
         CLI   5(R2),0             ANY INPUT IN FIELD?                          
         BE    VK220               NO                                           
         GOTO1 =A(VALIREP),RR=RELO REVALIDATE EVERYTIME                         
         BNZ   INVLREP             REP NOT FOUND                                
*                                                                               
VK220    DS    0H                                                               
         LA    R2,DE1RDATH                                                      
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
         LA    R2,DE1CMP1H                                                      
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
         CLI   DE1CMP1H+5,0        CAMPAIGN FILTER REQUESTED?                   
         BE    VK300               NO  - FIRST FIELD SPEAKS FOR ALL             
         CLC   =C'C=',DE1CMP1      YES - PULL FILTER FROM CONTRACT?             
         BE    VK260               YES                        ,                 
VK255    EQU   *                                                                
         CLI   DE1AGYH+5,0         AGENCY FILTER ENTERED?                       
         BE    NEEDAGY             NO AGENCY FILTER: ERROR                      
         CLI   DE1CMP2H+5,0        NO  - ALL 3 FIELDS MUST BE FILLED            
         BE    INVLCAMP            EMPTY:  ERROR                                
         CLI   DE1CMP3H+5,0                                                     
         BE    INVLCAMP            EMPTY:  ERROR                                
*                                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
*                                                                               
*   VALIDATION FOR THESE FIELDS IS UNCERTAIN.  MUST BE TESTED.                  
*                                                                               
         MVC   CAMPFLD1,DE1CMP1    USE AS ENTERED                               
         OC    CAMPFLD1,SPACES                                                  
         MVC   CAMPFLD2,DE1CMP2    USE AS ENTERED                               
         OC    CAMPFLD2,SPACES                                                  
         LA    R2,DE1CMP3H         SET A(ESTIMATE FIELD)                        
         GOTO1 VPACK               PACK ESTIMATE NUMBER                         
         LTR   R0,R0                                                            
         BZ    BADEST#             ERROR IN INPUT                               
         SPACE 1                                                                
         STCM  R0,15,CAMPFLD3      STORE CAMPAIGN ESTIMATE #                    
*                                                                               
         DROP  R4                                                               
         B     VK300                                                            
VK260    EQU   *                                                                
         CLI   DE1HDLNH+5,0        CONTRACT NUMBER ENTERED?                     
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
         XC    DE1HDLN,DE1HDLN     CLEAR CONTRACT NUMBER                        
         MVI   DE1HDLNH+5,0        SET FIELD LENGTH TO ZERO                     
         FOUT  DE1HDLNH                                                         
         XC    CDARNUM,CDARNUM                                                  
*                                  STUFF AGENCY CODE INTO SCREEN                
         OI    DE1AGYH+4,X'20'                                                  
         XC    DE1AGY,DE1AGY                                                    
         MVC   DE1AGY(4),CCONKAGY  AGENCY                                       
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    VK265                                                            
         MVI   DE1AGY+4,C' '                                                    
         LA    RE,DE1AGY                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
VK265    DS    0H                                                               
         MVC   AGYFILT,CDARAGY     SAVE FOR LIST FILTERING                      
         OI    DE1AGYH+6,X'80'     XMIT                                         
                                                                                
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
         MVC   DE1CMP1,CAMPFLD1                                                 
         MVC   DE1CMP2,CAMPFLD2                                                 
         MVC   DE1CMP3,CAMPFLD3                                                 
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
         STC   R0,DE1CMP3H+5       SET FIELD LENGTH TO WHAT'S LEFT              
VK277    EQU   *                                                                
         CLI   0(R1),C'0'                                                       
         BL    VK279               LESS THAN X'F0': NON-NUMERIC                 
         CLI   0(R1),C'9'                                                       
         BH    VK279               MORE THAN X'F9': NON-NUMERIC                 
         BCTR  R1,0                BACK UP ONE CHARACTER                        
         BCT   R0,VK277            DECREMENT ONE COUNT                          
*                                  DROP-THROUGH:  ALL NUMERIC                   
         OI    DE1CMP3H+4,X'08'    TURN ON VALID NUMERIC                        
VK279    EQU   *                                                                
*                                                                               
         FOUT  DE1CMP1H                                                         
         FOUT  DE1CMP2H                                                         
         FOUT  DE1CMP3H                                                         
*                                  FOUT THE ORIGINAL FORMAT, THEN               
*                                     CONVERT ORIGINAL TO BINARY, WITH          
*                                     ERROR IF NOT NUMERIC                      
*                                                                               
         LA    R2,DE1CMP3H         SET A(ESTIMATE FIELD)                        
         GOTO1 VPACK               PACK ESTIMATE NUMBER                         
         LTR   R0,R0                                                            
         BZ    BADEST#             ERROR IN INPUT                               
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
         OI    DE1STATH+4,X'20'                                                 
         OI    DE1GRPH+4,X'20'                                                  
         OI    DE1TEAMH+4,X'20'                                                 
         OI    DE1STATH+4,X'20'                                                 
         OI    DE1AGYH+4,X'20'                                                  
         OI    DE1OFFH+4,X'20'                                                  
         OI    DE1SALPH+4,X'20'                                                 
         OI    DE1UNWH+4,X'20'                                                  
         OI    DE1REPH+4,X'20'                                                  
         OI    DE1RDATH+4,X'20'                                                 
         OI    DE1CMP1H+4,X'20'                                                 
         OI    DE1CMP2H+4,X'20'                                                 
         OI    DE1CMP3H+4,X'20'                                                 
*                                                                               
VKX      DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
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
* PRINT A REPORT                                                                
***********************************************************************         
PR       NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         TM    BITFLAG,BFPRINT     CAME FROM MY SELECT SCREEN?                  
         BZ    PR05                                                             
         OC    RDARREP#,RDARREP#   UNLINK OR REVISION UNLINK                    
         BZ    PR02                                                             
         OC    RDARRNUM,RDARRNUM   REVISION??                                   
         BNZ   PR03                                                             
*                                                                               
PR02     DS    0H                                                               
         GOTO1 VREDAR30,DMCB,(RC),('QPRONE',0)                                  
         B     PR04                                                             
*                                                                               
PR03     DS    0H                                                               
         GOTO1 VREDAR22,DMCB,(RC),('QPRONE',0)                                  
*                                                                               
PR04     DS    0H                                                               
         GOTO1 VTOUCHED                                                         
         B     PR20                ALL DONE, CLOSE PQ                           
                                                                                
PR05     DS    0H                  PRINT FROM LIST SCREEN                       
         BAS   RE,TEST4PRT                                                      
         BNZ   PRNO                CHECK IF ANYTHING TO PRINT                   
                                                                                
         OI    CTLRFLG1,CF11STKY   SELECTION FOUND, REDISPLAY PAGE              
         OC    RDARREP#,RDARREP#   UNLINK OR REVISION UNLINK                    
         BZ    PR06                                                             
         OC    RDARRNUM,RDARRNUM   REVISION??                                   
         BNZ   PR08                                                             
PR06     DS    0H                                                               
         GOTO1 VREDAR30,DMCB,(RC),0                                             
         B     PR10                                                             
PR08     DS    0H                                                               
         GOTO1 VREDAR22,DMCB,(RC),('QPRONE',0)                                  
                                                                                
PR10     DS    0H                  PROCESS MULTIPLE PRINT REQUESTS              
* IF USER IS PRINTING FROM THE LIST SCREEN AND                                  
* IF THE AGENCY ORDER IS BEING PROCESSED FOR THE FIRST TIME                     
* RECORD CURRENT ACTIVITY DATE/TIME                                             
         GOTO1 VTOUCHED                                                         
                                                                                
         BAS   RE,TEST4PRT         CHECK IF ANYTHING LEFT TO PRINT              
         BNZ   PR20                IF NOT, CLOSE THE PQ                         
         OC    RDARREP#,RDARREP#   UNLINK OR REVISION UNLINK                    
         BZ    PR13                                                             
         OC    RDARRNUM,RDARRNUM   REVISION??                                   
         BNZ   PR15                                                             
PR13     DS    0H                                                               
         GOTO1 VREDAR30,DMCB,(RC),('QPRNEWPG',0)                                
         B     PR10                                                             
PR15     DS    0H                                                               
         GOTO1 VREDAR22,DMCB,(RC),('QPRNEWPG',0)                                
         B     PR10                                                             
                                                                                
PR20     DS    0H                                                               
         OC    RDARREP#,RDARREP#   UNLINK OR REVISION UNLINK                    
         BZ    PR25                                                             
         OC    RDARRNUM,RDARRNUM   REVISION??                                   
         BNZ   PR30                                                             
PR25     DS    0H                                                               
         GOTO1 VREDAR30,DMCB,(RC),('QPRCLOSE',0)                                
         B     PRYES                                                            
*                                                                               
PR30     DS    0H                                                               
         GOTO1 VREDAR22,DMCB,(RC),('QPRCLOSE',0)                                
*                                                                               
PRYES    SR    RC,RC                                                            
PRNO     LTR   RC,RC                                                            
PRXIT    XMOD1                                                                  
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO TEST FOR ANY PRINT CODE ON LIST ACTION COLUMN                      
* ON EXIT, CC IS SET AND                                                        
* IF YES, SELECTKY HAS KEY OF RECORD                                            
*******************************************************************             
TEST4PRT NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TESTPNO                                                          
*                                                                               
         LA    R4,MYLISTDA                                                      
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         B     TSEL30                                                           
*                                                                               
TSEL20   DS    0H                                                               
         LA    R4,4(R4)            NEXT DISK ADDRESS                            
                                                                                
TSEL30   DS    0H                                                               
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL60                                                           
         CLI   8(R2),C'P'                                                       
         BE    TSEL80                                                           
*                                                                               
TSEL60   ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TESTPNO             (E-O-S)                                      
         TM    1(R2),X'20'         SKIP IF PROTECTED                            
         BO    TSEL60                                                           
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL20              SELECT FIELD                                 
         B     TSEL60                                                           
*                                                                               
TSEL80   DS    0H                                                               
*        MVC   8(3,R2),SPACES      CLEAR ACTION                                 
         MVI   8(R2),C' '          CLEAR ACTION                                 
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
                                                                                
*                                  RETURNS KEY OF ORDER TO BE PRINTED           
         OC    0(4,R4),0(R4)                                                    
         BZ    TESTPNO             SELECTED EMPTY ENTRY, EXIT                   
         MVC   KEY+28(4),0(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   SELECTKY,0(R6)                                                   
         CLC   =C'BRAND',CONREC                                                 
         BE    TESTPYES                                                         
         MVC   SVSELKEY,SELECTKY                                                
         B     TESTPYES                                                         
*                                                                               
TESTPYES SR    RC,RC                                                            
TESTPNO  LTR   RC,RC                                                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VARIOUS SUBROUTINES                                                           
***********************************************************************         
CYCLCON  NTR1  LABEL=*,BASE=*                                                   
         XC    HALF,HALF                                                        
         L     RC,0(R1)                                                         
         LA    R2,DE1HDLNH                                                      
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
SVTWAAGY DS    CL2                                                              
SVKEYTYP DS    XL1                                                              
         DROP  R4,R6                                                            
         DS    0H                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VARIOUS SUBROUTINES                                                           
***********************************************************************         
SUBROUT  DS    0H                                                               
         NMOD1 0,**SUBR**,R7,RR=R4                                              
         L     RC,0(R1)                                                         
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         ST    R4,RELO2                                                         
                                                                                
QPR      EQU   1                   PRINT A DRAFT ORDER                          
QDISCON  EQU   2                   DISPLAY CONTRACT DETAIL                      
QDISAORD EQU   3                   DISPLAY AGENCY ORDER DETAIL                  
QGOCON   EQU   4                   GO AND SWAP TO CONTRACT                      
QSETPFKY EQU   5                   SET PFKEYS                                   
QCKGLOB  EQU   6                   CHECK IF GLOBBER CALLS                       
QVARINFO EQU   7                   VARIOUS INFO                                 
QBDKEY   EQU   8                   MAINTAIN BD PASSIVE KEY                      
QSWAPCON EQU   9                   SWAP TO CONTRACT AND DISPLAY IT              
QCFRMCON EQU   10                  CONFIRM CONTRACT                             
                                                                                
*        CLI   4(R1),QPR                                                        
*        BE    PR                                                               
         CLI   4(R1),QDISCON                                                    
         BE    DISCON                                                           
*        CLI   4(R1),QDISAORD                                                   
*        BE    DISAORD                                                          
         CLI   4(R1),QGOCON                                                     
         BE    GOCONPGM                                                         
         CLI   4(R1),QSETPFKY                                                   
         BE    SETPFKYS                                                         
         CLI   4(R1),QCKGLOB                                                    
         BE    CKGLOB                                                           
         CLI   4(R1),QVARINFO                                                   
         BE    VARINFO                                                          
         CLI   4(R1),QBDKEY                                                     
         BE    BDKEY                                                            
         CLI   4(R1),QSWAPCON                                                   
         BE    SWAPCON                                                          
         CLI   4(R1),QCFRMCON                                                   
         BE    CFRMCON                                                          
         DC    H'0'                                                             
*                                                                               
SUBYES   SR    RC,RC                                                            
SUBNO    LTR   RC,RC               SET CONDITION CODE AT EXIT                   
SUBRX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT CONTRACT SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         MVC   H1(20),=C'PENDING AGENCY ORDER'                                  
         GOTO1 CENTER,DMCB,H1,88                                                
         MVC   H2(6),ESTATION                                                   
         MVC   H2+9(20),EMKTNAME                                                
         MVC   H3(9),=C'CONTRACT#'                                              
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),CCONKNUM                                              
         EDIT  (P5,WORK+17),(8,H3+10),ALIGN=LEFT                                
         MVC   H3+22(21),=C'AGENCY PENDING ORDER#'                              
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),PAGYORD                                               
         EDIT  (P5,WORK+17),(8,H3+44),ALIGN=LEFT                                
                                                                                
HOOKX    DS    0H                                                               
         B     SUBRX                                                            
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     SUBRX                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE CONTRACT RECORD                                                   
***********************************************************************         
DISCON   DS    0H                                                               
* DISPLAY AGENCY + AGENCY EXPANSION                                             
                                                                                
         XC    DE5AGYC,DE5AGYC                                                  
         MVC   DE5AGYC(L'CCONKAGY),CCONKAGY                                     
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    DH10                                                             
         LA    RE,DE5AGYC                                                       
         MVI   DE5AGYC+4,C' '                                                   
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
DH10     DS    0H                                                               
         MVC   DE5AGYN,EAGYNAM1                                                 
                                                                                
* BUYER                                                                         
         MVC   DE5BUY,ECONBUYR                                                  
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DH15                                                             
* FOR DDS TERMINALS, DISPLAY D/A OF K, NOT BUYER NAME                           
         XC    DE5BUY,DE5BUY                                                    
         MVC   DE5BUY(4),=C'D/A='                                               
         GOTO1 HEXOUT,DMCB,CCONDKAD,DE5BUY+5,L'CCONDKAD,0                       
                                                                                
* ESTIMATE                                                                      
DH15     DS    0H                                                               
         MVC   DE5EST,CCONIEST                                                  
         CLC   DE5EST,SPACES                                                    
         BNE   *+10                                                             
         MVC   DE5EST,CCONXEST                                                  
                                                                                
* ADVERTISER + NAME                                                             
         MVC   DE5ADV,CCONKADV                                                  
         MVC   DE5ADVN,EADVNAME                                                 
                                                                                
* PRODUCT + PRODUCT NAME                                                        
         MVC   DE5PRD(3),CCONPRD                                                
         OC    CCONPRD,CCONPRD                                                  
         BZ    *+14                                                             
         MVC   DE5PRD+4(16),EPRDNAME                                            
         B     *+10                                                             
         MVC   DE5PRD,EPRDNAME                                                  
                                                                                
* STATION + MARKET                                                              
         MVC   DE5STA(4),CCONKSTA                                               
         MVC   DE5STA+4(3),=C'-TV'                                              
         CLI   CCONKSTA+4,C' '                                                  
         BE    DH17                                                             
         CLI   CCONKSTA+4,C'L'                                                  
         BNE   DH16                                                             
         MVC   DE5STA+4(3),=C'-L '                                              
         B     DH17                                                             
*                                                                               
DH16     DS    0H                                                               
         MVC   DE5STA+4(3),=C'- M'                                              
         MVC   DE5STA+5(1),CCONKSTA+4                                           
                                                                                
DH17     DS    0H                                                               
         CLI   CCONKSTA+3,C' '                                                  
         BNE   DH18                                                             
         MVC   DE5STA+3(3),DE5STA+4                                             
         MVI   DE5STA+6,C' '                                                    
                                                                                
DH18     DS    0H                                                               
         MVC   DE5STAN,EMKTNAME                                                 
                                                                                
* SALESPERSON                                                                   
         MVC   DE5SAL,CCONSAL                                                   
         MVC   DE5OFFC,CCONKOFF                                                 
         MVC   DE5SALN,ESALNAME                                                 
                                                                                
* START AND END DATES                                                           
         MVC   DE5DATE,ECONDATE                                                 
                                                                                
* DAYPART                                                                       
         LA    R2,MYWORK           DAYPARTS                                     
         XC    MYWORK,MYWORK                                                    
         XC    BYTE,BYTE           0=1 CHAR DPT CODE                            
         LA    R4,CSARDPT                                                       
         LA    R3,6                                                             
                                                                                
DH20     L     R6,=A(DPTABLE)                                                   
         A     R6,RELO2                                                         
         OC    0(1,R4),0(R4)                                                    
         BZ    DH50                NO DPT                                       
*                                                                               
         CLI   1(R4),X'C1'                                                      
         BL    DH30                NO CHAR IN CPP PART OF FIELD                 
         CLI   1(R4),X'E9'                                                      
         BH    DH30                NO CHAR IN CPP PART OF FIELD                 
*                                  3 CHAR DPT CODE                              
         MVI   BYTE,1                                                           
         B     DH40                                                             
*                                                                               
DH30     CLC   3(1,R6),0(R4)                                                    
         BE    DH40                ONE CHAR CODE IN RSARDPT                     
         CLI   0(R6),X'FF'                                                      
         BE    DH55                                                             
         LA    R6,L'DPTABLE(R6)                                                 
         B     DH30                                                             
*                                                                               
DH40     CH    R3,=H'6'                                                         
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
                                                                                
         CLI   BYTE,0                                                           
         BNE   *+14                DPT IS THREE CHAR CODE                       
         MVC   0(3,R2),0(R6)       DPT 1 CHAR MOVE FROM TABLE                   
         B     *+10                                                             
         MVC   0(3,R2),0(R4)       MOVE DPT 3 CHAR FROM RSARDPT                 
         LA    R2,3(R2)                                                         
                                                                                
DH50     LA    R4,3(R4)            NEXT DPT FIELD                               
         BCT   R3,DH20                                                          
*                                                                               
DH55     DS    0H                                                               
         MVC   DE5DPRT,MYWORK                                                   
                                                                                
* LENS                                                                          
         LA    R2,MYWORK           PUT OUT LENGTHS AND CLASS                    
         MVC   MYWORK,SPACES                                                    
         LA    R4,CSARLEN                                                       
         LA    R3,6                                                             
                                                                                
DH60     OC    0(2,R4),0(R4)                                                    
         BZ    DH70                NO LENGTH IN THIS FIELD                      
         CH    R3,=H'6'                                                         
         BE    *+12                FIRST TIME                                   
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
                                                                                
         ZIC   RE,1(R4)            LENGTH                                       
         MH    RE,=H'10'           LEFT ONE POSITION                            
         ZIC   RF,0(R4)            CLASS                                        
         AR    RE,RF                                                            
         EDIT  (RE),(5,0(R2)),1,ALIGN=LEFT                                      
         SH    R0,=H'2'                                                         
         AR    R2,R0               LENGTH OF OUTPUT                             
         CLC   0(2,R2),=C'.0'                                                   
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),SPACES                                                   
                                                                                
DH70     LA    R4,2(R4)                                                         
         BCT   R3,DH60                                                          
         MVC   DE5LEN,MYWORK                                                    
                                                                                
* DEMO                                                                          
         L     R6,AIO3                                                          
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
                                                                                
         LA    R4,MYWORK                                                        
         XC    MYWORK(30),MYWORK                                                
         MVC   0(L'RSARDEM,R4),CSARDEM        DEMOS + ENDING ZERO               
         LA    R3,6                                                             
                                                                                
DH80     CLI   1(R4),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R4),C'I'                                                       
         LA    R4,3(R4)                                                         
         BCT   R3,DH80                                                          
                                                                                
         GOTO1 DEMOCON,DMCB,(2,MYWORK),(9,DE5DEMO),(0,DBLOCKD)                  
         DROP  R6                                                               
                                                                                
DHX      DS    0H                                                               
         B     SUBRX                                                            
         EJECT                                                                  
***********************************************************************         
* GO AND SWAP TO CONTRACT                                                       
***********************************************************************         
GOCONPGM DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R3,BLOCK                                                         
         USING GLVXFRSY,R3                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
*        OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R3                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RCAUTOD,R3                                                       
*                                                                               
* DON'T RETURN ON ERROR + CALLER IS DARE                                        
*                                                                               
         MVI   RCAUFLAG,X'80'+X'40'                                             
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'                                                   
         BZ    GOCPGM03                                                         
         DROP  R2                                                               
*                                                                               
         OI    RCAUFLAG,X'08'      SET CALL BY MASTER                           
         MVC   RCAUSREP,RDARKREP   SET SUB REP CODE                             
*                                                                               
GOCPGM03 DS    0H                                                               
         MVC   RCAUSTAT,RDARKSTA                                                
*                                                                               
         MVC   RCAUAGY(3),RDARKAGY                                              
         MVC   RCAUAGOF,RDARKAOF                                                
*                                                                               
* RETREIVE DARE AGENCY EQUIVALENCY CODE, IF FOUND                               
*                                                                               
         XC    KEY,KEY                                                          
DAGYD    USING RAGKDKEY,KEY                                                     
         MVI   DAGYD.RAGKDTYP,RAGKDTYQ                                          
*                                                                               
         MVC   DAGYD.RAGKDREP,=C'MR'                                            
         CLC   =C'AM',AGENCY                                                    
         BE    GOCPGM05                                                         
         CLC   =C'CQ',AGENCY                                                    
         BE    GOCPGM05                                                         
         CLC   =C'NK',AGENCY                                                    
         BE    GOCPGM05                                                         
*                                                                               
         MVC   DAGYD.RAGKDREP,AGENCY                                            
*                                                                               
GOCPGM05 DS    0H                                                               
         MVC   DAGYD.RAGKDDAG,RDARKAGY                                          
         MVC   DAGYD.RAGKDDAO,RDARKAOF                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(RAGKDAGY-RAGKDKEY),KEYSAVE                                   
         BNE   GOCPGM10                                                         
         MVC   RCAUAGY,DAGYD.RAGKDAGY                                           
         MVC   RCAUAGOF,DAGYD.RAGKDAOF                                          
         DROP  DAGYD                                                            
*                                                                               
* IF MORE THAN ONE ASSIGNMENT, DEFAULT TO BLANK INSTEAD                         
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(RAGKDAGY-RAGKDKEY),KEYSAVE                                   
         BNE   GOCPGM10                                                         
         XC    RCAUAGY,RCAUAGY                                                  
         XC    RCAUAGOF,RCAUAGOF                                                
*                                                                               
GOCPGM10 DS    0H                                                               
         MVC   RCAUBUYR,RDARBUYR                                                
         MVC   RCAUPRD,RDARPRN1                                                 
         CLC   =C'$EDI$',RDARAGAD  EDI SKIP PRODUCT 2                           
         BE    GOCPGM20                                                         
         CLC   RDARPRN2,SPACES     PRODUCT 2?                                   
         BE    GOCPGM20                                                         
         MVI   RCAUPRD+10,C'/'                                                  
         MVC   RCAUPRD+11(9),RDARPRN2                                           
*                                                                               
GOCPGM20 DS    0H                                                               
*        MVC   RCAUSAL,RDARBUYC                                                 
         MVC   RCAUFLT,RDARESST                                                 
*                                                                               
         CLC   RTKODATE,FLTSTART   TAKEOVER DATE CHOPPING REQUIRED??            
         BL    GOCPGM25                                                         
         GOTO1 DATCON,DMCB,(3,RTKODATE),(2,RCAUFLT)                             
*                                                                               
GOCPGM25 DS    0H                                                               
         MVC   RCAUADVN,RDARCLNM   ADVERTISER NAME                              
         MVC   RCAUDEMO,RDARTDEM   TARGET DEMO                                  
*                                                                               
         CLC   =C'$EDI$',RDARAGAD  EDI SKIP FLIGHT#/TRAFFIC ID                  
         BNE   GOCPGM30                                                         
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'ED'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GOCPGM30                                                         
         MVC   RCAUFLTN,53(R6)     SAVE OFF FLIGHT#/TRAFFIC ID                  
         DROP  R3                                                               
*                                                                               
GOCPGM30 DS    0H                                                               
         BAS   RE,VARHEAD          IF BRAND,USE VARIOUS CONTRACT HEADER         
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',ELEM,RCAUELLQ,GLRCAUTO                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         L     R1,ATIOB                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LA    R2,CONSERVH                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
*                                                                               
         LA    R2,DE1HDLNH                                                      
         XC    DE1HDLN,DE1HDLN                                                  
         MVI   5(R2),0                                                          
         XMOD1 2                   EXIT ALL THE WAY OUT                         
         EJECT                                                                  
***********************************************************************         
* IF BRAND, USE VARIOUS HEADER TO CREATE BRAND HEADER                           
***********************************************************************         
VARHEAD  NTR1                                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'08'      BRAND ONLY                                   
         BZ    VARHX                                                            
         DROP  R6                                                               
*                                                                               
* RETRIEVE VARIOUS HEADER                                                       
*                                                                               
         MVC   KEY(L'RDARKEY),SVAGYVKY                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RDARREP#                                              
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCONKEY,R4                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,WORK                                                    
         DROP  R4                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         LA    R4,ELEM                                                          
         USING RCAUTOD,R4                                                       
         MVC   RCAUAGY,RCONKAGY                                                 
         MVC   RCAUAGOF,RCONKAOF                                                
         MVC   RCAUBUYR,RCONBUYR                                                
         MVC   RCAUADV,RCONKADV                                                 
         MVC   RCAUSAL,RCONSAL                                                  
         DROP  R4,R6                                                            
*                                                                               
VARHX    DS    0H                                                               
         B     SUBRX                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK IF ANYTHING IN GLOBBER                                                  
***********************************************************************         
CKGLOB   DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',BLOCK,RCAUELRQ,GLRCAUTO                   
         TM    DMCB+8,X'10'                                                     
         BO    SUBNO                                                            
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLRCAUTO                                
*                                                                               
         LA    R1,BLOCK                                                         
         USING RCAUTOD,R1                                                       
         OC    RCAUCON#,RCAUCON#                                                
         BZ    SUBNO                                                            
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RCAUCON#    CONTRACT NUMBER                           
         DROP  R1                                                               
*                                                                               
         EDIT  (P5,WORK+17),(8,DE1HDLN),ALIGN=LEFT                              
         STC   R0,DE1HDLNH+5                                                    
         OI    DE1HDLNH+4,X'08'                                                 
*                                                                               
         B     SUBYES                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY VARIOUS ORDER INFORMATION                                             
***********************************************************************         
VARINFO  DS    0H                                                               
         MVC   DARPFLN+19(11),=C'PF12 Return'                                   
*                                                                               
         LA    R6,AGYVKEY                                                       
         USING RDARKEY,R6                                                       
         XC    DARHDR1,DARHDR1                                                  
         MVC   DARHDR1+4(6),=C'BRAND#'                                          
         MVC   DARHDR1+36(9),=C'VAR O#/C#'                                      
         GOTO1 HEXOUT,DMCB,RDARKORD,DARHDR1+46,L'RDARKORD,0                     
         MVC   VARNUM,RDARKORD                                                  
         DROP  R6                                                               
*                                                                               
         MVC   KEY(L'RDARKEY),AGYVKEY                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVI   DARHDR1+54,C'/'                                                  
         GOTO1 HEXOUT,DMCB,RDARREP#,DARHDR1+55,L'RDARREP#,0                     
         MVC   VARKNUM,RDARREP#                                                 
         DROP  R6                                                               
*                                                                               
         XR    R3,R3                                                            
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVC   KEY(L'RDARKEY),AGYVKEY                                           
         MVI   RDARKRT,X'35'       BRAND RECORDS ONLY                           
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
*                                                                               
VARINF10 DS    0H                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   VARINF30                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARPDFG,X'80'                                                   
         BZ    VARINF20                                                         
         LA    R3,1(R3)                                                         
         DROP  R6                                                               
*                                                                               
VARINF20 DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     VARINF10                                                         
*                                                                               
VARINF30 DS    0H                                                               
         LA    R6,KEYSAVE                                                       
         USING RDARKEY,R6                                                       
         MVC   DARHDR1+64(4),=C'CNF:'                                           
         EDIT  (R3),(3,DARHDR1+68),FILL=0                                       
         MVI   DARHDR1+71,C'/'                                                  
         EDIT  RDARKSEQ,(3,DARHDR1+72),FILL=0                                   
         DROP  R6                                                               
*                                                                               
VARINFX  DS    0H                                                               
         B     SUBRX                                                            
         EJECT                                                                  
***********************************************************************         
* MAINTAIN BD PASSIVE KEY                                                       
***********************************************************************         
BDKEY    DS    0H                                                               
*                                                                               
* BUILD X'BD' PASSIVE KEY                                                       
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
KEYBD    USING RCONKEY,KEY                                                      
         MVI   KEYBD.RCONDATP,X'BD'                                             
         MVC   KEYBD.RCONDARP,AGENCY                                            
         MVC   KEYBD.RCONDAAG,RCONKAGY                                          
         MVC   KEYBD.RCONDAAO,RCONKAOF                                          
         MVC   KEYBD.RCONDAAD,RCONKADV                                          
         MVC   KEYBD.RCONDAST,RCONKSTA                                          
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,KEYBD.RCONDAFS)                      
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,KEYBD.RCONDAFE)                    
         MVC   KEYBD.RCONDACN,RCONKCON                                          
         DROP  KEYBD,R6                                                         
*                                  RESTORE KEY OR ADD NEW                       
         CLI   DRVLINK,C'U'                                                     
         BE    BDKEY20                                                          
*                                  RESTORE KEY OR ADD NEW                       
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   BDKEY10                                                          
         NI    KEY+27,X'FF'-X'80'                                               
         MVC   KEY+28(4),CCONDKAD                                               
         GOTO1 WRITE                                                            
         B     BDKEYX                                                           
*                                                                               
BDKEY10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),KEYSAVE                                                  
         MVC   KEY+28(4),CCONDKAD                                               
         GOTO1 ADD                                                              
         B     BDKEYX                                                           
*                                                                               
* DELETE X'BD' PASSIVE KEY                                                      
*                                  RESTORE KEY OR ADD NEW                       
BDKEY20  DS    0H                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   BDKEYX                                                           
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
BDKEYX   DS    0H                                                               
         B     SUBRX                                                            
         EJECT                                                                  
***********************************************************************         
* CONFIRM CONTRACT                                                              
***********************************************************************         
CFRMCON  DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
*        OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
CFPK010  DS    0H                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SEND THE CONTRACT NUMBER AND CONFIRM THE CONTRACT                             
*                                                                               
CFPK040  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLCONNUM,R6                                                      
         GOTO1 (RFCONNUM,REPFACS),DMCB,(1,CCONKNUM),(5,GLCONNUM)                
         MVC   GLCONCA(2),=C'CF'                                                
         OI    GLCONFLG,GLCONRET+GLCONRPQ                                       
*                                                                               
         MVC   GLCONRPY(2),=C'CF'                                               
*                                                                               
         TM    SVDARFLG,X'10'      STACF?                                       
         BZ    CFPK050             NO                                           
         TM    MISCFLG4,MF4CONCF   CONTRACT IS CONFIRMED NOW?                   
         BZ    CFPK050                                                          
         OI    GLCONFL2,X'40'      YES                                          
*                                                                               
CFPK050  EQU   *                                                                
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BZ    CFPK080             NO                                           
*                                                                               
K        USING RDARKEY,SELECTKY    PASS THE SUBREP CODE FOR MASTER CF           
         OI    GLCONFL2,X'80'                                                   
         MVC   GLCONSRP,K.RDARKREP                                              
         DROP  K                                                                
         DROP  R2                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
CFPK080  DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,GLCONLNQ,GLRKACT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
CFPK100  DS    0H                                                               
         L     R1,ATIOB                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LA    R2,CONSERVH `                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
*                                                                               
CFYES    SR    RC,RC                                                            
CFNO     LTR   RC,RC                                                            
CFCX     B     SUBRX                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SWAP TO CONTRACT AND APPLY MAKEGOOD                                           
***********************************************************************         
SWAPCON  DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
*        OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
SWPK010  DS    0H                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SEND THE CONTRACT NUMBER AND DISPLAY CONTRACT                                 
*                                                                               
*&&DO                                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARKTYP,X'51'      USING CONFIRMED CONTRACTS?                   
         BE    SWPK040             YES - CALL CONRACT MGO SCREEN                
         DROP  R6                                                               
*&&                                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLCONNUM,R6                                                      
         GOTO1 (RFCONNUM,REPFACS),DMCB,(1,CCONKNUM),(5,GLCONNUM)                
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DSM'                                               
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'                                                   
         BZ    SWPK030             MASTER REP SIGNED ON?                        
         DROP  R2                                                               
*                                                                               
         OI    GLCONRPY+2,X'80'    CALL BY MASTER FLAG                          
K        USING RDARKEY,SELECTKY    PASS THE SUBREP CODE FOR MASTER CF           
         MVC   GLCONRPY+3(2),K.RDARKREP                                         
         DROP  K                                                                
*                                                                               
SWPK030  DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,GLCONLNQ,GLRKACT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&DO                                                                           
SWPK040  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLCONNUM,R6                                                      
         GOTO1 (RFCONNUM,REPFACS),DMCB,(1,CCONKNUM),(5,GLCONNUM)                
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'MGL'                                               
         DROP  R6                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,GLCONLNQ,GLRKACT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         DROP  R4                                                               
*                                                                               
SWPK100  DS    0H                                                               
         L     R1,ATIOB                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LA    R2,CONSERVH                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
*                                                                               
         B     SUBRX                                                            
         EJECT                                                                  
*                                                                               
BUYTITLE DC    C'MC LIN DAYS         TIMES       LEN EFF. DATES'                
         DC    C'    NW         NPW       RATE SPT'                             
BUYTITLQ EQU   *-BUYTITLE                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS DS    0H                                                               
         XC    BITFLAG,BITFLAG                                                  
         XC    BITFLAG2,BITFLAG2                                                
         XC    BITFLAG3,BITFLAG3                                                
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
* SET TO SAVE DISK ADDRESS OF SELECTED RECORD                                   
* SINCE MAKEGOOD/LIST, BUY/LIST AND CONT/LIST ETC. TURNS THIS ON                
         NI    CTLRFLG1,X'FF'-CF1SVDAQ                                          
                                                                                
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
                                                                                
STPFKL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   STPFKV00                                                         
                                                                                
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPFKL10                                                         
                                                                                
         CLI   PFKEY,2             CURSOR CAN BE ANYWHERE FOR PF2               
         BE    STPFKL10                                                         
*                                                                               
* PREVENT ACCIDENTAL CALL TO THE WRONG APPROVAL/REJECT OVERLAY                  
*                                                                               
         CLI   PFKEY,10            CANNOT PF10                                  
         BE    INVPFKEY                                                         
*                                                                               
         CLI   PFKEY,11            CANNOT PF11                                  
         BE    INVPFKEY                                                         
*                                                                               
* CURSOR SHOULD BE WITHIN LIST                                                  
*                                                                               
STPFKL03 DS    0H                  FOR LONG LIST SCREEN                         
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
                                                                                
         CLI   MYSCRNUM,X'E5'      FOR SHORT LIST SCREEN                        
         BE    STPFKL05                                                         
         LA    R0,DARSELH          CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFD2                                                         
         LA    R0,DARLLINH                                                      
         CR    R2,R0                                                            
         BH    RECNTFD2                                                         
         B     STPFKL10                                                         
*                                                                               
STPFKL05 DS    0H                  FOR LONG LIST SCREEN                         
         LA    R0,DE5SELH          CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFD2                                                         
         LA    R0,DE5LLINH                                                      
         CR    R2,R0                                                            
         BH    RECNTFD2                                                         
*                                                                               
STPFKL10 LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
*                                                                               
* CHECK IF REVISION, REVISION REJECT GOES TO OVERLAY 21                         
* REVISION DIFFERENCE GOES TO OVERLAY 22                                        
*                                                                               
         L     R1,AFRSTREC                                                      
STPFKL13 DS    0H                                                               
         CLI   5(R1),1             THIS ACTUALLY STARTS AT 2ND HALF OF          
         BNE   STPFKL17            KEY SCREEN INSTEAD OF LIST SCREEN            
         CLI   8(R1),C'A'                                                       
         BE    STPFKL18                                                         
         CLI   8(R1),C'R'                                                       
         BE    STPFKL18                                                         
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   STPFKL15                                                         
         CLI   8(R1),C'+'          '+' ONLY FOR DDS                             
         BE    STPFKL16                                                         
*                                                                               
STPFKL15 DS    0H                                                               
         CLI   8(R1),C'D'          'D'IFFERENCE ONLY FOR REVISION               
         BNE   STPFKL17                                                         
STPFKL16 ZIC   RF,0(R1)                                                         
         LR    RE,R1                                                            
         AR    RE,RF                                                            
         CLC   =C'REV',10(RE)                                                   
         BE    STPFKL18                                                         
         MVI   8(R1),C'S'                                                       
*                                                                               
STPFKL17 ZIC   RF,0(R1)                                                         
         AR    R1,RF                                                            
         CLI   0(R1),0             STOP AT END OF SCREEN                        
         BE    STPFKL20                                                         
         TM    1(R1),X'20'         IGNORE PROTECTED FIELDS                      
         BO    STPFKL17                                                         
         B     STPFKL13                                                         
*                                                                               
STPFKL18 DS    0H                                                               
         ZIC   RF,0(R1)                                                         
         AR    R1,RF                                                            
*                                  REVISION CHECK                               
         NI    CTLRFLG1,X'FF'-CF1ISREV                                          
         CLC   =C'REV',10(R1)                                                   
         BNE   STPFKL20                                                         
         OI    CTLRFLG1,CF1ISREV   FLAG ORDER IS REVISION                       
*                                                                               
STPFKL20 DS    0H                                                               
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFINIT                                                         
         LA    R2,BLPFTAB                                                       
         B     STPFINIT                                                         
                                                                                
***************                                                                 
* FOR ACTION SELECT                                                             
***************                                                                 
STPFKV00 CLI   ACTNUM,MYACTSEL     ACTION SELECT?                               
         BNE   STPFINIT                                                         
                                                                                
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERR2                                                          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED, JUST IN CASE              
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   PFKEY,2             ONLY IF LINKED TO CONTRACT                   
         BNE   STPFKV03                                                         
         OC    RDARREP#,RDARREP#                                                
         BZ    ORDNTLK2                                                         
         OI    BITFLAG2,B2GOCON    SET SWAP TO CONTRACT                         
         B     STPFX                                                            
*                                                                               
STPFKV03 DS    0H                                                               
         CLI   PFKEY,10            Confirm?                                     
         BNE   STPFKV05                                                         
         CLI   RDARKTYP,X'51'                                                   
         BE    INVPFKEY                                                         
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   STPFKV05                                                         
         LR    R4,R6                                                            
         L     R6,AIO                                                           
*                                                                               
         USING RDARFLEM,R4                                                      
         TM    RDARFLG1,X'20'      PENDCF?                                      
         BO    INVPFKEY                                                         
         TM    RDARFLG1,X'10'      STACF                                        
         BO    STPFKV04            YES, ALLOW CONFIRM                           
*                                                                               
         TM    RDARFLG1,X'40'      -S?                                          
         BO    STPFK04                                                          
         CLI   RDARBSTS,C'M'                                                    
         BNE   STPFKV05            AMND ONLY                                    
         OI    CTLRFLG1,CF1PDING   THIS WILL FORCE THE DIFF SCR                 
         B     STPFKV15                                                         
*                                                                               
STPFK04  DS    0H                  CHECK TO SEE IF -S                           
         TM    RDARFLG1,X'C0'      MATCH-S?                                     
         BO    STPFKV04                                                         
         CLI   RDARBSTS,C'A'       If Open-S                                    
         BE    STPFKV04                                                         
         CLI   RDARBSTS,C'M'       If Amend-S                                   
         BE    STPFKV04                                                         
         B     STPFKV05                                                         
*                                                                               
STPFKV04 DS    0H                                                               
         OI    BITFLAG3,B3CFCON    CONFIRM CONTRACT                             
         B     STPFX                                                            
STPFK04A DS    0H                                                               
         DROP  R4                                                               
         B     STPFX                                                            
*                                                                               
STPFKV05 DS    0H                                                               
         CLI   PFKEY,4             CONTRACT LIST                                
         BNE   STPFKV07                                                         
         OC    RDARRNUM,RDARRNUM   IF REVISION AND IS MISSING CONTRACT          
         BZ    STPFKV07            ORDER MUST BE A TAKEOVER                     
         OC    RDARREP#,RDARREP#   WARN USER TO ENTER TAKEOVER CONTRACT         
         BZ    ERTKOVR2            MUST GENERATE TAKEOVER CONTRACT              
         B     INVPFKEY                                                         
*                                                                               
STPFKV07 DS    0H                                                               
         CLI   PFKEY,6             ONLY VARIOUS CAN DO BRAND/LIST               
         BNE   STPFKV10                                                         
         TM    RDARMISC,X'10'                                                   
         BZ    INVPFKEY            INVALID IF NOT VARIOUS                       
*                                                                               
STPFKV10 DS    0H                                                               
         CLI   PFKEY,12            ONLY VARIOUS CAN DO BRAND/LIST               
         BNE   *+8                                                              
         OI    CTLRFLG1,CF11STKY   USE FIRST KEY ON NEXT LIST PASS              
*                                                                               
         LA    R2,VPFTABLE         YES, USE SELECT PFKEY TABLE                  
*                                  REVISION CHECK                               
         NI    CTLRFLG1,X'FF'-CF1PDING                                          
         CLI   RDARMEDI,C'R'                                                    
         BNE   STPFKV15                                                         
         OI    MISCFLG4,MF4RADIO    RADIO EDI                                   
         GOTO1 =A(CHKPDING),RR=RELO                                             
         BE    STPFKV12                                                         
         OI    CTLRFLG1,CF1PDING   RADIO EDI PENDING CONTRACT                   
*                                  OVERRIDES SCREEN TO DIFFERENCE               
         NI    MISCFLG4,X'FF'-MF4PDING                                          
         B     STPFKV15                                                         
STPFKV12 DS    0H                                                               
         OI    MISCFLG4,MF4PDING                                                
*                                                                               
STPFKV15 DS    0H                                                               
         OC    RDARRNUM,RDARRNUM   CHANGE TABLE DYNAMICALLY FOR                 
         BZ    STPFKV20            REVISED ORDERS                               
         OI    CTLRFLG1,CF1ISREV   FLAG ORDER IS REVISION                       
         DROP  R6                                                               
*                                                                               
STPFKV20 DS    0H                                                               
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFINIT                                                         
         LA    R2,BPFTABLE                                                      
*                                                                               
STPFINIT DS    0H                                                               
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
         EJECT                                                                  
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFLL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   STPFLL50                                                         
                                                                                
         NI    BITFLAG,X'FF'-BFUNLINK-BFLINK                                    
         NI    CTLRFLG1,X'FF'-CF1BRDUL-CF1BRDLK                                 
                                                                                
         CLI   PFKEY,0             PFKEYING SOMEWHERE?                          
         BE    STPFX                                                            
                                                                                
         CLI   PFKEY,2             PFKEYING TO MAKEGOOD?                        
         BE    STPFLL40                                                         
                                                                                
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERR2                                                          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   RDARKRT,X'35'       PRODUCT FOR VARIOUS RECORD                   
         BNE   STPFLL02            NO PFKEY SUPPORT EXCEPT FOR PF12             
         CLI   PFKEY,12                                                         
         BE    STPFLL38                                                         
         MVI   PFKEY,0                                                          
         B     STPFX                                                            
*                                                                               
STPFLL02 DS    0H                                                               
         CLI   PFKEY,7             LINK?                                        
         BNE   STPFLL03                                                         
         OI    BITFLAG,BFLINK                                                   
         OI    CTLRFLG1,CF1BRDLK   SET BRAND LINK                               
         MVI   PFKEY,9                                                          
         B     STPFLL40                                                         
                                                                                
STPFLL03 DS    0H                                                               
         CLI   PFKEY,12            INCASE OF RETURN FROM BRAND/LIST             
         BE    STPFLL38                                                         
                                                                                
         B     STPFLL40            NO OTHER PFKEY SUPPORTED IN LIST             
*&&DO                                                                           
STPFLL05 DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERR2                                                          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   RDARKRT,X'35'       PRODUCT FOR VARIOUS RECORD                   
         BNE   STPFLL07            NO PFKEY SUPPORT EXCEPT FOR PF12             
         CLI   PFKEY,12                                                         
         BE    STPFLL38                                                         
         MVI   PFKEY,0                                                          
         B     STPFX                                                            
*                                                                               
STPFLL07 DS    0H                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
*                                                                               
         CLI   PFKEY,7             IF LINK                                      
         BNE   STPFLL10                                                         
*                                                                               
         TM    RDARMISC,X'04'                                                   
         BO    INVPFKEY            INVALID IF CONFIRMED VARIOUS                 
         TM    RDARMISC,X'20'                                                   
         BO    NDAREERR            INVALID IF NOTDARE                           
         CLI   RDARBSTS,C'C'                                                    
         BE    INVPFKEY            INVALID IF RECALLED                          
         OC    RDARREP#,RDARREP#   CANNOT LINK A LINKED ORDER                   
         BNZ   ERLINKD2                                                         
         CLI   RDARBSTS,C'R'       CANNOT LINK AN UNLINKED REJECTED             
         BE    ERLINKRJ              ORDER                                      
         OI    BITFLAG,BFLINK                                                   
         OI    CTLRFLG1,CF1BRDLK   SET BRAND LINK                               
         B     STPFLL20                                                         
                                                                                
STPFLL10 DS    0H                                                               
         CLI   PFKEY,8             IF UNLINK                                    
         BNE   STPFLL30                                                         
         TM    RDARMISC,X'04'                                                   
         BO    INVPFKEY            INVALID IF CONFIRMED VARIOUS                 
         TM    RDARMISC,X'20'                                                   
         BO    NDAREERR            INVALID IF NOTDARE                           
         CLI   RDARBSTS,C'C'                                                    
         BE    NDAREERR            INVALID IF RECALLED                          
         OC    RDARREP#,RDARREP#   CANNOT UNLINK AN UNLINKED ORDER              
         BZ    ERUNLKD2                                                         
         TM    RDARMISC,X'40'      CANNOT UNLINK AN APPROVED ORDER              
         BO    ERAPLNK2                                                         
         OI    BITFLAG,BFUNLINK                                                 
                                                                                
STPFLL20 DS    0H                                                               
         MVI   PFKEY,9             FORCE SELECT                                 
         B     STPFLL40                                                         
                                                                                
STPFLL30 DS    0H                                                               
         CLI   PFKEY,10            APPROVE?                                     
         BNE   STPFLL35                                                         
***                                                                             
*** FOR CONVERTED CONTRACTS, IF X'1D' IS MISSING, THEN PREVENT USER             
*** FROM APPROVING THIS ORDER. REASON IS BUYS CAN BE OVERWRITTEN WITH           
*** CHANGES THAT BOTH REP AND STATION DO NOT KNOW ABOUT                         
***                                                                             
         TM    RDARMISC,X'04'                                                   
         BO    INVPFKEY            INVALID IF CONFIRMED VARIOUS                 
         TM    CCONSTAT+1,X'10'    CONVERTED CONTRACT?                          
         BZ    STPFLL33            YES,                                         
         OC    CDARNUM,CDARNUM     NULLS MEAN NO X'1D' ELEMENT WAS              
         BZ    BADDRLNK            FOUND                                        
***                                                                             
STPFLL33 DS    0H                                                               
         TM    RDARMISC,X'20'                                                   
         BO    NDAREERR            INVALID IF NOTDARE                           
         CLI   RDARBSTS,C'C'                                                    
         BE    INVPFKEY            INVALID IF RECALLED                          
         OC    RDARREP#,RDARREP#   CANNOT APPROVE AN UNLINK ORDER               
         BZ    ORDNTLK2                                                         
         TM    RDARMISC,X'80'      PERMITTED IF RESENT                          
         BO    STPFLL40                                                         
         CLI   RDARBSTS,C'A'       CANNOT APPROVE AN APPROVED ORDER             
         BNE   STPFLL34                                                         
         CLI   RDARRNUM,0          UNLESS IT'S A REVISION, IT'LL JUST           
         BNE   STPFLL40            BRING UP THE DIFFERENCES SCREEN              
         B     ERAPPROV                                                         
*                                                                               
STPFLL34 DS    0H                                                               
         CLI   RDARBSTS,C'R'       CANNOT APPROVE A REJECTED ORDER              
         BE    ERREJECT                                                         
         B     STPFLL40                                                         
                                                                                
STPFLL35 DS    0H                                                               
         CLI   PFKEY,11            REJECT?                                      
         BNE   STPFLL38                                                         
*                                                                               
         TM    RDARMISC,X'04'                                                   
         BO    INVPFKEY            INVALID IF CONFIRMED VARIOUS                 
         CLC   =C'$EDI$',RDARAGAD  REJECT NOT ALLOWED FOR EDI ORDERS            
         BE    INVREJAC                                                         
         TM    RDARMISC,X'08'      INVALID IF BRAND                             
         BO    INVPFKEY                                                         
         TM    RDARMISC,X'20'      PERMITTED IF NOTDARE                         
         BO    STPFLL40                                                         
         CLI   RDARBSTS,C'C'       INVALID IF RECALLED                          
         BE    INVPFKEY                                                         
         TM    RDARMISC,X'80'      PERMITTED IF RESENT                          
         BO    STPFLL40                                                         
         CLI   RDARBSTS,C'R'       CANNOT REJECT A REJECTED ORDER               
         BE    ERREJECT                                                         
         B     STPFLL40                                                         
         DROP  R6                                                               
*&&                                                                             
*                                                                               
STPFLL38 DS    0H                                                               
         CLI   PFKEY,12            RETURN FOR BRAND/LIST ONLY                   
         BNE   STPFLL40                                                         
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFLL40                                                         
*        XC    DE1VARI,DE1VARI                                                  
         OC    SVSELKEY,SVSELKEY                                                
         BZ    BADERR2                                                          
         MVC   SELECTKY,SVSELKEY                                                
         NI    CTLRFLG1,X'FF'-CF1BRDQ                                           
         LA    R2,BLPFTAB                                                       
         B     STPFLL80            GO INIT AGAIN                                
*                                                                               
STPFLL40 DS    0H                                                               
         LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFLL80                                                         
         LA    R2,BLPFTAB                                                       
         B     STPFLL80            GO INIT AGAIN                                
         EJECT                                                                  
***************                                                                 
* FOR MY ACTION SELECT                                                          
***************                                                                 
STPFLL50 CLI   ACTNUM,MYACTSEL     ACTION SELECT?                               
         BNE   STPFX                                                            
                                                                                
         NI    BITFLAG,X'FF'-BFPRINT                                            
                                                                                
         CLI   PFKEY,5             PRINT A DRAFT ORDER?                         
         BNE   STPFLL55                                                         
         OI    BITFLAG,BFPRINT                                                  
         B     STPFX                                                            
                                                                                
STPFLL55 DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERR2                                                          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
                                                                                
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
                                                                                
         CLI   PFKEY,10            APPROVE?                                     
         BNE   STPFLL60                                                         
*                                                                               
***                                                                             
*** FOR CONVERTED CONTRACTS, IF X'1D' IS MISSING, THEN PREVENT USER             
*** FROM APPROVING THIS ORDER. REASON IS BUYS CAN BE OVERWRITTEN WITH           
*** CHANGES THAT BOTH REP AND STATION DO NOT KNOW ABOUT                         
***                                                                             
         TM    RDARMISC,X'04'                                                   
         BO    INVPFKEY            INVALID IF CONFIRMED VARIOUS                 
         TM    CCONSTAT+1,X'10'    CONVERTED CONTRACT?                          
         BZ    STPFLL58            YES,                                         
         OC    CDARNUM,CDARNUM     NULLS MEAN NO X'1D' ELEMENT WAS              
         BZ    BADDRLNK            FOUND                                        
***                                                                             
STPFLL58 DS    0H                                                               
         TM    RDARMISC,X'20'                                                   
         BO    NDAREERR            INVALID IF NOTDARE                           
         CLI   RDARBSTS,C'C'                                                    
         BE    INVPFKEY            INVALID IF RECALLED                          
         OC    RDARREP#,RDARREP#   CANNOT APPROVE AN UNLINK ORDER               
         BZ    INVPFKEY                                                         
         TM    RDARMISC,X'80'      PERMITTED IF RESENT                          
         BO    STPFLL70                                                         
         CLI   RDARBSTS,C'A'       CANNOT APPROVE AN APPROVED ORDER             
         BE    INVPFKEY                                                         
         CLI   RDARBSTS,C'R'       CANNOT APPROVE A REJECTED ORDER              
         BE    INVPFKEY                                                         
         B     STPFLL70                                                         
                                                                                
STPFLL60 DS    0H                                                               
         CLI   PFKEY,11            REJECT                                       
         BNE   STPFLL65                                                         
*                                                                               
         TM    RDARMISC,X'04'                                                   
         BO    INVPFKEY            INVALID IF CONFIRMED VARIOUS                 
         CLI   RDARKTYP,X'51'      INVALID IF CONFIRM                           
         BE    INVPFKEY                                                         
         CLC   =C'$EDI$',RDARAGAD  REJECT NOT ALLOWED FOR EDI ORDERS            
         BE    INVREJAC                                                         
         TM    RDARMISC,X'08'      INVALID IF BRAND                             
         BO    INVPFKEY                                                         
         TM    RDARMISC,X'20'      PERMITTED IF NOTDARE                         
         BO    STPFLL70                                                         
         CLI   RDARBSTS,C'C'       INVALID IF RECALLED                          
         BE    INVPFKEY                                                         
         TM    RDARMISC,X'80'      PERMITTED IF RESENT                          
         BO    STPFLL70                                                         
         CLI   RDARBSTS,C'R'       CANNOT REJECT A REJECTED ORDER               
         BE    INVPFKEY                                                         
         B     STPFLL70                                                         
         DROP  R6                                                               
                                                                                
STPFLL65 DS    0H                                                               
         CLI   PFKEY,12            RETURN TO BRAND/LIST                         
         BNE   STPFX                                                            
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFX                                                            
         MVC   AGYVKEY,SVAGYVKY    RESTORE AGENCY HEADER REC                    
                                                                                
STPFLL70 DS    0H                                                               
         LA    R2,VPFTABLE         YES, USE SELECT PFKEY TABLE                  
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFLL80                                                         
         LA    R2,BPFTABLE                                                      
                                                                                
STPFLL80 DS    0H                                                               
         ZIC   R0,PFKEY                                                         
         AH    R0,=H'12'                                                        
         STC   R0,PFKEY                                                         
                                                                                
* IF ACTION IS TAKEN ON AGENCY ORDER FOR THE FIRST TIME                         
* RECORD ACTIVITY DATE/TIME                                                     
*        GOTO1 VTOUCHED                                                         
                                                                                
         OI    CTLRFLG1,CF11STKY   USE FIRST KEY ON NEXT LIST PASS              
         OI    CTLRFLG1,CF1TSELQ   DON'T TEST THE SEL CODES IN TESTSEL          
         GOTO1 INITIAL,DMCB,(R2)   2ND PASS                                     
STPFX    B     SUBRX                                                            
         EJECT                                                                  
*                                                                               
RECNTFD2 MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND2                                                          
*                                                                               
INVPFKEY MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         B     ERREND2                                                          
*                                                                               
ERLINKD2 MVC   RERROR,=AL2(421)    ORDER ALREADY LINKED                         
         B     ERREND2                                                          
*                                                                               
ERNOHDLN MVC   RERROR,=AL2(422)    MUST SPECIFY A CONTRACT TO LINK              
         B     ERREND2                                                          
*                                                                               
ERUWCF   MVC   RERROR,=AL2(933)    ONLY POINT PERSON CONFIRM UNWIRE             
         B     ERREND2                                                          
*                                                                               
ERCFCON  MVC   RERROR,=AL2(934)    ERROR CONFIRMING A CONTRACT                  
         B     ERREND2                                                          
*                                                                               
MSGCFOK  MVC   RERROR,=AL2(935)    CONFIRM OK                                   
         B     INFEND2                                                          
*                                                                               
ORDNTLK2 MVC   RERROR,=AL2(423)    ORDER IS NOT LINKED TO ANY CONTRACT          
         B     ERREND2                                                          
*                                                                               
ERAPPROV MVC   RERROR,=AL2(424)    ORDER IS ALREADY APPROVED                    
         B     ERREND2                                                          
*                                                                               
ERREJECT MVC   RERROR,=AL2(425)    ORDER IS ALREADY REJECTED                    
         B     ERREND2                                                          
*                                                                               
ERUNLKD2 MVC   RERROR,=AL2(427)    ORDER ALREADY UNLINKED                       
         B     ERREND2                                                          
*                                                                               
ERAPLNK2 MVC   RERROR,=AL2(436)    CANNOT UNLINK AN APPROVED ORDER              
         B     ERREND2                                                          
*                                                                               
ERLINKRJ MVC   RERROR,=AL2(437)    CANNOT LINK AN UNLINKED REJECTED ORD         
         B     ERREND2                                                          
*                                                                               
BADERR2  MVC   RERROR,=AL2(440)    UNEXPECTED ERROR ENCOUNTERED,                
         LA    R2,DE1HDLNH          CALL DDS                                    
         B     ERREND2                                                          
*                                                                               
NDAREERR MVC   RERROR,=AL2(450)    ACTION REJECT ONLY FOR NOTDARE ORD           
         B     ERREND2                                                          
*                                                                               
INVREJAC MVC   RERROR,=AL2(567)    ACTION REJECT NOT AVAILABLE FOR THIS         
         B     ERREND2             ORDER                                        
*                                                                               
ERTKOVR2 MVC   RERROR,=AL2(755)    MUST GENERATE TAKEOVER CONTRACT              
         B     ERREND2                                                          
*                                                                               
BADDRLNK MVC   RERROR,=AL2(595)    MISSING DARE LINK                            
         LA    R2,DE1HDLNH          CALL DDS                                    
         B     ERREND2                                                          
*                                                                               
ERREND2  DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
*                                                                               
INFEND2  DS    0H                                                               
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
LPFTABLE DS    0C                                                               
                                                                                
* MGGROUP                                                                       
         DC    AL1(LPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF02X   EQU   *                                                                
                                                                                
* LINK                                                                          
         DC    AL1(LPF07X-*,7,0,0,0,PFTRETRN)                                   
         DC    CL3'L',CL8' ',CL8' '                                             
LPF07X   EQU   *                                                                
                                                                                
* UNLINK                                                                        
         DC    AL1(LPF08X-*,0,0,0,0,PFTRETRN)                                   
         DC    CL3'U',CL8' ',CL8' '                                             
LPF08X   EQU   *                                                                
                                                                                
* SELECT/DISPLAY                                                                
         DC    AL1(LPF09X-*,9,0,0,0,PFTRETRN)                                   
         DC    CL3'S',CL8' ',CL8' '                                             
LPF09X   EQU   *                                                                
                                                                                
* APPROVE                                                                       
         DC    AL1(LPF10X-*,10,0,0,0,PFTRETRN)                                  
         DC    CL3'A',CL8' ',CL8' '                                             
LPF10X   EQU   *                                                                
*                                                                               
* DIFFERENCE (SPECIAL CHARACTER THAT USERS DON'T KNOW ABOUT)                    
         DC    AL1(LPF10DX-*,10,0,0,0,PFTRETRN)                                 
         DC    CL3'+',CL8' ',CL8' '                                             
LPF10DX  EQU   *                                                                
                                                                                
* REJECT                                                                        
         DC    AL1(LPF11X-*,11,0,0,0,PFTRETRN)                                  
         DC    CL3'R',CL8' ',CL8' '                                             
LPF11X   EQU   *                                                                
                                                                                
* RETURN TO CALLER                                                              
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
                                                                                
* ACTUAL MGGROUP/LIST                                                           
         DC    AL1(LPF14X-*,14,0,0,0,0)                                         
         DC    CL3' ',CL8'MGGROUP',CL8'LIST'                                    
LPF14X   EQU   *                                                                
                                                                                
* ACTUAL SELECT/DISPLAY                                                         
         DC    AL1(LPF21X-*,21,PFTCPROG,0,(LPF21X-LPF21)/KEYLNQ,0)              
         DC    CL3' ',CL8'ORDER',CL8'SELECT '                                   
LPF21    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                   
LPF21X   EQU   *                                                                
                                                                                
* ACTUAL APPROVE                                                                
         DC    AL1(LPF22X-*,22,PFTCPROG,0,0,0)                                  
LPF22RAQ EQU   *                                                                
         DC    CL3' ',CL8'ORDER',CL8'OPEN'                                      
*        DC    CL3' ',CL8'REVISION',CL8'DIFFERE'                                
LPF22X   EQU   *                                                                
                                                                                
* ACTUAL REJECT                                                                 
         DC    AL1(LPF23X-*,23,PFTCPROG,0,0,0)                                  
LPF22RRQ EQU   *                                                                
         DC    CL3' ',CL8'ORDER',CL8'REJECT'                                    
*        DC    CL3' ',CL8'REVISION',CL8'REJECT'                                 
LPF23X   EQU   *                                                                
                                                                                
         DC    X'FF'                                                            
***********************************************************************         
* SELECT PFKEY TABLE DEFINITIONS                                                
***********************************************************************         
VPFTABLE  DS    0C                                                              
*                                                                               
* BUYLIST                                                                       
         DC    AL1(LPF03X-*,03,0,0,(LPF03X-LPF03)/KEYLNQ,0)                     
         DC    CL3' ',CL8'BUY',CL8'LIST'                                        
LPF03    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                   
LPF03X   EQU   *                                                                
*                                                                               
* CONTRACT                                                                      
         DC    AL1(LPF04X-*,04,0,0,(LPF04X-LPF04)/KEYLNQ,0)                     
         DC    CL3' ',CL8'CONTRACT',CL8'LIST'                                   
LPF04    DC    AL1(KEYTYTWA,L'DRVASTA-1),AL2(DRVASTA-T80FFFD)                   
         DC    AL1(KEYTYTWA,L'DE1OFF-1),AL2(DE1OFF-T80FFFD)                     
         DC    AL1(KEYTYTWA,L'DRVAAGY-1),AL2(DRVAAGY-T80FFFD)                   
         DC    AL1(KEYTYTWA,L'DRVADTE-1),AL2(DRVADTE-T80FFFD)                   
LPF04X   EQU   *                                                                
*                                                                               
* PRINT                                                                         
         DC    AL1(LPF05X-*,05,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF05X   EQU   *                                                                
*                                                                               
* BRAND LIST                                                                    
*        DC    AL1(LPF06X-*,06,0,0,(LPF06X-LPF06)/KEYLNQ,0)                     
*        DC    CL3' ',CL8'BRAND',CL8'LIST'                                      
*LPF06    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                  
         DC    AL1(LPF06X-*,06,0,0,0,0)                                         
         DC    CL3' ',CL8'BRAND',CL8'LIST'                                      
LPF06X   EQU   *                                                                
*                                                                               
* HISTORY LIST                                                                  
         DC    AL1(SPF07X-*,07,0,0,(SPF07X-SPF07)/KEYLNQ,0)                     
         DC    CL3' ',CL8'HISTORY',CL8'LIST'                                    
SPF07    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                   
SPF07X   EQU   *                                                                
*                                                                               
* APPROVE                                                                       
         DC    AL1(SPF10X-*,10,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF10X   EQU   *                                                                
                                                                                
* REJECT                                                                        
         DC    AL1(SPF11X-*,11,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF11X   EQU   *                                                                
                                                                                
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
* ACTUAL APPROVE                                                                
         DC    AL1(SPF22X-*,22,0,0,0,0)                                         
SPF22RAQ EQU   *                                                                
         DC    CL3' ',CL8'ORDER',CL8'OPEN'                                      
*        DC    CL3' ',CL8'REVISION',CL8'DIFFERE'                                
SPF22X   EQU   *                                                                
                                                                                
* ACTUAL REJECT                                                                 
         DC    AL1(SPF23X-*,23,0,0,0,0)                                         
SPF22RRQ EQU   *                                                                
         DC    CL3' ',CL8'ORDER',CL8'REJECT'                                    
*        DC    CL3' ',CL8'REVISION',CL8'REJECT'                                 
SPF23X   EQU   *                                                                
                                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* DUPLICATE TABLE FOR BRAND ORDER                                               
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
BLPFTAB  DS    0C                                                               
                                                                                
* MGGROUP                                                                       
         DC    AL1(BLPF02X-*,02,0,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8' ',CL8' '                                             
BLPF02X  EQU   *                                                                
                                                                                
* LINK                                                                          
         DC    AL1(BLPF07X-*,07,0,0,0,PFTRETRN)                                 
         DC    CL3'L',CL8' ',CL8' '                                             
BLPF07X  EQU   *                                                                
                                                                                
* UNLINK                                                                        
         DC    AL1(BLPF08X-*,08,0,0,0,PFTRETRN)                                 
         DC    CL3'U',CL8' ',CL8' '                                             
BLPF08X  EQU   *                                                                
                                                                                
* SELECT/DISPLAY                                                                
         DC    AL1(BLPF09X-*,09,0,0,0,PFTRETRN)                                 
         DC    CL3'S',CL8' ',CL8' '                                             
BLPF09X  EQU   *                                                                
                                                                                
* APPROVE                                                                       
         DC    AL1(BLPF10X-*,10,0,0,0,PFTRETRN)                                 
         DC    CL3'A',CL8' ',CL8' '                                             
BLPF10X  EQU   *                                                                
                                                                                
* REJECT                                                                        
         DC    AL1(BLPF11X-*,11,0,0,0,PFTRETRN)                                 
         DC    CL3'R',CL8' ',CL8' '                                             
BLPF11X  EQU   *                                                                
                                                                                
* RETURN TO CALLER                                                              
         DC    AL1(BLPF12X-*,12,0,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8' ',CL8' '                                             
BLPF12X  EQU   *                                                                
                                                                                
* ACTUAL MGGROUP/LIST                                                           
         DC    AL1(BLPF14X-*,14,0,0,0,0)                                        
         DC    CL3' ',CL8'MGGROUP',CL8'LIST'                                    
BLPF14X  EQU   *                                                                
                                                                                
* ACTUAL SELECT/DISPLAY                                                         
*        DC    AL1(BLPF21X-*,21,PFTCPROG,0,(BLPF21X-BLPF21)/KEYLNQ,0)           
         DC    AL1(BLPF21X-*,21,0,0,(BLPF21X-BLPF21)/KEYLNQ,0)                  
         DC    CL3' ',CL8'BRAND',CL8'SELECT '                                   
BLPF21   DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                   
BLPF21X  EQU   *                                                                
                                                                                
* ACTUAL APPROVE                                                                
         DC    AL1(BLPF22X-*,22,PFTCPROG,0,0,0)                                 
         DC    CL3' ',CL8'BRAND',CL8'OPEN'                                      
BLPF22X  EQU   *                                                                
                                                                                
* ACTUAL REJECT                                                                 
         DC    AL1(BLPF23X-*,23,PFTCPROG,0,0,0)                                 
         DC    CL3' ',CL8'BRAND',CL8'REJECT'                                    
BLPF23X  EQU   *                                                                
                                                                                
* ACTUAL RETURN TO CALLER                                                       
         DC    AL1(BLPF24X-*,24,0,0,0,0)                                        
         DC    CL3' ',CL8'ORDER',CL8'SELECT'                                    
BLPF24X  EQU   *                                                                
                                                                                
         DC    X'FF'                                                            
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                                       
***********************************************************************         
BPFTABLE  DS    0C                                                              
*                                                                               
* BUYLIST                                                                       
         DC    AL1(BPF03X-*,03,0,0,(BPF03X-BPF03)/KEYLNQ,0)                     
         DC    CL3' ',CL8'BUY',CL8'LIST'                                        
BPF03    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                   
BPF03X   EQU   *                                                                
*                                                                               
* CONTRACT                                                                      
         DC    AL1(BPF04X-*,04,0,0,(BPF04X-BPF04)/KEYLNQ,0)                     
         DC    CL3' ',CL8'CONTRACT',CL8'LIST'                                   
BPF04    DC    AL1(KEYTYTWA,L'DRVASTA-1),AL2(DRVASTA-T80FFFD)                   
         DC    AL1(KEYTYTWA,L'DE1OFF-1),AL2(DE1OFF-T80FFFD)                     
         DC    AL1(KEYTYTWA,L'DRVAAGY-1),AL2(DRVAAGY-T80FFFD)                   
         DC    AL1(KEYTYTWA,L'DRVADTE-1),AL2(DRVADTE-T80FFFD)                   
BPF04X   EQU   *                                                                
*                                                                               
* PRINT                                                                         
         DC    AL1(BPF05X-*,05,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
BPF05X   EQU   *                                                                
*                                                                               
* BRAND LIST                                                                    
*         DC    AL1(BPF06X-*,06,0,0,(BPF06X-BPF06)/KEYLNQ,0)                    
*         DC    CL3' ',CL8'BRAND',CL8'LIST'                                     
*BPF06    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                  
*BPF06X   EQU   *                                                               
*                                                                               
*                                                                               
* HISTORY LIST                                                                  
         DC    AL1(BPF07X-*,07,0,0,(BPF07X-BPF07)/KEYLNQ,0)                     
         DC    CL3' ',CL8'HISTORY',CL8'LIST'                                    
BPF07    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T80FFFD)                   
BPF07X   EQU   *                                                                
*                                                                               
* OPEN                                                                          
         DC    AL1(BPF10X-*,10,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
BPF10X   EQU   *                                                                
                                                                                
* REJECT                                                                        
         DC    AL1(BPF11X-*,11,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
BPF11X   EQU   *                                                                
                                                                                
* RETURN TO CALLER                                                              
*        DC    AL1(BPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    AL1(BPF12X-*,12,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
BPF12X   EQU   *                                                                
*                                                                               
* ACTUAL APPROVE                                                                
         DC    AL1(BPF22X-*,22,0,0,0,0)                                         
         DC    CL3' ',CL8'BRAND',CL8'OPEN'                                      
BPF22X   EQU   *                                                                
                                                                                
* ACTUAL REJECT                                                                 
         DC    AL1(BPF23X-*,23,0,0,0,0)                                         
         DC    CL3' ',CL8'BRAND',CL8'REJECT'                                    
BPF23X   EQU   *                                                                
                                                                                
* RETURN TO BRAND LIST                                                          
         DC    AL1(BPF24X-*,24,0,0,0,0)                                         
         DC    CL3' ',CL8'BRAND',CL8'LIST'                                      
BPF24X   EQU   *                                                                
                                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ISSPOT: IS THIS A SPOT ORDER?                                                 
***********************************************************************         
ISSPOT   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   ISSPOTX                                                          
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRE?                                      
         BO    ISSPNO                                                           
ISSPYES  SR    RC,RC                                                            
ISSPNO   LTR   RC,RC                                                            
ISSPOTX  XMOD1                                                                  
         EJECT                                                                  
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
         CLI   DE1HDLNH+5,0        AGENCY ORDER # ENTERED?                      
         BE    RKEY0900            NO  - NEED #                                 
         CLI   DE1HDLNH+5,8        EIGHT CHARACTERS ENTERED?                    
         BNE   RKEY0900            NO  - IN ERROR                               
         CLI   DE1STATH+5,0        STATION ENTERED?                             
         BE    RKEY0901            NO  - NEED STATION                           
         CLI   DE1AGYH+5,0         AGENCY ENTERED?                              
         BE    RKEY0902            NO  - NEED AGENCY                            
         LA    R2,DE1STATH                                                      
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
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
         LA    RF,DE1HDLNH                                                      
         ST    RF,DUB                                                           
         B     RKEY0910                                                         
RKEY0800 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     RKEY0920                                                         
RKEY0901 EQU   *                                                                
         MVI   DUB+4,1             SET ERROR TRANSFER                           
         LA    RF,DE1STATH                                                      
         ST    RF,DUB                                                           
         B     RKEY0910                                                         
RKEY0902 EQU   *                                                                
         MVI   DUB+4,1             SET ERROR TRANSFER                           
         LA    RF,DE1AGYH                                                       
         ST    RF,DUB                                                           
         B     RKEY0910                                                         
RKEY0903 EQU   *                                                                
         MVI   DUB+4,2             SET ERROR TRANSFER                           
         LA    RF,DE1HDLNH                                                      
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
         ZIC   RE,DE1AGYH+5        GET L(ROUTING AGENCY CODE)                   
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
         GOTO1 HEXIN,DMCB,DE1HDLN,RDARKORD,8                                    
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
         SR    R0,R0               SET CC ZERO                                  
         B     GAGY0220            EXIT CC ZERO                                 
GAGY0200 DS    0H                                                               
         LTR   RB,RB               SET CC NOT ZERO                              
GAGY0220 EQU   *                                                                
         XIT1                                                                   
GAGY0800 MVC   RDARKAGY(0),DE1AGY                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PERFORM S/P-P/P REASSIGNMENT IF NECESSARY                                     
***********************************************************************         
REASSIGN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DRVNPSNH+5,0        ANYTHING IN REASSIGN FIELD?                  
         BE    RASS0900            NO  - NO FURTHER ACTION                      
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVI   ELCODE,X'0A'        S/P-P/P ELEMENT                              
         BRAS  RE,GETEL                                                         
         BNE   RASS0880            NO ELEMENT                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         USING RDARFLEM,R6                                                      
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,6               SET TO S/P KEY                               
         TM    RDARFLG1,X'01'      UNWIRED ORDER?                               
         BNO   RASS0020            NO                                           
         MVI   KEY,X'31'           YES - SET TO P/P KEY                         
RASS0020 EQU   *                                                                
         MVC   KEY+22(2),AGENCY    INSERT REP CODE                              
         MVC   KEY+24(3),DRVNPSN   INSERT S/P-P/P CODE                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVC   REASSLDT,=X'FFFFFF' SET LEAVE DATE TO INFINITY                   
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BNE   RASS0800            NO  - SET TO RETURN ERROR                    
         MVC   AIO,AIO3            SET READ AREA TO IOAREA3                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC RETRIEVE NEW S/P-P/P RECORD                               
         L     R6,AIO                                                           
         CLI   KEY,X'06'           SALESPERSON RECORD?                          
         BNE   RASS0040            NO  - POINT PERSON                           
         USING RSALREC,R6          USE S/P RECORD                               
         MVC   REASSNAM,RSALNAME                                                
         MVC   REASSOFF,RSALOFF                                                 
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    RASS0025            NO                                           
         MVC   REASSLDT,RSALLEAV   YES - SAVE LEAVE DATE                        
RASS0025 EQU   *                                                                
         MVI   REASSFLG,0          CLEAR FLAG                                   
         TM    RSALFLG,X'20'       KEEP FROM EDI?                               
         BNO   RASS0030            NO                                           
         MVI   REASSFLG,1          YES - SET FLAG                               
RASS0030 EQU   *                                                                
         DROP  R6                                                               
         B     RASS0060                                                         
RASS0040 EQU   *                                                                
         USING RPTPREC,R6          USE P/P RECORD                               
         MVC   REASSNAM,RPTPNAME                                                
         MVC   REASSOFF,RPTPOFF                                                 
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    RASS0045            NO                                           
         GOTO1 DATCON,DMCB,(2,RPTPLDAT),(3,REASSLDT)                            
*                                  CONVERT LEAVE DATE TO 3-CHAR BIN             
RASS0045 EQU   *                                                                
         MVI   REASSFLG,0          CLEAR FLAG                                   
         TM    RPTPFLG,X'20'       KEEP FROM EDI?                               
         BNO   RASS0050            NO                                           
         MVI   REASSFLG,1          YES - SET FLAG                               
RASS0050 EQU   *                                                                
         DROP  R6                                                               
         B     RASS0060                                                         
RASS0060 EQU   *                                                                
         MVC   AIO,AIO1            RESET A(IO AREA)                             
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
***      OC    REASSOFF,REASSOFF   ANY OFFICE ENTERED?                          
***      BZ    RASS0080            NO  - DON'T FILTER                           
         GOTO1 DATCON,DMCB,(5,0),(3,WORK)                                       
         CLC   REASSLDT,WORK       LEFT TODAY OR EARLIER?                       
         BNH   RASS0870            YES                                          
         CLI   REASSFLG,1          KEEP FROM EDI?                               
         BE    RASS0860            YES - ERROR EXIT                             
         CLC   RDARKAOF,REASSOFF   YES - OFFICE MATCH?                          
         BNE   RASS0850            NO  - ERROR EXIT                             
RASS0080 EQU   *                                                                
         L     RF,AIO3             SET A(IOAREA3)                               
         XC    0(128,RF),0(RF)                                                  
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3:  KEY BUILD AREA                                                  
*        AIO1:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
***      GOTO1 =V(REGENDTR),DMCB,(X'01',ACOMFACS),AIO3,AIO1,          X         
***            AIO2,RR=RELO                                                     
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),AIO3,AIO1,     X        
               AIO2                                                             
*                                                                               
         MVC   KEY(27),RDARKEY     GET RECORD FOR UPDATE                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BE    *+6                 YES - MUST BE THERE                          
         DC    H'0'                NO  - DUMP IT OUT                            
         MVC   HDRDA,KEY+28        SAVE DISK ADDRESS OF RECORD                  
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC GET THE DARE RECORD                                       
         LA    RF,RDARELEM                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
RASS0100 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    RASS0700            YES                                          
         CLI   0(RF),X'0A'         S/P-P/P ELEMENT?                             
         BE    RASS0120            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     RASS0100            GO BACK FOR NEXT                             
RASS0120 EQU   *                                                                
         MVC   RDARPPSP-RDARPPEL(3,RF),DRVNPSN                                  
*                                  MOVE REASSIGNED P/P INTO ELEMENT             
         GOTO1 PUTREC              REWRITE THE RECORD                           
*                                                                               
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3+800  KEY BUILD AREA                                               
*        AIO1:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         L     R6,AIO3             SET A(KEY BUILD AREA)                        
         A     R6,=F'800'          ADD 800 TO ADDRESS                           
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R6),AIO1,     X        
               AIO2                                                             
*                                                                               
         XC    DRVPSN,DRVPSN                                                    
         MVC   DRVPSN(3),DRVNPSN   LOAD NEW POINT PERSON                        
         MVC   DRVPSN+4(20),REASSNAM                                            
*                                  INSERT NEW POINT PERSON NAME                 
         FOUT  DRVPSNH                                                          
         XC    DRVNPSN,DRVNPSN                                                  
         FOUT  DRVNPSNH                                                         
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         L     R4,AIO3             A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->NEW PASSIVE POINTERS                     
*                                                                               
***     GOTO1 =V(REGENDTR),DMCB,(X'02',ACOMFACS),AIO3,(R4),HDRDA,     X         
***           RR=RELO                                                           
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),AIO3,(R4),     X        
               HDRDA                                                            
                                                                                
RASS0700 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     RASS0900            EXIT CC ZERO                                 
RASS0800 EQU   *                                                                
         MVI   DUB,0               SET ERROR RETURN CODE                        
         B     RASS0890                                                         
RASS0850 EQU   *                                                                
         MVI   DUB,1               SET ERROR RETURN CODE                        
         B     RASS0890                                                         
RASS0860 EQU   *                                                                
         MVI   DUB,2               SET ERROR RETURN CODE                        
         B     RASS0890                                                         
RASS0870 EQU   *                                                                
         MVI   DUB,3               SET ERROR RETURN CODE                        
         B     RASS0890                                                         
RASS0880 EQU   *                                                                
         MVI   DUB,4               SET ERROR RETURN CODE                        
         B     RASS0890                                                         
RASS0890 EQU   *                                                                
         LTR   RB,RB               ERROR - SET CC NOT ZERO                      
RASS0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
* DISPLAY THE AGENCY ORDER                                                      
***********************************************************************         
DISAORD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   AGYVKEY,0(R6)       SAVE OFF AGY KEY                             
         TM    CTLRFLG1,CF1BRDQ    IF BRAND ORDER PROCESSING                    
         BO    DISA10              DON'T SAVE OFF AGY KEY                       
         MVC   SVAGYVKY,AGYVKEY    WE NEED IT FOR ORDER/LIST                    
*                                                                               
DISA10   DS    0H                                                               
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARKORD    AGENCY ORDER NUMBER                       
         EDIT  (P5,WORK+17),(8,DRVAGOD),ALIGN=LEFT,FILL=0                       
                                                                                
         MVC   DRVASTA(4),RDARKSTA AGENCY STATION                               
         CLI   RDARKSTA+4,C' '                                                  
         BE    DISA20                                                           
         MVC   DRVASTA+4(3),=C'- M'                                             
         MVC   DRVASTA+5(1),RDARKSTA+4                                          
         CLI   RDARKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   DRVASTA+6,C'V'                                                   
         CLI   RDARKSTA+3,C' '                                                  
         BNE   DISA20                                                           
         MVC   DRVASTA+3(3),DRVASTA+4                                           
         MVI   DRVASTA+6,C' '                                                   
                                                                                
DISA20   DS    0H                  UNLINK ORDERS NEED TO READ STATION           
*        OC    RDARREP#,RDARREP#   RECORD FOR MARKET NAME                       
*        BNZ   DISA30                                                           
         XC    EMKTNAME,EMKTNAME                                                
         LA    R2,DRVASTAH                                                      
         MVI   5(R2),6                                                          
         GOTO1 VALISTA             GET MARKET NAME                              
         BNZ   DISA30                                                           
         MVC   EMKTNAME,WORK+10                                                 
                                                                                
DISA30   DS    0H                                                               
         MVC   DRVAMKT,EMKTNAME                                                 
                                                                                
         XC    DRVAAGY,DRVAAGY                                                  
         MVC   DRVAAGY(L'RDARKAGY),RDARKAGY    AGENCY CODE                      
         OC    RDARKAOF,RDARKAOF                                                
         BZ    DISA40                                                           
         LA    RE,DRVAAGY                                                       
         MVI   DRVAAGY+3,C' '                                                   
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RDARKAOF    AGENCY OFFICE                                
         DROP  R6                                                               
*                                                                               
DISA40   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARKRT,X'35'       PRODUCTS FOR VARIOUS RECORD? EXIT            
         BNE   DISA45                                                           
         MVC   DRVAGOD,=8C'*'                                                   
         MVC   DRVSTAT(36),=C'*** PRODUCT(S) FOR VARIOUS ORDER ***'             
         MVC   DRVPRD1,RDARPDN1                                                 
         MVC   DRVPRD2,RDARPDN2                                                 
         XC    DE1HDLN,DE1HDLN                                                  
         OI    DE1HDLNH+6,X'40'    FORCE CURSOR HERE                            
         B     DISAX                                                            
         DROP  R6                                                               
*                                                                               
DISA45   DS    0H                                                               
         USING RDARELEM,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    RDARRNUM,RDARRNUM   CANNOT UNLINK FOR REVISIONS                  
         BNZ   DISA50                                                           
         MVC   DRVQLNK(16),=C'(U)nlink Order ?'                                 
         NI    DRVQLNKH+1,X'FF'-X'04' SHOW LNK/UNLNK QUESTION                   
         NI    DRVLINKH+1,X'FF'-X'20' UNPROTECT INPUT FIELD                     
                                                                                
DISA50 DS      0H                                                               
         XC    DRVSTAT,DRVSTAT     CLEAR STATUS                                 
                                                                                
         OC    RDARREP#,RDARREP#   CONTRACT NUMBER, IF LINKED                   
         BZ    DISA120                                                          
                                                                                
         LA    R2,DRVSTAT+28                                                    
         MVC   DRVSTAT(40),=C'*** ORDER IS LINKED TO CON#          ***'         
*                                                                               
         OC    RDARRNUM,RDARRNUM   PROTECT UNLINK FIELD IF REVISION             
         BZ    DISA60                                                           
         OI    DRVQLNKH+1,X'04'    HIDE LINK/UNLINK INPUT QUESTION              
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
         OI    DE1HDLNH+6,X'40'+X'80'                                           
*                                                                               
DISA60 DS      0H                                                               
         TM    RDARMISC,X'04'      ORDER IS CONFIRMED VARIOUS                   
         BZ    DISA70                                                           
         XC    DRVSTAT,DRVSTAT                                                  
         MVC   DRVSTAT(31),=C'*** CONFIRMED VARIOUS ORDER ***'                  
         OI    DRVQLNKH+1,X'04'    HIDE LINK/UNLINK INPUT QUESTION              
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
         B     DISA160                                                          
*                                                                               
DISA70   DS    0H                                                               
         L     RF,AIO              RDARREC                                      
         CLI   0(RF),X'51'         CONFIRMED ORDER?                             
         BNE   DISA75              NO                                           
         LA    R2,DRVSTAT+32                                                    
         MVC   DRVSTAT(42),=C'*CONFIRMED ORDER LINKED TO CON#          X        
               *'                                                               
         OI    DRVQLNKH+1,X'04'    HIDE LINK/UNLINK INPUT QUESTION              
         OI    DE1HDLNH+6,X'40'+X'80'                                           
         TM    CTLRFLG1,CF1BRDQ    SKIP FOR BRAND                               
         BO    *+8                 NEED AT LEAST ONE BLANK FIELD                
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
         B     DISA110                                                          
                                                                                
DISA75   DS    0H                                                               
         CLI   RDARBSTS,C'A'                                                    
         BNE   DISA80                                                           
         LA    R2,DRVSTAT+32                                                    
         MVC   DRVSTAT(42),=C'*  OPENED ORDER LINKED TO CON#           X        
               *'                                                               
         OI    DRVQLNKH+1,X'04'    HIDE LINK/UNLINK INPUT QUESTION              
         OI    DE1HDLNH+6,X'40'+X'80'                                           
         TM    CTLRFLG1,CF1BRDQ    SKIP FOR BRAND                               
         BO    *+8                 NEED AT LEAST ONE BLANK FIELD                
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
         B     DISA110                                                          
                                                                                
DISA80   DS    0H                                                               
         CLI   RDARBSTS,C'R'                                                    
         BE    DISA90                                                           
         CLI   RDARBSTS,C'M'                                                    
         BE    DISA90                                                           
         CLI   RDARBSTS,C'C'                                                    
         BNE   DISA110                                                          
                                                                                
DISA90   DS    0H                                                               
         LA    R2,DRVSTAT+32                                                    
         MVC   DRVSTAT(42),=C'* REJECTED ORDER LINKED TO CON#          X        
               *'                                                               
         CLI   RDARBSTS,C'M'                                                    
         BNE   *+10                                                             
         MVC   DRVSTAT+2(6),=C' AMEND'                                          
         TM    RDARMISC,X'20'      SHOW IF NOTDARED                             
         BZ    DISA100                                                          
         MVC   DRVSTAT+2(6),=C'NOTDAR'                                          
         CLC   =C'PV',AGENCY       POETIC PETRY                                 
         BNE   DISA110                                                          
         MVC   DRVSTAT+2(6),=C'NOTPOE'                                          
         B     DISA110                                                          
                                                                                
DISA100  DS    0H                                                               
         CLI   RDARBSTS,C'C'       SHOW IF RECALLED                             
         BNE   DISA110                                                          
         LA    R2,DRVSTAT+32                                                    
         MVC   DRVSTAT+2(6),=C'RECALL'                                          
         TM    RDARDELS,X'20'      RECALL CANCELLED                             
         BZ    DISA110                                                          
         MVC   DRVSTAT+1(7),=C'CANCELL'                                         
                                                                                
DISA110  DS    0H                                                               
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,0(R2)),ALIGN=LEFT                                
*        TM    RDARMISC,X'40'      OPENED AT LEAST ONCE?                        
*        BZ    DISA160                                                          
         B     DISA160                                                          
                                                                                
DISA120  DS    0H                  ORDER IS UNLINKED                            
         OC    RDARRNUM,RDARRNUM   IF REVISION AND IS MISSING CONTRACT          
         BZ    DISA125             ORDER MUST BE A TAKEOVER                     
         OC    RDARREP#,RDARREP#   WARN USER TO ENTER TAKEOVER CONTRACT         
         BNZ   DISA125                                                          
         XC    DRVSTAT,DRVSTAT                                                  
         MVC   DRVSTAT,=C'*** PLEASE GENERATE TAKEOVER CONTRACT *** '           
         OI    DRVQLNKH+1,X'04'    HIDE LINK/UNLINK INPUT QUESTION              
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
*                                                                               
* IF THE USER RAN THE REREP1502 TAPE TAKEOVER UPDATE AFTER THE REVISION         
* HAS COME IN THE MAILBOX, WE SHOULD AUTOMATICALLY LINK UP THE CONTRACT         
* AND THE AGENCY ORDER HERE                                                     
*                                                                               
         L     R4,AIO              GET ADDRESSIBILLITY TO ORDER NUMBER          
         USING RDARREC,R4                                                       
         CLC   RDARKORD,CDARNUM                                                 
         BNE   DISA160                                                          
         DROP  R4                                                               
*                                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERR3                                                          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                  RE-READ FOR PUTREC                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RDARREP#,CCONKNUM                                                
*                                                                               
         GOTO1 PUTREC              WRITE OUT THE RECORD                         
*                                                                               
         OC    CCONDKAD,CCONDKAD                                                
         BZ    BADERR3                                                          
         MVC   KEY+28(4),CCONDKAD                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIOAREA                                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R4,R6                                                            
         L     R6,AIO1                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RDARELEM,R6                                                      
         USING RCONDREL,R4                                                      
         OI    RCONDRFG,X'80'      FLAG LINKED                                  
         MVC   RCONDRRV,RDARRNUM                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 PUTREC              UPDATE CONTRACT WITH NEW LINK INFO           
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         B     DISA50              REDISPLAY LINK INFO                          
*                                                                               
DISA125  DS    0H                                                               
         CLI   RDARBSTS,C'R'       REJECTED?                                    
         BE    DISA130                                                          
         CLI   RDARBSTS,C'C'                                                    
         BE    DISA130                                                          
         MVC   DRVSTAT(27),=C'*** ORDER IS NOT LINKED ***'                      
         MVC   DRVQLNK(16),=C'  (L)ink Order ?'                                 
         OI    DRVLINKH+6,X'40'    FORCE CURSOR HERE                            
         B     DISA160                                                          
                                                                                
DISA130  DS    0H                  ORDER IS UNLINKED AND REJECTED               
         MVC   DRVSTAT(36),=C'*** REJECTED ORDER IS NOT LINKED ***'             
         TM    RDARMISC,X'20'      SHOW IF NOTDARED                             
         BZ    DISA140                                                          
         MVC   DRVSTAT+4(6),=C'NOTDAR'                                          
         CLC   =C'PV',AGENCY       POETIC PETRY                                 
         BNE   DISA150                                                          
         MVC   DRVSTAT+4(6),=C'NOTPOE'                                          
         B     DISA150                                                          
                                                                                
DISA140  DS    0H                                                               
         CLI   RDARBSTS,C'C'       SHOW IF RECALLED                             
         BNE   DISA150                                                          
         MVC   DRVSTAT+4(6),=C'RECALL'                                          
                                                                                
DISA150  DS    0H                                                               
         OI    DRVQLNKH+1,X'04'    HIDE LINK/UNLINK INPUT QUESTION              
         TM    CTLRFLG1,CF1BRDQ    SKIP FOR BRAND                               
         BO    *+8                 NEED AT LEAST ONE BLANK FIELD                
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
                                                                                
DISA160  DS    0H                                                               
         MVC   DRVFROM,RDARBUYR    FROM BUYER                                   
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DISA170                                                          
* FOR DDS TERMINALS, DISPLAY D/A OF K, NOT BUYER NAME                           
         XC    DRVFROM,DRVFROM                                                  
         MVC   DRVFROM(4),=C'D/A='                                              
         GOTO1 HEXOUT,DMCB,DAREDKAD,DRVFROM+5,4,0                               
                                                                                
* PHONE                                                                         
DISA170  DS    0H                                                               
         MVC   DRVFONE+1(3),RDARBTEL                                            
         MVC   DRVFONE+6(3),RDARBTEL+3                                          
         MVC   DRVFONE+10(4),RDARBTEL+6                                         
         MVC   DRVFONE+19(6),RDARBXTN EXTENSION                                 
         MVC   DRVAAGX,RDARAGNM    AGENCY NAME                                  
*                                                                               
         CLC   =C'$EDI$',RDARAGAD  SKIP FOR EDI ORDERS                          
         BNE   DISA175             AGENCY NAME AREA USED FOR STORAGE            
         XC    DRVAAGX,DRVAAGX                                                  
*                                                                               
* ESTIMATE NUMBER                                                               
DISA175  DS    0H                                                               
         EDIT  RDAREST#,(8,DRVAEST),ALIGN=LEFT                                  
*                                                                               
* SHOW IF TRADE                                                                 
         XC    DRVTFLG,DRVTFLG                                                  
         NI    DRVTFLGH+1,X'FF'-X'08'-X'04'                                     
         CLI   RDARCORT,C'T'                                                    
         BNE   DISA180                                                          
         MVC   DRVTFLG,=C'TRADE'                                                
         OI    DRVTFLGH+1,X'08'                                                 
*                                                                               
* EST START-END DATES                                                           
DISA180  DS    0H                                                               
         LA    R2,DRVADTEH                                                      
         GOTO1 DATCON,DMCB,(2,RDARESST),(3,FLTSTART)                            
         GOTO1 DATCON,DMCB,(2,RDARESST),(5,8(R2))                               
         CLC   RTKODATE,FLTSTART   TAKEOVER DATE CHOPPING REQUIRED??            
         BL    DISA181                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,RTKODATE),(5,8(R2))                               
*                                                                               
DISA181  DS    0H                                                               
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,RDARESEN),(5,17(R2))                              
*                                                                               
* REVISION STATUS                                                               
*                                                                               
         OC    RDARRNUM,RDARRNUM                                                
         BZ    DISA190                                                          
         XC    DRVRVST,DRVRVST                                                  
         MVC   DRVRVST(8),=C'Revision'                                          
         EDIT  RDARRNUM,(3,DRVRVST+9),ALIGN=LEFT                                
         MVC   DRVRVSH(6),=C'Method'                                            
*                                                                               
         CLI   STAMETH,0           STATION OVERRIDE??                           
         BE    DISA183                                                          
         MVC   BYTE,STAMETH                                                     
         B     DISA185                                                          
*                                                                               
DISA183  DS    0H                                                               
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AH    R2,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R2                                                       
*                                                                               
         MVI   BYTE,3                                                           
         TM    SVPGPBIT+CNTDRV3B,CNTDRV3A                                       
         BO    DISA185                                                          
         MVI   BYTE,2                                                           
         TM    SVPGPBIT+CNTDRV2B,CNTDRV2A                                       
         BO    DISA185                                                          
         MVI   BYTE,1                                                           
         TM    SVPGPBIT+CNTDRV1B,CNTDRV1A                                       
         BZ    DISA187                                                          
*                                                                               
DISA185  DS    0H                                                               
         XC    DRVRVS2,DRVRVS2                                                  
         CLI   BYTE,1                                                           
         BNE   DISA185A                                                         
         MVC   DRVRVS2(20),=C'Cancel and Supercede'                             
         B     DISA187                                                          
*                                                                               
DISA185A DS    0H                                                               
         CLI   BYTE,2                                                           
         BNE   DISA185B                                                         
         MVC   DRVRVS2(19),=C'Add Additional Line'                              
         B     DISA187                                                          
*                                                                               
DISA185B DS    0H                                                               
         CLI   BYTE,3                                                           
         BNE   DISA187                                                          
         MVC   DRVRVS2(20),=C'Change Existing Line'                             
*                                                                               
DISA187  DS    0H                                                               
*&&DO                                                                           
         TM    CCONFLAG,CCONMANR                                                
         BZ    DISA190                                                          
         XC    DRVRVS2,DRVRVS2                                                  
         MVC   DRVRVS2(15),=C'Manual Revision'                                  
*&&                                                                             
*                                                                               
* RECEIVED DATE AND TIME                                                        
DISA190  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RDARDATE),(5,DRVRECD)                             
         EDIT  RDARTIME,(4,MYWORK)                                              
         MVC   DRVRECT(2),MYWORK   FORMAT TO HH:MM                              
         MVI   DRVRECT+2,C':'                                                   
         MVC   DRVRECT+3(2),MYWORK+2                                            
                                                                                
         XC    DRVAPVD,DRVAPVD     CLEAR APPROVE DATE AND TIME                  
         XC    DRVAPVT,DRVAPVT                                                  
         XC    DRVREJD,DRVREJD     CLEAR REJECT DATE AND TIME                   
         XC    DRVREJT,DRVREJT                                                  
                                                                                
         OC    RDARREP#,RDARREP#   IF NOT LINKED, ONLY POSSIBILITIES            
         BZ    DISA200             ARE DISPLAY OF REJECTION OR RECALL           
*                                  DATE/TIME                                    
                                                                                
         OC    CDARAPDT,CDARAPDT                                                
         BZ    DISA200                                                          
*                                                                               
* FOR REVISION, IF RECEIVED DATE IS MORE CURRENT, DON'T SHOW APPROVE DT         
*                                                                               
         OC    RDARRNUM,RDARRNUM                                                
         BZ    DISA195                                                          
         CLC   RDARDATE(2),CDARAPDT                                             
         BH    DISA200                                                          
         BL    DISA195                                                          
         ZICM  RF,RDARTIME,2                                                    
         CVD   RF,DUB                                                           
         ICM   RF,15,DUB+4                                                      
         SRL   RF,4                                                             
         STCM  RF,3,HALF           CHECK TIME TOO                               
         CLC   HALF(2),CDARAPTM                                                 
         BNL   DISA200                                                          
         DROP  R6                                                               
*                                                                               
* OPENED DATE                                                                   
DISA195  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,CDARAPDT),(5,DRVAPVD)                             
* OPENED TIME HH:MM                                                             
         GOTO1 HEXOUT,DMCB,CDARAPTM,DRVAPVT,1,0                                 
         MVI   DRVAPVT+2,C':'                                                   
         GOTO1 HEXOUT,DMCB,CDARAPTM+1,DRVAPVT+3,1,0                             
                                                                                
* IF RECALLED, SHOW DATE/TIME STAMPS                                            
DISA200  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         XC    DRVRJRC,DRVRJRC                                                  
         CLC   =C'$EDI$',RDARAGAD  SKIP REJECT DATE/TIME FOR EDI                
         BE    DISA230                                                          
         MVC   DRVRJRC,=C'Rejected:'                                            
         CLI   RDARBSTS,C'M'                                                    
         BNE   *+10                                                             
         MVC   DRVRJRC,=C'Amended: '                                            
         CLI   RDARBSTS,C'C'                                                    
         BNE   DISA210                                                          
         MVC   DRVRJRC,=C'Recalled:'                                            
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISA230                                                          
         USING RDARRKEM,R6                                                      
* RECALLED DATE                                                                 
         GOTO1 DATCON,DMCB,(2,RDARRKDT),(5,DRVREJD)                             
* RECALLED TIME HH:MM                                                           
         GOTO1 HEXOUT,DMCB,RDARRKTM,DRVREJT,1,0                                 
         MVI   DRVREJT+2,C':'                                                   
         GOTO1 HEXOUT,DMCB,RDARRKTM+1,DRVREJT+3,1,0                             
         B     DISA230                                                          
         DROP  R6                                                               
                                                                                
DISA210  DS    0H                                                               
         OC    CDARRJDT,CDARRJDT                                                
         BZ    DISA220                                                          
* REJECTED DATE                                                                 
         GOTO1 DATCON,DMCB,(2,CDARRJDT),(5,DRVREJD)                             
* REJECTED TIME HH:MM                                                           
         GOTO1 HEXOUT,DMCB,CDARRJTM,DRVREJT,1,0                                 
         MVI   DRVREJT+2,C':'                                                   
         GOTO1 HEXOUT,DMCB,CDARRJTM+1,DRVREJT+3,1,0                             
         B     DISA230                                                          
*                                  REJECTION DATE/TIME FOR UNLINKED ORD         
DISA220  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISA230                                                          
         USING RDARRTEM,R6                                                      
* REJECTED DATE                                                                 
         GOTO1 DATCON,DMCB,(2,RDARRTDT),(5,DRVREJD)                             
* REJECTED TIME HH:MM                                                           
         GOTO1 HEXOUT,DMCB,RDARRTTM,DRVREJT,1,0                                 
         MVI   DRVREJT+2,C':'                                                   
         GOTO1 HEXOUT,DMCB,RDARRTTM+1,DRVREJT+3,1,0                             
         DROP  R6                                                               
                                                                                
DISA230  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARCLEM,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   DRVPRD1(4),RDARPRD1 PRD CODE 1                                   
         MVC   DRVPRD1+5(L'DRVPRD1-5),RDARPRN1                                  
         MVC   DRVAADV,RDARCLI     ADV CODE                                     
         MVC   DRVAADX,RDARCLNM    ADV EXP                                      
         MVC   DRVPRD2(4),RDARPRD2 PRD CODE 2                                   
         MVC   DRVPRD2+5(L'DRVPRD2-5),RDARPRN2                                  
         MVC   DRVADEM(8),RDARTDEM    DEMO                                      
         DROP  R6                                                               
*                                                                               
* DEMO CATEGORIES                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'$EDI$',RDARAGAD                                               
         BE    DISA235             SKIP IF EDI                                  
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISA235                                                          
         USING RDARDMEL,R6                                                      
         CLC   RDARDEM1(16),SPACES                                              
         BE    DISA235                                                          
*                                                                               
         LA    R3,0                                                             
         LA    R6,RDARDEM1                                                      
         LA    R4,MYWORK                                                        
         XC    MYWORK(30),MYWORK                                                
         DROP  R6                                                               
*                                                                               
DISA231  DS    0H                                                               
         AHI   R3,1                                                             
         CLI   0(R6),C'('          SKIP USER DEFINED DEMOS                      
         BE    DISA231A                                                         
         MVC   1(1,R4),0(R6)                                                    
         CLI   1(R4),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R4),C'I'                                                       
         PACK  DUB(8),1(3,R6)                                                   
         CVB   RF,DUB                                                           
         STC   RF,2(R4)                                                         
*                                                                               
         LA    R4,3(R4)                                                         
*                                                                               
DISA231A DS    0H                                                               
         LA    R6,L'RDARDEM1(R6)                                                
         CLC   0(4,R6),SPACES                                                   
         BE    DISA232                                                          
         CHI   R3,4                                                             
         BL    DISA231                                                          
*                                                                               
DISA232  DS    0H                                                               
         L     R4,AIO3                                                          
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
                                                                                
         GOTO1 DEMOCON,DMCB,((R3),MYWORK),(2,WORK),(0,DBLOCKD)                  
         DROP  R4                                                               
*                                                                               
* FORMAT OUTPUT                                                                 
*                                                                               
         LA    R2,DRVADEM                                                       
         MVC   DRVADEM,SPACES                                                   
         LA    R4,WORK                                                          
*                                                                               
         CHI   R3,1                                                             
         BNH   DISA234A                                                         
         AHI   R3,-1                                                            
*                                                                               
DISA233  DS    0H                                                               
         MVC   0(7,R2),0(R4)                                                    
         AHI   R4,7                                                             
*                                                                               
DISA234  DS    0H                                                               
         AHI   R2,1                                                             
         CLI   0(R2),C' '                                                       
         BNE   DISA234                                                          
*                                                                               
         MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
         BCT   R3,DISA233                                                       
DISA234A MVC   0(7,R2),0(R4)                                                    
*                                                                               
DISA235  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'$EDI$',RDARAGAD  EDI ORDERS USE PRODUCT 2 NAME                
         BNE   DISA240             FOR SPECIAL STORAGE                          
         XC    DRVPRD2,DRVPRD2                                                  
         DROP  R6                                                               
*                                                                               
DISA240  DS    0H                                                               
         OC    DRVCTOT,DRVCTOT     TOTAL ALREADY DISPLAYED??                    
         BNZ   DISA260                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 =A(DISTOTAL),RR=RELO                                             
         MVC   AIO,AIO1                                                         
*                                                                               
DISA260  DS    0H                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 =A(DISPPRSN),RR=RELO                                             
         MVC   AIO,AIO1                                                         
*                                                                               
DISAX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
BADERR3  MVC   RERROR,=AL2(440)    UNEXPECTED ERROR ENCOUNTERED,                
         LA    R2,DE1HDLNH          CALL DDS                                    
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY POINT PERSON INFORMATION ABOUT THIS AGENCY ORDER                      
*                                                                               
***********************************************************************         
DISPPRSN NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO1             AIO1 CONTAINS THE DARE RECORD                
         MVI   ELCODE,X'0A'        ACCESS POINT PERSON ELEMENT                  
         BRAS  RE,GETEL                                                         
         BNE   DPPS0200            NOT FOUND: NOTHING TO DISPLAY                
         USING RDARPPCD,R6                                                      
         MVC   DRVPSN(3),RDARPPSP  INSERT POINT PERSON CODE                     
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1             AIO1 CONTAINS THE DARE RECORD                
         MVI   ELCODE,X'0F'        ACCESS MISC FLAG ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   DPPS0200            NOT FOUND: NOTHING TO DISPLAY                
         USING RDARFLEM,R6                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,6               SET TO S/P KEY                               
         TM    RDARFLG1,X'01'      UNWIRED ORDER?                               
         BNO   DPPS0020            NO                                           
         MVI   KEY,X'31'           YES - SET TO P/P KEY                         
*                                                                               
         DROP  R6                                                               
*                                                                               
DPPS0020 EQU   *                                                                
         MVC   KEY+22(2),AGENCY    INSERT REP CODE                              
         MVC   KEY+24(3),DRVPSN    INSERT S/P-P/P CODE                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BNE   DPPS0060            NO                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO              SET A(RECORD JUST READ)                      
         USING RSALREC,R6                                                       
         MVC   DRVPSN+4(20),RSALNAME                                            
*                                  INSERT S/P-P/P NAME                          
*                                     NOTE: BOTH IN SAME LOCATION               
         B     DPPS0080                                                         
DPPS0060 EQU   *                                                                
         MVC   DRVPSN+4(11),=C'NOT ON FILE'                                     
DPPS0080 EQU   *                                                                
         FOUT  DRVPSNH                                                          
****     XC    DRVNPSN,DRVNPSN     CLEAR NEW PSN FIELD                          
         FOUT  DRVNPSNH                                                         
DPPS0200 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CALCULATE GRAND TOTAL DOLLARS AND SPOTS. TAKE INTO ACCOUNT POSSIBLE           
* TAKEOVER ORDER                                                                
***********************************************************************         
DISTOTAL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    GTOTAL$,GTOTAL$                                                  
         XC    GSPT#,GSPT#                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),AGYVKEY                                     
*                                                                               
         MVI   KEY+RDARKRT-RDARKEY,X'40' BUY RECORD                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DIST10   DS    0H                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   DIST70                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   KEY+RDARKSRT-RDARKEY,X'00' BUY HEADER                            
         BE    DIST20                                                           
         CLI   KEY+RDARKSRT-RDARKEY,X'10' BUY ORBITS                            
         BE    DIST30                                                           
         CLI   KEY+RDARKSRT-RDARKEY,X'30' BUY DETAIL                            
         BE    DIST40                                                           
         B     DIST60                                                           
*                                                                               
* PROCESS HEADER RECORD                                                         
*                                                                               
DIST20   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARBYEL,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RDARBYDL,X'80'+X'40' HARD/SOFT DELETED??                         
         BNZ   DIST60              SKIP                                         
         MVC   BUYCOST,RDARBYCO                                                 
         MVI   STARTDAY,0          CLEAR START/END DAYS OF WEEK                 
         MVI   ENDDAY,0                                                         
         GOTO1 STARTEND,DMCB,RDARBYRO,WORK                                      
         B     DIST60                                                           
         DROP  R6                                                               
*                                                                               
DIST30   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDAROBEL,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RDAROBDL,X'80'+X'40' HARD/SOFT DELETED??                         
         BNZ   DIST60              SKIP                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 BUYORBIT,DMCB,AIO                                                
         B     DIST60              READ NEXT                                    
*                                                                               
DIST40   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARBDEL,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RDARBDDL,X'80'+X'40' HARD/SOFT DELETED??                         
         BNZ   DIST60              SKIP                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 BUYDETL,DMCB,AIO                                                 
         BNE   DIST60              READ NEXT                                    
*                                                                               
         L     R6,AIO                                                           
         USING RDARBUEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DIST50   DS    0H                                                               
         ZIC   R1,RDARBUWK                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),RDARBUSW                                               
         MH    R1,HALF                                                          
         ZICM  R0,GSPT#,4                                                       
         AR    R0,R1                                                            
         STCM  R0,15,GSPT#                                                      
         MVC   FULL,RDARBU$$                                                    
         M     R0,FULL                                                          
         ZICM  R0,GTOTAL$,4                                                     
         AR    R0,R1                                                            
         STCM  R0,15,GTOTAL$                                                    
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    DIST50                                                           
         DROP  R6                                                               
*                                                                               
DIST60   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     DIST10                                                           
*                                                                               
DIST70   DS    0H                  DISPLAY GRAND TOTAL DOLLAR/SPOT              
         EDIT  GTOTAL$,(14,DRVCTOT),2,COMMAS=YES,ALIGN=LEFT                     
         EDIT  GSPT#,(5,DRVTSPT),ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
DISTYES  SR    RC,RC                                                            
DISTNO   LTR   RC,RC                                                            
DISTX    XIT1                                                                   
***********************************************************************         
*                                                                               
*   STARTEND:  CONVERTS ROTATION MATRIX AND START DAY TO ONE-BYTE               
*        START/END DAY, INSERTS INTO ADDRESS                                    
*                                                                               
***********************************************************************         
STARTEND NTR1                                                                   
         L     R2,0(R1)            A(INPUT: ROTATION+START DAY)                 
         L     R3,4(R1)            A(RECEIVING FIELD)                           
*                                                                               
         ZIC   RF,7(R2)            ROTATION START DAY                           
         SLL   RF,28               STRIP OFF ZONE BITS                          
         SRL   RF,28               SHIFT START DAY BACK                         
         STC   RF,ROTATDAY         SAVE ROTATION START DAY                      
*                                                                               
         LA    R6,0(R2)            A(ROTATION FIELD)                            
         LR    RE,R6               CALCULATE END OF ROTATION FIELD              
         LA    RE,7(RE)                                                         
         AR    R6,RF               GET A(1ST DAY+1)                             
         BCTR  R6,0                BACK OFF TO A(1ST ENTRY)                     
         LR    RF,R6               SAVE A(1ST ENTRY)                            
*                                     FOR WHEN 1ST ENTRY IS ONLY ENTRY          
         LA    R0,7                SET LOOP CONTROL TO SIX DAYS                 
STEX0040 EQU   *                                                                
         CR    R6,RE               END OF ROTATION FIELD REACHED?               
         BNE   STEX0080            NO                                           
         LA    R6,0(R2)            YES  - GO BACK TO FIRST LOCATION             
STEX0080 EQU   *                                                                
         CLI   0(R6),C' '          ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   0(R6),0             ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   STARTDAY,0          ANYTHING IN START DAY?                       
         BNE   STEX0120            YES - DON'T REPLACE                          
         LA    R4,7                NO  - CALCULATE STARTDAY                     
         SR    R4,R0               SUBTRACT REMAINING DAYS                      
         ZIC   R1,ROTATDAY         OFFSET BY ROTATION START DAY                 
         AR    R4,R1                                                            
         CH    R4,=H'7'            WRAPAROUND?                                  
         BNH   STEX0100            NO                                           
         SH    R4,=H'7'            YES - SUBTRACT 7                             
STEX0100 EQU   *                                                                
         STC   R4,STARTDAY         SAVE CALCULATED STARTDAY                     
STEX0120 EQU   *                                                                
         OC    0(1,R3),0(R3)       RECEIVING FIELD ENTRY MADE?                  
         BNZ   STEX0160            YES - START DAY ENTERED                      
         SLL   R4,4                NO  - MOVE START DAY TO HIGH NYBBLE          
         STC   R4,0(R3)            INSERT INTO RECORD                           
STEX0160 EQU   *                                                                
         LR    RF,R6               YES - SAVE NEW ARRAY POSITION                
STEX0200 EQU   *                                                                
         LA    R6,1(R6)            BUMP TO NEXT ARRAY LOCATION                  
         BCT   R0,STEX0040         GO BACK AND CHECK NEXT                       
         LA    R6,0(R2)            A(START OF ARRAY)                            
         BCTR  R6,0                BACK UP 1 POSITION                           
         SR    RF,R6               CALCULATED DISPLACEMENT                      
         STC   RF,ENDDAY           SAVE END DAY FOR EFF DATE SETTING            
         ZIC   RE,0(R3)            RETRIEVE START DAY                           
         AR    RF,RE               ADD START TO END                             
         STC   RF,0(R3)            PUT IT BACK IN RECORD                        
         LA    R6,0(R2)            COUNT NUMBER OF DAYS                         
         SR    RF,RF                                                            
         LA    R0,7                                                             
STEX0240 EQU   *                                                                
         CLI   0(R6),C' '          DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         CLI   0(R6),0             DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         LA    RF,1(RF)            YES - ADD 1                                  
STEX0280 EQU   *                                                                
         LA    R6,1(R6)            BUMP TO NEXT POSITION                        
         BCT   R0,STEX0240         GO BACK AND CHECK NEXT                       
         C     RF,=F'1'            COUNT = 1?                                   
         BH    STEX0320            NO  - HIGHER - EXIT                          
         BE    *+6                 YES - SET START=END IN RECORD                
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                     MEANS EMPTY ARRAY!!!                      
         ZIC   RF,0(R3)            RETRIEVE START/END DAY                       
         SLL   RF,28               DROP START DAY                               
         SRL   RF,24               MOVE END DAY BACK TO HI NYBBLE               
         NI    0(R3),X'0F'         CLEAR START DAY                              
         ZIC   RE,0(R3)            RETRIEVE 0/END DAY                           
         AR    RE,RF               ADD NEW START DAY                            
         STC   RE,0(R3)            MOVE IT BACK                                 
STEX0320 EQU   *                                                                
         B     DISTX                                                            
         EJECT                                                                  
**********************************************************************          
* RETRIEVE AND SAVE ORBIT START DAY                                             
**********************************************************************          
BUYORBIT NTR1                                                                   
         L     R6,0(R1)            RESET A(X'41' RECORD)                        
         USING RDAROEEL,R6                                                      
         OI    MISCFLG1,MF1ORBP    SET 'ORBIT PRESENT' INDICATOR                
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE BEEN 1                           
*                                                                               
BORB0040 EQU   *                                                                
         MVI   STARTDAY,0          CLEAR START/END DAYS                         
         MVI   ENDDAY,0                                                         
         GOTO1 STARTEND,DMCB,RDAROERO,WORK                                      
*                                                                               
         CLI   ORBSTDAY,0          ANY ENTRY IN ORBIT START DAY?                
         BNZ   *+10                                                             
         MVC   ORBSTDAY,STARTDAY   NO  - SAVE FIRST ORBIT START DAY             
*                                                                               
         BRAS  RE,NEXTEL           MORE ORBITS?                                 
         BE    BORB0040            YES                                          
*                                                                               
         B     DISTX                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*   BUYDETL:  BUILDS BUY EFFECTIVE DATE ELEMENTS, THEN INSERTS                  
*        THEM INTO THE NEW BUY RECORD.                                          
***********************************************************************         
BUYDETL  NTR1                                                                   
         L     R6,0(R1)            RESET A(X'41' RECORD)                        
*                                                                               
         CLC   RTKODATE,FLTSTART   TAKEOVER DATE CHOPPING REQUIRED??            
         BL    DISTYES                                                          
*                                                                               
         ST    R6,DMCB             SETUP CHOPPING CALL                          
         MVC   DMCB+4(3),RTKODATE                                               
         MVC   DMCB+8(1),STARTDAY                                               
         MVC   DMCB+9(1),ENDDAY                                                 
         MVC   DMCB+10(1),ROTATDAY                                              
         MVC   DMCB+11(1),ORBSTDAY                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),DATCON                                                   
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),PERVERT                                               
         LA    RE,WORK                                                          
         ST    RE,DMCB+12                                                       
*                                                                               
         GOTO1 VREDARTK,DMCB                                                    
         BNE   DISTNO              BUY DELETED, EXIT                            
*                                                                               
         B     DISTYES                                                          
         EJECT                                                                  
*        GETELN R6,DATADISP,ELCODE,3                                            
         EJECT                                                                  
         LTORG                                                                  
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
         CLC   DE1OFF(2),0(R4)                                                  
         BNE   COFFNO                                                           
         B     COFFYES                                                          
*                                                                               
COFF40   DS    0H                                                               
         CLC   RDARKAOF,DE1OFF     NOT FOUND, USE AGENCY OFFICE                 
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
***********************************************************************         
*VALIDATE REP REQUEST TO REMOVE AN ORDER                                        
*  USES:                                                                        
*    BYTE - TO STORE CURRENT SYSTEM FOR SWITCHING BACK FROM CONTROL             
*                                                                               
*    WORK -  +00 XL1  ERROR CODE AFTER SWITCH                                   
*                      0 - RETURN CC EQUAL                                      
*                      1 - DIE                                                  
*                      2 - RETURN CC NOT EQUAL                                  
*            +01 CL6  MEDIA/STATION OF DARE ORDER                               
*            +10 CL10 ALPHA USERID                                              
*            +20 CL3  3 CHARACTER REP CODE                                      
*                                                                               
***********************************************************************         
VALREM   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         MVI   BYTE,0                                                           
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARBSTS,C'C'                                                    
         BE    VALREM10            RECALLED - OK, NO BIT CHECKS                 
*                                                                               
         CLI   RDARBSTS,C'R'                                                    
         BNE   VREMNO              NOT REJECTED - INVALID ACTION                
*                                                                               
         TM    RDARMISC,X'20'                                                   
         BO    VREMNO              NOT DARE STATUS - INVALID ACTION             
*                                                                               
VALREM10 DS    0H                                                               
         MVC   WORK+1(L'RDARKSTA),RDARKSTA                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   BYTE,FASYS          CONNECTED SYSTEM                             
         DROP  R1                                                               
*                                                                               
         CLI   BYTE,X'0A'          ARE WE ALREADY IN CONTROL SYSTEM?            
         BE    VALREM20            YES                                          
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALREM20 DS    0H                                                               
*                                                                               
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,TWAORIG                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,AIO                   
         CLI   8(R1),0                                                          
         BNE   VREMDIE                                                          
*                                                                               
         L     R6,AIO                                                           
         LA    R6,CTIDATA-CTIREC(R6)                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,FRSTEL4                                                       
         BNE   VREMDIE                                                          
*                                                                               
         MVC   WORK+10(10),2(R6)                                                
         OC    WORK+10(10),SPACES                                               
*                                                                               
         LA    RE,REPIDS                                                        
VALREM22 DS    0H                                                               
         CLI   0(RE),X'FF'         EOT?                                         
         BE    VREMDIE             YES                                          
         ZIC   RF,14(RE)                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   15(0,RE),WORK+10    FOUND ID?                                    
         BE    VALREM24            YES                                          
         LA    RE,L'REPIDS(RE)                                                  
         B     VALREM22            NO - NEXT REPID                              
*                                                                               
VALREM24 DS    0H                                                               
         MVC   WORK+20(3),0(RE)                                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING STAKEYD,RE                                                       
         MVI   STAKSYS,STAKSYSQ    BUILD KEY...                                 
         MVI   STAKTYP,STAKTYPQ                                                 
         MVC   STAKMEDA(1),WORK+5  MEDIA                                        
         MVC   STAKSTIN(5),WORK+1  STATION                                      
         DROP  RE                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEYSAVE,KEY               
         CLI   DMCB+8,0                                                         
         BNE   VREMDIE                                                          
         CLC   KEY(L'STAKEY),KEYSAVE                                            
         BNE   VREMDIE                                                          
*                                                                               
         LA    R0,KEY+(STAKDA-STAKEYD)                                          
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',(R0),AIO                  
         CLI   DMCB+8,0                                                         
         BNE   VREMDIE                                                          
*                                                                               
         L     R6,AIO                                                           
         LA    R6,STAFSTEL-STAKEYD(R6)                                          
         MVI   ELCODE,STAREPCQ                                                  
         USING STAREPD,R6                                                       
         BAS   RE,FRSTEL4                                                       
         BNE   VREMDIE                                                          
*                                                                               
         CLC   STAREPCR,WORK+20    CURRENT REP?                                 
         BE    VREMNEQ             YES - CAN'T REMOVE                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,WORK+5)                                    
         CP    STAREPED,WORK+5(4)  PAST EFFECTIVE DATE OF REP SWITCH?           
         BNL   VREMNEQ             NO                                           
*                                                                               
         B     VALREM30                                                         
*                                                                               
VREMDIE  DS    0H                                                               
         MVI   WORK,1                                                           
         B     VALREM30                                                         
*                                                                               
VREMNEQ  DS    0H                                                               
         MVI   WORK,2                                                           
*                                                                               
VALREM30 DS    0H                                                               
         CLI   BYTE,X'0A'          WERE WE ALREADY IN CONTROL SYSTEM?           
         BE    VALREM40                                                         
         MVC   DMCB(1),BYTE        NO -- SWITCH BACK TO OLD SYSTEM              
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALREM40 DS    0H                                                               
         CLI   WORK,0              EXIT EQ                                      
         BE    VREMYES                                                          
         CLI   WORK,2              EXIT NEQ                                     
         BE    VREMNO                                                           
         DC    H'0'                                                             
VREMYES  CR    RB,RB                                                            
         B     *+6                                                              
VREMNO   LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
         GETELN R6,DATADISP,ELCODE,4                                            
         LTORG                                                                  
*****************************                                                   
       ++INCLUDE DDDARETAB          DARE 3 CHARACTER REP CODES                  
         EJECT                                                                  
***********************************************************************         
*  MARK CONTRACT DR ELEMENT FOR REMOVED DARE ORDER                              
***********************************************************************         
MARKCON  NTR1  BASE=*,LABEL=*                                                   
         OC    CCONNUM,CCONNUM                                                  
         BZ    MKCONX                                                           
*                                                                               
*        OC    DRVDATE,DRVDATE     SPECIAL CASE, CONTRACT HAS BEEN              
*        BZ    MKCONX              PURGED OUT OF THE SYSTEM                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LA    R6,RCONELEM-RCONREC(R6)                                          
MKCON02  CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO ELEMENT, ODD                              
         CLI   0(R6),X'1D'         DARE AGENCY ORDER ELEMENT?                   
         BE    MKCON10             YES                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MKCON02                                                          
*                                                                               
MKCON10  DS    0H                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       SAVE THEN DELETE ELEMENT                     
         MVI   ELCODE,X'1D'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING RCONDREL,R6                                                      
         MVI   RCONDRLN,RCONDL2Q   ALWAYS NEW LENGTH                            
         OI    RCONDRF2,X'08'      SET ORDER REMOVED FLAG                       
         DROP  R6                                                               
*                                                                               
*        GOTO1 ADDELEM             LEAVE X'1D' DELETED (8/3/02 SKUI)            
         GOTO1 PUTREC                                                           
*                                                                               
MKCONX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UNDARE MAKEGOOD RECORDS, IF ANY                                               
* CONTRACT RECORD SHOULD BE IN AIO AREA                                         
***********************************************************************         
PROCMKG  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
MGKEYD   USING RMKGKEY,KEY                                                      
         MVI   MGKEYD.RMKGKTYP,X'11'                                            
         MVC   MGKEYD.RMKGKREP,AGENCY                                           
         MVC   MGKEYD.RMKGKOFF,RCONKOFF                                         
         MVC   MGKEYD.RMKGKSTA,RCONKSTA                                         
         PACK  WORK(1),CCONNUM+3(1) REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),CCONNUM+2(1)                                           
         PACK  WORK+2(1),CCONNUM+1(1)                                           
         PACK  WORK+3(1),CCONNUM(1)                                             
         MVC   MGKEYD.RMKGKCON,WORK                                             
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     PMKG20                                                           
*                                                                               
PMKG10   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PMKG20   DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   PMKGX                                                            
         OC    MGKEYD.RMKGKPLN(6),MGKEYD.RMKGKPLN                               
         BNZ   PMKG10                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         MVI   RMKGSFG1,0                                                       
         XC    RMKGDARN,RMKGDARN                                                
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 PUTREC                                                           
         B     PMKG10                                                           
*                                                                               
PMKGX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE - ORDER FROM HEADER TO TRAILER                                         
***********************************************************************         
DELORD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),RDARKEY                                     
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
DELO10   DS    0H                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   DELO20                                                           
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OI    RDARCNTL,X'80'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
                                                                                
         GOTO1 SEQ                                                              
                                                                                
         B     DELO10                                                           
*                                                                               
* DELETE ACTIVE/CONFIRM ORDER AS WELL                                           
*                                                                               
DELO20   DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         CLI   KEY,X'41'                                                        
         BE    DELO25                                                           
         MVI   KEY,X'41'                                                        
         B     DELO28                                                           
*                                                                               
DELO25   DS    0H                                                               
         MVI   KEY,X'51'                                                        
*                                                                               
DELO28   DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
DELO30   DS    0H                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   DELO40                                                           
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OI    RDARCNTL,X'80'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
                                                                                
         GOTO1 SEQ                                                              
         B     DELO30                                                           
*                                                                               
DELO40   DS    0H                                                               
         CLC   =C'CONFIRM',DE1HDLN                                              
         BNE   DELOX                                                            
*                                                                               
* DDS CONFIRM ACTION?                                                           
*                                                                               
DELO200  DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DELO210  DS    0H                                                               
         MVC   WORK(27),KEY        SAVE FOR SEQUENCE                            
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVI   RDARKTYP,X'51'                                                   
         NI    RDARCNTL,X'FF'-X'80' MARK UNDELETE                               
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   KEY(27),WORK                                                     
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 SEQ                                                              
         CLC   KEY(RDARKRT-RDARKEY),WORK                                        
         BE    DELO210                                                          
*                                                                               
DELOX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE CURRENT DARE ORDER TO LAST SENT DARE ORDER                            
***********************************************************************         
COMPORD  NMOD1 0,**COMP**,RR=R4                                                 
         L     RC,0(R1)                                                         
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R5,SYSSPARE                                                      
         ST    R4,RELO3                                                         
*                                                                               
* CHECK FLAG TO SEE IF COMPARISONS ARE ALREADY DONE, IF SO, SKIP.               
*                                                                               
         XC    MISCFLG2,MISCFLG2                                                
         NI    BITFLAG,X'FF'-BFREVNEW                                           
         GOTO1 =A(CHKHDR),RR=RELO3  CHK TO SEE IF WE NEED TO COMPARE            
         BNE   COMPORDX             NO!                                         
*                                                                               
COMP0030 DS    0H                   YES                                         
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),AGYVKEY                                     
K        USING RDARREC,KEY                                                      
         MVI   K.RDARKRT,X'40'      GET BUY RECORDS                             
         DROP  K                                                                
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  DO NOT READ FOR DELETE                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'RDARKEY-(RDARKRT-RDARKEY)),KEY                         
         BNE   COMPORDX                                                         
*                                                                               
COMP0050 DS    0H                                                               
         CLI   KEY+RDARKSRT-RDARREC,X'00'                                       
         BE    COMP0100             BUY HEADER?                                 
         GOTO1 SEQ                  LOOP                                        
         B     COMP0050             UNTIL A BUY HEADER IS FOUND                 
*                                                                               
COMP0100 DS    0H                                                               
         MVC   SVBUYKEY,KEY         SAVE BUY HEADER KEY                         
         GOTO1 =A(PREPCMT),RR=RELO3 PREPARE AIO3 FOR WRITING COMT ELT           
*                                                                               
         MVC   KEY,SVBUYKEY                                                     
         GOTO1 HIGH                 RE-READ THE HEADER REC                      
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC AIO->AIO2->BUY HEADER                                     
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   COMPORDX             FOR NOW, JUST EXIT                          
*                                                                               
         USING RDARBYEL,R6                                                      
         TM    RDARBYDL,X'40'       IF HARD DELETE, DO NOT COMPARE              
         BO    COMP0750             READ NEXT RECORD                            
         TM    RDARBYDL,X'80'       IF SOFT DELETE, SAVE DATE/SPOT              
         BZ    COMP0105             FROM THE SHADOW AND SKIP ALL                
*                                   OTHER COMPARISONS                           
         OI    MISCFLG2,MF2SFTDL    SOFT DELETE = BUYLINE CANCELLED             
*                                                                               
COMP0105 DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK              SAVE OFF THE TWO ELT FOR COMPARING          
*                                                                               
         ZIC   R4,1(R6)                                                         
         AR    R3,R4                                                            
         BCTR  R4,0                  -------------------                        
         EX    R4,*+8               | CURR 01 | CURR 20 |                       
         B     *+10                  -------------------                        
         MVC   WORK(0),0(R6)                                                    
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   COMP0110                                                         
*                                                                               
         ZIC   R4,1(R6)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)                                                    
*                                                                               
COMP0110 DS    0H                                                               
         MVI   KEY+1,X'01'                                                      
         GOTO1 =A(MODKEY),RR=RELO3  MODIFY KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    COMP0120                                                         
*                                                                               
         GOTO1 =A(ADDCHGEL),DMCB,(=AL1(RDARNBYQ),=AL1(1)),1,RR=RELO3            
         B     COMP0630             NO MATCH SHADOW, THIS IS A NEW BUY          
*                                                                               
COMP0120 DS    0H                   AIO2->PREVIOUS BUY HEADER REC               
         GOTO1 GETREC                                                           
*                                                                               
COMP0130 DS    0H                   AIO2->PREVIOUS BUY HEADER REC               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'         COMPARE 01 ELT                              
         BRAS  RE,GETEL                                                         
         BNE   COMP0500                                                         
*                                                                               
PREV     USING RDARBYEL,R6                                                      
CURR     USING RDARBYEL,WORK                                                    
         PRINT GEN                                                              
*                                  SET UP ROTAT START/END DAY                   
         MVI   STARTDAY,0          CLEAR START/END DAYS OF WEEK                 
         MVI   ENDDAY,0                                                         
         GOTO1 =A(STARTEN1),DMCB,PREV.RDARBYRO,MYWORK2,RR=RELO3                 
*                                                                               
         TM    MISCFLG2,MF2SFTDL    SOFT DELETE?                                
         BZ    COMP0140                                                         
*                                   YES, SKIP ALL COMPARISONS                   
         GOTO1 =A(SVALLDT),DMCB,RDARDATQ,RR=RELO3                               
         B     COMP0630             SAVE ALL THE DATE/SPOT FROM SHADOW          
*                                                                               
COMP0140 DS    0H                   DAY/TIME                                    
         MVC   MYWORK2(DAYTIMQ),CURR.RDARBYRO                                   
         MVC   MYWORK2+DAYTIMQ(DAYTIMQ),PREV.RDARBYRO                           
         MVC   PREROT,PREV.RDARBYRO                                             
         GOTO1 =A(COMPDAY),RR=RELO3                                             
*                                                                               
COMP0150 DS    0H                   LENGTH                                      
         CLC   CURR.RDARBYSL(L'RDARBYSU+L'RDARBYSL),PREV.RDARBYSL               
         BE    COMP0250                                                         
         GOTO1 =A(ADDCHGEL),DMCB,(=AL1(RDARLENQ),PREV.RDARBYSL),       >        
               L'RDARBYSL+L'RDARBYSU,RR=RELO3                                   
*                                                                               
COMP0250 DS    0H                   PROGRAM                                     
         CLC   CURR.RDARBYPN,PREV.RDARBYPN                                      
         BE    COMP0500                                                         
         LA    R1,PREV.RDARBYPN+L'RDARBYPN-1  GET TO THE END OF INPUT           
         CLI   0(R1),X'40'                                                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R2,PREV.RDARBYPN     LENGTH OF NET CHARACTER INPUT               
         SR    R1,R2                                                            
         AHI   R1,1                                                             
         ST    R1,DMCB+4                                                        
         GOTO1 =A(ADDCHGEL),DMCB,(=AL1(RDARPRGQ),PREV.RDARBYPN),       >        
               RR=RELO3                                                         
*                                                                               
         DROP  CURR                                                             
         DROP  PREV                                                             
*                                                                               
COMP0500 DS    0H                   COMPARE 20 ELT                              
         LA    R3,WORK                                                          
         ZIC   RF,WORK+1            BUMP R7 TO CURR 20 ELT                      
         AR    R3,RF                                                            
         L     R6,AIO2                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   COMP0600                                                         
*                                                                               
PREV     USING RDARBMEL,R6          DEMO                                        
CURR     USING RDARBMEL,R3                                                      
         CLC   CURR.RDARBDM1,PREV.RDARBDM1                                      
         BE    COMP0600                                                         
         GOTO1 =A(ADDCHGEL),DMCB,(=AL1(RDARDEMQ),PREV.RDARBDM1),       >        
               L'RDARBDM1,RR=RELO3                                              
*                                                                               
         PRINT NOGEN                                                            
COMP0600 DS    0H                   DATES                                       
*                                                                               
         GOTO1 =A(COMDATE),RR=RELO3  RATE/SPOT, SPOT/WK GOT SAVED OFF           
*                                     IN SUBROUTINE COMDATE                     
         BNE   COMP0630              IF DUPLICAT START DATE,                    
*                                       DAILY/WEEKLY INCONSISTENT               
*                                    THEN SKIP RATE, SPOT/WK COMPARE            
*                                    RATE, SPOT/WK COMPARISON                   
         GOTO1 =A(COMRATE),RR=RELO3  COMPARE RATE/SPOT                          
*                                                                               
COMP0620 DS    0H                   SPOT/WK                                     
         CLC   CURSPTW,PRVSPTW                                                  
         BE    COMP0630                                                         
         GOTO1 =A(ADDCHGEL),DMCB,(=AL1(RDARSPTQ),PRVSPTW),L'PRVSPTW,   >        
               RR=RELO3                                                         
*                                                                               
COMP0630 DS    0H                                                               
         NI    BITFLAG3,X'FF'-B3DAILYB                                          
*                                                                               
         L     R1,AIO3                                                          
         MVC   KEY,0(R1)                                                        
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
*        CLC   KEYSAVE(27),KEY                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
COMP0640 DS    0H                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
*                                                                               
COMP0650 DS    0H                                                               
         TM    MISCFLG2,MF2ADDE     IF THERE IS ELT ADDED                       
         BO    COMP0680             THEN DO ADDREC OR PUTREC                    
         TM    MISCFLG2,MF2CMTP     OR NO ELT ADDED, AND THERE WAS              
         BZ    COMP0750             NO COMMENT REC, THEN PROCESS NEXT           
*                                                                               
         L     R1,AIO3              OTHERWISE, THERE WAS DIFF, BUT              
         USING RDARREC,R1           NOW THEY ARE GONE, WE NEED TO MARK          
         CLI   RDARELEM,X'00'       THE RECORD FOR DELETION IF X'10'            
         BE    *+14                                                             
         MVC   AIO,AIO3                                                         
         B     COMP0700                                                         
*                                                                               
         OI    KEY+27,X'80'                                                     
         L     R6,AIO2                                                          
         OI    29(R6),X'80'                                                     
         MVC   AIO,AIO2             MARK FOR DEL AND WRITE ORIGINAL             
         B     COMP0700             RECORD BACK                                 
*                                                                               
COMP0680 DS    0H                                                               
         TM    MISCFLG2,MF2CMTP     IF COMMENT REC PRESENT                      
         BO    COMP0685             THEN DO PUTREC                              
         GOTO1 =A(BLD01),RR=RELO3                                               
         MVC   AIO,AIO3                                                         
         GOTO1 ADDREC               OTHERWISE, ADDREC                           
         B     COMP0750             READ NEXT BUY HEADER                        
COMP0685 DS    0H                                                               
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 =A(BLD01),RR=RELO3                                               
         L     R6,AIO3                                                          
         NI    29(R6),X'FF'-X'80'                                               
         MVC   AIO,AIO3                                                         
*                                                                               
COMP0700 DS    0H                                                               
         GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
*                                                                               
COMP0750 DS    0H                   PROCESS NEXT BUYLINE                        
         XC    MISCFLG2,MISCFLG2                                                
K        USING RDARREC,KEY          GET NEXT BUY LINE HEADER                    
         XC    KEY,KEY                                                          
         MVC   KEY,SVBUYKEY                                                     
         MVI   KEY+26,X'FF'                                                     
         DROP  K                                                                
         NI    DMINBTS,X'FF'-X'08'  DO NOT READ FOR DELETE                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(25),KEY                                                  
         BE    COMP0050                                                         
*                                                                               
COMP0800 DS    0H                   RESTORE SEQUENCE                            
*                                                                               
         GOTO1 =A(UPDTFLG),RR=RELO3 UPDATE FLAG IN AGY HEADER REC               
*                                                                               
         MVC   KEY,AGYVKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1             RESTORE RECORD                              
         GOTO1 GETREC                                                           
*                                                                               
COMPORDX DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE DATE AND SPOT COUNT                                                   
***********************************************************************         
COMDATE  NTR1  BASE=*,LABEL=*                                                   
* MAKE START DATE AS A MONDAY DATE                                              
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(2,FLGSTDAT),(0,MYWORK2)                             
         GOTO1 GETDAY,DMCB,MYWORK2,MYWORK2+6                                    
         ZIC   RF,DMCB                                                          
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    COMD0050            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(RF)                                
         MVC   MYWORK2(6),MYWORK2+6                                             
         GOTO1 DATCON,DMCB,(0,MYWORK2),(2,FLGSTDAT)                             
*                                                                               
COMD0050 DS    0H                                                               
         PRINT NOGEN                                                            
         NI    BITFLAG3,X'FF'-B3CSTOVR-B3DAILYB                                 
         XC    BUYFLAG1,BUYFLAG1                                                
*                                                                               
         GOTO1 =A(BLDGRID),DMCB,(X'00',CURGRID),RR=RELO3                        
         BNE   COMD0500             DUPLICATE START DATE                        
         GOTO1 =A(BLDGRID),DMCB,(X'01',PRVGRID),RR=RELO3                        
         BNE   COMD0500             DUPLICATE START DATE                        
*                                                                               
COMD0080 DS    0H                                                               
         LA    R2,CURGRID                                                       
         LA    R3,PRVGRID                                                       
         LHI   R7,TABWK#            TABWK# = 53                                 
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM+4            R6->FIRST DATE DIFFERENCE                   
         MVI   ELEM,X'10'                                                       
         MVI   ELEM+2,RDARDATQ      ELEM SUBCODE                                
         MVC   ELEM+3,BUYFLAG1      COPY BUY FLAG                               
*         LA    R6,1(R6)             SKIP COMPARING                             
*                                                                               
         CLI   BUYFLAG1,BYFDAILY    BOTH PREV/CURR ARE DAILY?                   
         BE    COMD0100             YES, COMPARE                                
         CLI   BUYFLAG1,BYFWKLY     BOTH PREV/CURR ARE WEEKLY?                  
         BE    COMD0100             YES, COMPARE                                
*                                   DO NOT COMPARE WKLY BUY WITH DAILY          
         LA    R4,ELEM                                                          
         SR    R6,R4                STICK LENGTH IN                             
         STC   R6,ELEM+1                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,0                        
         OI    MISCFLG2,MF2ADDE                                                 
         B     COMDTNO              SKIP COMPARING RATE AS WELL                 
*                                                                               
COMD0100 DS    0H                                                               
         SR    R0,R0                IF -1, MAKE IT 0                            
         CLI   0(R2),X'FF'                                                      
         BE    *+8                                                              
         IC    R0,0(R2)                                                         
*                                                                               
         SR    R1,R1                                                            
         CLI   0(R3),X'FF'                                                      
         BE    *+8                                                              
         IC    R1,0(R3)                                                         
*                                                                               
         SR    R0,R1                CALCULATE SPOT DIFFERENCE                   
         BNZ   COMDNXT1             LOOP UNTIL WE FIND DIFF IN SPOT#            
         LA    R2,ROWSIZE(R2)                                                   
         LA    R3,ROWSIZE(R3)                                                   
         BCT   R7,COMD0100                                                      
         B     COMDATX              NO DIFFERENCE FOUND                         
COMDNXT1 DS    0H                                                               
         STH   R0,LASTDIF                                                       
         STH   R7,WKINDX                                                        
         BCTR  R7,0                                                             
         LA    R2,ROWSIZE(R2)                                                   
         LA    R3,ROWSIZE(R3)                                                   
         B     COMD0150                                                         
*                                                                               
COMD0150 DS    0H                                                               
         SR    R0,R0                IF -1, MAKE IT 0                            
         CLI   0(R2),X'FF'                                                      
         BE    *+8                                                              
         IC    R0,0(R2)                                                         
*                                                                               
         SR    R1,R1                                                            
         CLI   0(R3),X'FF'                                                      
         BE    *+8                                                              
         IC    R1,0(R3)                                                         
*                                                                               
         SR    R0,R1                                                            
         CH    R0,LASTDIF                                                       
         BE    COMDNXT              SAME AS LAST COUNT                          
         OC    LASTDIF,LASTDIF      IF LAST DIFF = 0                            
         BZ    COMD0200                                                         
*                                                                               
         USING RDARCHSD,R6          WRITE THE DIFF TO ELT                       
         MVC   RDARCHSP,LASTDIF     LAST DIFF                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLGSTDAT),(0,MYWORK2)                             
         LHI   R4,TABWK#                                                        
         SH    R4,WKINDX                                                        
         TM    BITFLAG3,B3DAILYB    DAILY BUY?                                  
         BO    *+8                  YES,DON'T TIMES 7                           
         MHI   R4,7                                                             
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(R4)                                
*                                                                               
         TM    BITFLAG3,B3DAILYB    DAILY BUY?                                  
         BZ    COMD0180             YES,DON'T TIMES 7                           
         MVC   MYWORK2(6),MYWORK2+6                                             
         B     COMD0185                                                         
COMD0180 DS    0H                                                               
         ZIC   R4,STARTDAY                                                      
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,MYWORK2+6,MYWORK2,(R4)                                
COMD0185 DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,MYWORK2),(2,RDARCHSD)                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLGSTDAT),(0,MYWORK2)                             
         LHI   R4,TABWK#                                                        
         SR    R4,R7                                                            
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         BCTR  R4,0                                                             
         TM    BITFLAG3,B3DAILYB                                                
         BO    *+8                  YES, DON'T TIMES 7                          
         MHI   R4,7                                                             
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(R4)                                
*                                                                               
         TM    BITFLAG3,B3DAILYB    DAILY BUY?                                  
         BZ    COMD0190             YES,DON'T TIMES 7                           
         MVC   MYWORK2(6),MYWORK2+6                                             
         B     COMD0195                                                         
COMD0190 DS    0H                                                               
         ZIC   R4,ENDDAY                                                        
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,MYWORK2+6,MYWORK2,(R4)                                
COMD0195 DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,MYWORK2),(2,RDARCHED)                             
*                                                                               
         LA    R6,6(R6)             BUMP R6                                     
*                                                                               
COMD0200 DS    0H                                                               
         STH   R7,WKINDX                                                        
         STH   R0,LASTDIF                                                       
*                                                                               
COMDNXT  DS    0H                                                               
         LA    R2,ROWSIZE(R2)                                                   
         LA    R3,ROWSIZE(R3)                                                   
         BCT   R7,COMD0150                                                      
*                                                                               
COMD0400 DS    0H                                                               
         LA    R4,ELEM                                                          
         SR    R6,R4                STICK LENGTH IN                             
         STC   R6,ELEM+1                                                        
         B     COMD0600                                                         
*                                                                               
COMD0500 DS    0H                   DUP START DAY                               
         GOTO1 =A(SVALLDT),DMCB,RDARDT2Q,RR=RELO3                               
         B     COMDTNO              SAVE ALL DATE/SPOTS                         
*                                                                               
COMD0600 DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,0                        
         OI    MISCFLG2,MF2ADDE                                                 
COMDTYES SR    RC,RC                                                            
COMDTNO  LTR   RC,RC               SET CONDITION CODE                           
COMDATX  DS    0H                                                               
         DROP  R6                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* BLDGRID: BUILD GRID                                                           
* P1: BYTE 0     : X'00' = CURRENT DARE BUY REC                                 
*                  ELSE 51 CONFIRM REC OR 4101 SHADOW REC                       
*     BYTES 1-3  : A(GRID)                                                      
*     SET CC WHEN DUPLICATE DATE ORBIT IS FOUND                                 
****************************************************************                
BLDGRID  NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)             R4->A(GRID)                                 
         ST    R4,FULL                                                          
K        USING RDARKEY,KEY          READ BUY DETAIL REC                         
         MVC   KEY(27),SVBUYKEY                                                 
         MVI   KEY+1,X'00'                                                      
         CLI   0(R1),X'00'                                                      
         BE    BLDG0010                                                         
         GOTO1 =A(MODKEY),RR=RELO3                                              
BLDG0010 DS    0H                                                               
         MVI   K.RDARKSRT,X'30'                                                 
         DROP  K                                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   BLDGRIDX                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLDGRIDX                                                         
         USING RDARBDEL,R6                                                      
*                                                                               
         TM    RDARBDFL,X'80'       DAILY?                                      
         BZ    BLDG0015                                                         
         TM    BITFLAG3,B3DAILYB                                                
         BO    BLDG0012                                                         
         GOTO1 =A(GETSTDT),RR=RELO3 GET DAILY BUY START DATE                    
BLDG0012 DS    0H                                                               
         OI    BITFLAG3,B3DAILYB                                                
*                                                                               
BLDG0015 DS    0H                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLDGRIDX                                                         
         USING RDARBUEL,R6                                                      
*                                                                               
         CLC   KEY(2),=XL2'4100'   SAVE RATE/SPOT                               
         BE    BLDG0020            SAVE SPOT/WK                                 
         MVC   PRVSPTW,RDARBUSW                                                 
         B     BLDG0030                                                         
BLDG0020 DS    0H                                                               
         MVC   CURSPTW,RDARBUSW                                                 
*                                                                               
BLDG0030 DS    0H                                                               
         LR    R7,R4                                                            
         LHI   RF,L'CURGRID                                                     
         AR    R7,RF                R7->TABLE BOUNDRY                           
*                                                                               
         LR    R2,R4                                                            
         LHI   R3,L'CURGRID                                                     
         LA    RE,=X'FF'                                                        
         SR    RF,RF                                                            
         ICM   RF,8,=X'FF'                                                      
         MVCL  R2,RE                                                            
         B     BLDG0050                                                         
*                                                                               
BLDG0040 DS    0H                                                               
         CLC   RDARBU$$,LASTRAT                                                 
         BE    BLDG0050                                                         
         OI    BITFLAG3,B3CSTOVR                                                
*                                                                               
BLDG0050 DS    0H                                                               
         XC    ELEM,ELEM                                                        
         XC    MYWORK2,MYWORK2                                                  
         L     R4,FULL                                                          
         GOTO1 DATCON,DMCB,(2,FLGSTDAT),(5,ELEM)                                
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(5,ELEM+9)                              
         MVI   ELEM+8,C'-'                                                      
         GOTO1 PERVAL,DMCB,(17,ELEM),MYWORK2                                    
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,MYWORK2                                                  
         LA    R2,WKD.PVALNWKS                                                  
         TM    BITFLAG3,B3DAILYB                                                
         BZ    *+8                                                              
         LA    R2,WKD.PVALNDYS     DAY IF DAILY BUYS                            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,0(R2)          # WEEKS(DAY IF DAILY) ROUNDED UP             
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         MHI   R1,ROWSIZE                                                       
         AR    R4,R1               BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
BLDG0100 DS    0H                                                               
         ZIC   R1,RDARBUWK                                                      
*                                                                               
BLDG0200 DS    0H                                                               
         CR    R4,R7                                                            
         BL    *+12                                                             
         MVI   BUYFLAG1,BYFTABOV    TABLE OVERFLOW                              
         B     BLDGDYES                                                         
*                                                                               
         CLI   0(R4),X'FF'                                                      
         BE    BLDG0230                                                         
         OC    0(1,R4),0(R4)                                                    
         BNZ   BLDGDNO             DUPLICATE START DATE,EXIT WITH CC            
BLDG0230 DS    0H                                                               
         MVC   0(1,R4),RDARBUSW    SPOT/WK                                      
         MVC   1(4,R4),RDARBU$$    COST                                         
*                                                                               
BLDG0250 DS    0H                                                               
         LA    R4,ROWSIZE(R4)      BUMP TO NEXT WEEK (DAY IF DAILY)             
         LTR   R1,R1                                                            
         BZ    BLDG0300                                                         
         BCT   R1,BLDG0200         PROCESS FOR SPECIFIED NUMBER OF WKS          
*                                                                               
BLDG0300 DS    0H                  INCASE OF MULTIPLE EFFECTIVE DATES           
         MVC   LASTRAT,RDARBU$$                                                 
         BRAS  RE,NEXTEL                                                        
         BE    BLDG0040                                                         
         DROP  R6                                                               
*                                                                               
BLDGDYES SR    RC,RC                                                            
BLDGDNO  LTR   RC,RC               SET CONDITION CODE                           
         B     EXIT3                                                            
BLDGDLO  LNR   RC,RC                                                            
BLDGRIDX DS    0H                                                               
         B     EXIT3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************                      
* COMPARE DAY/TIME                                                              
*                                                                               
***********************************************************                     
COMPDAY  NTR1  BASE=*,LABEL=*                                                   
* CHK TO SEE IF THERE IS ORBIT FIRST                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'10'                                                       
         MVI   ELEM+2,RDARDTMQ                                                  
         NI    MISCFLG2,X'FF'-MF2ORBIT                                          
         GOTO1 =A(BLDOGRID),DMCB,(X'00',CURGRID),RR=RELO3                       
         BE    COMDY050             THERE IS ORBIT                              
         MVC   CURGRID(DAYTIMQ),MYWORK2  NO, USE THE DAY/TIME IN HEADER         
         MVI   CURGRID+DAYTIMQ,X'FF'                                            
         MVC   OBTNUM0,=H'1'                                                    
*                                                                               
COMDY050 DS    0H                                                               
         GOTO1 =A(BLDOGRID),DMCB,(X'01',PRVGRID),RR=RELO3                       
         BE    COMDY150                                                         
         MVC   PRVGRID(DAYTIMQ),MYWORK2+12                                      
         MVI   PRVGRID+DAYTIMQ,X'FF'                                            
         MVC   OBTNUM1,=H'1'                                                    
*                                                                               
COMDY150 DS    0H                                                               
         CLC   OBTNUM1,OBTNUM0                                                  
         BNE   COMDY250                                                         
*                                                                               
         LA    R4,CURGRID                                                       
         LA    R6,PRVGRID                                                       
*                                                                               
COMDY200 DS    0H                                                               
         CLI   0(R4),X'FF'                                                      
         BE    COMDYYES                                                         
         CLC   0(DAYTIMQ,R4),0(R6)                                              
         BNE   COMDY250                                                         
         LA    R4,DAYTIMQ(R4)                                                   
         LA    R6,DAYTIMQ(R6)                                                   
         B     COMDY200                                                         
*                                                                               
COMDY250 DS    0H                                                               
         LH    R4,OBTNUM1                                                       
         MHI   R4,DAYTIMQ                                                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+3(0),PRVGRID                                                
         AHI   R4,3                OVERHEAD                                     
         STC   R4,ELEM+1                                                        
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,0                        
         OI    MISCFLG2,MF2ADDE                                                 
*                                                                               
COMDYYES SR    RC,RC                                                            
COMDYNO  LTR   RC,RC               SET CONDITION CODE                           
COMDAYX  DS    0H                                                               
         B     EXIT3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* BLDOGRID: BUILD ORBIT GRID                                                    
* P1: BYTE 0     : X'00' = CURRENT DARE BUY REC                                 
*                  X'01' = SHADOW DARE BUY REC                                  
*     BYTES 1-3  : A(GRID)                                                      
****************************************************************                
BLDOGRID NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)             R4->A(GRID)                                 
         ST    R4,FULL                                                          
K        USING RDARKEY,KEY          READ BUY DETAIL REC                         
         L     R3,AIO2                                                          
         MVC   KEY,0(R3)                                                        
         GOTO1 =A(MODKEY),RR=RELO3                                              
         MVC   K.RDARKSTY(1),0(R1)  CURRENT/PRV RECORD                          
         MVI   K.RDARKSRT,X'10'     BUY ORBIT                                   
         DROP  K                                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   BLDODNO              NO ORBIT RECORD                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'        ORBIT ELEMENT                                
         BRAS  RE,GETEL                                                         
         BNE   BLDODNO                                                          
         USING RDAROEEL,R6                                                      
*                                                                               
         SR    R2,R2                                                            
         LHI   RF,L'CURGRID                                                     
         XCEF  (R4)                                                             
BLDO0050 DS    0H                                                               
         MVC   0(DAYTIMQ,R4),RDAROERO                                           
         LA    R4,DAYTIMQ(R4)                                                   
         LA    R2,1(R2)                                                         
BLDO0300 DS    0H                  INCASE OF MULTIPLE EFFECTIVE DATES           
         BRAS  RE,NEXTEL                                                        
         BE    BLDO0050                                                         
*                                                                               
         MVI   0(R4),X'FF'         END OF TABLE                                 
*                                                                               
         STH   R2,OBTNUM0                                                       
         CLI   KEY+1,X'01'                                                      
         BNE   *+8                                                              
         STH   R2,OBTNUM1          ORBIT COUNT                                  
         DROP  R6                                                               
*                                                                               
BLDODYES SR    RC,RC                                                            
BLDODNO  LTR   RC,RC               SET CONDITION CODE                           
BLDORIDX DS    0H                                                               
         B     EXIT3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
* SVALLDT: SAVE ALL THE DATE/SPOT ELEMENT (02) ELT FROM THE SHADOW              
*          GETS CALL WHEN A BUYLINE IS CANCELED, OR WHEN DUPLICATE              
*          START DATE BUY LINE IS DETECTED                                      
****************************************************************                
SVALLDT  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'10'                                                       
         MVC   ELEM+2(1),3(R1)                                                  
         CLI   3(R1),RDARDT2Q                                                   
         BNE   *+8                                                              
         OI    MISCFLG2,MF2SVCST    SAVE COST AS WELL                           
         LA    R2,ELEM+4                                                        
         USING RDARCHSD,R2                                                      
*                                                                               
K        USING RDARKEY,KEY          READ BUY DETAIL SHADOW RECORD               
         L     R3,AIO2                                                          
         MVC   KEY,0(R3)                                                        
         GOTO1 =A(MODKEY),RR=RELO3                                              
         MVI   K.RDARKSTY,X'01'     READ SHADOW                                 
         MVI   K.RDARKSRT,X'30'                                                 
         DROP  K                                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   SVALLDTX                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SVALLDTX                                                         
         USING RDARBUEL,R6                                                      
*                                                                               
SVALL100 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,MYWORK2)                             
         ZIC   R4,STARTDAY                                                      
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(R4)                                
         GOTO1 DATCON,DMCB,(0,MYWORK2+6),(2,RDARCHSD)                           
*                                                                               
         ZIC   R4,RDARBUWK                                                      
         BCTR  R4,0                                                             
         MHI   R4,7                                                             
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(R4)                                
         ZIC   R4,ENDDAY                                                        
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,MYWORK2+6,MYWORK2,(R4)                                
         GOTO1 DATCON,DMCB,(0,MYWORK2),(2,RDARCHED)                             
*                                                                               
         TM    MISCFLG2,MF2SVCST                                                
         BZ    SVALL150                                                         
         ZIC   R4,RDARBUSW                                                      
         STCM  R4,3,RDARCHSP        JUST SAVE THE SPOT COUNT                    
         B     SVALL180                                                         
SVALL150 DS    0H                                                               
         ZIC   R4,RDARBUSW          SAVE THE SPOT COUNT RELATIVE TO 0           
         SR    R3,R3                BECAUSE THE BUYLINE IS CANCELLED            
         SR    R3,R4                                                            
         STH   R3,HALF                                                          
         MVC   RDARCHSP,HALF                                                    
*                                                                               
SVALL180 DS    0H                                                               
         TM    MISCFLG2,MF2SVCST                                                
         BZ    SVALL200                                                         
         MVC   RDARCHCT,RDARBU$$                                                
         LA    R2,10(R2)                                                        
         B     *+8                                                              
SVALL200 DS    0H                                                               
         LA    R2,6(R2)                                                         
         BRAS  RE,NEXTEL                                                        
         BE    SVALL100                                                         
*                                                                               
         NI    MISCFLG2,X'FF'-MF2SVCST                                          
         LA    R4,ELEM                                                          
         SR    R2,R4                                                            
         STC   R2,ELEM+1                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,0                        
         OI    MISCFLG2,MF2ADDE                                                 
SVALLDTX DS    0H                                                               
         DROP  R2                                                               
         DROP  R6                                                               
         B     EXIT3                                                            
*RELO3    DS    A                                                               
*******************************************************                         
* GET DAILY BUY START DATE, TO SAVE STORAGE WHEN COMPARING                      
*******************************************************                         
GETSTDT  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(L'KEY),KEY                                                  
K        USING RDARKEY,KEY          READ BUY DETAIL REC                         
         MVC   KEY(27),SVBUYKEY                                                 
*                                                                               
         MVI   K.RDARKSRT,X'30'                                                 
         DROP  K                                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   GETSTDTX                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
GETSD015 DS    0H                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETSTDTX                                                         
         USING RDARBDEL,R6                                                      
         TM    RDARBDFL,X'80'                                                   
         BZ    GETSD020                                                         
         MVI   BUYFLAG1,BYFCRDLY                                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETSTDTX                                                         
         USING RDARBUEL,R6                                                      
*                                                                               
         MVC   FLGSTDAT,RDARBUSD                                                
         DROP  R6                                                               
*                                                                               
GETSD020 DS    0H                                                               
         GOTO1 =A(MODKEY),RR=RELO3                                              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   GETSTDTX                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
GETSD030 DS    0H                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETSTDTX                                                         
         USING RDARBDEL,R6                                                      
         TM    RDARBDFL,X'80'                                                   
         BZ    GST050               NOT DAILY                                   
*                                   PREVIOUS BUY IS DAILY                       
         CLI   BUYFLAG1,BYFCRDLY                                                
         BNE   GST030               CUR BUY NOT DAILY                           
         MVI   BUYFLAG1,BYFDAILY    BOTH ARE DAILY                              
         B     GST050                                                           
GST030   DS    0H                                                               
         MVI   BUYFLAG1,BYFPVDLY    ONLY PREVIOUS BUY IS DAILY                  
         DROP  R6                                                               
*                                                                               
GST050   DS    0H                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETSTDTX                                                         
         USING RDARBUEL,R6                                                      
*                                                                               
         CLC   FLGSTDAT,RDARBUSD                                                
         BL    *+10                                                             
         MVC   FLGSTDAT,RDARBUSD    USE THE EARLIER DATE                        
         DROP  R6                                                               
*                                                                               
         MVC   KEY,WORK                                                         
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
GETSTDTX DS    0H                                                               
         B     EXIT3                                                            
*******************************************************                         
* CHECK THE HEADER TO SEE IF WE NEED TO DO THE COMPARE                          
* ALSO DETERMINE WHICH RECORD WE SHOULD BE READING                              
*******************************************************                         
CHKHDR   NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY,AGYVKEY                                                      
         CLI   KEY,X'51'            ARE WE LOOKING AT A CF CONTRACT             
         BE    CHKHDNO              YES, DON'T NEED TO COMPARE                  
*                                                                               
CHKH050  DS    0H                                                               
         MVC   KEY,AGYVKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKHDNO                                                          
*                                                                               
         USING RDARCODE,R6                                                      
         TM    RDARMISC,X'01'       COMPARISONS ARE ONLY DONE ONCE              
         BO    CHKHDNO                                                          
*                                                                               
         TM    RDARMISC,X'80'       IS THIS A RESENT?                           
         BO    CHKH100              YES                                         
         CLI   RDARRNUM,0           NO, IF REV# = 0                             
         BE    CHKH100                                                          
         OI    BITFLAG,BFREVNEW     THIS IS A NEW REVISION                      
*                                   READ 51 FOR COMPARISON!                     
CHKH100  DS    0H                                                               
         MVC   FLGSTDAT,RDARESST    GET START DATE                              
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(MODKEY),RR=RELO3                                              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   CHKHDNO                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKHDNO                                                          
*                                                                               
         USING RDARCODE,R6                                                      
         CLC   FLGSTDAT,RDARESST                                                
         BL    *+10                                                             
         MVC   FLGSTDAT,RDARESST    USE THE EARLIER DATE                        
         DROP  R6                                                               
*                                                                               
CHKHDYES SR    RC,RC                                                            
CHKHDNO  LTR   RC,RC                SET CONDITION CODE                          
CHKHDX   DS    0H                                                               
         B     EXIT3                                                            
*RELO4    DS    A                                                               
***********************************************************************         
*    MODIFY KEY, IF REVISION NEW, READ 51, OR READ 4101 SHADOW                  
***********************************************************************         
MODKEY   NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY(2),=XL2'4101'                                                
         TM    BITFLAG,BFREVNEW                                                 
         BZ    MODKX                                                            
         MVC   KEY(2),=XL2'5100'                                                
MODKX    DS    0H                                                               
         B     EXIT3                                                            
         EJECT                                                                  
******************************************************                          
* PREPARE COMMENT RECORD, PUT COMMENT RECORD IN AIO3                            
* OR CREATES NEW COMMENT RECORD IF THERE ISN'T ANY                              
******************************************************                          
PREPCMT  NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO3                                                          
         LHI   RF,1000                                                          
         XCEF                                                                   
*                                                                               
         MVC   KEY,SVBUYKEY                                                     
K        USING RDARKEY,KEY                                                      
         MVI   K.RDARKSTY,X'00'     IS THERE A BUY COMMENT REC                  
         MVI   K.RDARKSRT,X'20'     IN THE NEW DARE ORDER                       
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    PREP0115             YES                                         
*                                                                               
         NI    MISCFLG2,X'FF'-MF2CMTP                                           
         L     R1,AIO3              NO, MOVE IN KEY, WILL DO ADDREC             
         MVC   0(27,R1),KEYSAVE                                                 
         B     PREPCMTX                                                         
*                                                                               
PREP0115 DS    0H                                                               
         OI    MISCFLG2,MF2CMTP     BUY COMMENT REC PRESENT                     
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'        PASS DELETED BACK AS WELL                   
         MVC   AIO,AIO3             AIO->AIO3->BUY COMMENT REC TO BE            
         GOTO1 GETREC ADDED                                                     
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'01'                                                     
PREPCMTX B     EXIT3                                                            
*******************************************************                         
* UPDATE FLAG IN HEADER, COMPARISONS ARE ONLY DONE ONCE                         
*******************************************************                         
UPDTFLG  NTR1  BASE=*,LABEL=*                                                   
         MVC   KEY,AGYVKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RDARCODE,R6                                                      
         OI    RDARMISC,X'01'       COMPARISONS ARE ONLY DONE ONCE              
         DROP  R6                                                               
         GOTO1 PUTREC                                                           
*                                                                               
UPDTFLGX DS    0H                                                               
EXIT4    DS    0H                                                               
         B     EXIT3                                                            
***********************************************************************         
*                                                                               
*   STARTEND:  CONVERTS ROTATION MATRIX AND START DAY TO ONE-BYTE               
*        START/END DAY, INSERTS INTO ADDRESS                                    
*                                                                               
***********************************************************************         
STARTEN1 NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(INPUT: ROTATION+START DAY)                 
         L     R3,4(R1)            A(RECEIVING FIELD)                           
*                                                                               
         ZIC   RF,7(R2)            ROTATION START DAY                           
         SLL   RF,28               STRIP OFF ZONE BITS                          
         SRL   RF,28               SHIFT START DAY BACK                         
         STC   RF,ROTATDAY         SAVE ROTATION START DAY                      
*                                                                               
         LA    R6,0(R2)            A(ROTATION FIELD)                            
         LR    RE,R6               CALCULATE END OF ROTATION FIELD              
         LA    RE,7(RE)                                                         
         AR    R6,RF               GET A(1ST DAY+1)                             
         BCTR  R6,0                BACK OFF TO A(1ST ENTRY)                     
         LR    RF,R6               SAVE A(1ST ENTRY)                            
*                                     FOR WHEN 1ST ENTRY IS ONLY ENTRY          
         LA    R0,7                SET LOOP CONTROL TO SIX DAYS                 
STE10040 EQU   *                                                                
         CR    R6,RE               END OF ROTATION FIELD REACHED?               
         BNE   STE10080            NO                                           
         LA    R6,0(R2)            YES  - GO BACK TO FIRST LOCATION             
STE10080 EQU   *                                                                
         CLI   0(R6),C' '          ARRAY POSITION USED?                         
         BE    STE10200            NO  - SKIP IT                                
         CLI   0(R6),0             ARRAY POSITION USED?                         
         BE    STE10200            NO  - SKIP IT                                
         CLI   STARTDAY,0          ANYTHING IN START DAY?                       
         BNE   STE10120            YES - DON'T REPLACE                          
         LA    R4,7                NO  - CALCULATE STARTDAY                     
         SR    R4,R0               SUBTRACT REMAINING DAYS                      
         ZIC   R1,ROTATDAY         OFFSET BY ROTATION START DAY                 
         AR    R4,R1                                                            
         CH    R4,=H'7'            WRAPAROUND?                                  
         BNH   STE10100            NO                                           
         SH    R4,=H'7'            YES - SUBTRACT 7                             
STE10100 EQU   *                                                                
         STC   R4,STARTDAY         SAVE CALCULATED STARTDAY                     
STE10120 EQU   *                                                                
         OC    0(1,R3),0(R3)       RECEIVING FIELD ENTRY MADE?                  
         BNZ   STE10160            YES - START DAY ENTERED                      
         SLL   R4,4                NO  - MOVE START DAY TO HIGH NYBBLE          
         STC   R4,0(R3)            INSERT INTO RECORD                           
STE10160 EQU   *                                                                
         LR    RF,R6               YES - SAVE NEW ARRAY POSITION                
STE10200 EQU   *                                                                
         LA    R6,1(R6)            BUMP TO NEXT ARRAY LOCATION                  
         BCT   R0,STE10040         GO BACK AND CHECK NEXT                       
         LA    R6,0(R2)            A(START OF ARRAY)                            
         BCTR  R6,0                BACK UP 1 POSITION                           
         SR    RF,R6               CALCULATED DISPLACEMENT                      
         STC   RF,ENDDAY           SAVE END DAY FOR EFF DATE SETTING            
         ZIC   RE,0(R3)            RETRIEVE START DAY                           
         AR    RF,RE               ADD START TO END                             
         STC   RF,0(R3)            PUT IT BACK IN RECORD                        
         LA    R6,0(R2)            COUNT NUMBER OF DAYS                         
         SR    RF,RF                                                            
         LA    R0,7                                                             
STE10240 EQU   *                                                                
         CLI   0(R6),C' '          DAY ACTIVE?                                  
         BE    STE10280            NO                                           
         CLI   0(R6),0             DAY ACTIVE?                                  
         BE    STE10280            NO                                           
         LA    RF,1(RF)            YES - ADD 1                                  
STE10280 EQU   *                                                                
         LA    R6,1(R6)            BUMP TO NEXT POSITION                        
         BCT   R0,STE10240         GO BACK AND CHECK NEXT                       
         C     RF,=F'1'            COUNT = 1?                                   
         BH    STE10320            NO  - HIGHER - EXIT                          
         BE    *+6                 YES - SET START=END IN RECORD                
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                     MEANS EMPTY ARRAY!!!                      
         ZIC   RF,0(R3)            RETRIEVE START/END DAY                       
         SLL   RF,28               DROP START DAY                               
         SRL   RF,24               MOVE END DAY BACK TO HI NYBBLE               
         NI    0(R3),X'0F'         CLEAR START DAY                              
         ZIC   RE,0(R3)            RETRIEVE 0/END DAY                           
         AR    RE,RF               ADD NEW START DAY                            
         STC   RE,0(R3)            MOVE IT BACK                                 
STE10320 EQU   *                                                                
         B     EXIT3                                                            
         EJECT                                                                  
***********************************************************************         
* COMPARE RATE/SPOT, HANDLE COST OVERRIDE                                       
***********************************************************************         
COMRATE  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CURGRID                                                       
         LA    R3,PRVGRID                                                       
         LHI   R7,TABWK#            TABWK# = 53                                 
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM+4            R6->FIRST DATE DIFFERENCE                   
         MVI   ELEM,X'10'                                                       
         MVI   ELEM+2,RDARRATQ      ELEM SUBCODE                                
         LA    R4,3                 LENGTH SO FAR                               
*                                                                               
         TM    BITFLAG3,B3CSTOVR                                                
         BZ    *+12                                                             
         MVI   ELEM+3,X'80'         COST OVERRIDE?                              
         NI    BITFLAG3,X'FF'-B3CSTOVR                                          
*                                                                               
COMR0100 DS    0H                                                               
         CLI   0(R2),X'FF'                                                      
         BNE   *+12                                                             
         OI    MISCFLG2,MF2EMPTY                                                
         B     COMR0120             NEXT                                        
         CLI   0(R3),X'FF'                                                      
         BNE   *+12                                                             
         OI    MISCFLG2,MF2EMPTY                                                
         B     COMR0120                                                         
*                                                                               
         ICM   R0,15,1(R2)                                                      
         ICM   R1,15,1(R3)                                                      
         SR    R0,R1                                                            
         BNZ   COMR0130                                                         
COMR0120 DS    0H                                                               
         LA    R2,ROWSIZE(R2)                                                   
         LA    R3,ROWSIZE(R3)                                                   
         BCT   R7,COMR0100                                                      
         B     COMRATX              NO DIFFERENCE FOUND                         
COMR0130 DS    0H                                                               
         ST    R0,LASTDRAT          DIFFERENCE IN RATE                          
         STH   R7,WKINDX                                                        
         MVC   LASTRAT,1(R3)        RATE ON PREVIOUS ORDER                      
         BCTR  R7,0                                                             
         LA    R2,ROWSIZE(R2)                                                   
         LA    R3,ROWSIZE(R3)                                                   
         B     COMR0150                                                         
*                                                                               
COMR0150 DS    0H                                                               
         ICM   R0,15,1(R2)                                                      
         ICM   R1,15,1(R3)                                                      
         SR    R0,R1                                                            
         C     R0,LASTDRAT                                                      
         BNE   COMR0152                                                         
         CLC   1(4,R3),LASTRAT      COMPARE WITH LAST RATE                      
         BE    COMRNXT                                                          
COMR0152 DS    0H                                                               
         CLI   0(R2),X'FF'                                                      
         BNE   *+12                                                             
         OI    MISCFLG2,MF2EMPTY                                                
         B     COMR0160                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   COMR0155                                                         
         OI    MISCFLG2,MF2EMPTY                                                
         B     COMR0160                                                         
COMR0155 DS    0H                                                               
         NI    MISCFLG2,X'FF'-MF2EMPTY                                          
COMR0160 DS    0H                                                               
         OC    LASTDRAT,LASTDRAT    IF LAST DIFF = 0                            
         BZ    COMR0200             NO NEED TO WRITE TO ELT                     
*                                                                               
         USING RDARCHS1,R6          WRITE THE RATE TO ELT                       
*                                                                               
         MVC   RDARCHRT,LASTRAT     LAST RATE                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLGSTDAT),(0,MYWORK2)                             
         LHI   R4,TABWK#                                                        
         SH    R4,WKINDX                                                        
*                                                                               
         TM    BITFLAG3,B3DAILYB    DAILY BUY?                                  
         BO    *+8                  YES,DON'T TIMES 7                           
*                                                                               
         MHI   R4,7                                                             
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(R4)                                
*                                                                               
         TM    BITFLAG3,B3DAILYB    DAILY BUY?                                  
         BZ    COMR0180             YES,DON'T ADD START DAY                     
         MVC   MYWORK2(6),MYWORK2+6                                             
         B     COMR0185                                                         
*                                                                               
COMR0180 DS    0H                                                               
         ZIC   R4,STARTDAY                                                      
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,MYWORK2+6,MYWORK2,(R4)                                
COMR0185 DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,MYWORK2),(2,RDARCHS1)                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLGSTDAT),(0,MYWORK2)                             
         LHI   R4,TABWK#                                                        
         SR    R4,R7                                                            
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         BCTR  R4,0                                                             
         TM    BITFLAG3,B3DAILYB    DAILY BUY?                                  
         BO    *+8                  YES, DON'T TIMES 7                          
         MHI   R4,7                                                             
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(R4)                                
*                                                                               
         TM    BITFLAG3,B3DAILYB    DAILY BUY?                                  
         BZ    COMR0190             YES,DON'T ADD ENDAY                         
         MVC   MYWORK2(6),MYWORK2+6                                             
         B     COMR0195                                                         
*                                                                               
COMR0190 DS    0H                                                               
         ZIC   R4,ENDDAY                                                        
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,MYWORK2+6,MYWORK2,(R4)                                
COMR0195 DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,MYWORK2),(2,RDARCHE1)                             
         DROP  R6                                                               
*                                                                               
         LA    R6,8(R6)             BUMP R6                                     
*                                                                               
COMR0200 DS    0H                                                               
         TM    MISCFLG2,MF2EMPTY                                                
         BO    COMR0300                                                         
         STH   R7,WKINDX                                                        
         MVC   LASTRAT,1(R3)                                                    
         ST    R0,LASTDRAT                                                      
         B     COMRNXT                                                          
COMR0300 DS    0H                                                               
         XC    LASTDRAT,LASTDRAT                                                
*                                                                               
COMRNXT  DS    0H                                                               
         LA    R2,ROWSIZE(R2)                                                   
         LA    R3,ROWSIZE(R3)                                                   
         BCT   R7,COMR0150                                                      
*                                                                               
COMR0400 DS    0H                                                               
         LA    R4,ELEM              SPIT OUT ALL THE SPOTS                      
         SR    R6,R4                STICK LENGTH IN                             
         STC   R6,ELEM+1                                                        
*                                                                               
COMR0600 DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,0                        
         OI    MISCFLG2,MF2ADDE                                                 
COMRATX  DS    0H                                                               
         B     EXIT3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BLD 01 SOFT/HARD DELETE ELT, ELT WILL BE ADDED TO AIO3                        
***********************************************************************         
BLD01    NTR1  BASE=*,LABEL=*                                                   
* AIO3->AREA WHERE BUY COMMENT REC IS TO BE CREATED, OR NEW ELT IS TO           
*       APPENDED TO                                                             
         XC    ELEM,ELEM                                                        
*                                                                               
K        USING RDARCMEL,ELEM                                                    
         MVI   K.RDARCMCD,X'01'                                                 
         MVI   K.RDARCMLN,4                                                     
         TM    MISCFLG2,MF2SFTDL                                                
         BZ    *+8                                                              
         OI    K.RDARCMDL,X'80'                                                 
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,0                        
*                                                                               
BLD01X   DS    0H                                                               
         B     EXIT3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD THE "CHANGE ELEMENT" TO THE BUY COMMENT REC                               
* P1: ELT SUBCODE, X'01'=DAY/TIME, X'02'=LENGTH ETC., A( CHANGE )               
* P2: L'CHANGE                                                                  
***********************************************************************         
ADDCHGEL NTR1  BASE=*,LABEL=*                                                   
* AIO3->AREA WHERE BUY COMMENT REC IS TO BE CREATED, OR NEW ELT IS TO           
*       APPENDED TO                                                             
         OI    MISCFLG2,MF2ADDE                                                 
         ZIC   R4,0(R1)             ELT SUBCODE                                 
         L     R3,0(R1)             A (CHANGE)                                  
         L     R6,4(R1)             L'CHANGE                                    
         XC    ELEM,ELEM                                                        
*                                                                               
K        USING RDARCHEL,ELEM                                                    
         MVI   K.RDARCHCD,X'10'                                                 
         AHI   R6,3                                                             
         STC   R6,K.RDARCHLN                                                    
         STC   R4,K.RDARCHTP                                                    
         SHI   R6,4                 EX,ELCODE,LENGTH                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   K.RDARCHCM(0),0(R3)                                              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,0                        
*                                                                               
ADDCHGEX DS    0H                                                               
         B     EXIT3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
EXIT3    XIT1                                                                   
***********************************************************************         
*    VALID DAYPARTS                                                             
***********************************************************************         
DPTABLE  DS    0CL4                                                             
         DC    CL4'MNGM'            MORNING                                     
         DC    CL4'DAYD'            DAYTIME                                     
         DC    CL4'ELYE'            EARLY FRINGE                                
         DC    CL4'ENWR'            EARLY NEWS                                  
         DC    CL4'ACCA'            PRIME ACCESS                                
         DC    CL4'LNWT'            LATE NEWS                                   
         DC    CL4'LTEL'            LATE FRINGE                                 
         DC    CL4'WKDW'            WEEKEND                                     
         DC    CL4'KIDK'            KIDS                                        
         DC    CL4'FRGF'            FRINGE                                      
         DC    CL4'NWSN'            NEWS                                        
         DC    CL4'PRIP'            PRIME                                       
         DC    CL4'MOVV'            MOVIES                                      
         DC    CL4'SPES'            SPECIALS                                    
         DC    CL4'SPOJ'            SPORTS                                      
         DC    CL4'SPSO'            SOAPS                                       
         DC    CL4'COMU'            COMPETITIVE                                 
         DC    CL4'LOCX'            LOCAL                                       
         DC    X'FF'                                                            
         EJECT                                                                  
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
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARE1D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REDARE5D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REDARE3D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REDARE4D                                                       
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
RELO     DS    A                                                                
RELO2    DS    A                                                                
MYWORK   DS    CL80                                                             
SVKEY    DS    CL32                                                             
*                                                                               
AORDNUM  DS    XL4                 AGENCY ORDER NUMBER                          
SPLKEYAD DS    CL133               EXTENDED SPOOLKEY AREA                       
FAKEHDR  DS    CL16                A FAKE HEADER FOR VCON                       
PSTATION DS    CL5                 STATION                                      
PHDLN    DS    XL4                 CONTRACT NUMBER                              
PAGYORD  DS    XL4                 AGENCY ORDER NUMBER                          
PMKTNAME DS    CL20                MARKET NAME                                  
AGYOFF   DS    CL2                                                              
AGYSALP  DS    CL3                                                              
SARLEN   DS    CL12                SECONDS (6 AT 2 BYTES)                       
SARBOOKS DS    CL18                BOOKS                                        
SARDEMO  DS    CL24                DEMOS (8 AT 3 BYTES)                         
BRANDSEQ DS    X                   BRAND SEQUENCE NUMBER                        
STAJONDT DS    XL3                 STATION JOIN DATE                            
ERRFLAG  DS    C                                                                
DATEBLCK DS    CL56                PERVAL OUTPUT BLOCK                          
ORDLSTDA DS    XL4                 CURRENT LIST RECORD DISK ADDRESS             
*                                                                               
FLTSTART DS    XL3                                                              
STARTDAY DS    XL1                 START DAY FOR BUY                            
ENDDAY   DS    XL1                 END DAY FOR BUY                              
ORBSTDAY DS    XL1                 ORBIT START DAY                              
ROTATDAY DS    XL1                 ROTATION START DAY                           
BUYCOST  DS    F                   COST OF SPOTS IN BUY                         
ROTARRAY DS    CL7                 ORBIT ROTATION DAYS                          
*                                                                               
MISCFLG1 DS    X                   MISC. FLAGS 1                                
MF1DAYL  EQU   X'10'                - DAILY BUY                                 
MF1ORBD  EQU   X'08'                - ORBITS DELETED                            
MF1ORBP  EQU   X'04'                - ORBITS PRESENT                            
MF1DTLP  EQU   X'02'                - DETAILS PRESENT                           
*                                                                               
MISCFLG2 DS    X                   MISC. FLAGS 2                                
MF2CMTP  EQU   X'01'                - COMMENT PRESENT                           
MF2ADDE  EQU   X'02'                - AT LEAST ONE ELT IS ADDED                 
MF2ORBT0 EQU   X'04'                - ORBIT FOUND IN CURRENT ORDER              
MF2ORBIT EQU   X'08'                - ORBIT FOUND IN PREVIUOS ORDER             
MF2SVCST EQU   X'10'                - SAVE COST AS WELL                         
MF2SFTDL EQU   X'20'                - CURRENT ORDER WAS CANCELLED               
MF2EMPTY EQU   X'40'                - EMPTY SPOT, DON'T SAVE TO LASTDIF         
*                                                                               
SVDARFLG DS    X                                                                
SF1SENT  EQU   X'40'               -S (SENT TO STATION)                         
SF1UW    EQU   X'01'               UNWIRED                                      
*                                                                               
BUYFLAG1 DS    XL1                                                              
BYFWKLY  EQU   0                                                                
BYFDAILY EQU   1                                                                
BYFCRDLY EQU   2                                                                
BYFPVDLY EQU   3                                                                
BYFTABOV EQU   4                                                                
*                                                                               
MYRECTYP DS    X                   RECORD TYPE FOR LISTING 41/51                
*                                                                               
FLGSTDAT DS    H                   FLIGHT START DATE                            
LASTDIF  DS    H                   THE LAST SPOT DIFF                           
WKINDX   DS    H                   WEEK INDEX RELATIVE TO 53                    
CURSPTW  DS    XL1                 SPOT/WK FOR CUR ORDER                        
PRVSPTW  DS    XL1                 SPOT/WK FOR PRV ORDER                        
SVBUYKEY DS    CL32                                                             
TABWK#   EQU   53                  WK#                                          
ROWSIZE  EQU   L'RDARBUSW+L'RDARBU$$  SPOT SIZE + COST SIZE                     
DAYTIMQ  EQU   RDARBYSL-RDARBYRO                                                
         DS    0H                                                               
CURGRID  DS    XL(TABWK#*ROWSIZE)  BUY GRID FOR CURRENT ORDER                   
*                                                                               
PRVGRID  DS    XL(TABWK#*ROWSIZE)  BUY GRID FOR SAVED DARE ORDER                
REMAIN   EQU   L'CURGRID-256                                                    
OBTNUM0  DS    H                                                                
OBTNUM1  DS    H                                                                
LASTRAT  DS    F                                                                
LASTDRAT DS    F                                                                
PREROT   DS    XL8                 PREVIOUS ORDER'S ROTATION                    
RELO3    DS    A                                                                
MYWORK2  DS    CL80                                                             
HDRDA    DS    CL4                 DISK ADDRESS OF REASSIGN RECORD              
REASSNAM DS    CL20                                                             
REASSOFF DS    CL2                                                              
REASSFLG DS    CL1                                                              
REASSLDT DS    XL3                                                              
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
LASTSTAT DS    CL7                 LAST REP/STATION SEEN                        
LASTMKT  DS    CL4                                                              
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
FTERREV  EQU   X'80'               REVISIION?                                   
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
MISCFLG4 DS    X                   MISC. FLAGS 4                                
MF4RADIO EQU   X'80'               RADIO STATION EDI                            
MF4NOREC EQU   X'40'               NO REC BEING DISPLAYED                       
MF4PDING EQU   X'20'               CONTRACT IS PENDING                          
MF4CONCF EQU   X'10'               CONTRACT IS CONFIRMED                        
MF4CNWIP EQU   X'08'               CONTRACT IS WIP                              
HDRDA2   DS    XL4                                                              
PENDCF   DS    X                                                                
*                                                                               
PRNTWORK DS    CL200               PRINT SETUP WORK AREA                        
REVNUM   DS    X                                                                
WORKLQ   EQU   *-MYAREAD                                                        
*                                                                               
* ONLINE LIST LINE (TWO PHYSICAL LINES)                                         
*                                                                               
LISTD    DSECT                                                                  
LSTSTATH DS    CL8                                                              
LSTCOLOR DS    CL1                                                              
LSTPRNTD DS    CL1                                                              
LSTSTAT  DS    CL6                                                              
LSTSTAT2 DS    CL2                                                              
LINE1CMP EQU   *                                                                
         DS    CL8                 HEADER FOR REMAINDER OF LINE                 
LIN1STRT EQU   *                                                                
LSTCON#  DS    CL8                                                              
         DS    CL1                                                              
LSTTFLAG DS    CL1                                                              
LSTAGY#  DS    CL8                                                              
         DS    CL1                                                              
LSTSTA   DS    CL6                                                              
         DS    CL1                                                              
LSTAGY   DS    CL6                                                              
         DS    CL1                                                              
LSTOFF   DS    CL2                                                              
         DS    CL2                                                              
LSTSALP  DS    CL3                                                              
         DS    CL1                                                              
LSTADV   DS    CL20                                                             
LLINE1   EQU   *-LSTCOLOR                                                       
LLINE1A  EQU   *-LSTCON#                                                        
         DS    CL8                                                              
LIN2PRNT EQU   *                                                                
LSTCOMP  DS    CL5                                                              
LIN2STRT EQU   *                                                                
LSTPRD1  DS    CL12                                                             
         DS    CL1                                                              
LSTPRD2  DS    CL12                                                             
         DS    CL2                                                              
LSTFLT   DS    CL17                                                             
*                                                                               
         ORG   LSTFLT                                                           
LSTSTDT  DS    CL8                                                              
LSTDASH  DS    C                                                                
LSTENDT  DS    CL8                                                              
*                                                                               
         DS    CL2                                                              
LSTEST#  DS    CL4                                                              
         DS    CL1                                                              
LSTTOTL  DS    CL14                                                             
         DS    CL1                                                              
LSTTSPT  DS    CL5                                                              
LLINE2   EQU   *-LIN2PRNT                                                       
LISTLEN  EQU   *-LSTSTATH                                                       
         DS    CL8                 'SEL' CONTROL FIELD                          
         DS    CL1                 'SEL' ENTRY FIELD                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'171REDAR02S  01/02/03'                                      
         END                                                                    
