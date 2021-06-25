*          DATA SET REROM04    AT LEVEL 186 AS OF 12/10/04                      
*PHASE T83F04B                                                                  
*INCLUDE REREPDIF                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE REDARTKO                                                               
*INCLUDE REGENDHT                                                               
*&&      SET   TT=N,T2=N,T1=N                                                   
*                                                                               
         TITLE 'T83F04 - REROM04 - REDI HEADER DISPLAY/SINGLE LINE '            
***********************************************************************         
*                                                                     *         
*  REROM04 (T83F04) --- REDI HEADER DISPLAY/SINGLE LINE INBOX         *         
*        SINGLE LINE DISPLAY VERSION                                  *         
*  THIS VERSION ACTIVATES IOCOUNT.                                    *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
* 17OCT02 (BU ) INITIAL RELEASE OF REDAR02                            *         
* 25OCT02 (BU ) SCREEN 'F0' REPLACED BY SCREEN 'E3'                   *         
* 05NOV02 (HQ ) NEW FILTERS INCORPORATED                              *         
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
* 21DEC02 (BU ) INITIAL RELEASE OF REDAR04                            *         
*               1.  SCREEN 'E3' REPLACED BY SCREEN 'E6'               *         
* 13FEB03 (BU ) **NEW*** HEADER SCREEN VERSION:  REDAREA              *         
* 18FEB03 (HQ ) FLIGHT START DATE/BUY DATE DUMP FIX                   *         
* 24FEB03 (BU ) revert to HEADER SCREEN VERSION:  REDARE1             *         
* 10MAR03 (HQ ) SCROLL BACK AT FIRST PAGE WITHOUT ANY DATA FIX        *         
* 11MAR03 (HQ ) DON'T DUMP ON DELETED KEYS/SET CTRFLAG                *         
*               BUG FIX, CON# FIELD SHOULDN'T BE CLEARED OUT          *         
* 14MAR03 (BU ) CAMPAIGN 'DAYS TILL' FIELD CORRECTION                 *         
* 26MAR03 (HQ ) PRE-VALIDATE CON# FIELD BEFORE GOING TO VCON          *         
*               FIX RECALL ORDER SHOW UP IN FILTER 1                  *         
* 22APR03 (BU ) MULTI-REASSIGNMENT FUNCTION                           *         
* 01MAY03 (BU ) CORRECT LIST AFTER REPORT BUG                         *         
* 13MAY03 (SKU) ADD TRADE FLAG                                        *         
* 13MAY03 (HQ ) REASSIGN AUDIT TRAIL                                  *         
* 19MAY03 (HQ ) DELETE PASSIVE KEYS FOR ACTION "KILLEM"&"CONFIRM"     *         
* 11JUL03 (HQ ) COMPARE FIX: OUT OF WEEK ROTATOR, EXPAND ORBIT GRID   *         
*               SIZE, USE LOCAL IO IN BLDOGRID                        *         
* 17JUL03 (HQ ) INBOX FIX FOR NOTDARE ORDER                           *         
* 14OCT03 (BU ) DARE CONFIRM/CONTRACT WIP FILTER                      *         
* 14OCT03 (HQ ) LOOSE CHECK ON PRODUCT DATE FOR TKO CONTRACT          *         
* 23OCT03 (HQ ) CHANGE PRINT OPTION ALONG WITH NEW REVISION SCREEN    *         
* 07JAN04 (HQ ) LOOSE CHECK ON SALESPERSON OFFICE                     *         
* 18FEB04 (BU ) REASSIGN: REMOVE OFFICE COMPARISON ERROR              *         
* 27FEB04 (BU ) MARKET FOR MASTER LEVEL                               *         
* 12MAR04 (BU ) REASSIGN: RESTORE OFFICE COMPARISON ERROR, FIX MKT    *         
*               FILTER                                                *         
* 12MAR04 (HQ ) KILLEM DATE/TIME STAMP                                *         
* 16MAR04 (BU ) CAMPAIGN FILTER: PROGRESSIVE                          *         
* 07APR04 (HQ ) SUPPRESS REVISION METHOD DISPLAY ON ORD/SELECT SCREEN *         
* 28JUN04 (HQ ) FIX CONFIRM ACTION ADDREC DUMPS                       *         
* 20JUL04 (HQ ) FIX 3 CHARACTER CALL LETTER FILTER                    *         
* 10DEC04 (HQ ) FIX SENTDATE/LIST WITH "RECEIVE DATE" FILTER DUMP     *         
*                                                                     *         
*                                                                     *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T83F04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLQ,*T83F04*,R7,RR=R3                                         
         LR    R5,RC                                                            
         USING MYAREAD,R5                                                       
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWANOGO)                                                   
         SH    RF,=Y(BACK5H)       BACK UP 500 BYTES                            
         ST    RF,ASETSET          SET A(SET OF SETS)                           
         SH    RF,=Y(BACK45K)      BACK UP 4,500 BYTES                          
         ST    RF,ASTASET          SET A(STATION SET)                           
*                                                                               
         MVC   BANNER,=C'MYAREAD '                                              
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLC   =C'SOON',CONWHEN    SOON REPORT?                                 
         BE    MAIN0020            YES - NO TSAR                                
         CLC   =C'NOW',CONWHEN     NOW  REPORT?                                 
         BE    MAIN0020            YES - NO TSAR                                
*                                                                               
TB       USING TSARD,TBLOCK        TSAR BLOCK                                   
TR       USING TLSTD,TSARREC       TSAR RECORD                                  
*                                                                               
         GOTO1 =A(INITTSAR),RR=RELO                                             
*                                                                               
MAIN0020 EQU   *                                                                
         NI    GENSTAT2,X'FF'-X'80'     TURN OFF 'USE MY OK MSG'                
         XC    BUYKEY,BUYKEY       THIS FIX THE DAR10 ORDER CMT BUG             
         XC    AUDITYP,AUDITYP     CLEAR AUDIT TYPE                             
         XC    PRNTWORK,PRNTWORK   CLEAR REPORT PRINT AREA                      
*                                                                               
         OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
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
*                                                                               
*   TEST                                                                        
         LA    RE,NLISTS                                                        
         LA    RF,LISTNUM                                                       
*   TEST END                                                                    
*                                                                               
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
         BZ    VKEY0020            NO ERROR                                     
         L     R2,DUB              SET A(FIELD IN ERROR)                        
         B     ERREND                                                           
VKEY0020 EQU   *                                                                
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS+12-CONHEADH                                          
*                                  SET TO A(DARE PROFILE)                       
         USING SVDSECT,R4                                                       
         TM    SVPGPBIT+DARLPARB,DARLPARA                                       
*                                  USE PAR SECURITY?                            
         BZ    VKEY0040            NO  - USE ORIGINAL SCREEN FIELDS             
         DROP  R4                                                               
         GOTO1 =A(PARSECY),RR=RELO                                              
         BZ    VKEY0040            PAR SECURITY PASSED                          
         OI    GENSTAT2,USMYOK     TURN ON 'USE MY OK MSG'                      
         MVI   ERROR,0                                                          
         L     RD,BASERD           QUICK EXIT                                   
         XIT1                      EXIT WITH CANNED MESSAGE                     
*                                                                               
*                                                                               
*        TEMPORARILY SAVE MYAREAD IN EVENT THIS IS A REPORT PASS                
*                                                                               
VKEY0040 EQU   *                                                                
         MVC   FULL(2),=Y(WORKLQ)                                               
         ZICM  R1,FULL,2                                                        
         PRINT GEN                                                              
         L     RF,AIO3             SET A(TEMP STORAGE SPACE)                    
         AHI   RF,4008             STORE TO IO4                                 
         LA    RE,MYAREAD          SET A(DATA TO BE SAVED)                      
         MOVE  ((RF),(R1)),(RE)                                                 
         PRINT NOGEN                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD, MOVE TO SUBROUTINE FOR ADDRESSIBILITY                        
***********************************************************************         
VR       DS    0H                                                               
         GOTO1 =A(VREC),RR=RELO                                                 
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
         XC    IOCOUNT,IOCOUNT     CLEAR IO COUNTER                             
*                                                                               
         CLI   PFKEY,3             EXIT ON SCROLL BACK                          
         BNE   *+14                                                             
         OC    SCRNPAGE,SCRNPAGE   ON FIRST PAGE THAT DOESN'T HAVE              
         BZ    USREXIT             ORDER AT ALL                                 
*                                                                               
         GOTOR CTRSTART                                                         
         GOTOR LRADDR                                                           
         CLI   MYSCRNUM,X'E6'      SHORT LIST SCREEN ALREADY LOADED?            
         BNE   LR05                NO  - DON'T CHECK FOR REASSIGN               
         CLC   =C'I=',DE1RQTR      REASSIGN REQUEST?                            
         BNE   LR05                NO                                           
*                                                                               
         MVC   DE1RQTR,SPACES                                                   
         OI    DE1RQTRH+4,X'20'    TURN ON 'PREV VALID'                         
         FOUT  DE1RQTRH                                                         
         MVC   LISTNUM,NLISTS                                                   
         ZICM  RF,LISTNUM                                                       
         LA    RF,1(RF)            ENSURE NOTHING GETS CLEARED                  
         STC   RF,LISTNUM                                                       
         MVI   REMULFLG,1          SET INDICATOR                                
         B     REASSXIT            EXIT THIS ROUTINE                            
LR05     EQU   *                                                                
*                                                                               
         CLI   MODE,PRINTREP       REPORT MODE?                                 
         BNE   LR10                NO                                           
*                                                                               
         LA    R2,DE1REPH          YES - RESTORE COMPREP                        
         XC    COMPREP,COMPREP                                                  
         CLI   5(R2),0             ANY INPUT IN FIELD?                          
         BE    LR10                NO                                           
         GOTOR VALIREP                                                          
*                                  NO CONDITION TESTING                         
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
         CLC   =C'O=',DE1HDLN      AGENCY ORDER SOUGHT?                         
         BE    LR40                YES -                                        
         DROP  RF                                                               
*                                                                               
         GOTOR SWAPHDRS,DMCB,DE9LHEDH                                           
         CLI   MYSCRNUM,X'E9'      SHORT LIST SCREEN ALREADY LOADED?            
         BNE   LR20                                                             
         TWAXC DE9AGYCH,DE9ESTH    YES, CLEAR IT                                
         TWAXC DE9SELH,DE9LLINH,PROT=Y                                          
         B     LR30                                                             
                                                                                
* LOAD LOWER SCREEN                                                             
LR20     DS    0H                                                               
         GOTO1 CALLOV,DMCB,DE1TAGH,X'D9083FE9'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCRNUM,X'E9'                                                   
LR30     DS    0H                                                               
         LA    R2,DE9SELH          SET FOR GENCON CURSOR POSITION               
         ST    R2,AFRSTREC                                                      
         LA    R2,DE9STATH                                                      
         ST    R2,ATHISLST                                                      
         OI    DE9SELH+6,X'40'     FORCE CURSOR HERE                            
                                                                                
*                                  DISPLAY CONTRACT INFORMATION                 
         GOTO1 =A(SUBROUT),DMCB,(RC),('QDISCON',0),RR=RELO                      
         MVC   DE9LAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         B     LR70                                                             
                                                                                
LR40     DS    0H                                                               
         CLI   MYSCRNUM,X'E6'      LONG LIST SCREEN ALREADY LOADED?             
         BNE   LR50                                                             
         CLC   =C'Act',DE6HDR1                                                  
         BNE   LR50                ADDITIONAL CHECK TO INSURE SCREEN IS         
*                                  LOADED                                       
         TWAXC DE6SELH,DE6LLIN,PROT=Y YES, CLEAR IT                             
**       MVC   DE6PFLN(17),=C'PF2 Makegood List'                                
         B     LR60                                                             
                                                                                
* LOAD LOWER SCREEN                                                             
LR50     DS    0H                                                               
         GOTO1 CALLOV,DMCB,DE1TAGH,X'D9083FE6'                                  
*                                  LOAD SINGLE LINE SCREEN                      
         MVI   MYSCRNUM,X'E6'                                                   
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR60     DS    0H                                                               
         GOTO1 =A(SWAPHDRS),DMCB,DE6HDR1H,RR=RELO                               
         LA    R2,DE6SELH          SET FOR GENCON CURSOR POSITION               
         ST    R2,AFRSTREC                                                      
         LA    R2,DE6STATH                                                      
         ST    R2,ATHISLST                                                      
         OI    DE6SELH+6,X'40'     FORCE CURSOR HERE                            
*                                                                               
         MVC   DE6LAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
*                                                                               
         L     R3,ATHISLST                                                      
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR650010            NO                                           
         LA    R3,PRNTWORK         SET A(PRINT LINE)                            
LR650010 EQU   *                                                                
         CLI   DE1HDLNH+5,0        CONTRACT # ENTERED?                          
         BE    LR70                NO                                           
*                                                                               
         LR    RF,RA               YES                                          
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         CLC   =C'O=',DE1HDLN      AGENCY ORDER SOUGHT?                         
         BE    LR70                YES - REGULAR PROCESSING                     
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
         DC    H'0'                UNIDENTIFIED RETURN                          
LR70     DS    0H                                                               
         XC    MYLISTDA,MYLISTDA   BUILD LIST OF DISK ADDRESSES                 
         LA    R4,MYLISTDA         FOR ACTION PRINT                             
                                                                                
         TM    CTLRFLG1,CF11STKY   REDISPLAY PAGE?                              
         BZ    LR80                NO                                           
         NI    CTLRFLG1,X'FF'-CF11STKY                                          
         MVC   SELCTKEY,FIRSTKEY   YES - RESTART KEY SET                        
         L     RF,SCRNPAGE         UPDATE THE PAGE COUNTER                      
         BCTR  RF,0                BACK UP 1                                    
         ST    RF,SCRNPAGE         SAVE ADJUSTED PAGE COUNTER                   
*                                                                               
*                                                                               
LR80     DS    0H                                                               
         CLC   =C'BRAND',CONREC                                                 
         BE    LR90                                                             
         TM    CTLRFLG1,CF1BRDQ                                                 
         BO    LR90                                                             
         CLI   MODE,PRINTREP       REPORT?                                      
         BE    LR90                YES - DON'T RESTART                          
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
         CLI   PFKEY,3             SCROLL BACK REQUESTED?                       
         BNE   LR900010            NO  - TAKE REGULAR PATH                      
         TM    CTRCNTRL,X'80'      COUNTER PASS?                                
         BNO   LR900020            NO  - DON'T RESET IT                         
         OC    ORDCOUNT,ORDCOUNT   ORDER COUNT ALREADY SET?                     
         BNZ   LR900020            YES - DON'T RESET IT                         
         L     RF,SCRNPAGE         LOAD SCRNPAGE                                
         BCTR  RF,0                BACK UP ONE PAGE                             
         MH    RF,=H'14'           MULTIPLY BY NUMBER OF PAGES                  
         ST    RF,ORDCOUNT         SAVE STARTING COUNT                          
         B     LR900020            FORCE INTO THIS PATH                         
*                                                                               
LR900010 EQU   *                                                                
         CLI   REMULFLG,1          REASSIGN PASS?                               
         BNE   LR900015            NO                                           
         MVI   REMULFLG,0          TURN OFF REASSIGN PASS FLAG                  
         B     LR900020            BYPASS RECORD TEST                           
LR900015 EQU   *                                                                
         CLI   KEY,X'41'           KEY ALREADY SET TO DARE REC?                 
         BE    LR900020            YES                                          
         CLI   KEY,X'51'           KEY ALREADY SET TO DARE REC?                 
         BNE   LR900040            NO                                           
LR900020 EQU   *                                                                
*                                                                               
         MVC   KEY(27),SAVRADKY    YES - RESET IT TO RADKEY                     
         MVC   KEYLOOK,KEY         SAVE CHARACTERISTICS OF KEY                  
         LA    R3,KEY              RESET A(KEY) FOR USING                       
         GOTOR HIGHCTR             READ (ACTUALLY, REREAD) THIS KEY             
         MVI   DMINBTS,0                                                        
         MVI   RDUPDATE,C'N'                                                    
***      GOTOR SEQCTR              READ THE NEXT KEY FOR RESTART                
         B     LR110                                                            
LR900040 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
         XC    ORDCOUNT,ORDCOUNT                                                
         NI    CTRCNTRL,X'FF'-X'80'                                             
*                                  TURN OFF 'COUNTER PASS'                      
*                                                                               
         CLI   PFKEY,3             SCROLL REQUEST?                              
         BE    LR900060            YES - DON'T CLEAR CURR PAGE #                
*                                                                               
         CLC   =C'CTR',DE1RQTR     COUNTER REQUEST?                             
         BE    LR900080            YES - DON'T CLEAR SCRNPAGE                   
                                                                                
         XC    SCRNPAGE,SCRNPAGE   CLEAR SCREEN PAGE COUNTER                    
         B     LR900080                                                         
LR900060 EQU   *                                                                
         OC    ORDCOUNT,ORDCOUNT   ANY VALUE IN ORDCOUNT?                       
         BZ    LR900080            NO  - DON'T RESET IT                         
         L     RF,SCRNPAGE         LOAD SCRNPAGE                                
         BCTR  RF,0                BACK UP ONE PAGE                             
         MH    RF,=H'14'           MULTIPLY BY NUMBER OF PAGES                  
         ST    RF,ORDCOUNT         SAVE STARTING COUNT                          
LR900080 EQU   *                                                                
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
         CLC   =C'VD',AGENCY       TEST?                                        
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
         LR    RF,RA                                                            
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP IN PROGRESS?                      
         BNO   LR100               NO  -                                        
*                                                                               
         DROP  RF                                                               
*                                                                               
*        CLI   RED01TYP,X'D0'      MASTER REP IN PROGRESS?                      
*        BNE   LR100               NO  - DON'T OVERRIDE SUB REP                 
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
         MVI   DMINBTS,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTOR HIGHCTR                                                          
                                                                                
LR110    DS    0H                                                               
*                                                                               
*   SET -S OVERLAY - THIS IS A TEST                                             
*                                                                               
         LA    RF,99                                                            
         STC   RF,CONVER#                                                       
*   SET END                                                                     
*                                                                               
         CLI   CONFILT,C'Y'        WAS CON# REQUESTED FOUND?                    
         BE    LRX                 YES - NO FURTHER SEARCH                      
*                                                                               
*   END OF KEYS TEST:  KEY BREAK = END OF SCAN                                  
*                                                                               
         CLC   KEYLOOK,KEY         SAME KEY TYPE/REP?                           
         BNE   LRX                 NO  - FINISHED SCAN                          
*                                                                               
         GOTO1 =A(RECDATCK),RR=RELO                                             
         BNZ   LRSEQ               FILTERED OUT BY RECV DATE                    
*                                                                               
*   TEST: FILTER ON A SPECIFIC ORDER                                            
*                                                                               
***      CLC   =X'40560229',KEY+20 PROCESS ONLY A SINGLE ORDER                  
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*                                                                               
         GOTO1 =A(INBOXCHK),DMCB,KEYDEFTB,RR=RELO                               
         BNZ   LRSEQ               FILTERED OUT BY INBOX CHECK                  
*                                                                               
         GOTO1 =A(STATNCHK),DMCB,KEYDEFTB,RR=RELO                               
         BNZ   LRSEQ               FILTERED OUT BY STATION CHECK                
*                                                                               
*   TEST DUMP                                                                   
*                                                                               
***      LA    RF,KEY-8                                                         
***      CLI   KEY+27,X'FF'        DELETED KEY?                                 
***      BNE   *+6                                                              
***      DC    H'0'                                                             
***      CLC   =X'40560229',KEY+20 PROCESS ONLY A SINGLE ORDER                  
***      BNE   LRSEQ                                                            
****     MVC   DIE(2),=X'0000'                                                  
TEST0020 EQU   *                                                                
*                                                                               
         MVI   PENDCF,0            CLEAR FLAG                                   
*                                                                               
         CLI   DE1HDLNH+5,0        ANY CON # ENTERED?                           
         BE    LR115100            NO  -                                        
         CLC   =C'O=',DE1HDLN      AGENCY ORDER SOUGHT?                         
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
*                                                                               
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
         BNZ   LRSEQ               NOT FOUND OR DELETED                         
*                                                                               
LR115120 DS    0H                                                               
         GOTO1 =A(CHKCAMPS),RR=RELO                                             
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
         CLI   DE1TYPEH+5,0        FILTER ON TYPE REQUESTED?                    
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
         GOTOR HIGHCTR             RE-ESTABLISH SEQ ORDER                       
                                                                                
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
*                                                                               
LR210A10 EQU   *                                                                
         GOTO1 =A(CHKCAMPS),RR=RELO                                             
         BNZ   LRSEQ               MISMATCH:  SKIP ORDER                        
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
*                                                                               
*   RESPECT OFFICE CHECK EVEN WHEN FILTERING OF SPECIFIC CONTRACT               
*                                                                               
**       OC    CDARNUM,CDARNUM     FILTERING ON SPECIFIC CONTRACT?              
**       BNZ   LR210B              YES - NO OFFICE CHECK                        
         CLI   DE1OFFH+5,0         FILTER ON OFFICE?                            
         BE    LR210B                                                           
***      CLC   =C'C=',DE1OFF                                                    
***      BE    LR210B                                                           
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
         CLI   DE1TYPEH+5,0        FILTER ON TYPE?                              
         BNE   LR211040            YES - CHECK TYPE FILTERS                     
*                                                                               
         L     R6,AIO              NO  - CAN'T DISPLAY A PENDCF                 
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'20'      DO NOT DISPLAY NOTDARE                       
         BO    LRSEQ                                                            
         DROP  R6                                                               
*                                                                               
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
         GOTO1 =A(MYGETREC),RR=RELO                                             
         BNZ   LRSEQ                                                            
*                                                                               
LR213B40 DS    0H                                                               
*                                                                               
         GOTO1 =A(CHKCAMPS),RR=RELO                                             
         BNZ   LRSEQ                                                            
*                                                                               
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
         TM    RDARMISC,X'20'      NOTDARE TAKES PRESEDENCE                     
         BO    LRSEQ                                                            
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
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR230A                                                           
         LR    RF,R6                                                            
         L     R6,AIO                                                           
         USING RDARFLEM,RF                                                      
         TM    RDARFLG1,X'80'+X'20'+X'10'                                       
         BNZ   LRSEQ               STACF/MATCH-S/PENDCF SHOULD SHOW UP          
         DROP  RF                  FILTER B OR L                                
*                                                                               
LR230A   DS    0H                                                               
         L     R6,AIO                                                           
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
*&&DO                                                                           
         L     RF,AIO                                                           
         USING RDARREC,RF                                                       
         CLC   RDARKORD,=X'31780023'                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*&&                                                                             
         L     R6,AIO                                                           
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
         CLI   DE1BUYRH+5,0        ANY BUYER  FILTER?                           
         BE    LR270012            NO  -                                        
         OC    DE1BUYR(3),SPACES   YES - SET BINARY ZERO TO SPACES              
         CLC   RDARBUYC,DE1BUYR    YES - BUYER CODE FOUND?                      
         BNE   LRSEQ               NO  - SKIP IT                                
LR270012 DS    0H                                                               
         CLI   DE1HDLNH+5,0        CONTRACT # ENTERED?                          
         BE    LR270013            NO                                           
*                                  YES - LAST CHECK FOR COMPANY CODE            
         CLI   DE1REPH+5,0         ANY COMPANY FILTER?                          
         BE    LR270013            NO  -                                        
         CLC   RDARKREP,DE1REP     YES - ORDER FOR THIS COMPANY?                
         BNE   LRSEQ               NO  - SKIP IT                                
LR270013 DS    0H                                                               
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR270015            NO                                           
         CLI   DE1INBXH+5,0        ANY IN-BOX FILTER?                           
         BE    LR270015            NO  -                                        
         L     R6,AIO              YES - FIND X'0A' ELEMENT                     
         MVI   ELCODE,X'0A'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                 MUST BE PRESENT                              
         DC    H'0'                                                             
         CLC   RDARPPSP-RDARPPEL(3,R6),DE1INBX                                  
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
         CLI   DE1OFFH+5,0         ANY OFFICE FILTER?                           
         BE    LR270020            NO  - DON'T FILTER                           
***      CLC   =C'C=',DE1OFF       YES - SPECIAL OVERRIDE?                      
***      BE    LR270020            YES - DONT CHECK OFFICE                      
         GOTO1 =A(CHECKOFF),RR=RELO                                             
*                                  NO  - CHECK OFFICE                           
         BNZ   LRSEQ               NO MATCH: SKIP THIS ORDER                    
*                                                                               
LR270020 EQU   *                                                                
*                                                                               
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
*                                                                               
*                                                                               
         L     R6,AIO              POINT R6 TO BEGINNING OF REC AGAIN           
         XC    AGYOFF,AGYOFF                                                    
         XC    AGYSALP,AGYSALP                                                  
                                                                                
LR280    DS    0H                                                               
         OC    RDARREP#,RDARREP#   ONLY LINKED ORDERS ARE PASSED FOR            
         BNZ   LR290                SALESPERSON FILTERS                         
         CLI   DE1SALPH+5,0        FILTER ON SALESPERSON?                       
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
         GOTOR HIGHCTR                                                          
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
*                                                                               
         CLC   =C'WIP',DE1RQTR     SPECIAL CHECK FOR WIP ORDERS?                
         BNE   LR297               NO                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'1F'        FIND SEND INFO ELEMENT                       
         BRAS  RE,GETEL            MEANS THE RECORD HAS BEEN PRINTED            
         BNE   LR297               MUST BE FOUND                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BO    LR320               YES - SKIP THIS ORDER:NOT WIP                
         DROP  R6                                                               
*                                                                               
LR297    EQU   *                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        FIND SEND INFO ELEMENT                       
         BRAS  RE,GETEL            MEANS THE RECORD HAS BEEN PRINTED            
         BE    *+6                 MUST BE FOUND                                
         DC    H'0'                                                             
         MVC   CONVER#,RCONSRV-RCONSEND(R6)                                     
*                                  SAVE VERSION NUMBER                          
****<<                                                                          
         CLC   =C'WIP',DE1RQTR     SPECIAL CHECK FOR WIP ORDERS?                
         BNE   LR300               NO                                           
         USING RCONSEND,R6                                                      
*                                                                               
*   LAST CONFIRMED BY STATION NO LONGER NEEDED.                                 
*                                                                               
****     TM    RCONSENF,X'02'      LAST CONFIRMED BY STATION?                   
****     BO    LR320               YES - SKIP THIS ORDER                        
         TM    RCONSENF,X'10'+X'20' WIP?                                        
         BO    LR320               NO  - SKIP THIS ORDER                        
*                                  YES - DISPLAY THIS ORDER                     
         DROP  R6                                                               
*                                                                               
****<<                                                                          
*                                                                               
LR300    DS    0H                                                               
         CLI   DE1SALPH+5,0         ON SALESPERSON?                             
         BE    LR330                                                            
         CLC   SALFILT,AGYSALP                                                  
         BE    LR330                                                            
                                                                                
LR320    DS    0H                  NO MATCH, RESTORE KEY AND SEQ NEXT           
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTOR HIGHCTR                                                          
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
         GOTOR HIGHCTR                                                          
*        CLC   KEY(L'RDARKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
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
         GOTOR HIGHCTR                                                          
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   LR335                                                            
*                                                                               
LR333    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
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
         GOTOR SEQCTR                                                           
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    LR333                                                            
         DROP  R6                                                               
*                                  NO MATCH, RESTORE KEY AND SEQ NEXT           
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTOR HIGHCTR                                                          
         B     LRSEQ                                                            
*                                                                               
LR335    DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT LIST DARE RECORD             
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVI   DMINBTS,0                                                        
         GOTOR HIGHCTR                                                          
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 =A(MYGETREC),RR=RELO                                             
         BNZ   LRSEQ                                                            
                                                                                
LR340    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'CTR',DE1RQTR     COUNT REQUEST IN PROGRESS?                   
         BNE   LR340010            NO  - NO COUNT                               
         L     RF,ORDCOUNT         YES - INCREASE COUNT                         
         LA    RF,1(RF)                                                         
         ST    RF,ORDCOUNT                                                      
LR340010 DS    0H                                                               
         CLC   LISTNUM,NLISTS      LISTING AT MAX?                              
         BNE   LR340080            NO                                           
         CLI   MODE,PRINTREP       REPORT REQUEST?                              
         BE    LR340060            YES - SKIP TSAR                              
         TM    TSARFLG,X'20'       PAGE ALREADY SAVED?                          
         BO    LR340060            YES - DON'T DO IT AGAIN                      
****     NI    TSARFLG,X'FF'-X'80' TURN OFF 'TSAR INITIALIZED' FLAG             
         GOTOX =A(GOTSAR),TSASAV,RR=RELO                                        
*                                  SAVE DATA FOR TSAR                           
         CLI   TB.TSERRS,0         ANY ERROR RETURNED?                          
         BE    *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAVE HAPPENED                
LR340060 EQU   *                                                                
         CLC   =C'CTR',DE1RQTR     YES - COUNT REQUEST IN PROGRESS?             
         BNE   LR440               NO  - WRAP IT UP                             
         B     LRSEQ               YES - CYCLE FOR COUNT ONLY                   
LR340080 EQU   *                                                                
         MVC   0(4,R4),KEY+28      SAVE DISK ADDRESS TO LIST                    
         L     R3,ATHISLST         =A(CURRENT LIST LINE)                        
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR342               NO                                           
         LA    R3,PRNTWORK         SET A(PRINT LINE)                            
LR342    EQU   *                                                                
         USING LISTD,R3                                                         
         CLI   MYSCRNUM,X'E9'                                                   
         BE    *+12                                                             
         LA    R0,DE6STATH         IF FIRST RECORD ON LIST                      
         B     *+8                                                              
         LA    R0,DE9STATH                                                      
         CR    R3,R0                                                            
         BH    LR342080                                                         
         TM    KEY,X'D0'           RADIO IN-BOX KEY?                            
         BNO   LR342080            NO  - CAN'T RESTART THIS KEY                 
         MVC   FIRSTKEY,KEY        SAVE FIRST KEY OF LIST                       
         XC    TR.TLREC,TR.TLREC   CLEAR KEY                                    
         MVC   TR.TLDARKEY,FIRSTKEY     SET DARE KEY FOR SCROLL                 
         L     RF,SCRNPAGE         GET CURRENT PAGE NUMBER                      
*                                                                               
         CLI   MODE,PRINTREP       REPORT REQUEST?                              
         BE    LR342080            YES - SKIP TSAR                              
*                                                                               
         CLI   PFKEY,3             SCROLL-BACK REQUESTED?                       
         BE    LR342040            YES - DON'T INCREMENT PAGE CTR               
*                                                                               
         LA    RF,1(RF)            NO  - INCREMENT PAGE COUNTER                 
         ST    RF,SCRNPAGE         SAVE NEW PAGE NUMBER                         
LR342040 EQU   *                                                                
         STCM  RF,3,TR.TLKEY       SET LOW ORDER TWO BYTES IN KEY               
*                                                                               
         CLI   PAGEDETL,1          COUNTER PASS?                                
         BE    LR342050            YES - DON'T SET PAGE# YET                    
         GOTO1 =A(SETPAGE#),RR=RELO                                             
LR342050 EQU   *                                                                
*                                                                               
         XC    TB.TSRNUM,TB.TSRNUM      CLEAR RECORD NUMBER                     
         MVC   WORK(32),TR.TLKEY   SAVE OFF KEY TEMPORARILY                     
         GOTOX =A(GOTSAR),TSARDH,RR=RELO                                        
*                                  READ HIGH: SEE IF KEY EXISTS                 
         TM    TB.TSERRS,TSERNF    RECORD NOT FOUND SET?                        
         BNO   LR342060            NO  - RECORD WAS FOUND                       
         MVC   TR.TLKEY(32),WORK   RESTORE KEY                                  
*                                                                               
         CLC   =H'0',TR.TLKEY      KEY = 0?                                     
         BNE   *+6                 SHOULDN'T HAPPEN                             
         DC    H'0'                                                             
*                                                                               
         GOTOX =A(GOTSAR),TSAADD,RR=RELO                                        
*                                  NO  - MUST BE ADDED TO LIST                  
         CLI   TB.TSERRS,0         ANY ERROR RETURNED?                          
         BZ    LR342080            NO                                           
         DC    H'0'                YES - SHOULDN'T HAVE HAPPENED                
LR342060 EQU   *                                                                
         MVC   TR.TLKEY(32),WORK   RESTORE KEY                                  
         GOTOX =A(GOTSAR),TSAWRT,RR=RELO                                        
*                                  REWRITE RECORD TO LIST                       
         CLI   TB.TSERRS,0         ANY ERROR RETURNED?                          
         BZ    LR342080            NO                                           
         DC    H'0'                YES - SHOULDN'T HAVE HAPPENED                
LR342080 EQU   *                                                                
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
***      TM    RDARMISC,X'04'      VARIOUS W/BRANDS                             
***      BZ    LR344A                                                           
***      MVC   LSTSTAT,=C'BRAND '                                               
***      B     LR380                                                            
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
*                                                                               
LR390    DS    0H                                                               
*                                                                               
*   STATION                                                                     
         MVC   LSTSTA,RDARKSTA     INSERT FIVE-CHAR STATION                     
                                                                                
*                                                                               
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
         MVC   LSTAGY,WORK         INSERT AGY CODE                              
*                                                                               
                                                                                
                                                                                
* FLIGHT DATES                                                                  
         GOTO1 DATCON,DMCB,(2,RDARESST),(5,WORK)                                
         MVC   LSTFSTRT,WORK                                                    
*                                                                               
                                                                                
* ESTIMATE NUMBER                                                               
*                                                                               
         EDIT  RDAREST#,(3,LSTEST),ALIGN=LEFT                                   
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR410                                                            
         USING RDARCLEM,R6                                                      
                                                                                
* ADVERTISER                                                                    
*                                                                               
         MVC   LSTCLI,RDARCLI      FIRST 3 CHARS OF CLIENT NUM                  
         MVI   LSTCLI+3,C'/'                                                    
         MVC   LSTADV,RDARCLNM     FIRST 9 CHARS OF CLIENT NAME                 
         MVC   LSTPRD,RDARPRD1     FIRST 3 CHARS OF PROD CODE                   
         MVI   LSTPRD+3,C'/'                                                    
                                                                                
         DROP  R6                                                               
*                                                                               
LR410    DS    0H                                                               
**++**++**                                                                      
* BUYER                                                                         
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR40D100            NO  - NOT ON SCREEN                          
         MVC   LSTVAR(03),RDARBUYC                                              
LR40D100 EQU   *                                                                
         DROP  R6                                                               
                                                                                
* SENTDATE/ELAPSED TIME                                                         
                                                                                
         LA    R6,KEY              SET A(KEY IN PROGRESS)                       
         USING RDARREC,R6          OVERLAY WITH PASSIVE KEYS                    
*                                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BE    LR40E100            YES - NOT ON SCREEN                          
*                                                                               
         CLI   SCRNHDRS,1          'CAMPAIGN' SCREEN?                           
         BNE   LR40E030            NO                                           
         GOTO1 =A(APERVERT),DMCB,(R3),RR=RELO                                   
         B     LR40E100                                                         
*                                                                               
LR40E030 EQU   *                                                                
         CLI   SCRNHDRS,5          'BUYERS' SCREEN?                             
         BNE   LR40E037            NO                                           
*                                                                               
*   1ST SENT DATE NO LONGER BEING DISPLAYED                                     
*                                                                               
         B     LR40E100                                                         
LR40E037 EQU   *                                                                
         CLI   SCRNHDRS,7          'FLIGHT' SCREEN?                             
         BNE   LR40E040            NO                                           
         GOTO1 =A(BPERVERT),DMCB,(R3),RR=RELO                                   
         B     LR40E100                                                         
LR40E040 EQU   *                                                                
         CLI   SCRNHDRS,9          'SENTDATE ' SCREEN?                          
         BNE   LR40E100            NO                                           
         XC    WORK(24),WORK                                                    
*                                                                               
*   FIRST SENT DATE NO LONGER BEING SHOWN                                       
*                                                                               
*        GOTO1 DATCON,DMCB,(2,RED07FST),(5,WORK)                                
*        MVC   LIN1STRT+08(08),WORK                                             
*                                                                               
         CLI   KEY,X'D0'           PASSIVE KEY?                                 
         BE    LR40E042                                                         
         CLI   KEY,X'D1'           PASSIVE KEY?                                 
         BNE   LR40E044                                                         
LR40E042 EQU   *                                                                
         GOTO1 DATCON,DMCB,(2,RED07FST),(0,WORK)                                
         B     LR40E046                                                         
LR40E044 EQU   *                                                                
         L     R6,AIO              41-KEY: USE DARE RECORD IO AREA              
         GOTO1 DATCON,DMCB,(2,RDARDATE),(0,WORK)                                
*                                  CONVERT DATE TO YYMMDD EBCDIC                
LR40E046 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)                                     
*                                  CONVERT TODAY'S DATE SIMILARLY               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZICM  R1,DMCB+8,4         GET DAYS INCLUSIVE - INSERT WHOLE            
*                                     WORD FOR SIGN PURPOSES                    
         SRA   R1,16               SHIFT BACK DOWN, PROPAGATE SIGN              
         LTR   R1,R1                                                            
         BNP   LR40E050            NOT POSITIVE                                 
         BCTR  R1,0                POS:  SUB 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(4,LSTVAR),FLOAT=+,ZERO=NOBLANK,ALIGN=LEFT                  
         B     LR40E100                                                         
LR40E050 EQU   *                                                                
         AHI   R1,1                NEG: ADD 1 FOR 'NOT INCLUSIVE'               
         EDIT  (R1),(4,LSTVAR),FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT                  
         B     LR40E100                                                         
LR40E100 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              YES - FIND X'0A' ELEMENT                     
         MVI   ELCODE,X'0A'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                 MUST BE PRESENT                              
         DC    H'0'                                                             
         USING RDARPPEL,R6                                                      
         MVC   DMWORK(3),RDARPPSP  SAVE IN-BOX CODE                             
         DROP  R6                                                               
*                                                                               
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
         GOTOR HIGHCTR                                                          
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
*                                                                               
*   THIS IS A LAZY APPROACH:                                                    
*        1.  DOLLARS ARE PUT OUT AS 123456.00                                   
*        2.  .00 IS BLANKED OUT                                                 
*                                                                               
         EDIT  (P6,RDARTDOL),(09,LSTTOTL),2,COMMAS=NO                           
         MVC   LSTTOTL+6(3),SPACES                                              
*                                  CLEAR .00                                    
         DROP  R6                                                               
*                                                                               
         CLI   SCRNHDRS,3          'ORDERS' SCREEN?                             
         BNE   LR40F               NO                                           
*                                                                               
         MVC   LSTVAR(3),DMWORK    INSERT IN-BOX CODE                           
*                                                                               
         LTR   R0,R0               A DUMP LOCATION                              
*                                                                               
LR40F    EQU   *                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
* IF TRADE, PUT T IN LAST COLUMN FOR TRADE INDICATOR                            
         CLI   RDARCORT,C'T'                                                    
         BNE   LR40G                                                            
         MVI   LSTVAR+4,C'T'                                                    
*                                                                               
LR40G    EQU   *                                                                
         MVC   LSTPOWER,RDARKREP   INSERT POWER CODE ON SCREEN                  
*                                                                               
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
         GOTOR HIGHCTR                                                          
*        CLC   KEY(L'RDARKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 =A(MYGETREC),RR=RELO                                             
         BNZ   LRSEQ                                                            
         MVC   SELCTKEY,KEY                                                     
                                                                                
LR430    DS    0H                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BE    LR440               YES - DON'T INCREMENT LISTDA                 
         LA    R4,4(R4)            SAVE OFF DISK ADD                            
                                                                                
LR440    DS    0H                                                               
         L     R2,ATHISLST         SET A(SCREEN LINE)                           
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR445                                                            
         LA    R2,PRNTWORK         SET A(PRINT LINE)                            
         USING LISTD,R2                                                         
LR445    DS    0H                                                               
         LR    RF,RA                                                            
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BNO   LR450               NO  -                                        
         DROP  RF                                                               
         L     RF,AIO                                                           
         USING RDARREC,RF                                                       
***      MVC   LSTCOMP+2(2),RDARKREP                                            
         DROP  RF                                                               
LR450    EQU   *                                                                
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   LR460                                                            
         MVC   P+2(10),LSTCOLOR                                                 
         MVC   P+13(LLSTCON#),LSTCON#                                           
*                                                                               
***      DC    H'0'                                                             
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    PRNTWORK,PRNTWORK   CLEAR PRINTLINE SETUP AREA                   
         DROP  R2                                                               
         B     LR480                                                            
LR460    DS    0H                                                               
         GOTO1 LISTMON                                                          
LR480    DS    0H                                                               
LRSEQ    DS    0H                                                               
*&&DO                                                                           
         L     RF,AIO                                                           
         USING RDARREC,RF                                                       
         CLC   RDARKORD,=X'40160055'                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*&&                                                                             
         CLI   DE1HDLNH+5,0        CONTRACT # ENTERED?                          
         BE    LRSEQ20             NO                                           
         CLC   =C'O=',DE1HDLN      AGENCY ORDER FILTER?                         
         BE    LRSEQ20             YES - CONSIDER IT NOT ENT                    
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
         MVI   DMINBTS,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTOR SEQCTR                                                           
*                                                                               
         LA    R3,KEY              RESET A(KEY FOR FILTERING)                   
         CLC   LISTNUM,NLISTS      LISTING AT MAX?                              
         BNE   LRSEQ30             NO                                           
         CLC   =C'CTR',DE1RQTR     YES - COUNT REQUEST IN PROGRESS?             
         BNE   LRSEQ30             NO  -                                        
         CLI   CTRFLAG,C'Y'        COUNTER FLAG ON?                             
         BE    LRSEQ40             YES - DON'T SAVE ANY MORE KEYS               
         MVI   CTRFLAG,C'Y'        NO  - SET FLAG TO 'YES'                      
LRSEQ30  DS    0H                                                               
         MVC   SAVRADKY,KEY        SAVE RADIO EDI KEY FOR NEXT PAGE             
*                                                                               
*  ENTRY OF MARKET FILTER PLUS CAMPAIGN CODES REQUIRES AN IO COUNT              
*                                                                               
**       CLI   DE1MRKTH+5,0        FILTER ON MARKET?                            
**       BE    LRSEQ32             NO  - DON'T TEST COUNT                       
**       CLI   DE1CMP1H+5,0        YES - CAMPAIGN FILTER REQUESTED?             
**       BNE   LRSEQ35             YES - FIRST FIELD SPEAKS FOR ALL             
*                                     MUST TEST IO COUNT                        
LRSEQ32  DS    0H                                                               
         CLC   =C'WIP',DE1RQTR     SPECIAL CHECK FOR WIP ORDERS?                
         BNE   LRSEQ40             NO  - DON'T TEST IO COUNT                    
LRSEQ35  DS    0H                                                               
         CLC   IOCOUNT,=F'08000'   MAX IO COUNT REACHED?                        
***      CLC   IOCOUNT,=F'50000'   MAX IO COUNT REACHED?                        
         BL    LRSEQ40             NO                                           
         LA    R2,CONRECH          SET A(CURSOR)                                
         CLI   LISTNUM,0           ANY ORDERS FOUND?                            
         BZ    MAXSERCH            NO  - DISPLAY 'SEARCHING'                    
         B     MAXDATA             YES - DISPLAY 'SELECT OR SEARCH'             
***      B     LR440               END THIS CYCLE                               
LRSEQ40  DS    0H                                                               
*                                                                               
         B     LR110                                                            
*                                                                               
LRX      DS    0H                                                               
*                                                                               
*   LOOK FOR CONFIRM VERSION OF THE CONTRACT IF NO CON IS DISPLAYED             
*                                                                               
         CLI   MYSCRNUM,X'E9'      SHORT LIST SCREEN?                           
         BNE   LRXA                NO                                           
         CLI   LISTNUM,0           IS THERE ANY REC DISPLAYED?                  
         BNE   LRXA                YES                                          
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
         CLI   MODE,PRINTREP       REPORT REQUEST?                              
         BE    LRXA0005            YES - DON'T SET PAGE #                       
         GOTO1 =A(SETPAGE#),RR=RELO                                             
         TM    TSARFLG,X'20'       PAGE ALREADY SAVED?                          
         BO    LRXA0005            YES - DON'T DO IT AGAIN                      
         GOTOX =A(GOTSAR),TSASAV,RR=RELO                                        
*                                  SAVE LAST DATA FOR TSAR                      
LRXA0005 EQU   *                                                                
         MVC   DUB(3),DE1RQTR      SAVE VALUE FOR FLAG                          
*                                                                               
         CLC   =C'CTR',DE1RQTR     COUNTER REQUEST?                             
         BNE   LRXA0015            NO                                           
*                                                                               
         MVC   DE1RQTR,SPACES      YES - CLEAR OUT THE REQUEST                  
LRXA0010 EQU   *                                                                
         OI    DE1RQTRH+4,X'20'    TURN ON 'PREV VALID'                         
         FOUT  DE1RQTRH                                                         
LRXA0015 EQU   *                                                                
         MVC   DE6LAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         ZIC   R0,LISTNUM                                                       
         ZIC   R1,NLISTS                                                        
         CLC   =C'CTR',DUB         FLAG SET?                                    
         BNE   USREXIT             NO                                           
         OI    GENSTAT2,USMYOK     TURN ON 'USE MY OK MSG'                      
         MVI   ERROR,0             RESET ERROR                                  
         L     R1,=A(CTRDONE)                                                   
USRERRMS XC    CONHEAD,CONHEAD                                                  
*                                                                               
USRERRMA A     R1,RELO                                                          
         LA    RF,LCTRDONE                                                      
         BCTR  RF,0                                                             
         EX    RF,ERRMVC                                                        
         B     USREXIT                                                          
REASSXIT EQU   *                                                                
         OI    GENSTAT2,USMYOK     TURN ON 'USE MY OK MSG'                      
         MVI   ERROR,0             RESET ERROR                                  
         L     R1,=A(RASSDONE)                                                  
         XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         LA    RF,LRASSDON                                                      
         BCTR  RF,0                                                             
         EX    RF,ERRMVC                                                        
         XC    SELECTKY,SELECTKY   CLEAR SELECTKY FOR RESTART                   
         OI    WHENOK,X'01'        TURN ON 'OTHERS' FLAG                        
         B     USREXIT                                                          
         SPACE                                                                  
USREXIT  EQU   *                                                                
         XIT1                                                                   
*                                                                               
ERRMVC   MVC   CONHEAD(0),0(R1)                                                 
*                                                                               
CTRDONE  DC    C'COUNTER PASS HAS COMPLETED.  HIT <ENTER> TO PROCEED.'          
LCTRDONE EQU   *-CTRDONE                                                        
*ASSDONE DC    C'REASSIGNMENTS ARE COMPLETE.  HIT <ENTER> TO PROCEED.'          
RASSDONE DC    C'REASSIGNED. <ENTER/SCROLL BACK> TO SELECT FROM '               
         DC    C'THIS SCREEN'                                                   
LRASSDON EQU   *-RASSDONE                                                       
         DS    0H                                                               
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
         AHI   RE,4008             IO4                                          
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
*TRDONE  MVC   RERROR,=AL2(201)                                                 
*        B     INFEND                                                           
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
CMPMKTER MVC   RERROR,=AL2(970)    CAMPAIGN + MKT FILTER NG                     
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
NEEDAGY2 MVC   RERROR,=AL2(955)    AGY ORDR FILTER NEEDS AGENCY                 
         B     ERREND                                                           
*                                                                               
CONFERR  MVC   RERROR,=AL2(957)    CONFLICTING OPTIONS                          
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
BADREASS MVC   RERROR,=AL2(961)    INBOX CHANGE REQUIRES VALID CODE             
         B     ERREND                                                           
*                                                                               
NEEDINBX MVC   RERROR,=AL2(962)    INBOX CHANGE REQUIRES INBOX CODE             
         B     ERREND                                                           
*                                                                               
NOREPASS MVC   RERROR,=AL2(963)    NO REPORT WHEN REASSIGNING                   
         B     ERREND                                                           
*                                                                               
SPLEFT2  MVC   RERROR,=AL2(937)                                                 
         B     ERREND                                                           
*                                                                               
KEEPOFF2 MVC   RERROR,=AL2(936)                                                 
         B     ERREND                                                           
*                                                                               
ASSIGNNG MVC   RERROR,=AL2(964)                                                 
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
ERPRDDAT MVC   RERROR,=AL2(959)    ORDER DATES FALL OUTSIDE PRD DATE            
         B     ERREND                                                           
MAXSERCH MVC   RERROR,=AL2(171)    MAX IO:  ENTER TO CONTINUE                   
         B     INFEND                                                           
MAXDATA  MVC   RERROR,=AL2(172)    MAX IO:  DATE FOUND                          
         B     INFEND                                                           
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
***********************************************************************         
* CALL PAR SECURITY.  ERROR:  RETURN CC NOT ZERO .                              
***********************************************************************         
PARSECY  NTR1  LABEL=*,BASE=*                                                   
         MVC   SBGROUP(LSBFILTS),SPACES                                         
*                                  CLEAR THE BLOCK                              
         CLI   DE1GRPH+5,0         GROUP FILTER ENTERED?                        
         BE    PSEC0020            NO                                           
         ZIC   RF,DE1GRPH+5        EXTRACT FIELD LENGTH                         
         BCTR  RF,0                MINUS 1 FOR MOVE                             
         EX    RF,PSEC0800                                                      
PSEC0020 EQU   *                                                                
         CLI   DE1OFFH+5,0         OFFICE FILTER ENTERED?                       
         BE    PSEC0040            NO                                           
         ZIC   RF,DE1OFFH+5        EXTRACT FIELD LENGTH                         
         BCTR  RF,0                MINUS 1 FOR MOVE                             
         EX    RF,PSEC0801                                                      
PSEC0040 EQU   *                                                                
         CLI   DE1STATH+5,0        FILTER ON STATION?                           
         BE    PSEC0060            NO                                           
         MVC   SBSTATN(5),STAFILT  INSERT STATION CALL LETTERS                  
         OC    SBSTATN(5),SPACES                                                
PSEC0060 EQU   *                                                                
         CLI   DE1MRKTH+5,0        FILTER ON MARKET?                            
         BE    PSEC0080            NO                                           
         ZIC   RF,DE1MRKTH+5       EXTRACT FIELD LENGTH                         
         SH    RF,=H'1'            MINUS 3 FOR MOVE                             
         EX    RF,PSEC0802                                                      
PSEC0080 EQU   *                                                                
         CLI   DE1SALPH+5,0        S/P   FILTER ENTERED?                        
         BE    PSEC0100            NO                                           
         ZIC   RF,DE1SALPH+5       EXTRACT FIELD LENGTH                         
         BCTR  RF,0                MINUS 1 FOR MOVE                             
         EX    RF,PSEC0803                                                      
         B     PSEC0120            SKIP TEAM WHEN S/P PRESENT                   
PSEC0100 EQU   *                                                                
         CLI   DE1TEAMH+5,0        TEAM FILTER ENTERED?                         
         BE    PSEC0120            NO                                           
         ZIC   RF,DE1TEAMH+5       EXTRACT FIELD LENGTH                         
         BCTR  RF,0                MINUS 1 FOR MOVE                             
         EX    RF,PSEC0804                                                      
         MVI   SBSALES,X'FF'       FLAG AS TEAM                                 
PSEC0120 EQU   *                                                                
         L     RF,ACOMFACS                                                      
         ST    RF,RFBLOCK          SAVE A(COMFACS) IN REPFACS                   
         MVC   RFBLOCK+4(2),TWAAGY SAVE AGENCY IN REPFACS                       
         GOTOX (RFCKSEC,REPFACS),DMCB,SBLOCK,CONHEADH,0,RFBLOCK,REPFACS         
         BE    PSEC0960            EXIT CC ZERO                                 
         LTR   RB,RB               EXIT CC NOT ZERO                             
         B     PSEC0980                                                         
PSEC0800 EQU   *                                                                
         MVC   SBGROUP(0),DE1GRP   INSERT GROUP BY LENGTH                       
PSEC0801 EQU   *                                                                
         MVC   SBOFFICE(0),DE1OFF  INSERT OFFICE BY LENGTH                      
PSEC0802 EQU   *                                                                
         MVC   SBMARKET(0),DE1MRKT INSERT MARKET BY LENGTH                      
PSEC0803 EQU   *                                                                
         MVC   SBSALES(0),DE1SALP  INSERT S/P    BY LENGTH                      
PSEC0804 EQU   *                                                                
         MVC   SBSALES+1(0),DE1TEAM      INSERT TEAM   BY LENGTH                
PSEC0960 EQU   *                                                                
         SR    R0,R0               EXIT CC ZERO                                 
PSEC0980 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* HIGHCTR: INCREMENT IO COUNT, CALL 'HIGH'                                      
***********************************************************************         
HIGHCTR  NTR1  BASE=*,LABEL=*                                                   
         L     RF,IOCOUNT          INCREMENT IO COUNTER                         
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SEQCTR: INCREMENT IO COUNT, CALL 'SEQ'                                        
***********************************************************************         
SEQCTR   NTR1  BASE=*,LABEL=*                                                   
         L     RF,IOCOUNT          INCREMENT IO COUNTER                         
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         GOTO1 SEQ                                                              
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GETRCTR: INCREMENT IO COUNT, CALL 'GETREC'                                    
***********************************************************************         
GETRCTR  NTR1  BASE=*,LABEL=*                                                   
         L     RF,IOCOUNT          INCREMENT IO COUNTER                         
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         GOTO1 GETREC                                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SET PAGE NUMBER INFORMATION AT BOTTOM OF SCREEN                               
***********************************************************************         
SETPAGE# NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DE6LDAT,SPACES      CLEAR OUT DISPLAY LINE                       
         CLI   DE1HDLNH+5,0        CON # ENTERED?                               
         BNE   SPAG0160            YES - DON'T PUT OUT A MESSAGE                
         OC    SCRNPAGE,SCRNPAGE   ANY DATA?                                    
         BZ    SPAG0120            NO  - DISPLAY MESSAGE                        
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 BLUE  FOR PAGE LINE                          
         MVI   DE6LDAT,C'!'        SET COLOR TO 'BLUE'                          
         MVC   DE6LDAT+1(08),=C'PAGE XXX'                                       
         CLI   PAGEDETL,0          FLAG SET TO 'NO DETAIL'?                     
         BE    SPAG0040            YES - DON'T SHOW TOTALS                      
         MVC   DE6LDAT+1(21),=C'PAGE XXX OF XXX PAGES'                          
         SR    RE,RE               CALCULATE NUMBER OF PAGES                    
         L     RF,ORDCOUNT                                                      
         D     RE,=F'14'           14 ENTRIES PER PAGE                          
         LTR   RE,RE               ANY REMAINDER?                               
         BZ    SPAG0020            NO                                           
         LA    RF,1(RF)            YES - ADD 1 TO PAGE NUMBER CT                
SPAG0020 EQU   *                                                                
         EDIT  (RF),(3,DE6LDAT+13)                                              
*                                  INSERT NUMBER OF PAGES                       
SPAG0040 EQU   *                                                                
         EDIT  SCRNPAGE,(3,DE6LDAT+6)                                           
*                                  INSERT CURRENT PAGE NUMBER                   
         MVC   DE6LDAT+26(28),=C'RECORDS NNNN TO NNNN        '                  
         CLI   PAGEDETL,0          FLAG SET TO 'NO DETAIL'?                     
         BE    SPAG0080            YES - DON'T SHOW TOTALS                      
         MVC   DE6LDAT+26(28),=C'RECORDS NNNN TO NNNN OF NNNN'                  
         EDIT  ORDCOUNT,(4,DE6LDAT+50)                                          
SPAG0080 EQU   *                                                                
         L     RF,SCRNPAGE         LOAD SCREEN PAGE NUMBER                      
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         MH    RF,=H'14'           MULTIPLY BY # ITEMS ON PAGE                  
         LA    RF,1(RF)                                                         
         EDIT  (RF),(4,DE6LDAT+34)                                              
         LA    RF,13(RF)                                                        
         LA    RE,DE1SCRLH         ANYTHING IN SCROLL FIELD?                    
         CLI   5(RE),0                                                          
         BE    SPAG0085            NO                                           
*                                                                               
         XC    ORDCOUNT,ORDCOUNT                                                
         MVC   DE1SCRL,SPACES      CLEAR SCROLL PAGE                            
         FOUT  DE1SCRLH                                                         
         B     SPAG0090                                                         
SPAG0085 EQU   *                                                                
         OC    ORDCOUNT,ORDCOUNT   ANY COUNTER VALUE?                           
         BZ    SPAG0090            NO                                           
         C     RF,ORDCOUNT         PAST END OF COUNT?                           
         BH    SPAG0100            YES                                          
*                                                                               
SPAG0090 EQU   *                                                                
         EDIT  (RF),(4,DE6LDAT+42) NO  - USE CALCULATED VALUE                   
         B     SPAG0160                                                         
SPAG0100 EQU   *                                                                
         EDIT  ORDCOUNT,(4,DE6LDAT+42)                                          
*                                  USE COUNTED HIGH VALUE                       
         B     SPAG0160                                                         
SPAG0120 EQU   *                                                                
         MVC   DE6LDAT+1(13),=C'NO DATA FOUND'                                  
SPAG0160 EQU   *                                                                
*                                                                               
         NI    CTRCNTRL,X'FF'-X'80'                                             
*                                  TURN OFF 'COUNTER PASS'                      
         MVI   PAGEDETL,0          CLEAR DETAIL FLAG                            
         FOUT  DE6LDATH                                                         
         XIT1                                                                   
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
*                                                                               
*   A CAMPAIGN FILTER MAY NOW BE ENTERED AS CLIENT, CLIENT/PRODUCT,             
*        OR CLIENT/PRODUCT/ESTIMATE.  ACCEPTANCE OF ANY FORMAT IS               
*        MADE BY THE CODING BELOW.                                              
*                                                                               
         CLC   CAMPFLD3,SPACES     ANY CAMPAIGN EST# ENTERED?                   
         BE    CCAM0020            NO  - CHECK NEXT HIGHER FIELD                
         CLC   CAMPFLD3+1(3),RDAREST#                                           
*                                  ESTIMATE NUMBER MATCH?                       
         BNE   CCAM0100            NO  - SKIP THIS ORDER                        
CCAM0020 EQU   *                                                                
         LA    RF,RDARELEM                                                      
         ZIC   RE,1(RF)            BUMP TO X'02' ELEMENT                        
         AR    RF,RE                                                            
         CLI   0(RF),2             2ND DESCRIPTION ELEMENT?                     
         BNE   CCAM0100            NO  - SKIP THIS ORDER                        
         CLC   CAMPFLD1(4),RDARCLI-RDARCLEM(RF)                                 
*                                  SAME CLIENT?                                 
         BNE   CCAM0100            NO  - SKIP                                   
         CLC   CAMPFLD2,SPACES     YES - ANY CAMPAIGN PROD ENTERED?             
         BNH   CCAM0080            NO  - OKAY ON CLIENT MATCH                   
         CLC   CAMPFLD2(4),RDARPRD1-RDARCLEM(RF)                                
*                                  YES - SAME PRODUCT?                          
         BNE   CCAM0100            NO  - SKIP                                   
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
***********************************************************************         
* INITIALIZE TSAR                                                               
***********************************************************************         
INITTSAR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    TSARFLG,X'FF'-X'20' TURN OFF 'PAGE SAVED' FLAG                   
         TM    TSARFLG,X'80'       TSAR ALREADY INITIALIZED?                    
         BO    ITSA0020            YES                                          
         OI    TSARFLG,X'40'       NO  - SET PRE-INIT FLAG                      
         ICM   R0,14,=X'D9000A'                                                 
         ICM   R0,1,=AL1(QTSAR)                                                 
         GOTOX CALLOV,DMCB,0,(R0)                                               
         MVC   VATSAR,0(R1)        VATSAR                                       
*                                                                               
         GOTOX =A(GOTSAR),TSAINI,RR=RELO                                        
         B     ITSA0800            EXIT                                         
*                                                                               
ITSA0020 EQU   *                                                                
ITSA0800 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR BUFFER                               *         
* NTRY: R1 = REQUESTED ACTION                                         *         
*                                                                     *         
* EXIT: CC LOW FOR END OF FILE ERROR                                  *         
*       CC HIGH FOR RECORD NOT FOUND                                  *         
***********************************************************************         
GOTSAR   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         STCM  R1,1,TLACTN         REQUESTED ACTION                             
*                                                                               
         CLI   TLACTN,TSARES       EXPLICIT RESTORE?                            
         BE    GOTSAR03                                                         
         CLI   TLACTN,TSASAV       EXPLICIT SAVE?                               
         BE    GOTSAR03                                                         
         CLI   TLACTN,TSAINI       EXPLICIT INITIALISE?                         
         BNE   GOTSAR05                                                         
*                                                                               
GOTSAR03 DS    0H                  INITIALIZE TSAR BLOCK                        
         LA    RE,TBLOCK                                                        
         LA    RF,TSARDL                                                        
         XCEF                                                                   
*                                                                               
GOTSAR05 DS    0H                  INITIALIZE TSAR BLOCK                        
         MVC   TB.TSACTN,TLACTN                                                 
*                                                                               
         LA    RF,TSARREC                                                       
         ST    RF,TB.TSAREC                                                     
*                                                                               
         CLI   TLACTN,TSARES       EXPLICIT RESTORE?                            
         BE    GOTSAR10                                                         
         CLI   TLACTN,TSASAV       EXPLICIT SAVE?                               
         BE    GOTSAR10                                                         
         CLI   TLACTN,TSAINI       EXPLICIT INITIALISE?                         
         BNE   GOTSAR30                                                         
*                                                                               
GOTSAR10 DS    0H                                                               
         MVI   TB.TSPAGL,1         USE TEMPSTR PAGE 1                           
         MVI   TB.TSPAGN,1         NUMBER OF TEMPSTR PAGES TO USE               
*                                  USING PAGES, NOT C/I                         
         MVC   TB.TSACOM,ACOMFACS  SET A(COMFACS)                               
         MVI   TB.TSKEYL,L'TLKEY   SET KEY LENGTH                               
         MVI   TB.TSRECI,TSRVAR    SET 'RECORDS ARE VARIABLE'                   
         MVC   TB.TSRECL,=Y(032)   SET RECORD LENGTH = 32                       
         OI    TB.TSINDS,TSIXTTWA  14K RECORDS                                  
         OI    TB.TSINDS,TSIALLOC                                               
         MVI   TB.TSIND2,TSI2SCND  SET 'SECONDARY BUFFER USAGE'                 
***      MVI   TB.TSIND2,0         CLEAR INDICATOR 2                            
GOTSAR20 GOTOX VATSAR,TB.TSARD     CALL TO INITIALISE/RESTORE                   
         BE    GOTSARX                                                          
         DC    H'0'                ABEND                                        
*                                                                               
GOTSAR30 DS    0H                                                               
         MVC   TB.TSRNUM,TXNUM     SET TSAR NUMBER                              
*                                                                               
         CLI   TLACTN,TSAADD       TEST ADDING                                  
         BNE   GOTSAR50                                                         
***      CLC   TR.TLLEN,=Y(L'TLKEY+2+1)                                         
***      BNL   GOTSAR50                                                         
***      MVC   TR.TLLEN,=Y(L'TLKEY+2+1)                                         
         MVI   TR.TLLEN+1,32          SET L(RECORD) = 32 CHARS                  
*                                                                               
GOTSAR50 DS    0H                                                               
         GOTOX VATSAR,TB.TSARD                                                  
DIEHERE  EQU   *                                                                
         MVC   TXNUM,TB.TSRNUM     SET RECORD LIST NUMBER                       
*                                                                               
         CLI   TLACTN,TSAADD       CHECK IF END-OF-FILE ERROR                   
         BNE   GOTSAR70                                                         
         TM    TB.TSERRS,TSEEOF    DON'T DIE, DISPLAY ERROR INSTEAD             
         BZ    GOTSARX             ASK USER TO REQUEST REPORT OFFLINE           
         MVC   RERROR,=AL2(801)                                                 
         L     R2,ATWA                                                          
         LA    R2,CONRECH-CONHEADH+64(R2)                                       
         GOTO1 MYERROR                                                          
*                                                                               
GOTSAR70 DS    0H                                                               
         TM    TB.TSERRS,TSEEOF    RETURN CC=LOW FOR END-OF-FILE ERROR          
         BO    EXITL                                                            
         TM    TB.TSERRS,TSERNF    RETURN CC=HIGH IF RECORD NOT FOUND           
         BO    EXITH                                                            
*                                                                               
GOTSARX  DS    0H                                                               
*                                                                               
*   TEST                                                                        
***      CLI   TLACTN,TSASAV       EXPLICIT SAVE?                               
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   TEST                                                                        
*                                                                               
         B     EXITOK                                                           
EXITOK   CR    RB,RB                                                            
         B     GOEXIT                                                           
EXITL    CLI   *,X'FF'                                                          
         B     GOEXIT                                                           
EXITH    CLI   *,0                                                              
*                                                                               
GOEXIT   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   STATION CHECK:  IF FIELD ENTERED, VALIDATE VS THIS FILTER                   
*                                                                               
STATNCHK NTR1  LABEL=*,BASE=*                                                   
         L     RF,0(R1)            SET A(KEYDEFTB KEY DEFINITION TABLE)         
*                                                                               
*   TEST                                                                        
***      CLC   =X'40560229',KEY+20 PROCESS ONLY A SINGLE ORDER                  
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   TEST                                                                        
*                                                                               
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
         XC    DUB,DUB             CLEAR TEMPORARY STORAGE                      
*                                                                               
*   must set up key search for station filter as well as market                 
*        filter.  actual test is skipped if station filter                      
*        has not been entered.                                                  
*                                                                               
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
         ST    RF,DUB              SAVE A(REC TYPE IN TABLE)                    
*                                                                               
         CLI   DE1STATH+5,0        FILTER ON STATION?                           
         BE    STAT0080            NO  - CHECK FOR MARKET FILTER                
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
         LR    R2,RE               SET A(STATION IN ORDER)                      
*                                                                               
         CLI   STAFILT,C'*'        STATION SET?                                 
         BNE   STAT0070            NO  - SINGLE STATION                         
         LR    R2,RE               PASS IN A(STATION IN ORDER)                  
         GOTO1 =A(CHECKSET),DMCB,(R2),RR=RELO                                   
         BNZ   STAT0220            NOT IN SET                                   
         B     STAT0080            FOUND IN SET                                 
STAT0070 EQU   *                                                                
         CLC   STAFILT(5),0(RE)    SAME STATION?                                
         BE    STAT0080            NOT EQUAL - EXIT CC NOT ZERO                 
         CLI   STAFILT+3,C'-'      MATCH 3 CHARACTER C-LETTER                   
         BNE   STAT0220            WITHOUT CHANGING STAFILT                     
         MVC   WORK(5),STAFILT                                                  
         MVI   WORK+3,C' '                                                      
         CLC   WORK(5),0(RE)       SAME STATION?                                
         BNE   STAT0220                                                         
*                                                                               
*   NOW CHECK FOR MARKET FILTER.                                                
*                                                                               
*   MARKET FILTER MAY BE REQUESTED.  MKT FIELD CONTAINS "AAAA"                  
*        WHERE AAAA IS AN ALPHANUMERIC CODE IN WHICH THE RIGHT-                 
*        MOST POSITIONS MAY BE BINARY ZERO.                                     
*        1.   THE STATION MUST BE DETERMINED IF KEY IS CAMPAIGN KEY             
*        2.   PASSIVE KEY X'8306' MUST BE CHECKED TO DETERMINE                  
*             MARKET OF STATION                                                 
*                                                                               
STAT0080 EQU   *                                                                
*                                                                               
         CLI   DE1MRKTH+5,0        ANY MARKET FILTER ENTERED?                   
         BE    STAT0200            NO  - EXIT CC ZERO (ACCEPT)                  
*                                                                               
         L     RF,DUB              LOAD A(REC TYPE)                             
         CLI   DSTATION(RF),X'FF'  READ STATION INDICATOR?                      
         BE    STAT0220            YES - EXIT CC NOT ZERO                       
*                                                                               
         LA    RE,KEY              SET A(KEY IN PROGRESS)                       
         ZIC   R1,DSTATION(RF)     GET DISPLACEMENT TO STA CALLS                
         AR    RE,R1               DISPLACE TO STATION CALL LETTERS             
         LR    R2,RE               SET A(STATION IN ORDER)                      
*                                                                               
         LA    R1,STASET           SET A(STATION TABLE)                         
STAT0082 EQU   *                                                                
         CLI   0(R1),0             EMPTY SLOT?                                  
         BE    STAT0220            YES - NOT FOUND: EXIT CC NOT ZERO            
*                                                                               
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    STAT0220            YES - NOT FOUND: EXIT CC NOT ZERO            
*                                                                               
         CLC   0(5,R1),0(R2)       SAME STATION? STASET VS STA IN KEY           
         BE    STAT0200            EQUAL - EXIT CC ZERO                         
         LA    R1,5(R1)            BUMP TO NEXT SLOT                            
         B     STAT0082            GO BACK FOR NEXT                             
*                                                                               
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
*        A(KEYDEFTB) PASSED IN BY CALL TO ROUTINE.                              
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
         CLC   =C'SENTDATE',CONREC                                              
         BNE   RECD0010                                                         
*                                                                               
K        USING RDARKEY,KEY                                                      
         CLC   FILTDAT1,FILTDAT2   ONE DATE INPUT?                              
         BNE   RECD0005            NO                                           
         CLC   FILTDAT1,K.RED09NDT                                              
         BNE   RECD0750                                                         
         B     RECD0010                                                         
*                                                                               
RECD0005 DS    0H                                                               
         CLC   K.RED09NDT,FILTDAT1                                              
         BL    RECD0750                                                         
         CLC   K.RED09NDT,FILTDAT2                                              
         BH    RECD0750                                                         
         DROP  K                                                                
*                                                                               
RECD0010 OI    DMINBTS,X'08'       PASS BACK DELETES                            
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
         BNE   LRAD0040            NO                                           
*                                                                               
         GOTO1 SETHEAD                                                          
LRAD0040 EQU   *                                                                
         MVI   REDITYPE,3          SET SCREEN TO 'BASIC SEQ'                    
         ZIC   R1,CONRECH+5        EXCEPT FOR MAKEGOOD                          
         LTR   R1,R1               NO INPUT, ORDER (DEFAULT) SEQUENCE           
         BZ    LRAD0060                                                         
         BCTR  R1,0                                                             
         EX    R1,DAMN0060         LOOK FOR ORDER                               
         BE    LRAD0060                                                         
         MVI   REDITYPE,1          SET SCREEN TO 'CAMPAIGN'                     
         EX    R1,DAMN0061         LOOK FOR CAMPAIGN                            
         BE    LRAD0060                                                         
         MVI   REDITYPE,5          SET SCREEN TO 'BUYER SEQ'                    
         EX    R1,DAMN0062         LOOK FOR BUYER                               
         BE    LRAD0060                                                         
         MVI   REDITYPE,7          SET SCREEN TO 'FLIGHT SEQ'                   
         EX    R1,DAMN0063         LOOK FOR FLIGHT                              
         BE    LRAD0060                                                         
         MVI   REDITYPE,9          SET SCREEN TO 'SENTDATE  SEQ'                
         EX    R1,DAMN0064         LOOK FOR SENTDATE                            
         BE    LRAD0060                                                         
         DC    H'0'                UNRECOGNIZED TYPE                            
DAMN0060 CLC   CONREC(0),=C'ORDER   '       SUBTYPE = 3                         
DAMN0061 CLC   CONREC(0),=C'CAMPAIGN'       SUBTYPE = 1                         
DAMN0062 CLC   CONREC(0),=C'BUYER   '       SUBTYPE = 5                         
DAMN0063 CLC   CONREC(0),=C'FLIGHT  '       SUBTYPE = 7                         
DAMN0064 CLC   CONREC(0),=C'SENTDATE'       SUBTYPE = 9                         
*                                                                               
LRAD0060 EQU   *                                                                
         MVC   SCRNHDRS,REDITYPE   SAVE DATA TYPE INDICATOR                     
*                                                                               
         GOTO1 =A(SETFILTR),RR=RELO  SET UP FILTER TABLE                        
*                                                                               
* TEST IF PRINT FROM LIST SELECT COLUMN                                         
*                                                                               
         CLI   MODE,PRINTREP       REPORT REQUEST?                              
         BE    LRAD0800            YES - EXIT W/O TSAR                          
         CLI   PFKEY,3             SCROLL UP REQUESTED?                         
         BE    LRAD0080            YES, SKIP PRINT FROM LIST COLUMN             
*                                                                               
         CLI   MYSCRNUM,X'E6'      SHORT LIST SCREEN ALREADY LOADED?            
         BNE   LRAD0070            NO  - DON'T CHECK FOR REASSIGN               
         CLC   =C'I=',DE1RQTR      REASSIGN INBOX?                              
         BNE   LRAD0070            NO  - DON'T CHECK MULTIASS                   
         GOTO1 =A(MULTIASS),RR=RELO                                             
*                                  CHECK FOR MULTI-ASSIGN                       
         B     LRAD0800                                                         
LRAD0070 EQU   *                                                                
         GOTO1 =A(PR),RR=RELO                                                   
         TM    TSARFLG,X'40'       TSAR PRE-INIT DONE?                          
         BNO   LRAD0080            NO  - MUST BE COMPLETELY SET                 
         NI    TSARFLG,X'FF'-X'40' YES - TURN OFF PRE-INIT                      
         OI    TSARFLG,X'80'       TURN ON INITIALIZED                          
         B     LRAD0800            NOTHING TO RELOAD                            
LRAD0080 EQU   *                                                                
         GOTOX =A(GOTSAR),TSARES,RR=RELO                                        
*                                  RESTORE TSAR TABLE SO FAR                    
         CLI   PFKEY,3             SCROLL UP REQUESTED?                         
         BNE   LRAD0100            NO                                           
         CLI   DE1HDLNH+5,0        CON # ENTERED?                               
         BNE   LRAD0100            YES - NO SCROLL-UP ALLOWED.                  
         GOTO1 SETSCROL                                                         
LRAD0100 EQU   *                                                                
         LTR   RB,RB               SET A PLACE TO DUMP                          
LRAD0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
SETSCROL NTR1                                                                   
         L     RF,SCRNPAGE         RETRIEVE CURRENT SCREEN NUMBER               
         BCTR  RF,0                BACK UP ONE PAGE                             
         LTR   RF,RF               ANYTHING LEFT?                               
         BNZ   SSCR0020            YES - SCROLL BACK ONE PAGE                   
         L     RF,SCRNPAGE         NO  - GET CURRENT SCRN # AGAIN               
*                                     THIS WILL KEEP SCROLL AT                  
*                                        TOP PAGE                               
SSCR0020 EQU   *                                                                
         ST    RF,SCRNPAGE         RESET SCROLL PAGE                            
         XC    TSARREC,TSARREC     CLEAR RECORD                                 
         STCM  RF,3,TR.TLPAGE      PAGE NUMBER IS RECORD KEY                    
         GOTOX =A(GOTSAR),TSARDH,RR=RELO                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVRADKY(27),TR.TLDARKEY                                         
*                                  RESET KEY FOR STARTUP                        
         CLC   SCRNPAGE,=F'1'      FIRST PAGE?                                  
         BNE   SSCR0040            NO  - LEAVE KEY AS IS                        
         XC    SAVRADKY+4(23),SAVRADKY+4                                        
*                                  YES - SET TO PAGE BACK TO FIRST REC          
SSCR0040 EQU   *                                                                
         LTR   RF,RF                                                            
SSCR0100 EQU   *                                                                
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
         GOTOR GETRCTR             GET THE AGENCY RECORD                        
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
*   TEST LISTD                                                                  
         LA    RF,LISTD                                                         
         LTR   RF,RF                                                            
*   END TEST                                                                    
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
*                                                                               
*   FOR CONFIRMED ORDERS, THIS COLOR IS SET TO MAGENTA.                         
*                                                                               
**       MVI   LSTCOLOR,C'!'       SET TO BLUE                                  
         MVI   LSTCOLOR,C'V'       SET TO MAGENTA                               
         B     GMOV0390                                                         
                                                                                
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
         CLC   =C'-S',LSTSTAT2     'SENT'?                                      
         BNE   GMOV0385            NO                                           
***                                                                             
*   temporarily remove insertion of VERSION NUMBER                              
*                                                                               
         EDIT  CONVER#,(2,LSTSTAT2),ZERO=NOBLANK,FILL=0                         
GMOV0385 EQU   *                                                                
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
         MVI   LSTCOLOR,C'!'                                                    
         B     LR381080                                                         
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
* APERVERT:  DATE CALCULATION MOVED OUT FOR ADDRESSABILITY           *          
**********************************************************************          
APERVERT NTR1  LABEL=*,BASE=*                                                   
         L     R3,0(R1)            RESET A(LISTD)                               
         USING LISTD,R3                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'        FIRST AIRED DATE ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   PER10060            NOT FOUND - USE VALUE FROM KEY               
         USING RDAREL2M,R6                                                      
         GOTO1 DATCON,DMCB,(2,RDAR2FDT),(0,WORK)                                
*                                  CONVERT DATE TO YYMMDD EBCDIC                
         B     PER10080                                                         
         DROP  R6                                                               
PER10060 EQU   *                                                                
         L     R6,AIO                                                           
         GOTO1 DATCON,DMCB,(2,RDARESST-RDARREC(R6)),(0,WORK)                    
*                                  CONVERT DATE TO YYMMDD EBCDIC                
PER10080 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)                                     
*                                  CONVERT TODAY'S DATE SIMILARLY               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZICM  R1,DMCB+8,4         GET DAYS INCLUSIVE - INSERT WHOLE            
*                                     WORD FOR SIGN PURPOSES                    
         SRA   R1,16               SHIFT BACK DOWN, PROPAGATE SIGN              
         LTR   R1,R1                                                            
         BNP   PERV1090            NOT POSITIVE                                 
         BCTR  R1,0                POS:  SUB 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(4,LSTVAR),FLOAT=+,ZERO=NOBLANK,ALIGN=LEFT                  
         B     PERV1100                                                         
PERV1090 EQU   *                                                                
         A     R1,=F'1'            NEG:  ADD 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(4,LSTVAR),FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT                  
PERV1100 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BPERVERT:  DATE CALCULATION MOVED OUT FOR ADDRESSABILITY           *          
**********************************************************************          
BPERVERT NTR1  LABEL=*,BASE=*                                                   
         L     R3,0(R1)            RESET A(LISTD)                               
         USING LISTD,R3                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'        FIRST AIRED DATE ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   PER20060            NOT FOUND - USE VALUE FROM 01 ELT            
         USING RDAREL2M,R6                                                      
         GOTO1 DATCON,DMCB,(2,RDAR2FDT),(0,WORK)                                
*                                  CONVERT DATE TO YYMMDD EBCDIC                
         DROP  R6                                                               
         B     PER20080                                                         
PER20060 EQU   *                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
***>>>   GOTO1 DATCON,DMCB,(2,KEY+RED07FST-RED07KEY),(0,WORK)                   
         GOTO1 DATCON,DMCB,(2,RDARESST),(0,WORK)                                
*                                  CONVERT DATE TO YYMMDD EBCDIC                
         DROP  R6                                                               
PER20080 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)                                     
*                                  CONVERT TODAY'S DATE SIMILARLY               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZICM  R1,DMCB+8,4         GET DAYS INCLUSIVE - INSERT WHOLE            
*                                     WORD FOR SIGN PURPOSES                    
         SRA   R1,16               SHIFT BACK DOWN, PROPAGATE SIGN              
         LTR   R1,R1                                                            
         BNP   PER20120                                                         
         BCTR  R1,0                POS:  SUB 1 FOR 'NOT INCLUSIVE'              
         EDIT  (R1),(4,LSTVAR),FLOAT=+,ZERO=NOBLANK,ALIGN=LEFT                  
         B     PER20140                                                         
PER20120 EQU   *                                                                
         AHI   R1,1                NEG: ADD 1 FOR 'NOT INCLUSIVE'               
         EDIT  (R1),(4,LSTVAR),FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT                  
PER20140 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SWAP COLUMN HEADERS BASED ON REQUEST                                          
***********************************************************************         
SWAPHDRS NTR1  LABEL=*,BASE=*                                                   
*                                                                               
*   NEW HEADERS HAVE TO BE SET UP WHEN MODELS ARE DELIVERED                     
*                                                                               
         L     R2,0(R1)            SET A(OUTPUT LINE)                           
         LA    R1,MAINHDR          SET A(DEFAULT  HEADER)                       
                                                                                
         MVC   MAINHDR+72(5),=C'START'                                          
         CLI   SCRNHDRS,1          CAMPAIGN REQUEST?                            
         BE    SHDR0040            YES                                          
         MVC   MAINHDR+72(5),=C'INBOX'                                          
         CLI   SCRNHDRS,3          ORDER    REQUEST?                            
         BE    SHDR0040            YES                                          
         MVC   MAINHDR+72(5),=C'BUYER'                                          
         CLI   SCRNHDRS,5          BUYER   REQUEST?                             
         BE    SHDR0040            YES                                          
         MVC   MAINHDR+72(5),=C'START'                                          
         CLI   SCRNHDRS,7          FLIGHT   REQUEST?                            
         BE    SHDR0040            YES                                          
         MVC   MAINHDR+72(5),=C'SENT '                                          
         CLI   SCRNHDRS,9          SENTDATE REQUEST?                            
         BNE   SHDR0060            THIS SHOULDN'T HAPPEN                        
*                                                                               
SHDR0040 DS    0H                                                               
*                                                                               
SHDR0050 DS    0H                                                               
         XC    8(78,R2),8(R2)      CLEAR LINE OF HEADING                        
         MVC   8(LNEWHDR,R2),0(R1)                                              
*                                  INSERT NEW HEADER                            
         FOUT  (R2)                                                             
SHDR0060 DS    0H                                                               
         XIT1                                                                   
*                                                                               
*   REPLACEMENT SCREEN HEADERS FOR DIFFERING VIEWS.                             
*        KEEP ALL ENTRIES THE SAME SIZE.  LOOP DEPENDS ON IT                    
*                                                                               
MAINHDR  DC    C'A STATUS     CON NUM AGY   ADVRTSR CLI PRD EST '               
         DC    C'FSTART   STAT  TOTAL$    VAR  '                                
LNEWHDR  EQU   *-MAINHDR                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SWAP COLUMN HEADERS BASED ON REQUEST ON REPORT                                
***********************************************************************         
SWAPRHDR NTR1  LABEL=*,BASE=*                                                   
*                                                                               
*   NEW HEADERS HAVE TO BE SET UP WHEN MODELS ARE DELIVERED                     
*                                                                               
         LA    R1,RMAINHDR         SET A(DEFAULT  HEADER)                       
*                                                                               
         MVC   RMAINHDR+72(5),=C'START'                                         
         CLI   SCRNHDRS,1          CAMPAIGN REQUEST?                            
         BE    SRHD0040            YES                                          
         MVC   RMAINHDR+72(5),=C'INBOX'                                         
         CLI   SCRNHDRS,3          ORDER    REQUEST?                            
         BE    SRHD0040            YES                                          
         MVC   RMAINHDR+72(5),=C'BUYER'                                         
         CLI   SCRNHDRS,5          BUYER   REQUEST?                             
         BE    SRHD0040            YES                                          
         MVC   RMAINHDR+72(5),=C'START'                                         
         CLI   SCRNHDRS,7          FLIGHT   REQUEST?                            
         BE    SRHD0040            YES                                          
         MVC   RMAINHDR+72(5),=C'SENT '                                         
         CLI   SCRNHDRS,9          SENTDATE REQUEST?                            
         BNE   SRHD0060            THIS SHOULDN'T HAPPEN                        
*                                                                               
SRHD0040 DS    0H                                                               
         XC    H3,H3               CLEAR FIRST LINE OF HEADING                  
         MVC   H3(LRNEWHDR),0(R1)   MOVE TO HEAD2                               
*                                                                               
SRHD0060 DS    0H                                                               
         XIT1                                                                   
*                                                                               
*   REPLACEMENT SCREEN HEADERS FOR DIFFERING VIEWS.                             
*        KEEP ALL ENTRIES THE SAME SIZE.  LOOP DEPENDS ON IT                    
*                                                                               
RMAINHDR DC    C'A STATUS     CON NUM AGY   ADVRTSR CLI PRD EST '               
         DC    C'FStart   Stat  Total$    Var  '                                
LRNEWHDR  EQU   *-RMAINHDR                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SCAN ALL 8306 KEYS FOR MARKET.                                                
*        FOR MASTER RUN, SELECT SUBSIDIARIES ONLY FROM SBREPCDSX                
*            TABLE.                                                             
*        PLACE TABLE IN TWA SPACE FOR SAVE BETWEEN ENTERS                       
***********************************************************************         
SETSTAS  NTR1  LABEL=*,BASE=*                                                   
         LA    RE,STASET           CLEAR STATION SET TABLE                      
         LHI   RF,600                                                           
         XCEF                                                                   
*                                                                               
         LA    R3,STASET           SET A(STATION SET TABLE)                     
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BO    SSTA0200            YES -                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'           SET MARKET CODE PASSIVE                      
         MVI   KEY+1,X'06'                                                      
         MVC   KEY+16(2),AGENCY    NO  - INSERT REP POWER CODE                  
         MVC   KEY+18(4),DE1MRKT   INSERT MARKET CODE FROM SCREEN               
SSTA0020 EQU   *                                                                
         GOTOR HIGHCTR                                                          
         B     SSTA0060                                                         
SSTA0040 EQU   *                                                                
         GOTOR SEQCTR                                                           
SSTA0060 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY/REP/MARKET CODE?                    
         BNE   SSTA0980            NO  - FINISHED                               
         CLI   KEY+26,C'C'         COMBO STATION?                               
         BE    SSTA0040            YES - DON'T TABLE IT                         
         CLI   0(R3),X'FF'         END OF TABLE REACHED?                        
         BNE   *+6                 NO                                           
         DC    H'0'                MAKE TABLE LARGER                            
         MVC   0(5,R3),KEY+22      MOVE STATION TO STA TABLE                    
SSTA0080 EQU   *                                                                
         LA    R3,5(R3)            BUMP TO NEXT STA TABLE                       
         B     SSTA0040            GO BACK FOR NEXT KEY                         
SSTA0200 EQU   *                                                                
*                                                                               
*    MASTER REP IS BEING PROCESSED.  READ KEYS FOR EACH SUB REP, AND            
*        TABLE UP THE STATION CALL LETTERS FOUND IN EACH.                       
*                                                                               
         LA    R4,SBRPCDSX         SET A(FIRST SUBREP CODE)                     
SSTA0220 EQU   *                                                                
         CLC   0(2,R4),SPACES      ANY ENTRY IN SLOT?                           
         BNH   SSTA0980            NO  - FINISHED                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'83'           SET MARKET CODE PASSIVE                      
         MVI   KEY+1,X'06'                                                      
         MVC   KEY+16(2),0(R4)     NO  - INSERT SUBREP POWER CODE               
         MVC   KEY+18(4),DE1MRKT   INSERT MARKET CODE FROM SCREEN               
SSTA0240 EQU   *                                                                
         GOTOR HIGHCTR                                                          
         B     SSTA0280                                                         
SSTA0260 EQU   *                                                                
         GOTOR SEQCTR                                                           
SSTA0280 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY/REP/MARKET CODE?                    
         BNE   SSTA0360            NO  - FINISHED WITH SUBREP                   
         CLI   KEY+26,C'C'         COMBO STATION?                               
         BE    SSTA0260            YES - DON'T TABLE IT                         
         LA    RF,STASET           SET A(STATION TABLE)                         
SSTA0300 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    SSTA0320            YES - STATION NOT ALREADY IN TABLE           
         CLC   0(5,RF),KEY+22      NO  - KEY IN TABLE?                          
         BE    SSTA0260            YES - DON'T ENTER AGAIN                      
         LA    RF,5(RF)            NO  - BUMP TO NEXT SLOT                      
         B     SSTA0300            GO BACK FOR NEXT SLOT                        
SSTA0320 EQU   *                                                                
         CLI   0(R3),X'FF'         END OF TABLE REACHED?                        
         BNE   *+6                 NO                                           
         DC    H'0'                MAKE TABLE LARGER                            
*                                                                               
         MVC   0(5,R3),KEY+22      MOVE STATION TO STA TABLE                    
SSTA0340 EQU   *                                                                
         LA    R3,5(R3)            BUMP TO NEXT STA TABLE                       
         B     SSTA0260            GO BACK FOR NEXT KEY                         
SSTA0360 EQU   *                                                                
         LA    R4,2(R4)            BUMP TO NEXT SUBREP CODE                     
         B     SSTA0220            GO BACK FOR NEXT                             
SSTA0980 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE MARKET CODE                                                          
***********************************************************************         
VALIMKT  NTR1  LABEL=*,BASE=*                                                   
         XC    WORK,WORK                                                        
         ZIC   RF,DE1MRKTH+5       L(MARKET FIELD)                              
         CLI   DE1MRKTH+5,4        FIELD CAN ONLY CONTAIN 4 CHARS MAX           
         BH    VMKT0900            ERROR                                        
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R3,KEY                                                           
         USING RMKTKEY,R3                                                       
         MVI   KEY,X'2B'                                                        
         MVC   RMKTKREP,AGENCY     INSERT REP CODE                              
         MVC   RMKTKMKT,DE1MRKT    INSERT FOUR CHARS MKT CODE                   
         GOTOR HIGHCTR                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VMKT0900            NO  - SEND ERROR MESSAGE                     
*                                                                               
*   CAN'T SAVE MARKET FILTER AS ENTERED. NO STORAGE, AND IT DOESN'T             
*        REALLY MAKE SENSE.                                                     
*                                                                               
***>>>   MVC   STAFILT(4),DE1STAT  SAVE MKT FILTER AS ENTERED                   
*                                                                               
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
*                                                                               
*    KRG (K3)HAS SIGNED ON.  CLEAR CHANNEL (NU) IS NOT IN THE LIST OF           
*        SUBSIDIARIES ON THE KRG REP RECORD, AND ENTERING 'NU' IN THE           
*        FIELD WOULD RESULT IN AN ERROR IF NOT EXPLICITLY ACCEPTED.             
*                                                                               
         CLC   =C'NU',DE1REP       YES - CLEAR CHANNEL AS SUB?                  
         BE    VREP0030            YES - ACCEPT THIS INPUT                      
VREP0010 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,1               SET REP RECORD TYPE                          
         MVC   KEY+25(2),AGENCY    INSERT ALPHA ID INTO KEY                     
         GOTOR HIGHCTR                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                REP MUST BE ON FILE                          
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
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
         DROP  R2                                                               
***>>>                                                                          
**********************************************************************          
*  VALIDATE RECORD ROUTINE:                                          *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
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
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   VR05                                                             
         CLC   =C'CONFIRM',DE1HDLN                                              
         BE    VR03                                                             
         CLC   =C'KILLEM',DE1HDLN                                               
         BE    VR03                                                             
*                                                                               
         B     VR05                                                             
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
         GOTO1 CALLOV,DMCB,DE1TAGH,X'D9083FE4'                                  
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'SELECTKY),SELECTKY                                         
         BNE   MISSREC                                                          
*        OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
                                                                                
         MVC   DAREDKAD,KEY+28                                                  
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         GOTO1 =A(CHGPFLN),RR=RELO                                              
*                                                                               
VR50     DS    0H                                                               
         LA    RF,CCONLEN                                                       
         XCEF  CCONNUM,(RF)        CLEAR ALL CONTRACT GLOBAL VALUES             
         MVC   SELCTKEY,0(R6)      SAVE FOR WHEN WE RETURN TO LIST              
         MVC   AORDNUM,RDARKORD                                                 
* SHOULDN'T CLEAR, LINKING DEPENDS ON THIS                                      
* HQIU   XC    DE1HDLN,DE1HDLN     CLEAR HEADLINE FIELD                         
* HQIU   MVI   DE1HDLNH+5,0        SET FIELD LENGTH TO ZERO                     
         CLC   =C'O=',DE1HDLN      CLEAR IF IT IS IN O=AGYORD# FORMAT           
         BNE   *+14                                                             
         XC    DE1HDLN,DE1HDLN                                                  
         MVI   DE1HDLNH+5,0                                                     
*                                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    VR60                                                             
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(10,DE1HDLN),ALIGN=LEFT                             
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
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         MVC   SVDARSTD,RDARESST                                                
         MVC   SVDAREND,RDARESEN                                                
         CLI   RDARMEDI,C'R'       RADIO?                                       
         BNE   VR153A                                                           
*                                                                               
         OI    MISCFLG4,MF4RADIO   YES, RADIO EDI COULD LINK CON/BUY            
         B     VR153AA             SO NO NEED TO CHECK BUY                      
         DROP  R6                                                               
*                                                                               
VR153A   DS    0H                                                               
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   ERBUYFND                                                         
*                                                                               
VR153AA  DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   INVLFLD2                                                         
         GOTOR GETRCTR                                                          
                                                                                
* CHECK IF CONTRACT IS PENDING                                                  
         GOTOR VALLNK              LINKING VALIDATION                           
                                                                                
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
         GOTOR HIGHCTR                                                          
         MVC   HDRDA2,KEY+28       SAVE D/A OF DARE REC                         
*                                  RE-READ FOR PUTREC                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTOR GETRCTR                                                          
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
*                                                                               
         XC    DMCB,DMCB                                                        
         OI    DMCB,X'40'                                                       
         GOTOR DOAUDIT             DO AUDIT TRAIL                               
*                                                                               
* SHOW NEW PFKEY ACTIONS ALLOWED FOR LINKED ORDER                               
VR179    DS    0H                                                               
         GOTO1 =A(CHGPFLN),RR=RELO                                              
                                                                                
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   RECNTFND                                                         
         GOTOR GETRCTR                                                          
                                                                                
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
         GOTOR HIGHCTR                                                          
         MVC   HDRDA2,KEY+28       SAVE D/A OF DARE REC                         
*                                  RE-READ FOR PUTREC                           
         GOTOR GETRCTR                                                          
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   GFX                                                              
*                                                                               
         MVC   IMSVIO,AIO                                                       
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTOR GETRCTR                                                          
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
***********************************************************************         
* DOAUDIT                                                                       
* PARAMS: BYTE 1 = STATUS FLAG, X'40' = STATUS CHANGED BY LINKING               
*              2 = OPTION FLAG, X'80' = SAVE SALES PERSON CODE                  
***********************************************************************         
DOAUDIT  NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         OC    AUDITYP,AUDITYP                                                  
         BZ    DOAUDX                                                           
*                                                                               
         USING ADWORKD,R4                                                       
         MVC   ADSVFG(2),DMCB                                                   
         MVC   ADSVKEY,KEY                                                      
         XC    KEY,KEY                                                          
K        USING RDARKEY,KEY                                                      
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
         MVI   K.RDARKRT,X'70'     GET TRAILER RECORD                           
         DROP  K                                                                
*                                                                               
         GOTOR HIGHCTR                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DOAUDX                                                           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         MVC   ADSVIO,AIO                                                       
         LA    RE,ADIO                                                          
         ST    RE,AIO                                                           
         GOTOR GETRCTR                                                          
*                                                                               
         MVC   WORK(4),HELLO       RECORD DARE HISTORY                          
         MVC   WORK+4(4),DATCON                                                 
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,X'FF'        VALID ACTION                                 
         MVC   DMCB+5(1),AUDITYP   ACTION CONFIRM                               
         MVC   DMCB+6(1),REVNUM    REVISION NUMBER                              
         MVC   DMCB+7(1),ADSVFG                                                 
         GOTO1 =V(REGENDHT),DMCB,(ADSVFG2,ADIO),,WORK,SVSPCODE,RR=RELO          
*                                                                               
         GOTO1 PUTREC                                                           
         MVC   AIO,ADSVIO                                                       
*                                                                               
DOAUDX   DS    0H                                                               
         XC    AUDITYP,AUDITYP                                                  
         MVC   KEY,ADSVKEY                                                      
         GOTOR HIGHCTR                                                          
         XIT1                                                                   
         EJECT                                                                  
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
* EXITL IF NOT DE1LING WITH NEW FILTER                                          
******************************************************************              
TSTFLTR  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         TM    RDARMISC,X'10'+X'08'                                             
         BNZ   EXITH1              EXCLUDE VARIOUS, BRAND ORDER                 
         TM    RDARMISC,X'20'                                                   
         BO    EXITL1              EXCLUDE NOT DARE                             
*                                                                               
TST10    DS    0H                                                               
         CLI   STATFILT+1,C'E'     REVISION?                                    
         BNE   TST20                                                            
         CLI   STATFILT,C'1'                                                    
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
         CLI   STATFILT,C'1'                                                    
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
*                                                                               
         CLI   RDARBSTS,C'C'       RECALL?                                      
         BNE   *+12                                                             
         OI    RECSTATS,FTERRCAL                                                
         B     TST60                                                            
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   TST52A                                                           
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'10'      STACF?                                       
         BZ    TST52               NO                                           
         OI    RECSTATS+1,FTERSTCF                                              
         B     TST60               YES                                          
*                                                                               
TST52    DS    0H                                                               
         TM    RDARFLG1,X'80'      MATCH?                                       
         BZ    TST52A                                                           
         OI    RECSTATS+1,FTERMCH                                               
         B     TST55               CHECK IF -S                                  
*                                                                               
TST52A   DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
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
         BNE   TST55                                                            
         OI    RECSTATS+1,FTERAMD                                               
         B     TST55                                                            
*                                                                               
TST55    DS    0H                                                               
         GOTOR SSTACHK             ONLY FOR AMEND, OPEN, WE CHECK THIS          
*                                                                               
TST60    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*&&T1                                                                           
         CLC   RDARKORD,=XL4'00000000'                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
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
         TM    RECSTATS+1,FTERAMD                                               
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
         GOTOR GETRCTR             RETRIEVE AGENCY ORDER RECORD                 
         L     R6,AIO                                                           
U        USING RDARREC,R6                                                       
*                                                                               
         CLI   STAFILT,C'*'        STATION SET?                                 
         BNE   RAGO0020            NO  - SINGLE STATION                         
         LA    R2,RDARKSTA         PASS IN A(STATION IN ORDER)                  
         GOTO1 =A(CHECKSET),DMCB,(R2),RR=RELO                                   
         BNZ   RAGO0840            NOT IN SET                                   
         B     RAGO0040            FOUND IN SET                                 
RAGO0020 EQU   *                                                                
         CLI   DE1STATH+5,0        FILTER ON STATION?                           
         BE    RAGO0040            NO  - CHECK FOR MARKET FILTER                
         CLC   STAFILT(5),RDARKSTA SAME STATION?                                
         BNE   RAGO0840            NO  - EXIT CC NOT ZERO                       
RAGO0040 EQU   *                                                                
*                                                                               
*   NOW CHECK FOR A MARKET FILTER.  IF NOT PRESENT, EXIT WITH                   
*        CC ZERO.                                                               
         CLI   DE1MRKTH+5,0        ANY MARKET FILTER ENTERED?                   
         BE    RAGO0800            NO  - EXIT CC ZERO (ACCEPT)                  
*                                                                               
*   MARKET FILTER HAS BEEN REQUESTED.  MKT FIELD HAS "AAAA"                     
*        WHERE AAAA IS AN ALPHANUMERIC CODE IN WHICH THE RIGHT-                 
*        MOST POSITIONS MAY BE BINARY ZERO.                                     
*                                                                               
RAGO0080 EQU   *                                                                
         LA    R1,STASET           SET A(STATION TABLE)                         
RAGO0100 EQU   *                                                                
         CLI   0(R1),0             EMPTY SLOT?                                  
         BE    RAGO0840            YES - NOT FOUND: EXIT CC NOT ZERO            
*                                                                               
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    RAGO0840            YES - NOT FOUND: EXIT CC NOT ZERO            
*                                                                               
         CLC   0(5,R1),RDARKSTA    SAME STATION?                                
         BE    RAGO0800            EQUAL - EXIT CC ZERO                         
         LA    R1,5(R1)            BUMP TO NEXT SLOT                            
         B     RAGO0100            GO BACK FOR NEXT                             
RAGO0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     RAGO0900                                                         
RAGO0840 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
*                                                                               
         DROP  R6                                                               
RAGO0900 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHECKSET:  SCAN THE SET FOR STATION IN AGENCY ORDER.  IF NOT                
*        FOUND, RETURN CC NOT ZERO.                                             
*                                                                               
CHECKSET NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            LOAD A(STATION IN ORDER)                     
         L     R3,ASTASET          SET A(STATION SET)                           
CSET0020 EQU   *                                                                
         CLI   0(R3),X'00'         END OF TABLE REACHED?                        
         BE    CSET0900            YES - SET CC NOT ZERO                        
         CLC   0(5,R2),0(R3)       STATION IN TABLE?                            
         BE    CSET0800            YES - SET CC ZERO                            
         LA    R3,5(R3)            NO  - BUMP TO NEXT ENTRY IN TABLE            
         B     CSET0020            GO BACK FOR NEXT                             
CSET0800 EQU   *                                                                
         TM    QWSETFLG,X'80'      EXCLUSION FLAG SET?                          
         BO    CSET0950            YES - EXCLUDE THIS STATION                   
CSET0850 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     CSET0990                                                         
CSET0900 EQU   *                                                                
         TM    QWSETFLG,X'80'      EXCLUSION FLAG SET?                          
         BO    CSET0850            YES - INCLUDE THIS STATION                   
CSET0950 EQU   *                                                                
         LTR   RB,RB                                                            
CSET0990 EQU   *                                                                
         XIT1                                                                   
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   RECNTFND                                                         
*                                                                               
         GOTOR GETRCTR                                                          
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
VALLNK   NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         USING IMWORKD,R4                                                       
         MVC   IMSVKEY,KEY                                                      
         MVC   IMSVIO,AIO                                                       
*                                                                               
         NI    MISCFLG4,X'FF'-MF4TKO                                            
         L     R6,AIO              IS THIS A PENDING CONTRACT?                  
         MVI   ELCODE,X'1C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         OI    MISCFLG4,MF4TKO     YES, CONTRACT IS A TKO                       
*                                                                               
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
         BE    UWLK50                                                           
         CLI   RCONTYPE,C'X'                                                    
         BE    UWLK50                                                           
         CLI   RCONTYPE,C'D'       UNWIRED ORDER CAN'T BE LINKED TO             
         BNE   ERUWLK              NON-UNWIRED CONTRACT                         
*                                                                               
UWLK50   DS    0H                  CHECK FLIGHT DATE CONFLICT                   
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         XC    KEY,KEY                                                          
K        USING RPRDKEY,KEY                                                      
         MVI   K.RPRDKTYP,X'09'                                                 
         MVC   K.RPRDKADV,RCONKADV                                              
         MVC   K.RPRDKPRD,RCONPRD                                               
         MVC   K.RPRDKREP,RCONKREP                                              
         DROP  K                                                                
         GOTOR HIGHCTR                                                          
*                                                                               
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTOR GETRCTR                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'04'                                                     
         BRAS  RE,GETEL                                                         
         BNE   UWLK100                                                          
*                                                                               
         USING RPRDAGFL,R6                                                      
         GOTO1 DATCON,DMCB,(2,SVDARSTD),(3,ELEM)                                
         GOTO1 DATCON,DMCB,(2,SVDAREND),(3,ELEM+3)                              
*                                                                               
         CLC   ELEM(3),RPRDAGDF                                                 
         BL    UWLK80                                                           
         CLC   ELEM+3(3),RPRDAGDT                                               
         BH    UWLK80                                                           
         B     UWLK100                                                          
*                                                                               
UWLK80   DS    0H                                                               
         OC    RPRDAGDF(L'RPRDAGDF+L'RPRDAGDT),RPRDAGDF                         
         BNZ   ERPRDDAT            EMPTY?                                       
         TM    MISCFLG4,MF4TKO     TAKE OVER CONTRACT?                          
         BZ    ERPRDDAT            NO,ERROR MSG                                 
*                                                                               
UWLK100  DS    0H                                                               
         MVC   AIO,IMSVIO          NEED TO RESTORE GETREC SEQUENCE              
         MVC   KEY,IMSVKEY                                                      
         GOTOR HIGHCTR                                                          
         GOTOR GETRCTR                                                          
*                                                                               
VALX     XIT1                                                                   
         DROP  R6                                                               
         DROP  R4                                                               
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
* THIS ROUTINE DE1LS WITH LINKING ORDER WITH CONTRACT/ BUYLINES                 
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
         MVI   AUDITYP,DHSTACFQ    MARK STACF,WILL DO AUDIT TRAIL               
         GOTOR SCFDT               RECORD STACF DATE/TIME                       
         B     REDILNKX                                                         
LK300    DS    0H                                                               
         OI    RDARFLG1,X'C0'      -> MATCH-S                                   
         MVI   AUDITYP,DHMATCHQ                                                 
*                                                                               
REDILNKX DS    0H                                                               
         DROP  R4                                                               
         XIT1                                                                   
*                                                                               
***********************************************************************         
* CTRSTART:  START NEXT PAGE IF COUNTER OPTION ON PRIOR SCREEN                  
***********************************************************************         
CTRSTART NTR1  BASE=*,LABEL=*                                                   
         CLI   CTRFLAG,C'Y'        COUNTER FLAG SET TO YES?                     
         BNE   CSTA0800            NO  - EXIT                                   
         MVI   CTRFLAG,C'N'        YES - SET FLAG TO 'NO'                       
         MVC   DUB(4),AIO          SAVE CURRENT A(IO AREA)                      
         L     RF,AIO3                                                          
         ST    RF,AIO              SET AIO AREA                                 
         MVC   KEY(27),SAVRADKY                                                 
         GOTOR HIGHCTR                                                          
         CLC   KEY(27),KEYSAVE                                                  
* HQIU   BE    *+6                 PASSIVE KEYS COULD BE DELETED                
* HQIU   DC    H'0'                BY ROM ACTIONS(OPEN,LINK...)                 
         GOTOR GETRCTR             SHOULDN'T BE DELETED                         
         L     RF,AIO3                                                          
         MVC   KEY(27),0(RF)       PRIME KEY WITH 41/51 KEY                     
*                                     FROM SCREEN LIST                          
         MVC   AIO,DUB             RESTORE A(IO AREA)                           
CSTA0800 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* ROUTINE TO ADD STACF DATE/TIME ELEMENT TO DARE RECORD                         
**********************************************************************          
SCFDT    NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,X'50'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
K        USING RDARSTEM,ELEM                                                    
         MVI   ELEM,X'50'                                                       
         MVI   K.RDARSTLN,RDARSTLQ                                              
         GOTO1 DATCON,DMCB,(5,0),(2,K.RDARSTDT)                                 
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,K.RDARSTTM                                                  
         GOTO1 ADDELEM                                                          
         DROP  K                                                                
SCFDTX   EQU   *                                                                
EXIT5    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* VALKEY                                                                        
***********************************************************************         
SUBVKEY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,DE1RQTRH                                                      
         CLC   =C'I=',8(R2)        INBOX CHANGE REQUESTED?                      
         BE    SVKY0020            YES - REVALIDATE EACH TIME                   
*                                                                               
*   NOT AN INBOX CHANGE REQUEST:  CAN'T HAVE 'I' ON ANY SELECT FIELD            
*                                                                               
         GOTO1 =A(REASSCHK),RR=RELO                                             
         BNZ   ASSIGNNG                                                         
*                                                                               
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BZ    SVKY0180            NO                                           
*                                                                               
SVKY0020 EQU   *                                                                
         MVI   PAGEDETL,0          CLEAR FLAG TO 'NO DETAIL'                    
         CLC   =C'CTR',DE1RQTR     CTR REQUEST?                                 
         BNE   SVKY0040            NO                                           
         BAS   RE,TST4PRT2         COUNTER: CAN'T REQUEST PRINT                 
         BNZ   CONFERR             CONFLICTING OPTIONS: ERROR                   
         XC    ORDCOUNT,ORDCOUNT   CLEAR ORDER COUNTER                          
         MVI   PAGEDETL,1          CLEAR FLAG TO 'FULL DETAIL'                  
         OI    CTRCNTRL,X'80'      SET 'CTR PASS' FLAG                          
         OC    SCRNPAGE,SCRNPAGE   FIRST PAGE?                                  
         BZ    SVKY0040            YES - DON'T FAKE OUT                         
         MVI   PFKEY,3             YES - FAKE OUT SCROLL REQUEST                
         L     RF,SCRNPAGE                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SCRNPAGE         FAKE OUT PAGENUMBER FOR COUNT                
SVKY0040 EQU   *                                                                
         CLI   5(R2),0             YES - ANY DATA?                              
         BNE   SVKY0080            YES - CHECK OUT THE DATA                     
SVKY0060 EQU   *                                                                
         LA    RF,0                                                             
         ST    RF,SCRNPAGE         RESET SCREEN PAGE                            
         XC    REQINIT,REQINIT     NO  - CLEAR REPEATER                         
         OC    SAVRADKY,SAVRADKY   ANY RESTART KEY?                             
         BZ    SVKY0260            NO                                           
         XC    SAVRADKY+4(23),SAVRADKY+4                                        
*                                  CLEAR LOW ORDER KEY                          
         MVI   CTRFLAG,C'N'        SET FLAG TO 'NO'                             
         B     SVKY0260                                                         
*                                                                               
SVKY0080 DS    0H                                                               
         CLC   =C'I=',8(R2)        INBOX REASSIGN REQUEST?                      
         BNE   SVKY0160            NO                                           
         ZIC   RF,CONACTH+5        GET LENGTH OF ACTION FIELD                   
         BCTR  RF,0                BACK OFF 1 CHARACTER                         
         EX    RF,SVKY0100                                                      
         BE    NOREPASS            NO REASSIGN WHEN REPORTING                   
         B     SVKY0120                                                         
SVKY0100 CLC   CONACT(0),=C'REPORT'                                             
SVKY0120 EQU   *                                                                
         GOTO1 =A(CHKREASS),RR=RELO                                             
         BZ    SVKY0140            NO ERROR                                     
         CLI   DUB,3               CODE NOT ON FILE                             
         BE    BADREASS            ERROR                                        
         CLI   DUB,2               KEEP FROM EDI?                               
         BE    KEEPOFF2            ERROR                                        
         CLI   DUB,1               LEAVE DATE?                                  
         BE    SPLEFT2             ERROR                                        
SVKY0140 EQU   *                                                                
         CLI   DE1INBXH+5,0        ANY IN-BOX FILTER?                           
         BNE   SVKY0260            YES                                          
         LA    R2,DE1INBXH         SET CURSOR POSITION                          
         B     NEEDINBX            NO  -                                        
SVKY0160 DS    0H                                                               
         CLI   5(R2),3             THREE CHARACTERS INPUT?                      
         BNE   BADREQTR            NO  - ERROR                                  
         CLC   =C'CTR',DE1RQTR     COUNTER REQUEST?                             
         BE    SVKY0260            YES - DON'T SAVE INPUT IN REPEATER           
         MVC   REQINIT,DE1RQTR     YES - SAVE INPUT IN REPEATER                 
         B     SVKY0260                                                         
*                                                                               
SVKY0180 DS    0H                                                               
         CLI   DE1SCRLH+5,0        ANY SCROLL PAGE ENTERED?                     
         BE    SVKY0240            N                                            
         ZIC   R2,DE1SCRLH+5       EXTRACT LENGTH OF INPUT                      
         GOTO1 CASHVAL,DMCB,(0,DE1SCRL),(R2)                                    
         CLI   DMCB,X'FF'          INVALID VALUE?                               
         BNE   SVKY0200            NO                                           
         LA    RF,DE1SCRLH                                                      
         ST    RF,DUB                                                           
         MVC   RERROR,=AL2(951)                                                 
         B     VKERR                                                            
SVKY0200 EQU   *                                                                
         SR    RE,RE                                                            
         L     RF,DMCB+4           LOAD RESULT FOR DIVIDE                       
         LA    R1,100                                                           
         DR    RE,R1               DROP "PENNIES" FROM RESULT                   
         C     RF,SCRNPAGE         VALUE ENTERED VS CURRENT PAGE                
         BNH   SVKY0220                                                         
         LA    RF,DE1SCRLH                                                      
         ST    RF,DUB                                                           
         MVC   RERROR,=AL2(954)                                                 
         B     VKERR                                                            
SVKY0220 EQU   *                                                                
         LA    RF,1(RF)            BUMP REQUESTED PAGE # UP BY 1                
         ST    RF,SCRNPAGE         INSERT REQUESTED PAGE NUMBER                 
         MVI   PFKEY,3             FAKE OUT PFKEY HIT                           
         OI    CTRCNTRL,X'80'      SET 'CTR PASS + DISPLAY' FLAG                
SVKY0240 DS    0H                                                               
         CLC   REQINIT,=C'CTR'     IS REPEATER 'CTR'?                           
         BE    SVKY0260            YES - DON'T RESET WITH IT                    
         MVC   DE1RQTR,SPACES                                                   
         MVC   DE1RQTR(3),REQINIT  RESET INPUT FROM REPEATER                    
*                                                                               
SVKY0260 DS    0H                                                               
         CLI   ACTNUM,ACTLIST      ONLY FOR ACTION LIST                         
         BNE   VKNOERR             EXIT WITH NO ERROR                           
                                                                                
         MVI   NLISTS,14           SET TO SINGLE LINE DISPLAY                   
         LA    R2,DE1HDLNH                                                      
         CLI   5(R2),0                                                          
         BE    SVKY0380                                                         
         XC    HAGYORDR,HAGYORDR   CLEAR AGENCY ORDER FILTER                    
         CLC   =C'O=',DE1HDLN      AGENCY ORDER FILTER?                         
         BNE   SVKY0280            NO  -                                        
         CLI   DE1AGYH+5,0         AGENCY FILTER ENTERED?                       
         BE    NEEDAGY2            NO AGENCY FILTER: ERROR                      
         GOTO1 HEXIN,DMCB,DE1HDLN+2,HAGYORDR,8                                  
         B     SVKY0380                                                         
SVKY0280 EQU   *                                                                
*                                                                               
*   AS THERE IS NO CONTRACT AT THE MASTER LEVEL, THE USE OF THE                 
*        CAMPAIGN FILTER IS IGNORED IF THE SIGNON IS 'MASTER'.                  
*                                                                               
         TM    DE1HDLNH+4,X'08'    IS INPUT VALID NUMERIC?                      
         BZ    INVLFLD             NO, ERROR                                    
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,DARPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BO    SVKY0380            YES - DON'T PROCESS SINGLE                   
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   NLISTS,4            SHORT LIST W/CONTRACT HEADER                 
                                                                                
*        TM    4(R2),X'20'         PREVIOUSLY VALID?                            
*        BNZ   SVKY0380                                                         
                                                                                
         GOTO1 VALICON,DMCB,(R2)                                                
         BNZ   MISSREC                                                          
         OI    4(R2),X'20'                                                      
         XC    SELCTKEY,SELCTKEY                                                
                                                                                
         CLI   ACTNUM,ACTLIST      FOR LIST WITH CONTRACT HEADER                
         BNE   SVKY0380            SPECIFIED, STUFF FILTER WITH                 
         CLC   =C'C=',DE1CMP1      CAMPAIGN FILTER?                             
         BE    SVKY0380            YES - DON'T STUFF ANYTHING                   
SVKY0300 EQU   *                                                                
         MVC   DE1STAT(6),ESTATION INSERT FILTERS FROM CONTRACT                 
         OI    DE1STATH+4,X'20'                                                 
         MVI   DE1STATH+5,6                                                     
         OI    DE1STATH+6,X'80'    XMIT                                         
         CLC   DE1STAT+4(2),SPACES                                              
         BNE   *+10                                                             
         MVC   DE1STAT+4(2),=C'-T'                                              
                                                                                
         MVC   STAFILT,ESTATION    SAVE FOR LIST FILTERING                      
         CLI   STAFILT+4,C' '                                                   
         BNE   SVKY0320                                                         
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
         B     SVKY0340                                                         
SVKY0320 DS    0H                                                               
         CLI   STAFILT+4,C'-'                                                   
         BNE   SVKY0340                                                         
         MVC   STAFILT+4(1),STAFILT+5     SLIDE MEDIA DOWN                      
         MVI   STAFILT+5,C' '             CLEAR OLD MEDIA                       
         B     SVKY0340                                                         
SVKY0340 DS    0H                                                               
                                                                                
         OI    DE1AGYH+4,X'20'                                                  
         XC    DE1AGY,DE1AGY                                                    
         MVC   DE1AGY(4),CCONKAGY  AGENCY                                       
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    SVKY0360                                                         
         MVI   DE1AGY+4,C' '                                                    
         LA    RE,DE1AGY                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
SVKY0360 DS    0H                                                               
         MVC   AGYFILT,CDARAGY     SAVE FOR LIST FILTERING                      
         OI    DE1AGYH+6,X'80'     XMIT                                         
                                                                                
SVKY0380 DS    0H                                                               
*        MVI   MYRECTYP,X'41'      DEFAULT                                      
         LA    R2,DE1TYPEH                                                      
***      TM    4(R2),X'20'                                                      
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BNO   SVKY0400            NO  - DON'T RESET PAGE                       
*                                                                               
         LA    RF,1                                                             
         ST    RF,SCRNPAGE         SET SCREEN PAGE COUNTER TO 1                 
SVKY0400 EQU   *                                                                
         XC    SELCTKEY,SELCTKEY                                                
         MVC   STATFILT,DE1TYPE                                                 
         OC    STATFILT,SPACES                                                  
         LA    R3,STATLIST                                                      
SVKY0420 CLC   STATFILT(1),0(R3)                                                
         BE    SVKY0440                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    INVLFLD                                                          
         B     SVKY0420                                                         
*                                                                               
SVKY0440 DS    0H                                                               
         LA    R3,STATLIST                                                      
SVKY0460 CLC   STATFILT+1(1),0(R3)                                              
         BE    SVKY0480                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    INVLFLD                                                          
         B     SVKY0460                                                         
*                                                                               
* VARIOUS FILTER WILL ALWAYS BE PLACE IN STATFILT+1, ALL OTHER REGULAR          
* FILTERS WILL GO IN THE FIRST BYTE OF STATFILT                                 
*                                                                               
* REVISION FILTER WILL ALWAYS BE PLACE IN STATFILT+1, ALL OTHER REGULAR         
* FILTERS WILL GO IN THE FIRST BYTE OF STATFILT                                 
*                                                                               
SVKY0480 DS    0H                                                               
         CLI   STATFILT,C'V'                                                    
         BNE   SVKY0500                                                         
         MVC   STATFILT(1),9(R2)                                                
         MVI   STATFILT+1,C'V'                                                  
         OC    STATFILT,SPACES                                                  
         B     SVKY0520                                                         
*                                                                               
SVKY0500 DS    0H                                                               
         CLI   STATFILT,C'E'                                                    
         BNE   SVKY0520                                                         
         MVC   STATFILT(1),9(R2)                                                
         MVI   STATFILT+1,C'E'                                                  
         OC    STATFILT,SPACES                                                  
         B     SVKY0520                                                         
*                                                                               
SVKY0520 DS    0H                                                               
         CLI   STATFILT,C'O'                                                    
         BNE   SVKY0540                                                         
         MVC   STATFILT(1),9(R2)                                                
         MVI   STATFILT+1,C'O'                                                  
         OC    STATFILT,SPACES                                                  
         B     SVKY0540                                                         
*                                                                               
SVKY0540 DS    0H                                                               
*        CLI   STATFILT,C'F'       CONFIRMED FILTER?                            
*        BNE   *+8                 NO                                           
*        MVI   MYRECTYP,X'51'      YES - USE 51 RECORDS                         
         B     SVKY0560                                                         
*                                                                               
*                                                                               
* LIST OF VALID FILTERS                                                         
*                                                                               
STATLIST DC    C' ULBARNCSVWIE*FO123PM'                                         
         DC    X'FF'                                                            
*                                                                               
SVKY0560 DS    0H                  GROUP/SUBGROUP FILTER                        
         LA    R2,DE1GRPH                                                       
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY0580                                                         
         XC    SELCTKEY,SELCTKEY                                                
         XC    GRPFILT,GRPFILT                                                  
         CLI   5(R2),0                                                          
         BE    SVKY0580                                                         
         GOTO1 VALIGRP                                                          
         BNE   INVLGRP                                                          
         MVC   GRPFILT,8(R2)                                                    
         OC    GRPFILT,SPACES                                                   
                                                                                
SVKY0580 DS    0H                  DIV/TEAM FILTER                              
         LA    R2,DE1TEAMH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY0600                                                         
         XC    SELCTKEY,SELCTKEY                                                
         XC    TEAMFILT,TEAMFILT                                                
         CLI   5(R2),0                                                          
         BE    SVKY0600                                                         
         GOTO1 VALITEAM                                                         
         BNE   INVLTEAM                                                         
         MVC   TEAMFILT,8(R2)                                                   
         OC    TEAMFILT,SPACES                                                  
                                                                                
SVKY0600 DS    0H                                                               
         LA    R2,DE1UNWH          ANY FLAG IN UNWIRED?                         
         TM    DE1UNWH+4,X'20'     PREVIOUSLY VALID?                            
         BO    SVKY0660            YES - DON'T CHECK AGAIN                      
         NI    DE1REPH+4,X'FF'-X'20'                                            
*                                  NO  - FORCE REVALID OF REP FIELD             
         CLI   5(R2),0                                                          
         BE    SVKY0660            NO                                           
         CLC   =C'MR',AGENCY       KATZ TV?                                     
         BE    SVKY0620            YES                                          
         CLC   =C'K3',AGENCY       KATZ RADIO?                                  
         BE    SVKY0620            YES                                          
         CLC   =C'IR',AGENCY       INTEREP?                                     
         BE    SVKY0620            YES                                          
         CLC   =C'MS',AGENCY       TEST?                                        
         BNE   SVKY0640            NO                                           
SVKY0620 EQU   *                                                                
         CLI   8(R2),C'Y'          MASTER: CAN ONLY ASK FOR UNWIRED             
         BNE   MASUNWNG            ERROR                                        
SVKY0640 EQU   *                                                                
         CLI   8(R2),C'Y'          YES ENTERED?                                 
         BE    SVKY0660            YES                                          
         CLI   8(R2),C'N'          NO  ENTERED?                                 
         BNE   INVLUNW             NO                                           
SVKY0660 EQU   *                                                                
*                                                                               
         LA    R2,DE1STATH                                                      
         CLI   8(R2),C'*'          SET REQUEST?                                 
         BNE   SVKY0665            NO  -                                        
*                                  YES -                                        
*   MUST REFRESH SET FOR EACH SCREEN.  TWA IS CLEARED, AND                      
*        MECHANISM TO SAVE AND RESTORE IT IS NOT KEEPING THE                    
*        TABLE USED FOR THE STATION SET.                                        
*                                                                               
         GOTO1 =A(VSTASET),RR=RELO                                              
         BNZ   SVKY0675            ERROR: SET NOT FOUND                         
         MVC   STAFILT(5),DE1STAT  SAVE FILTER                                  
         B     SVKY0700            SAVED: CHECK MKT FILTER                      
SVKY0665 EQU   *                                                                
*                                                                               
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY0700            NOT CHANGED                                  
         XC    SELCTKEY,SELCTKEY                                                
         XC    STAFILT,STAFILT                                                  
         CLI   5(R2),0             ANY STATION FILTER?                          
         BE    SVKY0700            NO  - CHECK MARKET FILTER                    
SVKY0670 EQU   *                                                                
         GOTO1 VALISTA                                                          
         BZ    SVKY0680            ACCEPTED                                     
         GOTOR VALISTA2                                                         
         BZ    SVKY0680            ACCEPTED                                     
SVKY0675 EQU   *                                                                
         LA    R2,DE1STATH         SET A(CURSOR)                                
         ST    R2,DUB                                                           
         MVC   RERROR,=AL2(150)                                                 
         B     VKERR                                                            
SVKY0680 EQU   *                                                                
         MVC   STAFILT,WORK                                                     
         OC    STAFILT,SPACES                                                   
         CLI   STAFILT+4,C' '                                                   
         BNE   SVKY0700                                                         
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
SVKY0700 EQU   *                                                                
         LA    R2,DE1MRKTH                                                      
         CLI   5(R2),0             ANY MARKET FILTER?                           
         BE    SVKY0760            NO  - CHECK AGENCY FILTER                    
*                                                                               
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY0760                                                         
         ZIC   R1,CONRECH+5                                                     
         LTR   R1,R1               NO INPUT                                     
         BZ    SVKY0740                                                         
         BCTR  R1,0                                                             
         EX    R1,SVKY0720         LOOK FOR 'CAMPAIGN'                          
         BNE   SVKY0740                                                         
         B     CMPMKTER            MKT= + CAMPAIGN INVALID                      
*                                                                               
SVKY0720 CLC   CONREC(0),=C'CAMPAIGN'                                           
*                                                                               
SVKY0740 EQU   *                                                                
         GOTO1 =A(VALIMKT),RR=RELO                                              
         BNZ   INVLMKT                                                          
*                                                                               
*   MARKET IS VALID:  NOW TABLE UP STATIONS IN MARKET.                          
*                                                                               
         GOTO1 =A(SETSTAS),RR=RELO                                              
         B     SVKY0760                                                         
SVKY0760 DS    0H                                                               
         LA    R2,DE1AGYH                                                       
                                                                                
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY0840                                                         
                                                                                
         XC    SELCTKEY,SELCTKEY                                                
         XC    AGYFILT,AGYFILT                                                  
                                                                                
         CLI   5(R2),0                                                          
         BE    SVKY0840                                                         
                                                                                
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
         BE    SVKY0780                                                         
         CLI   0(RE),C' '                                                       
         BE    SVKY0800                                                         
         LA    RE,1(RE)                                                         
         B     *-20                                                             
                                                                                
SVKY0780 MVC   RAGK2AOF,1(RE)      AGENCY OFFICE                                
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,MOVEAGY                                                       
         B     SVKY0820                                                         
*                                                                               
MOVEAGY  MVC   RAGK2AGY(0),WORK                                                 
*                                                                               
SVKY0800 MVC   RAGK2AGY(4),WORK                                                 
         OC    RAGK2AOF,SPACES     SPACE PAD OFFICE                             
         DROP  R6                                                               
*                                                                               
SVKY0820 DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   MISDARAG            CHECK IF EQUIVALENCY CODE PRESENT            
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
         L     R6,AIO                                                           
         USING RAGY2REC,R6                                                      
         MVC   AGYFILT,RAGY2DAR                                                 
         DROP  R6                                                               
                                                                                
         OC    AGYFILT,AGYFILT     CHECK IF EQUIVALENCY CODE PRESENT            
         BZ    MISDARAG                                                         
         CLC   AGYFILT,SPACES                                                   
         BE    MISDARAG                                                         
*                                                                               
SVKY0840 DS    0H                                                               
         LA    R2,DE1OFFH                                                       
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY1080                                                         
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
         BZ    SVKY0860                                                         
         CLI   SIGNONID+4,C' '                                                  
         BE    SVKY1080                                                         
*                                                                               
SVKY0860 DS    0H                                                               
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    SVKY0880                                                         
         DROP  R4                                                               
*                                                                               
         CLI   SIGNONID+4,C'L'                                                  
         BE    SVKY1080                                                         
*                                                                               
SVKY0880 DS    0H                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    SVKY1040                                                         
         CLC   =X'800F',TWAAUTH    CHECK ACCESS                                 
         BE    SVKY1040                                                         
         CLC   =C'O=',TWAACCS                                                   
         BNE   SVKY1040                                                         
*                                                                               
*   ALL REQUESTS WILL NOW BE TREATED THE SAME:  STUFFING THE                    
*        SCREEN FIELD FOR FILTERING OFFICE.                                     
*                                                                               
*                                                                               
**       CLI   DE1HDLNH+5,0        ANYTHING IN CONTRACT FIELD?                  
**       BNE   SVKY0900            YES -                                        
*                                                                               
         CLI   5(R2),0             DEFAULT OFFICE RESTRICTION                   
         BNE   SVKY0920                                                         
         MVC   DE1OFF(2),TWAACCS+2 STUFF OFFICE INTO SCREEN FILTER              
         MVI   DE1OFFH+5,2         STUFF DATA LENGTH INTO HEADER                
         B     SVKY1060            GO AND STUFF INTO FILTER BUFFER              
*                                                                               
SVKY0900 DS    0H                                                               
*                                                                               
*   THIS WILL NEVER BE REACHED.                                                 
*                                                                               
         CLC   =C'BRAND',CONREC                                                 
         BE    SVKY1060                                                         
         MVC   DE1OFF(2),=C'C='                                                 
         MVC   DE1OFF+2(2),TWAACCS+2                                            
         MVI   DE1OFFH+5,4                                                      
         B     SVKY1060                                                         
*                                                                               
SVKY0920 DS    0H                                                               
         CLC   =C'BL',AGENCY                                                    
         BNE   SVKY0940                                                         
         CLC   =C'SA',TWAACCS+2                                                 
         BNE   SVKY1020                                                         
         CLC   =C'PO',DE1OFF                                                    
         BE    SVKY1060                                                         
         CLC   =C'C=PO',DE1OFF                                                  
         BE    SVKY1060                                                         
         B     SVKY1020                                                         
SVKY0940 DS    0H                                                               
         CLC   =C'PV',AGENCY                                                    
         BNE   SVKY0960                                                         
         CLC   =C'SE',TWAACCS+2                                                 
         BNE   SVKY1020                                                         
         CLC   =C'PO',DE1OFF                                                    
         BE    SVKY1060                                                         
         CLC   =C'C=PO',DE1OFF                                                  
         BE    SVKY1060                                                         
SVKY0960 DS    0H                                                               
         CLC   =C'CQ',AGENCY                                                    
         BNE   SVKY0980                                                         
         CLC   =C'LA',TWAACCS+2                                                 
         BNE   SVKY1000                                                         
         CLC   =C'SF',DE1OFF                                                    
         BE    SVKY1060                                                         
         CLC   =C'C=SF',DE1OFF                                                  
         BE    SVKY1060                                                         
*                                                                               
* SPECIAL CODING UNTIL C-FILE RESOLVES THIS OFFICE CODE MAPPING PROB.           
*                                                                               
SVKY0980 DS    0H                                                               
         CLC   =C'SZ',AGENCY                                                    
         BE    SVKY1000                                                         
         CLC   =C'AM',AGENCY                                                    
         BNE   SVKY1020                                                         
*                                                                               
SVKY1000 DS    0H                                                               
         CLC   =C'DV',TWAACCS+2                                                 
         BNE   SVKY1020                                                         
         CLC   =C'DN',DE1OFF                                                    
         BE    SVKY1060                                                         
         CLC   =C'DV',DE1OFF                                                    
         BNE   SVKY1020                                                         
         MVC   DE1OFF(2),=C'DN'                                                 
         B     SVKY1060                                                         
*********                                                                       
SVKY1020 DS    0H                                                               
         CLC   DE1OFF(2),TWAACCS+2                                              
         BE    SVKY1040                                                         
         CLC   =C'C=',DE1OFF                                                    
         BNE   NOACCESS                                                         
         CLC   DE1OFF+2(2),TWAACCS+2                                            
         BNE   NOACCESS                                                         
*                                                                               
SVKY1040 DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    SVKY1080                                                         
*                                                                               
SVKY1060 DS    0H                                                               
         MVC   OFFFILT,8(R2)                                                    
*                                                                               
SVKY1080 DS    0H                                                               
         LA    R2,DE1SALPH                                                      
         TM    DE1SALPH+4,X'20'                                                 
         BO    SVKY1120                                                         
         XC    SELCTKEY,SELCTKEY                                                
         XC    SALFILT,SALFILT                                                  
         CLI   5(R2),0                                                          
         BE    SVKY1120                                                         
         CLI   DE1OFFH+5,0         SALESPERSON FILTER NEEDS OFFICE              
         BNE   SVKY1100                                                         
         LA    R2,DE1OFFH                                                       
         B     NEEDOFF                                                          
                                                                                
SVKY1100 DS    0H                                                               
         GOTO1 VALISAL             REVALIDATE EVERYTIME                         
         BNE   INVLSAL                                                          
**HQ     CLC   OFFFILT,WORK+20     SALESPERSON OFFICE MUST MATCH OFFICE         
**HQ     BNE   INVLSAL             FILTER                                       
         MVC   SALFILT,8(R2)                                                    
         OC    SALFILT,SPACES                                                   
*                                                                               
SVKY1120 DS    0H                                                               
         LA    R2,DE1REPH                                                       
***      TM    DE1REPH+4,X'20'     VALIDATE REP EVERY TIME.                     
***      BO    SVKY1140                                                         
         XC    COMPREP,COMPREP                                                  
         CLI   5(R2),0             ANY INPUT IN FIELD?                          
         BE    SVKY1140            NO                                           
         GOTO1 =A(VALIREP),RR=RELO REVALIDATE EVERYTIME                         
         BNZ   INVLREP             REP NOT FOUND                                
*                                                                               
SVKY1140 DS    0H                                                               
         LA    R2,DE1RDATH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY1180                                                         
         XC    SELCTKEY,SELCTKEY                                                
         XC    FILTDAT1(4),FILTDAT1                                             
         CLI   5(R2),0                                                          
         BE    SVKY1180                                                         
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),DATEBLCK                               
         CLI   DMCB+4,0            FIELDS 1 & 2 VALID                           
         BE    SVKY1160                                                         
         CLI   DMCB+4,4            ONLY ONE DATE INPUT                          
         BNE   INVRDATE                                                         
*                                                                               
SVKY1160 MVC   FILTDAT1,DATEBLCK+34     COMPRESSED FIRST DATE                   
         MVC   FILTDAT2,DATEBLCK+36     COMPRESSED LAST DATE                    
*                                                                               
SVKY1180 EQU   *                                                                
         LA    R2,DE1RDATH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY1200            NOT CHANGED                                  
         CLI   5(R2),0             CHANGED: ANY DATA THIS TIME?                 
         BE    SVKY1200            NO                                           
         XC    SELCTKEY,SELCTKEY   YES - CLEAR SELECT KEY                       
*                                                                               
SVKY1200 EQU   *                                                                
         LA    R2,DE1CMP1H                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BNO   SVKY1220            CHANGED                                      
         LA    R2,DE1CMP2H                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BNO   SVKY1220            CHANGED                                      
         LA    R2,DE1CMP3H                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    SVKY1380            NO CHANGE                                    
SVKY1220 EQU   *                                                                
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
         BE    SVKY1380            NO  - FIRST FIELD SPEAKS FOR ALL             
         CLC   =C'C=',DE1CMP1      YES - PULL FILTER FROM CONTRACT?             
         BE    SVKY1260            YES                        ,                 
SVKY1240 EQU   *                                                                
         CLI   DE1AGYH+5,0         AGENCY FILTER ENTERED?                       
         BE    NEEDAGY             NO AGENCY FILTER: ERROR                      
***      CLI   DE1CMP2H+5,0        NO  - ALL 3 FIELDS MUST BE FILLED            
***      BE    INVLCAMP            EMPTY:  ERROR                                
***      CLI   DE1CMP3H+5,0                                                     
***      BE    INVLCAMP            EMPTY:  ERROR                                
*                                                                               
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
*                                                                               
*   VALIDATION FOR THESE FIELDS IS UNCERTAIN.  MUST BE TESTED.                  
*                                                                               
         MVC   CAMPFLD1,DE1CMP1    USE AS ENTERED                               
         OC    CAMPFLD1,SPACES                                                  
         MVC   CAMPFLD2,DE1CMP2    USE AS ENTERED (IF NOT PRESENT,              
         OC    CAMPFLD2,SPACES        WILL SHOW AS SPACES)                      
         CLI   DE1CMP3H+5,0        ESTIMATE NUMBER ENTERED?                     
         BE    SVKY1250            NO  - CLEAR CAMPFLD3                         
         LA    R2,DE1CMP3H         SET A(ESTIMATE FIELD)                        
         GOTO1 VPACK               PACK ESTIMATE NUMBER                         
         LTR   R0,R0                                                            
         BZ    BADEST#             ERROR IN INPUT                               
         SPACE 1                                                                
         STCM  R0,15,CAMPFLD3      STORE CAMPAIGN ESTIMATE #                    
*                                                                               
         B     SVKY1380                                                         
SVKY1250 EQU   *                                                                
         MVC   CAMPFLD3,SPACES     CLEAR EST # TO SPACES                        
         B     SVKY1380                                                         
         DROP  R4                                                               
SVKY1260 EQU   *                                                                
         CLI   DE1HDLNH+5,0        CONTRACT NUMBER ENTERED?                     
         BE    NEEDCON             NO CONTRACT NUMBER: ERROR                    
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),CCONDKAD  INSERT D/A OF CON                            
         GOTOR GETRCTR                                                          
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
         BE    SVKY1280                                                         
         MVI   DE1AGY+4,C' '                                                    
         LA    RE,DE1AGY                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
SVKY1280 DS    0H                                                               
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
SVKY1300 EQU   *                                                                
         CLI   0(R1),C' '          VALID CHARACTER IN FIELD?                    
         BH    SVKY1320            YES - FINISHED                               
         BCTR  R1,0                BACK UP ONE CHARACTER                        
         BCT   R0,SVKY1300         DECREMENT ONE COUNT                          
*                                  DROP THROUGH WILL RETURN ERROR               
SVKY1320 EQU   *                                                                
         STC   R0,DE1CMP3H+5       SET FIELD LENGTH TO WHAT'S LEFT              
SVKY1340 EQU   *                                                                
         CLI   0(R1),C'0'                                                       
         BL    SVKY1360            LESS THAN X'F0': NON-NUMERIC                 
         CLI   0(R1),C'9'                                                       
         BH    SVKY1360            MORE THAN X'F9': NON-NUMERIC                 
         BCTR  R1,0                BACK UP ONE CHARACTER                        
         BCT   R0,SVKY1340         DECREMENT ONE COUNT                          
*                                  DROP-THROUGH:  ALL NUMERIC                   
         OI    DE1CMP3H+4,X'08'    TURN ON VALID NUMERIC                        
SVKY1360 EQU   *                                                                
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
SVKY1380 DS    0H                                                               
         OI    DE1TYPEH+4,X'20'                                                 
         OI    DE1GRPH+4,X'20'                                                  
         OI    DE1TEAMH+4,X'20'                                                 
         OI    DE1STATH+4,X'20'                                                 
         OI    DE1AGYH+4,X'20'                                                  
         OI    DE1OFFH+4,X'20'                                                  
         OI    DE1SALPH+4,X'20'                                                 
         OI    DE1UNWH+4,X'20'                                                  
         OI    DE1REPH+4,X'20'                                                  
         OI    DE1RDATH+4,X'20'                                                 
         OI    DE1RQTRH+4,X'20'                                                 
         OI    DE1CMP1H+4,X'20'                                                 
         OI    DE1CMP2H+4,X'20'                                                 
         OI    DE1CMP3H+4,X'20'                                                 
         OI    DE1MRKTH+4,X'20'                                                 
VKNOERR  EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     SVKY9990                                                         
VKERR    EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
*                                                                               
SVKY9990 DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO TEST FOR ANY PRINT CODE ON LIST ACTION COLUMN                      
* ON EXIT, CC IS SET NON-ZERO IF 'PRINT' HAS BEEN SELECTED                      
*******************************************************************             
TST4PRT2 NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TST2PNO                                                          
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
TST20040 DS    0H                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TST20060                                                         
         CLI   8(R2),C'P'          PRINT REQUEST?                               
         BE    TST2PNO             'P' FOUND: RETURN NOT ZERO CC                
         CLI   8(R2),C'I'          INBOX CHANGE REQUEST?                        
         BE    TST2PNO             'I' FOUND: RETURN NOT ZERO CC                
         CLI   8(R2),C'$'          INBOX CHANGE-ALL REQUEST?                    
         BE    TST2PNO             '+' FOUND: RETURN NOT ZERO CC                
*                                                                               
TST20060 ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0             END OF SCREEN?                               
         BE    TST2PYES            YES - NO 'P'/'I'/'+' FOUND                   
         TM    1(R2),X'20'         SKIP IF PROTECTED                            
         BO    TST20060                                                         
         B     TST20040            CHECK NEXT LINE                              
*                                                                               
TST2PYES SR    RC,RC               SET CC ZERO                                  
TST2PNO  LTR   RC,RC               TEST CC FOR SETTING                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION SET CODE                                                     
***********************************************************************         
VSTASET  NTR1  LABEL=*,BASE=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),AGENCY                                                 
         MVC   KEY+21(2),=C'ST'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
         GOTOR HIGHCTR             LOOK FOR EXACT KEY                           
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   VSET0900            NO                                           
VSET0800 EQU   *                                                                
         GOTO1 =A(LOADSET),RR=RELO LOAD STATION SET                             
         SR    R0,R0               SET CC ZERO                                  
         B     VSET0990                                                         
VSET0900 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
VSET0990 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PHYSICAL LOAD OF STATION SET                                                  
***********************************************************************         
LOADSET  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         MVC   ANEXTSET,ASTASET    SET A(FIRST ENTRY IN STATION SET)            
         MVI   SETBYTE,C'N'        SET OF SETS --> NO                           
         GOTOR GETRCTR             RETRIEVE RECORD                              
         L     RF,ASTASET          SET A(ADDR OF SET IN QWKD AREA)              
         L     R3,AIO              SET A(IOAREA)                                
         USING RSETREC,R3                                                       
         LA    R4,RSETELEM                                                      
         CLI   0(R4),X'01'         NEW FORMAT/01 DESC ELT?                      
         BNE   LSET0060            NO  - USE OLD FORMAT RECORD                  
         MVI   QWSETFLG,0          CLEAR EXCLUDE SET FLAG                       
         TM    RSET1FLG-RSET1DES(R4),X'08'                                      
*                                  EXCLUSION SET?                               
         BNO   LSET0020            NO                                           
         OI    QWSETFLG,X'80'      YES - SET A(EXCLUDE FLAG BYTE)               
LSET0020 EQU   *                                                                
         TM    RSET1FLG-RSET1DES(R4),X'80'                                      
*                                  SET OF SETS?                                 
         BNO   LSET0060            NO  -                                        
         L     RF,ASETSET          SET A(SET OF SETS)                           
         L     R6,AIO                                                           
         MOVE  ((RF),500),(R6)                                                  
*                                  YES - SAVE SET OF SETS RECORD                
*                                     SET OF SETS WILL NEVER BE VERY            
*                                     LONG RECORDS                              
*                                                                               
         L     R4,ASETSET          ADDRESS R4 TO NEW AREA                       
         LA    R4,34(R4)           BUMP TO FIRST ELEMENT                        
*                                                                               
         MVI   SETBYTE,C'Y'        SET OF SETS = YES                            
         ZIC   RF,1(R4)            BUMP TO X'10' ELEMENT                        
         AR    R4,RF                                                            
         ZIC   RF,1(R4)            BUMP TO X'20' ELEMENT                        
         AR    R4,RF                                                            
         CLI   0(R4),X'20'         20 ELEMENT IN SET OF SETS                    
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND: 1ST ELT MUST BE THERE             
         ZIC   RF,1(R4)            SET END OF RECORD INDICATOR                  
         LR    RE,R4                                                            
         AR    RE,RF                                                            
         XC    0(4,RE),0(RE)       SET TO ZERO                                  
*                                                                               
         LA    R4,3(R4)            BUMP TO 1ST ELEMENT IN SET                   
LSET0040 EQU   *                                                                
         MVC   KEY+23(4),0(R4)     INSERT SET CODE FROM SET OF SETS             
         GOTOR HIGHCTR             RETRIEVE RECORD FOR SET                      
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   LSET0080            NO  - SKIP IT                                
         GOTOR GETRCTR             RETRIEVE SET RECORD                          
*                                                                               
LSET0060 EQU   *                                                                
         BAS   RE,LOADSET2         UNLOAD SET TO STORAGE                        
         CLI   SETBYTE,C'N'        SET OF SETS?                                 
         BE    LSET0100            NO  - SINGLE SET LOADED                      
LSET0080 EQU   *                                                                
         LA    R4,4(R4)            BUMP TO NEXT SET CODE                        
         OC    0(4,R4),0(R4)       ANY SET CODE?                                
         BNZ   LSET0040            YES - GO BACK FOR NEXT                       
LSET0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   LOADSET2:  UNLOADS INDIVIDUAL SET RECORD TO STORAGE.   SET UP               
*              AS SUBROUTINE TO PRESERVE REGISTERS USED.                        
*                                                                               
LOADSET2 NTR1                                                                   
         LA    R4,RSETELEM         SET R4 LOCALLY TO DETAIL SET REC             
         CLI   0(R4),1             NEW/OLD FORMAT ELEMENT?                      
         BNE   LSET2015            OLD FORMAT - NO X'01' ELEMENT                
*                                     ALREADY AT X'10'                          
         ZIC   RF,1(R4)            BUMP TO X'10' ELEMENT                        
         AR    R4,RF                                                            
LSET2015 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO X'20' ELEMENT                        
         AR    R4,RF                                                            
         CLI   0(R4),X'20'                                                      
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND: 1ST ELT MUST BE THERE             
         ST    R4,SAVEREGX         SAVE A(ELT BEING PROCESSED)                  
         ZIC   RF,1(R4)            GET ELEMENT LENGTH                           
         LA    RE,3                SUBTRACT CONTROL                             
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'5'            DIVIDE BY SIZE OF ENTRY                      
         L     RE,ANEXTSET         SET A(NEXT SLOT) IN WORKSPACE                
         LA    R4,3(R4)            A(1ST GROUP/SUBGRP IN SET)                   
LSET2020 EQU   *                                                                
         LA    R1,5                SET SIZE OF ENTRY                            
         EX    R1,LSETMVC          MOVE  ENTRY BY LENGTH                        
         AH    RE,=H'5'            BUMP TO NEXT SLOT                            
         EX    R1,LSETXC           CLEAR ENTRY BY LENGTH                        
         B     LSET2025                                                         
*                                                                               
LSETMVC  MVC   0(0,RE),0(R4)                                                    
LSETXC   XC    0(0,RE),0(RE)       CLEAR THE NEXT SLOT: SETS END                
*                                                                               
LSET2025 EQU   *                      OF SET VALUE OF X'00'                     
         AH    R4,=H'5'            BUMP TO NEXT SLOT                            
         BCT   RF,LSET2020         LOOP                                         
         ST    RE,ANEXTSET         SAVE NEXT SLOT FOR NEXT ELT                  
         L     R4,SAVEREGX         RESET A(ELT BEING PROCESSED)                 
LSET2030 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         CLI   0(R4),0             END OF RECORD?                               
         BE    LSET2040            YES                                          
         CLI   0(R4),X'20'         SET ELEMENT?                                 
         BNE   LSET2030            NO  - SKIP IT                                
         ST    R4,SAVEREGX         YES - SAVE A(ELT BEING PROCESSED)            
         ZIC   RF,1(R4)            GET ELT LENGTH                               
         LA    RE,3                SUBTRACT CONTROL                             
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'5'            DIVIDE BY ENTRY LENGTH                       
         LA    R4,3(R4)            A(1ST ENTRY IN ELT)                          
         L     RE,ANEXTSET         RESTORE NEXT SLOT FOR NEW ELT                
         B     LSET2020            GO BACK AND LOAD ELEMENTS                    
LSET2040 EQU   *                                                                
         ST    RE,ANEXTSET         SET A(NEXT OPEN SLOT)                        
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**<<<<<----->>>>>                                                               
***********************************************************************         
* STATION NOT FOUND:  IF KRG, MAY BE A CLEAR CHANNEL STATION NOT                
*        REPRESENTED AS A MASTER STATION FOR KRG.                               
***********************************************************************         
VALISTA2 NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'K3',AGENCY       KRG?                                         
         BNE   VSTA0800            NO  - EXIT CC NOT ZERO                       
         XC    KEY,KEY             YES                                          
         MVI   KEY,2               SET STATION TYPE                             
         MVC   KEY+20(2),=C'NU'    FORCE CLEAR CHANNEL                          
         MVC   KEY+22(5),WORK      INSERT STATION                               
         GOTOR HIGHCTR             READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND FOR CLEAR CHANNEL?                 
         BE    VSTA0900            YES - EXIT CC ZERO                           
*                                  NO                                           
VSTA0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     VSTA0900            EXIT                                         
VSTA0900 EQU   *                                                                
         XIT1                                                                   
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BNE   CHKPDNO                                                          
*        DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTOR GETRCTR                                                          
                                                                                
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
         OI    O1+1,FTERAMD+FTERMCH+FTERSTCF                                    
         OI    E1,FTERREV+FTERNEW+FTEROPEN+FTERRSNT+FTERCFAP                    
         OI    E1+1,FTERAMD                                                     
         OI    O2,FTERRJCT+FTERRCAL                                             
         OI    O2+1,FTERAMDS                                                    
         OI    E2,FTERREV+FTERRJCT+FTERRCAL                                     
         OI    E2+1,FTERAMDS                                                    
         OI    O3,FTEROPS                                                       
         OI    O3+1,FTERMCHS                                                    
         OI    E3,FTERREV+FTEROPS                                               
         OI    E3+1,FTERMCHS                                                    
         MVI   ONE,C'1'                                                         
         MVI   TWO,C'2'                                                         
         MVI   THREE,C'3'                                                       
         MVI   FLTEREND,X'00'                                                   
SETFILTX DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
* MULTI REASSIGNMENT CODE                                                       
***********************************************************************         
MULTIASS NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   NBOX0500                                                         
*                                                                               
         MVI   REASSALL,0          CLEAR 'REASSIGN ALL' FLAG                    
         LA    R4,MYLISTDA                                                      
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         B     NBOX0040                                                         
*                                                                               
NBOX0020 DS    0H                                                               
         LA    R4,4(R4)            NEXT DISK ADDRESS                            
                                                                                
NBOX0040 DS    0H                                                               
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   REASSALL,1          'REASSIGN ALL' SET ON?                       
         BE    NBOX0080            YES - TREAT AS 'I'                           
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    NBOX0060                                                         
         CLI   8(R2),C'$'          'REASSIGN ALL' REQUEST?                      
         BNE   NBOX0050            NO                                           
         MVI   REASSALL,1          YES - SET 'REASSIGN ALL' ON                  
         B     NBOX0080                                                         
NBOX0050 DS    0H                                                               
         CLI   8(R2),C'I'                                                       
         BE    NBOX0080                                                         
*                                                                               
NBOX0060 ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    NBOX0400            (E-O-S)                                      
         TM    1(R2),X'20'         SKIP IF PROTECTED                            
         BO    NBOX0060                                                         
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   NBOX0020            SELECT FIELD                                 
         B     NBOX0060                                                         
*                                                                               
NBOX0080 DS    0H                                                               
*        MVC   8(3,R2),SPACES      CLEAR ACTION                                 
         MVI   8(R2),C'*'          CLEAR ACTION TO 'DONE'                       
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
*                                  RETURNS KEY OF ORDER TO BE REASGNED          
         OC    0(4,R4),0(R4)                                                    
         BZ    NBOX0400            SELECTED EMPTY ENTRY, EXIT                   
         MVC   KEY+28(4),0(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
         L     R6,AIO                                                           
         MVC   SELECTKY,0(R6)                                                   
         MVC   SVSELKEY,SELECTKY                                                
         GOTO1 =A(REASSBAT),RR=RELO                                             
*                                  FOUND: DO A BATCH REASSIGNMENT               
         B     NBOX0060            GO BACK FOR NEXT ROW                         
*                                                                               
NBOX0400 SR    RC,RC                                                            
NBOX0500 LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO TEST FOR ANY IRSSC CHANGE ON LIST ACTION COLUMN                    
*        IF REQUESTOR IS NOT 'I='                                               
* IF YES, THIS IS AN ERROR, AND CC NOT ZERO IS RETURNED                         
*******************************************************************             
REASSCHK NTR1  LABEL=*,BASE=*                                                   
         CLC   =C'I=',DE1RQTR      REASSIGNMENT REQUEST?                        
         BE    RSSC0400            YES - EXIT CC ZERO                           
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   RSSC0400                                                         
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
RSSC0020 DS    0H                                                               
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    RSSC0060                                                         
         CLI   8(R2),C'I'          REASSIGNMENT REQUESTED FOR LINE?             
         BE    RSSC0500            YES - ERROR ENCOUNTERED                      
         CLI   8(R2),C'$'          REASSIGNMENT REQUESTED FOR ALL?              
         BE    RSSC0500            YES - ERROR ENCOUNTERED                      
*                                                                               
RSSC0060 ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    RSSC0400            (E-O-S)                                      
         TM    1(R2),X'20'         SKIP IF PROTECTED                            
         BO    RSSC0060                                                         
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   RSSC0020            SELECT FIELD                                 
         B     RSSC0060                                                         
*                                                                               
*                                                                               
RSSC0400 SR    RC,RC                                                            
RSSC0500 LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT A REPORT                                                                
***********************************************************************         
PR       NTR1  BASE=*,WORK=(R4,IMWORKQ),LABEL=*                                 
         USING IMWORKD,R4                                                       
         MVC   IMSVIO,AIO                                                       
         MVC   IMSVKEY,KEY                                                      
         MVC   IMSVKEY2,SELECTKY                                                
*                                                                               
         TM    BITFLAG,BFPRINT     CAME FROM MY SELECT SCREEN?                  
         BO    PR01                                                             
         BAS   RE,TEST4PRT                                                      
         BNZ   PRNO                CHECK IF ANYTHING TO PRINT                   
*                                                                               
PR01     DS    0H                                                               
         NI    MISCFLG4,X'FF'-MF4PRDIF                                          
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         OC    RDARREP#,RDARREP#   UNLINK OR REVISION UNLINK                    
         BZ    PR02A                                                            
*                                                                               
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
*        MVC   SVKEY,0(RE)                                                      
*                                                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RDARREP#                                              
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCONKEY,KEY                                                      
R        USING RCONREC,IMIO                                                     
         MVI   K.RCONPTYP,X'8C'                                                 
         MVC   K.RCONPREP,RDARKREP                                              
         MVC   K.RCONPCON,WORK                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         DROP  K                                                                
*                                                                               
         TM    R.RCONMODR,X'10'    IS THIS A PENDING CONTRACT?                  
         BZ    PR02A               YES, BUY LIST ONLY                           
         OI    MISCFLG4,MF4PRDIF                                                
*                                                                               
PR02A    DS    0H                                                               
*                                                                               
         TM    BITFLAG,BFPRINT     CAME FROM MY SELECT SCREEN?                  
         BZ    PR05                                                             
         TM    MISCFLG4,MF4PRDIF                                                
         BO    PR03                                                             
*                                                                               
PR02     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'30',0),('QPRONE',0)                                
         B     PR04                                                             
*                                                                               
PR03     DS    0H                                                               
         GOTO1 VREDAR24,DMCB,(RC),('QPRONE',0)                                  
*                                                                               
PR04     DS    0H                                                               
         GOTO1 VTOUCHED                                                         
         B     PR20                ALL DONE, CLOSE PQ                           
                                                                                
PR05     DS    0H                  PRINT FROM LIST SCREEN                       
         OI    CTLRFLG1,CF11STKY   SELECTION FOUND, REDISPLAY PAGE              
*                                                                               
         TM    MISCFLG4,MF4PRDIF                                                
         BO    PR08                                                             
PR06     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'30',0),0                                           
         B     PR10                                                             
PR08     DS    0H                                                               
         GOTO1 VREDAR24,DMCB,(RC),('QPRONE',0)                                  
                                                                                
PR10     DS    0H                  PROCESS MULTIPLE PRINT REQUESTS              
* IF USER IS PRINTING FROM THE LIST SCREEN AND                                  
* IF THE AGENCY ORDER IS BEING PROCESSED FOR THE FIRST TIME                     
* RECORD CURRENT ACTIVITY DATE/TIME                                             
         GOTO1 VTOUCHED                                                         
                                                                                
         BAS   RE,TEST4PRT         CHECK IF ANYTHING LEFT TO PRINT              
         BNZ   PR20                IF NOT, CLOSE THE PQ                         
*                                                                               
         NI    MISCFLG4,X'FF'-MF4PRDIF                                          
         L     R6,AIO              GETREC IS DONE IN TEST4PRT                   
         USING RDARREC,R6                                                       
*                                                                               
         OC    RDARREP#,RDARREP#   UNLINK OR REVISION UNLINK                    
         BZ    PR12                USE ROM30 TO PRINT REPORT                    
*                                                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RDARREP#                                              
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
*                                                                               
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCONKEY,KEY                                                      
R        USING RCONREC,IMIO                                                     
         MVI   K.RCONPTYP,X'8C'                                                 
         MVC   K.RCONPREP,RDARKREP                                              
         MVC   K.RCONPCON,WORK                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         DROP  K                                                                
*                                                                               
         TM    R.RCONMODR,X'10'    IS THIS A PENDING CONTRACT?                  
         BZ    PR12                YES, BUY LIST ONLY                           
         OI    MISCFLG4,MF4PRDIF                                                
*                                                                               
PR12     DS    0H                                                               
         TM    MISCFLG4,MF4PRDIF                                                
         BO    PR15                                                             
PR13     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'30',0),('QPRNEWPG',0)                              
         B     PR10                                                             
PR15     DS    0H                                                               
         GOTO1 VREDAR24,DMCB,(RC),('QPRNEWPG',0)                                
         B     PR10                                                             
                                                                                
PR20     DS    0H                                                               
         TM    MISCFLG4,MF4PRDIF                                                
         BO    PR30                                                             
PR25     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'30',0),('QPRCLOSE',0)                              
         B     PRYES                                                            
*                                                                               
PR30     DS    0H                                                               
         GOTO1 VREDAR24,DMCB,(RC),('QPRCLOSE',0)                                
*                                                                               
PRYES    SR    R5,R5                                                            
PRNO     LTR   R5,R5                                                            
         MVC   AIO,IMSVIO                                                       
         MVC   KEY,IMSVKEY                                                      
         MVC   SELECTKY,IMSVKEY2                                                
PRXIT    XMOD1                                                                  
         DROP  R6                                                               
         DROP  R4                                                               
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
         GOTOR GETRCTR                                                          
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
*                                                                               
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
         GOTOR HIGHCTR             READ AGENCY ORDER RECORD                     
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
         GOTOR GETRCTR             RETRIEVE THE AGENCY ORDER                    
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
                                                                                
         XC    DE9AGYC,DE9AGYC                                                  
         MVC   DE9AGYC(L'CCONKAGY),CCONKAGY                                     
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    DH10                                                             
         LA    RE,DE9AGYC                                                       
         MVI   DE9AGYC+4,C' '                                                   
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
DH10     DS    0H                                                               
         MVC   DE9AGYN,EAGYNAM1                                                 
                                                                                
* BUYER                                                                         
         MVC   DE9BUY,ECONBUYR                                                  
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DH15                                                             
* FOR DDS TERMINALS, DISPLAY D/A OF K, NOT BUYER NAME                           
         XC    DE9BUY,DE9BUY                                                    
         MVC   DE9BUY(4),=C'D/A='                                               
         GOTO1 HEXOUT,DMCB,CCONDKAD,DE9BUY+5,L'CCONDKAD,0                       
                                                                                
* ESTIMATE                                                                      
DH15     DS    0H                                                               
         MVC   DE9EST,CCONIEST                                                  
         CLC   DE9EST,SPACES                                                    
         BNE   *+10                                                             
         MVC   DE9EST,CCONXEST                                                  
                                                                                
* ADVERTISER + NAME                                                             
         MVC   DE9ADV,CCONKADV                                                  
         MVC   DE9ADVN,EADVNAME                                                 
                                                                                
* PRODUCT + PRODUCT NAME                                                        
         MVC   DE9PRD(3),CCONPRD                                                
         OC    CCONPRD,CCONPRD                                                  
         BZ    *+14                                                             
         MVC   DE9PRD+4(16),EPRDNAME                                            
         B     *+10                                                             
         MVC   DE9PRD,EPRDNAME                                                  
                                                                                
* STATION + MARKET                                                              
         MVC   DE9STA(4),CCONKSTA                                               
         MVC   DE9STA+4(3),=C'-TV'                                              
         CLI   CCONKSTA+4,C' '                                                  
         BE    DH17                                                             
         CLI   CCONKSTA+4,C'L'                                                  
         BNE   DH16                                                             
         MVC   DE9STA+4(3),=C'-L '                                              
         B     DH17                                                             
*                                                                               
DH16     DS    0H                                                               
         MVC   DE9STA+4(3),=C'- M'                                              
         MVC   DE9STA+5(1),CCONKSTA+4                                           
                                                                                
DH17     DS    0H                                                               
         CLI   CCONKSTA+3,C' '                                                  
         BNE   DH18                                                             
         MVC   DE9STA+3(3),DE9STA+4                                             
         MVI   DE9STA+6,C' '                                                    
                                                                                
DH18     DS    0H                                                               
         MVC   DE9STAN,EMKTNAME                                                 
                                                                                
* SALESPERSON                                                                   
         MVC   DE9SAL,CCONSAL                                                   
         MVC   DE9OFFC,CCONKOFF                                                 
         MVC   DE9SALN,ESALNAME                                                 
                                                                                
* START AND END DATES                                                           
         MVC   DE9DATE,ECONDATE                                                 
                                                                                
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
         MVC   DE9DPRT,MYWORK                                                   
                                                                                
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
         MVC   DE9LEN,MYWORK                                                    
                                                                                
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
                                                                                
         GOTO1 DEMOCON,DMCB,(2,MYWORK),(9,DE9DEMO),(0,DBLOCKD)                  
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
         MVC   GLVXFRPR,=C'ROM'    DARE PROGRAM                                 
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
* RETRIEVE DARE AGENCY EQUIVALENCY CODE, IF FOUND                               
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(RAGKDAGY-RAGKDKEY),KEYSAVE                                   
         BNE   GOCPGM10                                                         
         MVC   RCAUAGY,DAGYD.RAGKDAGY                                           
         MVC   RCAUAGOF,DAGYD.RAGKDAOF                                          
         DROP  DAGYD                                                            
*                                                                               
* IF MORE THAN ONE ASSIGNMENT, DEFAULT TO BLANK INSTEAD                         
*                                                                               
         GOTOR SEQCTR                                                           
         CLC   KEY(RAGKDAGY-RAGKDKEY),KEYSAVE                                   
         BNE   GOCPGM10                                                         
         XC    RCAUAGY,RCAUAGY                                                  
         XC    RCAUAGOF,RCAUAGOF                                                
*                                                                               
GOCPGM10 DS    0H                                                               
*MN      PASS ADDRESS OF REC TO RECNT10 IF PROFILE IS ON                        
         LR    R1,RA                                                            
         AH    R1,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTRMECB,CNTRMECA                                       
         BZ    GOCPGM15                                                         
         DROP  R1                                                               
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(27),0(R6)                                                    
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEYSAVE,KEY,0,0               
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   RCAUDRDA,KEY+28                                                  
                                                                                
GOCPGM15 EQU   *                                                                
*MN                                                                             
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
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
         XC    DE1HDLN,DE1HDLN     CLEAR HEADLINE FIELD                         
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
         GOTOR HIGHCTR                                                          
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
         GOTOR HIGHCTR                                                          
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
         MVC   GLVXFRPR,=C'ROM'    DARE PROGRAM                                 
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
         MVC   GLVXFRPR,=C'ROM'    DARE PROGRAM                                 
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
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLCONNUM,R6                                                      
         GOTO1 (RFCONNUM,REPFACS),DMCB,(1,CCONKNUM),(5,GLCONNUM)                
         MVC   GLCONCA(3),=C'DIS'                                               
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
                                                                                
SPFK0020 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   SPFV0020                                                         
                                                                                
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    SPFK0120                                                         
                                                                                
         CLI   PFKEY,2             CURSOR CAN BE ANYWHERE FOR PF2               
         BE    SPFK0120                                                         
                                                                                
         CLI   PFKEY,3             CURSOR CAN BE ANYWHERE FOR PF3               
         BE    SPFK0120                                                         
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
SPFK0040 DS    0H                  FOR LONG LIST SCREEN                         
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
                                                                                
         CLI   MYSCRNUM,X'E9'      FOR SHORT LIST SCREEN                        
         BE    SPFK0100                                                         
         LA    R0,DE6SELH          CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFD2                                                         
         LA    R0,DE6LLINH                                                      
         CR    R2,R0                                                            
         BH    RECNTFD2                                                         
         B     SPFK0120                                                         
*                                                                               
SPFK0100 DS    0H                  FOR LONG LIST SCREEN                         
         LA    R0,DE9SELH          CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFD2                                                         
         LA    R0,DE9LLINH                                                      
         CR    R2,R0                                                            
         BH    RECNTFD2                                                         
*                                                                               
SPFK0120 LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
*                                                                               
* CHECK IF REVISION, REVISION REJECT GOES TO OVERLAY 21                         
* REVISION DIFFERENCE GOES TO OVERLAY 22                                        
*                                                                               
         L     R1,AFRSTREC                                                      
SPFK0140 DS    0H                                                               
         CLI   5(R1),1             THIS ACTUALLY STARTS AT 2ND HALF OF          
         BNE   SPFK0200            KEY SCREEN INSTEAD OF LIST SCREEN            
         CLI   8(R1),C'A'                                                       
         BE    SPFK0220                                                         
         CLI   8(R1),C'R'                                                       
         BE    SPFK0220                                                         
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   SPFK0160                                                         
         CLI   8(R1),C'+'          '+' ONLY FOR DDS                             
         BE    SPFK0180                                                         
*                                                                               
SPFK0160 DS    0H                                                               
         CLI   8(R1),C'D'          'D'IFFERENCE ONLY FOR REVISION               
         BNE   SPFK0200                                                         
SPFK0180 ZIC   RF,0(R1)                                                         
         LR    RE,R1                                                            
         AR    RE,RF                                                            
         CLC   =C'REV',10(RE)                                                   
         BE    SPFK0220                                                         
         MVI   8(R1),C'S'                                                       
*                                                                               
SPFK0200 ZIC   RF,0(R1)                                                         
         AR    R1,RF                                                            
         CLI   0(R1),0             STOP AT END OF SCREEN                        
         BE    SPFK0240                                                         
         TM    1(R1),X'20'         IGNORE PROTECTED FIELDS                      
         BO    SPFK0200                                                         
         B     SPFK0140                                                         
*                                                                               
SPFK0220 DS    0H                                                               
         ZIC   RF,0(R1)                                                         
         AR    R1,RF                                                            
*                                  REVISION CHECK                               
         NI    CTLRFLG1,X'FF'-CF1ISREV                                          
         CLC   =C'REV',10(R1)                                                   
         BNE   SPFK0240                                                         
         OI    CTLRFLG1,CF1ISREV   FLAG ORDER IS REVISION                       
*                                                                               
SPFK0240 DS    0H                                                               
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    SPFV0220                                                         
         LA    R2,BLPFTAB                                                       
         B     SPFV0220                                                         
                                                                                
***************                                                                 
* FOR ACTION SELECT                                                             
***************                                                                 
SPFV0020 CLI   ACTNUM,MYACTSEL     ACTION SELECT?                               
         BNE   SPFV0220                                                         
                                                                                
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERR2                                                          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED, ORDER CAN BE              
         GOTOR HIGHCTR             CONFIRMED, IN WHICH CASE, 41S ARE            
*                                  DELETED.                                     
         TM    KEY+27,X'80'        IF THIS IS DELETED                           
         BZ    SPFV0030                                                         
         MVI   KEY,X'51'           THEN READ THE 51 KEY                         
         NI    DMINBTS,X'FF'-X'08'                                              
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY     51 KEY IS THERE?                             
         BE    SPFV0025            YES                                          
*                                                                               
         CLI   PFKEY,12                                                         
         BNE   INVPFKEY            ONLY PF12 IS ALLOWED                         
         LA    R2,VPFTABLE                                                      
         B     SPFV0220                                                         
*                                                                               
SPFV0025 DS    0H                                                               
         MVI   SELECTKY,X'51'      YES, CHANGE SELECTKY TO 51S FOR              
*                                  CORRECT PROCESSING OF OTHER MODULES          
SPFV0030 MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS BACK DELETED, JUST IN CASE              
         GOTOR GETRCTR                                                          
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   PFKEY,2             ONLY IF LINKED TO CONTRACT                   
         BNE   SPFV0040                                                         
         OC    RDARREP#,RDARREP#                                                
         BZ    ORDNTLK2                                                         
         OI    BITFLAG2,B2GOCON    SET SWAP TO CONTRACT                         
         B     STPFX                                                            
*                                                                               
SPFV0040 DS    0H                                                               
         CLI   PFKEY,10            Confirm?                                     
         BNE   SPFV0100                                                         
         CLI   RDARKTYP,X'51'                                                   
         BE    INVPFKEY                                                         
*                                                                               
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SPFV0100                                                         
         LR    R4,R6                                                            
         L     R6,AIO                                                           
*                                                                               
         USING RDARFLEM,R4                                                      
         TM    RDARFLG1,X'10'      STACF                                        
         BO    SPFV0080            YES, ALLOW CONFIRM                           
*                                                                               
         TM    RDARFLG1,X'40'      -S?                                          
         BO    SPFV0060                                                         
         CLI   RDARBSTS,C'M'                                                    
         BNE   SPFV0100            AMND ONLY                                    
         OI    CTLRFLG1,CF1PDING   THIS WILL FORCE THE DIFF SCR                 
         B     SPFV0180                                                         
*                                                                               
SPFV0060 DS    0H                  CHECK TO SEE IF -S                           
         TM    RDARFLG1,X'C0'      MATCH-S?                                     
         BO    SPFV0080                                                         
         CLI   RDARBSTS,C'A'       If Open-S                                    
         BE    SPFV0080                                                         
         CLI   RDARBSTS,C'M'       If Amend-S                                   
         BE    SPFV0080                                                         
         B     SPFV0100                                                         
*                                                                               
SPFV0080 DS    0H                                                               
         OI    BITFLAG3,B3CFCON    CONFIRM CONTRACT                             
         B     STPFX                                                            
         DROP  R4                                                               
*                                                                               
SPFV0100 DS    0H                                                               
         CLI   PFKEY,4             CONTRACT LIST                                
         BNE   SPFV0120                                                         
         OC    RDARRNUM,RDARRNUM   IF REVISION AND IS MISSING CONTRACT          
         BZ    SPFV0120            ORDER MUST BE A TAKEOVER                     
         OC    RDARREP#,RDARREP#   WARN USER TO ENTER TAKEOVER CONTRACT         
         BZ    ERTKOVR2            MUST GENERATE TAKEOVER CONTRACT              
         B     INVPFKEY                                                         
*                                                                               
SPFV0120 DS    0H                                                               
         CLI   PFKEY,6             ONLY VARIOUS CAN DO BRAND/LIST               
         BNE   SPFV0140                                                         
         TM    RDARMISC,X'10'                                                   
         BZ    INVPFKEY            INVALID IF NOT VARIOUS                       
*                                                                               
SPFV0140 DS    0H                                                               
         CLI   PFKEY,12            ONLY VARIOUS CAN DO BRAND/LIST               
         BNE   *+8                                                              
         OI    CTLRFLG1,CF11STKY   USE FIRST KEY ON NEXT LIST PASS              
*                                                                               
         LA    R2,VPFTABLE         YES, USE SELECT PFKEY TABLE                  
*                                  REVISION CHECK                               
         NI    CTLRFLG1,X'FF'-CF1PDING                                          
         CLI   RDARMEDI,C'R'                                                    
         BNE   SPFV0180                                                         
         OI    MISCFLG4,MF4RADIO    RADIO EDI                                   
         GOTO1 =A(CHKPDING),RR=RELO                                             
         BE    SPFV0160                                                         
         OI    CTLRFLG1,CF1PDING   RADIO EDI PENDING CONTRACT                   
*                                  OVERRIDES SCREEN TO DIFFERENCE               
         NI    MISCFLG4,X'FF'-MF4PDING                                          
         B     SPFV0180                                                         
SPFV0160 DS    0H                                                               
         OI    MISCFLG4,MF4PDING                                                
*                                                                               
SPFV0180 DS    0H                                                               
         OC    RDARRNUM,RDARRNUM   CHANGE TABLE DYNAMICALLY FOR                 
         BZ    SPFV0200            REVISED ORDERS                               
         OI    CTLRFLG1,CF1ISREV   FLAG ORDER IS REVISION                       
         DROP  R6                                                               
*                                                                               
SPFV0200 DS    0H                                                               
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    SPFV0220                                                         
         LA    R2,BPFTABLE                                                      
*                                                                               
SPFV0220 DS    0H                                                               
         LA    RF,MYLISTDA                                                      
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
                                                                                
         CLI   PFKEY,3             PFKEYING TO SCROLL?                          
         BE    STPFX               YES - GET OUT OF THIS ROUTINE                
                                                                                
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERR2                                                          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTOR HIGHCTR                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
                                                                                
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
         GOTOR HIGHCTR                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
                                                                                
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
*        DC    AL1(LPF02X-*,02,0,0,0,PFTRETRN)                                  
*        DC    CL3' ',CL8' ',CL8' '                                             
*LPF02X   EQU   *                                                               
                                                                                
* SCROLL                                                                        
         DC    AL1(LPF03XX-*,03,0,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8' ',CL8' '                                             
LPF03XX  EQU   *                                                                
                                                                                
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
*        DC    AL1(LPF14X-*,14,0,0,0,0)                                         
*        DC    CL3' ',CL8'MGGROUP',CL8'LIST'                                    
*LPF14X   EQU   *                                                               
                                                                                
* ACTUAL SELECT/DISPLAY                                                         
         DC    AL1(LPF21X-*,21,PFTCPROG,0,(LPF21X-LPF21)/KEYLNQ,0)              
         DC    CL3' ',CL8'ORDER',CL8'SELECT '                                   
LPF21    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                   
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
LPF03    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                   
LPF03X   EQU   *                                                                
*                                                                               
* CONTRACT                                                                      
         DC    AL1(LPF04X-*,04,0,0,(LPF04X-LPF04)/KEYLNQ,0)                     
         DC    CL3' ',CL8'CONTRACT',CL8'LIST'                                   
LPF04    DC    AL1(KEYTYTWA,L'DRVASTA-1),AL2(DRVASTA-T83FFFD)                   
         DC    AL1(KEYTYTWA,L'DE1OFF-1),AL2(DE1OFF-T83FFFD)                     
         DC    AL1(KEYTYTWA,L'DRVAAGY-1),AL2(DRVAAGY-T83FFFD)                   
         DC    AL1(KEYTYTWA,L'DRVADTE-1),AL2(DRVADTE-T83FFFD)                   
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
*LPF06    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                  
         DC    AL1(LPF06X-*,06,0,0,0,0)                                         
         DC    CL3' ',CL8'BRAND',CL8'LIST'                                      
LPF06X   EQU   *                                                                
*                                                                               
* HISTORY LIST                                                                  
         DC    AL1(SPF07X-*,07,0,0,(SPF07X-SPF07)/KEYLNQ,0)                     
         DC    CL3' ',CL8'HISTORY',CL8'LIST'                                    
SPF07    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                   
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
BLPF21   DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                   
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
BPF03    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                   
BPF03X   EQU   *                                                                
*                                                                               
* CONTRACT                                                                      
         DC    AL1(BPF04X-*,04,0,0,(BPF04X-BPF04)/KEYLNQ,0)                     
         DC    CL3' ',CL8'CONTRACT',CL8'LIST'                                   
BPF04    DC    AL1(KEYTYTWA,L'DRVASTA-1),AL2(DRVASTA-T83FFFD)                   
         DC    AL1(KEYTYTWA,L'DE1OFF-1),AL2(DE1OFF-T83FFFD)                     
         DC    AL1(KEYTYTWA,L'DRVAAGY-1),AL2(DRVAAGY-T83FFFD)                   
         DC    AL1(KEYTYTWA,L'DRVADTE-1),AL2(DRVADTE-T83FFFD)                   
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
*BPF06    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                  
*BPF06X   EQU   *                                                               
*                                                                               
*                                                                               
* HISTORY LIST                                                                  
         DC    AL1(BPF07X-*,07,0,0,(BPF07X-BPF07)/KEYLNQ,0)                     
         DC    CL3' ',CL8'HISTORY',CL8'LIST'                                    
BPF07    DC    AL1(KEYTYTWA,L'DE1HDLN-1),AL2(DE1HDLN-T83FFFD)                   
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
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IMSVIO,AIO                                                       
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTOR GETRCTR                                                          
*                                                                               
         MVC   KEY,IMSVKEY         RESTORE KEY AND AIO                          
*        GOTOR HIGHCTR                                                          
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
****>>>> B     ISMASTX                                                          
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
         GOTOR HIGHCTR                                                          
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
*                                  41 RECORD FOUND?                             
         BE    GAGY0060            YES - PROCESS IT                             
         MVC   KEY(27),KEYSAVE     NO  - RESET KEY TO 41 NOT FOUND              
         MVI   KEY,X'51'           SET TO 51 KEY                                
         GOTOR HIGHCTR             READ FOR AN UNDELETED 51 KEY                 
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
* VALIDATE NEW S/P-P/P FOR CHANGE FUNCTION                                      
*        R2  --> REQUESTOR FIELD HEADER                                         
***********************************************************************         
CHKREASS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,6               SET TO S/P KEY                               
         CLI   DE1UNW,C'Y'         UNWIRED ORDER?                               
         BNE   CASS0020            NO                                           
         MVI   KEY,X'31'           YES - SET TO P/P KEY                         
CASS0020 EQU   *                                                                
         MVC   KEY+22(2),AGENCY    INSERT REP CODE                              
         MVC   KEY+24(3),10(R2)    INSERT S/P-P/P CODE                          
         MVI   RDUPDATE,C'N'                                                    
         GOTOR HIGHCTR                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BNE   CASS0830            NO  - SET TO RETURN ERROR                    
*                                                                               
         MVC   AIO,AIO3            SET READ AREA TO IOAREA3                     
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR             RETRIEVE NEW S/P-P/P RECORD                  
         L     R6,AIO                                                           
         XC    REASSLDT,REASSLDT   CLEAR FIELD                                  
         CLI   KEY,X'06'           SALESPERSON RECORD?                          
         BNE   CASS0080            NO  - POINT PERSON                           
         USING RSALREC,R6          USE S/P RECORD                               
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    CASS0040            NO                                           
         MVC   REASSLDT,RSALLEAV   YES - SAVE LEAVE DATE                        
CASS0040 EQU   *                                                                
         MVI   REASSFLG,0          CLEAR FLAG                                   
         TM    RSALFLG,X'20'       KEEP FROM EDI?                               
         BNO   CASS0060            NO                                           
         MVI   REASSFLG,1          YES - SET FLAG                               
CASS0060 EQU   *                                                                
         DROP  R6                                                               
         B     CASS0140                                                         
CASS0080 EQU   *                                                                
         USING RPTPREC,R6          USE P/P RECORD                               
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    CASS0100            NO                                           
         GOTO1 DATCON,DMCB,(2,RPTPLDAT),(3,REASSLDT)                            
*                                  CONVERT LEAVE DATE TO 3-CHAR BIN             
CASS0100 EQU   *                                                                
         MVI   REASSFLG,0          CLEAR FLAG                                   
         TM    RPTPFLG,X'20'       KEEP FROM EDI?                               
         BNO   CASS0120            NO                                           
         MVI   REASSFLG,1          YES - SET FLAG                               
CASS0120 EQU   *                                                                
         DROP  R6                                                               
         B     CASS0140                                                         
CASS0140 EQU   *                                                                
         OC    REASSLDT,REASSLDT                                                
*                                  ANY LEAVE DATE?                              
         BZ    CASS0160            NO                                           
         GOTO1 DATCON,DMCB,(5,0),(3,WORK)                                       
         CLC   REASSLDT,WORK       LEFT TODAY OR EARLIER?                       
         BNH   CASS0810            YES - SET ERROR RETURN                       
CASS0160 EQU   *                                                                
         CLI   REASSFLG,1          KEEP FROM EDI?                               
         BE    CASS0820            YES - ERROR EXIT                             
CASS0800 EQU   *                                                                
         MVI   DUB,0               SET ERROR RETURN CODE                        
         SR    R0,R0               SET CC = ZERO                                
         B     CASS0900                                                         
CASS0810 EQU   *                                                                
         MVI   DUB,1               LEAVE DATE RETURN CODE                       
         B     CASS0890                                                         
CASS0820 EQU   *                                                                
         MVI   DUB,2               KEEP FROM EDI RETURN CODE                    
         B     CASS0890                                                         
CASS0830 EQU   *                                                                
         MVI   DUB,3               RECORD NOT FOUND RETURN CODE                 
         B     CASS0890                                                         
CASS0890 EQU   *                                                                
         LTR   RB,RB                                                            
CASS0900 EQU   *                                                                
         MVC   AIO,AIO1            RESET READ AREA TO IOAREA1                   
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**>>>                                                                           
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
         MVI   ELCODE,X'0F'        MISC FLAG ELEMENT                            
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
         GOTOR HIGHCTR                                                          
*                                                                               
         MVC   REASSLDT,=X'FFFFFF' SET LEAVE DATE TO INFINITY                   
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BNE   RASS0800            NO  - SET TO RETURN ERROR                    
         MVC   AIO,AIO3            SET READ AREA TO IOAREA3                     
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR             RETRIEVE NEW S/P-P/P RECORD                  
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
*   COMMENT OUT TWO FOLLOWING STATEMENTS TO REMOVE OFFICE                       
*        COMPARISON TEST FOR REASSIGNMENTS                                      
*                                                                               
         OC    REASSOFF,REASSOFF   ANY OFFICE ENTERED?                          
         BZ    RASS0080            NO  - DON'T FILTER                           
         GOTO1 DATCON,DMCB,(5,0),(3,WORK)                                       
         CLC   REASSLDT,WORK       LEFT TODAY OR EARLIER?                       
         BNH   RASS0870            YES                                          
         CLI   REASSFLG,1          KEEP FROM EDI?                               
         BE    RASS0860            YES - ERROR EXIT                             
*                                                                               
*   OFFICE MATCH ON REASSIGN REQUIREMENT IS REMOVED.                            
*                                                                               
***>>>   CLC   RDARKAOF,REASSOFF   YES - OFFICE MATCH?                          
***>>>   BNE   RASS0850            NO  - ERROR EXIT                             
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BE    *+6                 YES - MUST BE THERE                          
         DC    H'0'                NO  - DUMP IT OUT                            
         MVC   HDRDA,KEY+28        SAVE DISK ADDRESS OF RECORD                  
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTOR GETRCTR GET THE DARE RECORD                                      
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
         MVC   SVSPCODE,RDARPPSP-RDARPPEL(RF)                                   
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
*                                                                               
         MVI   AUDITYP,DHREASSQ                                                 
         XC    DMCB,DMCB                                                        
         MVI   DMCB+1,X'80'                                                     
         GOTOR DOAUDIT             DO AUDIT TRAIL                               
*                                                                               
                                                                                
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
**>>>                                                                           
***********************************************************************         
* PERFORM S/P-P/P REASSIGNMENT FOR A SINGLE LIST LINE, 'BATCH MODE'             
***********************************************************************         
REASSBAT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,TESTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TESTCTR                                                       
*                                                                               
         MVC   SVGENST1,GENSTAT1                                                
         OI    GENSTAT1,X'04'      SET RDUPAPPL                                 
         XC    KEY,KEY                                                          
         MVC   KEY(27),SELECTKY    LOAD SELECTED KEY                            
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTOR HIGHCTR                                                          
         MVC   HDRDA,KEY+28        SAVE DISK ADDRESS OF RECORD                  
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                KEY MUST BE ON FILE                          
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTOR GETRCTR                                                          
         USING RDARREC,R6                                                       
         MVC   REVNUM,RDARRNUM                                                  
         DROP  R6                                                               
*                                                                               
RBAT0080 EQU   *                                                                
         L     RF,AIO3             SET A(IOAREA3)                               
         XC    0(128,RF),0(RF)                                                  
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3:  KEY BUILD AREA                                                  
*        AIO1:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),AIO3,AIO1,     X        
               AIO2                                                             
*                                                                               
*   REGENDTR DOES I/O.  NEED TO RESET THE CURRENCY OF DARE RECORD TO            
*        PERMIT UPDATING.                                                       
*                                                                               
*                                                                               
*&&DO                                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(27),SELECTKY    LOAD SELECTED KEY                            
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTOR HIGHCTR                                                          
         MVC   HDRDA,KEY+28        SAVE DISK ADDRESS OF RECORD                  
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                KEY MUST BE ON FILE                          
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTOR GETRCTR                                                          
*&&                                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
*                                                                               
         LA    R1,RDARELEM                                                      
*                                                                               
         DROP  R6                                                               
*                                                                               
RBAT0100 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    RBAT0700            YES                                          
         CLI   0(R1),X'0A'         S/P-P/P ELEMENT?                             
         BE    RBAT0120            YES                                          
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     RBAT0100            GO BACK FOR NEXT                             
RBAT0120 EQU   *                                                                
         MVC   SVSPCODE,RDARPPSP-RDARPPEL(R1)                                   
         MVC   RDARPPSP-RDARPPEL(3,R1),DE1RQTR+2                                
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
*                                                                               
         MVI   AUDITYP,DHREASSQ                                                 
         XC    DMCB,DMCB                                                        
         MVI   DMCB+1,X'80'                                                     
         GOTOR DOAUDIT             DO AUDIT TRAIL                               
*                                                                               
                                                                                
RBAT0700 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     RBAT0900            EXIT CC ZERO                                 
RBAT0900 EQU   *                                                                
         MVC   GENSTAT1,SVGENST1   RESTORE GENSTAT1                             
         XIT1                                                                   
         EJECT                                                                  
TESTCTR  DC    F'0'                                                             
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
         TM    RDARMISC,X'40'      OPENED AT LEAST ONCE?                        
         BZ    DISA160                                                          
         OI    DRVQLNKH+1,X'04'    HIDE LINK/UNLINK INPUT QUESTION              
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
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
         GOTOR HIGHCTR                                                          
*                                  RE-READ FOR PUTREC                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTOR GETRCTR                                                          
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
         GOTOR GETRCTR                                                          
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
*&&DO                                                                           
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
         GOTOR HIGHCTR                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BNE   DPPS0060            NO                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
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
         XC    DMINBTS,DMINBTS                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),AGYVKEY                                     
*                                                                               
         MVI   KEY+RDARKRT-RDARKEY,X'40' BUY RECORD                             
         MVI   RDUPDATE,C'N'                                                    
         GOTOR HIGHCTR                                                          
*                                                                               
DIST10   DS    0H                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   DIST70                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
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
         GOTOR SEQCTR                                                           
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
*** SPECIAL CODING FOR MOVE1HO TO SKIP MEDIAOCEAN ENTRY IN DDDARETAB            
         LA    RF,RDARRCVR+L'RDARRCVR-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RE,RDARRCVR                                                      
         SR    RF,RE                                                            
         AHI   RF,1                                                             
*                                                                               
COFF10   DS    0H                                                               
         CLC   RDARRCVR(3),=C'MOV'                                              
         BNE   COFF10A                                                          
         CHI   RF,5                                                             
         BNH   COFF10A             SKIP MEDIA OCEAN ENTRY                       
         CLC   3(10,R3),=C'MEDIAOCEAN'                                          
         BE    COFF20                                                           
**** SPECIAL CODING ENDS                                                        
COFF10A  DS    0H                                                               
         ZIC   R1,14(R3)                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RDARRCVR(0),15(R3)                                               
         BE    COFF30                                                           
COFF20   DS    0H                                                               
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
COFFNO   DS    0H                                                               
*&&DO                                                                           
         L     RF,AIO                                                           
         USING RDARREC,RF                                                       
         CLC   RDARKORD,=X'31910003'                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*&&                                                                             
         LTR   RC,RC                                                            
COFFX    DS    0H                                                               
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
         GOTOR HIGHCTR                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
                                                                                
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GETRCTR                                                          
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
*&&DO                                                                           
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
*&&                                                                             
*                                                                               
* LEAVE 1D IN CONTRACT ELEMENT, DELETE AGY LINK                                 
* DATE/TIME STAMP FOR TROUBLESHOOTING                                           
*                                                                               
         USING RCONDREL,R6                                                      
         MVI   RCONDRFG,0                                                       
         GOTO1 DATCON,DMCB,(5,0),(2,RCONDRDR) KILLEM DATE                       
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,6,RCONDRTR                                                    
         DROP  R6                                                               
*                                                                               
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
         GOTOR HIGHCTR                                                          
         B     PMKG20                                                           
*                                                                               
PMKG10   DS    0H                                                               
         GOTOR SEQCTR                                                           
*                                                                               
PMKG20   DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   PMKGX                                                            
         OC    MGKEYD.RMKGKPLN(6),MGKEYD.RMKGKPLN                               
         BNZ   PMKG10                                                           
*                                                                               
         GOTOR GETRCTR                                                          
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
         GOTOR HIGHCTR                                                          
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTOR GETRCTR                                                          
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),RDARKEY                                     
         DROP  R6                                                               
                                                                                
         GOTOR HIGHCTR                                                          
                                                                                
DELO10   DS    0H                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   DELO20                                                           
                                                                                
         GOTOR GETRCTR                                                          
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARKRT,X'10'       AGENCY HEADER?                               
         BNE   DELO15                                                           
         GOTOR DELPAS              DELETE PASSIVES KEYS TO THIS ORDER           
*                                                                               
DELO15   DS    0H                                                               
         OI    RDARCNTL,X'80'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
                                                                                
         GOTOR SEQCTR                                                           
                                                                                
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
         GOTOR HIGHCTR                                                          
                                                                                
DELO30   DS    0H                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   DELO40                                                           
                                                                                
         GOTOR GETRCTR                                                          
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY HEADER?                               
         BNE   DELO35                                                           
         GOTOR DELPAS              DELETE PASSIVES KEYS TO THIS ORDER           
*                                                                               
DELO35   DS    0H                                                               
         OI    RDARCNTL,X'80'                                                   
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
                                                                                
         GOTOR SEQCTR                                                           
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
         GOTOR HIGHCTR                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DELO210  DS    0H                                                               
         MVC   WORK(27),KEY        SAVE FOR SEQUENCE                            
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTOR GETRCTR                                                          
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVI   RDARKTYP,X'51'                                                   
         NI    RDARCNTL,X'FF'-X'80' MARK UNDELETE                               
*                                                                               
         CLI   RDARKRT,X'10'                                                    
         BNE   DELO250                                                          
         L     R4,AIO3             A(PASSIVE BUILD AREA)                        
         XCEF  (R4),1600           R4->ALL NULL                                 
         AHI   R4,800              BUILD ALL NEW PTS                            
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R4),AIO                
*                                                                               
DELO250  DS    0H                                                               
         MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELO260                                                          
*                                                                               
         MVC   AIO,AIO2            51 ALREADY THERE?                            
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              DO PUTREC                                    
         NI    KEY+27,X'FF'-X'80'  UNDELETE THE KEY                             
         GOTO1 WRITE                                                            
*                                                                               
         CLI   RDARKRT,X'10'                                                    
         BNE   DELO300                                                          
         MVC   HDRDA,KEY+28                                                     
         B     DELO280                                                          
*                                                                               
DELO260  DS    0H                                                               
         GOTO1 ADDREC                                                           
*                                                                               
DELO270  DS    0H                                                               
         CLI   RDARKRT,X'10'                                                    
         BNE   DELO300                                                          
         MVC   HDRDA,KEY                                                        
*                                                                               
DELO280  DS    0H                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),AIO3,(R4),HDRDA         
*                                                                               
DELO300  DS    0H                                                               
         DROP  R6                                                               
         MVC   KEY(27),WORK                                                     
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTOR HIGHCTR                                                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTOR SEQCTR                                                           
         CLC   KEY(RDARKRT-RDARKEY),WORK                                        
         BE    DELO210                                                          
*                                                                               
DELOX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DELPAS   NTR1  BASE=*,LABEL=*                                                   
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO3: KEY BUILD AREA                                                   
*        AIO:  CURRENT LOCATION OF AGENCY ORDER RECORD                          
*                                                                               
         L     R4,AIO3                                                          
         XCEF  (R4),1600                                                        
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'81',ACOMFACS),AIO3,AIO                
*                                                                               
         L     R4,AIO3             A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->ALL NULL                                 
         LHI   RF,800              THIS WILL DELETE ALL THE OLD PTS             
         XCEF  (R4)                                                             
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),AIO3,(R4),     X        
               HDRDA                                                            
*                                                                               
DELPASX  XIT1                                                                   
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
*                                                                               
* CHECK FLAG TO SEE IF COMPARISONS ARE ALREADY DONE, IF SO, SKIP.               
*                                                                               
         XC    MISCFLG2,MISCFLG2                                                
         NI    BITFLAG,X'FF'-BFREVNEW                                           
         GOTOR CHKHDR               CHK TO SEE IF WE NEED TO COMPARE            
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
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(RDARKSEQ-RDARKEY),KEY                                    
         BNE   COMPORDX                                                         
*                                                                               
COMP0050 DS    0H                                                               
         CLI   KEY+RDARKSRT-RDARREC,X'00'                                       
         BE    COMP0100             BUY HEADER?                                 
         GOTOR SEQCTR               LOOP                                        
         B     COMP0050             UNTIL A BUY HEADER IS FOUND                 
*                                                                               
COMP0100 DS    0H                                                               
         MVC   SVBUYKEY,KEY         SAVE BUY HEADER KEY                         
         GOTOR PREPCMT              PREPARE AIO3 FOR WRITING COMT ELT           
*                                                                               
         MVC   KEY,SVBUYKEY                                                     
         GOTOR HIGHCTR              RE-READ THE HEADER REC                      
         MVC   AIO,AIO2                                                         
         GOTOR GETRCTR AIO->AIO2->BUY HEADER                                    
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
         GOTOR MODKEY               MODIFY KEY                                  
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    COMP0120                                                         
*                                                                               
         GOTOR ADDCHGEL,DMCB,(=AL1(RDARNBYQ),=AL1(1)),1                         
         B     COMP0630             NO MATCH SHADOW, THIS IS A NEW BUY          
*                                                                               
COMP0120 DS    0H                   AIO2->PREVIOUS BUY HEADER REC               
         GOTOR GETRCTR                                                          
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
         GOTOR STARTEN1,DMCB,PREV.RDARBYRO,MYWORK2                              
*                                                                               
         TM    MISCFLG2,MF2SFTDL    SOFT DELETE?                                
         BZ    COMP0140                                                         
*                                   YES, SKIP ALL COMPARISONS                   
         GOTOR SVALLDT,DMCB,RDARDATQ                                            
         B     COMP0630             SAVE ALL THE DATE/SPOT FROM SHADOW          
*                                                                               
COMP0140 DS    0H                   DAY/TIME                                    
         MVC   MYWORK2(DAYTIMQ),CURR.RDARBYRO                                   
         MVC   MYWORK2+DAYTIMQ(DAYTIMQ),PREV.RDARBYRO                           
         MVC   PREROT,PREV.RDARBYRO                                             
         GOTOR COMPDAY                                                          
*                                                                               
COMP0150 DS    0H                   LENGTH                                      
         CLC   CURR.RDARBYSL(L'RDARBYSU+L'RDARBYSL),PREV.RDARBYSL               
         BE    COMP0250                                                         
         GOTOR ADDCHGEL,DMCB,(=AL1(RDARLENQ),PREV.RDARBYSL),           >        
               L'RDARBYSL+L'RDARBYSU                                            
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
         GOTOR ADDCHGEL,DMCB,(=AL1(RDARPRGQ),PREV.RDARBYPN)                     
                                                                                
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
         GOTOR ADDCHGEL,DMCB,(=AL1(RDARDEMQ),PREV.RDARBDM1),           >        
               L'RDARBDM1                                                       
*                                                                               
         PRINT NOGEN                                                            
COMP0600 DS    0H                   DATES                                       
*                                                                               
         GOTOR COMDATE               RATE/SPOT, SPOT/WK GOT SAVED OFF           
*                                     IN SUBROUTINE COMDATE                     
         BNE   COMP0630              IF DUPLICAT START DATE,                    
*                                       DAILY/WEEKLY INCONSISTENT               
*                                    THEN SKIP RATE, SPOT/WK COMPARE            
*                                    RATE, SPOT/WK COMPARISON                   
         GOTOR COMRATE               COMPARE RATE/SPOT                          
*                                                                               
COMP0620 DS    0H                   SPOT/WK                                     
         CLC   CURSPTW,PRVSPTW                                                  
         BE    COMP0630                                                         
         GOTOR ADDCHGEL,DMCB,(=AL1(RDARSPTQ),PRVSPTW),L'PRVSPTW                 
                                                                                
*                                                                               
COMP0630 DS    0H                                                               
         NI    BITFLAG3,X'FF'-B3DAILYB                                          
*                                                                               
         L     R1,AIO3                                                          
         MVC   KEY,0(R1)                                                        
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTOR HIGHCTR                                                          
*        CLC   KEYSAVE(27),KEY                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
COMP0640 DS    0H                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTOR GETRCTR                                                          
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
         BE    COMP0660             AND X'01' ARE THE ONLY ELEMENT              
         MVC   AIO,AIO3                                                         
         GOTOR BLD01                                                            
         B     COMP0700                                                         
*                                                                               
COMP0660 DS    0H                                                               
         OI    KEY+27,X'80'                                                     
         L     R6,AIO2                                                          
         OI    29(R6),X'80'                                                     
         MVC   AIO,AIO2             MARK FOR DEL AND WRITE ORIGINAL             
         B     COMP0700             RECORD BACK                                 
*                                                                               
COMP0680 DS    0H                                                               
         TM    MISCFLG2,MF2CMTP     IF COMMENT REC PRESENT                      
         BO    COMP0685             THEN DO PUTREC                              
         GOTOR BLD01                                                            
         MVC   AIO,AIO3                                                         
         GOTO1 ADDREC               OTHERWISE, ADDREC                           
         B     COMP0750             READ NEXT BUY HEADER                        
COMP0685 DS    0H                                                               
         NI    KEY+27,X'FF'-X'80'                                               
         GOTOR BLD01                                                            
         L     R6,AIO3                                                          
         NI    29(R6),X'FF'-X'80'                                               
         MVC   AIO,AIO3                                                         
*                                                                               
COMP0700 DS    0H                                                               
         GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
*                                                                               
COMP0750 DS    0H                   PROCESS NEXT BUYLINE                        
         MVC   FLGSTDAT,SVSTDAT     MOVE ORIGINAL START DATE BACK               
*                                   AS IT MIGHT BE CHANGED ALONG THE            
*                                   PROCESS LEGALLY                             
         XC    MISCFLG2,MISCFLG2                                                
K        USING RDARREC,KEY          GET NEXT BUY LINE HEADER                    
         XC    KEY,KEY                                                          
         MVC   KEY,SVBUYKEY                                                     
         MVI   KEY+26,X'FF'                                                     
         DROP  K                                                                
         NI    DMINBTS,X'FF'-X'08'  DO NOT READ FOR DELETE                      
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(25),KEY                                                  
         BE    COMP0050                                                         
*                                                                               
COMP0800 DS    0H                   RESTORE SEQUENCE                            
*                                                                               
         GOTOR UPDTFLG              UPDATE FLAG IN AGY HEADER REC               
*                                                                               
         MVC   KEY,AGYVKEY                                                      
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1             RESTORE RECORD                              
         GOTOR GETRCTR                                                          
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
         GOTOR BLDGRID,DMCB,(X'00',CURGRID)                                     
         BNE   COMD0500             DUPLICATE START DATE                        
         GOTOR BLDGRID,DMCB,(X'01',PRVGRID)                                     
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
         BZ    COMD0188                                                         
         CLC   STARTDAY,ENDDAY     OUT OF WEEK ROTATOR                          
         BH    COMD0188                                                         
         BCTR  R4,0                                                             
*                                                                               
COMD0188 DS    0H                                                               
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
         GOTOR SVALLDT,DMCB,RDARDT2Q                                            
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
         GOTOR MODKEY                                                           
BLDG0010 DS    0H                                                               
         MVI   K.RDARKSRT,X'30'                                                 
         DROP  K                                                                
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   BLDGRIDX                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTOR GETRCTR                                                          
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
         GOTOR GETSTDT              GET DAILY BUY START DATE                    
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
*                                  COMPARE COMPRESS DATE                        
         GOTOR CPCPDAT,DMCB,FLGSTDAT,RDARBUSD                                   
         BL    *+10                IF BUY DATE BEFORE EST START DATE            
*                                  THEN CHANGE START DATE TO BUY START          
*                                  DATE TEMPORARILY TO ASSURE PROPER            
         MVC   FLGSTDAT,RDARBUSD   COMPARING PROCESS                            
*                                                                               
BLDG0080 DS    0H                                                               
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
         GOTOR BLDOGRID,DMCB,(X'00',ORBGRID)                                    
         BE    COMDY050             THERE IS ORBIT                              
         MVC   ORBGRID(DAYTIMQ),MYWORK2  NO, USE THE DAY/TIME IN HEADER         
         MVI   ORBGRID+DAYTIMQ,X'FF'                                            
         MVC   OBTNUM0,=H'1'                                                    
*                                                                               
COMDY050 DS    0H                                                               
         GOTOR BLDOGRID,DMCB,(X'01',ORBGRID2)                                   
         BE    COMDY150                                                         
         MVC   ORBGRID2(DAYTIMQ),MYWORK2+12                                     
         MVI   ORBGRID2+DAYTIMQ,X'FF'                                           
         MVC   OBTNUM1,=H'1'                                                    
*                                                                               
COMDY150 DS    0H                                                               
         CLC   OBTNUM1,OBTNUM0                                                  
         BNE   COMDY250                                                         
*                                                                               
         LA    R4,ORBGRID                                                       
         LA    R6,ORBGRID2                                                      
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
         MVC   ELEM+3(0),ORBGRID2                                               
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
BLDOGRID NTR1  BASE=*,WORK=(R3,IMWORKQ),LABEL=*                                 
         L     R4,0(R1)             R4->A(GRID)                                 
         ST    R4,FULL                                                          
K        USING RDARKEY,KEY          READ BUY DETAIL REC                         
         USING IMWORKD,R3                                                       
         MVC   IMSVIO,AIO                                                       
         MVC   IMSVKEY,KEY                                                      
*                                                                               
         L     R8,AIO2                                                          
         MVC   KEY,0(R8)                                                        
         GOTOR MODKEY                                                           
         MVC   K.RDARKSTY(1),0(R1)  CURRENT/PRV RECORD                          
         MVI   K.RDARKSRT,X'10'     BUY ORBIT                                   
         DROP  K                                                                
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   BLDODNO              NO ORBIT RECORD                             
*                                                                               
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTOR GETRCTR                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        ORBIT ELEMENT                                
         BRAS  RE,GETEL                                                         
         BNE   BLDODNO                                                          
         USING RDAROEEL,R6                                                      
*                                                                               
         SR    R2,R2                                                            
         LHI   RF,L'ORBGRID                                                     
         XCEF  (R4)                                                             
         LA    R8,L'ORBGRID(R4)                                                 
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
BLDODYES SR    R4,R4                                                            
BLDODNO  LTR   R4,R4               SET CONDITION CODE                           
BLDORIDX DS    0H                                                               
         MVC   AIO,IMSVIO                                                       
         MVC   KEY,IMSVKEY                                                      
         B     EXIT3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R3                                                               
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
CHGD     USING RDARCHSD,R2                                                      
*                                                                               
K        USING RDARKEY,KEY          READ BUY DETAIL SHADOW RECORD               
         L     R3,AIO2                                                          
         MVC   KEY,0(R3)                                                        
         GOTOR MODKEY                                                           
         MVI   K.RDARKSTY,X'01'     READ SHADOW                                 
         MVI   K.RDARKSRT,X'30'                                                 
         DROP  K                                                                
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   SVALLDTX                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTOR GETRCTR                                                          
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
         GOTO1 DATCON,DMCB,(0,MYWORK2+6),(2,CHGD.RDARCHSD)                      
*                                                                               
         ZIC   R4,RDARBUWK                                                      
         BCTR  R4,0                                                             
         MHI   R4,7                                                             
         GOTO1 ADDAY,DMCB,MYWORK2,MYWORK2+6,(R4)                                
         ZIC   R4,ENDDAY                                                        
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,MYWORK2+6,MYWORK2,(R4)                                
         GOTO1 DATCON,DMCB,(0,MYWORK2),(2,CHGD.RDARCHED)                        
*                                                                               
         TM    MISCFLG2,MF2SVCST                                                
         BZ    SVALL150                                                         
         ZIC   R4,RDARBUSW                                                      
         STCM  R4,3,CHGD.RDARCHSP   JUST SAVE THE SPOT COUNT                    
         B     SVALL180                                                         
SVALL150 DS    0H                                                               
         ZIC   R4,RDARBUSW          SAVE THE SPOT COUNT RELATIVE TO 0           
         SR    R3,R3                BECAUSE THE BUYLINE IS CANCELLED            
         SR    R3,R4                                                            
         STH   R3,HALF                                                          
         MVC   CHGD.RDARCHSP,HALF                                               
*                                                                               
SVALL180 DS    0H                                                               
         TM    MISCFLG2,MF2SVCST                                                
         BZ    SVALL200                                                         
         MVC   CHGD.RDARCHCT,RDARBU$$                                           
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
         DROP  CHGD                                                             
         DROP  R6                                                               
         B     EXIT3                                                            
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
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   GETSTDTX                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTOR GETRCTR                                                          
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
         GOTOR MODKEY                                                           
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   GETSTDTX                                                         
*                                                                               
         GOTOR GETRCTR                                                          
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
         GOTOR CPCPDAT,DMCB,FLGSTDAT,RDARBUSD                                   
         BL    *+10                                                             
         MVC   FLGSTDAT,RDARBUSD    USE THE EARLIER DATE                        
         DROP  R6                                                               
*                                                                               
         MVC   KEY,WORK                                                         
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOR GETRCTR                                                          
GETSTDTX DS    0H                                                               
         B     EXIT3                                                            
*******************************************************                         
* COMPARE COMPRESS DATE FORMAT                                                  
*******************************************************                         
CPCPDAT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         XC    ELEM,ELEM                                                        
         GOTO1 DATCON,DMCB,(2,(R3)),(3,ELEM)                                    
         GOTO1 DATCON,DMCB,(2,(R4)),(3,ELEM+6)                                  
         CLC   ELEM(3),ELEM+6                                                   
CPCPDATX DS    0H                                                               
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
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   CHKHDNO             RECORD IS NOT THERE, EXIT                    
*        DC    H'0'                RECORD COULD BE DELETED, NO DUMP             
         MVC   AIO,AIO1                                                         
         GOTOR GETRCTR                                                          
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
         GOTOR MODKEY                                                           
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   CHKHDNO                                                          
*                                                                               
         GOTOR GETRCTR                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CHKHDNO                                                          
*                                                                               
         USING RDARCODE,R6                                                      
         GOTOR CPCPDAT,DMCB,FLGSTDAT,RDARESST                                   
         BL    *+10                                                             
         MVC   FLGSTDAT,RDARESST    USE THE EARLIER DATE                        
         MVC   SVSTDAT,FLGSTDAT     SAVE OFF EST START DATE                     
         DROP  R6                                                               
*                                                                               
CHKHDYES SR    RC,RC                                                            
CHKHDNO  LTR   RC,RC                SET CONDITION CODE                          
CHKHDX   DS    0H                                                               
         B     EXIT3                                                            
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
         GOTOR HIGHCTR                                                          
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
         GOTOR GETRCTR ADDED                                                    
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
         GOTOR HIGHCTR                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTOR GETRCTR                                                          
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
         L     R6,AIO3                                                          
         MVI   ELCODE,X'01'        IS THERE AN 01 ALREADY?                      
         BRAS  RE,GETEL                                                         
         BE    BLD01X                                                           
*                                                                               
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
       ++INCLUDE REDARPROF                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE REROMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REROME1D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REROME9D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REROME3D                                                       
         ORG   DE1TAGH                                                          
       ++INCLUDE REROME6D                                                       
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
       ++INCLUDE REGENSET                                                       
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDTSARD                                                        
*                                                                               
IMWORKD  DSECT                                                                  
IMSVIO   DS    A                                                                
IMSVKEY  DS    XL27                                                             
IMSVKEY2 DS    XL27                                                             
IMIO     DS    XL2000                                                           
IMBYTE   DS    XL1                                                              
IMWORKQ  EQU   *-IMWORKD                                                        
*                                                                               
ADWORKD  DSECT                                                                  
ADSVIO   DS    A                                                                
ADSVFG   DS    X                                                                
ADSVFG2  DS    X                                                                
ADSVKEY  DS    XL27                                                             
ADIO     DS    XL2000                                                           
ADWORKQ  EQU   *-ADWORKD                                                        
*                                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
BANNER   DS    CL8                                                              
*                                                                               
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
PAGEDETL DS    X                   PAGE DETAIL INDICATOR                        
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
ORBGRID  DS    XL500               EXPAND SIZE OF GRID FOR ORBIT                
         ORG   ORBGRID                                                          
CURGRID  DS    XL(TABWK#*ROWSIZE)  BUY GRID FOR CURRENT ORDER                   
         ORG   ORBGRID+L'ORBGRID                                                
ORBGRID2 DS    XL500                                                            
         ORG   ORBGRID2                                                         
PRVGRID  DS    XL(TABWK#*ROWSIZE)  BUY GRID FOR SAVED DARE ORDER                
         ORG   ORBGRID2+L'ORBGRID2                                              
REMAIN   EQU   L'CURGRID-256                                                    
OBTNUM0  DS    H                                                                
OBTNUM1  DS    H                                                                
LASTRAT  DS    F                                                                
LASTDRAT DS    F                                                                
PREROT   DS    XL8                 PREVIOUS ORDER'S ROTATION                    
RELO3    DS    A                                                                
MYWORK2  DS    CL80                                                             
SVSTDAT  DS    H                   SAVED FLIGHT START DATE                      
HDRDA    DS    CL4                 DISK ADDRESS OF REASSIGN RECORD              
HAGYORDR DS    XL4                 HEX AGENCY ORDER NUMBER                      
REASSNAM DS    CL20                                                             
REASSOFF DS    CL2                                                              
REASSFLG DS    CL1                                                              
REASSLDT DS    XL3                                                              
REDITYPE DS    XL1                 REDI ORDER SCREEN TYPE                       
*                                  0  =  ORDER    (DEFAULT SEQUENCE)            
*                                  1  =  CAMPAIGN                               
*                                  2  =  BUYER                                  
*                                  3  =  FLIGHT                                 
*                                  4  =  SENTDATE                               
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
MISCFLG4 DS    X                   MISC. FLAGS 4                                
MF4RADIO EQU   X'80'               RADIO STATION EDI                            
MF4NOREC EQU   X'40'               NO REC BEING DISPLAYED                       
MF4PDING EQU   X'20'               CONTRACT IS PENDING                          
MF4CONCF EQU   X'10'               CONTRACT IS CONFIRMED                        
MF4CNWIP EQU   X'08'               CONTRACT IS WIP                              
MF4TKO   EQU   X'04'               CONTRACT IS A TAKEOVER                       
MF4PRDIF EQU   X'02'               PRINT DIFFERENCE REPORT                      
*                                                                               
HDRDA2   DS    XL4                                                              
PENDCF   DS    X                                                                
*                                                                               
PRNTWORK DS    CL200               PRINT SETUP WORK AREA                        
REVNUM   DS    X                                                                
AUDITYP  DS    C                   AUDIT TRAIL TYPE                             
SVSPCODE DS    CL3                 SAVED SALES PERSON CODE                      
*                                                                               
TLACTN   DS    XL1                 REQUESTED ACTION                             
TLST     DS    XL(L'TLREC+2)       TSAR2 RECORD BUFFER                          
         ORG   TLST                                                             
TXNUM    DS    XL2                                                              
TXREC    DS    0X                                                               
TXLEN    DS    XL2                                                              
TXKEY    DS    0X                                                               
         ORG                                                                    
TBLOCK   DS    XL(TSARDL)          TSAR BUFFER                                  
*                                                                               
TSARREC  DS    XL(L'TLREC)                                                      
TSARKEY  DS    XL(L'TLKEY)                                                      
SVDARSTD DS    XL2                 SAVED DARE FLIGHT START DATE                 
SVDAREND DS    XL2                 SAVED DARE FLIGHT END DATE                   
SVGENST1 DS    XL1                                                              
REASSALL DS    XL1                                                              
SVTWAAGY DS    CL2                                                              
SVKEYTYP DS    XL1                                                              
*                                                                               
PARBLOCK DS    0H                                                               
       ++INCLUDE REGENSBLK                                                      
         DS    0F                                                               
RFBLOCK  DS    CL6                 REPFACS BLOCK                                
*                                  BYTES 1  -  4  =  A(COMFACS)                 
*                                  BYTES 5  -  6  =  REP CODE                   
TWANOGO  EQU   16384                                                            
BACK45K  EQU   4500                                                             
BACK5H   EQU   500                                                              
*                                                                               
ASTASET  DS    A                   A(STATION SET, IF PRESENT)                   
ASETSET  DS    A                   A(SET OF SETS, IF PRESENT)                   
ANEXTSET DS    A                                                                
SAVEREGX DS    F                                                                
QWSETFLG DS    CL1                 X'80' = EXCLUDE SET                          
SETBYTE  DS    CL1                                                              
WORKLQ   EQU   *-MYAREAD                                                        
*                                                                               
*   TSAR RECORD DSECT                                                           
*                                                                               
TLSTD    DSECT                     TSAR RECORD DSECT                            
TLREC    DS    0XL32               *** TSAR RECORD ***                          
TLLEN    DS    XL2                 RECORD LENGTH                                
TLKEY    DS    0XL2                                                             
TLPAGE   DS    XL2                 DARE SCREEN PAGE NUMBER                      
TLDARKEY DS    XL27                DARE RECORD KEY                              
         DS    CL1                 SPARE                                        
*                                                                               
*                                                                               
* ONLINE LIST LINE (TWO PHYSICAL LINES)                                         
*                                                                               
LISTD    DSECT                                                                  
*                                                                               
*   STATUS FIELD HEADER:  FIELD IS HIGH INTENSITY, REQUIRES                     
*        ITS OWN HEADER TO SET THAT ATTRIBUTE                                   
*                                                                               
LSTSTATH DS    CL8                                                              
LSTCOLOR DS    CL1                                                              
LSTPRNTD DS    CL1                                                              
LSTSTAT  DS    CL6                                                              
LSTSTAT2 DS    CL2                                                              
LIN1STRT EQU   *                                                                
*                                                                               
*   DATA   FIELD HEADER:  FIELD IS NORMAL INTENSITY, REQUIRES                   
*        ITS OWN HEADER TO SET THAT ATTRIBUTE                                   
*                                                                               
         DS    CL8                                                              
LSTCON#  DS    CL8                                                              
********************LSTTFLAG DS    CL1                                          
LSTAGY   DS    CL5                                                              
         DS    CL1                                                              
LSTADV   DS    CL7                 FIRST NINE CHARS OF ADVERT                   
         DS    CL1                                                              
LSTCLI   DS    CL3                                                              
         DS    CL1                                                              
LSTPRD   DS    CL3                                                              
         DS    CL1                                                              
LSTEST   DS    CL3                                                              
         DS    CL1                                                              
LSTFSTRT DS    CL8                                                              
         DS    CL1                                                              
LSTSTA   DS    CL5                 NO HYPHEN FOR BAND                           
         DS    CL1                                                              
LSTTOTL  DS    CL6                                                              
         DS    CL1                                                              
LSTPOWER DS    CL2                 POWER CODE OF ORDER                          
         DS    CL1                                                              
LSTVAR   DS    CL5                                                              
         DS    CL1                 SPARE FOR ALIGNMENT                          
LLINE1   EQU   *-LIN1STRT                                                       
LISTLEN  EQU   *-LSTSTATH                                                       
LLSTCON# EQU   *-LSTCON#                                                        
         DS    CL8                 'SEL' CONTROL FIELD                          
         DS    CL1                 'SEL' ENTRY FIELD                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'186REROM04   12/10/04'                                      
         END                                                                    
