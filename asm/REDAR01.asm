*          DATA SET REDAR01    AT LEVEL 202 AS OF 08/02/06                      
*PHASE T80F01C                                                                  
         TITLE 'T80F01 - REDAR01 - DARE HEADER DISPLAY/LIST'                    
***********************************************************************         
*                                                                     *         
*  REDAR01 (T80F01) --- DARE HEADER DISPLAY/LIST                      *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
* JUL1204 (HQ ) FIX CONFIRM ADDREC DUMPS                              *         
* 11MAR04 (HQ ) KILLEM DATE/TIME STAMP                                *         
* 11SEP03 (BU ) STATION SETS                                          *         
* 23JUL03 (HQ ) PREVENT NOTDARE ORDER FROM SHOWING UP IN INBOX        *         
* 11JUL03 (HQ ) COMPARE FIX: OUT OF WEEK ROTATOR, EXPAND ORBIT GRID   *         
*               SIZE, USE LOCAL IO IN BLDOGRID                        *         
* 13JUN03 (HQ ) BUG FIX: CLEAR CCONNUM FROM PREVIOUS TRANSACTION      *         
* 10JUN03 (BU ) INSTALL PAR SECURITY                                  *         
* 02MAR03 (HQ ) FLAG DEMO IF CHANGED ON REVISION                      *         
*         (HQ ) ADD DEMO CAT TO COMPARISON                            *         
* 14MAY03 (HQ ) PROTECT UNLINK FIELD IF OPEN AT LEAST ONCE FOR REJECT *         
*               BUG FIX, SVALLDT IN CORRECTLY REGISTER USING          *         
* 02JAN03 (HQ ) BUG FIX: CHANGE ST DATE IF COMES AFER BUY ST DATE     *         
* 21JAN03 (SKU) SUPPORT ALPHA NUMERIC EST NUMBER                      *         
* 02JAN03 (HQ ) BUG FIX: REVERT TO ORIGINAL START DATE AFTER DAILY BUY*         
* 23DEC02 (HQ ) BUG FIX: DO NOT READ FOR DELETED BUY HEADER           *         
* 18NOV02 (SKU) DISPLAY AGENCY PRODUCT CODE                           *         
* 06NOV02 (HQ ) DISALLOW REJECT ON CONFIRM ORDER                      *         
*               FIX BUY COMMENT RECORD SOFT/HARD DELETE BUG           *         
* 06SEP02 (HQ ) COMPARE DIFFERENCE B/T PREVIOUS AND CURRENT ORDER     *         
*               BUG FIX, CLEAR BUYKEY BEFORE CALLING DAR10 BUYLIST    *         
* 03SEP02 (HQ ) DISP CF CON IF CON# IS SPECIFIED & NOTHING IS FOUND   *         
*               RESET PFKEY LINE AFTER UNLINK                         *         
*               DARE SCREEN BUG FIX BY JDON                           *         
*               CHANGE READ TO CTFILE TO BE NON-UPDATIVE PER ALAN A.  *         
* 09AUG02 (SKU) KILLEM TO UPDATE CONTRACT AND MAKEGOOD RECORDS        *         
* 02JUL02 (SKU) DIRECT PROCESSING BYPASS OFFICE LIMIT ACCESS          *         
* 04JAN02 (SKU) CHANGE APPROVE TO OPEN                                *         
*               SUPPORT DIRECT ORDER PROCESSING                       *         
*               ALLOW CONFIRMED BRAND ORDERS TO LIST IN ORDER/LIST SCN*         
* 12DEC01 (SKU) REVISION DIFFERENCE SUPPORT                           *         
* 03DEC01 (SKU) SHOW TRADE FLAG                                       *         
* 12NOV01 (SKU) ADD HISTORY BUTTON                                    *         
* 31OCT01 (SKU) ADD ORDER HISTORY SUPPORT                             *         
* 03OCT01 (SKU) FIX APP-S BUG AGAIN                                   *         
* 20SEP01 (SKU) FIX APP-S BUG                                         *         
* 17JUL01 (SKU) FIX '*' COMBO FILTER                                  *         
* 21MAY01 (SKU) DEMO OVERRIDE BUG FIX                                 *         
* 25OCT00 (SKU) ALLOW FTS AND UTS LA OFFICE SEE JW-DN AND H7-DN ORDERS*         
* 11AUG00 (SKU) FLAG 'TOUCHED' ONLY FOR PRINTING                      *         
*               NEW SENT STATUS                                       *         
* 12JUL00 (SKU) SUPPORT HOME MARKET SIGNON INBOX FILTERING            *         
*               NEW REQUESTOR FIELD                                   *         
* 28JUN00 (SKU) REMOVE REFERENCE TO GLV1GOTO PER MEL                  *         
*               SKIP USER DEFINED DEMOS                               *         
* 27JUN00 (SKU) FIX FOR STEREO TO PASS CORRECT SCREEN NUMBER          *         
* 22MAR00 (SKU) DDS CONFIRM ACTION TO CHANGE 41'S TO 51'S             *         
* 24JAN00 (SKU) ALLOW DV OFFICE ACCESS TO DN FOR SZ, AM, CQ, BL AND PV*         
* 02JAN00 (SKU) ZERO PAD ORDER NUMBER IN LIST DISPLAY                 *         
* 12OCT99 (SKU) FIX CONFIRM STATUS LIST DISPLAY                       *         
* 19AUG99 (SKU) COLUMN PRINT BUG FIX                                  *         
* 29JUN99 (SKU) ADD NEW C= OFFICE FILTER                              *         
* 22JAN99 (RHV) CONFIRMED ORDER DISPLAY                               *         
*         (SKU) ALLOW PETRY SEATTLE ACCESS TO PETRY PORTLAND          *         
* 03JUN98 (SKU) SUPPORT AGENCY DEMOS                                  *         
* 29MAY98 (SKU) SUPPORT STEREO COLORING SCHEME                        *         
* 18MAY98 (SKU) ALLOW LINK OF RECALL/RESEND ORDERS                    *         
* 11MAR98 (SKU) TEMP CODE TO ALLOW BLRSA TO SEE BLRPO ORDERS          *         
* 23JAN98 (SKU) FIX LIST LOOPING BUG                                  *         
* 17JAN98 (SKU) FIX PFKEY GOING TO INCORRECT APPROVAL OVERLAY         *         
* 16JAN98 (SKU) FIX TOTAL CALCULATION SKIPPING CANCELLED BUYS         *         
* 18JUL97 (SKU) AGENCY CHANGES/REVISION SUPPORT                       *         
* 30MAY97 (JRD) ADD REMOVE ACTION FOR REJECTED ORDERS                 *         
* 13MAY97 (SKU) ADD X'BD' CONTRACT PASSIVE KEY                        *         
* 03MAR97 (SKU) VARIOUS/TRUE POOL ORDER SUPPORT                       *         
* 18DEC96 (DBU) NEW FILTER TO FILTER ON RECEIVED DATES                          
* 11DEC96 (SEP) NEW FILTER TO FILTER ON RECEIVED DATES                          
* 23OCT96 (SKU) NEW FILTER TO FILTER ON 'UNTOUCHED' ORDERS            *         
* 03SEP96 (SKU) ALLOW DELETE FOR EDI ORDERS ONLY.                     *         
* 24SEP96 (SKU) ADD OFFICE LIMIT ACCESS CHECK                         *         
* 12AUG96 (SKU) PFKEY CHECK BUG FIX FOR NOT-DARE ORDERS               *         
* 21JUN96 (SKU) PREVENT APPROVAL OF ORDERS IF CONVERTED CONTRACTS ARE *         
*               MISSING THE X'1D' DARE LINK ELEMENT                   *         
* 21MAR96 (SKU) KATZ EDI, AUTO-HEADER SEND FLIGHT#/TRAFFICE ID        *         
* 20FEB96 (SKU) SHOW POEM INSTEAD OF DARE FOR PETRY (PV)              *         
* 29JAN96 (SKU) KATZ EDI SUPPORT. AUTO-HEADER SUPPORT                 *         
* 21JUN95 (SKU) READ STATION RECORD FOR MARKET NAME FOR UNLINK ORDERS *         
* 18MAY95 (SKU) FILTER ON RECALLED ORDERS                             *         
* 01MAY95 (SKU) MAKEGOOD SUPPORT                                      *         
* 02MAR95 (SKU) SPACE PAD AGENCY OFFICE IF NO AGENCY OFFICE           *         
* 01FEB95 (SKU) FIX BUG OF COMPARING AGENCY OFFICES                   *         
* 03JAN95 (SKU) TAG ORDER IF VIEWED OR PRINTED                        *         
* 22DEC94 (SKU) ADD NOTDARE FILTER AND SUPPORT                        *         
* 19DEC94 (BU ) DEMO FIELD OVERFLOW PROBLEM                           *         
* 29NOV94 (SKU) EXPAND OFFICE FILTER TO INCLUDE UNLINKED ORDERS       *         
* 18NOV94 (SKU) ADD MULTI-AGENCY EQUIVALENCY SUPPORT                  *         
* 10OCT94 (SKU) ADD GROUP/SUBGROUP AND TEAM FILTERS                   *         
* 18MAY94 (SKU) INITIAL RELEASE                                       *         
*               ***  END TOMBSTONE  ***                               *         
***********************************************************************         
T80F01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLQ,*T80F01*,R7,RR=R3                                         
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
         XC    BUYKEY,BUYKEY       THIS FIX THE DAR10 ORDER CMT BUG             
*                                                                               
         OI    GLSTSTAT,APPLCDSP+RETEXTRA   SET I DISPLAY                       
         MVC   LLIST,=Y(LISTLEN)   SET L'DATE LINE                              
*                                  SETUP THE PFKEYS                             
         GOTO1 =A(SUBROUT),DMCB,(RC),('QSETPFKY',0),RR=RELO                     
*                                                                               
         GOTOR CHKREV                                                           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
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
         LR    R4,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R4,DARPROFS-CONHEADH                                             
         USING SVDSECT,R4                                                       
         TM    SVDGPBIT+DARLPARB,DARLPARA                                       
         BZ    VKEY0020            NOT SET: DON'T DO PAR SECURITY               
         DROP  R4                                                               
         GOTO1 =A(PARSECY),RR=RELO                                              
         BZ    EXIT                PAR SECURITY PASSED                          
         OI    GENSTAT2,USMYOK     TURN ON 'USE MY OK MSG'                      
         MVI   ERROR,0                                                          
         L     RD,BASERD           QUICK EXIT                                   
         XIT1                      EXIT WITH CANNED MESSAGE                     
*                                                                               
VKEY0020 EQU   *                                                                
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
* TEST IF PRINT FROM LIST SELECT COLUMN                                         
         GOTO1 =A(PR),RR=RELO                                                   
                                                                                
LR10     DS    0H                                                               
         CLI   DRFHDLNH+5,0                                                     
         BE    LR40                                                             
*                                                                               
         CLI   MYSCRNUM,X'FD'      SHORT LIST SCREEN ALREADY LOADED?            
         BNE   LR20                                                             
         TWAXC DRHAGYCH,DRHESTH    YES, CLEAR IT                                
         TWAXC DRHSELH,DRHLLINH,PROT=Y                                          
         B     LR30                                                             
                                                                                
* LOAD LOWER SCREEN                                                             
LR20     DS    0H                                                               
         GOTO1 CALLOV,DMCB,DRFTAGH,X'D9080FFD'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCRNUM,X'FD'                                                   
*                                                                               
LR30     DS    0H                                                               
         LA    R2,DRHSELH          SET FOR GENCON CURSOR POSITION               
         ST    R2,AFRSTREC                                                      
         LA    R2,DRHSTATH                                                      
         ST    R2,ATHISLST                                                      
         OI    DRHSELH+6,X'40'     FORCE CURSOR HERE                            
                                                                                
*                                  DISPLAY CONTRACT INFORMATION                 
         GOTO1 =A(SUBROUT),DMCB,(RC),('QDISCON',0),RR=RELO                      
         MVC   DRHLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
         B     LR70                                                             
                                                                                
LR40     DS    0H                                                               
         CLI   MYSCRNUM,X'FE'      LONG LIST SCREEN ALREADY LOADED?             
         BNE   LR50                                                             
         CLC   =C'Act',DARHDR1                                                  
         BNE   LR50                ADDITIONAL CHECK TO INSURE SCREEN IS         
*                                  LOADED                                       
         TWAXC DARSELH,DARLLIN,PROT=Y YES, CLEAR IT                             
         B     LR60                                                             
                                                                                
* LOAD LOWER SCREEN                                                             
LR50     DS    0H                                                               
         GOTO1 CALLOV,DMCB,DRFTAGH,X'D9080FFE'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCRNUM,X'FE'                                                   
*                                                                               
LR60     DS    0H                                                               
         LA    R2,DARSELH          SET FOR GENCON CURSOR POSITION               
         ST    R2,AFRSTREC                                                      
         LA    R2,DARSTATH                                                      
         ST    R2,ATHISLST                                                      
         OI    DARSELH+6,X'40'     FORCE CURSOR HERE                            
*                                                                               
         MVC   DARLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
                                                                                
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
         XC    SELCTKEY,SELCTKEY                                                
         B     LR100                                                            
*                                                                               
LR90     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
*                                                                               
***********************************************************************         
* LIST BRAND SUBSET OF ORDER                                                    
***********************************************************************         
         NI    CTLRFLG1,X'FF'-CF1BRDQ                                           
         CLC   =C'BRAND',CONREC                                                 
         BNE   LR95                                                             
*                                                                               
BLR00    DS    0H                                                               
         GOTO1 =A(SUBROUT),DMCB,(RC),('QVARINFO',0),RR=RELO                     
*                                                                               
         OI    CTLRFLG1,CF1BRDQ                                                 
         MVC   KEY(L'RDARKEY),AGYVKEY                                           
         MVI   RDARKRT,X'35'       BRAND RECORDS ONLY                           
         MVC   RDARKSEQ,BRANDSEQ                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
BLR10    DS    0H                                                               
         LA    R6,KEY                                                           
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    BLR15                                                            
         NI    BITFLAG,X'FF'-BFBRDLST                                           
         XC    BRANDSEQ,BRANDSEQ                                                
         B     LRX                                                              
*                                                                               
BLR15    DS    0H                                                               
         MVC   BRANDSEQ,RDARKSEQ                                                
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OC    RDARPDON,RDARPDON                                                
         BNZ   BLR20                                                            
*                                                                               
* VARIOUS ORDER HAS NOT BEEN CONFIRMED YET. BRAND NOT YET TRANSMITTED           
* FROM AGENCY. LIST THE BRANDS FOR USER TO SEE                                  
*                                                                               
*        XC    DARHDR1,DARHDR1                                                  
         XC    DARHDR2+28(50),DARHDR2+28                                        
*        MVC   DARHDR1+4(6),=C'BRAND#'                                          
         L     R3,ATHISLST                                                      
         USING LISTD,R3                                                         
         EDIT  RDARKSEQ,(3,LSTSTAT),ALIGN=LEFT                                  
         LA    R2,LSTSTAT                                                       
         AR    R2,R0                                                            
         MVI   1(R2),C':'                                                       
         MVC   LSTPRD1,RDARPDN1                                                 
         MVC   LSTPRD2,RDARPDN2                                                 
         B     BLR200                                                           
         DROP  R3                                                               
*                                                                               
* CONFIRMED VARIOUS ORDER, HAS BRAND ORDERS                                     
*                                                                               
BLR20    DS    0H                                                               
         TM    RDARPDFG,X'80'      BRAND ORDER CONFIRMED?                       
         BO    LRSEQ               YES, SKIP IT                                 
*                                                                               
KYD      USING RDARKEY,KEY                                                      
         MVC   KEY(L'RDARKEY),AGYVKEY                                           
         MVC   KYD.RDARKORD,RDARPDON                                            
         B     LR100                                                            
         DROP  KYD,R6                                                           
*                                                                               
BLR200   DS    0H                                                               
         OI    BITFLAG,BFBRDLST                                                 
         GOTO1 LISTMON                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     BLR10                                                            
***********************************************************************         
* END OF LIST BRAND SUBSET OF ORDER                                             
***********************************************************************         
         EJECT                                                                  
LR95     DS    0H                                                               
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
         MVI   RDARKTYP,X'41'                                                   
         CLI   STATFILT,C'F'       CONFIRMED FILTER?                            
         BNE   *+8                                                              
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,AGENCY                                                  
         CLI   DRFSTATH+5,0        FILTER ON STATION                            
         BE    LR100                                                            
         CLI   DRFSTAT,C'*'        STATION SET REQUESTED?                       
         BE    LR100               YES - DON'T SET STATION                      
         MVC   RDARKSTA,STAFILT                                                 
         DROP  R3                                                               
                                                                                
LR100    DS    0H                                                               
*        OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
LR110    DS    0H                                                               
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
         MVI   MYRECTYP,X'41'                                                   
         CLI   STATFILT,C'F'       CONFIRMED FILTER?                            
         BNE   *+8                 NO                                           
         MVI   MYRECTYP,X'51'      YES - USE 51 RECORDS                         
         CLC   RDARKTYP(1),MYRECTYP                                             
         BNE   LRX                                                              
         CLC   RDARKREP,AGENCY                                                  
         BNE   LRX                                                              
***********************************************************************         
* LIST BRAND SUBSET OF ORDER                                                    
***********************************************************************         
         CLC   =C'BRAND',CONREC                                                 
         BNE   LR115                                                            
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    LR113                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    LR210                                                            
*                                                                               
         L     R6,ATHISLST         =A(CURRENT LIST LINE)                        
         USING LISTD,R6                                                         
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),KEYSAVE+20(4)                                         
         EDIT  (P5,WORK+17),(8,LSTAGY#),ALIGN=LEFT                              
*                                                                               
         CLI   RDARCORT,C'T'                                                    
         BNE   *+8                                                              
         MVI   LSTTFLAG,C'T'                                                    
*                                                                               
         MVC   LSTSTAT,=6C'*'                                                   
         MVC   LSTPRD1,=C'*MISSING ORD'                                         
         B     LR440                                                            
         DROP  R6                                                               
*                                                                               
LR113    DS    0H                                                               
         CLI   RDARKRT,X'35'       AGENCY HEADER                                
         BNE   LRX                                                              
         B     LR210                                                            
***********************************************************************         
* END LIST BRAND SUBSET OF ORDER                                                
***********************************************************************         
*                                                                               
LR115    DS    0H                                                               
         CLI   RDARKRT,X'10'       AGENCY HEADER                                
         BNE   LRSEQ                                                            
                                                                                
LR118    DS    0H                                                               
         CLI   DRFSTATH+5,0        FILTER ON STATION                            
         BE    LR120                                                            
         CLI   DRFSTAT,C'*'        STATION SET REQUESTED?                       
         BNE   LR119               NO  - CHECK SINGLE STATION                   
         GOTO1 =A(CHECKSET),DMCB,RDARKSTA,RR=RELO                               
         BNZ   LRSEQ               NOT IN SET                                   
         B     LR120               FOUND IN SET                                 
LR119    DS    0H                                                               
         CLC   RDARKSTA(5),STAFILT                                              
         BNE   LRX                 JUST COMPARE FIRST BAND CHARACTER            
                                                                                
LR120    DS    0H                                                               
         CLI   DRFAGYH+5,0         FILTER ON AGENCY                             
         BE    LR140                                                            
         LA    RE,AGYFILT                                                       
         LA    RF,4                                                             
                                                                                
LR130    DS    0H                                                               
         OC    0(5,RE),0(RE)                                                    
         BZ    LRSEQ                                                            
         CLC   0(5,RE),SPACES                                                   
         BE    LRSEQ                                                            
         CLC   0(5,RE),RDARKAGY                                                 
         BE    LR140                                                            
         LA    RE,5(RE)                                                         
         BCT   RF,LR130                                                         
         B     LRSEQ                                                            
*                                                                               
LR140    DS    0H                                                               
         MVI   ERRFLAG,C'N'                                                     
         CLI   DRFHDLNH+5,0        IF CON# SPECIFIED, LIST ONLY THE             
         BE    LR150                AGENCY ORDER LINKED TO THIS CON#            
         OC    CDARNUM,CDARNUM     SKIP IF THERE ARE NONE                       
         BZ    LRX                                                              
         CLC   CDARNUM,RDARKORD                                                 
         BNE   LRSEQ                                                            
         DROP  R3                                                               
                                                                                
LR150    DS    0H                  FILTER ON RECEIVED DATE?                     
         CLI   DRFRDATH+5,0        RECD DATES ARE OPTIONAL                      
         BE    LR155                                                            
*                                                                               
LR152    DS    0H                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              AGENCY HEADER RECORD                         
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARCNTL,X'80'                                                   
         BZ    LR152A                                                           
         TM    RDARDELS,X'20'      FOR DELETES, PASS ONLY IF RECALL             
         BZ    LRSEQ               CANCELLED                                    
         DROP  R6                                                               
*                                                                               
LR152A   DS    0H                                                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRSEQ                                                            
         USING RDARELEM,R6                                                      
*                                                                               
         LR    R1,RA                                                            
         AHI   R1,DARPROFS-CONHEADH                                             
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    LR152AA                                                          
         DROP  R1                                                               
*                                                                               
         GOTO1 =A(CHKLOCAL),RR=RELO                                             
         BNZ   LRSEQ               CHECK IF LOCAL ORDER IN USE                  
*                                                                               
LR152AA  DS    0H                                                               
         LR    R1,RA                                                            
         AHI   R1,DARPROFS-CONHEADH                                             
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    LR152B                                                           
         DROP  R1                                                               
*                                                                               
         GOTO1 =A(CHKDIREC),RR=RELO                                             
         BNZ   LRSEQ               CHECK IF DIRECT ORDER IN USE                 
*                                                                               
LR152B   DS    0H                                                               
         CLC   FILTDAT1,FILTDAT2   ONE DATE INPUT?                              
         BNE   LR153               NO                                           
         CLC   FILTDAT1,RDARDATE                                                
         BNE   LRSEQ                                                            
         B     LR155                                                            
*                                                                               
LR153    DS    0H                                                               
         CLC   RDARDATE,FILTDAT1                                                
         BL    LRSEQ                                                            
         CLC   RDARDATE,FILTDAT2                                                
         BH    LRSEQ                                                            
         DROP  R6                                                               
*                                                                               
LR155    DS    0H                                                               
         CLI   DRFGRPH+5,0         IF EITHER GROUP OR TEAM FILTER               
         BNE   LR160               REQUESTED, READ STATION RECORD               
         CLI   DRFTEAMH+5,0        IN ORDER TO VALIDATE                         
         BE    LR210                                                            
                                                                                
LR160    DS    0H                  GET GROUP AND TEAM CODES                     
         LA    R3,KEY                                                           
         USING RDARKEY,R3                                                       
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK+8(4),RDARKSTA                                             
         MVI   MYWORK+5,4                                                       
         CLI   RDARKSTA+3,C' '                                                  
         BNE   *+8                                                              
         MVI   MYWORK+5,3          IF 3 LETTER CALL LETTERS                     
         MVI   MYWORK+2,X'40'      SET ALPHA                                    
         CLI   RDARKSTA+4,C'T'      NO NEED FOR TV BAND                         
         BE    LR170                                                            
         MVI   MYWORK+12,C'-'                                                   
         MVC   MYWORK+13(1),RDARKSTA+4                                          
         MVI   MYWORK+5,6                                                       
         CLI   RDARKSTA+3,C' '                                                  
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
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
                                                                                
         CLI   DRFGRPH+5,0         GROUP FILTER?                                
         BE    LR180                                                            
         ZIC   RF,DRFGRPH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   GRPFILT(0),WORK+31                                               
         BNE   LRSEQ                                                            
                                                                                
LR180    DS    0H                                                               
         CLI   DRFTEAMH+5,0        TEAM FILTER?                                 
         BE    LR210                                                            
                                                                                
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
         ZIC   RF,DRFTEAMH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TEAMFILT(0),RSTAOTTM                                             
         BNE   LRSEQ                                                            
         DROP  R3,R6                                                            
                                                                                
LR210    DS    0H                                                               
         MVC   ORDLSTDA,KEY+28                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARCNTL,X'80'                                                   
         BZ    LR210A                                                           
         TM    RDARDELS,X'20'      FOR DELETES, PASS ONLY IF RECALL             
         BZ    LRSEQ               CANCELLED                                    
         DROP  R6                                                               
*                                                                               
LR210A   DS    0H                                                               
         LR    R1,RA                                                            
         AHI   R1,DARPROFS-CONHEADH                                             
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTRPEAB,CNTRPEAA                                       
         BZ    LR210AA                                                          
         DROP  R1                                                               
*                                                                               
         GOTO1 =A(CHKLOCAL),RR=RELO                                             
         BNZ   LRSEQ                                                            
*                                                                               
LR210AA  DS    0H                                                               
         LR    R1,RA                                                            
         AHI   R1,DARPROFS-CONHEADH                                             
         USING SVDSECT,R1                                                       
         TM    SVPGPBIT+CNTDIRCB,CNTDIRCA                                       
         BZ    LR210AB                                                          
         DROP  R1                                                               
*                                                                               
         GOTO1 =A(CHKDIREC),RR=RELO                                             
         BNZ   LRSEQ               CHECK IF DIRECT ORDER IN USE                 
*                                                                               
LR210AB  DS    0H                                                               
         CLI   DRFOFFH+5,0         FILTER ON OFFICE?                            
         BE    LR210B                                                           
         CLC   =C'C=',DRFOFF                                                    
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
         CLI   DRFTYPEH+5,0        FILTER ON TYPE                               
         BNE   LR211040                                                         
*                                                                               
         L     R6,AIO              NO                                           
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'20'      NOTDARE?                                     
         BO    LRSEQ               YES, DO NOT DISPLAY IN INBOX                 
         DROP  R6                                                               
         B     LR270               NO, DO DISPLAY IN INBOX                      
*                                                                               
LR211040 DS    0H                                                               
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
         CLI   STATFILT,C' '       COMINATION??                                 
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
         CLI   STATFILT+1,C'E'     REVISION?                                    
         BNE   LR214                                                            
         CLI   STATFILT,C' '       COMINATION??                                 
         BE    LR213AB                                                          
         OC    RDARRNUM,RDARRNUM                                                
         BZ    LRSEQ                                                            
         CLI   STATFILT,C'*'       COMINATION??                                 
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
         TM    RDARMISC,X'20'      EXCLUE NOTDARE ORDER                         
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
         CLI   STATFILT,C'F'       CONFIRMED?                                   
         BNE   LR275                                                            
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
         BNZ   LR290                SALSPERSON FILTERS                          
         CLI   DRFSALPH+5,0        FILTER ON SALESPERSON?                       
         BNE   LRSEQ               IF YES, LINKED ORDERS ONLY                   
         B     LR340                                                            
*&&DO                                                                           
         CLI   DRFOFFH+5,0         FILTER ON OFFICE?                            
         BE    LR340                                                            
         CLC   DRFOFF(2),RDARKAOF                                               
         BE    LR340                                                            
         CLC   =C'C=',DRFOFF                                                    
         BNE   *+14                                                             
         CLC   DRFOFF+2(2),RDARKAOF                                             
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
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING RCONKEY,RF                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,WORK                                                    
         DROP  RF                                                               
                                                                                
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
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         MVC   AGYOFF,RCONKOFF                                                  
         MVC   AGYSALP,RCONSAL                                                  
         DROP  R6                                                               
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
LR298    DS    0H                                                               
         OI    BITFLAG2,B2SENT                                                  
         DROP  R6                                                               
*                                                                               
LR300    DS    0H                                                               
         CLI   DRFOFFH+5,0         FILTER ON OFFICE OR                          
         BE    LR310                                                            
         CLC   =C'C=',DRFOFF                                                    
         BNE   LR310                                                            
         CLC   AGYOFF,DRFOFF+2                                                  
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
         CLI   DRFSALPH+5,0         ON SALESPERSON?                             
         BE    LR330                                                            
         CLC   SALFILT,AGYSALP                                                  
         BE    LR330                                                            
                                                                                
LR320    DS    0H                  NO MATCH, RESTORE KEY AND SEQ NEXT           
         MVC   KEY,SVKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
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
         GOTO1 HIGH                                                             
*        CLC   KEY(L'RDARKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
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
         GOTO1 GETREC                                                           
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
         GOTO1 HIGH                                                             
         B     LRSEQ                                                            
*                                                                               
LR335    DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT LIST DARE RECORD             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*        CLC   KEY(L'RDARKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
LR340    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   LISTNUM,NLISTS      IF LISTING AT MAX, GOTO LISTMON              
         BE    LR440                                                            
                                                                                
         MVC   0(4,R4),KEY+28      SAVE DISK ADDRESS TO LIST                    
                                                                                
         L     R3,ATHISLST         =A(CURRENT LIST LINE)                        
         USING LISTD,R3                                                         
         CLI   MYSCRNUM,X'FD'                                                   
         BE    *+12                                                             
         LA    R0,DARSTATH         IF FIRST RECORD ON LIST                      
         B     *+8                                                              
         LA    R0,DRHSTATH                                                      
         CR    R3,R0                                                            
         BH    *+10                                                             
         MVC   FIRSTKEY,RDARKEY    SAVE FIRST KEY OF LIST                       
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
         TM    RDARMISC,X'04'      VARIOUS W/BRANDS                             
         BZ    LR344A                                                           
         MVC   LSTSTAT,=C'BRAND '                                               
         B     LR380                                                            
*                                                                               
LR344A   DS    0H                                                               
         TM    RDARMISC,X'20'      ORDER NOTDARED FROM AGENCY SIDE?             
         BZ    LR345                                                            
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 GREEN FOR NOTDARE                            
         MVI   LSTCOLOR,C'G'                                                    
                                                                                
         MVC   LSTSTAT,=C'NTDARE'                                               
         B     LR380                                                            
                                                                                
LR345    DS    0H                                                               
         CLI   RDARBSTS,C'C'       ORDER RECALLED?                              
         BNE   LR350                                                            
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 RED FOR RECALL/CANCEL                        
         MVI   LSTCOLOR,C'R'                                                    
                                                                                
         MVC   LSTSTAT,=C'RECALL'                                               
         TM    RDARDELS,X'20'      REVISION CANCELLED??                         
         BZ    LR380                                                            
         MVC   LSTSTAT,=C'CANCEL'                                               
         B     LR380                                                            
                                                                                
LR350    DS    0H                                                               
         CLI   RDARKTYP,X'51'      LOOKING AT CONFIRMED ORDERS?                 
         BNE   LR351               NO                                           
         MVC   LSTSTAT,=C'CNFRMD'                                               
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR383                                                            
*        OI    LSTSTATH+1,X'08'                                                 
         MVI   LSTCOLOR,C'!'                                                    
         B     LR383               WILL TURN THIS TO BLUE                       
                                                                                
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
         MVC   LSTSTAT,=C'UNLNKD'  STATUS DEFAULT TO UNLINKED                   
         OC    RDARREP#,RDARREP#                                                
         BZ    LR370                                                            
         MVC   LSTSTAT,=C'LINKED ' ORDER IS LINKED                              
                                                                                
LR360    DS    0H                                                               
         CLI   RDARBSTS,C'A'                                                    
         BNE   LR370                                                            
         MVC   LSTSTAT,=C'OPENED'                                               
*                                                                               
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR365               GREEN FOR OPENED                             
         MVI   LSTCOLOR,C'G'                                                    
*                                                                               
LR365    DS    0H                                                               
         TM    BITFLAG2,B2SENT                                                  
         BZ    LR380                                                            
         MVC   LSTSTAT2,=C'-S'                                                  
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR380                                                            
*        OI    LSTSTATH+1,X'08'                                                 
         MVI   LSTCOLOR,C'!'                                                    
         B     LR380               WILL TURN THIS TO BLUE                       
*                                                                               
LR370    DS    0H                  LINKED OR UNLINKED, ORDER CAN                
         CLI   RDARBSTS,C'R'         BE REJECTED                                
         BNE   LR380                                                            
                                                                                
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    *+8                 RED FOR REJECT                               
         MVI   LSTCOLOR,C'R'                                                    
                                                                                
         MVC   LSTSTAT,=C'REJCTD'                                               
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
         MVC   WORK(L'LSTSTAT),LSTSTAT                                          
         MVC   LSTSTAT(3),=C'REV'                                               
         MVC   LSTSTAT+3(3),WORK                                                
         CLC   =C'OPE',LSTSTAT+3                                                
         BNE   *+8                                                              
         MVI   LSTSTAT+5,C'N'      CHANGE 'REVOPE' TO REVOPN'                   
         CLC   =C'UNL',WORK                                                     
         BE    LR380A                                                           
         CLC   =C'LIN',WORK                                                     
         BE    LR380A                                                           
*                                                                               
* FLAG IF SENT TO STATION                                                       
*                                                                               
         TM    BITFLAG2,B2SENT                                                  
         BZ    LR381                                                            
         CLC   =C'OP',WORK                                                      
         BNE   LR381                                                            
         MVC   LSTSTAT2,=C'-S'                                                  
         TM    STEREOFG,STFINUSE   COLOR SCHEME FOR STEREO                      
         BZ    LR381                                                            
*        OI    LSTSTATH+1,X'08'                                                 
         MVI   LSTCOLOR,C'!'                                                    
         B     LR381               WILL TURN THIS TO BLUE                       
*                                                                               
LR380A   DS    0H                                                               
         MVC   LSTSTAT+3(3),=C'NEW'                                             
*                                                                               
LR381    DS    0H                  UNLINK REVISION CAN ONLY OCCUR               
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
         EDIT  (P5,WORK+17),(8,LSTAGY#),ALIGN=LEFT,FILL=0                       
*                                                                               
         CLI   RDARCORT,C'T'                                                    
         BNE   *+8                                                              
         MVI   LSTTFLAG,C'T'                                                    
*                                                                               
* STATION                                                                       
         MVC   LSTSTA(4),RDARKSTA                                               
         CLI   RDARKSTA+4,C' '                                                  
         BE    LR395                                                            
         MVI   LSTSTA+4,C'-'                                                    
         MVC   LSTSTA+5(1),RDARKSTA+4                                           
         CLI   RDARKSTA+3,C' '                                                  
         BNE   LR395                                                            
         MVC   LSTSTA+3(2),LSTSTA+4                                             
         MVI   LSTSTA+5,C' '                                                    
                                                                                
* AGENCY                                                                        
LR395    DS    0H                                                               
         MVC   LSTAGY(3),RDARKAGY                                               
         CLC   RDARKAGY+3(2),SPACES                                             
         BE    LR400                                                            
         LA    RE,LSTAGY                                                        
         MVI   LSTAGY+3,C' '                                                    
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RDARKAOF    AGENCY OFFICE                                
                                                                                
* OFFICE                                                                        
LR400    DS    0H                                                               
         MVC   LSTOFF,AGYOFF                                                    
                                                                                
* SALESPERSON                                                                   
         MVC   LSTSALP,AGYSALP                                                  
                                                                                
* FLIGHT DATES                                                                  
         GOTO1 DATCON,DMCB,(2,RDARESST),(5,LSTSTDT)                             
         MVI   LSTDASH,C'-'                                                     
         GOTO1 DATCON,DMCB,(2,RDARESEN),(5,LSTENDT)                             
                                                                                
* ESTIMATE NUMBER                                                               
         EDIT  RDAREST#,(4,LSTEST#),ALIGN=LEFT                                  
         DROP  R6                                                               
*                                                                               
* ESTIMATE NUMBER INCASE IT'S EBCDIC                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR402                                                            
         USING RDAREL2M,R6                                                      
         OC    RDAR2EST,RDAR2EST                                                
         BZ    LR402                                                            
         MVC   LSTEST#,RDAR2EST                                                 
         DROP  R6                                                               
                                                                                
LR402    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR410                                                            
         USING RDARCLEM,R6                                                      
                                                                                
* ADVERTISER                                                                    
         MVC   LSTADV,RDARCLNM                                                  
                                                                                
* PRODUCT 1                                                                     
         MVC   LSTPRD1,RDARPRN1                                                 
                                                                                
* PRODUCT 2                                                                     
         MVC   LSTPRD2,RDARPRN2                                                 
         DROP  R6                                                               
*                                                                               
LR403    DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLC   =C'$EDI$',RDARAGAD  CAN'T REJECT FOR EDI ORDERS                  
         BNE   LR410               REMOVE REJECT PFKEY OPTION                   
         XC    LSTPRD2,LSTPRD2                                                  
         DROP  R6                                                               
*                                                                               
LR410    DS    0H                                                               
         MVC   SVKEY,KEY                                                        
                                                                                
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'50'       GET TRAILER RECORD                           
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   LR430                                                            
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR420                                                            
         USING RDARELE9,R6                                                      
                                                                                
* TOTAL DOLLARS                                                                 
         EDIT  (P6,RDARTDOL),(14,LSTTOTL),2,COMMAS=YES                          
*                                                                               
         CLI   RDARCORT,C'T'                                                    
         BNE   LR415                                                            
         MVI   LSTTOTL,C'T'                                                     
                                                                                
* TOTAL SPOTS                                                                   
LR415    DS    0H                                                               
         EDIT  RDARTSPT,(5,LSTTSPT)                                             
         DROP  R3,R6                                                            
                                                                                
LR420    DS    0H                  RESTORE LIST KEY, IO AREA                    
         MVC   KEY,SVKEY           BEFORE GOING TO LISTMON                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*        CLC   KEY(L'RDARKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   SELCTKEY,KEY                                                     
                                                                                
LR430    DS    0H                                                               
         LA    R4,4(R4)            SAVE OFF DISK ADD                            
                                                                                
LR440    DS    0H                                                               
         GOTO1 LISTMON                                                          
*                                                                               
LRSEQ    DS    0H                                                               
         L     RF,AIO              DEBUGGING                                    
         USING RDARREC,RF                                                       
         CLC   RDARKORD,ORDNUM                                                  
         BNE   ORDNUMX                                                          
         DC    X'0770'             PATCH POINT                                  
         B     ORDNUMX                                                          
         DROP  RF                                                               
ORDNUM   DC    X'31740004'                                                      
ORDNUMX  DS    0H                                                               
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
*                                                                               
LRSEQ10  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
*   TEST                                                                        
***      LA    R0,2                                                             
****     CLC   KEY+RDARKSTA-RDARKEY(4),=C'DAB '                                 
****     BNH   TEST0020                                                         
***      MVC   DIE,=X'0000'                                                     
TEST0020 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         B     LR110                                                            
*                                                                               
LRX      DS    0H                                                               
*   LOOK FOR CONFIRM VERSION OF THE CONTRACT IF NO CON IS DISPLAYED             
         CLI   MYSCRNUM,X'FD'      SHORT LIST SCREEN?                           
         BNE   LRXA                NO                                           
         CLI   LISTNUM,0           IS THERE ANY REC BEEN DISPLAYED?             
         BNE   LRXA                NO                                           
         CLI   DRFHDLNH+5,0        CONTRACT# SPECIFIED?                         
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
INVLSTAT MVC   RERROR,=AL2(150)    INVALID STATION/STA SET                      
         B     ERREND                                                           
*                                                                               
INVLOFF  MVC   RERROR,=AL2(447)    INVALID OFFICE                               
         B     ERREND                                                           
*                                                                               
INVLSAL  MVC   RERROR,=AL2(448)    INVALID SALESPERSON                          
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
         LA    R2,DRFHDLNH          CALL DDS                                    
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
NOACCESS MVI   GERROR1,55          SECURITY LOCKOUT                             
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
ERREND   MVI   RMSGTYPE,C'E'                                                    
         B     *+8                                                              
INFEND   MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
         EJECT                                                                  
VREC     NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'REMOVE',DRFHDLN  REMOVE REQUEST?                              
         BNE   VR01                NO                                           
*                                                                               
         GOTO1 =A(VALREM),RR=RELO                                               
         BNE   INVLACT2                                                         
*                                                                               
         GOTO1 =A(DELORD),RR=RELO                                               
*                                                                               
         GOTO1 =A(MARKCON),RR=RELO                                              
*                                                                               
         XC    DRFHDLN,DRFHDLN                                                  
         MVI   DRFHDLNH+5,0                                                     
         B     EXIT2                                                            
*                                                                               
VR01     DS    0H                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   VR05                                                             
         CLC   =C'CONFIRM',DRFHDLN                                              
         BE    VR03                                                             
         CLC   =C'KILLEM',DRFHDLN                                               
         BE    VR03                                                             
*                                                                               
         B     VR05                                                             
*&&DO                                                                           
         CLC   =C'DELETE',DRFHDLN                                               
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
         CLC   =C'KILLEM',DRFHDLN                                               
         BNE   VR04                                                             
         GOTO1 =A(MARKCON),RR=RELO                                              
         GOTO1 =A(PROCMKG),RR=RELO                                              
*                                                                               
VR04     DS    0H                                                               
         XC    DRFHDLN,DRFHDLN                                                  
         MVI   DRFHDLNH+5,0                                                     
         B     EXIT2                                                            
                                                                                
VR05     DS    0H                                                               
         CLI   ACTNUM,MYACTSEL     ONLY FOR MY SELECT ACTION                    
         BNE   VRX                                                              
*                                                                               
         TM    BITFLAG2,B2GOCON    WAS PF2 PRESSED? SWAP TO CONTRACT            
         BZ    VR08                                                             
         GOTO1 =A(SUBROUT),DMCB,(RC),('QSWAPCON',0),RR=RELO                     
         NI    BITFLAG2,X'FF'-B2GOCON                                           
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
         CLI   MYSCRNUM,X'FB'      AGENCY ORDER SCREEN ALREADY LOADED?          
         BE    VR30                                                             
* LOAD LOWER SCREEN                                                             
         GOTO1 CALLOV,DMCB,DRFTAGH,X'D9080FFB'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   MYSCRNUM,X'FB'                                                   
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
*                                                                               
         GOTOR CHECKDIF                                                         
*                                                                               
*        OI    DMINBTS,X'08'       PASS BACK DELETES                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   DAREDKAD,KEY+28                                                  
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
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
         XC    DRVPFKL,DRVPFKL                                                  
         MVC   DRVPFKL(72),=C'PF2 Contract  3 Buy List  5 Print  7 HistX        
                 10 Open  11 Reject  12 Return'                                 
*                                                                               
* IF LINKED AND REVISION                                                        
*                                                                               
         GOTOR DISPDIFF                                                         
         BE    VR35H                                                            
*                                                                               
         CLI   RDARRNUM,0          REVISION - SHOW DIFF                         
         BE    VR36                                                             
*                                                                               
VR35H    DS    0H                                                               
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
         BE    VR47A                                                            
         B     VR50                                                             
*                                                                               
VR47A    DS    0H                                                               
         MVC   DRVPFKL+47(8),DRVPFKL+58                                         
         XC    DRVPFKL+55(11),DRVPFKL+55                                        
*                                                                               
VR50     DS    0H                                                               
         LA    RF,CCONLEN                                                       
         XCEF  CCONNUM,(RF)        CLEAR ALL CONTRACT GLOBAL VALUES             
         MVC   SELCTKEY,0(R6)      SAVE FOR WHEN WE RETURN TO LIST              
         MVC   AORDNUM,RDARKORD                                                 
         OC    RDARREP#,RDARREP#                                                
         BZ    VR60                                                             
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,DRFHDLN),ALIGN=LEFT                              
         STC   R0,DRFHDLNH+5       SET LENGTH OF DESTINATION                    
         MVI   DRFHDLNH+4,X'08'    SET VALID NUMERIC                            
         DROP  R6                                                               
*                                                                               
VR60     DS    0H                                                               
         LA    R2,DRFHDLNH                                                      
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
                                                                                
VR90     DS    0H                  VALIDATE LINK/UNLINK                         
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
         LA    R2,DRFHDLNH                                                      
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
         LA    R2,DRFHDLNH                                                      
         B     ERTKOVER                                                         
*                                                                               
VR123    DS    0H                  IF LINK IS REQUESTED                         
         LA    R2,DRFHDLNH         AND NO CONTRACT NUMBER SPECIFIED             
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
         LA    R2,DRFHDLNH         POINT HERE IF ERROR                          
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
         BE    ERBUYFND            BUY FOUND IN CONTRACT                        
*                                                                               
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
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
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
*                                  RE-READ FOR PUTREC                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   RDARREP#,CCONKNUM                                                
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
         DROP  R6                                                               
*                                                                               
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
*                                  RE-READ FOR PUTREC                           
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         XC    RDARREP#,RDARREP#                                                
         GOTO1 PUTREC                                                           
         XC    DRVSTAT,DRVSTAT                                                  
         MVC   DRVSTAT(27),=C'*** ORDER IS NOT LINKED ***'                      
         OI    6(R2),X'80'         XMIT                                         
         MVC   DRVQLNK(16),=C'  (L)ink Order ?'                                 
         OI    DRVQLNKH+6,X'80'                                                 
         XC    DRVLINK,DRVLINK                                                  
         MVI   DRVLINKH+5,0                                                     
         OI    DRVLINKH+6,X'80'+X'40' XMIT AND PLACE CURSOR                     
*                                                                               
* RESET PFKEY AFTER UNLINKED                                                    
*                                                                               
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
INVLFLD2 MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* CHECK IF LOCAL SIGNON HOME MARKET IN USE                                      
* IF LOCAL SIGNON HOME MARKET IN USE, ONLY PASS LOCAL ORDERS BY                 
* FILTERING SIGNON ID AGAINST RECEIVER ID IN DARE HEADER RECORD                 
*                                                                               
* ASSUMES AIO HAS DARE HEADER RECORD                                            
*                                                                               
CHKLOCAL NTR1  BASE=*,LABEL=*                                                   
         LR    R4,RA               USE R2                                       
         AHI   R4,DARPROFS-CONHEADH                                             
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
CHKLYES  SR    RC,RC                                                            
CHKLNO   LTR   RC,RC                                                            
CHKLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*=====================================================================          
* CHKREV: CHECK TO SEE IF WE SHOULD CALL OLD REVISION                           
*         OR THE NEW REVISION, MODIFY THE RECACT TABLE ON THE FLY               
*=====================================================================          
CHKREV   NTR1  BASE=*,LABEL=*                                                   
         NI    MISCFLG1,X'FF'-MF1NREV                                           
         GOTOR ISNEWREV            READ OFFICE RECORD                           
         BNE   CHKRX                                                            
*                                                                               
CHKR150  DS    0H                                                               
         OI    MISCFLG1,MF1NREV                                                 
*                                                                               
CHKRX    DS    0H                                                               
         XIT1                                                                   
*=====================================================================          
* NEWREV: READ OFFICE RECORD TO SEE IF WE ARE GOING TO NEW REVISION             
*         CC EQU: USE NEW REVISION                                              
*         CC NEQU: USE OLD REVISION                                             
*=====================================================================          
ISNEWREV NTR1  BASE=*,WORK=(R4,IMWORKQ),LABEL=*                                 
         USING IMWORKD,R4                                                       
         MVC   IMSVKEY,KEY                                                      
         MVC   IMSVIO,AIO                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ROFF2KEY,R6                                                      
         MVI   ROFF2TYP,X'44'                                                   
         MVC   ROFF2REP,AGENCY     REP                                          
*                                                                               
         CLC   =C'O=',TWAACCS                                                   
         BNE   ISNEWNO                                                          
         MVC   ROFF2OFF,TWAACCS+2                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,         X        
               0,0                                                              
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ISNEWNO                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IMIO,DMWORK,0         
*                                                                               
         LA    R6,IMIO                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   ISNEWNO                                                          
         USING ROFF2CDE,R6                                                      
*                                                                               
         TM    ROFF2PRF+1,X'40'                                                 
         BZ    ISNEWNO                                                          
*                                                                               
ISNEWYES SR    R5,R5                                                            
ISNEWNO  LTR   R5,R5                                                            
ISNEWRX  DS    0H                                                               
         MVC   AIO,IMSVIO                                                       
         MVC   KEY,IMSVKEY                                                      
         XIT1                                                                   
         DROP  R4,R6                                                            
*======================================================================         
* DISPLAY DIFFENRECE BUTTON?                                                    
*======================================================================         
DISPDIFF NTR1  BASE=*,LABEL=*                                                   
         TM    MISCFLG1,MF1NREV    NEW REVISION                                 
         BZ    DDIFNO                                                           
         TM    CTLRFLG1,CF1PDING   NOT PENDING - SHOW DIFF                      
         BZ    DDIFNO                                                           
DDIFYES  SR    RC,RC                                                            
DDIFNO   LTR   RC,RC                                                            
DDIFX    XIT1                                                                   
*                                                                               
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
         GOTOR GETREC                                                           
                                                                                
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
         LA    R2,DRFRQTRH                                                      
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
         MVC   REQINIT,DRFRQTR                                                  
         B     VK05                                                             
*                                                                               
VK03     DS    0H                                                               
         MVC   DRFRQTR,REQINIT                                                  
*                                                                               
VK05     DS    0H                                                               
         CLI   ACTNUM,ACTLIST      ONLY FOR ACTION LIST                         
         BNE   VKX                                                              
                                                                                
         MVI   NLISTS,7            LONG LIST                                    
                                                                                
         LA    R2,DRFHDLNH                                                      
         CLI   5(R2),0                                                          
         BE    VK50                                                             
                                                                                
         MVI   NLISTS,4            SHORT LIST W/CONTRACT HEADER                 
                                                                                
*        TM    4(R2),X'20'                                                      
*        BNZ   VK50                                                             
                                                                                
         GOTO1 VALICON,DMCB,(R2)                                                
         BNZ   MISSREC                                                          
         OI    4(R2),X'20'                                                      
         XC    SELCTKEY,SELCTKEY                                                
                                                                                
         CLI   ACTNUM,ACTLIST      FOR LIST WITH CONTRACT HEADER                
         BNE   VK50                SPECIFIED, STUFF FILTER WITH                 
         MVC   DRFSTAT(6),ESTATION INSERT FILTERS FROM CONTRACT                 
         OI    DRFSTATH+4,X'20'                                                 
         MVI   DRFSTATH+5,6                                                     
         OI    DRFSTATH+6,X'80'    XMIT                                         
         CLC   DRFSTAT+4(2),SPACES                                              
         BNE   *+10                                                             
         MVC   DRFSTAT+4(2),=C'-T'                                              
                                                                                
         MVC   STAFILT,ESTATION    SAVE FOR LIST FILTERING                      
         CLI   STAFILT+4,C' '                                                   
         BNE   VK10                                                             
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
                                                                                
VK10     DS    0H                                                               
         OI    DRFAGYH+4,X'20'                                                  
         XC    DRFAGY,DRFAGY                                                    
         MVC   DRFAGY(4),CCONKAGY  AGENCY                                       
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    VK20                                                             
         MVI   DRFAGY+4,C' '                                                    
         LA    RE,DRFAGY                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
VK20     DS    0H                                                               
         MVC   AGYFILT,CDARAGY     SAVE FOR LIST FILTERING                      
         OI    DRFAGYH+6,X'80'     XMIT                                         
                                                                                
VK50     DS    0H                                                               
*        MVI   MYRECTYP,X'41'      DEFAULT                                      
         LA    R2,DRFTYPEH                                                      
         TM    4(R2),X'20'                                                      
         BO    VK90                                                             
*                                                                               
         XC    SELCTKEY,SELCTKEY                                                
         MVC   STATFILT,DRFTYPE                                                 
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
*        CLI   STATFILT,C'F'       CONFIRMED FILTER?                            
*        BNE   *+8                 NO                                           
*        MVI   MYRECTYP,X'51'      YES - USE 51 RECORDS                         
         B     VK90                                                             
*                                                                               
*                                                                               
* LIST OF VALID FILTERS                                                         
*                                                                               
STATLIST DC    C' ULBARNCSVIE*F'                                                
         DC    X'FF'                                                            
*                                                                               
VK90     DS    0H                  GROUP/SUBGROUP FILTER                        
         LA    R2,DRFGRPH                                                       
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
         LA    R2,DRFTEAMH                                                      
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
         LA    R2,DRFSTATH                                                      
         CLI   8(R2),C'*'          SET REQUEST?                                 
         BNE   VK112               NO  -                                        
*                                  YES -                                        
*   MUST REFRESH SET FOR EACH SCREEN.  TWA IS CLEARED, AND                      
*        MECHANISM TO SAVE AND RESTORE IT IS NOT KEEPING THE                    
*        TABLE USED FOR THE STATION SET.                                        
*                                                                               
         GOTO1 =A(VSTASET),RR=RELO                                              
         BNZ   INVLSTAT            ERROR: SET NOT FOUND                         
         MVC   STAFILT(5),DRFSTAT  SAVE FILTER                                  
         B     VK120               SAVED: PROCEED TO NEXT CHECK                 
VK112    EQU   *                                                                
*                                                                               
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK120                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    STAFILT,STAFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK120                                                            
         GOTO1 VALISTA                                                          
         MVC   STAFILT,WORK                                                     
         OC    STAFILT,SPACES                                                   
         CLI   STAFILT+4,C' '                                                   
         BNE   VK120                                                            
         MVI   STAFILT+4,C'T'      DEFAULT TO TV                                
                                                                                
VK120    DS    0H                                                               
         LA    R2,DRFAGYH                                                       
                                                                                
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
         MVC   WORK(L'DRFAGY),DRFAGY                                            
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
         LA    R2,DRFOFFH                                                       
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
         CLI   DRFHDLNH+5,0                                                     
         BNE   VK165                                                            
*                                                                               
         CLI   5(R2),0             DEFAULT OFFICE RESTRICTION                   
         BNE   VK170                                                            
         MVC   DRFOFF(2),TWAACCS+2                                              
         MVI   DRFOFFH+5,2                                                      
         B     VK190                                                            
*                                                                               
VK165    DS    0H                                                               
         CLC   =C'BRAND',CONREC                                                 
         BE    VK190                                                            
         MVC   DRFOFF(2),=C'C='                                                 
         MVC   DRFOFF+2(2),TWAACCS+2                                            
         MVI   DRFOFFH+5,4                                                      
         B     VK190                                                            
*                                                                               
VK170    DS    0H                                                               
*&&DO                                                                           
*HQ                                                                             
         CLC   =C'BL',AGENCY                                                    
         BNE   VK175                                                            
         CLC   =C'SA',TWAACCS+2                                                 
         BNE   VK178                                                            
         CLC   =C'PO',DRFOFF                                                    
         BE    VK190                                                            
         CLC   =C'C=PO',DRFOFF                                                  
         BE    VK190                                                            
         B     VK178                                                            
VK175    DS    0H                                                               
         CLC   =C'PV',AGENCY                                                    
         BNE   VK176                                                            
         CLC   =C'SE',TWAACCS+2                                                 
         BNE   VK178                                                            
         CLC   =C'PO',DRFOFF                                                    
         BE    VK190                                                            
         CLC   =C'C=PO',DRFOFF                                                  
         BE    VK190                                                            
VK176    DS    0H                                                               
         CLC   =C'CQ',AGENCY                                                    
         BNE   VK177                                                            
         CLC   =C'LA',TWAACCS+2                                                 
         BNE   VK177A                                                           
         CLC   =C'SF',DRFOFF                                                    
         BE    VK190                                                            
         CLC   =C'C=SF',DRFOFF                                                  
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
         CLC   =C'DN',DRFOFF                                                    
         BE    VK190                                                            
         CLC   =C'DV',DRFOFF                                                    
         BNE   VK178                                                            
         MVC   DRFOFF(2),=C'DN'                                                 
         B     VK190                                                            
*HQ                                                                             
*&&                                                                             
VK178    DS    0H                                                               
         CLC   DRFOFF(2),TWAACCS+2                                              
         BE    VK180                                                            
         CLC   =C'C=',DRFOFF                                                    
         BNE   NOACCESS                                                         
         CLC   DRFOFF+2(2),TWAACCS+2                                            
         BNE   NOACCESS                                                         
*                                                                               
VK180    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VK200                                                            
*                                                                               
VK190    DS    0H                                                               
         MVC   OFFFILT,8(R2)                                                    
         CLC   =C'C=',DRFOFF                                                    
         BNE   VK200                                                            
*                                                                               
VK195    DS    0H                                                               
         GOTO1 VALIOFF             VALIDATE ONLY IF FILTERING ON                
         BNE   INVLOFF             CONTRACT OFFICE                              
         MVC   OFFFILT,10(R2)                                                   
                                                                                
VK200    DS    0H                                                               
         LA    R2,DRFSALPH                                                      
         TM    DRFSALPH+4,X'20'                                                 
         BO    VK220                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    SALFILT,SALFILT                                                  
         CLI   5(R2),0                                                          
         BE    VK220                                                            
         CLI   DRFOFFH+5,0         SALESPERSON FILTER NEEDS OFFICE              
         BNE   VK210                                                            
         LA    R2,DRFOFFH                                                       
         B     NEEDOFF                                                          
                                                                                
VK210    DS    0H                                                               
         GOTO1 VALISAL             REVALIDATE EVERYTIME                         
         BNE   INVLSAL                                                          
         CLC   OFFFILT,WORK+20     SALESPERSON OFFICE MUST MATCH OFFICE         
         BNE   INVLSAL             FILTER                                       
         MVC   SALFILT,8(R2)                                                    
         OC    SALFILT,SPACES                                                   
*                                                                               
VK220    DS    0H                                                               
         LA    R2,DRFRDATH                                                      
         TM    4(R2),X'20'         REVALIDATE IF CHANGED                        
         BO    VK300                                                            
         XC    SELCTKEY,SELCTKEY                                                
         XC    FILTDAT1(4),FILTDAT1                                             
         CLI   5(R2),0                                                          
         BE    VK300                                                            
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),DATEBLCK                               
         CLI   DMCB+4,0            FIELDS 1 & 2 VALID                           
         BE    VK230                                                            
         CLI   DMCB+4,4            ONLY ONE DATE INPUT                          
         BNE   INVRDATE                                                         
*                                                                               
VK230    MVC   FILTDAT1,DATEBLCK+34 COMPRESSED FIRST DATE                       
         MVC   FILTDAT2,DATEBLCK+36 COMPRESSED LAST DATE                        
*                                                                               
VK300    DS    0H                                                               
         OI    DRFSTATH+4,X'20'                                                 
         OI    DRFGRPH+4,X'20'                                                  
         OI    DRFTEAMH+4,X'20'                                                 
         OI    DRFSTATH+4,X'20'                                                 
         OI    DRFAGYH+4,X'20'                                                  
         OI    DRFOFFH+4,X'20'                                                  
         OI    DRFSALPH+4,X'20'                                                 
         OI    DRFRDATH+4,X'20'                                                 
*                                                                               
VKX      DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
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
*                                                                               
         CLC   0(5,R2),0(R3)       STATION IN TABLE -                           
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
         GOTO1 HIGH                LOOK FOR EXACT KEY                           
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
         GOTO1 GETREC              RETRIEVE RECORD                              
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
         GOTO1 HIGH                RETRIEVE RECORD FOR SET                      
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   LSET0080            NO  - SKIP IT                                
         GOTO1 GETREC              RETRIEVE SET RECORD                          
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
         BNE   LSET2010            OLD FORMAT - NO X'01' ELEMENT                
*                                     ALREADY AT X'10'                          
         ZIC   RF,1(R4)            BUMP TO X'10' ELEMENT                        
         AR    R4,RF                                                            
LSET2010 EQU   *                                                                
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
         LA    R1,4                SET SIZE OF ENTRY (-1 FOR EX)                
         EX    R1,LSETMVC          MOVE  ENTRY BY LENGTH                        
         CLI   4(RE),C' '          MEDIA = SPACE?                               
         BH    LSET2030            NO                                           
         MVI   4(RE),C'T'          YES - SET TO 'T'                             
LSET2030 EQU   *                                                                
         AH    RE,=H'5'            BUMP TO NEXT SLOT                            
         EX    R4,LSETXC           CLEAR ENTRY BY LENGTH                        
         B     LSET2040                                                         
*                                                                               
LSETMVC  MVC   0(0,RE),0(R4)                                                    
LSETXC   XC    0(0,RE),0(RE)       CLEAR THE NEXT SLOT: SETS END                
*                                                                               
LSET2040 EQU   *                      OF SET VALUE OF X'00'                     
         AH    R4,=H'5'            BUMP TO NEXT SLOT                            
         BCT   RF,LSET2020         LOOP                                         
         ST    RE,ANEXTSET         SAVE NEXT SLOT FOR NEXT ELT                  
         L     R4,SAVEREGX         RESET A(ELT BEING PROCESSED)                 
LSET2050 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         CLI   0(R4),0             END OF RECORD?                               
         BE    LSET2060            YES                                          
         CLI   0(R4),X'20'         SET ELEMENT?                                 
         BNE   LSET2050            NO  - SKIP IT                                
         ST    R4,SAVEREGX         YES - SAVE A(ELT BEING PROCESSED)            
         ZIC   RF,1(R4)            GET ELT LENGTH                               
         LA    RE,3                SUBTRACT CONTROL                             
         SR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'5'            DIVIDE BY ENTRY LENGTH                       
         LA    R4,3(R4)            A(1ST ENTRY IN ELT)                          
         L     RE,ANEXTSET         RESTORE NEXT SLOT FOR NEW ELT                
         B     LSET2020            GO BACK AND LOAD ELEMENTS                    
LSET2060 EQU   *                                                                
         ST    RE,ANEXTSET         SET A(NEXT OPEN SLOT)                        
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
* PRINT A REPORT                                                                
***********************************************************************         
PR       NTR1  BASE=*,WORK=(R4,IMWORKQ),LABEL=*                                 
         USING IMWORKD,R4                                                       
         MVC   IMSVIO,AIO                                                       
         MVC   IMSVKEY,KEY                                                      
         MVC   IMSVKEY2,SELECTKY                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         MVI   BYTE2,X'32'          NEW REVISION                                
         TM    MISCFLG1,MF1NREV                                                 
         BO    *+8                                                              
         MVI   BYTE2,X'30'          OLD REVISION                                
*                                                                               
         TM    BITFLAG,BFPRINT     CAME FROM MY SELECT SCREEN?                  
         BO    PR01                YES, SKIP                                    
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
         GOTO1 VLOAD,DMCB,(BYTE2,0),('QPRONE',0)                                
         B     PR04                                                             
*                                                                               
PR03     DS    0H                                                               
         L     R3,VREDAR24                                                      
         GOTOR NEWDIF                                                           
         BE    *+8                                                              
         L     R3,VREDAR22                                                      
*                                                                               
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC),('QPRONE',0)                                      
*        GOTO1 VREDAR22,DMCB,(RC),('QPRONE',0)                                  
*                                                                               
PR04     DS    0H                                                               
         GOTO1 VTOUCHED                                                         
         B     PR20                ALL DONE, CLOSE PQ                           
                                                                                
PR05     DS    0H                  PRINT FROM LIST SCREEN                       
         OI    CTLRFLG1,CF11STKY   SELECTION FOUND, REDISPLAY PAGE              
         TM    MISCFLG4,MF4PRDIF                                                
         BO    PR08                                                             
PR06     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(BYTE2,0),0                                           
         B     PR10                                                             
PR08     DS    0H                                                               
         L     R3,VREDAR24                                                      
         GOTOR NEWDIF                                                           
         BE    *+8                                                              
         L     R3,VREDAR22                                                      
*                                                                               
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC),('QPRONE',0)                                      
*        GOTO1 VREDAR22,DMCB,(RC),('QPRONE',0)                                  
                                                                                
PR10     DS    0H                  PROCESS MULTIPLE PRINT REQUESTS              
* IF USER IS PRINTING FROM THE LIST SCREEN AND                                  
* IF THE AGENCY ORDER IS BEING PROCESSED FOR THE FIRST TIME                     
* RECORD CURRENT ACTIVITY DATE/TIME                                             
         GOTO1 VTOUCHED                                                         
                                                                                
         BAS   RE,TEST4PRT         CHECK IF ANYTHING LEFT TO PRINT              
         BNZ   PR20                IF NOT, CLOSE THE PQ                         
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
*                                                                               
PR13     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(BYTE2,0),('QPRNEWPG',0)                              
         B     PR10                                                             
PR15     DS    0H                                                               
         L     R3,VREDAR24                                                      
         GOTOR NEWDIF                                                           
         BE    *+8                                                              
         L     R3,VREDAR22                                                      
*                                                                               
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC),('QPRNEWPG',0)                                    
*        GOTO1 VREDAR22,DMCB,(RC),('QPRNEWPG',0)                                
         B     PR10                                                             
                                                                                
PR20     DS    0H                                                               
         TM    MISCFLG4,MF4PRDIF                                                
         BO    PR30                                                             
PR25     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(BYTE2,0),('QPRCLOSE',0)                              
         B     PRYES                                                            
*                                                                               
PR30     DS    0H                                                               
         L     R3,VREDAR24                                                      
         GOTOR NEWDIF                                                           
         BE    *+8                                                              
         L     R3,VREDAR22                                                      
*                                                                               
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,(RC),('QPRCLOSE',0)                                    
*        GOTO1 VREDAR22,DMCB,(RC),('QPRCLOSE',0)                                
*                                                                               
PRYES    SR    R5,R5                                                            
PRNO     LTR   R5,R5                                                            
         MVC   AIO,IMSVIO                                                       
         MVC   KEY,IMSVKEY                                                      
         MVC   SELECTKY,IMSVKEY2                                                
PRXIT    XMOD1                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
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
                                                                                
         XC    DRHAGYC,DRHAGYC                                                  
         MVC   DRHAGYC(L'CCONKAGY),CCONKAGY                                     
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    DH10                                                             
         LA    RE,DRHAGYC                                                       
         MVI   DRHAGYC+4,C' '                                                   
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
                                                                                
DH10     DS    0H                                                               
         MVC   DRHAGYN,EAGYNAM1                                                 
                                                                                
* BUYER                                                                         
         MVC   DRHBUY,ECONBUYR                                                  
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   DH15                                                             
* FOR DDS TERMINALS, DISPLAY D/A OF K, NOT BUYER NAME                           
         XC    DRHBUY,DRHBUY                                                    
         MVC   DRHBUY(4),=C'D/A='                                               
         GOTO1 HEXOUT,DMCB,CCONDKAD,DRHBUY+5,L'CCONDKAD,0                       
                                                                                
* ESTIMATE                                                                      
DH15     DS    0H                                                               
         MVC   DRHEST,CCONIEST                                                  
         CLC   DRHEST,SPACES                                                    
         BNE   *+10                                                             
         MVC   DRHEST,CCONXEST                                                  
                                                                                
* ADVERTISER + NAME                                                             
         MVC   DRHADV,CCONKADV                                                  
         MVC   DRHADVN,EADVNAME                                                 
                                                                                
* PRODUCT + PRODUCT NAME                                                        
         MVC   DRHPRD(3),CCONPRD                                                
         OC    CCONPRD,CCONPRD                                                  
         BZ    *+14                                                             
         MVC   DRHPRD+4(16),EPRDNAME                                            
         B     *+10                                                             
         MVC   DRHPRD,EPRDNAME                                                  
                                                                                
* STATION + MARKET                                                              
         MVC   DRHSTA(4),CCONKSTA                                               
         MVC   DRHSTA+4(3),=C'-TV'                                              
         CLI   CCONKSTA+4,C' '                                                  
         BE    DH17                                                             
         CLI   CCONKSTA+4,C'L'                                                  
         BNE   DH16                                                             
         MVC   DRHSTA+4(3),=C'-L '                                              
         B     DH17                                                             
*                                                                               
DH16     DS    0H                                                               
         MVC   DRHSTA+4(3),=C'- M'                                              
         MVC   DRHSTA+5(1),CCONKSTA+4                                           
                                                                                
DH17     DS    0H                                                               
         CLI   CCONKSTA+3,C' '                                                  
         BNE   DH18                                                             
         MVC   DRHSTA+3(3),DRHSTA+4                                             
         MVI   DRHSTA+6,C' '                                                    
                                                                                
DH18     DS    0H                                                               
         MVC   DRHSTAN,EMKTNAME                                                 
                                                                                
* SALESPERSON                                                                   
         MVC   DRHSAL,CCONSAL                                                   
         MVC   DRHOFFC,CCONKOFF                                                 
         MVC   DRHSALN,ESALNAME                                                 
                                                                                
* START AND END DATES                                                           
         MVC   DRHDATE,ECONDATE                                                 
                                                                                
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
         MVC   DRHDPRT,MYWORK                                                   
                                                                                
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
         MVC   DRHLEN,MYWORK                                                    
                                                                                
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
                                                                                
         GOTO1 DEMOCON,DMCB,(2,MYWORK),(9,DRHDEMO),(0,DBLOCKD)                  
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
*MN      PASS ADDRESS OF REC TO RECNT10 IF PROFILE IS ON                        
         LR    R1,RA                                                            
         AHI   R1,DARPROFS-CONHEADH                                             
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
         LA    R2,DRFHDLNH                                                      
         XC    DRFHDLN,DRFHDLN                                                  
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
         EDIT  (P5,WORK+17),(8,DRFHDLN),ALIGN=LEFT                              
         STC   R0,DRFHDLNH+5                                                    
         OI    DRFHDLNH+4,X'08'                                                 
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
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARKTYP,X'51'      USING CONFIRMED CONTRACTS?                   
         BE    SWPK040             YES - CALL CONRACT MGO SCREEN                
         DROP  R6                                                               
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',CCONKNUM,L'CCONKNUM,GLRDISPK              
         CLI   DMCB+8,0                                                         
         BE    SWPK100                                                          
         DC    H'0'                                                             
*                                                                               
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
                                                                                
         CLI   MYSCRNUM,X'FD'      FOR SHORT LIST SCREEN                        
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
         LA    R0,DRHSELH          CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFD2                                                         
         LA    R0,DRHLLINH                                                      
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
         GOTO1 GETREC                                                           
*                                                                               
*                                                                               
         NI    CTLRFLG1,X'FF'-CF1PDING                                          
         GOTOR CHKPDING                                                         
         BE    *+8                                                              
         OI    CTLRFLG1,CF1PDING   ON = NOT PENDING                             
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
*                                                                               
         CLI   PFKEY,2             ONLY IF LINKED TO CONTRACT                   
         BNE   STPFKV05                                                         
         OC    RDARREP#,RDARREP#                                                
         BZ    ORDNTLK2                                                         
         OI    BITFLAG2,B2GOCON    SET SWAP TO CONTRACT                         
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
         NI    CTLRFLG1,X'FF'-CF1ISREV                                          
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
*        XC    DRFVARI,DRFVARI                                                  
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
         AHI   R0,12                                                            
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
         LA    R2,DRFHDLNH          CALL DDS                                    
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
         LA    R2,DRFHDLNH          CALL DDS                                    
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
LPF21    DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                   
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
LPF03    DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                   
LPF03X   EQU   *                                                                
*                                                                               
* CONTRACT                                                                      
         DC    AL1(LPF04X-*,04,0,0,(LPF04X-LPF04)/KEYLNQ,0)                     
         DC    CL3' ',CL8'CONTRACT',CL8'LIST'                                   
LPF04    DC    AL1(KEYTYTWA,L'DRVASTA-1),AL2(DRVASTA-T80FFFD)                   
         DC    AL1(KEYTYTWA,L'DRFOFF-1),AL2(DRFOFF-T80FFFD)                     
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
*LPF06    DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                  
         DC    AL1(LPF06X-*,06,0,0,0,0)                                         
         DC    CL3' ',CL8'BRAND',CL8'LIST'                                      
LPF06X   EQU   *                                                                
*                                                                               
* HISTORY LIST                                                                  
         DC    AL1(SPF07X-*,07,0,0,(SPF07X-SPF07)/KEYLNQ,0)                     
         DC    CL3' ',CL8'HISTORY',CL8'LIST'                                    
SPF07    DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                   
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
BLPF21   DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                   
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
BPF03    DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                   
BPF03X   EQU   *                                                                
*                                                                               
* CONTRACT                                                                      
         DC    AL1(BPF04X-*,04,0,0,(BPF04X-BPF04)/KEYLNQ,0)                     
         DC    CL3' ',CL8'CONTRACT',CL8'LIST'                                   
BPF04    DC    AL1(KEYTYTWA,L'DRVASTA-1),AL2(DRVASTA-T80FFFD)                   
         DC    AL1(KEYTYTWA,L'DRFOFF-1),AL2(DRFOFF-T80FFFD)                     
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
*BPF06    DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                  
*BPF06X   EQU   *                                                               
*                                                                               
*                                                                               
* HISTORY LIST                                                                  
         DC    AL1(BPF07X-*,07,0,0,(BPF07X-BPF07)/KEYLNQ,0)                     
         DC    CL3' ',CL8'HISTORY',CL8'LIST'                                    
BPF07    DC    AL1(KEYTYTWA,L'DRFHDLN-1),AL2(DRFHDLN-T80FFFD)                   
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
         XC    DRFHDLN,DRFHDLN                                                  
         OI    DRFHDLNH+6,X'40'    FORCE CURSOR HERE                            
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
         OI    DRFHDLNH+6,X'40'+X'80'                                           
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
         OI    DRFHDLNH+6,X'40'+X'80'                                           
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
         OI    DRFHDLNH+6,X'40'+X'80'                                           
         TM    CTLRFLG1,CF1BRDQ    SKIP FOR BRAND                               
         BO    *+8                 NEED AT LEAST ONE BLANK FIELD                
         OI    DRVLINKH+1,X'20'    PROTECT INPUT FIELD                          
         B     DISA110                                                          
                                                                                
DISA80   DS    0H                                                               
         CLI   RDARBSTS,C'R'                                                    
         BE    DISA90                                                           
         CLI   RDARBSTS,C'C'                                                    
         BNE   DISA110                                                          
                                                                                
DISA90   DS    0H                                                               
         LA    R2,DRVSTAT+32                                                    
         MVC   DRVSTAT(42),=C'* REJECTED ORDER LINKED TO CON#          X        
               *'                                                               
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
* ESTIMATE NUMBER INCASE IT'S EBCDIC                                            
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DISA178                                                          
         USING RDAREL2M,R6                                                      
         OC    RDAR2EST,RDAR2EST                                                
         BZ    DISA178                                                          
         XC    DRVAEST,DRVAEST                                                  
         MVC   DRVAEST(L'RDAR2EST),RDAR2EST                                     
         DROP  R6                                                               
                                                                                
*                                                                               
* SHOW IF TRADE                                                                 
DISA178  DS    0H                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RDARELEM,R6                                                      
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
         AHI   R2,DARPROFS-CONHEADH                                             
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
*                                                                               
         GOTOR COMPDEM             COMPARE DEMO CATEGORY ON RESENT/REV          
*                                                                               
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
         TM    DEMOFLG,X'80'       FLAG PRIMARY DEMO IF IT IS CHANGED           
         BZ    *+12                                                             
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
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
         ZIC   R1,BYTE                                                          
         SLL   R1,1                                                             
         STC   R1,BYTE                                                          
         BCT   R3,DISA233                                                       
DISA234A DS    0H                                                               
         MVC   0(7,R2),0(R4)                                                    
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
         BNZ   DISAX                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 =A(DISTOTAL),RR=RELO                                             
         MVC   AIO,AIO1                                                         
*                                                                               
DISAX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
BADERR3  MVC   RERROR,=AL2(440)    UNEXPECTED ERROR ENCOUNTERED,                
         LA    R2,DRFHDLNH          CALL DDS                                    
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE DEMO CAT WITH SHADOW OR 51 RECORD, AND MARK DEMOFLG                   
* R6 -> DEMO ELEMENT                                                            
***********************************************************************         
COMPDEM  NTR1  BASE=*,WORK=(R2,IMWORKQ),LABEL=*                                 
*                                                                               
         USING IMWORKD,R2                                                       
         MVC   IMSVKEY,KEY                                                      
         MVC   IMSVIO,AIO                                                       
*                                                                               
         XC    DEMOFLG,DEMOFLG                                                  
         USING RDARDMEL,R6                                                      
         LA    R4,RDARDEM1                                                      
         DROP  R6                                                               
*                                                                               
         CLI   KEY,X'51'            ARE WE LOOKING AT A CF CONTRACT             
         BE    COMPDEMX             YES, DON'T NEED TO COMPARE                  
*                                                                               
CDEM0050 DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   COMPDEMX                                                         
*                                                                               
         USING RDARCODE,R6                                                      
         L     RF,AIO                                                           
         MVC   KEY(27),0(RF)                                                    
         MVI   KEY+1,X'01'                                                      
         TM    RDARMISC,X'80'       IS THIS A RESENT?                           
         BO    CDEM0100             YES                                         
         CLI   RDARRNUM,0           NO, IF REV# = 0                             
         BE    CDEM0100                                                         
         MVC   KEY(2),=X'5100'      READ 51 FOR COMPARISON!                     
         DROP  R6                                                               
*                                                                               
CDEM0100 DS    0H                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   CDEMNO                                                           
*                                                                               
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CDEMNO                                                           
         USING RDARDMEL,R6                                                      
         LA    R3,RDARDEM1                                                      
         DROP  R6                                                               
*                                                                               
         LA    R1,4                                                             
*                                                                               
CDEM0200 DS    0H                                                               
         CLC   0(L'RDARDEM1,R3),0(R4)                                           
         BE    *+8                                                              
         OI    DEMOFLG,X'08'                                                    
         ZIC   RE,DEMOFLG                                                       
         SLL   RE,1                                                             
         STC   RE,DEMOFLG                                                       
         LA    R3,L'RDARDEM1(R3)                                                
         LA    R4,L'RDARDEM1(R4)                                                
         BCT   R1,CDEM0200                                                      
*                                                                               
CDEMYES  SR    R5,R5                                                            
CDEMNO   LTR   R5,R5                                                            
         MVC   KEY,IMSVKEY                                                      
         MVC   AIO,IMSVIO                                                       
COMPDEMX XIT1                                                                   
         DROP  R2                                                               
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
         CLC   DRFOFF(2),0(R4)                                                  
         BNE   COFFNO                                                           
         B     COFFYES                                                          
*                                                                               
COFF40   DS    0H                                                               
         CLC   RDARKAOF,DRFOFF     NOT FOUND, USE AGENCY OFFICE                 
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
         CLC   =C'CONFIRM',DRFHDLN                                              
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
         MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELO260                                                          
         MVC   AIO,AIO2            51 ALREADY THERE?                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              DO PUTREC                                    
         NI    KEY+27,X'FF'-X'80'  UNDELETE THE KEY                             
         GOTO1 WRITE                                                            
         B     DELO280                                                          
*                                                                               
DELO260  DS    0H                                                               
         GOTO1 ADDREC                                                           
DELO280  DS    0H                                                               
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
         NI    DMINBTS,X'FF'-X'08'                                              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(RDARKSEQ-RDARKEY),KEY                                    
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
         GOTOR PREPCMT              PREPARE AIO3 FOR WRITING COMT ELT           
*                                                                               
         MVC   KEY,SVBUYKEY                                                     
         GOTO1 HIGH                 RE-READ THE HEADER REC                      
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC               AIO->AIO2->BUY HEADER                       
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
         GOTOR COMPDAY                                                          
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
         XC    MYWORK2,MYWORK2                                                  
         OC    MYWORK2,SPACES                                                   
         CLC   CURR.RDARBDM1,PREV.RDARBDM1                                      
         BE    *+10                                                             
         MVC   MYWORK2(L'PREV.RDARBDM1),PREV.RDARBDM1                           
*                                                                               
         CLC   OLDEM,NWDEM                                                      
         BE    *+10                                                             
         MVC   MYWORK2+L'PREV.RDARBDM1(L'OLDEM),OLDEM                           
*                                                                               
         CLC   MYWORK2(L'PREV.RDARBDM1+L'OLDEM),SPACES                          
         BE    COMP0600             ALL SPACES? YES, NO CHANGE                  
*                                                                               
         GOTO1 =A(ADDCHGEL),DMCB,(=AL1(RDARDEMQ),MYWORK2),             >        
               L'RDARBDM1+L'OLDEM,RR=RELO3                                      
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
         GOTO1 =A(ADDCHGEL),DMCB,(=AL1(RDARSPTQ),PRVSPTW),L'PRVSPTW,   >        
               RR=RELO3                                                         
*                                                                               
COMP0630 DS    0H                                                               
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
         MVC   FLGSTDAT,SVSTDAT    MOVE ORIGINAL START DATE BACK                
*                                  AS IT MIGHT BE CHANGED ALONG THE             
*                                  PROCESS LEGALLY                              
         NI    BITFLAG3,X'FF'-B3DAILYB                                          
         XC    MISCFLG2,MISCFLG2                                                
K        USING RDARREC,KEY          GET NEXT BUY LINE HEADER                    
         XC    KEY,KEY                                                          
         MVC   KEY,SVBUYKEY                                                     
         MVI   KEY+26,X'FF'                                                     
         DROP  K                                                                
         NI    DMINBTS,X'FF'-X'08'   DO NOT READ DELETED REC                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(25),KEY                                                  
         BE    COMP0050                                                         
*                                                                               
COMP0800 DS    0H                   RESTORE SEQUENCE                            
*                                                                               
         GOTOR UPDTFLG              UPDATE FLAG IN AGY HEADER REC               
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
         BZ    COMD0188                                                         
         CLC   STARTDAY,ENDDAY      OUT OF WEEK ROTATOR                         
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
         GOTOR MODKEY                                                           
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
         XC    MYWORK2,MYWORK2                                                  
         L     R4,FULL                                                          
*                                                                               
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
         BE    COMDY050            THERE IS ORBIT                               
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
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   BLDODNO              NO ORBIT RECORD                             
*                                                                               
         LA    RE,IMIO                                                          
         ST    RE,AIO                                                           
         GOTO1 GETREC                                                           
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
         LA    R8,L'ORBGRID(R4)    BOUNDRY                                      
BLDO0050 DS    0H                                                               
         MVC   0(DAYTIMQ,R4),RDAROERO                                           
         LA    R4,DAYTIMQ(R4)                                                   
         CR    R4,R8                                                            
         BL    *+6                 OUT OF BOUND                                 
         DC    H'0'                                                             
*                                                                               
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
         MVC   AIO,IMSVIO                                                       
         MVC   KEY,IMSVKEY                                                      
BLDORIDX DS    0H                                                               
         B     EXIT3                                                            
         DROP  R3                                                               
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
CHGD     USING RDARCHSD,R2                                                      
*                                                                               
K        USING RDARKEY,KEY          READ BUY DETAIL SHADOW RECORD               
         L     R3,AIO2                                                          
         MVC   KEY,0(R3)                                                        
         GOTOR MODKEY                                                           
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
         DROP  R6                                                               
         DROP  CHGD                                                             
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
         GOTOR MODKEY                                                           
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
         GOTOR CPCPDAT,DMCB,FLGSTDAT,RDARBUSD                                   
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
         GOTOR GETDMCAT,DMCB,NWDEM  GET CURRENT DEMO CATEGORY                   
         DROP  R6                                                               
*                                                                               
         GOTOR MODKEY                                                           
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
         GOTOR CPCPDAT,DMCB,FLGSTDAT,RDARESST                                   
         BL    *+10                                                             
         MVC   FLGSTDAT,RDARESST    USE THE EARLIER DATE                        
         MVC   SVSTDAT,FLGSTDAT     SAVE OFF EST START DATE                     
         GOTOR GETDMCAT,DMCB,OLDEM  SAVE OFF PREVIOUS DEMO CAT                  
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
***********************************************************************         
*    GETDMCAT SAVES FIRST CATEGORY FROM THE ORDER HEADER FOR COMPARISON         
*    P1 -> A(DEMO SAVE AREA)                                                    
***********************************************************************         
GETDMCAT NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         XC    0(L'RDARDEM1,R2),0(R2)                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETDMCAX                                                         
         USING RDARDMEL,R6                                                      
         MVC   0(L'RDARDEM1,R2),RDARDEM1                                        
         DROP  R6                                                               
GETDMCAX DS    0H                                                               
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
         GOTO1 GETREC               ADDED                                       
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
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
* CALL PAR SECURITY.  ERROR:  RETURN CC NOT ZERO .                              
***********************************************************************         
PARSECY  NTR1  LABEL=*,BASE=*                                                   
         MVC   SBGROUP(LSBFILTS),SPACES                                         
*                                  CLEAR THE BLOCK                              
         CLI   DRFGRPH+5,0         GROUP FILTER ENTERED?                        
         BE    PSEC0020            NO                                           
         ZIC   RF,DRFGRPH+5        EXTRACT FIELD LENGTH                         
         BCTR  RF,0                MINUS 1 FOR MOVE                             
         EX    RF,PSEC0800                                                      
PSEC0020 EQU   *                                                                
         CLI   DRFOFFH+5,0         OFFICE FILTER ENTERED?                       
         BE    PSEC0040            NO                                           
         ZIC   RF,DRFOFFH+5        EXTRACT FIELD LENGTH                         
         BCTR  RF,0                MINUS 1 FOR MOVE                             
         EX    RF,PSEC0801                                                      
PSEC0040 EQU   *                                                                
         CLI   DRFSTATH+5,0        FILTER ON STATION?                           
         BE    PSEC0080            NO                                           
*                                                                               
*   THERE IS NO MARKET FILTER FOR DARE INBOX                                    
*                                                                               
***      CLC   =C'M=',DRFSTAT      YES - MARKET FILTER?                         
***      BE    PSEC0060            YES - PROCESS THAT                           
*                                  NO  - PROCESS STATION                        
         MVC   SBSTATN(5),STAFILT  INSERT STATION CALL LETTERS                  
         B     PSEC0080                                                         
PSEC0060 EQU   *                                                                
***      ZIC   RF,DRFSTATH+5       EXTRACT FIELD LENGTH                         
***      SH    RF,=H'3'            MINUS 3 FOR MOVE, "M="                       
***      EX    RF,PSEC0802                                                      
PSEC0080 EQU   *                                                                
         CLI   DRFSALPH+5,0        S/P   FILTER ENTERED?                        
         BE    PSEC0100            NO                                           
         ZIC   RF,DRFSALPH+5       EXTRACT FIELD LENGTH                         
         BCTR  RF,0                MINUS 1 FOR MOVE                             
         EX    RF,PSEC0803                                                      
         B     PSEC0120            SKIP TEAM WHEN S/P PRESENT                   
PSEC0100 EQU   *                                                                
         CLI   DRFTEAMH+5,0        TEAM FILTER ENTERED?                         
         BE    PSEC0120            NO                                           
         ZIC   RF,DRFTEAMH+5       EXTRACT FIELD LENGTH                         
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
         MVC   SBGROUP(0),DRFGRP   INSERT GROUP BY LENGTH                       
PSEC0801 EQU   *                                                                
         MVC   SBOFFICE(0),DRFOFF  INSERT OFFICE BY LENGTH                      
PSEC0802 EQU   *                                                                
         MVC   SBMARKET(0),DRFSTAT+2     INSERT MARKET BY LENGTH                
PSEC0803 EQU   *                                                                
         MVC   SBSALES(0),DRFSALP  INSERT S/P    BY LENGTH                      
PSEC0804 EQU   *                                                                
         MVC   SBSALES+1(0),DRFTEAM      INSERT TEAM   BY LENGTH                
PSEC0960 EQU   *                                                                
         SR    R0,R0               EXIT CC ZERO                                 
PSEC0980 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF ORDER SHOULD REALLY BE PROCESSED WITH NEW DIFFERENCES                
***********************************************************************         
CHECKDIF NTR1  BASE=*,WORK=(R4,IMWORKQ),LABEL=*                                 
         USING IMWORKD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
         MVI   KEY+RDARKRT-RDARKEY,X'70' FIND AUDIT TRAIL REC                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0,0               
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CDIFX                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IMIO,DMWORK,0         
*                                                                               
         LA    R6,IMIO                                                          
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CDIFX                                                            
*                                                                               
CDIF10   DS    0H                                                               
         LA    R2,DIFOFF                                                        
*                                                                               
         USING RDARHSEM,R6                                                      
         CLI   RDARHSAC,C'O'       CHECK IF FIRST REVISION PERFORM B4           
         BNE   CDIF20              SWITCH OVER DATE                             
         CLI   RDARHSVR,1                                                       
         BE    CDIF30                                                           
*                                                                               
CDIF20   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    CDIF10                                                           
         B     CDIFX                                                            
*                                                                               
CDIF30   DS    0H                                                               
         CLC   AGENCY,0(R2)                                                     
         BNE   CDIF40                                                           
         CLC   TWAACCS+2(2),2(R2)                                               
         BNE   CDIF40                                                           
         CLC   RDARHSDT,4(R2)      USE OLD DIFFERENCES INSTEAD                  
         BNL   CDIFX                                                            
         NI    MISCFLG1,X'FF'-MF1NREV                                           
*                                                                               
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         OI    DMISFLGX,X'04'      USE OLD SPOT BASE DIFF SCRN                  
         B     CDIFX                                                            
         DROP  R2                                                               
*                                                                               
CDIF40   DS    0H                                                               
         AHI   R2,L'DIFOFF                                                      
         CLI   0(R2),X'FF'                                                      
         BNE   CDIF30                                                           
*                                                                               
CDIFX    DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* CHECK IF ORDER SHOULD REALLY BE PROCESSED WITH NEW DIFFERENCES                
***********************************************************************         
NEWDIF   NTR1  BASE=*,WORK=(R4,IMWORKQ),LABEL=*                                 
         USING IMWORKD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),SELECTKY                                    
         MVI   KEY+RDARKRT-RDARKEY,X'70' FIND AUDIT TRAIL REC                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0,0               
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NDIFX                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IMIO,DMWORK,0         
*                                                                               
         LA    R6,IMIO                                                          
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL                                                         
         BNE   NDIFX                                                            
*                                                                               
NDIF10   DS    0H                                                               
         LA    R2,DIFOFF                                                        
*                                                                               
         USING RDARHSEM,R6                                                      
         CLI   RDARHSAC,C'O'       CHECK IF FIRST REVISION PERFORM B4           
         BNE   NDIF20              SWITCH OVER DATE                             
         CLI   RDARHSVR,1                                                       
         BE    NDIF30                                                           
*                                                                               
NDIF20   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    NDIF10                                                           
         B     NDIFX                                                            
*                                                                               
NDIF30   DS    0H                                                               
         CLC   AGENCY,0(R2)                                                     
         BNE   NDIF40                                                           
         CLC   TWAACCS+2(2),2(R2)                                               
         BNE   NDIF40                                                           
         CLC   RDARHSDT,4(R2)      USE OLD DIFFERENCES INSTEAD                  
         BL    NDIFOLD                                                          
*                                                                               
NDIFNEW  DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SR    RC,RC                                                            
         B     NDIFX                                                            
*                                                                               
NDIFOLD  DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         LTR   RC,RC                                                            
         B     NDIFX                                                            
*                                                                               
NDIF40   DS    0H                                                               
         AHI   R2,L'DIFOFF                                                      
         CLI   0(R2),X'FF'                                                      
         BNE   NDIF30                                                           
         B     NDIFOLD                                                          
*                                                                               
NDIFX    DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE REDIFOFF                                                       
         EJECT                                                                  
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
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF0D                                                       
         ORG   DRFTAGH                                                          
       ++INCLUDE REDARFDD                                                       
         ORG   DRFTAGH                                                          
       ++INCLUDE REDARFED                                                       
         ORG   DRFTAGH                                                          
       ++INCLUDE REDARFBD                                                       
         ORG   DRFTAGH                                                          
       ++INCLUDE REDARF9D                                                       
       ++INCLUDE REDARTW3                                                       
       ++INCLUDE REDARWORKD                                                     
RECS     DSECT                                                                  
       ++INCLUDE REDARDSECT                                                     
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENOFF2                                                      
*                                                                               
       ++INCLUDE DDPERVALD                                                      
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
MF1NREV  EQU   X'80'                - USE NEW REVISION                          
*                                                                               
MISCFLG2 DS    X                   MISC. FLAGS 2                                
MF2CMTP  EQU   X'01'                - COMMENT PRESENT                           
MF2ADDE  EQU   X'02'                - AT LEAST ONE ELT IS ADDED                 
MF2ORBT0 EQU   X'04'                - ORBIT FOUND IN CURRENT ORDER              
MF2ORBIT EQU   X'08'                - ORBIT FOUND IN PREVIUOS ORDER             
MF2SVCST EQU   X'10'                - SAVE COST AS WELL                         
MF2SFTDL EQU   X'20'                - CURRENT ORDER WAS CANCELLED               
MF2EMPTY EQU   X'40'                - EMPTY SPOT, DON'T SAVE TO LASTDIF         
MF2CHSTD EQU   X'80'                - CHANGE START DATE                         
*                                                                               
MISCFLG4 DS    X                   MISC. FLAGS 4                                
MF4PRDIF EQU   X'80'               PRINT DIFF                                   
*                                                                               
BUYFLAG1 DS    XL1                                                              
BYFWKLY  EQU   0                                                                
BYFDAILY EQU   1                                                                
BYFCRDLY EQU   2                                                                
BYFPVDLY EQU   3                                                                
BYFTABOV EQU   4                                                                
*                                                                               
BYTE2    DS    X                                                                
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
ORBGRID  DS    XL500                                                            
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
DEMOFLG  DS    X                   X'80' = DEMO1 IS CHANGED                     
*                                  X'40' = DEMO2 IS CHANGED                     
*                                  X'20' = DEMO3 IS CHANGED                     
*                                  X'10' = DEMO4 IS CHANGED                     
*                                                                               
NWDEM    DS    CL4                 SAVED CURRENT DEMO CAT                       
OLDEM    DS    CL4                 SAVED PREVIOUS DEMO CAT                      
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
*                                                                               
IMWORKD  DSECT                                                                  
IMSVIO   DS    A                                                                
IMSVKEY  DS    XL27                                                             
IMSVKEY2 DS    XL27                                                             
IMIO     DS    XL3800                                                           
IMWORKQ  EQU   *-IMWORKD                                                        
*                                                                               
*                                                                               
* ONLINE LIST LINE (TWO PHYSICAL LINES)                                         
*                                                                               
LISTD    DSECT                                                                  
LSTSTATH DS    CL8                                                              
LSTCOLOR DS    CL1                                                              
LSTPRNTD DS    CL1                                                              
LSTSTAT  DS    CL6                                                              
LSTSTAT2 DS    CL2                                                              
         DS    CL8                                                              
LSTCON#  DS    CL8                                                              
         DS    CL8                                                              
LSTTFLAG DS    CL1                                                              
LSTAGY#  DS    CL8                                                              
         DS    CL8                                                              
LSTSTA   DS    CL6                                                              
         DS    CL8                                                              
LSTAGY   DS    CL6                                                              
         DS    CL8                                                              
LSTOFF   DS    CL2                                                              
         DS    CL8                                                              
LSTSALP  DS    CL3                                                              
         DS    CL8                                                              
LSTADV   DS    CL20                                                             
         DS    CL8                                                              
         DS    CL1                                                              
         DS    CL8                                                              
LSTPRD1  DS    CL12                                                             
         DS    CL8                                                              
LSTPRD2  DS    CL12                                                             
         DS    CL8                                                              
LSTFLT   DS    CL17                                                             
*                                                                               
         ORG   LSTFLT                                                           
LSTSTDT  DS    CL8                                                              
LSTDASH  DS    C                                                                
LSTENDT  DS    CL8                                                              
*                                                                               
         DS    CL8                                                              
LSTEST#  DS    CL4                                                              
         DS    CL8                                                              
LSTTOTL  DS    CL14                                                             
         DS    CL8                                                              
LSTTSPT  DS    CL5                                                              
LISTLEN  EQU   *-LSTSTATH                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'202REDAR01   08/02/06'                                      
         END                                                                    
