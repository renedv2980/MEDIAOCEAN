*          DATA SET RECNT2B    AT LEVEL 144 AS OF 01/17/13                      
*PHASE T8022BC,+0                                                               
*INCLUDE RETIMVAL                                                               
*INCLUDE DAYVAL                                                                 
*INCLUDE OUTDAY                                                                 
         TITLE 'T8022B - REPPAK MAKEGOOD CONTROL'                               
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT2B (T8022B) --- MAKEGOOD OFFER CONTROL                 *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 17JAN13 SKU ADD EYECATCHER '*' FOR STATION CREATED OFFERS       *             
* 04JUN03 SKU DISPLAY PROGRAM NAME AND COMMENT                    *             
* 15MAY03 SKU SELF APPLY                                          *             
* 04FEB03 SKU FIX STATION CREATED OFFER DISPLAY                   *             
* 08NOV02 HQ  FIX STATION MGL SECOND SCREEN BUG                   *             
* 01FEB02 SKU REPLACEMENT OFFER FOR NA SUPPORT                    *             
* 11OCT00 SKU TURN ON FAX CAPABILITY                              *             
* 11SEP00 SKU MAKEGOOD FORM GENERATION                            *             
* 02AUG00 SKU FIX STEREO/GLOBBER FIELD PROBLEM LOCALLY            *             
* 21JUN00 SKU MAKEGOOD LIST BUG FIX                               *             
* 22DEC99 SKU NEW MAKEGOOD SUPPORT                                *             
* 05NOV99 SKU BYPASS BAD TAKEOVER MAKEGOOD OFFERS                 *             
* 03AUG99 SKU FIX SCROLLER CHOPPING OFF DISPLAY                   *             
* 13OCT98 SKU MAKEGOOD OFFER TOTAL SCREEN                         *             
* 26AUG98 SKU ADD C'-' TO SHOW DATE RANGE IN LIST                 *             
* 29MAY97 SKU BONUS SUPPORT                                       *             
* 07APR97 SKU SPECIAL ROUTINE FOR FOX/PETRY MAKEGOOD FIX          *             
* 09JAN97 SKU MAKEGOOD FOR MAKEGOOD                               *             
* 16OCT96 SKU SUPPORT BONUSES AND PREEMPTS                        *             
* 07OCT96 SKU SUPPORT LOW POWER STATION                           *             
* 24MAY96 SKU CHECK IF USER SELECTED NOTHING                      *             
* 02MAY96 SKU HIDE OFFER FROM OPPOSITE PARTY IF NOT YET SENT      *             
* 25JAN96 SKU DARE AUTO-APPLY                                     *             
* 22MAR95 SKU ADD (A)PPLY AND (B)ACKOUT SELECT CODES              *             
* 06MAR95 BU  NEW GROUP CODE FORMAT, REPLACING DATE/TIME          *             
* 14FEB95 BU  FULL-SCREEN VERSION                                 *             
* 08FEB95 BU  CHANGE TO 'ADD TO GROUP' MECHANISM                  *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8022B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8022B,R9                                                      
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
*                                                                               
         GOTO1 =A(FOXFIX),DMCB,(RC),(RA),RR=Y                                   
*                                                                               
MAINLINE EQU   *                                                                
         XC    MGCLSBY,MGCLSBY                                                  
         OI    MGCLSBYH+6,X'80'    XMIT                                         
         LA    R6,RCONREC          DISPLAY IF LAST SENT BY REP/STA/NONE         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   MAIN0005                                                         
         USING RCONMGEL,R6                                                      
         TM    RCONMGFG,X'40'                                                   
         BZ    MAIN0003                                                         
         MVC   MGCLSBY,=C'Rep'                                                  
         B     MAIN0005                                                         
*                                                                               
MAIN0003 DS    0H                                                               
         TM    RCONMGFG,X'20'                                                   
         BZ    MAIN0005                                                         
         MVC   MGCLSBY,=C'Sta'                                                  
         DROP  R6                                                               
*                                                                               
MAIN0005 DS    0H                                                               
         LR    R6,RA               NO  - INDICATE BUY SELECTED                  
         AH    R6,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R6                                                       
         XC    TWAMKGDS,TWAMKGDS   CLEAR ADDR OF PRIMARY RECORD                 
         XC    TWAMKGD2,TWAMKGD2   CLEAR ADDR OF SELECTION                      
         MVI   TWAMKGL#,0          CLEAR LINE NUMBER                            
         NI    TWAMKGFG,X'10'      CLEAR ALL BUT LAST COMMENT FLAG              
*                                  TURN OFF M/G FLAGS                           
         LA    RF,CONCACTH         ANY BUY ACTION CHANGE?                       
         CLI   5(RF),0             ANY INPUT?                                   
         BNE   *+6                 SHOULD HAVE INPUT                            
         DC    H'0'                HOW WOULD WE GET HERE?                       
         TM    4(RF),X'20'         ACTION PREVIOUSLY VALIDATED?                 
         BZ    MAIN0060            NO  - FIRST REQUEST                          
*                                                                               
MAIN0010 DS    0H                  CHECK FOR NEW BUY OFFER FIRST                
         LA    R3,BUYERR           SET ERROR MESSAGE                            
         LA    R2,MGCBNUMH         BUY # FOR NEW OFFER?                         
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    MAIN0020            NO  - CHECK FOR SELECTION                    
*                                                                               
         BAS   RE,CHKDARE          CHECK FOR DARE EXCEPTIONS                    
*                                                                               
         XC    TWAMKGD2,TWAMKGD2                                                
*                                                                               
         CLI   8(R2),C'B'          BONUS??                                      
         BNE   MAIN0015                                                         
         OI    TWAMKGFG,X'08'      FLAG AS BONUS                                
         LA    R2,MGCBNUMH                                                      
         XC    DUB,DUB                                                          
         B     MAIN0900                                                         
*                                                                               
MAIN0015 DS    0H                  CHECK FOR NEW BUY OFFER FIRST                
         GOTO1 VPACK               PACK/VALIDATE BUY NUMBER                     
         LTR   R0,R0               ERROR?                                       
         BZ    ERROR               YES                                          
         ST    R0,TWAMKGDS         LOAD LINE NUMBER                             
         GOTO1 GETBUYDA            RETRIEVE BUY DISK ADDRESS                    
         LA    R2,MGCBNUMH                                                      
         LA    R3,BUYERR                                                        
         BNZ   ERROR               ERROR - NO BUY RECORD WITH THAT #            
*                                                                               
         B     MAIN0900            NEW OFFER TO BE ENTERED - EXIT               
*                                                                               
MAIN0020 EQU   *                                                                
         GOTO1 CHKSELEX            CHECK FOR SELECTION                          
         BNZ   MAIN0900            SELECTION MADE - EXIT THIS MOD               
*                                     XFER TO OFFER MOD FROM RECNT01            
         OC    DUB,DUB             ERROR PASSED BACK?                           
         BZ    MAIN0040            NO                                           
         CLI   DUB+4,X'FF'         EXIT FOR ACTIONS: APPLY, REJECT,             
         BNE   MAIN0030            CANCEL OR BACKOUT                            
         XC    TWAMKGDS,TWAMKGDS   SET NO DISPLAY                               
         B     EXXMOD                                                           
                                                                                
MAIN0030 DS    0H                                                               
         L     R3,DUB              YES - LOAD ERROR RETURN                      
         L     R2,DUB+4            LOAD CURSOR ADDRESS                          
         B     ERROR               EXIT WITH ERROR MESSAGE                      
MAIN0040 EQU   *                   NO SELECTION: FILL CONTROL SCREEN            
         GOTO1 STARTNXT            USE LAST D/A TO GET NEXT RECORD              
         BNZ   MAIN0080            LOAD TO SCREEN                               
*                                                                               
MAIN0060 EQU   *                                                                
         TWAXC MGCMSELH,PROT=Y     CLEAR LIST PORTION OF SCREEN                 
*                                                                               
         GOTO1 =A(DISPCON),DMCB,(RC),(RA),RR=Y                                  
*                                  DISPLAY CONTRACT HEADER INFO                 
         GOTO1 FIRSTMG             SET UP FIRST M/G KEY FOR CON#                
         BZ    MAIN0080            NOTHING FOUND - EXIT                         
         TM    TWADARE,X'02'       DARE AUTO APPLY?                             
         BZ    MAIN0900                                                         
         LA    R3,565              ERROR!                                       
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
*    STILL NEED TO SET PREVALIDS                                                
*                                                                               
MAIN0080 EQU   *                                                                
*                                  RETRIEVE MAKEGOOD RECORD                     
         GOTO1 LOADMGS             LOAD MAKEGOODS TO SCREEN                     
         XC    TWAMKGDS,TWAMKGDS   SET NO DISPLAY                               
*                                                                               
MAIN0900 EQU   *                                                                
         LA    R2,CONCACTH         SET BUY ACTION PREVALID                      
         OI    4(R2),X'20'                                                      
         LA    R2,MGCBNUMH         SET CURSOR TO BUY NUM FLD                    
         OI    6(R2),X'40'         SET CURSOR RETURN ADDRESS                    
         OI    1(R2),X'01'         TURN ON MODIFIED FOR AUTO PAGING             
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         TM    TWADARE,X'02'       IF DARE AUTO-APPLY                           
         BZ    MAINX               BRANCH BACK TO EXECUTE APPLY                 
         TM    TWADARE,X'04'       UNLESS WE DID IT ALREADY                     
         BO    MAINX               ALL DONE?                                    
         MVI   MGCMSEL,C'A'        FORCE APPLY                                  
         MVI   MGCMSELH+5,1                                                     
         B     MAINLINE                                                         
*                                                                               
MAINX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DARE REVISION ORDER MUST BE APPROVED/REJECTED/CANCELLED IN ORDER TO           
* ADD ANY MAKEGOOD OFFERS                                                       
***********************************************************************         
CHKDARE  NTR1                      MUST BE CONFIRMED AT LEAST ONCE              
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        DARE ORDER??                                 
         BAS   RE,GETEL                                                         
         BNE   CKMGX                                                            
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q   EXTENDED ELEMENT??                           
         BL    CKMGX               ONLY FOR REVISION                            
         OC    RCONDRRV,RCONDRRV   CONFIRMED/CANCELLED/NOT REVISION?            
         BZ    CKMGX                                                            
         LA    R3,734              MUST PROCESS DARE REVISION ORDER             
         OC    RCONDRDD(4),RCONDRDD                                             
         BZ    ERROR               MUST HAVE DELIVERY DATE                      
         TM    RCONDRFG,X'40'+X'20'                                             
         BZ    ERROR               MUST BE APPROVED OR REJECTED                 
         CLC   RCONDRDA(4),RCONDRDD                                             
         BH    CKMGX                                                            
         CLC   RCONDRDR(4),RCONDRDD                                             
         BL    ERROR                                                            
*                                                                               
CKMGX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*  FIRSTMG:   KEY FOR THE MAKEGOODS PERTAINING TO THIS CONTRACT IS              
*      BUILT.                                                                   
*                                                                               
FIRSTMG  NTR1                                                                   
         LR    R6,RA                                                            
         AHI   R6,TWAWORKQ                                                      
         USING TWAWORK,R6                                                       
*                                                                               
         XC    TWAMKGDA(TWAMKGLA-TWAMKGDA),TWAMKGDA                             
*                                  CLEAR THE TABLE                              
*                                                                               
         LA    R2,CONCNUMH         CONVERT HEADLINE #                           
*                                                                               
*                                  CONTRACT NUMBER HAS BEEN VALIDATED           
*                                     BY HEADLINE VALIDATION MODULE             
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),0                                     
         LA    R4,WORK             POINT TO SCANNER BLOCK                       
         ICM   R0,15,4(R4)         LOAD BINARY VALUE OF CON#                    
         CVD   R0,DUB              CONVERT IT BACK TO DECIMAL                   
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
*                                  CALCULATE 9'S COMP                           
         MVO   WORK(5),WORK+10(5)                                               
         XC    RMKGREC(32),RMKGREC                                              
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,TWAAGY     INSERT POWER CODE                            
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE CODE                           
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION CALL LETTERS                  
*                                  REVERSE THE CONTRACT NUMBER                  
         PACK  RMKGKCON+0(1),WORK+3(1)                                          
         PACK  RMKGKCON+1(1),WORK+2(1)                                          
         PACK  RMKGKCON+2(1),WORK+1(1)                                          
         PACK  RMKGKCON+3(1),WORK+0(1)                                          
*                                                                               
         CLC   =C'MGL,',CONCACT    USER SPECIFIED GROUP?                        
         BE    FIMG0010                                                         
         OC    TWAMKGDT,TWAMKGDT   LAST GROUP DISPLAYED?                        
         BZ    FIMG0020                                                         
         MVC   RMKGKGRP,TWAMKGDT                                                
         XC    TWAMKGDT,TWAMKGDT                                                
         B     FIMG0015                                                         
         DROP R6                                                                
*                                                                               
FIMG0010 DS    0H                                                               
         MVC   RMKGKGRP,CONCACT+4  YES! START LIST AT SPECIFIED GROUP           
FIMG0015 XC    CONCACT,CONCACT     REMOVE SPECIFIED GROUP FROM FIELD            
         MVC   CONCACT(3),=C'MGL'                                               
         MVI   CONCACTH+5,3                                                     
*                                                                               
         MVC   KEY,RMKGREC         LOAD KEY FROM RECORD                         
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
*                                  SAME KEY - THROUGH CON#                      
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    FIMG0040            RECORD FOUND                                 
         B     FIMG0025                                                         
*                                                                               
FIMG0020 DS    0H                                                               
         MVC   KEY,RMKGREC         LOAD KEY FROM RECORD                         
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
*                                  SAME KEY - THROUGH CON#                      
FIMG0025 CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   FIMG0030            RECORD NOT FOUND                             
         OC    KEY+21(6),KEY+21                                                 
         BZ    FIMG0040            MUST BE GROUP RECORD                         
*                                                                               
FIMG0028 GOTO1 VSEQ                NOT GROUP REC? MUST BE CORRUPTED             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE     SKIP                           
         BE    FIMG0028                                                         
         B     FIMG0025            RECORD FOUND                                 
*                                                                               
FIMG0030 DS    0H                                                               
         LTR   RB,RB               NO KEY FOUND - SET CC NOT ZERO               
         B     FIMG0080                                                         
FIMG0040 EQU   *                                                                
         SR    R0,R0               FOUND - SET CC = ZERO                        
FIMG0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  CHKSELEX:  CHECK TO SEE IF ANY FIELDS HAVE BEEN SELECTED, OR IF              
*      REQUEST IS FOR A 'NEW' OFFER                                             
*                                                                               
NEXTSELF EQU   MGCMSE2H-MGCMSELH                                                
NEXTSEL2 EQU   (MGCMSE2H-MGCMSELH)*2                                            
*                                  TO BUMP OVER TWO SCREEN LINES                
*                                                                               
SELINDER EQU   456                 SELECTION INDICATOR ERROR                    
*                                                                               
CHKSELEX NTR1                                                                   
         XC    DUB,DUB             CLEAR ERROR INDICATOR                        
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         XC    TWAMKGD2,TWAMKGD2   CLEAR ADDR OF SELECTION                      
         NI    TWAMKGFG,X'10'      CLEAR ALL BUT LAST COMMENT FLAG              
*                                  TURN OFF M/G FLAGS                           
CSEL0020 EQU   *                                                                
         LA    R2,MGCMSELH         A(1ST SELECTION FIELD HDR)                   
         LA    R3,MGCLAST          A(END OF SCREEN)                             
         LA    R4,TWAMKGDA         A(LIST OF D/A FROM SCREEN)                   
CSEL0040 EQU   *                                                                
         CR    R2,R3               END OF SCREEN REACHED?                       
         BNL   CSELYES             YES - EXIT SETTING CC = ZERO                 
         CLI   5(R2),0             ANY DATA IN FIELD?                           
         BNE   CSEL0060            YES                                          
CSEL0050 EQU   *                                                                
         LA    R2,NEXTSELF(R2)     NO  - BUMP TO NEXT SELECT FIELD              
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    CSEL0040                                                         
         LA    R4,4(R4)            BUMP TO NEXT D/A IN LIST                     
         B     CSEL0040                                                         
CSEL0060 EQU   *                                                                
         CLI   8(R2),C'T'          ITEM SELECTED?                               
         BNE   CSEL0065            NO  - CHECK FOR OTHER VALUE                  
         OI    TWAMKGFG,X'04'      FLAG MAKEGOOD TOTAL                          
         MVC   TWAMKGD2,0(R4)      INSERT D/A OF SELECTED ITEM                  
         B     CSELNO              EXIT THIS OVERLAY                            
CSEL0065 EQU   *                                                                
         CLI   8(R2),C'S'          ITEM SELECTED?                               
         BE    CSEL0070            NO  - CHECK FOR OTHER VALUE                  
         CLI   8(R2),C'A'          APPLY ITEM?                                  
         BE    CSEL0080            NO  - CHECK FOR OTHER VALUE                  
*        CLI   8(R2),C'B'          BACKOUT ITEM?                                
*        BE    CSEL0080            NO  - CHECK FOR OTHER VALUE                  
         CLI   8(R2),C'R'          REJECT ITEM?                                 
         BE    CSEL0080            NO  - CHECK FOR OTHER VALUE                  
*        CLI   8(R2),C'C'          CANCEL ITEM?                                 
*        BE    CSEL0080            NO  - CHECK FOR OTHER VALUE                  
         CLI   8(R2),C'P'          PRINT FORM?                                  
         BE    CSEL0080            NO  - CHECK FOR OTHER VALUE                  
         CLI   8(R2),C'F'          FAX FORM?                                    
         BE    CSEL0080            NO  - CHECK FOR OTHER VALUE                  
         B     CSEL0150                                                         
*                                                                               
* ONLY ACTION 'S' IS ALLOWED FOR OFFER RECORDS                                  
*                                                                               
CSEL0070 DS    0H                                                               
         LR    RF,R2                                                            
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         LA    R3,506              CANNOT SELECT GROUP RECORD                   
         TM    1(RF),X'08'         OFFER RECORDS ARE NOT HI-LITED               
         BO    ERROR                                                            
         B     CSEL0100                                                         
*                                                                               
* ALL ACTIONS EXCEPT FOR 'S' ARE ALLOWED FOR GROUP HEADER RECORDS               
*                                                                               
CSEL0080 DS    0H                                                               
         LR    RF,R2                                                            
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         TM    1(RF),X'08'         GROUP HEADER RECORDS ARE HI-LITED            
         BO    CSEL0100                                                         
         LA    R3,507              CAN ONLY SELECT AN OFFER REC                 
         B     ERROR                                                            
*                                                                               
CSEL0100 EQU   *                                                                
         MVC   TWAMKGD2,0(R4)      YES - INSERT D/A OF SELECTED ITEM            
*                                                                               
         LA    R3,576                                                           
         OC    TWAMKGD2,TWAMKGD2   CHECK IF NO D/A PRESENT                      
         BZ    ERROR                                                            
*                                                                               
         OI    TWAMKGFG,X'80'      INSERT 'SELECT' FLAG                         
         LA    R3,132                                                           
         CLI   8(R2),C'A'          APPLY ITEM?                                  
         BE    CSEL0110            YES - GO AND APPLY MAKEGOOD                  
         LA    R3,133                                                           
         CLI   8(R2),C'R'          REJECT/RECALL ITEM?                          
         BE    CSEL0110            YES - GO AND REJECT/RECALL MAKEGOOD          
*        LA    R3,134                                                           
*        CLI   8(R2),C'C'          RECALL ITEM?                                 
*        BE    CSEL0110            YES - GO AND CANCEL MAKEGOOD                 
         LA    R3,135                                                           
         CLI   8(R2),C'B'          BACKOUT ITEM?                                
         BE    CSEL0140            YES - GO AND BACKOUT MAKEGOOD                
         CLI   8(R2),C'P'          PRINT FORM?                                  
         BE    CSEL0135            YES - GO AND PRINT                           
         CLI   8(R2),C'F'          FAX FORM?                                    
         BE    CSEL0135            YES - GO AND FAX                             
         B     CSEL0190                                                         
*                                                                               
CSEL0110 DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'37',0),(R2)                                        
         STH   R3,DUB                                                           
         ST    R2,DUB+4                                                         
*                                                                               
* REFRESH STATUS                                                                
*                                                                               
         LR    RF,R2                                                            
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         CLI   8(R2),C'A'                                                       
         BNE   CSEL0120            APPLIED                                      
         MVC   8+MGSTAT-MGCNTRL(3,RF),=C'APL'                                   
         OI    6(RF),X'80'         XMIT STATUS                                  
         TM    TWADARE,X'02'       DARE AUTO APPLY?                             
         BZ    CSEL0130                                                         
         OI    TWADARE,X'04'       SET RETURN FLAG FOR AUTO-APPLY               
         B     CSEL0130                                                         
*                                                                               
CSEL0120 DS    0H                  GET REFRESH MESSAGE FOR RECALL               
         CLC   =C'REC',8+MGSTAT-MGCNTRL(RF)                                     
         BNE   CSEL0130                                                         
         MVC   DUB(2),=H'134'                                                   
*                                                                               
CSEL0130 DS    0H                                                               
         XC    8(L'MGCMSEL,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'+X'40'   TRANSMIT AND FORCE CURSOR HERE               
         MVI   DUB+4,X'FF'         FLAG TO EXIT                                 
         B     CSELYES                                                          
*                                                                               
CSEL0135 EQU   *                                                                
         GOTO1 VLOAD,DMCB,(X'73',0),(R2)  MAKEGOOD FORM GEN MODULE              
         XC    DUB(2),DUB          WE'LL HANDLE THE MESSAGE                     
         MVI   DUB,X'FF'           FLAG THIS SPECIAL CASE                       
         ST    R2,DUB+4                                                         
         XC    8(L'MGCMSEL,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'+X'40'                                                
         MVI   DUB+4,X'FF'         FLAG TO EXIT                                 
*                                                                               
         LA    R2,MGCBNUMH         SET CURSOR TO BUY NUM FLD                    
         OI    1(R2),X'01'         TURN ON MODIFIED FOR AUTO PAGING             
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         B     CSELYES             EXIT                                         
*                                                                               
CSEL0140 EQU   *                                                                
         GOTO1 VLOAD,DMCB,(X'35',0),(R2)                                        
         STH   R3,DUB                                                           
         ST    R2,DUB+4                                                         
         XC    8(L'MGCMSEL,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         MVI   DUB+4,X'FF'         FLAG TO EXIT                                 
         B     CSELYES             EXIT                                         
CSEL0150 EQU   *                   FIELD MAY CONTAIN A BUY NUMBER               
         CLC   8(2,R2),MYSPACES    BOGUS INPUT BY STEREO/GLOBBER?               
         BNE   CSEL0153                                                         
         MVI   5(R2),0                                                          
         XC    8(2,R2),8(R2)                                                    
         B     CSEL0050                                                         
*                                                                               
CSEL0153 DS    0H                                                               
         LA    R3,SELINDER                                                      
         B     ERROR                                                            
*&&DO                                                                           
         GOTO1 VPACK               PACK/VALIDATE BUY NUMBER                     
         LTR   R0,R0               ERROR?                                       
         BZ    ERROR               YES                                          
         ST    R0,TWAMKGDS         LOAD LINE NUMBER                             
*                                                                               
         LR    RF,R2               CHECK IF MAKEGOOD APPLIED                    
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         LA    R3,504              INVALID ACTION, MAKEGOOD APPLIED             
         CLC   =C'APL',8+MGSTAT-MGCNTRL(RF)                                     
         BE    ERROR                                                            
*                                                                               
         GOTO1 GETBUYDA            RETRIEVE BUY DISK ADDRESS                    
         BZ    CSEL0160            RECORD FOUND FOR BUY NUMBER                  
         LA    R3,BUYERR           NO RECORD FOUND:  SEND ERROR                 
         B     ERROR                                                            
CSEL0160 EQU   *                                                                
         OC    0(4,R4),0(R4)       ANY D/A IN LIST FOR ENTRY?                   
         BZ    CSEL0050            NO  - BUMP TO NEXT SELECT FIELD              
*                                  THIS CAN NO LONGER HAPPEN:                   
*                                     EMPTY ACT FIELDS ARE PROTECTED            
         MVC   TWAMKGD2,0(R4)      YES - INSERT D/A OF SELECTED ITEM            
         OI    TWAMKGFG,X'40'      INSERT 'ADD' FLAG                            
*&&                                                                             
*                                                                               
CSEL0190 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED RECORD                   
         GOTO1 VGETREC,DMCB,IOAREA                                              
*        LA    R2,IOAREA           SET A(IOAREA)                                
         TM    TWAMKGFG,X'80'      'SELECT' FLAG?                               
         BO    CSEL0200            YES - FORCE BL OF M/G SELECTED               
*                                  NO  - CHECK FOR ALTERNATE BUYLINE            
         OC    TWAMKGDS,TWAMKGDS   BUYLINE OTHER THAN THAT IN OFFER             
*                                     SELECTED?                                 
         BNZ   CSELNO              YES - DON'T USE SELECTION'S #                
CSEL0200 EQU   *                                                                
         LR    RF,R2               CHECK IF BONUS MAKEGOOD                      
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         CLC   =C'BONUS',8+MGMSDATE-MGCNTRL(RF)                                 
         BNE   CSEL0210                                                         
         OI    TWAMKGFG,X'08'                                                   
         B     CSELNO                                                           
*                                  NO  - USE SELECTION                          
CSEL0210 EQU   *                                                                
         LA    R6,IOAREA           SET A(IOAREA)                                
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
         MVC   TWAMKGDS+3(1),RMKGMGLI                                           
         DROP  R6                                                               
*                                  LOAD BUY # OF TARGET BUY FROM                
*                                     SELECTED MG OFFER RECORD                  
         GOTO1 GETBUYDA            RETRIEVE ORIGINAL BUY D/A WHICH              
*                                     IS PUT IN TWAMKGDS TO PASS                
         BZ    CSELNO              RECORD FOUND: OK RETURN                      
         MVC   TWAMKGDS,=F'-2'                                                  
         B     CSELNO                                                           
*                                                                               
*        LA    R3,BUYERR                                                        
*        B     ERROR                                                            
*                                                                               
CSELYES  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     CSELX                                                            
*                                     EXIT WITH ERROR                           
CSELNO   EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO RETURN                     
CSELX EQU      *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*  STARTNXT:  USER IS REQUESTING THE DISPLAY OF THE NEXT SCREEN OF              
*      CONTROL DATA.  IF THE FIELD TWAMKGLA CONTAINS A DISK ADDRESS,            
*      THIS IS THE LAST ENTRY ON THE SCREEN, IN WHICH CASE THERE MAY            
*      OR MAY NOT BE ANOTHER ENTRY.  IF THE FIELD IS X'00' FILLED,              
*      THERE ARE NO FURTHER ENTRIES.                                            
*      IF LAST LINE ON SCREEN WAS A COMMENT, TWAMKGFG X'10' WILL BE             
*      SET.  IN THIS CASE, DO NOT ADVANCE TO THE NEXT KEY AFTER                 
*      TWAMKGLA D/A IS USED.  THIS IS THE STARTING POINT.                       
*                                                                               
STARTNXT NTR1                                                                   
         LR    R6,RA                                                            
         AH    R6,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R6                                                       
         OC    TWAMKGLA,TWAMKGLA   ANYTHING IN LAST SCREEN FLD?                 
         BZ    SNXT0080            NO  - FINISHED                               
         MVC   KEY+28(4),TWAMKGLA                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE LAST RECORD                         
         MVC   KEY,RMKGREC         RESET KEY                                    
         GOTO1 VREAD               ESTABLISH READ SEQUENCING                    
         TM    TWAMKGFG,X'10'      'LAST LINE= COMMENT' SET?                    
         BO    SNXT0040            YES - SKIP READING NEXT KEY                  
         GOTO1 VSEQ                GET NEXT KEY                                 
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE     SAME CONTRACT?                 
         BNE   SNXT0060            NO  - NOTHING LEFT                           
SNXT0040 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     SNXT0080                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
SNXT0060 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
SNXT0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  LOADMGS:  BEGINNING WITH THE RECORD RETRIEVED, LOAD THE SCREEN               
*      WITH A MAXIMUM OF 'N' LINES.  (SEE THE DSECT FOR THE SCREEN,             
*      AS THIS MAY CHANGE.  IF ALL LINES ARE FILLED, THE DISK ADDRESS           
*      OF THE RECORD DISPLAYED ON THE LAST LINE IS TO BE STORED IN              
*      TWAMKGLA, ELSE CLEAR THE FIELD.                                          
*                                                                               
LOADMGS  NTR1                                                                   
*                                                                               
         BAS   RE,UNPROT           UNPROTECT TEXT FIELDS                        
         GOTO1 VFOUTBLK,DMCB,MGCMSELH,MGCLAST,0                                 
*                                  CLEAR SCREEN                                 
         BAS   RE,PROT             PROTECT TEXT FIELDS                          
         USING MGCNTRL,R2          SET DISPLAY FIRST LINE DSECT                 
         USING MGCNTRL2,R8         SET DISPLAY SECND LINE DSECT                 
         LA    R2,MGCMDES          SET A(1ST DESCRIPTION DETAIL)                
         LA    R8,NEXTSELF(R2)     SET A(NEXT LINE )                            
         LA    R7,MGCMSELH         SET A(1ST SELECT HEADER)                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         XC    TWAMKGDA(TWAMKGLA-TWAMKGDA),TWAMKGDA                             
*                                  CLEAR THE TABLE                              
         LA    R5,TWAMKGDA         SET A(LIST OF D/A)                           
         OC    TWAMKGLA,TWAMKGLA   ANY LAST ON SCREEN?                          
         BZ    LMGS0060            NO  - PROCEED                                
*                                  YES - KEY HAS ALREADY BEEN READ              
*                                     IN CALL TO 'STARTNXT'                     
         DROP  RF                                                               
*                                                                               
LMGS0005 OC    KEY+21(6),KEY+21    GROUP COMMENT RECORD?                        
         BZ    LMGS0060            YES                                          
         XC    KEY+21(6),KEY+21    NO  - RETRIEVE GROUP COMMENT RECORD          
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    LMGS0010            YES -                                        
LMGS0008 GOTO1 VSEQ                SKIP THIS GROUP                              
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    LMGS0008                                                         
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BE    LMGS0005                                                         
         B     LMGSX                                                            
*        DC    H'0'                NO  - MUST BE THERE...                       
*                                                                               
LMGS0010 DS    0H                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                                                               
* MGXSEL CANCELLED REJECTED SELF-APPLIED OFFER SHOULD NOT SHOW UP               
* IN MGL                                                                        
*                                                                               
         TM    RMKGSFG1,RMGF1MCN   EDI CANCELLED OFFER?                         
         BZ    LMGS0012                                                         
         TM    RMKGSFG3,RMGF3SAQ   WAS SELF-APPLIED?                            
         BO    LMGS0040                                                         
*                                                                               
*                                                                               
* APPLIED ORDERS SHOULD BE HIDDEN IF THEY HAVE LOWER MOD NUMBER                 
* THAN THE CONTRACTS                                                            
*                                                                               
LMGS0012 DS    0H                                                               
         TM    RMKGSCST,RMKGSAPQ   APPLIED ORDERS ONLY                          
         BZ    LMGS0015                                                         
         TM    RMKGSFG3,RMGF3SAQ   SELF-APPLY?                                  
         BZ    LMGS0013                                                         
         TM    RMKGSFG3,RMGF3ARQ   APPROVAL RECEIVED?                           
         BZ    LMGS0015            NOT YET, STILL SHOW OFFER                    
*                                                                               
LMGS0013 DS    0H                                                               
         CLC   RCONMOD,RMKGAPMN                                                 
         BH    LMGS0040                                                         
*                                                                               
* NEWLY CREATED OFFERS THAT HAVE NOT BEEN MGS'ED SHOULD NOT APPEAR ON           
* LIST OF THE OPPOSITE PARTY                                                    
*                                                                               
LMGS0015 DS    0H                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BNZ   LMGS0050            ALREADY IN DARE, SHOW IT                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0050                                                         
         USING RCONMGEL,R6                                                      
*                                                                               
         TM    RMKGSCST,RMKGSCRQ                                                
         BZ    LMGS0020            REP IS OFFERER                               
         CLI   TWAACCS,C'$'        STATION IS DOING THE LIST                    
         BNE   LMGS0050                                                         
*                                                                               
         CLC   RMKGSCRD,RCONRMDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0018            THAN LAST REP MGS DATE/TIME                  
         BL    LMGS0050            DON'T SHOW IN LIST                           
         CLC   RMKGSCRT,RCONRMTM                                                
         BH    LMGS0040                                                         
         B     LMGS0050                                                         
*                                                                               
LMGS0018 DS    0H                  STATION IS OFFERER                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0040                                                         
         USING RCONSEND,R6                                                      
*                                                                               
         CLC   RMKGSCRD,RCONSRDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0040            THAN LAST STATION SEND DATE/TIME             
         BL    LMGS0050            DON'T SHOW IN LIST                           
         GOTO1 HEXIN,DMCB,RCONSRTI,WORK,6                                       
         CLC   RMKGSCRT,WORK                                                    
         BL    LMGS0050                                                         
         DROP  R6                                                               
*                                                                               
LMGS0020 DS    0H                  STATION IS OFFERER                           
         CLI   TWAACCS,C'$'                                                     
         BE    LMGS0050                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0050                                                         
         USING RCONMGEL,R6                                                      
*                                                                               
         CLC   RMKGSCRD,RCONMGDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0030            THAN LAST STATION MGS DATE/TIME              
         BL    LMGS0050            DON'T SHOW IN LIST                           
         CLC   RMKGSCRT,RCONMGTM                                                
         BL    LMGS0050                                                         
         DROP  R6                                                               
*                                                                               
LMGS0030 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0040                                                         
         USING RCONSEND,R6                                                      
*                                                                               
         CLC   RMKGSCRD,RCONSSDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0040            THAN LAST STATION SEND DATE/TIME             
         BL    LMGS0050            DON'T SHOW IN LIST                           
         GOTO1 HEXIN,DMCB,RCONSSTI,WORK,6                                       
         CLC   RMKGSCRT,WORK                                                    
         BL    LMGS0050                                                         
         DROP  R6                                                               
*                                                                               
LMGS0040 DS    0H                  LOOK FOR NEXT GROUP                          
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE     FOUND?                         
         BE    LMGS0040                                                         
*                                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE     FOUND?                         
         BE    LMGS0045            MUST BE SAME CONTRACT                        
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         XC    TWAMKGLA,TWAMKGLA   START OVER ON THE NEXT PASS                  
         B     LMGSX                                                            
         DROP  RF                                                               
*                                                                               
LMGS0045 DS    0H                  LOOK FOR NEXT GROUP                          
         OC    KEY+21(6),KEY+21    GROUP COMMENT RECORD?                        
         BZ    LMGS0010            MUST BE HEADER RECORD!                       
         DC    H'0'                NO  - MUST BE THERE...                       
*                                  RETRIEVE THE GROUP COMMENT RECORD            
LMGS0050 DS    0H                                                               
         GOTO1 =A(GRPCOMNT),DMCB,(R2),(R7),RR=Y                                 
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT LINE                            
         LA    R8,NEXTSELF(R8)     BUMP TO NEXT LINE                            
         LA    R7,NEXTSELF(R7)     BUMP TO NEXT SET'S SELECT FIELD              
         LA    R5,4(R5)            DUMP TO NEXT D/A SPACE                       
         GOTO1 STARTNXT            REESTABLISH ORIGINAL START                   
LMGS0060 EQU   *                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         XC    TWAMKGLA,TWAMKGLA   CLEAR THE SELECTED FIELD                     
*                                                                               
LMGS0070 EQU   *                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWAMKGLA,KEY+28     SAVE D/A LAST ENTRY ON SCREEN                
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE THE RECORD                          
         OC    RMKGKPLN(6),RMKGKPLN                                             
*                                  GROUP COMMENT RECORD?                        
         BNZ   LMGS0120            NO  - DETAIL RECORD                          
*                                                                               
* MGXSEL CANCELLED REJECTED SELF-APPLIED OFFER SHOULD NOT SHOW UP               
* IN MGL                                                                        
*                                                                               
LMGS0080 EQU   *                                                                
         TM    RMKGSFG1,RMGF1MCN   EDI CANCELLED OFFER?                         
         BZ    LMGS0082                                                         
         TM    RMKGSFG3,RMGF3SAQ   WAS SELF-APPLIED?                            
         BO    LMGS0090                                                         
*                                                                               
* APPLIED ORDERS SHOULD BE HIDDEN IF THEY HAVE LOWER MOD NUMBER                 
* THAN THE CONTRACTS                                                            
*                                                                               
LMGS0082 EQU   *                                                                
         TM    RMKGSCST,RMKGSAPQ   APPLIED ORDERS ONLY                          
         BZ    LMGS0085                                                         
         TM    RMKGSFG3,RMGF3SAQ   SELF-APPLY?                                  
         BZ    LMGS0083                                                         
         TM    RMKGSFG3,RMGF3ARQ   APPROVAL RECEIVED?                           
         BZ    LMGS0085            NOT YET, STILL SHOW OFFER                    
*                                                                               
LMGS0083 DS    0H                                                               
         CLC   RCONMOD,RMKGAPMN                                                 
         BH    LMGS0090                                                         
*                                                                               
* NEWLY CREATED OFFERS THAT HAVE NOT BEEN MGS'ED SHOULD NOT APPEAR ON           
* LIST OF THE OPPOSITE PARTY                                                    
*                                                                               
LMGS0085 DS    0H                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BNZ   LMGS0100            ALREADY IN DARE, SHOW IT                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0100                                                         
         USING RCONMGEL,R6                                                      
*                                                                               
         TM    RMKGSCST,RMKGSCRQ                                                
         BZ    LMGS0087            REP IS OFFERER                               
         CLI   TWAACCS,C'$'        STATION IS DOING THE LIST                    
         BNE   LMGS0100                                                         
*                                                                               
         CLC   RMKGSCRD,RCONRMDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0086            THAN LAST REP MGS DATE/TIME                  
         BL    LMGS0100            DON'T SHOW IN LIST                           
         CLC   RMKGSCRT,RCONRMTM                                                
         BH    LMGS0090                                                         
         B     LMGS0100                                                         
*                                                                               
LMGS0086 DS    0H                  STATION IS OFFERER                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0090                                                         
         USING RCONSEND,R6                                                      
*                                                                               
         CLC   RMKGSCRD,RCONSRDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0090            THAN LAST STATION SEND DATE/TIME             
         BL    LMGS0100            DON'T SHOW IN LIST                           
         GOTO1 HEXIN,DMCB,RCONSRTI,WORK,6                                       
         CLC   RMKGSCRT,WORK                                                    
         BL    LMGS0100                                                         
         DROP  R6                                                               
*                                                                               
LMGS0087 DS    0H                  STATION IS OFFERER                           
         CLI   TWAACCS,C'$'                                                     
         BE    LMGS0100                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0100                                                         
         USING RCONMGEL,R6                                                      
*                                                                               
         CLC   RMKGSCRD,RCONMGDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0088            THAN LAST STATION MGS DATE/TIME              
         BL    LMGS0100            DON'T SHOW IN LIST                           
         CLC   RMKGSCRT,RCONMGTM                                                
         BL    LMGS0100                                                         
         DROP  R6                                                               
*                                                                               
LMGS0088 DS    0H                  STATION IS OFFERER                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0090                                                         
         USING RCONSEND,R6                                                      
*                                                                               
         CLC   RMKGSCRD,RCONSSDT   IF CREATION DATE/TIME IS LATER               
         BH    LMGS0090            THAN LAST STATION SEND DATE/TIME             
         BL    LMGS0100            DON'T SHOW IN LIST                           
         GOTO1 HEXIN,DMCB,RCONSSTI,WORK,6                                       
         CLC   RMKGSCRT,WORK                                                    
         BL    LMGS0100                                                         
         DROP  R6                                                               
*                                                                               
LMGS0090 DS    0H                  LOOK FOR NEXT GROUP                          
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE     FOUND?                         
         BE    LMGS0090                                                         
*                                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE     FOUND?                         
         BE    LMGS0095            MUST BE SAME CONTRACT                        
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         XC    TWAMKGLA,TWAMKGLA   START OVER ON THE NEXT PASS                  
         B     LMGSX                                                            
         DROP  RF                                                               
*                                                                               
LMGS0095 DS    0H                                                               
         OC    KEY+21(6),KEY+21    GROUP COMMENT RECORD?                        
         BNZ   LMGS0090            MUST BE HEADER RECORD!                       
*        DC    H'0'                NO  - MUST BE THERE...                       
*                                  SKIP MAKEGOOD INSTEAD OF DEATH               
         GOTO1 VGETREC,DMCB,RMKGREC                                             
         B     LMGS0080                                                         
*                                                                               
LMGS0100 DS    0H                                                               
         GOTO1 =A(GRPCOMNT),DMCB,(R2),(R7),RR=Y                                 
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT LINE                            
         LA    R8,NEXTSELF(R8)     BUMP TO NEXT LINE                            
         LA    R7,NEXTSELF(R7)     BUMP TO NEXT SET'S SELECT FIELD              
         LA    RF,MGCMLSTH         CHECK FOR END OF SCREEN                      
*                                     NEED 2 LINES FOR DATA, SO                 
*                                        CHECKING 1 LINE SHORT OF END           
         CR    R2,RF                                                            
         BNL   LMGS0600            END OF SCREEN REACHED                        
         LA    R5,4(R5)            BUMP D/A TABLE TO NEXT ENTRY -               
*                                     SKIP ONLY 1 ENTRY!                        
         B     LMGS0560            DON'T DISPLAY ANYTHING MORE                  
*                                                                               
LMGS0120 EQU   *                                                                
         TM    RMKGRTS,X'20'       BONUS                                        
         BZ    LMGS0130                                                         
         TM    RMKGRTS,X'20'+X'04' LATE RUN BONUS, SKIP MISSED CMT              
         BO    LMGS0280                                                         
         CLC   LASTLINS,RMKGKMLN   SAME MASTER/MAKEGOOD LINE#?                  
         BE    LMGS0280            YES - DON'T DISPLAY MISSED COMMT             
         MVC   LASTLINS(2),RMKGKMLN                                             
         B     LMGS0140                                                         
*                                                                               
LMGS0130 EQU   *                                                                
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
*                                                                               
*        CLC   RMKGMGLI,LASTLINS   SAME MASTER/MAKEGOOD LINE#?                  
*        BE    LMGS0280            YES - DON'T DISPLAY MISSED COMMT             
         OC    LASTLINS,LASTLINS                                                
         BNZ   LMGS0280            DISPLAY ONLY ONCE                            
*                                                                               
         MVC   LASTLINS(1),RMKGMGLI                                             
         DROP  R6                                                               
*                                                                               
LMGS0140 EQU   *                   SAVE MASTER/MAKEGOOD LINE#                   
         LA    R6,RMKGELEM         LOOK FOR X'10' CONTROL ELEMENT               
LMGS0160 EQU   *                                                                
         ZIC   RF,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BE    LMGS0560            YES - NO CONTROL ELEMENT - SKIP IT           
         CLI   0(R6),X'10'         CONTROL ELEMENT?                             
         BL    LMGS0160            NO  - GO BACK FOR NEXT                       
         BH    LMGS0230                                                         
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,RMKGCDDS-RMKGCDCD                                             
*                                  CALCULATE L(COMMENT)                         
         SR    RF,RE                                                            
         LTR   RF,RF               ANYTHING LEFT?                               
         BZ    LMGS0230            IF NO COMMENT, SKIP DISPLAY                  
*                                                                               
LMGS0180 EQU   *                                                                
         BCTR  RF,0                - 1 FOR EX                                   
         CH    RF,=H'67'           MAX DISPLAYABLE IS 67 CHARS                  
         BNH   *+8                 LESS THAN MAX                                
         LA    RF,67               ONLY LOAD FIRST 67 CHARS                     
         EX    RF,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   MGOFCMT(0),RMKGCDDS-RMKGCDCD(R6)                                 
*                                                                               
         LR    RF,R2                                                            
         SH    RF,=H'8'                                                         
         NI    1(RF),X'FF'-X'08'   SET TO NORMAL INTENSITY                      
*                                                                               
LMGS0220 EQU   *                                                                
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT LINE                            
         LA    R8,NEXTSELF(R8)     BUMP TO NEXT LINE                            
         LA    R7,NEXTSELF(R7)     BUMP TO NEXT SELECT FIELD                    
*                                                                               
LMGS0230 EQU   *                                                                
         MVC   0(4,R5),KEY+28      SAVE D/A IN CASE COMMENT IS LAST             
*                                     ITEM ON SCREEN                            
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OI    TWAMKGFG,X'10'      TURN ON 'LAST LINE=COMMENT'                  
         DROP  RF                                                               
*                                                                               
         LA    RF,MGCMLSTH         CHECK FOR END OF SCREEN                      
*                                     NEED 2 LINES FOR DATA, SO                 
*                                        CHECKING 1 LINE SHORT OF END           
         CR    R2,RF                                                            
         BNL   LMGS0600            END OF SCREEN REACHED                        
*        LA    R5,4(R5)            BUMP D/A TABLE TO NEXT ENTRY                 
         B     LMGS0120            GO BACK AND REINSERT INTO DISPLAY            
*                                                                               
LMGS0280 EQU   *                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         NI    TWAMKGFG,X'FF'-X'10'                                             
*                                  TURN OFF 'LAST LINE=COMMENT' FLAG            
         DROP  RF                                                               
*                                                                               
*                                  CALC A(DESCRIPTION FIELD HEADER)             
LMGS0285 EQU   *                                                                
         LR    RF,R7               SET A(SELECT FIELD HEADER)                   
         ZIC   RE,0(R7)            EXTRACT L(SELECT FIELD)                      
         AR    RF,RE               BUMP TO NEXT FIELD, WHICH                    
*                                     IS DESCRIP FLD HDR                        
         NI    1(RF),X'F7'         TURN OFF HI-INTENSITY BIT FOR                
*                                     LINE'S DESCRIPTION                        
         LA    RF,NEXTSELF(RF)                                                  
         NI    1(RF),X'F7'         TURN OFF HI-INTENSITY BIT FOR                
*                                     NEXT LINE'S DESCRIPTION                   
*                                                                               
         LA    R6,RMKGREC                                                       
         TM    RMKGRTS,X'10'       PREEMPT                                      
         BZ    LMGS0286                                                         
         MVC   MGOFDATE(7),=C'PREEMPT'                                          
         B     LMGS0289                                                         
*                                                                               
LMGS0286 EQU   *                                                                
         TM    RMKGRTS,X'02'       REPLACEMENT OFFER?                           
         BZ    LMGS0288                                                         
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    LMGS0288                                                         
         MVC   MGOFDATE(13),=C'CREDIT FOR NA'                                   
         B     LMGS0289                                                         
*                                                                               
LMGS0288 EQU   *                                                                
         GOTO1 OFFRDATE,DMCB,(RC),(R2)                                          
*                                  INSERT OFFER DATE/# SPOTS                    
LMGS0289 EQU   *                                                                
         ZIC   RF,RMKGKRTY         CHECK RECORD NUMBER                          
         SLL   RF,28               DROP ALL BUT LAST 4 BITS                     
         SRL   RF,28               RESTORE                                      
         CH    RF,=H'1'            FIRST LINE OF POSSIBLE SET? (0/1)            
         BH    LMGS0290            YES                                          
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   LMGS0320            NO  - NOTHING TO SET                         
         B     LMGS0300            YES - CHECK IF CHOSEN                        
*                                                                               
LMGS0290 EQU   *                                                                
         MVC   MGORIND(3),=C'AND'  NO  - INSERT 'AND' INDICATOR                 
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   LMGS0320            NO  - LEAVE 'AND' SET                        
         MVC   MGORIND(3),=C'OR '  YES - SET 'OR' INDICATOR                     
*                                                                               
LMGS0300 EQU   *                                                                
         LA    R6,RMKGREC          FOR A CHOICE OFFER                           
         MVI   ELCODE,X'20'        SHOW IF CHOSEN                               
         BAS   RE,GETEL                                                         
         BNE   LMGS0320                                                         
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'                                                   
         BZ    LMGS0320                                                         
         MVI   MGORIND+2,C'*'                                                   
         DROP  R6                                                               
                                                                                
LMGS0320 EQU   *                                                                
         CLC   =C'AND',MGORIND     ANYTHING IN FIELD?                           
         BE    LMGS0330            YES - DON'T SHOW MISSED DATE                 
*                                     OR LINE #S AGAIN                          
         CLC   =C'OR',MGORIND      CHECK OTHER POSSIBLE VALUE                   
         BNE   LMGS0340                                                         
*                                                                               
* DISPLAY MISSED LINE/DATE ON ALL LINES IF OFFER IS PREEMPT OR LATE RUN         
*                                                                               
LMGS0330 EQU   *                                                                
         TM    RMKGRTS,X'20'       SKIP BONUS/LATE RUN BONUS                    
         BO    LMGS0390                                                         
         TM    RMKGRTS,X'10'+X'08'+X'04'                                        
         BZ    LMGS0400                                                         
         B     LMGS0350                                                         
*                                                                               
LMGS0340 EQU   *                                                                
         NI    1(R7),X'FF'-X'20'   NO, TURN OFF PROTECT BIT FOR LINE'S          
*                                     SELECT COLUMN                             
         MVI   MGEYECAT,C'<'       PUT IN 'EYE-CATCHER'                         
*                                                                               
LMGS0350 EQU   *                                                                
         LA    R3,RMKGELEM         FIND MG CONTROL DESC ELT                     
*                                                                               
LMGS0360 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         CLI   0(R3),0             END OF RECORD?                               
         BE    LMGS0400            YES - NO CONTROL - SKIP IT                   
         CLI   0(R3),X'05'         BUY M/G REF ELEMENT?                         
         BL    LMGS0360            NO  - CHECK FOR CONTROL ELEMENT              
         BH    LMGS0390                                                         
         USING RMKGMGEL,R3                                                      
*                                  YES - DISPLAY ITS DATA                       
*                                                                               
         EDIT  RMKGMGLI,(3,MGORGBUY),ALIGN=LEFT                                 
*                                  INSERT ORIGINAL LINE #                       
         CLI   MGEYECAT,C'<'       ONLY FOR EYE-CATCHER LINE                    
         BNE   LMGS0370                                                         
         EDIT  RMKGKLIN,(3,MGBUYOFR),ALIGN=LEFT                                 
*                                  INSERT OFFER# FOR THIS LINE                  
         DROP  R3                                                               
*                                                                               
LMGS0370 EQU   *                                                                
         USING RMKGMGEL,R3                                                      
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),(5,MGMSDATE)                            
*                                  INSERT MISSED DATE                           
*                                                                               
         MVI   MGMSDATE+8,C'('     INSERT PAREN                                 
         LA    R4,MGMSDATE+9                                                    
         EDIT  RMKGMGSP,(3,(R4)),ALIGN=LEFT                                     
         AR    R4,R0               ADD SIGNIFICANT CHARS FROM EDIT              
         MVI   0(R4),C')'          INSERT OTHER PAREN                           
*                                  INSERT NUMBER SPOTS MISSED                   
         AHI   R4,1                                                             
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    LMGS0380                                                         
         MVI   0(R4),C'-'          SIGNIFY THAT THIS IS A DATE RANGE            
         AHI   R4,1                                                             
         CHI   R0,1                SKIP MULTI CHECK, WON'T FIT ANYWAY           
         BH    LMGS0390                                                         
         DROP  R3                                                               
*                                                                               
LMGS0380 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         CLI   0(R3),X'05'         MULTI-MISSED DATES?                          
         BNE   LMGS0390                                                         
         MVI   0(R4),C'*'          YES, JUST SHOW THAT THERE ARE MORE           
*                                                                               
LMGS0390 EQU   *                                                                
         LA    R6,RMKGREC                                                       
         TM    RMKGRTS,X'20'       BONUS                                        
         BZ    LMGS0400                                                         
         XC    MGMSDATE,MGMSDATE                                                
         MVC   MGMSDATE(5),=C'BONUS'                                            
         CLI   MGEYECAT,C'<'                                                    
         BE    LMGS0395                                                         
         B     LMGS0400                                                         
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
         EDIT  RMKGKMLN,(3,MGORGBUY),ALIGN=LEFT                                 
         DROP  R6                                                               
*                                  INSERT ORIGINAL LINE #                       
LMGS0395 EQU   *                                                                
         EDIT  RMKGKLIN,(3,MGBUYOFR),ALIGN=LEFT                                 
*                                  INSERT OFFER# FOR THIS LINE                  
*                                                                               
LMGS0400 EQU   *                                                                
         CLI   MGEYECAT,C'<'       SAVE D/A FOR SELECTABLE RECS ONLY            
         BNE   *+10                                                             
         MVC   0(4,R5),KEY+28      TABLE DISK ADDRESS                           
*                                                                               
         LR    R4,R2                                                            
*                                                                               
         GOTO1 =A(MKGDETLS),DMCB,(RC),(RA),(R2),(R8),RR=Y                       
*                                                                               
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   LMGS0410                                                         
         USING RMKGDCEL,R6                                                      
         ZIC   R1,RMKGDCLN         GET ELEMENT LENGTH                           
         CHI   R1,3                                                             
         BL    LMGS0410                                                         
*                                                                               
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT SET OF LINES                    
         LA    R8,NEXTSELF(R8)     BUMP TO NEXT SET OF LINES                    
         LA    R7,NEXTSELF(R7)     BUMP TO NEXT SET'S SELECT FIELD              
*                                                                               
         LA    RF,MGCMLSTH         CHECK FOR END OF SCREEN                      
*                                     NEED 1 LINES FOR DATA, SO                 
*                                        CHECKING 1 LINE SHORT OF END           
         CR    R2,RF                                                            
         BH    LMGS0410            END OF SCREEN REACHED                        
*                                                                               
         ZIC   R1,RMKGDCLN         GET ELEMENT LENGTH                           
         SHI   R1,3                SUBTRACT FOR CONTROL + EX                    
         CHI   R1,30               MAX DISPLAYABLE IS 30 CHARS                  
         BL    *+8                 LESS THAN MAX                                
         LA    R1,29               ONLY LOAD FIRST 30 CHARS                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGDESC+4(0),RMKGDCCM                                             
         MVC   MGDESC(3),=C'CMT'                                                
*                                                                               
         LR    RF,R8                                                            
         SHI   RF,8                                                             
         NI    1(RF),X'FF'-X'08'   SET TO NORMAL INTENSITY                      
         OI    6(RF),X'80'         XMIT                                         
*                                                                               
*                                                                               
LMGS0410 EQU   *                                                                
         LA    R2,NEXTSEL2(R2)     BUMP TO NEXT SET OF LINES                    
         LA    R8,NEXTSEL2(R8)     BUMP TO NEXT SET OF LINES                    
         LA    R7,NEXTSEL2(R7)     BUMP TO NEXT SET'S SELECT FIELD              
*                                                                               
         LA    RF,MGCMLSTH         CHECK FOR END OF SCREEN                      
*                                     NEED 2 LINES FOR DATA, SO                 
*                                        CHECKING 1 LINE SHORT OF END           
         CR    R2,RF                                                            
         BNL   LMGS0600            END OF SCREEN REACHED                        
         CLI   0(R4),C'<'          SAVE D/A FOR SELECTABLE RECS ONLY            
         BNE   *+8                                                              
         LA    R5,4(R5)            BUMP D/A TABLE TO NEXT ENTRY -               
*                                                                               
LMGS0560 EQU   *                                                                
         GOTO1 VSEQ                READ NEXT RECORD                             
*                                  SAME KEY THROUGH CONTRACT?                   
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   LMGSX               NO  - FINISHED                               
         B     LMGS0070            YES - GO BACK FOR NEXT                       
LMGS0600 EQU   *                                                                
*        LR    RF,RA                                                            
*        AH    RF,=Y(TWAWORKQ)                                                  
*        USING TWAWORK,RF                                                       
*        MVC   TWAMKGLA,0(R5)      SAVE D/A LAST ENTRY ON SCREEN                
*        DROP  RF                                                               
LMGSX    EQU   *                                                                
         XIT1                                                                   
LMGS0680 MVC   0(0,R4),2(R6)       INSERT DETAIL LINE COMMENT                   
         DROP  R8                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  OFFRDATE:  RETRIEVE/DISPLAY OFFER DATE(S) ON SECOND LINE OF                  
*        DISPLAY.  INSERT # SPOTS/WEEK OFFERED.                                 
*                                                                               
OFFRDATE NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(SCREEN LINE)                         
         USING MGCNTRL,R2                                                       
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MGOFDATE         OUTPUT                                       
*                                  DISPLAY DATES                                
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R3))                                   
*                                  START DATE                                   
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R3,5(R3)                                                         
         B     OFDT0010                                                         
*                                                                               
         MVI   5(R3),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R3))                                    
*                                  END DATE                                     
         LA    R3,11(R3)                                                        
*                                                                               
OFDT0010 TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                  DISPLAY NPW IF NOT = RBUYNW                  
         CLC   RBUYNW,9(R6)                                                     
         BE    OFDT0020                                                         
         MVI   0(R3),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R3)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R3),C'0'                                                       
         LA    R3,2(R3)                                                         
         MVI   2(R3),C' '                                                       
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C')'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
OFDT0020 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   OFDTX                                                            
         MVI   0(R3),C'*'                                                       
*                                                                               
OFDTX    DS    0H                                                               
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*  UNPROT:    UNPROTECT TEXT FIELDS FOR FOUT.  ALSO UNPROTECT ALL               
*        'SELECT' FIELDS IN EVENT ANY WERE 'PROTECTED' BECAUSE                  
*        COMMENTS WERE DISPLAYED THERE.                                         
*                                                                               
UNPROT   NTR1                                                                   
         LA    R2,MGCMDESH         SET A(1ST DESCRIPTION FIELD)                 
         LA    R3,MGCLAST          SET A(EOSCREEN)                              
UNPR0020 EQU   *                                                                
         CR    R2,R3               END OF SCREEN REACHED?                       
         BNL   UNPR0080            YES - EXIT                                   
         NI    1(R2),X'DF'         TURN OFF PROTECT BIT (DESCRIPT)              
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT LINE                            
         B     UNPR0020            GO BACK FOR NEXT                             
UNPR0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
*  PROT:    PROTECT TEXT FIELDS AFTER FOUT.  PROTECT SELECT FIELDS              
*        ALSO.  WILL BE OPENED UP WHEN SELECTION DATA PUT IN.                   
*                                                                               
PROT     NTR1                                                                   
         LA    R7,MGCMSELH         SET A(1ST SELECT FIELD)                      
         LA    R2,MGCMDESH         SET A(1ST DESCRIPTION FIELD)                 
         LA    R3,MGCLAST          SET A(EOSCREEN)                              
PROT0020 EQU   *                                                                
         CR    R2,R3               END OF SCREEN REACHED?                       
         BNL   PROT0080            YES - EXIT                                   
         OI    1(R2),X'08'         TURN ON  HIGH INTENSITY (DESCRIPT)           
         OI    1(R2),X'20'         TURN ON  PROTECT BIT (DESCRIPT)              
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         OI    1(R7),X'20'         TURN ON  PROTECT BIT (SELECT)                
         OI    6(R7),X'80'         TURN ON TRANSMIT BIT                         
         LA    R2,NEXTSELF(R2)     BUMP TO NEXT LINE                            
         LA    R7,NEXTSELF(R7)     BUMP TO NEXT LINE                            
         B     PROT0020            GO BACK FOR NEXT                             
PROT0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*  GETBUYDA:  RETRIEVE DISK ADDRESS FOR THIS RECORD, INSERT INTO                
*      TWAMKGDS FIELD, SET ERROR FLAG IF NOT FOUND.                             
*                                                                               
GETBUYDA NTR1                                                                   
         LR    R6,RA                                                            
         AHI   R6,TWAWORKQ                                                      
         USING TWAWORK,R6                                                       
*                                                                               
         XC    RBUYREC(32),RBUYREC                                              
         MVI   RBUYKTYP,X'0B'      SET KEY FOR BUY RECORD                       
         MVC   RBUYKREP,TWAAGY     INSERT POWER CODE                            
         MVC   RBUYKCON,TWACNUM                                                 
*                                                                               
         MVC   KEY,RBUYREC         LOAD KEY FROM RECORD                         
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
GETB0010 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY - THROUGH CON#                      
         BE    GETB0020            YES - CHECK FOR BUY LINE #                   
         LTR   RB,RB               NO KEY FOUND - SET CC NOT ZERO               
         B     GETB0080                                                         
GETB0020 EQU   *                                                                
         CLC   TWAMKGDS+3(1),KEY+RBUYKLIN-RBUYKEY                               
*                                  LINE NUMBER FOUND?                           
*        BNE   GETB0040            NO  - GO FOR NEXT LINE                       
*        CLC   KEY+RBUYKLIN-RBUYKEY(1),KEY+RBUYKMLN-RBUYKEY                     
*                                  YES - MASTER LINE = LINE #?                  
         BE    GETB0060            YES - LINE FOUND                             
GETB0040 EQU   *                                                                
         GOTO1 VSEQ                GET NEXT RECORD                              
         B     GETB0010            GO BACK FOR COMPARISON                       
GETB0060 EQU   *                                                                
         MVC   TWAMKGDS,KEY+28     SET DISK ADDRESS                             
         MVC   TWAMKGL#,KEY+RBUYKLIN-RBUYKEY                                    
*                                  SAVE LINE NUMBER                             
         OI    TWAMKGFG,X'40'      INSERT 'ADD' FLAG                            
         OI    TWAMKGFG,X'20'      INDICATE 'FROM BUYLINE'                      
*                                                                               
         DROP  R6                                                               
*                                                                               
         SR    R0,R0               SET CC = ZERO                                
GETB0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   GRPCOMNT:  DISPLAY GROUP COMMENT AND GROUP CODE ON COMMENT                  
*        LINE.                                                                  
*                                                                               
GRPCOMNT NTR1                                                                   
         L     R2,0(R1)            SET A(SCREEN LINE BEING SET UP)              
         L     R7,4(R1)            SET A(SCREEN LINE BEING SET UP)              
         USING MGCNTRL,R2                                                       
*                                                                               
         NI    1(R7),X'FF'-X'20'   TURN OFF PROTECT BIT FOR HEADER              
*                                                                               
         MVI   MGEYECAT,C'<'                                                    
         MVC   MGGRPCDE,RMKGKGR1   GROUP CODE                                   
*                                                                               
         TM    RMKGSCST,RMKGSCRQ                                                
         BNZ   GCOM0010                                                         
         MVC   MGCREATE,=C'*STA'   SHOW IF STATION CREATED                      
*                                                                               
GCOM0010 EQU   *                                                                
*                                                                               
* DISPLAY STATUS                                                                
*                                                                               
         MVC   MGSTAT,=C'NEW'      DEFAULT TO NEW                               
* APPLIED ?                                                                     
         TM    RMKGSCST,RMKGSAPQ                                                
         BZ    GCOM0013                                                         
         MVC   MGSTAT,=C'APL'                                                   
         TM    RMKGSFG3,RMGF3SAQ   SELF APPLY?                                  
         BZ    GCOM0015                                                         
         TM    RMKGSFG3,RMGF3ARQ   APPROVAL ALREADY RECEIVED?                   
         BNZ   GCOM0015                                                         
         MVC   MGSTAT,=C'SEL'      NO, SHOW AS SEL INSTEAD OF APL               
         CLC   RMKGAPMN,RCONMOD    SHOW IF STATION HAS SUBSEQUENTLY             
         BNL   GCOM0015            CONFIRMED THE CONTRACT                       
         MVI   MGSTAT+3,C'*'                                                    
         B     GCOM0015                                                         
* RECALLED?                                                                     
GCOM0013 EQU   *                                                                
         TM    RMKGSCST,RMKGSRCQ                                                
         BZ    *+14                                                             
         MVC   MGSTAT,=C'REC'                                                   
         B     GCOM0015                                                         
* REJECTED?                                                                     
         TM    RMKGSCST,RMKGSRJQ                                                
         BZ    *+14                                                             
         MVC   MGSTAT,=C'REJ'                                                   
         B     GCOM0015                                                         
* REVISED?                                                                      
         TM    RMKGSCST,RMKGSRVQ                                                
         BZ    GCOM0014                                                         
         MVC   MGSTAT,=C'REV'                                                   
         B     GCOM0015                                                         
                                                                                
GCOM0014 DS    0H                                                               
         TM    RMKGSCST,RMKGSCNQ                                                
         BZ    GCOM0015                                                         
         MVC   MGSTAT,=C'MGX'                                                   
                                                                                
GCOM0015 DS    0H                  GET GROUP COMMENT                            
         LA    R6,RMKGREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   GCOM0030                                                         
         USING RMKGGCEM,R6                                                      
         ZIC   RE,RMKGGCLN         DERIVE LENGTH OF COMMENT                     
         SH    RE,=H'3'            MINUS CONTROL , 1 FOR EX                     
         LA    R4,MGOFCMT          SET GROUP COMMENT INTO SCREEN                
         CH    RE,=H'60'           MAX GROUP COMMENT?                           
         BL    GCOM0020            NO                                           
         LA    RE,59               YES - SET TO MAX SIZE                        
GCOM0020 EQU   *                                                                
         EX    RE,*+8              MOVE COMMENT BY LENGTH                       
         B     *+10                                                             
         MVC   0(0,R4),RMKGGCCM    INSERT DESCRIPTION                           
GCOM0030 EQU   *                                                                
         XC    LASTLINS,LASTLINS   CLEAR COMPARE VALUE                          
         MVC   0(4,R5),KEY+28      INSERT D/A OF RECORD IN CASE                 
*                                     COMMENT IS LAST ITEM ON SCREEN            
         LR    R6,RA                                                            
         AH    R6,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R6                                                       
         OI    TWAMKGFG,X'10'      TURN ON 'LAST LINE=COMMENT'                  
         B     EXXMOD                                                           
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTD1D                                                       
MGCNTRL  DSECT                                                                  
MGEYECAT DS    CL1                                                              
MGORIND  DS    CL3                 'CHOICE' INDICATOR (OR-LITERAL)              
*                                                                               
         ORG   MGORIND                                                          
MGGRPCDE DS    CL2    +1           MAKEGOOD OFFER GROUP CODE                    
         DS    CL1    +3                                                        
MGSTAT   DS    CL3    +4           MAKEGOOD OFFER STATUS                        
*                                                                               
         ORG   MGSTAT                                                           
MGBUYOFR DS    CL3    +4           MAKEGOOD OFFER # WITHIN BUY                  
         DS    CL1    +7                                                        
MGOFCMT  DS    0C                                                               
MGORGBUY DS    CL3    +8           ORIGINAL BUY # OF MISSED SPOTS               
         DS    CL1    +11                                                       
MGMSDATE DS    CL13   +12          DATE/#SPOTS MISSED                           
         DS    CL1    +25                                                       
MGOFDATE DS    CL18   +26          DATE/#SPOTS OFFERED                          
         DS    CL1    +44                                                       
MGDAYTIM DS    CL24   +45          DAY STRING(S)                                
         DS    CL1    +69                                                       
MGLENGTH DS    CL3    +70          TIME STRING(S)                               
*                                                                               
         ORG   MGOFCMT                                                          
         DS    CL60                                                             
         DS    CL1                                                              
MGCREATE DS    CL4                 OFFER CREATED BY REP OR STA                  
*                                  SECOND LINE DESCRIPTION                      
MGCNTRL2 DSECT                                                                  
MGEYECT2 DS    CL1    +0                                                        
         DS    CL11   +1                                                        
MGNPW    DS    CL3    +12          SPOTS PER WEEK                               
         DS    CL1    +15                                                       
MGCOST   DS    CL10   +16          COST                                         
         DS    CL1    +26                                                       
MGDESC   DS    CL42   +27          DETAIL LINE DESCRIPTION                      
*                                                                               
         EJECT                                                                  
*                                                                               
*   FOXFIX: ROUTINE TO READ PETRY MAKEGOOD OFFERS FOR THIS CONTRACT             
*           AND ADD THEM UNDER FOX FOR STATIONS:                                
*           WNYW, KTTV, WFLD, WTXF AND WFXT.                                    
*                                                                               
*           IF THE MAKEGOOD OFFERS ALREADY EXIST IN THE FOX FILE THEN           
*           WE SHOULD EXIT. WE DON'T WANT TO OVERWRITE                          
*           ANY NEW OR PREVIOUSLY ADDED AND SUBSEQUENTLY CHANGED                
*           MAKEGOOD OFFERS!!                                                   
*                                                                               
         CSECT                                                                  
         DS    0F                                                               
FOXFIX   NMOD1 0,*FOXX*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         USING GENOLD,RC                                                        
         L     RA,4(R1)            RESET A(TWASPACE)                            
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
         CLC   =C'FN',REPALPHA                                                  
         BNE   FOXXIT                                                           
         CLC   RCONKCON,=X'02512334'                                            
         BH    FOXXIT              'HIGH' CONTRACT NUMBER                       
         CLC   =C'WNYW',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'KTTV',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'WFLD',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'WTXF',RCONKSTA                                                
         BE    FOX10                                                            
         CLC   =C'WFXT',RCONKSTA                                                
         BNE   FOXXIT                                                           
*                                                                               
FOX10    DS    0H                                                               
         XC    RMKGREC(32),RMKGREC                                              
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,=C'PV'     INSERT PETRY POWER CODE                      
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE CODE                           
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION CALL LETTERS                  
         MVC   RMKGKCON,TWACNUM                                                 
*                                                                               
         MVC   KEY,RMKGREC         LOAD KEY FROM RECORD                         
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
*                                                                               
FOX20    DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   FOXXIT              EXIT IF NO RECORD FOUND OR DONE              
*                                  FOUND ONE, SEE IF THIS EXISTS                
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  SAME KEY - THROUGH CON#                      
         MVC   KEY+6(2),=C'FN'     IN THE FOX FILE                              
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RMKGKEY),KEYSAVE                                           
         BE    FOXXIT              EXIT IF RECORD FOUND                         
*                                                                               
         MVC   RMKGKREP,=C'FN'     ADD RECORD AS FOX                            
         GOTO1 VADDREC,DMCB,RMKGREC                                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                HOUSTON, WE'VE GOT A PROBLEM                 
*                                                                               
         MVC   KEY(L'RMKGKEY),RMKGKEY                                           
         MVC   KEY+6(2),=C'PV'     IN THE FOX FILE                              
*                                  GET NEXT PETRY MAKEGOOD OFFER                
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'N'         SET UPDATE TO NO                             
         GOTO1 VSEQ                                                             
         B     FOX20                                                            
*                                                                               
FOXXIT   DS    0H                                                               
         XMOD1                                                                  
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*   DISPCON:   DISPLAY CONTRACT PORTION OF SCREEN FROM CONTRACT                 
*        RECORD.  THIS CODE HAS BEEN LIFTED FROM THE RECNT20 MOD.               
*                                                                               
         CSECT                                                                  
         DS    0F                                                               
DISPCON  NMOD1 0,*DCON*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         USING GENOLD,RC                                                        
         L     RA,4(R1)            RESET A(TWASPACE)                            
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
         LA    RF,CONCNUMH         SET CONTRACT NUMBER TO PREVALID              
         OI    4(RF),X'20'                                                      
*                                                                               
*              BUILD AGENCY                                                     
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,10         AGENCY REC TYPE                              
         MVC   RAGYKAGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RAGYNAM1,WAGYEXP                                                 
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0120                                                         
*                                                                               
DDIS0040 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAAGNM1,RAGYNAM1                                                
         MVC   TWAAGNM2,RAGYNAM2                                                
****>    MVC   TWAAGAD1,RAGYADD1                                                
****>    MVC   TWAAGAD2,RAGYADD2                                                
         MVC   TWAAGSTT,RAGYSTAT                                                
         MVC   TWAAGZIP,RAGYZIP                                                 
         MVC   TWAAEASY,RAGYPRO2   PROFILE FOR EASYLINK AGY COPY                
         MVC   TWAARISK,RAGYRISK   AGENCY CREDIT RISK                           
         MVC   TWAALIAB,RAGYLIAB   AGENCY LIABILITY POSITION                    
*                                                                               
DDIS0060 MVC   MGCAGY(4),RAGYKAGY  AGENCY                                       
         CLC   RAGYKAOF,MYSPACE3   OFFICE?                                      
         BE    DDIS0080                                                         
         LA    RE,MGCAGY                                                        
         MVI   MGCAGY+4,C' '                                                    
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RAGYKAOF    AGENCY OFFICE                                
DDIS0080 DS    0H                                                               
         MVC   MGCAGYN,RAGYNAM1                                                 
*                                                                               
*                                                                               
DDIS0120 DS    0H                                                               
*              ADVERTISER                                                       
DDIS0180 XC    IOAREA(32),IOAREA                                                
         MVI   RADVKTYP,8                                                       
         MVC   RADVKADV,RCONKADV   ADVERTISER                                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RADVNAME,WADVEXP                                                 
         OC    WADVEXP,WADVEXP                                                  
         BNZ   DDIS0220                                                         
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0240                                                         
*                                                                               
DDIS0200 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DDIS0220 MVC   MGCADV(4),RADVKADV                                               
         MVC   MGCADVN,RADVNAME                                                 
         EJECT                                                                  
*              STATION                                                          
DDIS0240 XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   RSTAMKT,WSTAEXP                                                  
         OC    WSTAEXP,WSTAEXP                                                  
         BNZ   DDIS0300                                                         
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
DDIS0300 DS    0H                                                               
         MVC   MGCSTAM,RSTAMKT                                                  
*                                                                               
         MVC   WORK(4),RSTAKSTA                                                 
*                                                                               
         MVC   WORK+4(3),=C'-FM'                                                
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVI   WORK+5,C'A'                                                      
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVI   WORK+5,C'C'                                                      
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'L '                                                 
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    DDIS0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'TV'                                                 
DDIS0320 CLI   WORK+3,C' '                                                      
         BNE   *+14                                                             
         MVC   WORK+3(3),WORK+4                                                 
         MVI   WORK+6,C' '                                                      
*                                                                               
         MVC   MGCSTA(7),WORK      STATION                                      
*                                                                               
DDIS0380 CLC   RCONPRD,MYSPACE3    PRODUCT CODE?                                
         BE    DDIS0440                                                         
* GET PRODUCT RECORD                                                            
         XC    IOAREA(32),IOAREA                                                
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
         MVC   RPRDNAME,WPRDEXP                                                 
         OC    WPRDEXP,WPRDEXP     PRODUCT LOOKED UP ALREADY?                   
         BZ    DDIS0400                                                         
         MVC   MGCPRD(2),=C'C='                                                 
         MVC   MGCPRD+2(3),RCONPRD                                              
         MVI   MGCPRD+5,0                                                       
         MVC   MGCPRD+6(14),RPRDNAME                                            
         B     DDIS0460                                                         
*                                                                               
DDIS0400 GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DDIS0460                                                         
*                                                                               
DDIS0420 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAPRDNM,RPRDNAME                                                
         MVC   MGCPRD(2),=C'C='                                                 
         MVC   MGCPRD+2(3),RCONPRD                                              
         MVI   MGCPRD+5,0                                                       
         MVC   MGCPRD+6(14),RPRDNAME                                            
         B     DDIS0460                                                         
*              FIND PRODUCT ELEMENT                                             
DDIS0440 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCOD3,X'05'                                                     
         BAS   RE,GETEL3                                                        
         BE    *+6                                                              
         DC    H'0'                NO X'05'                                     
         MVC   MGCPRD,2(R6)        PRODUCT EXPANSION                            
         EJECT                                                                  
*              GET SALESMAN                                                     
DDIS0460 XC    IOAREA(32),IOAREA                                                
         MVI   RSALKTYP,6          KEY TYPE - SALESMAN RECORD                   
         MVC   RSALKREP,REPALPHA   REP                                          
         MVC   RSALKSAL,RCONSAL    SALESMAN CODE                                
         MVC   KEY,IOAREA                                                       
         MVC   RSALNAME,WSALEXP                                                 
         OC    WSALEXP,WSALEXP                                                  
         BNZ   DDIS0480                                                         
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCOD3,X'9F'                                                     
         BAS   RE,GETEL3                                                        
         BNE   DDIS0480                                                         
         MVC   TWASALAS,22(R6)     SALES ASSISTANT                              
*                                                                               
DDIS0480 DC    0H'0'                                                            
         MVC   MGCSAL(3),RSALKSAL  SALESMAN CODE                                
         MVC   MGCSALN,RSALNAME                                                 
*                                                                               
*              K START DATE                                                     
DDIS0620 DS    0H                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),MGCODTS,0,DUB             
*                                                                               
* SET LENGTH OF DATES FOR RE-INPUT (ADDR)                                       
         MVI   MGCODTSH+5,17                                                    
*                                                                               
         XMOD1                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
GETEL3   EQU   *                                                                
         LA    R6,34(R6)           PASS THE KEY                                 
GETEL3A  EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    GETEL3X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCOD3,0(R6)                                                     
         BER   RE                  RETURN WITH CC =                             
         B     GETEL3A                                                          
GETEL3X  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT =                         
*                                                                               
*                                                                               
NXTEL3   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
FSTEL3   CLI   0(R6),0                                                          
         BE    NXTEL3X                                                          
         CLC   0(1,R6),ELCOD3                                                   
         BNE   NXTEL3                                                           
         BR    RE                                                               
*                                                                               
NXTEL3X  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
ELCOD3   DS    CL1                                                              
MYSPACE3 DC    60C' '                                                           
*                                                                               
         DROP  R8,RA,RC                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*   MKGDETLS:  DISPLAY DAY/TIME/LENGTH/COST OF SPOTS OF MAKEGOOD                
*        OFFER.                                                                 
*                                                                               
         CSECT                                                                  
         DS    0F                                                               
MKGDETLS NMOD1 0,*MKGD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         USING GENOLD,RC                                                        
         L     RA,4(R1)            RESET A(TWASPACE)                            
         USING TWAD,RA                                                          
         L     R2,8(R1)            RESET A(SCREEN LINE 1)                       
         USING MGCNTRL,R2                                                       
         L     R3,12(R1)           RESET A(SCREEN LINE 1)                       
         USING MGCNTRL2,R3                                                      
*                                  GET 1ST DAY-TIME ELEMENT                     
         LA    R6,RMKGREC                                                       
         TM    RMKGRTS,X'10'       PREEMPT                                      
         BO    MKGD0060                                                         
*                                  GET 1ST DAY-TIME ELEMENT                     
         TM    RMKGRTS,X'02'       REPLACEMENT FOR NA                           
         BZ    MKGD0010                                                         
         MVI   ELCOD4,X'03'        CHECK IF CREDIT FOR NA                       
         BAS   RE,GETEL4                                                        
         BNE   MKGD0060                                                         
*                                                                               
MKGD0010 EQU   *                                                                
         LA    R6,RMKGREC                                                       
         MVI   ELCOD4,X'02'                                                     
         BAS   RE,GETEL4                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MGDAYTIM(24),MYSPACE4                                            
*                                  CLEAR DAY/TIME STRING AREA                   
*                                  DISPLAY DAY-TIME FIELDS                      
         GOTO1 =V(OUTDAY),DMCB,3(R6),2(R6),MGDAYTIM,RR=YES                      
*                                  TIME                                         
         LA    R7,MGDAYTIM         CHECK FOR LAST POSITION                      
         LA    RF,24               MAX FIELD SIZE                               
MKGD0020 EQU   *                                                                
         CLI   0(R7),C' '          SPACE FOUND?                                 
         BE    MKGD0040            YES                                          
         LA    R7,1(R7)            GO BACK FOR NEXT                             
         BCT   RF,MKGD0020                                                      
         DC    H'0'                NOT FOUND - ERROR                            
MKGD0040 EQU   *                                                                
         MVI   0(R7),C'/'          INSERT SEPARATOR                             
         LA    R7,1(R7)            BUMP TO NEXT POSITION                        
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTO1 CALLOV,DMCB,0       GET ADDRESS OF UNTIME                        
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),4(R6),0(R7)                                            
*                                  LENGTH                                       
         MVC   HALF,RMKGDUR                                                     
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,MGLENGTH),ALIGN=LEFT                                 
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LA    R7,MGLENGTH                                                      
         LR    RE,R0               ADD LENGTH OF EDITED FIELD                   
         AR    RE,R7                                                            
         TM    RMKGDUR,X'80'       MINUTES?                                     
         BZ    MKGD0050                                                         
         MVI   0(RE),C'M'                                                       
MKGD0050 EQU   *                                                                
         EDIT  RMKGNW,(3,MGNPW),ALIGN=LEFT                                      
*                                                                               
         EDIT  RMKGCOS,(10,MGCOST),2,ALIGN=LEFT                                 
*                                                                               
* PROGRAM                                                                       
*                                                                               
MKGD0060 EQU   *                                                                
         LA    R6,RMKGREC                                                       
         MVI   ELCOD4,X'21'                                                     
         BAS   RE,GETEL4                                                        
         BNE   MKGDX                                                            
         USING RMKGPGEL,R6                                                      
         ZIC   RF,RMKGPGLN         GET ELEMENT LENGTH                           
         CHI   RF,3                                                             
         BL    MKGDX                                                            
         SHI   RF,3                SUBTRACT FOR CONTROL + EX                    
         CHI   RF,30               MAX DISPLAYABLE IS 30 CHARS                  
         BL    *+8                 LESS THAN MAX                                
         LA    RF,29               ONLY LOAD FIRST 30 CHARS                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MGDESC+4(0),RMKGPGM                                              
         MVC   MGDESC(3),=C'PGM'                                                
                                                                                
                                                                                
MKGDX    EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
GETEL4   EQU   *                                                                
         LA    R6,34(R6)           PASS THE KEY                                 
GETEL4A  EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    GETEL4X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCOD4,0(R6)                                                     
         BER   RE                  RETURN WITH CC =                             
         B     GETEL4A                                                          
GETEL4X  LTR   RE,RE                                                            
         BR    RE                  RETURN WITH CC NOT =                         
*                                                                               
*                                                                               
NXTEL4   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
FSTEL4   CLI   0(R6),0                                                          
         BE    NXTEL4X                                                          
         CLC   0(1,R6),ELCOD4                                                   
         BNE   NXTEL4                                                           
         BR    RE                                                               
*                                                                               
NXTEL4X  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
ELCOD4   DS    CL1                                                              
MYSPACE4 DC    60C' '                                                           
*                                                                               
         DROP  R2,R3,RA,RC                                                      
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'144RECNT2B   01/17/13'                                      
         END                                                                    
