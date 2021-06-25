*          DATA SET RECNT2F    AT LEVEL 049 AS OF 07/25/01                      
*PHASE T8022FC                                                                  
*INCLUDE OUTDAY                                                                 
         TITLE 'T8022F - REPPAK BUY-TO-RECOVERY-FILE'                           
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT2F (T8022F) --- BUY-TO-RECOVERY-FILE'                   *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
*                                                                     *         
* 25JUL01 BU  NEW SCREEN FIELDS                                       *         
* 11FEB98 BU  NEW EST/INV BUCKET DELETE OPTION.  B=MMM/YY OR          *         
*             I=MMM/YY DELETES CORRESPONDING BUCKET                   *         
*             'B=' = BOOKED/ESTIMATED 'I=' = INVOICED                 *         
* 12JAN98 BU  NEW 'RESET' OPTION.  DELETES X'08', REBUILDS AND        *         
*             REINSERTS.  COMBINATION 'ALLCLR' + 'ALLSEED'            *         
* 17MAR97 BU  NEW 'ALLSEED' OPTION.  WHERE NO X'08' ELEMENT EXISTS,   *         
*             BUILDS IT FOR THE BUYLINE                               *         
* 12DEC96 BU  NEW 'ALLCLR' OPTION:  RTS'S ALL BUYLINES, CLEARS        *         
*             TRANSFER INFO IN X'08' ELEMENT, AND RESETS TARGET       *         
*             REP (SPOT SIDE)                                         *         
* 05JAN96 SKU NEW 'ALL' OPTION TO RTS ALL BUYLINES                    *         
* 06OCT95 SKU 2K CONTRACT SUPPORT                                     *         
* 30DEC93 BU  ADD RANGE TEST TO RTS COMMAND                           *         
* 24MAR93 BU  WIPE OUT TRANSFER INFO IN X'08' ELEMENT: IN             *         
*             CONBNUM, IF 1ST POSITIONS ARE 'X=', NEXT                *         
*             POSITIONS ARE BUYLINE NUMBER.                           *         
* 05MAR93 BU  PERMIT ESTIMATE# CHANGE:  IN CONBNUM, IF 1ST            *         
*             POSITIONS ARE 'E=', NEXT 3 POSITIONS ARE OLD            *         
*             EST#, NEXT 3 POSITIONS ARE NEW EST#.  IF OLD            *         
*             NUMBER IS NOT FOUND IN X'08' ELEMENT OF BUY             *         
*             RECORD, RECORD IS NOT CHANGED.  ALL BUYS FOR            *         
*             CONTRACT ARE SCANNED.                                   *         
* 28SEP92 BU  ORIGINAL ENTRY                                          *         
*             THIS MODULE READS THE INDICATED BUY RECORD,             *         
*             FLIP/FLOPS THE RTS BIT, AND THEN REWRITES THE           *         
*             RECORD.  THIS ENSURES THAT IT APPEARS ON THE            *         
*             RECOVERY TAPE WITH NO OTHER ACTIVITY BEING              *         
*             DONE.                                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
T8022F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8022F                                                         
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         SPACE 1                                                                
DISP0020 LA    R2,CONBNUMH                                                      
         CLC   =C'E=',CONBNUM      ESTIMATE # CHANGE?                           
         BE    ESTCHG              YES - GO DO IT                               
         CLC   =C'X=',CONBNUM      TRANSFER INFO PURGE?                         
         BE    XFERPURG            YES - GO DO IT                               
         CLC   =C'B=',CONBNUM      DELETE BOOKED/ESTIMATE BUCKET?               
         BE    DELBUCK             YES - GO DO IT                               
         CLC   =C'I=',CONBNUM      DELETE INVOICE  BUCKET?                      
         BE    DELBUCK             YES - GO DO IT                               
         LA    R3,BUYERR                                                        
*                                                                               
         CLC   =C'RESET',8(R2)     RESET ALL BUYS?                              
         BE    DISP0024            YES - BUILD NEW ELEMENT                      
         CLC   =C'ALLSEED',8(R2)   SEED ALL BUYS?                               
         BNE   DISP0120            NO                                           
*                                  YES                                          
DISP0024 EQU   *                                                                
*                                                                               
*   BUILD A REP-TO-SPOT INTERFACE ELEMENT FOR SEEDING BUYS                      
*                                                                               
         XC    SPIFELT,SPIFELT     CLEAR THE ELEMENT                            
         MVC   SPIFCDLN,=X'0830'   SET ELEMENT CODE/LENGTH                      
*                                                                               
*   THE CONTRACT RECORD IS LOCATED AT RCONREC AT THIS TIME                      
*                                                                               
         MVC   SPIFSTAT(5),RCONKSTA                                             
*                                  INSERT STATION CALL LETTERS                  
         MVC   SPIFRADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   SPIFRPRD,RCONPRD    INSERT PRODUCT CODE                          
*                                                                               
*   TEST                                                                        
***>>>   MVC   SPIFRPRD,=C'001'    TEST PRODUCT CODE                            
*   TEST END                                                                    
*                                                                               
*   THE REP RECORD IS LOCATED AT IOAREA AT THIS TIME                            
*                                                                               
         LA    RF,RREPELEM         FIND REP X'05' ELEMENT                       
         LA    R3,NOREPSPT         SET POSSIBLE ERROR CODE                      
DISP0040 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    ERROR               YES                                          
         CLI   0(RF),5             SPOTPAK INTERFACE CODES?                     
         BE    DISP0060            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     DISP0040            GO BACK FOR NEXT                             
DISP0060 EQU   *                                                                
         MVC   SPIFAGCD,RREPSPPC-RREPSPOT(RF)                                   
*                                  INSERT SPOTPAK AGENCY CODE                   
         MVC   SPIFSMED,RREPMED-RREPSPOT(RF)                                    
*                                  INSERT MEDIA CODE                            
*                                                                               
*   RETRIEVE PRODUCT RECORD TO GET SPOT CODES                                   
*                                                                               
         MVI   RPRDKTYP,09         PRODUCT RECORD TYPE                          
         MVC   RPRDKREP,REPALPHA   REP CODE                                     
         MVC   RPRDKADV,RCONKADV   INSERT ADV CODE FROM CON REC                 
         LA    R3,NOPRODCD         NO PRODUCT CODE ERROR MESSAGE                
         OC    RCONPRD,RCONPRD     ANY PRODUCT CODE?                            
         BZ    ERROR               NO  - DON'T PROCESS                          
         CLC   RCONPRD,MYSPACES    ANY PRODUCT CODE?                            
         BE    ERROR               NO  - DON'T PROCESS                          
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE INTO KEY                 
*                                                                               
*   TEST                                                                        
***>>>   MVC   RPRDKPRD,=C'001'    INSERT PRODUCT CODE INTO KEY                 
*   TEST END                                                                    
*                                                                               
         MVC   KEY(27),RPRDKEY     MOVE SETUP KEY TO KEY                        
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                KEY NOT FOUND                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  READ PRODUCT RECORD INTO IOAREA              
         LA    RF,RPRDELEM         FIND PRODUCT X'03' ELEMENT                   
         LA    R3,NOSPOTPK         SET POSSIBLE ERROR                           
DISP0080 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    ERROR               YES - NO X'03' ELEMENT                       
         CLI   0(RF),3             SPOTPAK INTERFACE ELEMENT?                   
         BE    DISP0100            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     DISP0080            GO BACK FOR NEXT                             
DISP0100 EQU   *                                                                
         MVC   SPIFSCLT(6),RPRDSPCL-RPRDSPOT(RF)                                
*                                  MOVE CLIENT/PRODUCT CODES                    
         MVC   SPIFEST#,RPRDSPES-RPRDSPOT(RF)                                   
*                                  MOVE ESTIMATE NUMBER                         
*                                                                               
*                                                                               
DISP0120 EQU   *                                                                
         MVI   RBUYKTYP,11         BUY REC TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  RF                                                               
*                                                                               
         CLC   =C'RESET',8(R2)     RTS RESET OF ALL BUYS?                       
         BE    DISP0140                                                         
         CLC   =C'ALL',8(R2)       RTS ALL BUYS?                                
         BNE   DISP0160                                                         
DISP0140 EQU   *                                                                
         LA    R0,1                                                             
         MVC   PATCH(2),=X'FF01'   SET RANGE FROM 1 TO 255                      
         B     DISP0180                                                         
*                                                                               
DISP0160 DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),(1,WORK2),0                                    
         TM    WORK2+2,X'80'       FIRST LINE # NUMERIC?                        
         BNO   ERROR               NO  - ERROR                                  
         ZIC   R0,WORK2+7          TAKE FIRST LINE NUMBER                       
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,PATCH            SET LAST LINE # TO FIRST                     
         STC   R0,PATCH+1          SET RANGE COUNTER                            
         OC    WORK2+1(1),WORK2+1  ANY SECOND VALUE?                            
         BZ    DISP0180            NO  - PROCEED                                
         TM    WORK2+3,X'80'       2ND LINE # NUMERIC?                          
         BNO   ERROR               NO  - ERROR                                  
         MVC   PATCH(1),WORK2+11   YES - TAKE 2ND LINE NUMBER                   
*                                                                               
DISP0180 DS    0H                                                               
         CH    R0,=H'254'                                                       
         BH    ERROR                                                            
         MVC   KEY,RBUYREC                                                      
         STC   R0,RBUYKLIN         LINE NUMBER                                  
*                                                                               
         GOTO1 VHIGH                                                            
         B     DISP0220                                                         
DISP0200 GOTO1 VSEQ                                                             
DISP0220 CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   DISP0360            FINISHED                                     
         CLC   KEY+26(1),PATCH     WITHIN RANGE?                                
         BH    DISP0360            NO  - HIGH - FINISHED                        
         LR    RF,RA               DIRECTORY RECORD FOUND                       
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWABADDR,KEY+28     SAVE DISK ADDR                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         XI    RBUYRTS,X'80'       FLIP/FLOP BIT FOR REWRITE                    
         LA    R2,CONBNUMH                                                      
         CLC   =C'ALLCLR',8(R2)    RESET TRANSFER CODES AND TARGET              
*                                     REP CODE?                                 
         BNE   DISP0280            NO                                           
         LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
DISP0240 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    DISP0340            YES - NO X'08' - REWRITE RECORD              
         CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
         BE    DISP0260            YES - PROCESS IT                             
         ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,R0                                                            
         B     DISP0240            GO BACK FOR NEXT ELEMENT                     
DISP0260 EQU   *                                                                
         USING RBUYSPEL,R1                                                      
         XC    RBUYSPL#,RBUYSPL#   ERASE SPOTPAK BUY NUMBER                     
         XC    RBUYSPDT,RBUYSPDT   ERASE SPOTPAK TRANSFER DATE                  
         XC    RBUYSPTM,RBUYSPTM   ERASE SPOTPAK TRANSFER TIME                  
         LR    R7,RA               SET ADDRESSABILITY                           
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         MVC   RBUYSPAG,TWASPAG    RESET AGENCY POWER CODE                      
         DROP  R1,R7                                                            
         B     DISP0296                                                         
DISP0280 EQU   *                                                                
         CLC   =C'RESET',8(R2)     DELETE X'08' ELEMENT AND REBUILD?            
         BNE   DISP0296            NO                                           
         LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
DISP0284 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    DISP0340            YES - NO X'08' - REWRITE RECORD              
         CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
         BE    DISP0288            YES - PROCESS IT                             
         ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,R0                                                            
         B     DISP0284            GO BACK FOR NEXT ELEMENT                     
DISP0288 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'08',RBUYREC),0,0                
*                                  DELETE OLD SPOTPAK EST ELEMENT               
         B     DISP0320            GO INSERT NEW X'08' ELEMENT                  
DISP0296 EQU   *                                                                
         CLC   =C'ALLSEED',8(R2)   SEED ALL BUYS IF NO TRANSFER ELT?            
         BNE   DISP0340            NO  - REWRITE THIS RECORD                    
         LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
DISP0300 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    DISP0320            YES - NO X'08' - ADD NEW X'08'               
         CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
         BE    DISP0340            YES - REWRITE RECORD W/NO NEW ELT            
         ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,R0                                                            
         B     DISP0300            GO BACK FOR NEXT ELEMENT                     
DISP0320 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,SPIFELT                                    
*                                  ADD NEW X'08' ELEMENT                        
DISP0340 EQU   *                                                                
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
         B     DISP0200            GO BACK FOR NEXT                             
DISP0360 B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   ESTCHG:  USES OLD AND NEW ESTIMATE NUMBERS TO CHANGE THE FIELD              
*     IN THE X'08' ELEMENT (RTS ELEMENT) OF ALL BUYS IN THE CONTRACT            
*                                                                               
ESTERR   EQU   376                                                              
*                                                                               
ESTCHG   NTR1                                                                   
         LA    R2,CONBNUMH                                                      
         LA    R1,CONBNUM+2        CHECK ALL SIX POSITIONS                      
         LA    R0,6                LOOP CONTROL                                 
         LA    R3,ESTERR                                                        
ESTC0010 EQU   *                                                                
         CLI   0(R1),C'0'                                                       
         BL    ERROR                                                            
         CLI   0(R1),C'9'                                                       
         BH    ERROR                                                            
         LA    R1,1(R1)            BUMP TO NEXT POSITION                        
         BCT   R0,ESTC0010         GO BACK FOR NEXT                             
*                                  ALL CHECKED AND OKAY                         
         PACK  WORK(8),CONBNUM+2(3)                                             
*                                  PACK ORIGINAL ESTIMATE NUMBER                
         CVB   R2,WORK             CONVERT IT TO BINARY                         
         PACK  WORK(8),CONBNUM+5(3)                                             
*                                  PACK NEW      ESTIMATE NUMBER                
         CVB   R3,WORK             CONVERT IT TO BINARY                         
         STC   R2,WORK                                                          
         STC   R3,WORK+1                                                        
*                                                                               
*   SET UP THE BUY KEY FOR THIS CONTRACT                                        
*                                                                               
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,11         BUY REC TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=H'4096'         LOCATE THE TWA                               
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    CONTRACT NUMBER                              
         MVC   KEY,RBUYREC         LOAD KEY FOR READ                            
         DROP  RF                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         B     ESTC0030                                                         
*                                                                               
ESTC0020 GOTO1 VSEQ                                                             
*                                                                               
ESTC0030 CLC   KEY(22),KEYSAVE     SAME CONTRACT?                               
         BNE   EXXMOD              NO  - FINISHED                               
         LR    RF,RA               YES - PROCESS THE BUYLINE                    
         AH    RF,=H'4096'         LOCATE THE TWA                               
         USING TWAWORK,RF                                                       
         MVC   TWABADDR,KEY+28     SAVE DISK ADDR                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
ESTC0040 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    ESTC0020            YES - NO X'08' - GET NEXT RECORD             
         CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
         BE    ESTC0050            YES - PROCESS IT                             
         ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,R0                                                            
         B     ESTC0040            GO BACK FOR NEXT ELEMENT                     
ESTC0050 EQU   *                                                                
         USING RBUYSPEL,R1                                                      
         CLC   RBUYSPES,WORK       REC HAS OLD EST#?                            
         BNE   ESTC0020            NO  - GET NEXT RECORD                        
         MVC   RBUYSPES,WORK+1     YES - REPLACE IT WITH NEW                    
         XC    RBUYSPL#,RBUYSPL#   RESET SPOT BUY # TO ZERO                     
         XC    RBUYSPDT(7),RBUYSPDT RESET TRANSFER DATE/TIME                    
         DROP  R1                                                               
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
         B     ESTC0020            GO BACK FOR NEXT RECORD                      
         EJECT                                                                  
*                                                                               
* XFERPURG:  WIPE OUT TRANSFER INFORMATION IN X'08' ELEMENT.                    
*                                                                               
XFERPURG EQU   *                                                                
         LA    R3,BUYERR                                                        
*                                                                               
         MVI   RBUYKTYP,11         BUY REC TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=H'4096'         4K                                           
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 XFERPACK            PACK BUYLINE NUMBER                          
         L     R0,DUB              LOAD RETURNED VALUE                          
         LTR   R0,R0               ANY VALUE RETURNED?                          
         BZ    ERROR               NO  - ERROR                                  
*                                                                               
         CH    R0,=H'254'                                                       
         BH    ERROR                                                            
         MVC   KEY,RBUYREC                                                      
         STC   R0,RBUYKLIN         LINE NUMBER                                  
*                                                                               
         GOTO1 VHIGH                                                            
         B     XFER0020                                                         
XFER0010 GOTO1 VSEQ                                                             
XFER0020 CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   ERROR                                                            
         CLC   KEY+26(1),RBUYKLIN                                               
         BNE   XFER0010                                                         
* REPDIR REC FOUND                                                              
         LR    RF,RA                                                            
         AH    RF,=H'4096'         4K                                           
         USING TWAWORK,RF                                                       
         MVC   TWABADDR,KEY+28     SAVE DISK ADDR                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
XFER0030 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    XFER0050            YES - NO X'08' - DON'T UPDATE                
         CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
         BE    XFER0040            YES - PROCESS IT                             
         ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,R0                                                            
         B     XFER0030            GO BACK FOR NEXT ELEMENT                     
XFER0040 EQU   *                                                                
         USING RBUYSPEL,R1                                                      
         XC    RBUYSPL#,RBUYSPL#   ERASE SPOTPAK BUY NUMBER                     
         XC    RBUYSPDT,RBUYSPDT   ERASE SPOTPAK TRANSFER DATE                  
         XC    RBUYSPTM,RBUYSPTM   ERASE SPOTPAK TRANSFER TIME                  
         DROP  R1                                                               
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
         SPACE 1                                                                
XFER0050 B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   XFERPACK:  PACKS NUMBER IN FIELD ADDRESSED BY 10(R2), AFTER SUB-            
*     TRACTING 2 FROM LENGTH FOR 'X=' INDICATOR.                                
*   NOTE:  FOR SOME UNKNOWN REASON, THE SCREEN LOADER IN THE BASE               
*     FORCES THE SIZE OF THE BUY NUMBER FIELD TO '3'.  THIS ELIMINATES          
*     THE ACTUAL SIZE OF THE FIELD.  THEREFORE, THE MAXIMUM SIZE OF             
*     THE FIELD IS USED TO DETERMINE THE ACTUAL CONTENTS OF THE                 
*     FIELD.  THE LENGTH FIELD IS ONLY USED TO DETERMINE IF THE                 
*     FIELD CONTAINS ANYTHING AT ALL!                                           
*                                                                               
XFERPACK NTR1                                                                   
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)            GET LENGTH                                   
         LTR   R1,R1               ANY VALUE?                                   
         BZ    XFERPKEX            NO  - ERROR                                  
         BCTR  R1,0                SUBTRACT LENGTH OF 'X='                      
         BCTR  R1,0                                                             
         LTR   R1,R1               ANY VALUE REMAINING?                         
         BNP   XFERPKEX            NO  - ERROR                                  
*                                                                               
*   NOW SCAN FIELD BACKWARD LOOKING FOR 1ST NON-SPACE/X'00' POSIT.              
*     BECAUSE OF ABOVE TESTS, THERE SHOULD BE AT LEAST 1.                       
*                                                                               
         ZIC   RF,0(R2)            LENGTH OF FIELD                              
         SH    RF,=H'8'            SUBTRACT L(HEADER)                           
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES - SUBTRACT L(EXT HDR)                    
         LR    RE,R2               SET RE = A(HEADER FIELD)                     
         LA    RE,8(RE)            BUMP TO DATA                                 
         AR    RE,RF               ADD L(DATA FIELD)                            
         BCTR  RE,0                BACK UP TO LAST POSIT                        
         BCTR  RF,0                SUBTRACT LENGTH OF                           
         BCTR  RF,0                   'X=' FIELD                                
XFPA0010 EQU   *                                                                
         CLI   0(RE),X'00'         X'00' IN POSITION?                           
         BE    XFPA0020            YES                                          
         CLI   0(RE),X'40'         SPACE IN POSITION?                           
         BNE   XFPA0030            NO                                           
XFPA0020 EQU   *                                                                
         BCTR  RE,0                BACK UP ONE POSITION                         
         BCT   RF,XFPA0010         GO BACK FOR NEXT                             
         DC    H'0'                SHOULD NEVER HAPPEN!                         
*                                                                               
*   RF AT THIS POINT HAS THE LENGTH OF THE REMAINING CHARACTERS                 
*      IN THE BUY-NUMBER FIELD                                                  
*                                                                               
XFPA0030 EQU   *                   TEST EACH BYTE FOR NUMERIC                   
         LR    R1,RF               SAVE LENGTH OF INPUT                         
         LA    RE,10(R2)           POINT TO 1ST BYTE OF LINE NUMBER             
XFPA0040 EQU   *                                                                
         CLI   0(RE),C'0'                                                       
         BL    XFERPKEX            < ZERO:  ERROR                               
         CLI   0(RE),C'9'                                                       
         BH    XFERPKEX            > NINE:  ERROR                               
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCT   RF,XFPA0040         GO BACK FOR NEXT                             
         BCTR  R1,0                ADJUST FOR EXEC                              
         EX    R1,PACKIT                                                        
         CVB   R0,DUB              CONVERT TO BINARY                            
         ST    R0,DUB              SAVE FOR PASS-BACK                           
XFERPKEX XIT1                                                                   
*                                                                               
PACKIT   PACK  DUB,10(0,R2)        PACK BUYLINE NUMBER                          
*                                                                               
         EJECT                                                                  
*                                                                               
* DELBUCK:   WIPE OUT ESTIMATE OR INVOICE BUCKET FOR MONTH SPECIFIED            
*                                                                               
DELBUCK  EQU   *                                                                
         MVC   KEY,RCONREC                                                      
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RCONREC                                     
         LA    R3,DDATERR                                                       
*                                                                               
         CLI   CONBNUM+5,C'/'      ENSURE FORMAT = MMM/YY                       
         BNE   ERROR                                                            
         GOTO1 DATVAL,DMCB,(2,CONBNUM+2),TESTDATE                               
         OC    DMCB,DMCB           VALIDATED OKAY?                              
         BZ    ERROR               NO  - EXIT WITH ERROR MESSAGE                
         GOTO1 DATCON,DMCB,(0,TESTDATE),(3,TESTDATE+6)                          
         MVI   BUCKTYPE,3          SET BUCKET TO 'ESTIMATE/BOOKED'              
         CLI   CONBNUM,C'B'        BOOKED DELETE?                               
         BE    DBUK0020            YES                                          
         MVI   BUCKTYPE,4          SET BUCKET TO 'INVOICED'                     
DBUK0020 EQU   *                                                                
         LA    R2,RCONELEM                                                      
DBUK0040 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    DBUK0200            YES - FINISHED                               
         CLC   0(1,R2),BUCKTYPE    TYPE OF BUCKET SOUGHT?                       
         BNE   DBUK0080            YES                                          
         CLC   2(2,R2),TESTDATE+6  MONTH OF SERVICE TO DELETE?                  
         BNE   DBUK0080            NO                                           
         MVI   0(R2),X'FF'         YES - SET TO DELETE                          
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0                
*                                  DELETE BUCKET MARKED                         
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                  REWRITE RECORD                               
         B     DBUK0200            EXIT                                         
DBUK0080 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     DBUK0040            GO BACK FOR NEXT                             
DBUK0200 EQU   *                                                                
         SPACE 1                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
*   LOCAL WORKING STORAGE                                                       
*                                                                               
NOPRODCD EQU   676                                                              
NOSPOTPK EQU   677                                                              
NOREPSPT EQU   678                                                              
DDATERR  EQU   775                 DATE FORMAT ERROR                            
*                                                                               
*   DUMMY X'08' ELEMENT FOR INCLUSION WHEN DOING 'ALLSEED'                      
*                                                                               
SPIFELT  DS    0XL48               SPOTPAK INTERFACE ELEMENT                    
SPIFCDLN DS    XL2      +0         X'0830' CODE/LENGTH                          
SPIFAGCD DS    CL2      +2         SPOTPAK AGENCY CODE                          
SPIFSMED DS    CL1      +4         SPOTPAK MEDIA (FROM REP RECORD)              
SPIFSCLT DS    CL3      +5         SPOTPAK CLIENT                               
SPIFPROD DS    CL3      +8         SPOTPAK PRODUCT CODE                         
SPIFEST# DS    CL1      +11        SPOTPAK ESTIMATE NUMBER                      
         DS    CL6      +12        FILLER                                       
SPIFSTAT DS    CL5      +18        STATION CALL LETTERS                         
SPIFRADV DS    CL4      +23        REPPAK ADVERTISER                            
SPIFRPRD DS    CL3      +27        REPPAK PRODUCT                               
         DS    CL18     +30        FILLER                                       
*                                                                               
TESTDATE DS    CL12                                                             
BUCKTYPE DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049RECNT2F   07/25/01'                                      
         END                                                                    
