*          DATA SET RELDSTRP   AT LEVEL 021 AS OF 05/01/02                      
*PHASE RELDSTRP                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RELDSTRP - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
**********************************************************************          
* HISTORY OF CHANGES                                                 *          
* SEP11/97 (BU ) --- VERSION TO STRIP BANNER/CHRISTAL/EASTMAN RADIO  *          
*                    LOAD TO REPFIL9 TV TEST CODES                   *          
*                                                                    *          
* JUL13/98 (BU ) --- MODIFIED TO STRIP FOX FOR ABC TEST SETUP        *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
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
*******************************************************************             
*                                                                 *             
*   MODIFY ALL REFERENCES TO BF/CR/EA/K3 TO QT/OT/K8/YT           *             
*          BYPASS ALL OTHER RECORDS                               *             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
                                                                                
         LA    R5,CODES                                                         
*                                                                               
* GET DISPLACEMENT TO REP CODE IN RECORD                                        
DMXREC1  CLC   0(1,R3),8(R5)       RECORD TYPE TO TABLE                         
         BE    DMXREC2                                                          
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   DMXREC1                                                          
         B     DMXPURGE            UNKNOWN REC - PURGE                          
                                                                                
DMXREC2  ZIC   R7,9(R5)            DISP TO REPCODE                              
         AR    R7,R3               R7 TO REP CODE IN RECORD                     
*                                                                               
*        MODIFY THE OUTPUT RECORD                                               
*                                                                               
         CLC   =C'FN',0(R7)        FOX TV?                                      
         BNE   DMXPURGE            NO  - KEEP ONLY FTSNY RECORDS                
         MVC   0(2,R7),=C'9S'      YES - CHANGE TO 9S (TEST)                    
         AP    CHANGE,=P'1'                                                     
         AP    10(5,R5),=P'1'                                                   
*                                                                               
DMXR0000 EQU   *                                                                
         CLI   0(R3),X'01'         REP?                                         
         BNE   DMXR0020                                                         
*                                  ALREADY CHANGED:  JUST KEEP IT               
         B     DMXKEEP                                                          
DMXR0020 EQU   *                                                                
         CLI   0(R3),X'02'         STATION?                                     
         BE    DMXR0022                                                         
         CLI   0(R3),X'42'         STATION?                                     
         BNE   DMXR0060                                                         
*                                                                               
DMXR0022 EQU   *                                                                
         USING RSTAREC,R3          YES                                          
         MVI   RSTAGRUP,C'T'       CHANGE GROUP TO 'T'                          
         LA    R2,RSTAKSTA         SET A(STATION CALL LETTERS)                  
         GOTO1 SWAPSTAT,DMCB,(R2),(0,0)                                         
         MVC   RSTAOWN,SAVEOWNR    INSERT NEW OWNER CODE                        
         LA    R4,RSTAELEM         FIND COMPETITIVE ELEMENT                     
DMXR0024 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    DMXR0038            YES                                          
         CLI   0(R4),2             COMPETITIVE STATION ELEMENT?                 
         BE    DMXR0026            YES                                          
DMXR0025 EQU   *                                                                
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     DMXR0024            GO BACK FOR NEXT                             
DMXR0026 EQU   *                                                                
         LR    R7,R4                                                            
         LA    R7,2(R7)            BYPASS ELEMENT CONTROL                       
         GOTO1 SWAPSTAT,DMCB,(R7),(1,0)                                         
*                                  SWAP STATION                                 
         B     DMXR0025            GO BACK FOR NEXT ELEMENT                     
DMXR0038 EQU   *                                                                
         BAS   RE,DISPREC          DISPLAY RECORD                               
         B     DMXKEEP                                                          
         DROP  R3                                                               
DMXR0060 EQU   *                                                                
         CLI   0(R3),X'07'         GROUP?                                       
         BNE   DMXR0080                                                         
*                                                                               
         MVI   RGRPKGRP-RGRPREC(R3),C'T'                                        
*                                  YES - CHANGE TO 'T'                          
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
DMXR0080 EQU   *                                                                
         CLI   0(R3),X'0C'         CONTRACT?                                    
         BNE   DMXR0100                                                         
*                                                                               
         USING RCONREC,R3                                                       
         LA    R2,RCONKSTA         SET A(CONTRACT STATION)                      
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         MVI   RCONKGRP,C'T'       INSERT NEW GROUP CODE                        
         CLI   RCONTYPE,C'X'       CHANGE CONTRACT TYPE, IF NEEDED              
         BNH   DMXR0082            NOT NEEDED                                   
         MVI   RCONTYPE,C'X'       RESET TO 'X'                                 
DMXR0082 EQU   *                                                                
         LA    R4,RCONELEM         FIND COMPETITIVE ELEMENT                     
DMXR0084 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    DMXR0098            YES                                          
         CLI   0(R4),6             SPL ELEMENT?                                 
         BE    DMXR0086            YES                                          
         ZIC   RF,1(R4)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R4,RF                                                            
         B     DMXR0084            GO BACK FOR NEXT                             
DMXR0086 EQU   *                                                                
         USING RCONSPEL,R4                                                      
         ZIC   R0,RCONSPNU         # OF MINI ELEMENTS IN SPL                    
         LA    R2,RCONSPST         A(1ST STATION MINI ELEMENT)                  
DMXR0088 EQU   *                                                                
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         LA    R2,9(R2)            BUMP TO NEXT ELEMENT                         
         BCT   R0,DMXR0088         GO BACK FOR NEXT                             
DMXR0098 EQU   *                                                                
         CLC   CONCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,CONCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         LA    R6,=C'CON'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
CONCTR   DS    F                                                                
DMXR0100 EQU   *                                                                
         CLI   0(R3),X'11'         MAKEGOOD?                                    
         BNE   DMXR0120                                                         
*                                                                               
         USING RMKGREC,R3                                                       
         LA    R2,RMKGKSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   MKGCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,MKGCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,MKGCTR                                                        
         LA    R6,=C'MKG'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
MKGCTR   DS    F                                                                
DMXR0120 EQU   *                                                                
         CLI   0(R3),X'12'         INVENTORY?                                   
         BNE   DMXR0140                                                         
*                                                                               
         USING RINVREC,R3                                                       
         OC    RINVKSTA,RINVKSTA   STATION IN KEY?                              
         BZ    DMXPURGE            NO  - PURGE RECORD                           
         LA    R2,RINVKSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   INVCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,INVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,INVCTR                                                        
         LA    R6,=C'INV'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
INVCTR   DS    F                                                                
DMXR0140 EQU   *                                                                
         CLI   0(R3),X'13'         BUDGET?                                      
         BNE   DMXR0160                                                         
*                                                                               
         USING RBUDREC,R3                                                       
         LA    R2,RBUDKSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   BUDCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,BUDCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUDCTR                                                        
         LA    R6,=C'BUD'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
BUDCTR   DS    F                                                                
DMXR0160 EQU   *                                                                
         CLI   0(R3),X'1B'         EOP 1B?                                      
         BNE   DMXR0180                                                         
*                                                                               
         USING REOPREC,R3                                                       
         LA    R2,REOPKSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   EO1CTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,EO1CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO1CTR                                                        
         LA    R6,=C'EO1'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
EO1CTR   DS    F                                                                
DMXR0180 EQU   *                                                                
         CLI   0(R3),X'1C'         EOP 1C?                                      
         BNE   DMXR0200                                                         
*                                                                               
         USING REO2REC,R3                                                       
         LA    R2,REO2KSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   EO2CTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,EO2CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO2CTR                                                        
         LA    R6,=C'EO2'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
EO2CTR   DS    F                                                                
DMXR0200 EQU   *                                                                
         CLI   0(R3),X'1D'         EOP 1D?                                      
         BNE   DMXR0220                                                         
*                                                                               
         USING REO3REC,R3                                                       
         LA    R2,REO3KSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   EO3CTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,EO3CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO3CTR                                                        
         LA    R6,=C'EO3'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
EO3CTR   DS    F                                                                
DMXR0220 EQU   *                                                                
         CLI   0(R3),X'1E'         EOP 1E?                                      
         BNE   DMXR0240                                                         
*                                                                               
         USING REO4REC,R3                                                       
         LA    R2,REO4KSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   EO4CTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,EO4CTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,EO4CTR                                                        
         LA    R6,=C'EO4'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
EO4CTR   DS    F                                                                
DMXR0240 EQU   *                                                                
         CLI   0(R3),X'29'         COMMISSION?                                  
         BNE   DMXR0250                                                         
*                                                                               
         USING RCOMREC,R3                                                       
         LA    R2,RCOMKSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   COMCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,COMCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,COMCTR                                                        
         LA    R6,=C'COM'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
COMCTR   DS    F                                                                
DMXR0250 EQU   *                                                                
         CLI   0(R3),X'2A'         OWNERSHIP?                                   
         BNE   DMXR0260                                                         
*                                                                               
         MVC   24(3,R3),=C'DDA'    INSERT FALSE OWNERSHIP                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
DMXR0260 EQU   *                                                                
         CLI   0(R3),X'2B'         MARKET?                                      
         BNE   DMXR0280                                                         
*                                                                               
         USING RMKTREC,R3                                                       
         LA    R4,RMKTELEM         FIND STATION ELEMENT                         
         DROP  R3                                                               
DMXR0262 EQU  *                                                                 
         CLI   0(R4),0             END OF RECORD?                               
         BE    DMXR0278            YES                                          
         CLI   0(R4),2             STN ELEMENT?                                 
         BE    DMXR0264            YES                                          
         ZIC   RF,1(R4)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R4,RF                                                            
         B     DMXR0262            GO BACK FOR NEXT                             
DMXR0264 EQU  *                                                                 
         USING RMKTSTEL,R4                                                      
         LA    R2,RMKTSTAT         A(1ST STATION MINI ELEMENT)                  
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         DROP  R4                                                               
DMXR0278 EQU  *                                                                 
         CLC   MKTCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,MKTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,MKTCTR                                                        
         LA    R6,=C'MKT'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
MKTCTR   DS    F                                                                
DMXR0280 EQU   *                                                                
         CLI   0(R3),X'2C'         AUR?                                         
         BNE   DMXR0300                                                         
*                                                                               
         USING RAURREC,R3                                                       
         LA    R2,RAURKSTA         SET STATION CALL LETTERS                     
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         MVI   RAURKGRP,C'T'       SET GROUP TO 'T'                             
         CLC   AURCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,AURCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AURCTR                                                        
         LA    R6,=C'AUR'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
AURCTR   DS    F                                                                
DMXR0300 EQU   *                                                                
         CLI   0(R3),X'2E'         COMMENT?  IF YES, DROP IT                    
         BNE   DMXR0320                                                         
*                                                                               
         B     DMXPURGE            DROP IT                                      
DMXR0320 EQU   *                                                                
         CLI   0(R3),X'32'         CONTRACT TYPE?                               
         BNE   DMXR0340                                                         
*                                                                               
         USING RCTYREC,R3                                                       
         CLI   RCTYKCTY,C'X'                                                    
         BH    DMXPURGE            IF AFTER 'X', DROP IT                        
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
DMXR0340 EQU   *                                                                
         CLI   0(R3),X'34'         OFFICE COMMENT?  IF YES, DROP IT             
         BNE   DMXR0360                                                         
*                                                                               
         B     DMXPURGE                                                         
DMXR0360 EQU   *                                                                
         CLI   0(R3),X'38'         SET?                                         
         BNE   DMXR0380                                                         
*                                                                               
         USING RSETREC,R3                                                       
         CLC   =C'ST',RSETKSET     STATION SET RECORD?                          
         BNE   DMXKEEP             NO                                           
         LA    R4,RSETELEM         FIND STATION ELEMENT                         
         DROP  R3                                                               
DMXR0362 EQU  *                                                                 
         CLI   0(R4),0             END OF RECORD?                               
         BE    DMXR0378            YES                                          
         CLI   0(R4),X'20'         STN ELEMENT?                                 
         BE    DMXR0364            YES                                          
         ZIC   RF,1(R4)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R4,RF                                                            
         B     DMXR0362            GO BACK FOR NEXT                             
DMXR0364 EQU  *                                                                 
         ZIC   RF,1(R4)            CALC NUMBER OF ENTRIES                       
         SH    RF,=H'3'            SUBTRACT CONTROL                             
         SR    RE,RE                                                            
         D     RE,=F'5'            DIVIDE BY SIZE OF ENTRY                      
         LR    R0,RF               SET LOOP CONTROL                             
         LA    R4,3(R4)            SKIP OVER CONTROL INFO                       
DMXR0366 EQU  *                                                                 
         GOTO1 SWAPSTAT,DMCB,(R4),(1,0)                                         
         LA    R4,5(R4)            BUMP TO NEXT ELEMENT                         
         BCT   R0,DMXR0366         GO BACK FOR NEXT                             
DMXR0378 EQU  *                                                                 
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
DMXR0380 EQU   *                                                                
         CLI   0(R3),X'41'         DARE?                                        
         BE    DMXR0382                                                         
         CLI   0(R3),X'51'         DARE HISTORICAL?                             
         BNE   DMXR0420                                                         
DMXR0382 EQU   *                                                                
*                                                                               
         USING RDARREC,R3                                                       
         LA    R2,RDARKSTA         SET A(STATION CALLS)                         
         GOTO1 SWAPSTAT,DMCB,(R2),(1,0)                                         
         CLC   DARCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,DARCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,DARCTR                                                        
         LA    R6,=C'DAR'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
DARCTR   DS    F                                                                
DMXR0420 EQU   *                                                                
         CLI   0(R3),X'43'         PROPOSAL (SWS)?                              
         BNE   DMXR0440                                                         
*                                                                               
         LA    R4,34(R3)           FIND PASSIVE KEY ELEMENT                     
DMXR0422 EQU  *                                                                 
         CLI   0(R4),0             END OF RECORD?                               
         BE    DMXR0438            YES                                          
         CLI   0(R4),2             PASSIVE KEY ELEMENT?                         
         BE    DMXR0424            YES                                          
         CLI   0(R4),X'30'         STATION ELEMENT?                             
         BE    DMXR0426            YES                                          
DMXR0423 EQU  *                                                                 
         ZIC   RF,1(R4)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R4,RF                                                            
         B     DMXR0422            GO BACK FOR NEXT                             
DMXR0424 EQU  *                                                                 
         LR    R5,R4                                                            
         LA    R5,16(R5)           SET A(STATION)                               
         GOTO1 SWAPSTAT,DMCB,(R5),(1,0)                                         
         LA    R5,15(R5)           SET A(GROUP/SUBGROUP)                        
         MVI   0(R5),C'T'          INSERT GROUP 'T'                             
         B     DMXR0423            GO BACK FOR NEXT                             
DMXR0426 EQU  *                                                                 
         LR    R5,R4                                                            
         LA    R5,09(R5)           SET A(STATION)                               
         GOTO1 SWAPSTAT,DMCB,(R5),(1,0)                                         
         B     DMXR0423            GO BACK FOR NEXT                             
DMXR0438 EQU  *                                                                 
         CLC   PROCTR,=F'12'       FIRST 12  DISPLAYED                          
         BH    DMXKEEP                                                          
         L     RF,PROCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PROCTR                                                        
         LA    R6,=C'PRO'                                                       
         BAS   RE,DISPREC                                                       
         B     DMXKEEP                                                          
PROCTR   DS    F                                                                
                                                                                
DMXR0440 EQU   *                                                                
*                                                                               
***      BAS   RE,DISPREC2                                                      
         B     DMXKEEP                                                          
*                                     KEEP ALL OTHER RECORDS                    
*   SWAPSTAT  -  FINDS STATION IN TABLE, INSERTS AT (R2), WHERE                 
*        CALLS ARE TO BE SWITCHED                                               
*                                                                               
SWAPSTAT NTR1                                                                   
         L     R2,0(R1)                                                         
         MVC   COMPSTAT,4(R1)      SAVE COMPSTAT FLAG                           
         LA    R6,STATABLE                                                      
SSTA0020 EQU   *                                                                
         CLI   0(R6),0             END OF TABLE?                                
         BNE   SSTA0030            NO                                           
         CLI   COMPSTAT,0          REQUIRES A FIND?                             
         BNE   SSTA0060                                                         
*                                     FOR DISPLAY                               
         LA    R6,=C'NST'                                                       
         BAS   RE,DISPREC                                                       
         B     SSTA0060            EXIT NOT FOUND                               
SSTA0030 EQU   *                                                                
         CLC   0(4,R6),0(R2)       STATION IN TABLE?                            
         BE    SSTA0040            YES                                          
         LA    R6,LSTATABL(R6)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     SSTA0020            GO BACK FOR NEXT                             
SSTA0040 EQU   *                                                                
         MVC   0(4,R2),4(R6)       INSERT NEW STATION CALLS                     
         CLI   COMPSTAT,0          COMPETITIVE (SPL LOOKUP)?                    
         BNE   SSTA0060            YES - DON'T RESET OWNER                      
         MVC   SAVEOWNR,8(R6)      SAVE OWNER, IF NEEDED                        
SSTA0060 EQU   *                                                                
         XIT1                                                                   
SAVEOWNR DS    CL3                 OWNER CODE                                   
COMPSTAT DS    CL1                                                              
         DS    0H                  REALIGNMENT                                  
         EJECT                                                                  
*                                                                               
*   DISPLAY OUTPUT RECORD - USES RCONREC DSECT                                  
*                                                                               
DISPREC  NTR1                                                                   
         USING RCONREC,R3                                                       
         MVC   HALF,RCONLEN                                                     
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         DROP  R3                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DISPLAY OUTPUT RECORD - USES RCONREC DSECT                                  
*                                                                               
DISPREC2 NTR1                                                                   
         MVC   HALF,=H'32'                                                      
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE 1                                                                
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(33),=C'DETAILS OF PURGED RECORDS FOLLOWS'                    
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,CODES                                                         
DC10     MVC   P+3(8),0(R5)                                                     
         EDIT  (P5,10(R5)),(7,P+13)                                             
         GOTO1 VPRINTER                                                         
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   DC10                                                             
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CODES    DS    0CL15                                                            
         DC    CL8'REP     ',XL1'01',AL1(RREPKREP-RREPREC),PL5'0'               
         DC    CL8'STATION ',XL1'02',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    CL8'STATION2',XL1'42',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    CL8'REGION  ',XL1'03',AL1(RREGKREP-RREGREC),PL5'0'               
         DC    CL8'OFFICE  ',XL1'04',AL1(ROFFKREP-ROFFREC),PL5'0'               
         DC    CL8'OFFICE2 ',XL1'44',AL1(ROFFKREP-ROFFREC),PL5'0'               
         DC    CL8'DIVISION',XL1'05',AL1(RTEMKREP-RTEMREC),PL5'0'               
         DC    CL8'MAN     ',XL1'06',AL1(RSALKREP-RSALREC),PL5'0'               
         DC    CL8'MAN2    ',XL1'46',AL1(RSALKREP-RSALREC),PL5'0'               
         DC    CL8'GROUP   ',XL1'07',AL1(RGRPKREP-RGRPREC),PL5'0'               
         DC    CL8'ADVERTIS',XL1'08',AL1(RADVKREP-RADVREC),PL5'0'               
         DC    CL8'PRODUCT ',XL1'09',AL1(RPRDKREP-RPRDREC),PL5'0'               
         DC    CL8'AGENCY  ',XL1'0A',AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    CL8'AGENCY2 ',XL1'1A',AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    CL8'BUY     ',XL1'0B',AL1(RBUYKREP-RBUYREC),PL5'0'               
         DC    CL8'CONTRACT',XL1'0C',AL1(RCONKREP-RCONREC),PL5'0'               
         DC    CL8'CLASS   ',XL1'0D',AL1(RCLSKREP-RCLSREC),PL5'0'               
         DC    CL8'EDI     ',XL1'0E',AL1(14),PL5'0'                             
         DC    CL8'CATEGORY',XL1'0F',AL1(RCTGKREP-RCTGREC),PL5'0'               
         DC    CL8'MKGD OFF',XL1'11',AL1(06),PL5'0'                             
         DC    CL8'INVENTRY',XL1'12',AL1(RINVKREP-RINVREC),PL5'0'               
         DC    CL8'BUDGET  ',XL1'13',AL1(RBUDKREP-RBUDREC),PL5'0'               
         DC    CL8'AVAIL   ',XL1'14',AL1(RAVLKREP-RAVLREC),PL5'0'               
         DC    CL8'SECURITY',XL1'15',AL1(15),PL5'0'                             
         DC    CL8'PROPOSAL',XL1'16',AL1(RPRPKREP-RPRPREC),PL5'0'               
         DC    CL8'EOM     ',XL1'18',AL1(REOMKREP-REOMREC),PL5'0'               
         DC    CL8'OFF BUD ',XL1'19',AL1(17),PL5'0'                             
         DC    CL8'EOP ADV ',XL1'1B',AL1(15),PL5'0'                             
         DC    CL8'EOP AGY ',XL1'1C',AL1(13),PL5'0'                             
         DC    CL8'EOP OFF ',XL1'1D',AL1(17),PL5'0'                             
         DC    CL8'EOP SAL ',XL1'1E',AL1(16),PL5'0'                             
         DC    CL8'TKO EQU ',XL1'1F',AL1(16),PL5'0'                             
         DC    CL8'ALT CAL ',XL1'20',AL1(01),PL5'0'                             
         DC    CL8'STN CTRL',XL1'21',AL1(15),PL5'0'                             
         DC    CL8'OVR UPLD',XL1'22',AL1(13),PL5'0'                             
         DC    CL8'DEMOMENU',XL1'23',AL1(RDEMKREP-RDEMREC),PL5'0'               
         DC    CL8'DAYPART ',XL1'24',AL1(24),PL5'0'                             
         DC    CL8'PRG TYPE',XL1'25',AL1(24),PL5'0'                             
         DC    CL8'SDD     ',XL1'26',AL1(20),PL5'0'                             
         DC    CL8'ATHENA  ',XL1'27',AL1(01),PL5'0'                             
         DC    CL8'SWITCH  ',XL1'28',AL1(13),PL5'0'                             
         DC    CL8'CMISSION',XL1'29',AL1(11),PL5'0'                             
         DC    CL8'OWNRSHIP',XL1'2A',AL1(22),PL5'0'                             
         DC    CL8'MARKET  ',XL1'2B',AL1(21),PL5'0'                             
         DC    CL8'AUR     ',XL1'2C',AL1(04),PL5'0'                             
         DC    CL8'SBB     ',XL1'2D',AL1(12),PL5'0'                             
         DC    CL8'COMMENT ',XL1'2E',AL1(15),PL5'0'                             
         DC    CL8'TYPE    ',XL1'30',AL1(17),PL5'0'                             
         DC    CL8'PT PRSN ',XL1'31',AL1(22),PL5'0'                             
         DC    CL8'K TYPE  ',XL1'32',AL1(24),PL5'0'                             
         DC    CL8'RADAR   ',XL1'33',AL1(17),PL5'0'                             
         DC    CL8'OCM     ',XL1'34',AL1(20),PL5'0'                             
         DC    CL8'DIRESPON',XL1'35',AL1(13),PL5'0'                             
         DC    CL8'LABEL   ',XL1'36',AL1(17),PL5'0'                             
         DC    CL8'GOAL    ',XL1'37',AL1(13),PL5'0'                             
         DC    CL8'SET     ',XL1'38',AL1(19),PL5'0'                             
         DC    CL8'STRATEGY',XL1'39',AL1(13),PL5'0'                             
         DC    CL8'DEV SAL ',XL1'3A',AL1(22),PL5'0'                             
         DC    CL8'DEV K TP',XL1'3B',AL1(23),PL5'0'                             
         DC    CL8'RSRCH DP',XL1'3C',AL1(24),PL5'0'                             
         DC    CL8'TERRITOR',XL1'3D',AL1(23),PL5'0'                             
         DC    CL8'GEN AVAL',XL1'3E',AL1(10),PL5'0'                             
         DC    CL8'DARE    ',XL1'41',AL1(07),PL5'0'                             
         DC    CL8'PROPOSAL',XL1'43',AL1(07),PL5'0'                             
         DC    CL8'SCRIBE  ',XL1'45',AL1(03),PL5'0'                             
         DC    CL8'CFC     ',XL1'47',AL1(21),PL5'0'                             
         DC    CL8'CVRSHEET',XL1'48',AL1(16),PL5'0'                             
         DC    CL8'CVRSHET2',XL1'49',AL1(16),PL5'0'                             
         DC    CL8'BUS ACT ',XL1'4A',AL1(17),PL5'0'                             
         DC    CL8'DARE 2  ',XL1'51',AL1(07),PL5'0'                             
         DC    CL8'EDI PERF',XL1'52',AL1(20),PL5'0'                             
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*   STATION RENAME TABLE:                                                       
*        POSITIONS   1  -  4  =  ORIGINAL STATION LETTERS                       
*        POSITIONS   5  -  8  =  REPLACEMENT LETTERS                            
*        POSITIONS   9  - 11  =  OWNERSHIP REPLACEMENT                          
*                                                                               
STATABLE EQU   *                                                                
         DC    C'KDFIKAAADDA'                                                   
LSTATABL EQU   *-STATABLE          L(STATABLE ENTRY)                            
         DC    C'KDFWKBBBDDB'                                                   
         DC    C'KDVRKCCCDDC'                                                   
         DC    C'KNSDKDDDDDD'                                                   
         DC    C'KRIVKEEEDDE'                                                   
         DC    C'KSAZKFFFDDA'                                                   
         DC    C'KSTUKGGGDDB'                                                   
         DC    C'KTBCKHHHDDC'                                                   
         DC    C'KTTVKIIIDDD'                                                   
         DC    C'KTVIKJJJDDE'                                                   
         DC    C'KVC KKKKDDA'                                                   
         DC    C'KDAFKLLLDDB'                                                   
         DC    C'WAGAWAAADDC'                                                   
         DC    C'WBRCWBBBDDD'                                                   
         DC    C'WDAFWCCCDDE'                                                   
         DC    C'WFLDWDDDDDA'                                                   
         DC    C'WFXTWEEEDDB'                                                   
         DC    C'WGHPWFFFDDC'                                                   
         DC    C'WHBQWGGGDDD'                                                   
         DC    C'WITIWHHHDDE'                                                   
         DC    C'WJBKWIIIDDA'                                                   
         DC    C'WJW WJJJDDB'                                                   
         DC    C'WNYWWWORDDC'                                                   
         DC    C'WTTGWLLLDDD'                                                   
         DC    C'WTVTWMMMDDE'                                                   
         DC    C'WTXFWNNNDDA'                                                   
         DC    C'WVTMWOOODDB'                                                   
         DC    X'0000'                                                          
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
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
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE REGENEOP                                                       
       ++INCLUDE REGENCOM                                                       
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE REGENAUR                                                       
       ++INCLUDE REGENCTY                                                       
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021RELDSTRP  05/01/02'                                      
         END                                                                    
