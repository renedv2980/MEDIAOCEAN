*          DATA SET RECNT49    AT LEVEL 202 AS OF 05/04/18                      
*PHASE T80249A                                                                  
         TITLE 'T80249 - REP ORDER DISPLAY/EDIT'                                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT49 (T80249) --- ORDER DISPLAY/EDIT                  *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 26MAR90 (EFJ) --- ADDED TOMBSTONE (HISTORY LOST)                *             
*                                                                 *             
* CNT HISTORY:                                                    *             
*                                                                 *             
* 05APR90 (EFJ) --- USE GETEL MACRO                               *             
*                                                                 *             
* 03AUG90 (EFJ) --- COMBINE WITH CNT48                            *             
*                                                                 *             
* 15MAY91 (EFJ) --- CHANGE 'WU' TO 'ESL' PER KARI                 *             
*                                                                 *             
* 20NOV91 (SKU) --- CLEAR ORD/STA COMMENT FIELDS BEFORE REDISPLAY *             
*                                                                 *             
* 14DEC91 (SKU) --- SUPPORT FOR STORE COMMENTS                    *             
*                                                                 *             
* 21SEP93 (SKU) --- DELETE OPPOSITE STA/REP ORD COMMENTS WHEN     *             
*                   VERSION HAS INCREMENTED                       *             
*                                                                 *             
* 12OCT93 (SKU) --- IF EC, COPY STA ADV AND AGY TO BIAS OR JDS    *             
*                   EC ELEMENTS X'13'                             *             
*                                                                 *             
* 30MAR94 (BU ) --- FIX ABORT WHEN ADVERTISER IS DELETED.         *             
*                                                                 *             
* 18OCT94 (SKU) --- EXPAND X'20' SEND ELEMENT TO SAVE LAST 3      *             
*                   REP/STA VERSION DATES                         *             
*                                                                 *             
* 29JAN95 (SKU) --- SUPPORT MULTIPLE STATION SENDS BY CALLING     *             
*                   REGENVER AND BUMP VERSION NUMBERS             *             
*                   SHORTEN X'9F' ELEMENT BY REMOVING SPARES      *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 08MAR96 (RHV) --- ALLOW ENTRY OF TRAFFIC# IN CONACT FIELD       *             
*                                                                 *             
* 10MAY96 (SKU) --- OPEN UP EOP CODES FOR ALL FORMATS K AND C     *             
*                   SUPPORT FOR EOP SAL AND OFF FIELDS            *             
*                                                                 *             
* 17JUL96 (SKU) --- UPDATE EOP ALL THE TIME FOR AGY AND ADV       *             
*                                                                 *             
* 04MAR97 (RHV) --- SUPPORT FULL SIZE ORD SCREEN                  *             
*                                                                 *             
* 17JUN97 (SKU) --- SUPPORT FOR 'REMOVED' DARE ORDERS             *             
*                                                                 *             
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
*                                                                 *             
* 26AUG97 (SKU) --- REGENVER BUG FIX                              *             
*                                                                 *             
* 15OCT97 (SKU) --- AUTO ORD TOTAL FOR ALL DARE EXCEPT FOR REMOVED*             
*                                                                 *             
* 30DEC97 (RHV) --- ORD=NNNNNNNN                                  *             
*                                                                 *             
* 26JAN99 (RHV) --- CORRECT STATION LOCKOUT BUG                   *             
*                                                                 *             
* 11JAN00 (SKU & SCH) - EOP BUG FIX                               *             
*                                                                 *             
* 11DEC00 (BU ) --- UPGRADE FOR TRADE AGENCY CODING               *             
*                                                                 *             
* 25OCT01 (RHV) --- EOP FOR VCI                                   *             
*                                                                 *             
* 15APR02 (HQ ) --- SALES ASSISTANT FOR ORD SCREEN                *             
*                                                                 *             
* 11NOV02 (JRD) --- SALES ASSISTANT EMAIL FOR ORD SCREEN          *             
*                                                                 *             
* 11NOV02 (HQ ) --- ADD SALES ASSISTANT TO CONTRACT RECORD        *             
*                                                                 *             
* 14JAN03 (BU ) --- ADD PASSIVES FOR EZPOST                       *             
*                                                                 *             
* 13AUG10 (SKU) --- SCRIPT UPLOAD SUPPORT                         *             
*                                                                 *             
* 13AUG12 (KWA) --- MQ MESSAGE FOR TRAFFIC ORDER# UPDATE          *             
*                                                                 *             
* 22JUL13 (KWA) --- VALIDATE E-MAIL ADDRESS                       *             
*                                                                 *             
* 03MAY18 (SKU) --- SPEC-23635: CHECK FOR DUPLICATE TRAFFIC ORDER *             
*                   ASSIGNMENTS                                   *             
*                   ***  END TOMBSTONE  ***                       *             
*******************************************************************             
*                                                                               
T80249   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80249,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         SPACE 3                                                                
         L     R2,4(R1)                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   *+8                                                              
         BAS   RE,STASEC           STATION SECURITY CHECK                       
*                                                                               
         CLI   TWATRAFL,C'Y'            WAS TRAFFIC# GIVEN IN CONACT?           
         BNE   *+8                                                              
         BAS   RE,UPTRAF                YES - GO PUT IT IN RECORD               
*                                                                               
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
DISP     DS    0H                                                               
         TM    PROFILES+CNTSAEVB,CNTSAEVA                                       
         BZ    DISP02                                                           
*                                        DISPLAY EMAIL OVERRIDE                 
         NI    ORDEMHH+1,X'FF'-X'0C'     TURN OF ZERO INTENSITY                 
         NI    ORDEMLH+1,X'FF'-X'0C'                                            
*                                                                               
         NI    ORDEMLH+1,X'FF'-X'20'     UNPROTECTED                            
*                                                                               
DISP02   DS    0H                                                               
         GOTO1 VFOUTBLK,DMCB,ORDTRFH,ORDLAST,0                                  
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   DISP10                                                           
                                                                                
         LA    R2,ORDTOTH          TOTAL                                        
         OI    1(R2),X'20'         STATION SHOULD NOT CHANGE THIS FIELD         
         LA    R2,ORDASSTH         SALES ASSISTANT                              
         OI    1(R2),X'20'         OR THIS FIELD                                
         B     DISP20                                                           
*                                                                               
DISP10   LA    R2,ORDADVH          ADVERTISER                                   
         OI    1(R2),X'20'         REP SHOULD NOT CHANGE THIS FIELD             
         LA    R2,ORDAGYH          AGENCY                                       
         OI    1(R2),X'20'         OR THIS FIELD                                
         LA    R2,ORDTRFH          TRAFFIC NUMBER                               
         OI    1(R2),X'20'         OR THIS ONE                                  
         LA    R2,ORDOFFH          EOP OFFICE NUMBER                            
         OI    1(R2),X'20'         OR THIS ONE                                  
         LA    R2,ORDSALH          EOP SALESPERSON NUMBER                       
         OI    1(R2),X'20'         OR THIS ONE                                  
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    DISP11                                                           
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    DISP20                                                           
*        TM    RCONDRF2,X'80'      IS THIS VARIOUS??                            
*        BZ    DISP20              YES, WANT AUTO ORD TOTAL                     
         TM    RCONDRF2,X'08'      REMOVED?                                     
         BO    DISP20                                                           
         DROP  R6                                                               
*                                                                               
DISP11   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         USING RCONDREL,R6                                                      
*        TM    RCONDRFG,X'80'                                                   
*        BZ    DISP20                                                           
         OC    RCONDRLK,RCONDRLK                                                
         BZ    DISP20                                                           
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    DISP11A                                                          
         TM    RCONDRF2,X'08'      REMOVED?                                     
         BO    DISP20                                                           
         DROP  R6                                                               
*                                                                               
DISP11A  DS    0H                                                               
         LA    R2,ORDTOTH          TOTAL                                        
         OI    1(R2),X'20'         REP SHOULD NOT CHANGE THIS FIELD             
*                                                                               
         SR    R1,R1                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP12   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DISP15                                                           
         ZICM  R0,6(R6),4                                                       
         AR    R1,R0                                                            
         B     DISP12                                                           
DISP15   DS    0H                                                               
         LR    R3,R1               SAVE OFF TOTAL                               
         EDIT  (R1),(12,ORDTOT),2,ALIGN=LEFT,FLOAT=-                            
         STC   R0,ORDTOTH+5                                                     
*                                                                               
* UPDATE CONTRACT IF TOTALS ON SCREEN AND ON REC ARE DIFFERENT                  
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP40                                                           
         USING RCONXEL,R6                                                       
         ICM   R1,15,RCONTOT                                                    
         CR    R3,R1                                                            
         BE    DISP40                                                           
         STCM  R3,15,RCONTOT                                                    
         DROP  R6                                                               
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     DISP40                                                           
*                                                                               
DISP20   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP40                                                           
         USING RCONXEL,R6                                                       
         SPACE 1                                                                
         LA    R2,ORDTOTH          TOTAL                                        
*                                                                               
         OC    RCONTOT,RCONTOT                                                  
         BZ    DISP40                                                           
         EDIT  (4,RCONTOT),(12,ORDTOT),2,ALIGN=LEFT,FLOAT=-                     
         STC   R0,ORDTOTH+5                                                     
         DROP  R6                                                               
*                                                                               
DISP40   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9E'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP42                                                           
         USING RCONXOEL,R6                                                      
*                                                                               
         LA    R2,ORDEMLH          SALES ASSISTANT EMAIL                        
         ZIC   RE,1(R6)                                                         
*                                                                               
         AHI   RE,-3               SUBTRACT 3: 1 FOR EX, 2 FOR CTL              
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    RCONXOSA(RE),RCONXOSA                                            
         BZ    DISP42                                                           
*                                                                               
         CHI   RE,L'ORDEML-1       VERIFY LENGTH IS OK                          
         BNH   *+8                                                              
         LHI   RE,L'ORDEML-1                                                    
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONXOSA                                                 
         DROP  R6                                                               
*                                                                               
DISP42   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP42A                                                          
         USING RCONXXEL,R6                                                      
*                                                                               
         LA    R2,ORDASSTH         SALES ASSISTANT                              
         OC    RCONXAST,RCONXAST                                                
         BZ    DISP42AA                                                         
*                                                                               
         MVC   8(L'RCONXAST,R2),RCONXAST                                        
         B     DISP45                                                           
*                                                                               
DISP42A  DS    0H                                                               
         GOTOR DISASST             GET SALES ASSISTANT NAME FROM REC            
         B     DISP47              NO 9F SKIP EOP                               
DISP42AA DS    0H                                                               
         GOTOR DISASST                                                          
*                                                                               
*                                                                               
*                                                                               
* AUTO UPDATE THE EOP CODES IF NOT ALREADY PRESENT IN THE X'9F' ELEMENT         
*                                                                               
DISP45   DS    0H                                                               
         OC    RCONXADV,RCONXADV                                                
         BZ    *+14                                                             
         MVC   ORDADV(L'RCONXADV),RCONXADV                                      
         B     DISP46                                                           
*                                                                               
         GOTO1 CHKEOP,DMCB,(X'1B',ORDADVH),(4,RCONKADV),(15,0)                  
*                                                                               
DISP46   OC    RCONXAGY,RCONXAGY                                                
         BZ    DISP46A                                                          
         MVC   ORDAGY(L'RCONXAGY),RCONXAGY                                      
         B     DISP47                                                           
DISP46A  EQU   *                                                                
         GOTO1 CHKEOP,DMCB,(X'1C',ORDAGYH),(6,RCONKAGY),(13,1)                  
         DROP  R6                                                               
*                                                                               
DISP47   EQU   *                                                                
*                                                                               
*   ON-THE-FLY AVAILABILITY OF 'TRAFFIC NUMBER' FIELD.                          
*        THIS FIELD WILL BE PROTECTED IF CONTRACT IS 'ACE' (ON-LINE),           
*              AND SIGNON IS FROM THE REP SIDE.                                 
*        THIS FIELD WILL BE UNPROTECTED IF CONTRACT IS                          
*              1.  GRAPHNET (OFF-LINE), SIGNON FROM REP SIDE                    
*              2.  WEB CONFIRM, SIGNON FROM REP SIDE                            
*                                                                               
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    DISP4760            YES - DON'T PLAY GAMES WITH FLAGS            
*                                                                               
         LA    R2,ORDTRFH          TRAFFIC NUMBER                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'        RCU CONTRACT?                                
         BAS   RE,GETEL            ALLOW TRAFFIC NUMBER EDITING                 
         BE    DISP4740                                                         
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         TM    TWASTAOB,X'08'      WEB CONFIRM?                                 
         BO    DISP4740            YES - TURN OFF 'PROTECTED'                   
         GOTOR OPTRFFIC            UNPROTECT TRAFFIC NUMBER FIELD?              
         BE    DISP4740            YES - TURN OFF 'PROTECTED'                   
*                                                                               
         DROP  RF                                                               
*                                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET CONTRACT?                           
         BNO   DISP4750            NO  - ACE (ON-LINE)                          
DISP4740 EQU   *                                                                
         NI    1(R2),X'FF'-X'20'   YES - TURN OFF 'PROTECTED'                   
         B     DISP4760                                                         
DISP4750 EQU   *                                                                
         OI    1(R2),X'20'         NO  - TURN ON  'PROTECTED'                   
DISP4760 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP47A                                                          
         USING RCONXEL,R6                                                       
         LA    R2,ORDTRFH          TRAFFIC NUMBER                               
         OC    RCONTRF,RCONTRF                                                  
         BZ    *+10                                                             
         MVC   8(L'RCONTRF,R2),RCONTRF                                          
         DROP  R6                                                               
DISP47A  EQU   *                                                                
         FOUT ORDTRFH                                                           
*                                                                               
         OC    ORDADV,ORDADV                                                    
         BNZ   DISP48                                                           
         OC    ORDAGY,ORDAGY                                                    
         BZ    DISP50                                                           
*                                                                               
DISP48   DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BE    DISP49                                                           
         XC    WORK2(40),WORK2                                                  
         MVC   WORK2(2),=X'9F28'                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
*                                                                               
DISP49   DS    0H                                                               
         USING RCONXXEL,R6                                                      
         OC    RCONXADV,MYSPACES                                                
         OC    RCONXAGY,MYSPACES                                                
*                                                                               
         CLC   RCONXADV,ORDADV                                                  
         BNE   DISP49A                                                          
         CLC   RCONXAGY,ORDAGY                                                  
         BE    DISP50                                                           
*                                                                               
DISP49A  DS    0H                                                               
         MVC   RCONXADV,ORDADV                                                  
         MVC   RCONXAGY,ORDAGY                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
* DISPLAY EOP OFFICE AND SALESPERSON CODES IF PRESENT                           
*                                                                               
DISP50   DS    0H                                                               
*                                                                               
*   TEST                                                                        
***      MVC   DIE(2),=X'0000'     TEST DUMP                                    
*   TEST END                                                                    
*                                                                               
         GOTO1 CHKEOP,DMCB,(X'1D',ORDOFFH),(2,RCONKOFF),(17,0)                  
         GOTO1 CHKEOP,DMCB,(X'1E',ORDSALH),(3,RCONSAL),(16,0)                   
*                                                                               
         LA    R2,ORDRCMTH         REP ORDER COMMENT                            
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   DISP63                                                           
         LA    R5,10               FOR BCT LOOP                                 
DISP60   OI    1(R2),X'20'         STATION SHOULD NOT CHANGE THIS FIELD         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT TO NEXT CMT LINE                       
         BCT   R5,DISP60                                                        
*                                                                               
DISP63   LA    R2,ORDRCMTH         CLEAR ORD CMNT FIELDS BEFORE REDISP          
         LA    R5,10               FOR BCT LOOP                                 
DISP65   XC    8(L'ORDRCMT,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BCT   R5,DISP65                                                        
*                                                                               
         LA    R2,ORDRCMTH         BACK TO 1ST REP ORDER CMT                    
*                                                                               
DISP70   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP90                                                           
         LA    R5,10               FOR BCT LOOP                                 
DISP80   MVC   WORK3(80),MYSPACES                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
         SPACE 1                                                                
*                                                                               
         L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(1,(R2)),(R7),DATAMGR,RCONREC,GETTXT               
         BZ    *+10                                                             
         MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BAS   RE,NEXTEL                                                        
         BNE   DISP90                                                           
         BCT   R5,DISP80                                                        
         SPACE 1                                                                
DISP90   LA    R2,ORDSCMTH         STATION ORDER COMMENT                        
*                                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    DISP103                                                          
         LA    R5,10               FOR BCT LOOP                                 
DISP100  OI    1(R2),X'20'         REP SHOULD NOT CHANGE THIS FIELD             
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT TO NEXT STATION ORDER CMT              
         BCT   R5,DISP100                                                       
*                                                                               
DISP103  LA    R2,ORDSCMTH         CLEAR STA CMNT FIELDS BEFORE REDISP          
         LA    R5,10               FOR BCT LOOP                                 
DISP105  XC    8(L'ORDSCMT,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BCT   R5,DISP105                                                       
*                                                                               
         LA    R2,ORDSCMTH         BACK TO 1ST STATION ORDER CMT                
*                                                                               
DISP110  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP130                                                          
         LA    R5,10                                                            
DISP120  MVC   WORK3(80),MYSPACES                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT FIELD                                   
         BAS   RE,NEXTEL                                                        
         BNE   DISP130                                                          
         BCT   R5,DISP120                                                       
         SPACE 1                                                                
DISP130  GOTO1 VFOUTBLK,DMCB,ORDTOTH,ORDLAST,1                                  
*                                                                               
         LA    R2,ORDTOTH                                                       
         LA    R5,ORDTABH                                                       
         SR    R1,R1               MARK ALL FIELDS                              
         OI    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         IC    R1,0(R2)                                                         
         LA    R2,0(R1,R2)                                                      
         CR    R2,R5                                                            
         BL    *-14                                                             
*                                                                               
         CLI   TWATRAFL,C'Y'       HAVE WE COME DIRECTLY IN W/TRAFFIC           
         BNE   DISPX               NUM ENTERED IN CONACT FIELD?                 
         LA    R3,577                                                           
         LA    R2,CONCACTH                                                      
         MVC   8(3,R2),=C'CF='     FILL IN CF ACTION                            
         EDIT  STLNUM,(3,11(R2)),ALIGN=LEFT  FILL IN CORRECT VER#               
         NI    4(R2),X'FF'-X'20'  TURN OFF VAL PREVIOUSLY                       
         OI    6(R2),X'01'        CHANGE TO MODIFIED                            
         OI    6(R2),X'80'        XMIT                                          
         MVI   TWATRAFL,0         SET FLAG BACK TO NULL                         
         B     ERROR              PROMPT USER                                   
*                                                                               
DISPX    DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION                                      
         BE    EXXMOD                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXXMOD                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BNZ   EXXMOD                                                           
         OC    ORDTOT,MYSPACES                                                  
         CLC   ORDTOT,MYSPACES     ORDTOT BLANK?                                
         BNE   DISPX1              NO                                           
         XC    ORDTOT,ORDTOT       YES - MAKE IT ZERO                           
         OC    RCONTOT,RCONTOT     ORDER TOTAL ALSO ZERO?                       
         BZ    EXXMOD              YES                                          
         B     DISPX2              NO                                           
*                                                                               
DISPX1   XC    DMCB+4(4),DMCB+4                                                 
         MVC   DMCB+7(1),ORDTOTH+5 FIELD LENGTH                                 
         GOTO1 CASHVAL,DMCB,ORDTOT                                              
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RCONTOT,DMCB+4      ORDER TOTAL                                  
         BE    EXXMOD                                                           
         DROP  R6                                                               
*                                                                               
DISPX2   LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        LOOK AT DARE ELEM                            
         BAS   RE,GETEL                                                         
         BNE   EXXMOD                                                           
         USING RCONDREL,R6                                                      
*        TM    RCONDRFG,X'80'      LINKED TO DARE ORDER?                        
*        BZ    EXXMOD              NO - EXIT                                    
         OC    RCONDRLK,RCONDRLK                                                
         BZ    EXXMOD                                                           
         DROP  R6                                                               
         EJECT                     YES - FALL THRU TO EDIT ROUTINE              
EDIT     DS    0H                                                               
*                                                                               
         MVI   UPVER,0        ACE/GRAPHNET FLAG-UP VERSION & UNCONFIRM          
         CLC   CONACT,=C'ADDO'                                                  
         BE    EDIT10                                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
***>     MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         SPACE 1                                                                
EDIT10   DC    0H'0'                                                            
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    EDIT110             YES                                          
         OI    ORDTRFH+1,X'20'     NO  - ORDER TRAFFIC FIELD PROTECTED?         
         BO    EDIT15                                                           
         BAS   RE,ORDTRF#          PROCESS ORDER TRAFFIC NUMBER                 
EDIT15   EQU   *                                                                
         SPACE 1                                                                
* DEAL WITH REP INPUT FIELDS - TOTAL AND REP ORDER COMMENT                      
         SPACE 1                                                                
         XC    FULL,FULL                                                        
         LA    R2,ORDTOTH          TOTAL                                        
         CLI   5(R2),0             BLANK                                        
         BE    EDIT20                                                           
         SPACE 1                                                                
         LA    R3,2                INVALID INPUT FIELD                          
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     FIELD LENGTH                                 
         GOTO1 CASHVAL,DMCB,ORDTOT                                              
         SPACE 1                                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   FULL,DMCB+4                                                      
         SPACE 1                                                                
EDIT20   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT30                                                           
         USING RCONXEL,R6                                                       
         CLC   RCONTOT,FULL        NEW TOTAL = OLD TOTAL?                       
*                                     (YES = NO CHANGE TO VALUE)                
         BE    EDIT25              YES - NO UPDATE TO VER                       
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
EDIT25   EQU   *                                                                
         MVC   RCONTOT,FULL        ORDER TOTAL                                  
         B     EDIT40                                                           
         DROP  R6                                                               
         SPACE 1                                                                
EDIT30   CLI   5(R2),0             IF 0, DON'T BOTHER WITH ELEMENT              
         BE    EDIT40                                                           
         XC    WORK2,WORK2         BUILD NEW ELEMENT IN WORK2                   
         MVC   WORK2(2),=X'1F18'                                                
         MVC   WORK2+18(4),FULL    ORDER TOTAL                                  
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         SPACE 1                                                                
EDIT40   LA    R2,ORDEMLH          SALES ASSISTANT EMAIL                        
         TM    PROFILES+CNTSAEMB,CNTSAEMA                                       
         BZ    EDIT40A                                                          
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
*                                                                               
         TM    TWASTAOB,X'08'      WEB CONFIRM?                                 
         BZ    EDIT40A             NO - FIELD NOT REQUIRED                      
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   5(R2),0             BLANK                                        
         BNE   EDIT40A             NO                                           
*                                                                               
         LA    R3,1                MISSING INPUT                                
         BE    ERROR                                                            
*                                                                               
EDIT40A  LLC   RE,ORDEMLH+5        E-MAIL INPUT LENGTH                          
         LA    RF,8(R2)                                                         
EDIT40B  CLI   0(RF),C'@'          CONTAIN AT LEAST ONE '@' CHAR?               
         JE    EDIT40C                                                          
         LA    RF,1(RF)            BUMP TO NEXT CHAR IN INPUT                   
         JCT   RE,EDIT40B                                                       
         J     EDIT40E                                                          
EDIT40C  LLC   RE,ORDEMLH+5        E-MAIL INPUT LENGTH                          
         LA    RF,8(R2)                                                         
EDIT40D  CLI   0(RF),C'.'          CONTAIN AT LEAST ONE '.' CHAR?               
         JE    EDIT40K                                                          
         LA    RF,1(RF)            BUMP TO NEXT CHAR IN INPUT                   
         JCT   RE,EDIT40D                                                       
EDIT40E  LA    R3,10               INVALID ADDRESS FORMAT                       
         J     ERROR                                                            
*                                                                               
EDIT40K  GOTO1 VDELELEM,DMCB,(X'9E',RCONREC)                                    
*                                                                               
         XC    WORK2(2+L'ORDEML),WORK2                                          
         MVI   WORK2,X'9E'                                                      
*                                                                               
         ZIC   RE,ORDEMLH+5                                                     
         CHI   RE,0                                                             
         BE    EDIT42                                                           
*                                                                               
         AHI   RE,-1                                                            
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)                                                 
*                                                                               
         AHI   RE,3                LENGTH OF ELEM                               
         STC   RE,WORK2+1                                                       
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
EDIT42   LA    R2,ORDASSTH         SALES ASSISTANT                              
***      TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
***      BO    EDIT60                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT50                                                           
         USING RCONXXEL,R6                                                      
         MVC   RCONXAST,8(R2)                                                   
         B     EDIT60                                                           
         DROP  R6                                                               
*                                                                               
EDIT50   XC    WORK2(40),WORK2                                                  
         MVC   WORK2(2),=X'9F28'                                                
         MVC   WORK2+22(9),8(R2)                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
EDIT60   LA    R2,ORDRCMTH         REP ORDER CMT                                
         SR    R1,R1                                                            
         LA    R5,10                                                            
EDIT70   TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BNO   EDIT80              IF ANY LINE CHANGES, REVALIDATE ALL          
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT70                                                        
         B     EDIT220                                                          
         SPACE 1                                                                
*                 DELETE ALL REP COMMENT ELEMENTS                               
EDIT80   GOTO1 VDELELEM,DMCB,(X'82',RCONREC)                                    
*                 DELETE ALL STA COMMENT ELEMENTS                               
         GOTO1 VDELELEM,DMCB,(X'92',RCONREC)                                    
*                                                                               
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         LA    R2,ORDRCMTH         REP ORDER COMMENT                            
*                                                                               
         GOTO1 =A(GETORD),RR=YES   COPY CMT FROM OTHER K?                       
         BNZ   EDIT220                                                          
*                                                                               
         LA    R5,10                                                            
EDIT90   CLI   5(R2),0                                                          
         BE    EDIT100                                                          
*                                                                               
         L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R7),DATAMGR,RCONREC,GETTXT               
         BZ    *+12                                                             
         L     RD,BASERD           ERROR RETURN CONTROL TO USER                 
         B     EXXMOD                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)    DATA TO ELEMENT                              
         AH    R1,=H'3'                                                         
         STC   R1,WORK2+1                                                       
         MVI   WORK2,X'82'         REP ORDER COMMENT                            
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
EDIT100  EQU   *                                                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT90                                                        
         B     EDIT220                                                          
*                                                                               
*  DEAL WITH STATION FIELDS - TRAFFIC NO., ADV, AGY AND STA ORD CMT             
* CHANGE TO TRAFFIC NO, ADV OR AGY DOES NOT UP VERSION OR UNCONFIRM             
         SPACE 1                                                                
EDIT110  EQU   *                                                                
         XC    MGWORK,MGWORK       TEMPORARY STORAGE                            
         LA    R2,ORDTRFH          TRAFFIC NUMBER                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    EDIT122                                                          
*                                                                               
* CHECK IF TRAFFIC NUMBER ALREADY ASSIGNED TO A ANOTHER CONTRACT                
*                                                                               
         CLI   5(R2),0             SKIP CHECK IF NO DATA                        
         BE    EDIT113                                                          
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),=X'A203'     INSERT KEY TYPE                              
         MVC   KEY+11(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+13(10),8(R2)    INSERT TRAFFIC NUMBER                        
         GOTO1 VHIGH                                                            
         CLC   KEY(23),KEYSAVE     KEY FOUND ON FILE?                           
         BNE   EDIT113             YES, SHOW CONTRACT ASSIGNED                  
*                                                                               
         LA    R3,1037                                                          
         ZAP   WORK2+20(5),=P'0'                                                
         MVO   WORK2+20(5),KEY+23(4)                                            
         EDIT  (P5,WORK2+20),(8,WORK2+30),ALIGN=LEFT                            
         LA    R1,WORK2+30                                                      
         ST    R1,DMCB+12                                                       
         MVI   DMCB+12,8                                                        
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    ORDTRFH+6,X'40'     FORCE CURSOR TO ACTION                       
         NI    ORDTRFH+1,X'FF'-X'20'   TURN OFF 'PROTECTED'                     
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
*                                                                               
EDIT113  EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6          UPDATE ELEMENT                               
         MVC   MGWORK(10),RCONTRF  SAVE ORIGINAL TRAFFIC NUMBER                 
*                                     MAY NOT BE 10 CHARS                       
*        DISK ADDRESS IS NOT PRESENT AT THIS TIME.  MUST BE RETRIEVED           
*           LATER WHEN RECORD IS RETRIEVED FOR UPDATE                           
*                                                                               
         MVC   MGWORK+24(4),RCONKCON                                            
*                                  SAVE CONTRACT NUMBER                         
         CLI   5(R2),0             IF NO DATA NOW                               
         BNE   EDIT115                                                          
         XC    RCONTRF,RCONTRF     ERASE WHAT WAS THERE                         
         B     EDIT120                                                          
*                                                                               
EDIT115  EQU   *                                                                
         MVC   RCONTRF,8(R2)       TRAFFIC NUMBER                               
         MVC   MGWORK+10(10),8(R2)                                              
*                                  SAVE NEW TRAFFIC NUMBER                      
         DROP  R6                                                               
         SPACE 1                                                                
EDIT120  EQU   *                                                                
EDIT122  EQU   *                                                                
         LA    R2,ORDADVH          ADVERTISER                                   
         XC    WORK(20),WORK                                                    
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    EDIT130                                                          
*                                                                               
         CLI   5(R2),0             NOTHING THERE NOW                            
         BNE   *+14                                                             
         MVC   WORK(10),MYSPACES   INDICATE DATA CHANGED                        
         B     EDIT130             ZERO LENGTH NOW BYPASSES MOVE                
                                                                                
         LA    R3,2                INVALID INPUT                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        FOR EC BIAS                                  
         BNE   EDIT125                                                          
         CLI   5(R2),6                                                          
         BH    ERROR                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
         B     EDIT128                                                          
                                                                                
EDIT125  DS    0H                                                               
         CLI   TWAECON,C'J'        FOR EC JDS/2000                              
         BNE   EDIT128                                                          
         CLI   5(R2),4                                                          
         BH    ERROR                                                            
         DROP  RF                                                               
                                                                                
EDIT128  DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
*                                                                               
EDIT130  LA    R2,ORDAGYH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    EDIT140                                                          
*                                                                               
         CLI   5(R2),0             NOTHING THERE NOW                            
         BNE   *+14                                                             
         MVC   WORK+10(10),MYSPACES INDICATED DATA CHANGED                      
         B     EDIT140                                                          
                                                                                
         LA    R3,2                INVALID INPUT                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'B'        FOR EC BIAS                                  
         BNE   EDIT135                                                          
         CLI   5(R2),6                                                          
         BH    ERROR                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
         B     EDIT138                                                          
                                                                                
EDIT135  DS    0H                                                               
         CLI   TWAECON,C'J'        FOR EC JDS/2000                              
         BNE   EDIT138                                                          
         CLI   5(R2),6             UP TO 6                                      
         BH    ERROR                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
         DROP  RF                                                               
                                                                                
EDIT138  DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+10(0),8(R2)                                                 
*                                                                               
EDIT140  OC    WORK(20),WORK                                                    
         BZ    EDIT160             NOTHING HAS CHANGED                          
*                                                                               
         DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT150                                                          
*                                                                               
         USING RCONXXEL,R6                                                      
         OC    WORK(10),WORK       DID ADV CHANGE                               
         BZ    *+10                                                             
         MVC   RCONXADV,WORK       YES                                          
*                                                                               
         OC    WORK+10(10),WORK+10 DID AGY CHANGE                               
         BZ    *+10                                                             
         MVC   RCONXAGY,WORK+10    YES                                          
         DROP  R6                                                               
         B     EDIT160                                                          
*                                                                               
EDIT150  XC    WORK2(40),WORK2                                                  
         MVC   WORK2(2),=X'9F28'                                                
         MVC   WORK2+2(20),WORK                                                 
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
EDIT160  LA    R2,ORDSCMTH         STATION ORDER CMT                            
         SR    R1,R1                                                            
         LA    R5,10                                                            
EDIT170  TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BNO   EDIT180             IF ANY LINE CHANGES, REVALIDATE ALL          
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT170                                                       
         B     EDIT220                                                          
         SPACE 1                                                                
*              NO CHANGES ALLOWED IF STATION WAS LAST TO SEND                   
         SPACE 1                                                                
EDIT180  DS    0H                                                               
* DELETE ALL STATION ORDER COMMENTS                                             
         GOTO1 VDELELEM,DMCB,(X'92',RCONREC)                                    
* DELETE ALL REP ORDER COMMENTS                                                 
         GOTO1 VDELELEM,DMCB,(X'82',RCONREC)                                    
         SPACE 1                                                                
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         LA    R2,ORDSCMTH         STATION ORDER COMMENT                        
         LA    R5,10                                                            
EDIT200  CLI   5(R2),0                                                          
         BE    EDIT210                                                          
         SPACE 1                                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)    DATA TO ELEMENT                              
         AH    R1,=H'3'                                                         
         STC   R1,WORK2+1                                                       
         MVI   WORK2,X'92'         STATION ORDER COMMENT                        
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
EDIT210  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R5,EDIT200                                                       
         EJECT                                                                  
EDIT220  CLC   CONACT,=C'ADDO'     IF ADDING CONTRACT & ORDER RECORD,           
         BE    EXXMOD     T80211 ADDS RECORD, X'1F', & X'20' ELEMENTS           
         SPACE 2                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    EDIT290                                                          
         SPACE 1                                                                
*                                                                               
*  FOR ACE/GRAPHNET CONTRACTS ONLY -                                            
*    IF JUST ADDING/CHANGING ORDER RECORD, UNCONFIRM CONTRACT                   
*    AND UPDATE VERSION NUMBER IF NECESSARY                                     
* OTHERWISE, JUST PUT CHANGED RECORD                                            
*                                                                               
         CLI   UPVER,1   DO WE NEED TO UPDATE VERS NUMBER & UNCONFIRM           
         BNE   EDIT300                                                          
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'              THERE SHOULD ALREADY BE A '1F'                 
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         SPACE 1                                                                
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    *+12                                                             
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREVIOUSLY                 
         OI    RCONCONF,X'80'      TURN ON NOT CONFIRMED                        
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THERE SHOULD BE A SEND ELEMENT               
         SPACE 1                                                                
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
         CLI   TWAACCS,C'$'        STATION                                      
         BE    EDIT240                                                          
*                                                                               
* CAN ONLY MAKE CHANGES IF STATION VERSION NOT ADVANCED                         
*                                                                               
         TM    RCONSENF,X'10'      STA VERSION NOT ADVANCED                     
         BO    EDIT230                                                          
         LA    R3,167              LATEST STATION VERSION NOT YET SENT          
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
EDIT230  TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    EDIT300                                                          
                                                                                
* ADVANCE REP VERSION AND UPDATE VERSION DATE                                   
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),(X'80',WORK)                        
         BNZ   ERROR                                                            
         B     EDIT260                                                          
*                                                                               
* CAN ONLY MAKE CHANGES IF REP VERSION NOT ADVANCED                             
*                                                                               
EDIT240  TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BO    EDIT250                                                          
         LA    R3,168              LATEST REP VERSION NOT YET SENT              
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
*                                                                               
EDIT250  TM    RCONSENF,X'10'      STATION VERSION NOT ADVANCED                 
         BZ    EDIT300                                                          
         DROP  R6                                                               
                                                                                
* ADVANCE STA VERSION AND UPDATE VERSION DATE                                   
         MVC   WORK(4),HELLO                                                    
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'S',RCONREC),(X'80',WORK)                        
         BNZ   ERROR                                                            
                                                                                
EDIT260  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THERE SHOULD BE A SEND ELEMENT               
         USING RCONSEND,R6                                                      
*                                                                               
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION NUMBER                   
         BH    EDIT270                                                          
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     EDIT280                                                          
EDIT270  EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
EDIT280  MVC   CONMOD(7),=C'WIP VER'                                            
         TM    RCONMODR+1,X'40'    GRAPHNET                                     
         BZ    *+10                                                             
         MVC   CONMOD(7),=C'ESL VER'                                            
         FOUT  CONMODH                                                          
         B     EDIT300                                                          
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* NON-ACE/GRAPHNET CONTRACTS NEED TO HAVE THEIR MOD NUMBER BUMPED               
*                                                                               
EDIT290  DS    0H                                                               
         BAS   RE,BUMPNUM                                                       
                                                                                
* SHOW MOD NUMBER ON SCREEN                                                     
                                                                                
         MVC   CONMOD,MYSPACES                                                  
         CLI   RCONMOD,0                                                        
         BE    EDIT300                                                          
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD                                                
         CLI   HALF+1,250                                                       
         BL    *+8                                                              
         MVI   HALF,255                                                         
         EDIT  (2,HALF),(3,CONMOD+8),ALIGN=LEFT,FLOAT=-                         
         MVC   CONMOD(7),=C'MOD NUM'                                            
         FOUT  CONMODH                                                          
                                                                                
EDIT300  DS    0H                  RE-READ K FOR PUTREC                         
         MVC   KEY(27),RCONKEY                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         MVC   MGWORK+20(4),KEY+28                                              
*                                  INSERT DISK ADDRESS FOR KEY MAINT            
         GOTO1 TRAFKEYS            MAINTAIN TRAFFIC PASSIVES                    
*                                                                               
*                                                                               
         TM    ORDADVH+4,X'20'     VALIDATED PREVIOUSLY                         
         BO    EDIT330                                                          
         OC    ORDADV,ORDADV                                                    
         BZ    EDIT330                                                          
         GOTO1 CHGEOP,DMCB,(X'1B',ORDADVH),(4,RCONKADV),(15,0)                  
                                                                                
EDIT330  DS    0H                                                               
         TM    ORDAGYH+4,X'20'     VALIDATED PREVIOUSLY                         
         BO    EDIT340                                                          
         OC    ORDAGY,ORDAGY                                                    
         BZ    EDIT340                                                          
         GOTO1 CHGEOP,DMCB,(X'1C',ORDAGYH),(6,RCONKAGY),(13,0)                  
                                                                                
EDIT340  DS    0H                                                               
         TM    ORDOFFH+4,X'20'     VALIDATED PREVIOUSLY                         
         BO    EDIT350                                                          
         OC    ORDOFF,ORDOFF                                                    
         BZ    EDIT350                                                          
         GOTO1 CHGEOP,DMCB,(X'1D',ORDOFFH),(2,RCONKOFF),(17,0)                  
                                                                                
EDIT350  DS    0H                                                               
         TM    ORDSALH+4,X'20'     VALIDATED PREVIOUSLY                         
         BO    EDITX                                                            
         OC    ORDSAL,ORDSAL                                                    
         BZ    EDITX                                                            
         GOTO1 CHGEOP,DMCB,(X'1E',ORDSALH),(3,RCONSAL),(16,0)                   
                                                                                
EDITX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*   PROCESS ORDER TRAFFIC NUMBER FIELD IF FIELD MADE AVAILABLE FOR              
*        REP SIGNON AS EITHER WEB CONFIRM OR GRAPHNET STATION                   
*                                                                               
*   RCU CONTRACT CAN EDIT THIS FIELD                                            
*                                                                               
ORDTRF#  NTR1                                                                   
         XC    MGWORK,MGWORK       TEMPORARY STORAGE                            
         LA    R2,ORDTRFH          TRAFFIC NUMBER                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    OTRFX               YES - LEAVE IT ALONE                         
*                                                                               
* CHECK IF TRAFFIC NUMBER ALREADY ASSIGNED TO A ANOTHER CONTRACT                
*                                                                               
         CLI   5(R2),0             SKIP CHECK IF NO DATA                        
         BE    OTRF0010                                                         
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),=X'A203'     INSERT KEY TYPE                              
         MVC   KEY+11(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+13(10),8(R2)    INSERT TRAFFIC NUMBER                        
         GOTO1 VHIGH                                                            
         CLC   KEY(23),KEYSAVE     KEY FOUND ON FILE?                           
         BNE   OTRF0010            YES, SHOW CONTRACT ASSIGNED                  
*                                                                               
         LA    R3,1037                                                          
         ZAP   WORK2+20(5),=P'0'                                                
         MVO   WORK2+20(5),KEY+23(4)                                            
         EDIT  (P5,WORK2+20),(8,WORK2+30),ALIGN=LEFT                            
         LA    R1,WORK2+30                                                      
         ST    R1,DMCB+12                                                       
         MVI   DMCB+12,8                                                        
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    ORDTRFH+6,X'40'     FORCE CURSOR TO ACTION                       
         NI    ORDTRFH+1,X'FF'-X'20'   TURN OFF 'PROTECTED'                     
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
*                                                                               
OTRF0010 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6          UPDATE ELEMENT                               
         MVC   MGWORK(10),RCONTRF  SAVE ORIGINAL TRAFFIC NUMBER                 
*                                     MAY NOT BE 10 CHARS                       
         MVC   MGWORK+24(4),RCONKCON                                            
*                                  SAVE CONTRACT NUMBER                         
         CLI   5(R2),0             IF NO DATA NOW                               
         BNE   OTRF0020                                                         
         XC    RCONTRF,RCONTRF     ERASE WHAT WAS THERE                         
         B     OTRFX                                                            
*                                                                               
OTRF0020 EQU   *                                                                
         MVC   RCONTRF,8(R2)       TRAFFIC NUMBER                               
         MVC   MGWORK+10(10),8(R2)                                              
*                                  SAVE NEW TRAFFIC NUMBER                      
         DROP  R6                                                               
*                                                                               
OTRFX    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DELETE OLD TRAFFIC NUMBER PASSIVE, ADD NEW TRAFFIC NUMBER                   
*        PASSIVE, AS APPROPRIATE.                                               
*                                                                               
TRAFKEYS NTR1                                                                   
*                                                                               
*   THIS TEST IS NOT ACTIVATED                                                  
*                                                                               
***      CLC   MGWORK(10),MGWORK+10                                             
*                                  SAME TRAFFIC NUMBER?                         
***      BE    TRAF0800            YES - EXIT WITH NO CHANGE                    
         OC    MGWORK,MGWORK       OLD KEY ENTERED?                             
         BZ    TRAF0120            NO  - NOTHING TO DELETE                      
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),=X'A203'     INSERT KEY TYPE                              
         MVC   KEY+11(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+13(10),MGWORK   INSERT ORIGINAL TRAFFIC NUMBER               
         MVC   KEY+23(4),MGWORK+24 INSERT ORIGINAL CONTRACT NUMBER              
         MVI   UPDATE,C'Y'         SET 'READ FOR UPDATE'                        
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
         BNE   TRAF0120            NO  - NOTHING TO TURN OFF                    
         OI    KEY+27,X'80'        YES - SET KEY TO 'DELETED'                   
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
TRAF0120 EQU   *                                                                
         OC    MGWORK+10(10),MGWORK+10                                          
*                                  ANYTHING IN NEW KEY?                         
         BZ    TRAF0800            NO  - NO NEW KEY TO WRITE                    
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),=X'A203'     INSERT KEY TYPE                              
         MVC   KEY+11(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+13(10),MGWORK+10       INSERT NEW TRAFFIC NUMBER             
         MVC   KEY+23(4),MGWORK+24 INSERT ORIGINAL CONTRACT NUMBER              
         MVI   UPDATE,C'Y'         SET 'READ FOR UPDATE'                        
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
         BNE   TRAF0140            NO  - KEY MUST BE ADDED                      
         NI    KEY+27,X'FF'-X'80'  YES - TURN OFF DELETE BIT                    
         MVC   KEY+28(4),MGWORK+20 INSERT NEW RECORD DISK ADDRESS               
         GOTO1 VWRITE              WRITE NEW RECORD                             
         B     TRAF0300                                                         
TRAF0140 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY SOUGHT                           
         MVI   KEY+27,0            CLEAR CONTROL BYTE                           
         MVC   KEY+28(4),MGWORK+20 INSERT NEW RECORD DISK ADDRESS               
         GOTO1 VADD                ADD NEW RECORD KEY                           
*                                                                               
TRAF0300 LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         OI    TWACONFG,TW_TRFOQ   TRAFFIC ORDER# IS CHANGED                    
         DROP  RF                                                               
*                                                                               
TRAF0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*           IF STATION IS USER, CHECK THAT THE STATION IS                       
*           AUTHORIZED TO SEE THIS CONTRACT.                                    
*                                                                               
STASEC   NTR1                                                                   
         XC    KEY,KEY                                                          
SS       USING RSTAREC,KEY                                                      
         MVI   SS.RSTAKTYP,2                                                    
         MVC   SS.RSTAKREP,REPALPHA                                             
         MVC   SS.RSTAKSTA,RCONKSTA                                             
         DROP  SS                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         LA    R2,CONCNUMH                                                      
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERROR                                                            
*                                                                               
         USING RSTASOEL,R6         STATION'S CONTRACTS                          
STASEC20 CLC   TWAUSRID,RSTASID                                                 
         BE    EXXMOD                                                           
         BAS   RE,NEXTEL                                                        
         BE    STASEC20                                                         
         B     ERROR                                                            
         DROP  R6                                                               
OPTRFFIC NTR1                                                                   
         XC    KEY,KEY                                                          
SS       USING RSTAREC,KEY                                                      
         MVI   SS.RSTAKTYP,2                                                    
         MVC   SS.RSTAKREP,REPALPHA                                             
         MVC   SS.RSTAKSTA,RCONKSTA                                             
         DROP  SS                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   OPTRFNO                                                          
*                                                                               
         USING RSTACODE,R6                                                      
         CLI   RSTAP1,C'Y'         PROFILE: UNPROTECT TRAFFIC# ON ORD           
         BNE   OPTRFNO                                                          
OPTRFYES SR    RC,RC                                                            
OPTRFNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
* ROUTINE TO UPDATE TRAFFIC# WHEN ENTERED IN CONACT FIELD                       
***********************************************************************         
UPTRAF   NTR1                                                                   
         XC    MGWORK,MGWORK       TEMPORARY STORAGE                            
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         MVC   MGWORK+20(4),TWAKADDR                                            
*                                  SAVE DISK ADDRESS OF RECORD                  
         DROP  RF                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
*                                                                               
         MVC   MGWORK(10),RCONTRF  SAVE ORIGINAL TRAFFIC NUMBER                 
         MVC   MGWORK+10(10),TWATRAF                                            
*                                  SAVE NEW TRAFFIC NUMBER                      
         MVC   MGWORK+24(4),RCONKCON                                            
*                                  SAVE CONTRACT NUMBER                         
         MVC   RCONTRF,TWATRAF     SAVE OFF TRAFFIC# FROM CONACT FIELD          
         DROP  R6                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
         GOTO1 TRAFKEYS            MAINTAIN TRAFFIC PASSIVES                    
*                                                                               
         XIT1                                                                   
                                                                                
***********************************************************************         
* ROUTINE TO BUMP CONTRACT MODIFICATION MUMBER                                  
***********************************************************************         
BUMPNUM  OI    RCONMODR,X'80'      CON HEADLINE CHANGE IND                      
         CLC   RCONCREA,TODAY      NEW K TODAY?                                 
         BCR   8,RE                                                             
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BNZ   BUMP5                                                            
*                                                                               
         CLC   RCONMODD,TODAY      MODIFIED TODAY?                              
         BCR   8,RE                                                             
* UPDATE MODIFICATION NUMBER                                                    
BUMP5    OI    TAREQ,1             T/A REQ IND                                  
         NI    RCONMODR,X'9F'      NO BUY HEADLINE CHANGE OR RESET              
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'  ACE/GRAPHNET- DON'T BUMP MOD NUMBER            
         BNZ   BUMP10                                                           
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,RCONMOD          MOD NUMBER                                   
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
BUMP10   MVC   RCONMODD,TODAY                                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS EOP 1B/1C RECORDS FOR EC BIAS/JDS TYPE CONTRACTS                      
* P1=(B1)RECORD TYPE, (B2-3)SCREEN HEADER ADDRESS WITH DATA                     
* P2=(B1)LENGTH OF EOP FIELD, (B2-3) ADDRESS OF EOP FIELD                       
* P3=(B1)OFFSET TO FIRST NON-NULL FIELD OF EOP KEY                              
***********************************************************************         
CHGEOP   NTR1                                                                   
         MVC   WORK(12),0(R1)                                                   
         ZICM  R2,WORK+1,3                                                      
                                                                                
         LA    R6,IOAREA                                                        
         USING REOPKEY,R6                                                       
         XC    IOAREA(256),IOAREA                                               
         MVC   REOPKTYP(1),WORK    RECORD TYPE                                  
         DROP  R6                                                               
                                                                                
         ZIC   RF,WORK+8           GET OFFSET OF EOP FIELDS IN KEY              
         AR    R6,RF                                                            
         MVC   0(L'REOPKREP,R6),REPALPHA   REP                                  
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVI   2(R6),1             BIAS                                         
         CLI   TWATRFMT,C'B'                                                    
         BE    CHGEOP05                                                         
         CLI   TWATRFMT,C'W'       ALLOW W FOR BIAS                             
         BE    CHGEOP05                                                         
         MVI   2(R6),2             JDS                                          
         CLI   TWATRFMT,C'J'                                                    
         BE    CHGEOP05                                                         
         MVI   2(R6),3             ENTERPRISE                                   
         CLI   TWATRFMT,C'K'                                                    
         BE    CHGEOP05                                                         
         MVI   2(R6),4             COLUMBINE                                    
         CLI   TWATRFMT,C'C'                                                    
         BE    CHGEOP05                                                         
         MVI   2(R6),5             VCI                                          
         CLI   TWATRFMT,C'S'                                                    
         BNE   CHGEOPX                                                          
         DROP  RF                                                               
*                                                                               
CHGEOP05 DS    0H                                                               
         MVC   3(L'REOPKSTA,R6),RCONKSTA   STATION+MEDIA                        
                                                                                
         ZICM  RF,WORK+5,3                                                      
         ZIC   R1,WORK+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),0(RF)       ADVERTISER/AGENCY                            
                                                                                
         OI    DMINBTS,X'08'       READ DELETES                                 
         MVC   KEY(L'REOPKEY),IOAREA                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BE    CHGEOP50                                                         
                                                                                
         LA    R6,IOAREA                                                        
         USING REOPKEY,R6                                                       
         MVI   REOPLEN+1,50        REC LEN (34 + 16)                            
         MVI   REOPCODE,X'01'      EOP ELEMENT CODE                             
         MVI   REOPELLN,16         EOP ELEMENT LENGTH                           
         GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'J'        FOR EC BIAS/JDS                              
         BNE   CHGEOP10            JDS ADV IS ALPHANUMERIC                      
         CLI   REOPKTYP,X'1B'      ALL OTHERS ARE NUMERIC ONLY                  
         BNE   CHGEOP10                                                         
         DROP  RF                                                               
                                                                                
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(4),8(R2)                                                
         OC    REOPEQUV(6),MYSPACES                                             
         B     CHGEOP20                                                         
                                                                                
CHGEOP10 DS    0H                                                               
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(6),=6C'0'     LEADING ZEROES                            
         LA    RE,REOPEQUV+6                                                    
         ZIC   RF,5(R2)                                                         
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)                                                    
         DROP  R6                                                               
                                                                                
CHGEOP20 DS    0H                                                               
         GOTO1 VADDREC,DMCB,IOAREA ADD THE RECORD                               
         B     CHGEOPX                                                          
                                                                                
CHGEOP50 DS    0H                  CHANGE EXISTING RECORD                       
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R6,IOAREA                                                        
         USING REOPREC,R6                                                       
                                                                                
         TM    REOPFLAG,X'80'      IS CODE ACTIVE?                              
         BZ    CHGEOP60                                                         
         LA    R3,406              CANNOT CHANGE ACTIVE CODE                    
         B     ERROR                                                            
                                                                                
CHGEOP60 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWAECON,C'J'        FOR EC BIAS/JDS                              
         BNE   CHGEOP70            JDS ADV IS ALPHANUMERIC                      
         CLI   REOPKTYP,X'1B'      ALL OTHERS ARE NUMERIC                       
         BNE   CHGEOP70                                                         
         DROP  RF                                                               
                                                                                
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(4),8(R2)                                                
         OC    REOPEQUV(6),MYSPACES                                             
         B     CHGEOP80                                                         
                                                                                
CHGEOP70 DS    0H                                                               
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(6),=6C'0'     LEADING ZEROES                            
         LA    RE,REOPEQUV+6                                                    
         ZIC   RF,5(R2)                                                         
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)                                                    
                                                                                
CHGEOP80 DS    0H                                                               
         NI    REOPCNTL,X'FF'-X'80'  RESTORE INCASE RECORD IS DELETED           
                                                                                
         GOTO1 VPUTREC,DMCB,IOAREA                                              
                                                                                
         TM    KEY+27,X'80'        RESTORE DELETED RECORD                       
         BZ    CHGEOPX                                                          
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE                                                           
                                                                                
CHGEOPX  DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS EOP 1B/1C RECORDS FOR EC BIAS/JDS TYPE CONTRACTS                      
* P1=(B1)RECORD TYPE, (B2-3)SCREEN HEADER ADDRESS WITH DATA                     
* P2=(B1)LENGTH OF EOP FIELD, (B2-3) ADDRESS OF EOP FIELD                       
* P3=(B1)OFFSET TO FIRST NON-NULL FIELD OF EOP KEY                              
***********************************************************************         
CHKEOP   NTR1                                                                   
         MVC   WORK(12),0(R1)                                                   
         XC    HALF,HALF           CLEAR PASS-BACK FIELD                        
*                                                                               
*   PASS-BACK FIELD IS USED FOR TRADE AGENCY PROCESSING.  THIS WILL             
*        CONTAIN THE ALTERNATE TRADE OFFICE CODE IF A TRADE                     
*        ORDER'S AGENCY CODE MUST BE ASSIGNED.                                  
*        DO NOT - REPEAT, NOT - STEP ON THE FIRST BYTE OF HALF                  
*        IN THIS ROUTINE.                                                       
*                                                                               
         CLI   WORK+11,0           P3 (2-4) ZERO? (NOT AGENCY)                  
         BE    CHKE0060            YES - NOT AGENCY                             
         LA    R6,RCONREC          DISPLAY CONFLICT CODES                       
         MVI   ELCODE,X'1E'        RANDOM FLAG ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   CHKE0060            NOT FOUND                                    
         TM    RCONRF1-RCONRFEL(R6),X'08'                                       
*                                  TRADE ORDER SET?                             
         BNO   CHKE0060            NO                                           
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'1A'           SET AGENCY 2 REC                             
         MVC   KEY+19(6),RCONKAGY  INSERT AGENCY+OFF                            
         MVC   KEY+25(2),RCONKREP  INSERT REP CODE                              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                KEY MUST BE ON FILE                          
         GOTO1 VGETREC,DMCB,IOAREA RETRIEVE THE AGENCY2 RECORD                  
         LA    R6,IOAREA                                                        
         USING RAGY2REC,R6                                                      
         LA    RF,RAGY2FXE         1ST ELEMENT IN RAGY2REC                      
         DROP  R6                                                               
CHKE0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO  -                                        
         DC    H'0'                MUST FIND THE ELEMENT                        
         CLI   0(RF),X'1F'         AGENCY ELEMENT?                              
         BE    CHKE0040            YES -                                        
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     CHKE0020            GO BACK FOR NEXT                             
CHKE0040 EQU   *                                                                
         MVI   HALF,X'FF'          SET NO-FIND FOR ALT TRADE                    
         CLI   RAG2TRAD-RAG2ELEM(RF),C' '                                       
*                                  ANYTHING IN ALT TRADE CODE FIELD?            
         BNH   CHKE0060            NO  - PASS BACK EMPTY FIELD                  
         MVC   HALF(1),RAG2TRAD-RAG2ELEM(RF)                                    
*                                  PASS BACK ALT TRADE CODE                     
CHKE0060 EQU   *                                                                
         ZICM  R2,WORK+1,3                                                      
*        NI    1(R2),X'FF'-X'20'   DEFAULT UNPROTECT                            
                                                                                
         XC    IOAREA(256),IOAREA                                               
         LA    R6,IOAREA                                                        
         USING REOPKEY,R6                                                       
         MVC   REOPKTYP(1),WORK    RECORD TYPE                                  
         DROP  R6                                                               
         ZIC   RF,WORK+8           GET OFFSET OF EOP FIELDS IN KEY              
         AR    R6,RF                                                            
         MVC   0(L'REOPKREP,R6),REPALPHA   REP                                  
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVI   2(R6),1             BIAS                                         
         CLI   TWATRFMT,C'B'                                                    
         BE    CHKE0080                                                         
         CLI   TWATRFMT,C'W'       ALLOW W FOR BIAS                             
         BE    CHKE0080                                                         
         MVI   2(R6),2             JDS                                          
         CLI   TWATRFMT,C'J'                                                    
         BE    CHKE0080                                                         
         MVI   2(R6),3             ENTERPRISE                                   
         CLI   TWATRFMT,C'K'                                                    
         BE    CHKE0080                                                         
         MVI   2(R6),4             COLUMBINE                                    
         CLI   TWATRFMT,C'C'                                                    
         BE    CHKE0080                                                         
         MVI   2(R6),5             VCI                                          
         CLI   TWATRFMT,C'S'                                                    
         BNE   CHKE0120                                                         
         DROP  RF                                                               
*                                                                               
CHKE0080 DS    0H                                                               
         MVC   3(L'REOPKSTA,R6),RCONKSTA   STATION+MEDIA                        
                                                                                
         ZICM  RF,WORK+5,3                                                      
         ZIC   R1,WORK+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),0(RF)       ADVERTISER/AGENCY/OFFICE/SALESPERSON         
                                                                                
*                                                                               
*   VALUE IN 'HALF' IS SET LOCALLY, IF THE AGENCY2 RECORD CONTAINS              
*        AN ALTERNATE AGENCY CODE VALUE.  THE AGENCY OFFICE IS THEN             
*        OVERLAID BY THE SPECIAL CHARACTER + ALTERNATE AGENCY CODE              
*        PRIOR TO ACCESSING THE RECORD.                                         
*                                                                               
         CLI   HALF,C' '           ANY TRADE ALT AGENCY CODE?                   
         BNH   CHKE0100            NO                                           
         MVI   12(R6),C'#'         YES - INSERT INDICATOR                       
         MVC   13(1,R6),HALF       INSERT TRADE ALT AGENCY CODE                 
CHKE0100 EQU   *                                                                
*        OI    DMINBTS,X'08'       READ DELETES                                 
         MVC   KEY(L'REOPKEY),IOAREA                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
DIE      EQU   *                                                                
         BNE   CHKE0120                                                         
                                                                                
*        OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R6,IOAREA                                                        
         USING REOPREC,R6                                                       
*        TM    REOPFLAG,X'80'      IF ORDER ACTIVE                              
*        BZ    CHKE0120                                                         
*        OI    1(R2),X'20'         STATION CANNOT CHANGE THIS FIELD             
         MVC   8(6,R2),REOPEQUV                                                 
         DROP  R6                                                               
                                                                                
CHKE0120 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONMOD+L'CONMOD                                                  
       ++INCLUDE RECNTF0D                                                       
GENSAL2  DSECT                                                                  
       ++INCLUDE REGENSAL2                                                      
         EJECT                                                                  
*                                                                               
T80249   CSECT                                                                  
         DROP  RB                                                               
GETORD   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'ORD=',8(R2)       HAVE 'ORD=NNNNNNNN' SYNTAX?                 
         BE    GORD010              YES                                         
         SR    R0,R0                NO - SET CC                                 
         B     EXXMOD               BYE                                         
*                                                                               
GORD010  DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),AIO4                                           
         LA    R3,2                                                             
         CLI   4(R1),1                                                          
         BNE   ERROR                                                            
         L     R4,AIO4              SCANNER OUTPUT                              
         TM    3(R4),X'80'         NUMERIC?                                     
         BZ    ERROR                                                            
*                                                                               
         XC    KEY,KEY              LOOKUP CONTRACT                             
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),REPALPHA                                               
         GOTOX (RFCONNUM,VREPFACS),DMCB,(7,8(R4)),(2,KEY+23)                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         CLC   RCONKCON,23(R6)     SAME K?                                      
         BE    ERROR                                                            
*                                                                               
         MVI   ELCODE,X'82'                                                     
         BAS   RE,GETEL                                                         
         BE    GORD020                                                          
         LA    R3,757              NO CMT ON SOURCE K                           
         B     ERROR                                                            
GORD020  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,0(R6)                                      
         BAS   RE,NEXTEL                                                        
         BE    GORD020                                                          
*                                                                               
         LTR   RB,RB                                                            
         B     EXXMOD                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DISASST  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY        READ S/P2 RECORD                                  
K        USING RSA2KEY,KEY                                                      
         MVI   KEY,X'46'                                                        
         MVC   K.RSA2KREP,RCONKREP                                              
         MVC   K.RSA2KSAL,RCONSAL                                               
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DISASSTX                                                         
         DROP  K                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,ORDASSTH                                                      
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISASSTX                                                         
*                                                                               
         USING RSASEMEM,R6                                                      
         MVC   8(L'ORDASST,R2),RSASEMNM                                         
         DROP  R6                                                               
DISASSTX B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'202RECNT49   05/04/18'                                      
         END                                                                    
