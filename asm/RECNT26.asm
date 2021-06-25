*          DATA SET RECNT26    AT LEVEL 108 AS OF 07/26/13                      
*PHASE T80226A                                                                  
*INCLUDE OUTDAY                                                                 
*INCLUDE REGENPBB                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE UNTIME                                                                 
         TITLE 'T80226 - REPPAK MULTIPLE BUY DISPLAY'                           
***********************************************************************         
*                                                                     *         
*  RECNT26 -- MULTIPLE BUY DISPLAY FOR A CONTRACT                     *         
*                                                                     *         
*  HISTORY:                                                           *         
*                                                                     *         
*  09/05/89  PJS  FIX STARTING NUMBER FILTER FOR NEW DATAMGR.         *         
*                 (CAN'T SAVE LINE NUMBER IN KEYSAVE ANYMORE)         *         
*                                                                     *         
*  09/16/92  BU   'REGENPBY' --> 'REGENPBB': CHANGES POSITIONING OF   *         
*                 FIELDS ON SCREEN AT BLAIR'S REQUEST.                *         
*                 'REGENPBYD' --> 'REGENPBYB' (DSECT CHANGE)          *         
*                 INCLUDE OF 'REGENPBY' --> 'REGENPBB'                *         
*                                                                     *         
*  10/06/95  SKU  2K CONTRACT SUPPORT                                 *         
*                                                                     *         
*  11JUN96 RHV DISPLAY MONTHLY # OF SPOT TOTALS                                 
*                                                                     *         
*  24APR00 SKU <MORE> TAG AND MULTI-MAKEGOOD SUPPORT IN REGENPBBY     *         
*                                                                     *         
*  01JUN00 BU  TRADE BUY DISPLAY, EXPANDED TOTALS                     *         
*                                                                     *         
*  25OCT00 RHV SPORTS BUY SUPPORT                                     *         
*                                                                     *         
*  19JAN01 RHV PGM= DISPLAY                                           *         
*                                                                     *         
*  21MAR02 SKU FIX DSM/NEXT BUG AND REPLACEMENT OFFER SUPPORT         *         
*                                                                               
*  01JUL02 HQ  DEMO CAT AND VALUE DISPLAY                             *         
*                                                                     *         
*  15MAY12 BOB ADD GRPS TO DISPLAY                                    *         
*                                                                     *         
*  26JUL13 SKU FIX NOT INITIALIZING REGISTER IN MONCASE               *         
*                                                                     *         
***********************************************************************         
T80226   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80226,RR=R5                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=H'4096'         4K                                           
         USING TWAWORK,R7                                                       
* FOUT AND CLEAR NON-ZERO LINES                                                 
MAIN0020 LA    R2,LINLIN1H                                                      
         LA    R3,LINLAST-9                                                     
         SR    RE,RE                                                            
*                                                                               
MAIN0040 EQU   *                                                                
         NI    1(R2),X'FF'-X'08'   TURN OFF HIGH-INTENSITY                      
         OC    8(79,R2),8(R2)                                                   
         BZ    MAIN0060                                                         
         XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
MAIN0060 IC    RE,0(R2)            FIELD LEN                                    
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3                                                            
         BL    MAIN0040                                                         
*                                                                               
MAIN0080 MVC   KEY+28(4),TWAKADDR  K DISK ADDR                                  
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         L     RE,=V(OUTDAY)                                                    
         AR    RE,R5                                                            
         ST    RE,DMCB2                                                         
         L     RE,=V(UNTIME)                                                    
         AR    RE,R5                                                            
         ST    RE,DMCB2+4                                                       
         MVC   DMCB2+8(4),DATCON                                                
         L     RE,DEMCON           REALLY DEMOCON (RELOCATED IN T80280)         
         ST    RE,DMCB2+20                                                      
         SR    R6,R6               LINE CTR                                     
         LA    R2,LINLIN1H                                                      
*                                                                               
         MVI   RBUYKTYP,X'0B'      BUILD BUY KEY                                
         MVC   RBUYKREP,REPALPHA   REP                                          
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
*                                                                               
         LA    R2,CONBNUMH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    MAIN0090            YES                                          
         MVI   TWATRFLG,0          NO                                           
MAIN0090 EQU   *                                                                
         MVI   BYFLT,0             ZERO FLIGHT NUMBER                           
         CLI   CONBNUM,C'F'        TEST FOR FLIGHT NUMBER FILTER PREFIX         
         BNE   MAIN0100            NOT THERE                                    
         SPACE                                                                  
         LA    R3,INVINP                                                        
         CLI   5(R2),1             TEST INPUT LENGTH FOR UP TO                  
         BE    ERROR               2 DIGIT FLIGHT NUMBER                        
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         SPACE                                                                  
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         MVC   WORK(3),=3X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),CONBNUM+1                                                
         CLC   WORK(3),=3X'F0'                                                  
         BL    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CONBNUM+1(0)                                                 
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR               ZERO IS INVALID FLIGHT NUMBER                
         STC   R0,BYFLT                                                         
         B     MAIN0100                                                         
         SPACE                                                                  
MAIN0100 GOTO1 VPACK                                                            
*                                                                               
         STC   R0,STARTLIN         STARTING LINE NUMBER                         
*                                                                               
         LA    R2,LINLIN1H                                                      
         CLC   CONBNUM(4),=C'NEXT'                                              
         BE    MAIN0140                                                         
MAIN0120 XC    TWALSTKY,TWALSTKY                                                
         XC    TWALSTPL,TWALSTPL                                                
         XC    TWATTLAR,TWATTLAR                                                
         XC    TWATRADE,TWATRADE                                                
         XC    TWATOTAL,TWATOTAL                                                
         LAY   RF,TWABYTOT                                                      
         XC    0(256,RF),0(RF)     INIT TOTAL AREA                              
         LAY   RF,TWABYTRD                                                      
         XC    0(256,RF),0(RF)     INIT TRADE AREA                              
         B     MAIN0160                                                         
*                                                                               
* NEXT PAGE REQUESTED                                                           
MAIN0140 CLC   TWALSTKY(22),RBUYKEY     SAME K?                                 
         BNE   MAIN0120                                                         
         MVC   RBUYKEY+22(5),TWALSTKY+22                                        
*                                                                               
MAIN0160 MVC   KEY,RBUYKEY                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   CONBNUM(4),=C'NEXT' NEXT REQUEST?                                
         BNE   NEXTBUY1            NO - DON'T SKIP A REC                        
         CLC   KEY(22),KEYSAVE     NOTHING TO SHOW NEXT                         
         BNE   TOTAL                                                            
*                                                                               
NEXTBUY  OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
NEXTBUY1 CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   TOTAL                                                            
         TM    KEY+27,X'C0'        VOID?                                        
         BO    NEXTBUY                                                          
*                                                                               
* GET BUY RECORD                                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
*   FOR NEW 'COMBO' FACILITY, A BUY ACTION OF 'DSS' CAN FORCE                   
*     A COMBO ORDER TO ENTER THIS MODULE FOR THE DISPLAY OF A                   
*     SINGLE STATION'S PORTION OF THAT ORDER.  IF SUCH AN ORDER                 
*     IS REQUESTED, 'NA' BUYLINES SHOULD NOT - REPEAT, NOT - BE                 
*     DISPLAYED ON THIS SCREEN.                                                 
*                                                                               
         TM    RBUYCOMB,X'80'      IS 'NA' FLAG SET FOR BUYLINE?                
         BO    NEXTBUY             YES - SKIP THIS BUYLINE                      
* DISPLAY BUY                                                                   
         XC    BYTE,BYTE                                                        
         GOTO1 CKSTPROF            CHECK STATION PROFILE                        
         BE    MAIN0170                                                         
         OI    BYTE,X'02'          NO, DO NOT PRINT DEMO                        
*&&DO                                                                           
*        LR    RF,RA                                                            
*        AH    RF,=Y(TWAWORKQ)     4K                                           
*        USING TWAWORK,RF                                                       
         CLI   TWAACCS,C'$'        ONLY CHK PROFILE BIT FOR STATION             
         BNE   MAIN0170                                                         
         TM    TWASTAOB,X'02'      STATION PROFILE BIT = NO DEM PRT             
         BZ    *+8                                                              
         OI    BYTE,X'02'          NO, DO NOT PRINT DEMO                        
*        DROP  RF                                                               
*&&                                                                             
MAIN0170 DS    0H                                                               
         MVC   SVDEMFLG,BYTE       SAVE DEMO PRINT OPTION                       
         NI    SVDEMFLG,X'02'                                                   
*                                                                               
         XC    DMCB2+12(4),DMCB2+12   NO ALT WEEK EXPLOSION                     
         MVC   DMCB2+16(4),VREPFACS                                             
         GOTO1 =V(REGENPBB),DMCB,(BYTE,RBUYREC),(12,AIO2),DMCB2,       +        
               RCONREC,PROFILES,ACOMFACS,RR=Y                                   
         CLI   DMCB+4,0            NO LINES?                                    
         BE    NEXTBUY                                                          
*                                                                               
* DISPLAY LINE                                                                  
         CLC   CONBNUM(3),=C'TOT'  TOTALS ONLY?                                 
         BE    MAIN0240                                                         
*                                                                               
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   MAIN0240            NO                                           
*                                                                               
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BE    MAIN0180                                                         
*                                                                               
         CLC   KEY+26(1),STARTLIN                                               
         BNE   MAIN0240                                                         
         MVI   STARTLIN,0          DON'T USE AGAIN                              
*                                                                               
MAIN0180 SR    RE,RE                                                            
         IC    RE,DMCB+4           NUMBER OF BUY DISPLAY LINES                  
         AR    R6,RE                                                            
         CH    R6,=H'12'           MAX                                          
         BH    EXIT0120                                                         
*                                                                               
         L     R8,AIO2                                                          
MAIN0200 MVC   8(79,R2),0(R8)                                                   
*                                                                               
         CLI   DMCB+4,11                                                        
         BNH   MAIN0220                                                         
         CHI   RE,1                                                             
         BNE   MAIN0220                                                         
         MVC   77(6,R2),=C'<MORE>'                                              
*                                                                               
MAIN0220 EQU   *                                                                
         NI    1(R2),X'FF'-X'08'   TURN OFF HIGH-INTENSITY                      
         FOUT  (R2)                                                             
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         LA    R2,0(R4,R2)         NEXT FIELD                                   
         LA    R8,L'PRTLN(R8)                                                   
         BCT   RE,MAIN0200         LOOP TO DISPLAY BUY LINES                    
*                                                                               
         MVC   TWALSTKY,KEY                                                     
*                                                                               
MAIN0240 TM    RBUYCNTL,X'80'      DELETED?                                     
         BO    NEXTBUY             DON'T ADD INTO TOTALS                        
*                                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    NEXTBUY             DON'T ADD INTO TOTALS                        
*                                                                               
         MVC   WORK(4),VGTBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
*                                                                               
*        GET BUCKETS WITH GRPS                                                  
*                                                                               
***>     GOTO1 =V(REGENBUC),DMCB,RBUYREC,AIO4,WORK,RR=YES                       
         GOTO1 (RFGENBUC,VREPFACS),DMCB,RBUYREC,AIO4,(X'40',WORK)               
*                                                                               
         L     RF,AIO4             POINT TO RETURNED BUCKETS                    
*                                                                               
         CLC   0(2,RF),=H'2'      NO DATA                                       
         BE    NEXTBUY                                                          
*                                                                               
         SR    R0,R0                                                            
* ADD BUY BUCKETS TO TOTAL CONTRACT BUCKETS                                     
         L     R3,AIO4                                                          
         LA    R3,2(R3)            FIRST BUCKET                                 
MAIN0260 EQU   *                                                                
*****    LA    R4,TWATTLAR+2       SET A(CASH  ARRAY)                           
         LAY   R4,TWABYTOT+2       SET A(CASH  ARRAY)                           
*                                                                               
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   MAIN0280            NO                                           
*                                                                               
*****    LA    R4,TWATRADE+2       YES - SET A(TRADE ARRAY)                     
         LAY   R4,TWABYTRD+2       YES - SET A(TRADE ARRAY)                     
*                                                                               
MAIN0280 CLI   0(R4),0             LAST?                                        
         BE    MAIN0300                                                         
*                                                                               
         CLC   2(2,R3),2(R4)       COMPARE BUCKET DATES                         
         BL    MAIN0300                                                         
         BE    MAIN0360                                                         
*                                                                               
* GET NEXT TOTAL BUCKET                                                         
*                                                                               
         LLC   RF,1(R4)            LEN                                          
         AR    R4,RF                                                            
         B     MAIN0280                                                         
*                                                                               
* ADD NEW TOTAL BUCKET                                                          
*                                                                               
MAIN0300 EQU   *                                                                
*                                                                               
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    MAIN0320            YES                                          
*                                                                               
*****    GOTO1 VRECUP,DMCB,(X'FF',TWATTLAR),(R3),(R4)                           
         LAY   RF,TWABYTOT                                                      
         GOTO1 VRECUP,DMCB,(X'FF',0(RF)),(R3),(R4)                              
*                                  ADD TO CASH  TOTAL ARRAY                     
         B     MAIN0340                                                         
MAIN0320 EQU   *                                                                
*****    GOTO1 VRECUP,DMCB,(X'FF',TWATRADE),(R3),(R4)                           
         LAY   RF,TWABYTRD                                                      
         GOTO1 VRECUP,DMCB,(X'FF',0(RF)),(R3),(R4)                              
*                                  ADD TO TRADE TOTAL ARRAY                     
*                                                                               
* GET NEXT BUY BUCKET                                                           
*                                                                               
MAIN0340 IC    R0,1(R3)            LENGTH                                       
         AR    R3,R0                                                            
*                                                                               
         CLI   0(R3),0             LAST?                                        
         BNE   MAIN0260                                                         
*                                                                               
         MVC   TWALSTKY,KEY                                                     
*                                                                               
         B     MAIN0380            ADD TO GRAND TOTALS                          
*                                                                               
* ADD TO CASH OR TRADE TOTAL BUCKET                                             
*                                                                               
MAIN0360 MVC   DUB(8),6(R3)        CASH, SPOTS                                  
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R4)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R4),DUB                                                      
*                                                                               
         ICM   RF,15,14(R4)        GRPS                                         
         ICM   RE,15,14(R3)                                                     
         AR    RE,RF                                                            
         STCM  RE,15,14(R4)                                                     
*                                                                               
         B     MAIN0340                                                         
*                                                                               
* ADD TO CASH + TRADE TOTAL BUCKET                                              
*                                                                               
MAIN0380 EQU   *                                                                
         L     R3,AIO4                                                          
         LA    R3,2(R3)            FIRST BUCKET                                 
MAIN0390 EQU   *                                                                
****     LA    R4,TWATOTAL+2       SET A(CASH+TRADE TOTAL ARRAY)                
         LAY   R4,TWABYTTL+2       SET A(CASH+TRADE TOTAL ARRAY)                
*                                                                               
MAIN0400 EQU   *                                                                
*                                                                               
         CLI   0(R4),0             LAST?                                        
         BE    MAIN0420                                                         
*                                                                               
         CLC   2(2,R3),2(R4)       COMPARE BUCKET DATES                         
         BL    MAIN0420                                                         
         BE    MAIN0460                                                         
*                                                                               
* GET NEXT TOTAL BUCKET                                                         
*                                                                               
         IC    R0,1(R4)            LEN                                          
         AR    R4,R0                                                            
         B     MAIN0400                                                         
*                                                                               
MAIN0420 EQU   *                                                                
*                                                                               
******   GOTO1 VRECUP,DMCB,(X'FF',TWATOTAL),(R3),(R4)                           
         LAY   RF,TWABYTTL                                                      
         GOTO1 VRECUP,DMCB,(X'FF',0(RF)),(R3),(R4)                              
*                                  ADD TO CASH+TRADE TOTAL ARRAY                
* GET NEXT BUY BUCKET                                                           
*                                                                               
MAIN0440 IC    R0,1(R3)            LENGTH                                       
         AR    R3,R0                                                            
*                                                                               
         CLI   0(R3),0             LAST?                                        
         BNE   MAIN0390                                                         
*                                                                               
         MVC   TWALSTKY,KEY                                                     
         B     NEXTBUY                                                          
*                                                                               
* ADD TO CASH OR TRADE TOTAL BUCKET                                             
*                                                                               
MAIN0460 MVC   DUB(8),6(R3)                                                     
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R4)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R4),DUB                                                      
*                                                                               
         ICM   RF,15,14(R4)        GRPS                                         
         ICM   RE,15,14(R3)                                                     
         AR    RE,RF                                                            
         STCM  RE,15,14(R4)                                                     
*                                                                               
         B     MAIN0440                                                         
*                                                                               
*              DISPLAY TOTALS - GET CONTRACT                                    
*                                                                               
TOTAL    DS    0H                                                               
*                                                                               
         TM    RCONMODR+1,X'20'    MON DATA ADDED?                              
         BZ    *+8                                                              
         BAS   RE,MONCASE                                                       
*                                                                               
         CLI   TWATRFLG,0          ANY RESTART? (0 = NO RESTART)                
         BNE   TOTA0100            YES - CHECK NEXT LEVEL                       
*****                                                                           
*****    OC    TWATTLAR,TWATTLAR   YES - ANY CASH BUYS?                         
*****    BNZ   TOTA0040            YES                                          
*****    OC    TWATRADE,TWATRADE   ANY TRADE BUYS?                              
*****    BNZ   TOTA0040            YES -                                        
*                                                                               
         LAY   RF,TWABYTOT                                                      
         OC    0(256,RF),0(RF)     YES - ANY CASH BUYS?                         
         BNZ   TOTA0040            YES                                          
*                                                                               
         LAY   RF,TWABYTRD                                                      
         OC    0(256,RF),0(RF)     ANY TRADE BUYS?                              
         BNZ   TOTA0040            YES -                                        
*                                                                               
         MVC   68(15,R2),=C'TOTAL      $.00'                                    
*                                                                               
******   OC    TWATRADE,TWATRADE   ANY TRADE BUYS                               
         LAY   RF,TWABYTRD                                                      
         OC    0(256,RF),0(RF)     ANY TRADE BUYS?                              
         BZ    TOTA0020            NO                                           
*                                                                               
         MVC   74(04,R2),=C'CASH'  YES - INDICATE 'NO CASH'                     
*                                                                               
TOTA0020 EQU   *                                                                
*                                                                               
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     TOTA0100                                                         
*                                                                               
TOTA0040 DS    0H                                                               
         LAY   RF,TWABYTOT                                                      
         GOTO1 =V(REGENTL2),DMCB,(SVDEMFLG,0(RF)),AIO2,RR=YES                   
*                                                                               
*  MOVE BUYLINE TOTALS TO SCREEN                                                
*                                                                               
         L     R8,AIO2                                                          
*                                                                               
         ZIC   RE,DMCB+4           NUMBER OF TOTAL LINES                        
         AR    R6,RE                                                            
*                                                                               
         LAY   RF,TWABYTRD                                                      
         OC    0(256,RF),0(RF)     ANY TRADE FIGURES                            
         BZ    TOTA0060            NO                                           
*                                                                               
         LA    R6,1(R6)            YES - ADD FOR 'CASH' DISPLAY LINE            
*                                                                               
TOTA0060 EQU   *                                                                
*                                                                               
         CH    R6,=H'12'           MAX 12 DISPLAY LINES                         
         BH    EXIT0120                                                         
*                                                                               
         LAY   RF,TWABYTRD                                                      
         OC    0(256,RF),0(RF)     ANY TRADE FIGURES                            
         BZ    TOTA0080            NO  - PROCESS AS REGULAR ORDER &             
*                                     DON'T SHOW 'CASH' BANNER                  
*                                  YES - SHOW 'CASS' BANNER IF THERE            
*                                     IS ACTUALLY CASH                          
         LAY   RF,TWABYTOT                                                      
         OC    0(256,RF),0(RF)     ANY CASH DOLLARS IN ARRAY?                   
         BZ    TOTA0100            NO  - SKIP CASH DISPLAY TOTALLY              
*                                  YES - DISPLAY 'CASH' BANNER                  
         MVC   08(09,R2),=C'**CASH **'                                          
         MVC   65(09,R2),=C'**CASH **'                                          
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
*                                                                               
TOTA0080 EQU   *                                                                
*                                                                               
         MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,TOTA0080                                                      
*                                                                               
TOTA0100 EQU   *                                                                
*                                                                               
         LAY   RF,TWABYTRD                                                      
         OC    0(256,RF),0(RF)     ANY TRADE BUYS                               
         BZ    EXIT0020            NO  - NO MORE TO DISPLAY                     
*                                                                               
         CLI   TWATRFLG,1          NO RESTART OR START AT SECOND LEVEL?         
         BH    TOTA0160            NO  - CHECK NEXT LEVEL                       
*                                                                               
TOTA0120 LAY   RF,TWABYTRD                                                      
         GOTO1 =V(REGENTL2),DMCB,(SVDEMFLG,0(RF)),AIO2,RR=YES                   
*                                                                               
*  MOVE BUYLINE TRADE TOTALS TO SCREEN                                          
*                                                                               
         MVI   TWATRFLG,1          SET RESTART LEVEL                            
         L     R8,AIO2                                                          
         ZIC   RE,DMCB+4           NUMBER OF TOTAL LINES                        
         AR    R6,RE                                                            
         LA    R6,1(R6)            ADD 1 FOR 'TRADE LINE INDICATOR'             
         CH    R6,=H'12'                                                        
         BH    EXIT0120                                                         
         MVI   TWATRFLG,0          CLEAR RESTART AT 2ND LEVEL                   
         MVC   08(09,R2),=C'**TRADE**'                                          
         MVC   65(09,R2),=C'**TRADE**'                                          
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
*                                                                               
TOTA0140 MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,TOTA0140                                                      
*                                                                               
         LAY   RF,TWABYTOT                                                      
         OC    0(256,RF),0(RF)     ANY CASH BUYS?                               
         BZ    EXIT0020            NO  - JUST TRADE                             
*                                                                               
TOTA0160 EQU   *                                                                
*                                                                               
         CLI   TWATRFLG,2          NO RESTART OR START AT 1ST/2ND LVL?          
         BNH   *+6                 YES - NO MORE LEVELS                         
         DC    H'0'                UNKNOWN VALUE  ??                            
*                                                                               
         LAY   RF,TWABYTTL         BUY AND TRADE TOTALS                         
         GOTO1 =V(REGENTL2),DMCB,(SVDEMFLG,0(RF)),AIO2,RR=YES                   
*                                                                               
*  MOVE BUYLINE TOTAL TOTALS TO SCREEN                                          
*                                                                               
         MVI   TWATRFLG,2          SET RESTART TO 3RD LEVEL                     
*                                                                               
         L     R8,AIO2                                                          
         ZIC   RE,DMCB+4           NUMBER OF TOTAL LINES                        
         AR    R6,RE                                                            
         LA    R6,1(R6)            ADD 1 FOR 'TOTAL LINE INDICATOR'             
         CH    R6,=H'12'                                                        
         BH    EXIT0120                                                         
         MVI   TWATRFLG,0          CLEAR RESTART AT 3RD LEVEL                   
         MVC   08(09,R2),=C'**TOTAL**'                                          
         MVC   65(09,R2),=C'**TOTAL**'                                          
         OI    1(R2),X'08'         TURN ON HIGH INTENSITY                       
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
*                                                                               
TOTA0180 MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,TOTA0180                                                      
* DISPLAY TOTALS                                                                
EXIT0020 LA    R2,CONBACTH         CURSOR                                       
         XC    DMCB(24),DMCB                                                    
         SR    R3,R3                                                            
         MVI   DMCB+3,53                                                        
         CLI   BYFLT,0             HAVE BUYS BEEN FILTERED BY FLIGHT            
         BE    EXIT0040            NO                                           
         MVI   DMCB+3,54                                                        
         EDIT  (B1,BYFLT),(3,TEMP),ALIGN=LEFT                                   
         LR    R3,R0                                                            
         B     EXIT0060                                                         
*                                                                               
EXIT0040 CLC   CONBNUM(3),=C'TOT'   TOTALS ONLY?                                
         BNE   EXIT0060                                                         
         MVI   DMCB+3,55                                                        
EXIT0060 FOUT  CONBNUMH,MYSPACES,8                                              
EXIT0080 OI    CONBACTH+4,X'20'                                                 
         OI    CONBNUMH+4,X'20'                                                 
         LTR   R3,R3                                                            
         BZ    EXIT0100                                                         
         GOTO1 VDISMSG,DMCB,,0,0,((R3),TEMP)                                    
         B     EXIT                                                             
EXIT0100 GOTO1 VDISMSG,DMCB,,                                                   
         B     EXIT                                                             
*                                                                               
EXIT0120 DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         SR    R3,R3                                                            
         NI    CONBACTH+6,X'BF'    NO CURSOR                                    
         MVI   DMCB+3,56            HIT ENTER FOR NEXT SCRN                     
         CLI   BYFLT,0             DISPLAY THE FLIGHT FILTER IF ANY             
         BE    EXIT0140            NONE                                         
         MVI   DMCB+3,57            HIT ENTER FOR NEXT SCRN - FLIGHT NN         
         EDIT  (B1,BYFLT),(3,TEMP),ALIGN=LEFT                                   
         LR    R3,R0                                                            
EXIT0140 LA    R2,CONBNUMH                                                      
         MVC   8(4,R2),=C'NEXT'                                                 
         OI    1(R2),X'01'         TURN ON MODIFIED FOR AUTO PAGING             
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         OI    4(R2),X'20'         SET PREVIOUSLY VALID BIT                     
         B     EXIT0080                                                         
*                                                                               
*  ROUTINE TO BUFFER ACTUAL K '03' BUCKETS (CASE OF MON DATA)                   
*                                                                               
MONCASE  NTR1                                                                   
*                                                                               
         LAY   R5,TWABYTOT                                                      
         MVC   0(2,R5),=X'0002'                                                 
*                                                                               
         XC    FULL,FULL           INIT BUCKET YR/MONTH                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL            FIND CONTRACT BUCKET ELEMENT                 
         B     *+8                                                              
CONC10   BAS   RE,NEXTEL           FIND NEXT BUCKET ELEMENT                     
         BNE   MONCX               DONE IF NONE FOUND                           
*                                                                               
         CLC   FULL(2),2(R6)       IF YR/MONTH UNCHANGED                        
         MVC   FULL(2),2(R6)          (SAVE YEAR/MONTH)                         
         BNE   CONC15                                                           
*                                                                               
         ICM   R1,15,6(R4)            INCREMENT COSTS FOR MONTH                 
         ICM   R0,15,6(R6)                                                      
         AR    R1,R0                                                            
         STCM  R1,15,6(R4)                                                      
*                                                                               
         B     CONC10                 GO PROCESS NEXT BUCKET ELEMENT            
*                                                                               
CONC15   XC    WORK,WORK           INIT WORK AREA                               
         ZIC   R2,1(R6)            BUCKET ELM LENGTH                            
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)       MOVE BUCKET ELEMENT TO WORKAREA              
*                                                                               
*        ADDELEMENT TO BUFFER                                                   
*                                                                               
         MVI   WORK+1,X'12'        SET LENGTH OF BUCKET                         
         SR    R3,R3               CLEAR REGISTER                               
         ICM   R3,3,0(R5)          CURRENT LENGTH OF BUCKETS                    
         LA    R4,0(R3,R5)         POINT TO NEXT AVAILABLE AREA                 
         GOTO1 VRECUP,DMCB,(X'FF',0(R5)),WORK,(R4) ADD BKT ELM AT END           
*                                                                               
         B     CONC10                                                           
*                                                                               
MONCX    XIT1                                                                   
*                                                                               
*                                                                               
* CHECK STATION PROFILES TO SEE IF WE NEED TO DISPLAY DEMO OR NOT               
*                                                                               
CKSTPROF NTR1                                                                   
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   CKSTPRY                                                          
*                                                                               
         TM    PROFILES+CNTDEMOB,CNTDEMOA                                       
         BO    CKST0100            DISALLOW STATION TO SEE DEMO                 
*                                                                               
         TM    TWASTAOB,X'02'                                                   
         BO    CKSTPRN             DO NOT PROCESS DEMO                          
         B     CKSTPRY                                                          
CKST0100 DS    0H                                                               
         TM    TWASTAOB,X'02'                                                   
         BZ    CKSTPRN                                                          
*                                                                               
CKSTPRY  SR    RC,RC                                                            
CKSTPRN  LTR   RC,RC                                                            
CKSTPRX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         SPACE 2                                                                
STARTLIN DS    CL1                 1 BYTE BINARY STARTING LINE #                
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFDD                                                       
       ++INCLUDE REGENPBYB                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108RECNT26   07/26/13'                                      
         END                                                                    
