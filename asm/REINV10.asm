*          DATA SET REINV10    AT LEVEL 165 AS OF 05/01/02                      
*PHASE T80310A,*                                                                
*INCLUDE GETBROAD                                                               
         TITLE 'REINV10 (T80310) - REP MULTIPLE INVOICE CHANGE'                 
*                                                                               
***********************************************************************         
*                                                                     *         
*- REINV10 (T80310) --- REPPAK MULTIPLE INVOICE CHANGE MODULE         *         
*                                                                     *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
*  MOD LOG                                                            *         
*  -------                                                            *         
*                                                                     *         
*  08/24/89  PJS  CHANGE PHASE TO 'A' LEVEL                           *         
*                                                                     *         
*  11/02/89  PJS  CHECK FOR RECORD FULL FROM RECUP CALL AND FLAG      *         
*                 FIELD WITH '*REC FULL*' MESSAGE.                    *         
*                                                                     *         
*  04OCT90   EFJ  OUTPUT MINUS SIGN ON LARGE FLDS - SEE BASE MOD LOG  *         
*                                                                     *         
*  16AUG94   SKU  REP PROFILE DOUBLE LINE DISPLAY WITH INVOICE#       *         
*                                                                     *         
*  19JUL95   SKU  FIX BUG OF GOING TO GETREC WITH A ZERO DISK ADDRESS *         
*                                                                     *         
*  08MAY96   SMP  NEW DIFFERENCE COLUMN                               *         
*                                                                     *         
*  07MAR97   SKU  BUG FIX                                             *         
*                                                                     *         
*  24SEP97   BU   ALTERNATE INVOICE BUCKET CREATE                     *         
*                                                                     *         
*  06APR97   BU   INVOICE BUCKETS - ALTERNATE:  BACK UP ACTIVITY DATE *         
*                 FOR SIMULATED 'BACK PACING'                         *         
*                                                                     *         
*  19MAY00   BU   ADD TRADE PROCESSING                                *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                 ***   END TOMBSTONE   ***                           *         
***********************************************************************         
*                                                                               
T80310   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80310,R8                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T803FFD,RA                                                       
         EJECT                                                                  
* GET CONTRACTS THAT ARE DISPLAYED AND EDIT INVOICE DIFFERENCES                 
         LA    R2,INVLN01H         FIRST OUTPUT LINE                            
         LR    R5,R2                                                            
         LA    R9,TWACLIST         CONTRACT DISK ADDR LIST                      
         XC    INVTOT,INVTOT       INVOICE AMT ACCUMULATOR                      
* GET TO NEXT INVOICE FIELD                                                     
C50      SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               INVOICE FIELD                                
                                                                                
         C     R2,LASTFLD          LAST FIELD INPUT                             
         BNH   C75                                                              
         L     RE,4(R9)            INVOICE AMOUNT                               
* LOOP TO REDISPLAY INVOICE DATA WHICH WAS WIPED OUT BY CURSOR                  
         XC    8(10,R2),8(R2)                                                   
         CLC   4(4,R9),=C'ZERO'    0 INPUT                                      
         BNE   C60                                                              
         SR    RE,RE                                                            
         B     C62                                                              
C60      LTR   RE,RE                                                            
         BZ    C65                                                              
*                                                                               
         L     R6,INVTOT           ADD TO ACCUMULATOR                           
         AR    R6,RE                                                            
         ST    R6,INVTOT                                                        
*                                                                               
C62      EQU   *                                                                
         LPR   R0,RE                                                            
         C     R0,=F'100000000'    VALUE 1,000,000.00 OR >?                     
         BL    C63                 YES - USE REGULAR EDIT                       
         EDIT  (RE),(11,8(R2)),2,MINUS=YES                                      
         B     C65                                                              
C63      EQU   *                                                                
         EDIT  (RE),(11,8(R2)),2,COMMAS=YES,MINUS=YES                           
C65      EQU   *                                                                
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALID                                        
         B     C90                                                              
*                                                                               
* TEST IF INVOICE DATA CHANGED                                                  
C75      DS    0H                                                               
         TM    4(R2),X'20'         CHANGED                                      
         BZ    C300                                                             
                                                                                
* NOT CHANGED                                                                   
C78      DS    0H                                                               
         CLC   4(4,R9),=C'ZERO'    0 INPUT                                      
         BE    C90                                                              
         L     R6,INVTOT           ADD TO ACCUMULATOR                           
         A     R6,4(R9)                                                         
         ST    R6,INVTOT                                                        
                                                                                
C90      ZIC   R0,0(R2)            GET TO DIFFERENCE FIELD                      
         AR    R2,R0                                                            
                                                                                
C100     CLI   SVREPROF+7,C'Y'     CHECK IF DOUBLE DISPLAY                      
         BNE   C110                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            INVOICE FIELD                                
         AR    R2,R0                                                            
         TM    4(R2),X'20'         CHECK IF INVOICE# CHANGED                    
         BZ    C500                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               DIFFERENCE FIELD                             
*                                                                               
* GET NEXT LINE                                                                 
*                                                                               
C110     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               NEXT CONTRACT LINE                           
         LR    R5,R2                                                            
         LA    RF,INVLN18H                                                      
         CR    R2,RF               LAST CONTRACT LINE?                          
         BNH   C200                                                             
*                                                                               
* END                                                                           
END      MVC   INVMSG(L'INVMSG),SPACES                                          
         MVC   INVMSG(29),=C'CONTRACT INVOICE DATA CHANGED'                     
         FOUT  INVMSGH                                                          
*                                                                               
*   NEED TO KNOW IF ACTION WAS CHA/CHN/CTR/CTA TO RESET                         
*                                                                               
         CLC   =C'CHA',INVACT      REGULAR INVOICE CHANGE?                      
         BNE   END02               NO                                           
         MVC   INVACT(3),=C'DIS'                                                
         B     END04                                                            
END02    EQU   *                                                                
         CLC   =C'CHN',INVACT      ALTERNATE INVOICE CHANGE?                    
         BNE   END02A              NO                                           
         MVC   INVACT(3),=C'DIN'                                                
         B     END04                                                            
END02A   EQU   *                                                                
         CLC   =C'CTR',INVACT      TRADE INVOICE CHANGE?                        
         BNE   END02B              NO                                           
         MVC   INVACT(3),=C'DIT'                                                
         B     END04                                                            
END02B   EQU   *                                                                
         CLC   =C'CTA',INVACT      TRADE/ALTERNATE INVOICE CHANGE?              
         BNE   END02C              NO                                           
         MVC   INVACT(3),=C'DTA'                                                
         B     END04                                                            
END02C   EQU   *                                                                
         DC    H'0'                HOW'D WE GET HERE?                           
END04    EQU   *                                                                
         FOUT  INVACTH                                                          
*                                                                               
         CLI   SVREPROF+7,C'Y'     CHECK IF DOUBLE DISPLAY                      
         BNE   END05                                                            
         CLI   TWAPAGE,250         ASSUME 250 PAGE MAX                          
         BH    END10               IF BIGGER, SKIP TOTALS                       
         B     END08                                                            
                                                                                
END05    DS    0H                                                               
         CLI   TWAPAGE,125         ASSUME 125 PAGE MAX                          
         BH    END10               IF BIGGER, SKIP TOTALS                       
*                                                                               
END08    DS    0H                                                               
         ZIC   RF,TWAPAGE          PUT PAGE TOTAL IN PROPER SLOT                
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         LA    RF,TWAINVT(RF)                                                   
         L     RE,INVTOT                                                        
         ST    RE,0(RF)                                                         
*                                                                               
END10    LA    R2,INVACTH          CURSOR                                       
         B     EXIT                                                             
*                                                                               
* GET NEXT CONTRACT DISK ADDR                                                   
C200     LA    R9,8(R9)                                                         
         OC    0(4,R9),0(R9)       LAST CONTRACT DISPLAYED?                     
         BZ    END                                                              
         B     C50                                                              
*                                                                               
* EDIT NEW INVOICE DATA                                                         
* GET CONTRACT                                                                  
C300     DS    0H                                                               
         LA    R3,INVERR                                                        
         OC    0(4,R9),0(R9)       ERROR IF NO DISK ADDRESS                     
         BZ    ERROR                                                            
         MVC   KEY+28(4),0(R9)     K DISK ADDR                                  
         BAS   RE,GETREC                                                        
*                                                                               
* GET ORD AND INVOICE AMOUNTS                                                   
*                                                                               
         GOTO1 BUCKETS,DMCB,RCONREC,DOLLARS                                     
* GET NEW INVOICE AMOUNT                                                        
         SR    R6,R6                                                            
         XC    DMCB+4(3),DMCB+4                                                 
         MVC   DMCB+7(1),5(R2)     INPUT LEN                                    
*                                                                               
         MVI   BYTE3,0                                                          
         CLC   OPTNET$$,=C'00'                                                  
         BNE   MVNNOG                                                           
         MVI   NOG,C'G'            JR INIT NOG  NET  OR  GROSS                  
         B     ANYDETAL                                                         
*                                                                               
MVNNOG   MVI   NOG,C'N'            NET REQUESTED ON HEADER                      
ANYDETAL CLI   5(R2),0                                                          
         BE    C310                                                             
**                    DETAIL INVOICE AMOUNT CHG FOR NET OR GROSS                
**                                                                              
         ZIC   R1,5(R2)            LENGTH OF INVOICE FIELD ENTERED              
         LA    RF,8(R2)            ADDRESS OF INVOICE AMOUNT                    
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
                                                                                
         CLI   0(RF),C'G'          8500G                                        
         BE    DECRE                                                            
                                                                                
         CLI   0(RF),C'N'          8500N                                        
         BNE   VCAS                                                             
                                                                                
MVCN     L     (RF),DMCB+4         INPUT LENGTH                                 
         BCTR  (RF),0                                                           
         ST    (RF),DMCB+4                                                      
         B     VCAS                                                             
                                                                                
DECRE    L     (RF),DMCB+4         INPUT LENGTH                                 
         BCTR  (RF),0                                                           
         ST    (RF),DMCB+4                                                      
* EDIT INVOICE DATA                                                             
                                                                                
                                                                                
VCAS     GOTO1 VCASHVAL,DMCB,8(R2)                                              
         LA    R3,INVERR                                                        
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
*                                                                               
         L     R6,DMCB+4           NEW AMT                                      
*                                                                               
*                                  THIS NEW AMOUNT WILL ALSO BE                 
*                                  USED FOR UPDATING THE NET TO                 
*                                  A GROSS FIELD.                               
*                                                                               
         LTR   R6,R6                                                            
         BNZ   *+8                                                              
         MVI   BYTE3,1             INDICATE 0 HAS BEEN ENTERED                  
**                                 MUST LEAVE AT LEAST 1 BUCKET IF SO           
****                                                                            
****************************************************************                
*                    PLACE THE UPDATED OUTPUT NET DOLLARS                       
*                 INTO THE CONTRACT FILE.                                       
*                    A CHANGE TO PROVIDE AN INDICATOR TO IDENTIFY               
*                THIS 'OUTPUT NET DOLLARS FIELD'IS TRUELY                       
*                 NOT A GROSS DOLLAR AMOUNT (NETC310S)                          
*                                                                               
*                                                                               
************************************************************JR***               
C310     EQU   *                                                                
         BAS   RE,NETRTNS         THIS IS NET CALCULATION                       
*                                 RETURN FROM CALCULATION                       
         L     R7,DOLLARS+4        OLD AMT                                      
         SR    R6,R7          OLD FROM NEW                                      
         BNZ   C315                                                             
         LR    RE,R7                                                            
         CLI   BYTE3,1             HAS 0 BEEN ENTERED?                          
         BNE   C312                                                             
         CLI   BYTE4,0             DO INVOICE ELEMENTS EXIST FOR THIS           
*                                  MONTH?                                       
*                                  BYTE 4 IS SET BY 'BUCKETS' ROUTINE           
*                                  IF INVOICE BUCKETS FOUND FOR MONTH           
         BE    C315                                                             
         B     C425                                                             
         EJECT                                                                  
********************************************************************            
*                                   ROUTINE TO CALCULATE THE NET                
*                                   AMOUNT BY DEVIDING THE GROSS                
*                                   AMOUNT BY THE % ENTERED ON THE              
*                                   SCREEN                                      
*                                                                               
************************************************************JR                  
NETRTNS  NTR1                       NET CALCULATION ROUTINE                     
*                                                                               
*   FIRST CHECK DETAIL LINE OPTION:  IF NET, APPLY APPROPRIATE                  
*      NET VALUE.  IF GROSS, EXIT THE ROUTINE.  IF NEITHER,                     
*      CHECK SCREEN LEVEL OPTION FIELD FOR NET/GROSS.                           
*                                                                               
         ZIC   RF,5(R2)            GET LENGTH FROM SCREEN FIELD                 
         LA    RE,8(R2)            SET A(1ST CHARACTER INPUT)                   
         AR    RE,RF               ADD FIELD LENGTH TO ADDRESS                  
         BCTR  RE,0                BACK UP 1 POSITION                           
         CLI   0(RE),C'N'          NET OUT THIS FIELD?                          
         BE    NETR0020            YES - CHECK 85% OR OPTIONAL VALUE            
         CLI   0(RE),C'G'          GROSS OUT THIS FIELD?                        
         BNE   NETR0040            NO  - NOW CHECK TOP OPTION                   
         B     NETRTNX             YES - EXIT OUT                               
NETR0020 EQU   *                                                                
         CLC   OPTNET$$,=C'00'                                                  
         BNE   PKOPT                USE  OPTNET$$  FOR CALC                     
         ZAP   DIVS,=P'85'          DEFAULT 85 WHEN DET IS 'N'                  
         B     CVDIVD                                                           
                                                                                
NETR0040 EQU   *                                                                
*                                                                               
*   NO DETAIL LINE OPTION SET.  CHECK SCREEN LEVEL OPTION FOR                   
*        NET/GROSS                                                              
*                                                                               
         CLI   NOG,C'G'             GROSS WAS REQUESTED ??                      
         BE    NETRTNX              YES - EXIT                                  
****                               NET REQUESTED                                
         CLC   OPTNET$$,=C'00'                                                  
         BNE   PKOPT                USE  OPTNET$$  FOR CALC                     
         ZAP   DIVS,=P'85'          DEFAULT 85 WHEN DET IS 'N'                  
         B     CVDIVD                                                           
                                                                                
*               CHECK DETAIL COMPLETE                                           
PKOPT    PACK  DIVS,OPTNET$$        E.G.  85%                                   
CVDIVD   CVD   R6,DIVD              CONTRACT AMOUNT                             
         MP    DIVD,=P'100'         MULTIPLY BY 100 FOR ALIGNMENT               
         DP    DIVD,DIVS            REMAINDER 5 POS                             
         ZAP   DUB(8),DIVD(5)       FIRST 5 OF DIVD IS WHOLE NUMBER             
         CVB   R7,DUB                                                           
         LTR   R7,R7                                                            
         BM    CVDI0010             NEGATIVE                                    
         AH    R7,=H'50'            1000050                                     
         SR    R6,R6                ZERO OUT HI ORDER DIVIDEND                  
         B     CVDI0030                                                         
CVDI0010 EQU   *                                                                
         SH    R7,=H'50'            NEGATIVE = SUBTRACT VALUE                   
         LA    R6,1                 SET R6 TO ALL NEGATIVE                      
         LNR   R6,R6                                                            
CVDI0030 EQU   *                                                                
         D     R6,=F'100'                                                       
         LR    R6,R7                MOVE  QUOTIENT INTO R6                      
         MH    R6,=H'100'                                                       
*                                                                               
NETRTNX  EQU   *                    EXIT TO NET CALC ROUTINE                    
         XIT1  REGS=(R6)                                                        
*                                                                               
**************************************************************JR                
                                                                                
                                                                                
****************************************************************                
*                                                                               
*                                  MOVE THE PROPER INVOICE INPUT                
*                                  DOLLARS AND DO CALCULATION                   
*                                  BASED ON N OR G OR SPACE AT                  
*                                  END OF ENTRY ( 1000N)                        
***                                                                             
****     ZIC   R1,5(R2)            INPUT DATA LENGTH                            
****     LA    RF,8(R2)            ADDR OF INPUT FIELD                          
****     AR    RF,R1               ADD LENGTH TO INPUT FIELD                    
****     BCTR  RF,0                -1                                           
****     CLI   0(RF),C'N'                                                       
****     BNE                                                                    
*                                                                               
*                                                                               
* 0 HAS NOT BEEN ENTERED  -CHECK FOR BLANK AND ONLY 1 ELEMENT                   
C312     OC    DMCB+4(4),DMCB+4    INPUT = BLANK?                               
         BNZ   C425                                                             
         CLI   BYTE4,1             ONLY 1 INVOICE BUCKET EXISTS?                
         BE    C315                IF SO ELIMINATE THAT BUCKET                  
         B     C425                                                             
*                                                                               
* BUILD INVOICE BUCKET                                                          
C315     EQU   *                                                                
         MVC   WORK+2(2),TWAMON    INSERT MONTH OF SERVICE                      
         MVC   WORK(2),=X'040A'    CODE + LEN                                   
         CLC   =C'CHN',INVACT      CHANGE ALTERNATE INVOICE?                    
         BNE   C315A               NO                                           
         MVI   WORK,X'54'          YES - SET ALTERNATE BUCKET TYPE              
         BAS   RE,CHKACTDT         SET ACTIVITY DATE PROPERLY                   
*                                  (PACING APPROXIMATED WHERE NEEDED)           
         BZ    C319                ACTIVITY DATE SET BY ROUTINE                 
         B     C317                                                             
C315A    EQU   *                                                                
         CLC   =C'CTR',INVACT      CHANGE TRADE INVOICE?                        
         BNE   C315B               NO                                           
         MVI   WORK,X'64'          YES - SET TRADE BUCKET TYPE                  
         B     C317                                                             
C315B    EQU   *                                                                
         CLC   =C'CHN',INVACT      CHANGE ALTERNATE INVOICE?                    
         BNE   C315C               NO                                           
         MVI   WORK,X'64'          YES - SET TRADE/ALT BUCKET TYPE              
         BAS   RE,CHKACTDT         SET ACTIVITY DATE PROPERLY                   
*                                  (PACING APPROXIMATED WHERE NEEDED)           
         BZ    C319                ACTIVITY DATE SET BY ROUTINE                 
         B     C317                                                             
C315C    EQU   *                                                                
C317     EQU   *                                                                
* GET MONDAY DATA                                                               
         GOTO1 VDATCON,DMCB,(3,TODAY),DUB                                       
* GET DAY OF WEEK                                                               
         GOTO1 VGETDAY,(R1),DUB,FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R4,R4                                                            
         IC    R4,DMCB             DAY                                          
         BCTR  R4,R0                                                            
         LNR   R4,R4                                                            
         GOTO1 VADDAY,(R1),DUB,DMCB+12,(R4)                                     
         GOTO1 VDATCON,(R1),DMCB+12,(2,WORK+4)                                  
*                                                                               
C319     EQU   *                                                                
         ST    R6,FULL                                                          
         MVC   WORK+6(4),FULL                                                   
*                                                                               
* ADD BUCKET TO CONREC                                                          
         MVI   BYTE2,0                                                          
         LA    R3,RCONELEM         FIRST ELEM                                   
         SR    R4,R4                                                            
C320     IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         CLI   0(R3),0             LAST?                                        
         BE    C325                                                             
*                                                                               
* FIND WHETHER TO INSERT OR CHANGE                                              
         CLC   WORK(4),0(R3)       SAME MONTH?                                  
         BNE   C322                                                             
         IC    R4,BYTE2            BUCKET CTR                                   
         LA    R4,1(R4)                                                         
         STC   R4,BYTE2                                                         
C322     CLC   WORK(6),0(R3)                                                    
         BH    C320                                                             
         BE    C350                                                             
* ADD ELEM                                                                      
C325     OC    WORK+6(4),WORK+6                                                 
         BNZ   C327                                                             
         CLI   BYTE3,1             0 INPUT?                                     
         BNE   C420                                                             
         CLI   BYTE2,0             BUCKETS EXIST FOR MONTH?                     
         BNE   C420                                                             
C327     GOTO1 VRECUP,DMCB,(2,RCONREC),WORK,(C'R',(R3))                         
*                                                                               
         CLI   DMCB+8,C'R'         GOOD RETURN FROM RECUP?                      
         BE    C400                YES.                                         
*                                                                               
         MVC   8(10,R2),=CL10'*REC FULL*'  NOTE FLD IN ERROR                    
         B     C450                                                             
         SPACE                                                                  
*                                                                               
* ADD IN INVOICE DIFFERENCE                                                     
C350     MVC   FULL,6(R3)          OLD AMT                                      
         L     R7,FULL                                                          
         MVC   FULL,WORK+6                                                      
*                                                                               
         A     R7,FULL                                                          
         ST    R7,FULL                                                          
         MVC   6(4,R3),FULL                                                     
*                                                                               
         LTR   R7,R7                                                            
         BNZ   C400                                                             
         CLI   BYTE3,1             HAS 0 BEEN ENTERED?                          
         BNE   *+12                                                             
*                                  0 HAS BEEN ENTERED                           
         CLI   BYTE2,2             CHECK BUCKET CTR FOR THIS MONTH              
         BL    C400                                                             
* DELETE ZERO BUCKET                                                            
         GOTO1 VRECUP,DMCB,(2,RCONREC),(R3),(R3)                                
* WRITE REC                                                                     
C400     DS    0H                                                               
         MVC   RCONINVD,TODAY                                                   
         BAS   RE,PUTREC                                                        
*                                                                               
* REDISPLAY INVOICE AMOUNT                                                      
C420     GOTO1 BUCKETS,DMCB,RCONREC,DOLLARS                                     
         L     RE,DOLLARS+4        INVOICE AMOUNT                               
         ST    RE,4(R9)            SAVE INVOICE AMOUNT                          
         LTR   RE,RE                                                            
         BNZ   C425                                                             
         CLI   BYTE4,0                                                          
         BE    C425                                                             
         MVC   4(4,R9),=C'ZERO'                                                 
C425     CLC   4(4,R9),=C'ZERO'                                                 
         BE    C435                                                             
         L     RF,INVTOT           ADD TO ACCUMULATOR                           
         A     RF,4(R9)                                                         
         ST    RF,INVTOT                                                        
         SPACE 1                                                                
C435     XC    8(10,R2),8(R2)                                                   
         CLI   BYTE4,0             INVOICE BUCKETS FOR THIS MONTH?              
         BE    C450                                                             
         LPR   R0,RE                                                            
         C     R0,=F'100000000'    VALUE 1,000,000.00 OR >?                     
         BL    C440                YES - USE REGULAR EDIT                       
         EDIT  (RE),(11,8(R2)),2,MINUS=YES                                      
         B     C450                                                             
C440     EQU   *                                                                
         EDIT  (RE),(11,8(R2)),2,COMMAS=YES,MINUS=YES                           
C450     FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALID                                        
*                                                                               
         ZIC   R0,0(R2)            GET TO DIFFERENCE FIELD                      
         AR    R2,R0                                                            
         L     RE,DOLLARS                                                       
         S     RE,DOLLARS+4                                                     
         LPR   R0,RE                                                            
         C     R0,=F'10000000'     VALUE 100,000.00 OR >?                       
         BL    C470                YES - USE REGULAR EDIT                       
         EDIT  (RE),(10,8(R2)),2,MINUS=YES                                      
         B     C480                                                             
C470     EQU   *                                                                
         EDIT  (RE),(10,8(R2)),2,COMMAS=YES,MINUS=YES                           
C480     FOUT  (R2)                                                             
         B     C100                                                             
                                                                                
C500     DS    0H                  PUT INVOICE# IN CONTRACT RECORD              
         MVC   KEY+28(4),0(R9)     K DISK ADDR                                  
         BAS   RE,GETREC                                                        
         GOTO1 PUTINV#,DMCB,RCONREC,(R2)                                        
         OI    4(R2),X'20'         VALID                                        
         ZIC   R0,0(R2)            GET TO DIFFERENCE FIELD                      
         AR    R2,R0                                                            
         B     C110                                                             
         EJECT                                                                  
*                                                                               
*   CHKACTDT:  IF ALT CALENDAR BUCKET REQUIRES APPROXIMATELY PACED              
*     ACTIVITY DATE, INSERT IT WHEN:                                            
*        DATE OF SERVICE > 2 MONTHS PAST                                        
CHKACTDT NTR1                                                                   
         MVC   WORK+20(2),TWAMON   YR-MON                                       
         MVI   WORK+22,1           SET DAY TO 1                                 
         GOTO1 VDATCON,DMCB,(3,WORK+20),(0,WORK+23)                             
         GOTO1 VDATCON,DMCB,(3,TODAY),(0,WORK+29)                               
*                                                                               
         L     R7,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R7                                                      
         GOTO1 CPERVERT,DMCB,WORK+23,WORK+29                                    
         DROP  R7                                                               
*                                                                               
         ZICM  RF,DMCB+8,2         GET NUMBER OF DAYS                           
         CH    RF,=H'90'           90 DAYS PRIOR?                               
         BL    CHKA0200            NO  - DATE NOT SET HERE!                     
****<<<<                                                                        
*                                                                               
         ZIC   RF,WORK+21          BUMP MONTH BY 2                              
         LA    RF,2(RF)                                                         
         STC   RF,WORK+21          REPLACE MONTH                                
         CLI   WORK+21,12          MONTH > 12?                                  
         BNH   CHKA0040            NO  - OKAY TO USE DATE                       
         SH    RF,=H'12'           YES - MAKE JANUARY-RELATIVE                  
         STC   RF,WORK+21          REPLACE MONTH                                
         ZIC   RF,WORK+20          ADD 1 YEAR                                   
         LA    RF,1(RF)                                                         
         STC   RF,WORK+20          REPLACE YEAR                                 
CHKA0040 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,WORK+20),(0,WORK+23)                             
*                                  CONVERT DATE TO EBCDIC                       
         PRINT GEN                                                              
         GOTO1 VGTBROAD,DMCB,(1,WORK+23),WORK+29,VGETDAY,VADDAY                 
*                                  GET BROADCAST MONTH                          
         PRINT NOGEN                                                            
         CLC   WORK+31(2),WORK+37  START/END IN SAME MONTH?                     
         BE    CHKA0060            YES - USE START DATE AS ACTIVITY             
         LA    R4,7                NO  - BUMP BY 1 WEEK                         
         GOTO1 VADDAY,DMCB,WORK+29,WORK+43,(R4)                                 
         MVC   WORK+29(6),WORK+43                                               
*                                  RESTORE START DATE                           
CHKA0060 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(0,WORK+29),(2,WORK+4)                              
         SR    R0,R0               SET CC = ZERO                                
         B     CHKA0400                                                         
CHKA0200 EQU   *                                                                
         LTR   RB,RB                                                            
         B     CHKA0400            EXIT                                         
CHKA0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO FIND DOLLARS FOR TWAMON                                            
*              P1=A(CONREC)                                                     
*              P2=A(8-BYTE OUTPUT - FIRST WORD = ORDERED                        
*                                   SECOND     = INVOICE                        
BUCKETS  NTR1                                                                   
         MVI   BYTE4,0                                                          
         LM    R2,R3,0(R1)                                                      
         SR    R4,R4                                                            
         LA    R2,34(R2)           FIRST K ELEM                                 
         XC    0(8,R3),0(R3)       OUTPUT                                       
BUCK0020 LR    R5,R3                                                            
         CLC   =C'CHA',INVACT      CHANGE ACTION?                               
         BNE   BUCK0040            NO  -                                        
         CLI   0(R2),3             ORD?                                         
         BE    BUCK0100                                                         
         CLI   0(R2),4             INV?                                         
         BNE   BUCK0080            NO                                           
         B     BUCK0060            YES                                          
BUCK0040 EQU   *                                                                
         CLC   =C'CHN',INVACT      CHANGE ALTERNATE ACTION?                     
         BNE   BUCK0044            NO  -                                        
         CLI   0(R2),X'53'         ALTERNATE ORD?                               
         BE    BUCK0100                                                         
         CLI   0(R2),X'54'         ALTERNATE INV?                               
         BNE   BUCK0080            NO                                           
         B     BUCK0060            YES                                          
BUCK0044 EQU   *                                                                
         CLC   =C'CTR',INVACT      CHANGE TRADE ACTION?                         
         BNE   BUCK0048            NO  -                                        
         CLI   0(R2),X'63'         TRADE ORD?                                   
         BE    BUCK0100                                                         
         CLI   0(R2),X'64'         TRADE INV?                                   
         BNE   BUCK0080            NO                                           
         B     BUCK0060            YES                                          
BUCK0048 EQU   *                                                                
         CLC   =C'CTA',INVACT      CHANGE TRADE/ALTERNATE ACTION?               
         BNE   BUCK0052            NO  -                                        
         CLI   0(R2),X'83'         TRADE/ALTERNATE ORD?                         
         BE    BUCK0100                                                         
         CLI   0(R2),X'84'         TRADE/ALTERNATE INV?                         
         BNE   BUCK0080            NO                                           
         B     BUCK0060            YES                                          
BUCK0052 EQU   *                                                                
         DC    H'0'                HOW'D WE GET HERE?                           
BUCK0060 EQU   *                                                                
         LA    R5,4(R5)                                                         
         B     BUCK0100                                                         
BUCK0080 IC    R4,1(R2)                                                         
         AR    R2,R4               NEXT ELEM                                    
         CLI   0(R2),0             LAST?                                        
         BNE   BUCK0020                                                         
* LAST                                                                          
         XIT1                                                                   
* CHECK BUCKET MONTH                                                            
BUCK0100 CLC   2(2,R2),TWAMON                                                   
         BNE   BUCK0080                                                         
* SAME MONTH                                                                    
         MVC   FULL,6(R2)          DOLLARS                                      
         L     R6,FULL                                                          
         A     R6,0(R5)                                                         
         ST    R6,0(R5)                                                         
         CLC   =C'CHA',INVACT      CHANGE REGULAR INVOICE?                      
         BNE   BUCK0120            NO                                           
         CLI   0(R2),4                                                          
         BE    BUCK0140                                                         
         B     BUCK0080                                                         
BUCK0120 EQU   *                                                                
         CLC   =C'CHN',INVACT      CHANGE ALTERNATE INVOICE?                    
         BNE   BUCK0124            NO                                           
         CLI   0(R2),X'54'         CHANGE ALTERNATE INVOICE?                    
         BE    BUCK0140                                                         
         B     BUCK0080                                                         
BUCK0124 EQU   *                                                                
         CLC   =C'CTR',INVACT      CHANGE TRADE INVOICE?                        
         BNE   BUCK0128            NO                                           
         CLI   0(R2),X'64'         CHANGE TRADE INVOICE?                        
         BE    BUCK0140                                                         
         B     BUCK0080                                                         
BUCK0128 EQU   *                                                                
         CLC   =C'CTA',INVACT      CHANGE TRADE/ALTERNATE INVOICE?              
         BNE   BUCK0132            NO                                           
         CLI   0(R2),X'84'         CHANGE TRADE/ALTERNATE INVOICE?              
         BE    BUCK0140                                                         
         B     BUCK0080                                                         
BUCK0132 EQU   *                                                                
         DC    H'0'                HOW'D WE GET HERE?                           
BUCK0140 EQU   *                                                                
         IC    R4,BYTE4                                                         
         LA    R4,1(R4)                                                         
         STC   R4,BYTE4                                                         
         B     BUCK0080                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE CONTRACT RECORD WITH INVOICE #                              
*              P1=A(CONREC)                                                     
*              P2=A(INPUT FIELD)                                                
***********************************************************************         
PUTINV#  NTR1                                                                   
         L     R6,0(R1)                                                         
         L     R2,4(R1)                                                         
                                                                                
         XC    ELEM,ELEM                                                        
         XC    MYWORK,MYWORK                                                    
         LA    R6,34(R6)           POINT TO FIRST ELEMENT OF CONTRACT           
         SR    R4,R4                                                            
PUTI20   IC    R4,1(R6)                                                         
         AR    R6,R4                                                            
         CLI   0(R6),0                                                          
         BE    PUTI30                                                           
                                                                                
         CLI   0(R6),X'18'                                                      
         BE    PUTI40                                                           
         BNH   PUTI20                                                           
                                                                                
PUTI30   DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    PUTINVX                                                          
         LA    R3,ELEM             IF NO INVOICE# ELEMENT PRESENT,              
         USING RCONDVEL,R3          BUILD A NEW ONE                             
         MVI   RCONDVO,X'18'                                                    
                                                                                
         ZIC   RF,5(R2)                                                         
         LA    RF,11(RF)           OVERHEAD + ONE ENTRY                         
         STC   RF,RCONDVLN         ELEMENT LENGTH                               
                                                                                
         MVI   RCONDVI#,1                                                       
                                                                                
         ZIC   RF,5(R2)                                                         
         AH    RF,=H'3'                                                         
         STC   RF,RCONDVIL         INVOICE ENTRY LENGTH                         
                                                                                
         MVC   RCONDVID,TWAMON     INVOICE DATE:YM                              
                                                                                
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RCONDVIN(0),8(R2)   INVOICE NUMBER                               
         B     PUTI100                                                          
         DROP  R3                                                               
                                                                                
PUTI40   DS    0H                  UPDATE EXISTING X'18' ELEMENT                
         USING RCONDVEL,R6                                                      
         ZIC   R1,RCONDVLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RCONDVEL                                                 
         DROP  R6                                                               
                                                                                
         LA    R3,ELEM                                                          
         USING RCONDVEL,R3                                                      
         ZIC   R4,RCONDVLN                                                      
         AR    R4,R3               CALCULATE END OF ELEMENT                     
         LA    R5,RCONDVIL                                                      
         CLI   RCONDVI#,0          ANY INVOICE# IN THIS ELEMENT?                
         BNE   PUTI50                                                           
         CLI   5(R2),0             NO, CHECK IF A INVOICE# WAS ENTERED          
         BE    PUTINVX                 NO, THEN EXIT                            
         MVI   RCONDVI#,1              YES, ADD THIS INVOICE#                   
                                                                                
         ZIC   RF,5(R2)                                                         
         AH    RF,=H'3'                                                         
                                                                                
         ZIC   RE,RCONDVLN                                                      
         AR    RE,RF                                                            
         STC   RE,RCONDVLN         UPDATE ELEMENT LENGTH                        
                                                                                
         STC   RF,0(R5)            INVOICE ENTRY LENGTH                         
                                                                                
         MVC   1(2,R5),TWAMON      YM                                           
                                                                                
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R5),8(R2)                                                    
         B     PUTI90                                                           
                                                                                
PUTI50   DS    0H                  YES, WE HAVE A LIST OF INVOICE#'S            
         CLC   TWAMON,1(R5)        FOUND A MATCH?                               
         BE    PUTI53                                                           
         CLC   TWAMON,1(R5)        INVOICE# YEAR/MON LOWER?                     
         BL    PUTI60              YES, INSERT THIS ENTRY                       
         ZIC   RF,0(R5)                                                         
         AR    R5,RF               NO, KEEP LOOKING                             
         CR    R5,R4                                                            
         BL    PUTI50                                                           
         B     PUTI80              END OF ELEMENT, SO APPEND THIS ENTRY         
*                                                                               
* A MATCH WAS FOUND. DELETE ENTRY FIRST THEN INSERT NEW ENTRY                   
*                                                                               
PUTI53   DS    0H                                                               
         LR    R1,R5                                                            
         ZIC   RE,0(R1)            DELETE ENTRY                                 
         AR    R1,RE                                                            
         CLI   RCONDVI#,1          IF THIS IS THE ONLY ENTRY, SKIP              
         BE    PUTI55              MOVING REST OF ENTRIES UP                    
                                                                                
         ZIC   RF,RCONDVLN         MOVE REST OF INVOICE#'S UP                   
         AR    RF,R6                                                            
         SR    RF,R5                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R1)                                                    
                                                                                
PUTI55   DS    0H                                                               
         ZIC   RF,RCONDVI#                                                      
         BCTR  RF,0                                                             
         STC   RF,RCONDVI#         DECREMENT NUMBER OF INVOICE#'S               
                                                                                
         ZIC   RF,RCONDVLN         DECREMENT OVERALL ELEMENT LENGTH             
         SR    RF,RE                                                            
         STC   RF,RCONDVLN                                                      
                                                                                
         CLI   5(R2),0             USER WANTED TO DELETE INVOICE# ENTRY         
         BE    PUTI90                                                           
*                                                                               
* INSERT ENTRY                                                                  
*                                                                               
PUTI60   DS    0H                  INSERT ENTRY                                 
         XC    MYWORK,MYWORK                                                    
         LR    R1,R4                                                            
         SR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK(0),0(R5)     SAVE OFF REST OF ENTRIES                     
                                                                                
         ZIC   RF,RCONDVI#                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RCONDVI#         BUMP NUMBER OF INVOICE#'S                    
                                                                                
         ZIC   RF,5(R2)                                                         
         LA    RF,3(RF)                                                         
                                                                                
         ZIC   RE,RCONDVLN         UPDATE ELEMENT LENGTH                        
         AR    RE,RF                                                            
         STC   RE,RCONDVLN                                                      
                                                                                
         STC   RF,0(R5)            REWRITE NEW ENTRY LENGTH                     
                                                                                
         MVC   1(2,R5),TWAMON      YM                                           
                                                                                
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R5),8(R2)       #                                            
                                                                                
         ZIC   R0,0(R5)                                                         
         AR    R5,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),MYWORK      PUT BACK REST OF ENTRIES                     
         B     PUTI90                                                           
*                                                                               
* APPEND AN ENTRY                                                               
*                                                                               
PUTI80   DS    0H                  APPEND AN INVOICE#                           
         ZIC   RF,5(R2)                                                         
         LA    RF,3(RF)                                                         
                                                                                
         ZIC   RE,RCONDVLN         UPDATE ELEMENT LENGTH                        
         AR    RE,RF                                                            
         STC   RE,RCONDVLN                                                      
                                                                                
         ZIC   RE,RCONDVI#                                                      
         LA    RE,1(RE)                                                         
         STC   RE,RCONDVI#         BUMP NUMBER OF INVOICE#'S                    
                                                                                
         STC   RF,0(R5)            REWRITE NEW ENTRY LENGTH                     
                                                                                
         MVC   1(2,R5),TWAMON      YM                                           
                                                                                
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R5),8(R2)       #                                            
         DROP  R3                                                               
                                                                                
* DELETE ELEMENT                                                                
PUTI90   DS    0H                                                               
         GOTO1 VRECUP,DMCB,(2,RCONREC),(R6),0,0                                 
                                                                                
PUTI100  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(2,RCONREC),ELEM,(C'R',(R6))                         
                                                                                
PUTI110  DS    0H                                                               
         BAS   RE,PUTREC                                                        
                                                                                
PUTINVX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
* LOCAL VARIABLES                                                               
*                                                                               
ELCODE   DS    X                                                                
INVLEN   DS    X                                                                
INVENT#  DS    X                                                                
ELEM     DS    CL256                                                            
MYWORK   DS    CL256                                                            
         DS    0D                                                               
DIVD     DS    PL8                 DIVIDEND                                     
DIVS     DS    PL3                 DIVISOR                                      
NOG      DS    C                   NET  OR  GROSS INDICATOR                     
         EJECT                                                                  
       ++INCLUDE REINVGEN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165REINV10   05/01/02'                                      
         END                                                                    
