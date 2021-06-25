*          DATA SET SPREPDN02  AT LEVEL 051 AS OF 05/20/15                      
*PHASE SPDN02B                                                                  
         TITLE 'SPREPDN02   NETWK RPT PROCESS'                                  
*        CHANGE LOG                                                             
*                                                                               
* PWES 040 29MAY03 FIX PRINT OF GST/PST (8CHRS) FOR LARGER $ LOCAL BUYS         
*  MHER  9/24/01   FIX FOR CONV CANAD CABLE                                     
*                                                                               
*  BPLA  9/99      OPTIONS FIXED                                                
*                                                                               
*  SCHO 3/9/99     DISPLAY SPILL MARKETS                                        
*                  QOPT2: OPTION TO DISPLAY SPILL OR NO                         
*                                                                               
*  MHER  11/97     EDIT DAYS CORRECTLY FOR OOWR                                 
*                                                                               
*  BPLA  4/96      FOR NETWORK GST AND PST DISPLAY                              
*                  ALLOW 8 CHARACTERS (PROGRAM WAS                              
*                  TRUNCATING HIGH DIGIT WHEN GST WAS OVER                      
*                  9999.99                                                      
*                                                                               
*  SPRI  2/17/95   SUPPORT NEW BDCIND EQUATES                                   
*                                                                               
*  BPLA 12/20/93   ADD CODE TO SHOW PST                                         
*                                                                               
*  BPLA 9/9/93     DISABLE QOPT5 - NOW ALWAYS OMIT BUYS WITH NO                 
*                  SPOTS IN THE REQUEST PERIOD                                  
*                                                                               
*  BPLA 9/8/93     ACTSW (ACTIVITY SWITCH) CHANGES                              
*                                                                               
*  BPLA  9/7/93    ADD QOPT5 - "Y" = SKIP BUYS WITH NO SPOTS                    
*                                    IN REQUESTED PERIOD                        
*  BPLA  11/91/92  CHANGES FOR NEW PROFILE OPTIONS                              
*                  EXCLUDE TAX AND GST FROM GROSS AND NET                       
*                  GST COLUMN                                                   
*                                                                               
*  BPLA  11/10/92  CHANGES FOR NEW STATION FILE                                 
*                                                                               
*  BPLA  11/5/92   FIX PROBLEMS WITH NEGATIVE BUYS                              
*                                                                               
*  BPLA  10/4/90   FOR UNPAID REPORT  - SKIP HIATUS NETWORK AND                 
*                  STATION SPOTS                                                
*                                                                               
*        QOPTIONS                                                               
*                                                                               
*        QOPT1,QOPT2,QOPT3   BUY LINE NUMBER (OPTIONAL)                         
*        QOPT4  U=INPAID                                                        
*        QOPT5  NOT USED                                                        
*        QOPT6  Y= DISPLAY SPILL MARKETS    (QGRP - COL67)                      
*        QOPT7  B= FORCE TO BROADCAST MONTHS (QGRP+1 - COL68)                   
*                                                                               
QOPT6    EQU    QGRP                                                            
QOPT7    EQU    QGRP+1                                                          
*                                                                               
*        PROFILE OPTIONS                                                        
*        +0     Y=INCLUDE TAX IN GROSS (DEFAULT)                                
*               N=DON'T                                                         
*        +1     Y=INCLUDE GST IN GROSS (DEFAULT)                                
*               N=DON'T                                                         
*        +2     Y=INCLUDE TAX IN NET (DEFAULT)                                  
*               N=DON'T                                                         
*        +3     Y=INCLUDE GST IN NET (DEFAULT)                                  
*               N=DON'T                                                         
*        +4     Y=GST COLUMN                                                    
*               N=NO (DEFAULT)                                                  
SPDN02   CSECT                                                                  
         NMOD1 0,SPDN02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
*                                                                               
*                                                                               
         LA    R9,SPACEND                                                       
         USING SPDNWRKD,R9                                                      
         CLI   MODE,PROCBUY                                                     
         BE    NETBUY                                                           
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         L     R0,=A(SPDEMO)                                                    
         ST    R0,ASPDEMO                                                       
         L     R0,=A(PROCESS)                                                   
         ST    R0,APROCESS                                                      
         L     R0,=A(SBUYEND)                                                   
         ST    R0,ASBUYEND                                                      
EXIT     XIT1                                                                   
         EJECT                                                                  
NETBUY   DS    0H                                                               
         XC    NUNPDGR(20),NUNPDGR     CLEAR NET ACCUMS                         
         XC    NDEMTOT1(16),NDEMTOT1                                            
         L     R3,ADBUY                                                         
         USING BUYRECD,R3                                                       
*                                                                               
NETB1    L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
*                                                                               
NETB2    CLI   0(R2),X'0B'                                                      
         BE    NETB21                                                           
         CLI   0(R2),X'0C'         OTO'S                                        
         BE    NETB21                                                           
         CLI   0(R2),0                                                          
         BE    NETBEND                                                          
NETBNXT  LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     NETB2                                                            
***************************                                                     
         USING REGELEM,R2                                                       
NETB21   CLI   0(R2),X'0C'          SKIP MINUSED OTO'S                          
         BNE   NETB21A                                                          
         TM    RSTATUS,X'80'                                                    
         BNZ   NETBNXT                                                          
*                                                                               
NETB21A  CLC   RDATE,BQSTARTP      IGNORE ELEMS OUT OF REQ PERIOD               
         BL    NETBNXT                                                          
         CLC   RDATE,BQENDP                                                     
         BH    NETBNXT                                                          
         CLI   QOPT4,C'U'                                                       
         BNE   NETB3                                                            
**NEW 10/4/90                                                                   
         TM    RSTATUS,X'04'       SEE IF HIATUS                                
         BNZ   NETBNXT             CAN SKIP FOR UNPAID REPORT                   
*                                                                               
**NEW 6/8/88                                                                    
         TM    RSTATUS,X'40'       SEE IF MINUSED                               
         BZ    NETB2M              NO - JUST CHK IF PAID                        
         OC    RPAY,RPAY      IF MINUSED AND PAID SHOW AS UNPAID                
         BNZ   NETB2A         UNLESS NEXT ELEM (X'0C') IS PAID                  
         B     NETB3          MINUSED AND UNPAID - STILL SHOW                   
*                             SINCE STATION MIGHT BE DIFFERENT                  
*                                                                               
NETB2A   ST    R2,SAVR2           MUST SEE IF NEXT ELEM IS PAID                 
****** NEW CODE FOR OTO'S                                                       
         CLI   0(R2),X'0B'        ONLY IF PROCESSING X'0B' ELEM                 
         BNE   NETB2B                                                           
***************                                                                 
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'0C'        MAKE GOOD                                     
         BNE   NETB2B                                                           
         TM    RSTATUS,X'80'                                                    
         BZ    NETB2B                                                           
         OC    RPAY,RPAY                                                        
         BNZ   NETB2C                                                           
NETB2B   L     R2,SAVR2         NEXT ELEM IS UNPAID OR DOES NOT EXIST           
         B     NETB3            SO PROCESS AS UNPAID                            
NETB2C   L     R2,SAVR2           NEXT ELEM IS PAID SO BYPASS                   
         B     NETBNXT                                                          
*                                                                               
**NEW 6/8/88                                                                    
NETB2M   OC    RPAY,RPAY           SEE IF PAID                                  
         BNZ   NETBNXT             YES - BYPASS                                 
         B     NETB3                                                            
*                                                                               
NETBEND  DS    0H                  NO UNPAID ELEMS FOUND SKIP                   
         B     EXIT                THIS BUY                                     
*                                                                               
NETB3    DS    0H             GET HERE IF NOT DOING UNPAID REPORT               
*                             OR UNPAID AND I FOUND AN UNPAID ELEM              
*                                                                               
         MVI   ACTSW,C'Y'         SET ON ACTIVITY SWITCH                        
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVI   MEDBRAND,0                                                       
         XC    BUYDLN,BUYDLN                                                    
         MVC   BYPASS,=F'1'                                                     
*                                  BUILD BUYLINE INFO                           
         LA    R4,BUYDLN                                                        
         LLC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         MVI   3(R4),C'-'                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKBUY                                                       
         TM    BUYREC+15,BUYRLN2   TEST 2-BYTE LINE NUMBERS                     
         BZ    *+8                                                              
         ICM   R0,3,BUYKBUY                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         MVI   8(R4),C'*'                                                       
         LA    R4,10(R4)                                                        
* DISPLAY START DATE                                                            
         GOTO1 DATCON,DMCB,(3,BDSTART),(7,(R4))                                 
         MVI   5(R4),C'-'                                                       
         MVI   6(R4),C'E'                                                       
         CLI   BDINPUT,2           TEST INPUT METHOD                            
         BH    LD020               -E                                           
         BL    LD010               WEEKS                                        
* MUST BE END DATE                                                              
         GOTO1 (RF),(R1),(3,BDEND),(7,6(R4))                                    
         B     LD020                                                            
*                                                                               
LD010    SR    R0,R0                                                            
         IC    R0,BDWKS                                                         
         EDIT  (R0),(2,6(R4)),ALIGN=LEFT                                        
         LA    RE,6(R4)                                                         
         AR    RE,R0                                                            
         MVI   0(RE),C'W'                                                       
         CLI   BDWKIND,C'O'                                                     
         BE    LD020                                                            
         MVC   1(1,RE),BDWKIND                                                  
*                                                                               
LD020    MVI   11(R4),C'*'                                                      
*                                                                               
* DISPLAY DAYS                                                                  
         LA    R4,12(R4)                                                        
         GOTO1 CODAY,DMCB,(BDSEDAY,BDDAY),WORK                                  
         MVC   0(8,R4),WORK                                                     
         MVI   8(R4),C'*'                                                       
* NUMBER PER WEEK                                                               
         LA    R4,9(R4)                                                         
         SR    R0,R0                                                            
         IC    R0,BDNOWK                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         MVI   2(R4),C'*'                                                       
* TIME                                                                          
         LA    R4,3(R4)                                                         
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,DMCB,BDTIMST,WORK                                         
         MVC   0(11,R4),WORK                                                    
         MVI   11(R4),C'*'                                                      
* DPT                                                                           
         LA    R4,12(R4)                                                        
         MVC   0(1,R4),BDDAYPT                                                  
         MVI   1(R4),C'*'                                                       
* SPTLEN                                                                        
         LA    R4,2(R4)                                                         
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
         CLI   BDSEC,100                                                        
         BL    LD030                                                            
         BCTR  R4,0                                                             
         UNPK  0(3,R4),DUB                                                      
         LA    R4,1(R4)                                                         
LD030    MVI   2(R4),C'*'                                                       
*                                                                               
* PROGRAMMING                                                                   
*                                                                               
         LA    R4,3(R4)                                                         
         MVC   WORK(17),BDPROGRM                                                
LD040    MVC   0(17,R4),WORK                                                    
         MVI   19(R4),C'*'                                                      
* ADJ CODE                                                                      
         LA    R4,20(R4)                                                        
         L     R6,ADCLT                                                         
         USING CLTRECD,R6                                                       
         CLI   CPROF+9,C'0'                                                     
         BE    LD060                                                            
         MVC   WORK(1),BDPROGT                                                  
         MVI   WORK+1,C' '                                                      
         CLI   CPROF+9,C'1'      TEST ALPHA ADJ                                 
         BE    LD050               YES                                          
         SR    R0,R0                                                            
         IC    R0,BDPROGT                                                       
         SRDL  R0,4                                                             
         STC   R0,WORK                                                          
         OI    WORK,X'F0'                                                       
         SRL   R1,28                                                            
         STC   R1,WORK+1                                                        
         OI    WORK+1,X'F0'                                                     
LD050    SH    R4,=H'4'                                                         
         MVI   0(R4),C'*'                                                       
         MVC   1(2,R4),WORK                                                     
         MVI   3(R4),C'*'                                                       
         LA    R4,4(R4)                                                         
*                                                                               
* COST                                                                          
*                                                                               
AD20     TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    LD060                                                            
         MVC   WORK2(1),BDCIND                                                  
         B     LD070                                                            
LD060    MVI   WORK2,C' '                                                       
         TM    BDCIND,X'20'                                                     
         BO    LD070                                                            
         MVI   WORK2,C'F'                                                       
         TM    BDCIND,X'80'                                                     
         BO    LD070                                                            
         MVI   WORK2,C'Q'                                                       
         TM    BDCIND,X'40'                                                     
         BO    LD070                                                            
         MVI   WORK2,C'N'                                                       
         TM    BDCIND,X'10'                                                     
         BO    LD070                                                            
         MVI   WORK2,C'V'                                                       
         TM    BDCIND,X'08'                                                     
         BO    LD070                                                            
         MVI   WORK2,C'S'                                                       
         TM    BDCIND,X'04'                                                     
         BO    LD070                                                            
         MVI   WORK2,C'X'                                                       
         TM    BDCIND,X'02'                                                     
         BO    LD070                                                            
         MVI   WORK2,C'P'                                                       
*                                                                               
LD070    L     R0,BDCOST                                                        
         SRL   R0,8                                                             
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    LD080                                                            
         TM    BDCIND2,BDC2NEG     SEE IF NEGATIVE                              
         BNO   LD090                                                            
         B     LD085                                                            
LD080    TM    BDCIND,X'01'        SEE IF NEGATIVE                              
         BNO   LD090                                                            
LD085    LCR   R0,R0                                                            
*                                                                               
LD090    OC    BUYKMKT,BUYKMKT     NETWORK BUY?                                 
         BZ    LD100                                                            
         TM    BDCIND2,BDCNBRDQ    X'10' - TEST COST IN DOLLARS                 
         BO    LD105                                                            
         B     LD110                                                            
LD100    TM    BDCIND2,BDCRATPQ    X'01' - TEST COST IN PENNIES                 
         BO    LD110                                                            
LD105    MH    R0,=H'100'      ---> CONVERT TO PENNIES!                         
LD110    C     R0,=F'9999999'      IF COST TOO BIG                              
         BH    LD120                DROP CENTS                                  
         EDIT  (R0),(9,(R4)),2,FLOAT=-                                          
         B     LD130                                                            
*                                                                               
LD120    SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(9,(R4)),FLOAT=-                                            
*                                                                               
LD130    DS    0H                                                               
*                                                                               
* FLOAT COST CHAR TO LEFT OF COST                                               
*                                                                               
LD140    CLI   WORK2,C' '                                                       
         BE    LD150                                                            
* FIND FIRST BLANK TO LEFT OF COST                                              
         LA    R1,7(R4)                                                         
         LA    R0,8                                                             
         CLI   0(R1),C' '                                                       
         BE    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         MVC   0(1,R1),WORK2       MOVE SPECIAL COST CODE                       
*                                                                               
LD150    DS    0H                                                               
         L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'69'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   LD190                                                            
         OC    2(4,R2),2(R2)                                                    
         BZ    LD190                                                            
*                                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    LD160                                                            
         TM    BDCIND2,BDC2NEG     SEE IF NEGATIVE BUY                          
         BNO   LD180                                                            
         B     LD170                                                            
LD160    TM    BDCIND,X'01'        SEE IF NEGATIVE BUY                          
         BNO   LD180                                                            
LD170    L     R0,2(R2)             MUST MAKE TAX NEGATIVE                      
         LCR   R0,R0                AS IT IS ALWAYS POSTIVE IN ELEM             
         ST    R0,2(R2)                                                         
*                                                                               
LD180    LA    R4,11(R4)                                                        
         MVC   0(4,R4),=C'TAX='                                                 
         EDIT  (B4,2(R2)),(8,5(R4)),2,ALIGN=LEFT,FLOAT=-                        
         LA    R4,4(R4)            LD190 WILL BUMP R4 BY 10                     
*                                                                               
LD190    DS    0H                                                               
         BRAS  RE,GETCAP                                                        
         CLI   WORK2,C' '                                                       
         BNH   LD200                                                            
         MVC   10(32,R4),WORK2     MOVE CAPTION                                 
*                                                                               
LD200    DS    0H                                                               
         MVI   FULL,0                                                           
         L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'66'                                                     
*                                                                               
NETBUYB  BRAS  RE,NEXTEL                                                        
         BNE   NBUY010                                                          
         LLC   R0,2(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(1),DUB                                                       
         MVI   P+5,C'-'                                                         
         LLC   R5,1(R2)                                                         
         SH    R5,=H'4'                                                         
         LTR   R5,R5                                                            
         BM    NETBUYB             NO TEXT TO COMMENT                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),3(R2)       * EXCUTED *                                   
*                                                                               
NETBUYD  GOTO1 APRINTIT,DMCB,(RA)                                               
         OI    FULL,1                                                           
         B     NETBUYB                                                          
         EJECT                                                                  
NBUY010  DS    0H                                                               
         BRAS  RE,PROCESS                                                       
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
PROCESS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
PROC5    CLI   FULL,0                                                           
         BE    PROC5C                                                           
         GOTO1 APRINTIT,DMCB,(RA)  SKIP AFTER COMMENTS                          
*                                                                               
PROC5C   MVC   MID1(5),=C'STATN'                                                
         MVC   MID1+8(6),=C'MARKET'                                             
         MVC   MID1+39(4),=C'COST'                                              
         MVC   MID2(5),=6C'-'                                                   
         MVC   MID2+8(6),=6C'-'                                                 
         MVC   MID2+39(4),=6C'-'                                                
*                                                                               
NBUY015  LA    R6,MID1+45                                                       
         LA    R5,14               CAN ONLY DO 14 WEEKS/PAGE                    
         CLI   QOPT4,C'U'          UNPAID REPORT                                
         BNE   NBUY020                                                          
         LA    R5,15               MAX IS 15 FOR UNPAID REPORT                  
         CLI   PROGPROF+4,C'Y'     SEE IF DOING GST+PST COLUMNS                 
         BNE   *+8                                                              
         LA    R5,10               MAX IS 10                                    
*                                                                               
NBUY020  L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
*** NEW CODE FOR OTO'S                                                          
***OTO                                                                          
NBUY025  CLI   0(R2),X'0B'                                                      
         BE    NBUY035                                                          
         CLI   0(R2),X'0C'            OTO                                       
         BE    NBUY035                                                          
         CLI   0(R2),0                                                          
         BE    NBUY080                                                          
NBUY030  LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     NBUY025                                                          
*                                                                               
         USING REGELEM,R2                                                       
NBUY035  CLI   0(R2),X'0C'                                                      
         BNE   NBUY040                                                          
         TM    RSTATUS,X'80'       SEE IF MINUSED                               
         BNZ   NBUY030             SKIP MINUSED OTO'S                           
*                                                                               
NBUY040  CLC   RDATE,BQSTARTP      IGNORE ELEMS OUT OF REQ PERIOD               
         BL    NBUY030                                                          
         CLC   RDATE,BQENDP                                                     
         BH    NBUY030                                                          
         CLI   QOPT4,C'U'                                                       
         BNE   NBUY070                                                          
**NEW 10/4/90                                                                   
         TM    RSTATUS,X'04'       CAN SKIP HIATUS FOR UNPAID REPORT            
         BNZ   NBUY030                                                          
**NEW 6/8/88                                                                    
         TM    RSTATUS,X'40'       SEE IF MINUSED                               
         BZ    NBUY065             NO - JUST CHK IF PAID                        
         OC    RPAY,RPAY      IF MINUSED AND PAID SHOW AS UNPAID                
         BNZ   NBUY050        UNLESS NEXT ELEM (X'0C') IS PAID                  
         B     NBUY070        MINUSED AND UNPAID - STILL SHOW                   
*                             SINCE STATION MIGHT BE DIFFERENT                  
*                                                                               
NBUY050  ST    R2,SAVR2           MUST SEE IF NEXT ELEM IS PAID                 
         CLI   0(R2),X'0B'        ONLY IF PROCESSING X'0B' ELEM                 
         BNE   NBUY055                                                          
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'0C'        MAKE GOOD                                     
         BNE   NBUY055                                                          
         TM    RSTATUS,X'80'                                                    
         BZ    NBUY055                                                          
         OC    RPAY,RPAY                                                        
         BNZ   NBUY060                                                          
NBUY055  L     R2,SAVR2         NEXT ELEM IS UNPAID OR DOES NOT EXIST           
         B     NBUY070          SO PROCESS AS UNPAID                            
NBUY060  L     R2,SAVR2           NEXT ELEM IS PAID SO BYPASS                   
         B     NBUY030                                                          
*                                                                               
**NEW 6/8/88                                                                    
NBUY065  OC    RPAY,RPAY           SEE IF PAID                                  
         BNZ   NBUY030             YES - BYPASS                                 
*                                                                               
NBUY070  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RDATE),(5,WORK)                                   
         MVC   0(3,R6),WORK                                                     
         MVC   132(2,R6),WORK+3       DAY IN MID2                               
         MVI   RCODE,X'F5'         NETBUY9 WILL LOOK FOR X'F5' ELEMS            
         LA    R6,4(R6)                                                         
         BCT   R5,NBUY030                                                       
*                                                                               
NBUY080  LA    R5,MID1                                                          
         SR    R6,R5                                                            
         ST    R6,DEMDISP          SAVE DISPLACEMENT OF DEMOS                   
*                                                                               
         CLI   QOPT4,C'U'          UNPAID REPORT                                
         BNE   NBUY090                                                          
         LA    R5,MID1                                                          
         A     R5,DEMDISP                                                       
         MVC   0(10,R5),=C'GROSS UNPD'                                          
         MVC   12(8,R5),=C'NET UNPD'                                            
         MVC   24(3,R5),=C'TAX'                                                 
         MVC   132(10,R5),=CL10'----------'                                     
         MVC   144(8,R5),=CL10'----------'                                      
         MVC   156(3,R5),=CL10'----------'                                      
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   NBUY100                                                          
         MVC   32(3,R5),=C'GST'                                                 
         MVC   164(3,R5),=C'---'                                                
         MVC   40(3,R5),=C'PST'                                                 
         MVC   172(3,R5),=C'---'                                                
         B     NBUY100                                                          
*                                                                               
*                                  PUT DEMO DESCRIPTIONS IN MIDLINES            
NBUY090  DS    0H                                                               
         CLC   BYPASS,=F'1'        FIRST TIME                                   
         BNE   M14B                DON'T NEED TO FIND DEMOS                     
         XC    DNAME1(28),DNAME1                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'       CANADIAN                                     
*                                  GET DEMOS FROM PRDBUFF                       
         L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    R5,28(RE,RF)                                                     
         GOTO1 DEMOCON,DMCB,(4,(R5)),(2,DNAME1),(C'S',DBLOCK),         X        
               (SPOTPROF+9,EUSRNMS)                                             
         DROP  R6                                                               
*                                                                               
M14B     DS    0H                                                               
         LA    R5,MID1                                                          
         A     R5,DEMDISP                                                       
         MVC   0(7,R5),DNAME1                                                   
         MVC   8(7,R5),DNAME1+7                                                 
         MVC   16(7,R5),DNAME1+14                                               
         MVC   24(7,R5),DNAME1+21                                               
         CLC   DNAME1(7),SPACES                                                 
         BNH   *+10                                                             
         MVC   132(7,R5),=7C'-'                                                 
         CLC   DNAME1+7(7),SPACES                                               
         BNH   *+10                                                             
         MVC   140(7,R5),=7C'-'                                                 
         CLC   DNAME1+14(7),SPACES                                              
         BNH   *+10                                                             
         MVC   148(7,R5),=7C'-'                                                 
         CLC   DNAME1+21(7),SPACES                                              
         BNH   *+10                                                             
         MVC   156(7,R5),=7C'-'                                                 
         EJECT                                                                  
NBUY100  MVI   FORCEMID,C'Y'             PRINT NETWK BUY INFO                   
         MVC   P(5),=C'*ALL*'                                                   
         CLI   BDNRGN,C' '                                                      
         BNH   NBUY110                                                          
         MVC   P+6(6),=C'REGION'                                                
         MVC   P+13(1),BDNRGN                                                   
*                                                                               
NBUY110  DS    0H                  COST DISPLAY                                 
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),BDCOST                                                 
         L     R5,FULL                                                          
*                                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    NBUY120                                                          
         TM    BDCIND2,BDC2NEG     SEE IF NEGATIVE                              
         BNO   NBUY127                                                          
         B     NBUY125                                                          
NBUY120  TM    BDCIND,X'01'        SEE IF NEGATIVE                              
         BNO   NBUY127                                                          
NBUY125  LCR   R5,R5               SWITCH TO NEGATIVE                           
*                                                                               
NBUY127  OC    BUYKMKT,BUYKMKT     NETWORK BUY?                                 
         BZ    NBUY130                                                          
         TM    BDCIND2,BDCNBRDQ    X'10' - TEST COST IN DOLLARS                 
         BO    NBUY135                                                          
         B     NBUY140                                                          
NBUY130  TM    BDCIND2,BDCRATPQ    X'01' - TEST COST IN PENNIES                 
         BO    NBUY140                                                          
NBUY135  M     R4,=F'100'          NO-NETWORK BUY IS IN DOLLARS                 
NBUY140  EDIT  (R5),(10,P+33),2,FLOAT=-                                         
*                                                                               
         DROP  RE                                                               
NBUY145  DS    0H                  DISPLAY PRODUCT CODES                        
*                                  IF MINUSED PUT *M                            
         LA    R6,P+45                                                          
         L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
         XC    NPRDTAB,NPRDTAB                                                  
*                                  USED TO SAVE NETBUY PRDS                     
         LA    R7,NPRDTAB                                                       
         MVI   ELCODE,X'F5'                                                     
NBUY150  BAS   RE,NEXTEL                                                        
         BNE   NBUY200                                                          
         USING REGELEM,R2                                                       
         MVI   RCODE,X'F6'         SO I'LL BYPASS THIS ELEM                     
*                                  I'M THROUGH WITH IT                          
         TM    RSTATUS,X'40'       CHK MINUSED                                  
         BZ    NBUY160                                                          
         MVC   0(3,R6),=C'*M '                                                  
         MVI   0(R7),0                                                          
         MVI   1(R7),0                                                          
         B     NBUY190                                                          
*                                                                               
NBUY160  DS    0H                                                               
         TM    RSTATUS,X'04'       HIATUS                                       
         BZ    NBUY165                                                          
         MVC   0(3,R6),=C'*H '                                                  
         MVI   0(R7),0                                                          
         MVI   1(R7),0                                                          
         B     NBUY190                                                          
*                                                                               
NBUY165  DS    0H                                                               
         CLI   RLEN,10             UNALLOCATED                                  
         BNE   NBUY170                                                          
         MVC   0(3,R6),=C'*UA'                                                  
         MVI   0(R7),0                                                          
         MVI   1(R7),0                                                          
         B     NBUY190                                                          
*                                                                               
NBUY170  MVC   0(1,R7),RPPRD                                                    
         SR    RE,RE                                                            
         IC    RE,RPPRD                                                         
         CLI   RPPRD,X'FF'                                                      
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,0(RE,RF)                                                      
         MVC   0(3,R6),1(RE)                                                    
*                                                                               
         CLI   RLEN,14             TEST ONE PRODUCT ALLOCATED                   
         BNE   NBUY180                                                          
         TM    BDSTAT3,BDST3_SPODS TEST SPODS ALLOWED                           
         BZ    NBUY180                                                          
         CLC   BDSEC,RPTIME        TEST SPOT SLN = BUYREC SLN                   
         BE    NBUY180                                                          
         SR    R0,R0               DISPLAY LEN OF SPOD                          
         IC    R0,RPTIME                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  132(2,R6),DUB                                                    
         CHI   R0,99                                                            
         BNH   *+10                                                             
         UNPK  132(3,R6),DUB                                                    
*                                                                               
NBUY180  CLI   RLEN,18            SEE IF THIS IS A PIGGYBACK ELEM               
         BL    NBUY190                                                          
         SR    RE,RE                                                            
         MVC   1(1,R7),RPPRD+4                                                  
         IC    RE,RPPRD+4         PIGGYBACK PRD                                 
         CLI   RPPRD+4,X'FF'                                                    
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,0(RE,RF)                                                      
         MVC   132(3,R6),1(RE)                                                  
*                                                                               
NBUY190  LA    R6,4(R6)                                                         
         LA    R7,2(R7)                                                         
         B     NBUY150                                                          
NBUY200  DS    0H                                                               
         MVI   0(R7),X'FF'         LAST ELEM TO DISPLAY                         
         MVI   1(R7),X'FF'         LAST ELEM TO DISPLAY                         
         GOTO1 APRINTIT,DMCB,(RA)                                               
         EJECT                                                                  
SBUY     DS    0H                                                               
         XC    SUNPDGR(20),SUNPDGR   CLEAR STAT                                 
         XC    NDEMTOT1(16),NDEMTOT1       CLEAR DEMO TOTAL                     
         CLI   NPRDTAB,X'FF'                                                    
         BNE   SBUY1                                                            
         CLI   QOPT4,C'U'                                                       
         BNE   SBUY0                                                            
         OC    NUNPDGR(20),NUNPDGR     SEE IF NETWORK HAS $                     
         BZ    SBUY0                   YES - SKIP MESSAGE                       
         BRAS  RE,SBUYEND                                                       
         B     PEXIT                                                            
*                                                                               
*              NO SPOTS REPORTED                                                
SBUY0    MVC   P+45(32),=C'* NO SPOTS IN REQUESTED PERIOD *'                    
         GOTO1 APRINTIT,DMCB,(RA)                                               
         BRAS  RE,SBUYEND                                                       
         B     PEXIT                                                            
*                                                                               
SBUY1    GOTO1 APRINTIT,DMCB,(RA)  SKIP A LINE                                  
*                                  NETWORK STATION ELEMS                        
         L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'68'                                                     
         USING NTWKELEM,R2                                                      
*                                                                               
SBUY2    BAS   RE,NEXTEL                                                        
         BNE   SBUYX                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),MYBUYKEY      USE NETWK BUY KEY                          
         MVC   KEY+4(5),NTWKMKST                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SBUY2               NO BUY TRY FOR NEXT STAT/MKT                 
*                                                                               
         LA    R3,REC                                                           
         ST    R3,AREC                                                          
         GOTO1 GET                                                              
         USING BUYRECD,R3                                                       
*                                                                               
         CLI   PROGPROF+5,C'Y'                                                  
         BNE   *+8                                                              
         BRAS  RE,DEMOFIND                                                      
*                                                                               
         CLI   QOPT4,C'B'          SEE IF DOING A TURNAROUND                    
         BNE   SBUY3               NO                                           
         CLC   TODAYB,BDCHG                                                     
         BH    SBUY3                                                            
         MVI   P,C'*'              FLAG STATION BUYS CHGED TODAY                
SBUY3    DS    0H                                                               
         MVC   KEY(15),=15C'0'                                                  
         XC    KEY+15(2),KEY+15                                                 
         GOTO1 MSUNPK,DMCB,(X'80',REC+4),KEY+2,DUB                              
         MVC   P(4),DUB                                                         
         CLI   BUYKMSTA+4,X'B0'    TEST CONVERTED CABLE                         
         BL    *+10                                                             
         MVC   P(7),DUB                                                         
*                                                                               
         MVC   P+8(4),KEY+2        MKT                                          
         MVC   KEY(2),=C'MT'                                                    
         MVC   KEY+6(2),AGY                                                     
*                                                                               
         L     R6,ADMARKET                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         CLC   0(15,R6),KEY                                                     
         BE    SBUY4                                                            
         MVC   P+11(15),=C'*** UNKNOWN ***'                                     
         B     SBUY6                                                            
*                                                                               
SBUY4    MVC   P+13(22),18(R6)                                                  
*                                                                               
SBUY6    MVC   SAVER2,ADBUY                                                     
         LA    R0,REC                                                           
         ST    R0,ADBUY            FOR MEDGETBY                                 
         MVC   SVBUYKEY(13),REC                                                 
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
***NOP   MVC   MEDSPTLN,BDSEC                                                   
         MVI   MEDBRAND,X'FF'                                                   
         MVC   DMCB+4(4),=F'2'                                                  
         CLI   QOPT4,C'U'          SEE IF DOING UNPAID REPORT                   
         BNE   SBUY6A              NO                                           
         XC    DMCB+4(4),DMCB+4                                                 
SBUY6A   GOTO1 MEDGETBY,DMCB,(RA)                                               
*                                                                               
         MVC   ADBUY,SAVER2        RESTORE,ADBUY                                
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LA    RF,MEDPERD                                                       
         L     R8,4(RF)                                                         
         USING MEDDATA,R8                                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),BDCOST                                                 
         L     R5,FULL                                                          
*                                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    SBUY6A1A                                                         
         TM    BDCIND2,BDC2NEG     SEE IF NEGATIVE                              
         BNO   SBUY6A2                                                          
         B     SBUY6A1B                                                         
SBUY6A1A TM    BDCIND,X'01'        SEE IF NEGATIVE                              
         BNO   SBUY6A2                                                          
SBUY6A1B LCR   R5,R5               SWITCH TO NEGATIVE                           
*                                                                               
SBUY6A2  OC    BUYKMKT,BUYKMKT     NETWORK BUY?                                 
         BZ    SBUY6A3                                                          
         TM    BDCIND2,BDCNBRDQ    X'10' - TEST COST IN DOLLARS                 
         BO    SBUY6A4                                                          
         B     SBUY6A5                                                          
SBUY6A3  TM    BDCIND2,BDCRATPQ    X'01' - TEST COST IN PENNIES                 
         BO    SBUY6A5                                                          
SBUY6A4  M     R4,=F'100'          NO-NETWORK BUY IS IN DOLLARS                 
SBUY6A5  EDIT  (R5),(10,P+33),2,FLOAT=-                                         
*                                                                               
         LA    R5,P                                                             
         A     R5,DEMDISP          ADD DEMO DISP                                
         CLI   QOPT4,C'U'          UNPAID REPORT                                
         BNE   SBUY6D                                                           
         MVC   SUNPDGR,MEDBYUNP                                                 
         MVC   SUNPDNET,MEDBYNUP                                                
         XC    SUNPDTAX,SUNPDTAX                                                
         OC    BDNTAX,BDNTAX                                                    
         BNZ   SBUY6A8                                                          
         MVC   SUNPDTAX,MEDBYTXU   MEDGETBY'S UNPAID TAX                        
         B     SBUY6B                                                           
*                                                                               
SBUY6A8  DS    0H                                                               
         MVC   SAVTAX,BDNTAX       SAVE TAX                                     
         XC    BDNTAX,BDNTAX                                                    
         MVC   SAVER2,ADBUY                                                     
         LA    R0,REC                                                           
         ST    R0,ADBUY                                                         
         XC    DMCB+4(4),DMCB+4                                                 
         GOTO1 MEDGETBY,DMCB,(RA)                                               
         MVC   ADBUY,SAVER2                                                     
         L     R6,SUNPDGR                                                       
         S     R6,MEDBYUNP                                                      
         ST    R6,SUNPDTAX           DIFFERENCE IS TAX                          
         MVC   BDNTAX,SAVTAX         RESTORE TAX                                
*                                                                               
SBUY6B   DS    0H                    SEE IF I NEED GST                          
*                                                                               
SBUY6B5  DS    0H                      MUST GO TO MEDGETBUY AGAIN               
         MVI   RQGSTOPT,C'N'           ASKING FOR NO GST                        
         MVC   SAVER2,ADBUY                                                     
         LA    R0,REC                                                           
         ST    R0,ADBUY                                                         
         XC    DMCB+4(4),DMCB+4                                                 
         GOTO1 MEDGETBY,DMCB,(RA)                                               
         MVC   ADBUY,SAVER2                                                     
         L     R6,SUNPDGR            (GROSS WITH GST)                           
         S     R6,MEDBYUNP           (GROSS)                                    
         ST    R6,SUNPDGST           DIFFERENCE IS UNPAID GST                   
*                                                                               
         MVI   RQGSTOPT,C'P'           ASKING FOR GROSS WITH PST                
         L     R0,MEDBYUNP        SAVE GROSS UNPAID (NO GST OR PST)             
         ST    R0,SUNPDPST        SAVE IN UNPAID PST                            
         MVC   SAVER2,ADBUY                                                     
         LA    R0,REC                                                           
         ST    R0,ADBUY                                                         
         XC    DMCB+4(4),DMCB+4                                                 
         GOTO1 MEDGETBY,DMCB,(RA)                                               
         MVC   ADBUY,SAVER2                                                     
         L     R6,MEDBYUNP           (GROSS WITH PST)                           
         S     R6,SUNPDPST           (GROSS)                                    
         ST    R6,SUNPDPST           DIFFERENCE IS UNPAID PST                   
         MVI   RQGSTOPT,C'G'          RESET FOR GST WITHOUT PST                 
*                                                                               
         L     R6,SUNPDGR           AT THIS POINT UNPAID GROSS                  
*                                   INLUDES GST                                 
         A     R6,SUNPDPST          ADD IN UNPAID PST                           
         ST    R6,SUNPDGR           UNPAID GROSS NOW INCLUDES GST+PST           
*                                                                               
         L     R6,SUNPDNET          AT THIS POINT UNPAID NET                    
*                                   INLUDES GST                                 
         A     R6,SUNPDPST          ADD IN UNPAID PST                           
         ST    R6,SUNPDNET          UNPAID NET NOW INCLUDES GST+PST             
*                                                                               
SBUY6BX  DS    0H                                                               
         CLI   PROGPROF+0,C'Y'      SEE IF INCLUDING TAX IN GROSS               
         BE    SBUY6BX2                                                         
         L     R6,SUNPDGR                                                       
         S     R6,SUNPDTAX                                                      
         ST    R6,SUNPDGR                                                       
*                                                                               
SBUY6BX2 DS    0H                                                               
         CLI   PROGPROF+2,C'Y'      SEE IF INCLUDING TAX IN NET                 
         BE    SBUY6BX3                                                         
         L     R6,SUNPDNET                                                      
         S     R6,SUNPDTAX                                                      
         ST    R6,SUNPDNET                                                      
*                                                                               
SBUY6BX3 DS    0H                                                               
         CLI   PROGPROF+1,C'Y'      SEE IF INCLUDING GST/PST IN GROSS           
         BE    SBUY6BX4                                                         
         L     R6,SUNPDGR                                                       
         S     R6,SUNPDGST                                                      
         S     R6,SUNPDPST                                                      
         ST    R6,SUNPDGR                                                       
*                                                                               
SBUY6BX4 DS    0H                                                               
         CLI   PROGPROF+3,C'Y'      SEE IF INCLUDING GST/PST IN NET             
         BE    SBUY6BX5                                                         
         L     R6,SUNPDNET                                                      
         S     R6,SUNPDGST                                                      
         S     R6,SUNPDPST                                                      
         ST    R6,SUNPDNET                                                      
*                                                                               
SBUY6BX5 DS    0H                                                               
         EDIT  SUNPDGR,(10,0(R5)),2,FLOAT=-                                     
         EDIT  SUNPDNET,(10,10(R5)),2,FLOAT=-                                   
         EDIT  SUNPDTAX,(7,20(R5)),2,FLOAT=-,ZERO=BLANK                         
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   SBUY6BXX                                                         
         EDIT  SUNPDGST,(8,27(R5)),2,FLOAT=-,ZERO=BLANK                         
         EDIT  SUNPDPST,(8,35(R5)),2,FLOAT=-,ZERO=BLANK                         
*                                                                               
SBUY6BXX LA    R3,5                ROLL TO NETWORK TOTALS                       
         LA    R4,NUNPDGR                                                       
         LA    R5,SUNPDGR                                                       
*                                                                               
SBUY6C   L     R6,0(R4)                                                         
         A     R6,0(R5)                                                         
         ST    R6,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,SBUY6C                                                        
         B     SBUY9                                                            
*                                                                               
SBUY6D   LA    R3,P                                                             
         A     R3,DEMDISP                                                       
         LA    R6,4                                                             
         LA    R7,MEDBY1                                                        
         ST    R2,SAVER2                                                        
         LA    R2,NDEMTOT1                                                      
         LA    R1,DNAME1                                                        
*                                                                               
SBUY7    L     R5,0(R7)                                                         
         M     R4,=F'1'                                                         
         CLC   MEDBYSPT,=F'0'      NO SPOTS                                     
         BNE   SBUY7A                                                           
         XC    0(4,R7),0(R7)       AVOID DIVIDE BY ZERO                         
         B     SBUY8                                                            
*                                                                               
SBUY7A   D     R4,MEDBYSPT         DIVIDE BY NO. OF SPOTS                       
         ST    R5,0(R7)                                                         
                                                                                
SBUY8    DS    0H                                                               
*                                                                               
         BRAS  RE,DEMODIS                                                       
*                                                                               
         CLI   0(R1),C'R'          SEE IF RATING                                
         BE    SBUY8E                                                           
         CLI   0(R1),C'E'          OR EXTENDED DEMO                             
         BE    SBUY8E                                                           
         L     R0,0(R2)                                                         
         A     R0,0(R7)                                                         
         ST    R0,0(R2)                                                         
*                                                                               
SBUY8E   DS    0H                                                               
         LA    R2,4(R2)                                                         
         LA    R1,7(R1)                                                         
         LA    R3,8(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R6,SBUY7                                                         
         L     R2,SAVER2                                                        
*                                                                               
SBUY9    LA    R3,REC              RESTORE R3                                   
*                                                                               
         DROP  RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
SBUY10   DS    0H                                                               
         ST    R2,SAVER2                                                        
         LA    R2,REC+24                                                        
         USING REGELEM,R2                                                       
         LA    R6,P+45                                                          
         LA    R7,NPRDTAB                                                       
         L     R5,BYPASS           SET R5 TO NUMBER OF ELEMS                    
*                                  TO BYPASS                                    
*** NEW CODE FOR OTO'S                                                          
***OTO                                                                          
SBUY11   DS    0H                                                               
         CLI   0(R2),X'0B'                                                      
         BE    SBUY11P                                                          
         CLI   0(R2),X'0C'                                                      
         BE    SBUY11P                                                          
*                                                                               
SBUYNEXT CLI   0(R2),0                                                          
         BE    SBUY12X                                                          
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SBUY11                                                           
*                                                                               
SBUY11P  CLI   0(R2),X'0C'                                                      
         BNE   SBUY11PC                                                         
         TM    RSTATUS,X'80'       SKIP MINUSED OTO'S                           
         BNZ   SBUYNEXT                                                         
*                                                                               
SBUY11PC CLC   RDATE,BQSTARTP      IGNORE ELEMS OUT OF REQ PERIOD               
         BL    SBUYNEXT                                                         
         CLC   RDATE,BQENDP                                                     
         BH    SBUYNEXT                                                         
         CLI   QOPT4,C'U'          UNPAID REPORT                                
         BNE   SBUY11X                                                          
**NEW 10/4/90                                                                   
         TM    RSTATUS,X'04'       SEE IF HAITUS                                
         BNZ   SBUYNEXT            CAN SKIP FOR UNPAID REPORT                   
*                                                                               
**NEW 6/8/88                                                                    
         TM    RSTATUS,X'40'       SEE IF MINUSED                               
         BZ    SBUY11M             NO - JUST CHK IF PAID                        
         OC    RPAY,RPAY      IF MINUSED AND PAID SHOW AS UNPAID                
         BNZ   SBUY11A       UNLESS NEXT ELEM (X'0C') IS PAID                   
         B     SBUY11X        MINUSED AND UNPAID - STILL PROCESS                
*                             SINCE NETWORK MIGHT NOT BE MINUSED                
*                                                                               
SBUY11A  ST    R2,SAVR2           MUST SEE IF NEXT ELEM IS PAID                 
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'0C'        MAKE GOOD                                     
         BNE   SBUY11B                                                          
         TM    RSTATUS,X'80'                                                    
         BZ    SBUY11B                                                          
         OC    RPAY,RPAY                                                        
         BNZ   SBUY11C                                                          
SBUY11B  L     R2,SAVR2         NEXT ELEM IS UNPAID OR DOES NOT EXIST           
         B     SBUY11X         SO PROCESS AS UNPAID                             
SBUY11C  L     R2,SAVR2           NEXT ELEM IS PAID SO BYPASS                   
         B     SBUYNEXT                                                         
*                                                                               
**NEW 6/8/88                                                                    
SBUY11M  OC    RPAY,RPAY           SEE IF PAID                                  
         BNZ   SBUYNEXT            YES - BYPASS                                 
*                                                                               
SBUY11X  BCT   R5,SBUYNEXT         SKIPS ELEM PREVIOUSLY DISPLAYED              
         B     SBUY12A                                                          
*                                                                               
*                                                                               
*** NEW CODE FOR OTO'S                                                          
***OTO                                                                          
SBUY12   LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SBUY12X                                                          
         CLI   0(R2),X'0B'                                                      
         BE    SBUY12A                                                          
         CLI   0(R2),X'0C'             OTO                                      
         BE    SBUY12A                                                          
         B     SBUY12                                                           
*                                                                               
SBUY12A  DS    0H                                                               
         CLI   0(R7),X'FF'         LAST ELEM DISPLAYED                          
         BE    SBUY12X                                                          
         CLI   0(R2),X'0C'                                                      
         BNE   SBUY12A5                                                         
         TM    RSTATUS,X'80'      SKIP MINUSED OTO'S                            
         BNZ   SBUY12                                                           
*                                                                               
SBUY12A5 CLC   RDATE,BQSTARTP      IGNORE ELEMS OUT OF REQ PERIOD               
         BL    SBUY12                                                           
         CLC   RDATE,BQENDP                                                     
         BH    SBUY12                                                           
         CLI   QOPT4,C'U'          UNPAID REPORT                                
         BNE   SBUY12A1                                                         
**NEW 10/4/90                                                                   
         TM    RSTATUS,X'04'       SKIP HIATUS FOR UNPAID REPORT                
         BNZ   SBUY12                                                           
**NEW 6/8/88                                                                    
         TM    RSTATUS,X'40'       SEE IF MINUSED                               
         BZ    STAB12M             NO - JUST CHK IF PAID                        
         OC    RPAY,RPAY      IF MINUSED AND PAID SHOW AS UNPAID                
         BNZ   STAB12A       UNLESS NEXT ELEM (X'0C') IS PAID                   
         B     SBUY12A1       MINUSED AND UNPAID - STILL REPORT                 
*                             SINCE NETWORK MIGHT NOT BE MINUSED                
*                                                                               
STAB12A  ST    R2,SAVR2           MUST SEE IF NEXT ELEM IS PAID                 
         CLI   0(R2),X'0B'                                                      
         BNE   STAB12B                                                          
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'0C'        MAKE GOOD                                     
         BNE   STAB12B                                                          
         TM    RSTATUS,X'80'                                                    
         BZ    STAB12B                                                          
         OC    RPAY,RPAY                                                        
         BNZ   STAB12C                                                          
STAB12B  L     R2,SAVR2         NEXT ELEM IS UNPAID OR DOES NOT EXIST           
         B     SBUY12A1        SO PROCESS AS UNPAID                             
STAB12C  L     R2,SAVR2           NEXT ELEM IS PAID SO BYPASS                   
         B     SBUY12                                                           
*                                                                               
**NEW 6/8/88                                                                    
STAB12M  OC    RPAY,RPAY           UNPD ITEMS ONLY                              
         BNZ   SBUY12                                                           
*                                                                               
SBUY12A1 DS    0H                                                               
         TM    RSTATUS,X'40'       MINUSED                                      
         BZ    SBUY12A2                                                         
         MVC   0(3,R6),=C'*M '                                                  
         B     SBUY12D                                                          
*                                                                               
SBUY12A2 DS    0H                                                               
         TM    RSTATUS,X'04'       HIATUS                                       
         BZ    SBUY12A4                                                         
         MVC   0(3,R6),=C'*H '                                                  
         B     SBUY12D                                                          
*                                                                               
SBUY12A4 CLI   RLEN,X'0A'          UNALLOCATED                                  
         BNE   SBUY12A6                                                         
         MVC   0(3,R6),=C'*UA'                                                  
         B     SBUY12D                                                          
*                                                                               
SBUY12A6 CLC   RPPRD,0(R7)                                                      
         BNE   SBUY12B                                                          
         CLI   RLEN,X'12'          IF PIGGYBACK  - PIGGYBACK PRD                
*                                  MUST ALSO MATCH NETWORK BUY                  
         BNL   SBUY12A7                                                         
         CLI   1(R7),0             SEE IF NETWORK WAS PIGGY BACK                
         BNE   SBUY12B             YES - SHOW STATION CUT-IN                    
         B     SBUY12A9                                                         
*                                                                               
SBUY12A7 CLC   RPPRD+4(1),1(R7)                                                 
         BNE   SBUY12B                                                          
SBUY12A9 MVI   0(R6),C'-'          MATCHES NETWK PRD                            
         B     SBUY12D                                                          
*                                                                               
SBUY12B  SR    RE,RE                                                            
         IC    RE,RPPRD                                                         
         CLI   RPPRD,X'FF'                                                      
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,0(RE,RF)                                                      
         MVC   0(3,R6),1(RE)                                                    
*                                                                               
         CLI   RLEN,X'12'         SEE IF THIS IS A PIGGYBACK ELEM               
         BL    SBUY12D                                                          
         SR    RE,RE                                                            
         IC    RE,RPPRD+4         PIGGYBACK PRD                                 
         CLI   RPPRD+4,X'FF'                                                    
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,0(RE,RF)                                                      
         MVC   132(3,R6),1(RE)                                                  
*                                                                               
SBUY12D  DS    0H                                                               
*                                                                               
SBUY12E  DS    0H        FOR NON-ZERO COST DISPLAY                              
*                                                                               
SBUY12F  LA    R7,2(R7)                                                         
         LA    R6,4(R6)                                                         
         B     SBUY12                                                           
*                                                                               
SBUY12X  GOTO1 APRINTIT,DMCB,(RA)                                               
         L     R2,SAVER2           RESET R2 TO NET BUY ELEM                     
*                                                                               
         CLI   QOPT4,C'U'                                                       
         BE    SBUY20              UNPAID REPORT?                               
*                                                                               
         CLI   QOPT6,C'Y'          DISPLAY SPILL OR NO?                         
         BNE   SBUY20                                                           
*                                                                               
******   DISPLAY OF SPILL MARKETS  ******                                       
*                                                                               
         MVC   KEY1,KEY            BACKUP THE KEY                               
*                                                                               
         LA    R7,XSORTTAB         WE ARE SORTING BY AGY MKT NUM                
         SR    R5,R5               COUNTER OF SORT ENTRIES                      
         L     R2,AREC             THE ADDRESS OF BUY REC PER STATION           
         LA    R2,24(R2)                                                        
*                                                                               
         MVI   ELCODE,X'03'        SPILL ELEMENT                                
SBUY13   BAS   RE,NEXTEL                                                        
         BNE   SBUY20              NO SPILL, EXIT THIS SECTION                  
*                                                                               
SBUY13A  MVC   0(2,R7),4(R2)       STORE THE AGY MKT NUMBER IN TABLE            
         ST    R2,2(R7)            STORE THE DISK ADD OF THE ELEMENT            
         LA    R5,1(R5)                                                         
         LA    R7,6(R7)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   SBUY13B             NO MORE SPILL, SORT THE TABLE                
         B     SBUY13A             GET THE NEXT ELEMENT                         
*                                                                               
SBUY13B  GOTO1 XSORT,DMCB,(0,XSORTTAB),(R5),6,2,0                               
         LA    R7,XSORTTAB                                                      
*                                                                               
SBUY13C  MVC   KEY(17),=17C'0'     GET MARKET NAME FROM MKT RECORD              
         MVC   KEY(2),=C'MT'                                                    
         LH    R0,0(R7)            MARKET NUMBER                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGY                                                     
         L     R6,ADMARKET                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(15,R6),KEY                                                     
         BE    SBUY14                                                           
         MVC   P+11(15),=C'*** UNKNOWN ***'                                     
         B     SBUY15                                                           
*                                                                               
SBUY14   MVC   P+11(24),18(R6)                                                  
         MVC   P+6(4),KEY+2                                                     
*                                                                               
         L     R2,2(R7)            LOAD THE ADDRESS OF SPECIFIC ELEMENT         
         GOTO1 ASPDEMO,DMCB,(RA)                                                
*                                                                               
SBUY15   GOTO1 REPORT                                                           
         LA    R7,6(R7)                                                         
         BCT   R5,SBUY13C          NEXT SORTED ENTRY                            
*                                                                               
SBUY20   L     R2,SAVER2           RESTORE R2 TO POINT TO BUY REC               
         MVC   KEY,KEY1            RESTORE THE KEY BEFORE SPILL READS           
         MVI   ELCODE,X'68'        NEED TO RESET ELCODE                         
         B     SBUY2               GO DO NEXT STAT/MKT                          
*                                                                               
*                                  SEE IF ANY MORE ELEMENTS                     
*                                  WILL BE PROCESSED                            
*****************************************                                       
SBUYX    DS    0H                                                               
         L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'0B'                                                     
         SR    R7,R7                                                            
*                                                                               
***NEW CODE FOR OTO'S                                                           
***OTO                                                                          
*                                                                               
SBUYXB   LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SBUYXD                                                           
         CLI   0(R2),X'0B'                                                      
         BE    SBUYXB0                                                          
         CLI   0(R2),X'0C'                                                      
         BE    SBUYXB0                                                          
         B     SBUYXB                                                           
*                                                                               
         USING REGELEM,R2                                                       
SBUYXB0  CLI   0(R2),X'0C'         SKIP MINUSED OTO'S                           
         BNE   SBUYXB1                                                          
         TM    RSTATUS,X'80'                                                    
         BNZ   SBUYXB                                                           
SBUYXB1  CLC   RDATE,BQSTARTP                                                   
         BL    SBUYXB                                                           
         CLC   RDATE,BQENDP                                                     
         BH    SBUYXB                                                           
         CLI   QOPT4,C'U'          SEE IF DOING UNPAID RPT                      
         BNE   SBUYXC              NO                                           
**NEW 10/4/90                                                                   
         TM    RSTATUS,X'04'       SKIP HIATUS FOR UNPAID REPORT                
         BNZ   SBUYXB                                                           
**NEW 6/7/88                                                                    
         TM    RSTATUS,X'40'       SEE IF MINUSED                               
         BZ    SBUYXB5             NO                                           
         OC    RPAY,RPAY      IF MINUSED AND PAID SHOW AS UNPAID                
         BNZ   SBUYXB2                                                          
         B     SBUYXC         MINUSED AND UNPAID - USED TO SKIP                 
*                             STILL REPORT SINCE NETWORK MIGHT NOT              
*                             BE MINUSED                                        
*                                                                               
SBUYXB2  DS    0H                                                               
         ST    R2,SAVR2                                                         
         CLI   0(R2),X'0B'        ONLY LOOK AHEAD IF X'0B'                      
         BNE   SBUYXB3                                                          
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'0C'        MAKE GOOD                                     
         BNE   SBUYXB3                                                          
         TM    RSTATUS,X'80'                                                    
         BZ    SBUYXB3                                                          
         OC    RPAY,RPAY                                                        
         BNZ   SBUYXB4                                                          
*                                                                               
SBUYXB3  L     R2,SAVR2          NEXT ELEM IS UNPAID OR NOT THERE               
         B     SBUYXC            SO PROCESS                                     
SBUYXB4  L     R2,SAVR2                                                         
         B     SBUYXB             NEXT ELEM IS PAID SO BYPASS                   
*                                                                               
**NEW 6/7/88                                                                    
SBUYXB5  OC    RPAY,RPAY                                                        
         BNZ   SBUYXB              SKIP PAID                                    
*                                                                               
SBUYXC   LA    R7,1(R7)                                                         
         B     SBUYXB                                                           
*                                                                               
SBUYXD   DS   0H                                                                
         LTR   R7,R7                                                            
         BNZ   SBUYXF                                                           
         BRAS  RE,SBUYEND                                                       
         B     PEXIT                                                            
*                                                                               
SBUYXF   L     R2,ADBUY                                                         
         LA    R2,24(R2)                                                        
         MVI   ELCODE,X'F6'                                                     
         SR    R6,R6               COUNT NUMBER OF ELEMS TO BYPASS              
*                                                                               
SBUYXH   BAS   RE,NEXTEL                                                        
         BNE   SBUYXJ                                                           
         LA    R6,1(R6)                                                         
         B     SBUYXH                                                           
*                                                                               
SBUYXJ   LTR   R6,R6                                                            
         BNZ   SBUYXL              MUST BE DONE                                 
         BRAS  RE,SBUYEND                                                       
         B     PEXIT                                                            
*                                                                               
SBUYXL   XC    NDEMTOT1(16),NDEMTOT1        RESET NDEMTOTS                      
         XC    NUNPDGR(20),NUNPDGR          CLEAR UNPAID ACCUMS                 
         LA    R6,1(R6)                                                         
         ST    R6,BYPASS                                                        
         MVC   BUYDLN+93(11),=C'(CONTINUED)'                                    
         MVI   FORCEHED,C'Y'                                                    
         MVC   MID1+45(87),SPACES                                               
         MVC   MID2+45(87),SPACES                                               
         L     R3,ADBUY            RESET R3 TO NETWRK BUY                       
         B     PROC5               GO DO REST OF ELEMENTS                       
*                                                                               
PEXIT    XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         J     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***** DISPLAYING SPILL DEMO VALUES *****                                        
*                                                                               
*ON ENTRY R2 POINTS TO SPILL DEMO ELEMENT                                       
*                                                                               
         SPACE 1                                                                
SPDEMO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,P                                                             
         A     R3,DEMDISP                                                       
*                                                                               
         LA    R4,4                LOOP COUNTER                                 
*                                                                               
         LLC   R7,1(R2)                                                         
         AHI   R7,-24                                                           
         BNP   SPDX                                                             
         SRL   R7,3                NUMBER OF DEMO VALUES IN ELEMENT             
*                                                                               
         L     RF,PRDBUFF                                                       
         SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         LA    RF,0(RE,RF)                                                      
         MVC   MBDEMTAB(80),28(RF)                                              
*                                                                               
         LR    R6,R7               BACKUP THE COUNT OF DEMO VALS                
         LR    R8,R2               BACKUP THE DEMO POINTER                      
*                                                                               
         LA    R5,MBDEMTAB                                                      
         LA    R2,24(R2)           POINT TO THE FIRST ONE                       
*                                                                               
SPD05    CLC   0(3,R2),0(R5)                                                    
         BE    SPD10                                                            
         LA    R2,8(R2)                                                         
         BCT   R7,SPD05            UNTIL NO MORE DEMO VALS IN ELEMENT           
         B     SPD11                                                            
*                                                                               
SPD10    L     R0,4(R2)                                                         
         N     R0,=X'3FFFFFFF'                                                  
*                                                                               
         MVI   BYTE,1              1 DECIMAL DEMO                               
         TM    RQOPTS,RQOPTS_2DEC  REPORT SUPPORTS 2 DECIMAL DEMOS?             
         BZ    SPD10AA             NO                                           
         CLI   1(R2),C'R'          RATING?                                      
         BE    *+12                YES                                          
         CLI   1(R2),C'E'          EXTENDED RATING?                             
         BNE   SPD10AA             NO                                           
         MVI   BYTE,2              1 DECIMAL DEMO                               
*                                                                               
SPD10AA  CLI   PROGPROF+5,C'Y'                                                  
         BNE   SPD10A                                                           
         TM    4(R2),X'80'                                                      
         BZ    SPD10A                                                           
         CLI   BYTE,2              REPORT IN 2-DECIMAL?                         
         BE    SPD10AB             YES                                          
         EDIT  (R0),(7,(R3)),1,FLOAT=*                                          
         B     SPD11                                                            
SPD10AB  EDIT  (R0),(7,(R3)),2,FLOAT=*                                          
         B     SPD11                                                            
SPD10A   DS    0H                                                               
         CLI   BYTE,2              REPORT IN 2-DECIMAL?                         
         BE    SPD10AC             YES                                          
         EDIT  (R0),(7,(R3)),1                                                  
         B     SPD11                                                            
SPD10AC  EDIT  (R0),(7,(R3)),2                                                  
*                                                                               
SPD11    LA    R3,8(R3)                                                         
         LA    R2,24(R8)                                                        
         LR    R7,R6                                                            
         LA    R5,3(R5)            POINT TO NEXT ENTRY IN THE TABLE             
         BCT   R4,SPD05                                                         
*                                                                               
SPDX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
DEMODIS  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,1              1 DECIMAL DEMO                               
         TM    RQOPTS,RQOPTS_2DEC  REPORT SUPPORTS 2 DECIMAL DEMOS?             
         BZ    DEMD00              NO                                           
         CLI   0(R1),C'R'          RATING?                                      
         BE    *+12                YES                                          
         CLI   0(R1),C'E'          EXTENDED RATING?                             
         BNE   DEMD00              NO                                           
         MVI   BYTE,2              1 DECIMAL DEMO                               
*                                                                               
DEMD00   CLI   PROGPROF+5,C'Y'                                                  
         BNE   DEM8A                                                            
         LHI   R1,4                                                             
         SR    R1,R6                                                            
         LA    R1,MYFULL(R1)                                                    
         TM    0(R1),X'80'                                                      
         BZ    DEM8A                                                            
         CLI   BYTE,2              REPORT IN 2-DECIMAL?                         
         BE    DEMD05              YES                                          
         EDIT  (B4,0(R7)),(6,1(R3)),1,ZERO=BLANK,FLOAT=*                        
         B     DEM8B                                                            
DEMD05   EDIT  (B4,0(R7)),(6,1(R3)),2,ZERO=BLANK,FLOAT=*                        
         B     DEM8B                                                            
*                                                                               
DEM8A    CLI   BYTE,2              REPORT IN 2-DECIMAL?                         
         BE    DEMD10              YES                                          
         EDIT  (B4,0(R7)),(6,1(R3)),1,ZERO=BLANK                                
         B     DEM8B                                                            
DEMD10   EDIT  (B4,0(R7)),(6,1(R3)),2,ZERO=BLANK                                
*                                                                               
DEM8B    XIT1                                                                   
*                                                                               
* USES MYFULL                                                                   
DEMOFIND NTR1  BASE=*,LABEL=*                                                   
         XC    MYFULL,MYFULL                                                    
         L     R3,ADEST                                                         
         USING ESTHDR,R3                                                        
         LA    R3,EDEMLST-ESTHDR(R3)                                            
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,DNEXTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        LLC   RF,1(R4)                                                         
         LLC   RF,1(R6)                                                         
         LR    R1,R6                                                            
         AR    R1,RF               R1 POINTS TO NEXT ELEMENT                    
*                                                                               
         LA    R6,24(R6)                                                        
*                                                                               
         LA    R2,MYFULL                                                        
         LHI   R0,4              DEMO COUNTER                                   
DEMOF20  DS    0H                                                               
         LR    R4,R6               POINT R4 TO DEMOS IN '02'                    
DEMOF40  DS    0H                                                               
         CR    R4,R1                                                            
         BNL   DEMOF60                                                          
         CLC   0(3,R3),=3X'00'                                                  
         BE    DEMOF60                                                          
         CLC   1(2,R4),1(R3)       DEMO IN EST = DEMO IN BUY?                   
         BNE   *+14                UNEQUAL - DON'T SAVE THE BIT                 
         MVC   0(1,R2),4(R4)       SAVE FIRST BIT                               
         B     DEMOF60                                                          
         LA    R4,8(R4)            ADVANCE TO NEXT DEMO IN BUY                  
         B     DEMOF40                                                          
DEMOF60  DS    0H                                                               
         LA    R3,3(R3)            ADVANCE TO NEXT BUY IN EST                   
         LA    R2,1(R2)            ADVANCE TO NEXT LINE IN DEMTAB               
         BCT   R0,DEMOF20                                                       
         XIT1                                                                   
*                                                                               
*                                                                               
DNEXTEL  DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),0                                                          
         BE    DNEXTEL2                                                         
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     DNEXTEL                                                          
*                                                                               
DNEXTEL2 DS    0H                                                               
         LTR   R6,R6                                                            
         BR    RE                                                               
                                                                                
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
SBUYEND  NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT4,C'U'          UNPAID REPORT                                
         BNE   SBUYE8                DONE                                       
         GOTO1 APRINTIT,DMCB,(RA)  SKIP                                         
         LA    R3,P                                                             
         A     R3,DEMDISP                                                       
         S     R3,=F'7'                                                         
         MVC   0(6,R3),=C'TOTALS'                                               
         EDIT  NUNPDGR,(10,7(R3)),2,FLOAT=-                                     
         EDIT  NUNPDNET,(10,17(R3)),2,FLOAT=-                                   
         EDIT  NUNPDTAX,(7,27(R3)),2,FLOAT=-,ZERO=BLANK                         
         CLI   PROGPROF+4,C'Y'      SEE IF DOING GST COLUMN                     
         BNE   SBUYE1                                                           
         EDIT  NUNPDGST,(8,34(R3)),2,FLOAT=-,ZERO=BLANK                         
         EDIT  NUNPDPST,(8,42(R3)),2,FLOAT=-,ZERO=BLANK                         
*                                                                               
SBUYE1   GOTO1 APRINTIT,DMCB,(RA)                                               
*                                  ROLL TO REPORT TOTALS                        
         CLC   PROGPROF(4),=C'YYYY'    SEE IF BOTH GROSS AND NET                
         BE    SBUYE1X                 INCLUDED TAX AND GST/PST                 
*                                                                               
         LA    R3,P                                                             
         A     R3,DEMDISP                                                       
         S     R3,=F'26'                                                        
         MVC   0(25,R3),=C'INCLUDING TAX AND GST/PST'                           
         L     R0,NUNPDGR                                                       
         CLI   PROGPROF+0,C'Y'                                                  
         BE    *+8                                                              
         A     R0,NUNPDTAX                                                      
         CLI   PROGPROF+1,C'Y'       GST AND PST                                
         BE    *+12                                                             
         A     R0,NUNPDGST                                                      
         A     R0,NUNPDPST                                                      
         ST    R0,MYFULL                                                        
*                                                                               
         EDIT  MYFULL,(10,26(R3)),2,FLOAT=-                                     
*                                                                               
         L     R0,NUNPDNET                                                      
         CLI   PROGPROF+2,C'Y'                                                  
         BE    *+8                                                              
         A     R0,NUNPDTAX                                                      
         CLI   PROGPROF+3,C'Y'                                                  
         BE    *+12                                                             
         A     R0,NUNPDGST                                                      
         A     R0,NUNPDPST                                                      
         ST    R0,MYFULL                                                        
         EDIT  MYFULL,(10,36(R3)),2,FLOAT=-                                     
*                                                                               
         GOTO1 APRINTIT,DMCB,(RA)                                               
*                                                                               
SBUYE1X  DS    0H             ROLL TO REPORT TOTALS                             
         LA    R3,5                                                             
         LA    R4,RUNPDGR                                                       
         LA    R5,NUNPDGR                                                       
*                                                                               
SBUYE2   L     R6,0(R4)                                                         
         A     R6,0(R5)                                                         
         ST    R6,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R3,SBUYE2                                                        
         B     SBUYEX                                                           
*                                                                               
SBUYE8   OC    NDEMTOT1(16),NDEMTOT1        SEE IF I HAVE DEMOS                 
         BZ    SBUYEX                                                           
         GOTO1 APRINTIT,DMCB,(RA)  SKIP                                         
         LA    R3,P                                                             
         A     R3,DEMDISP                                                       
         S     R3,=F'7'                                                         
         MVC   0(6,R3),=C'TOTALS'                                               
         LA    R2,NDEMTOT1                                                      
         LA    R5,DNAME1                                                        
         LA    R3,7(R3)                                                         
         LA    R4,4                FOR BCT                                      
*                                                                               
SBUYE9   CLI   0(R5),C'R'          SEE IF RATING                                
         BE    SBUYE10                                                          
         CLI   0(R5),C'E'          OR EXTENDED DEMO                             
         BE    SBUYE10                                                          
         OC    0(4,R2),0(R2)                                                    
         BZ    SBUYE10                                                          
         EDIT  (B4,0(R2)),(7,0(R3)),1                                           
*                                                                               
SBUYE10  LA    R5,7(R5)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,SBUYE9                                                        
         GOTO1 APRINTIT,DMCB,(RA)                                               
*                                                                               
SBUYEX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET SPREPD202  AT LEVEL 086 AS OF 08/14/09                      
GETCAP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING PKGELEM,R5                                                       
         MVC   WORK2,SPACES                                                     
*                                                                               
GETCAP1  CLI   PKGCODE,5                                                        
         BE    GETCAP2                                                          
         CLI   PKGCODE,0                                                        
         BE    GETCAP4                                                          
         SR    RE,RE                                                            
         IC    RE,PKGLEN                                                        
         AR    R5,RE                                                            
         B     GETCAP1                                                          
*                                                                               
GETCAP2  DS    0H                                                               
         MVC   BYTE,PKGIND                                                      
         NI    BYTE,X'0F'          DROP2-BYTE LINE FLAG                         
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
         MVC   WORK2(7),=C'PKG MST'                                             
         B     GETCAPX                                                          
*                                                                               
         CLI   BYTE,3                                                           
         BNE   *+14                                                             
         MVC   WORK2(7),=C'ORB MST'                                             
         B     GETCAPX                                                          
*                                                                               
         CLI   BYTE,5                                                           
         BNE   *+14                                                             
         MVC   WORK2(7),=C'REV MST'                                             
         B     GETCAPX                                                          
*                                                                               
         MVC   WORK2(4),=C'MST='                                                
         LLC   R0,PKGLINES                                                      
         TM    PKGIND,X'10'        TEST 2-BYTE LINE NUMBERS                     
         BZ    *+8                                                              
         ICM   R0,3,PKGLINES                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK2+4(3),DUB                                                   
         B     GETCAPX                                                          
*                                                                               
GETCAP4  L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         CLI   BDMGDATE,X'C0'                                                   
         BNH   GETCAPX                                                          
         MVC   WORK2(7),=C'**MKGD '                                             
         MVC   WORK2+7(6),=C'GROUP '                                            
         MVC   WORK2+13(2),BDMGDATE                                             
         MVI   WORK2+15,C'*'                                                    
*                                                                               
GETCAPX  XIT1                                                                   
         LTORG                                                                  
         DROP  R5,RE                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
*                                                                               
       ++INCLUDE SPDNWRK                                                        
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051SPREPDN02 05/20/15'                                      
         END                                                                    
