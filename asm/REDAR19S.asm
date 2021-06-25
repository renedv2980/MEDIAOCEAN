*          DATA SET REDAR19S   AT LEVEL 178 AS OF 08/26/02                      
*PHASE T80F19C                                                                  
         TITLE 'T80F19 - REDAR19 - DARE MAKEGOOD OFFERS LIST'                   
***********************************************************************         
*                                                                     *         
*  REDAR19 (T80F19) --- DARE MAKEGOOD OFFERS LIST                     *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 04JUN02 (SKU) FIX SEND LOGIC                                        *         
* 26MAR02 (SKU) REPLACEMENT OFFER SUPPORT                             *         
* 02JAN02 (SKU) REMOVE 5-MINUTE RULE                                  *         
* 30NOV01 (SKU) FIX TAKEOVER BUG                                      *         
* 18OCT01 (SKU) RESEND BUG FIX                                        *         
*               SUPPORT DELIVERY NOTICE DISPLAY IN HISTORY            *         
* 23AUG01 (SKU) FIX ERROR MESSAGE NUMBER DISPLAY IN HISTORY           *         
* 16AUG01 (SKU) FIX TAKEOVER BUG                                      *         
* 01FEB01 (SKU) ON RESEND, DON'T SEND MKGCAN IF LAST STATUS WAS       *         
*               CANCEL WITH MORE (RECALLED) OR REJECTED               *         
* 17JAN01 (HWO) IGNORE AGENCY OFFICE WHEN BULDING X'51' DARE RECORD   *         
* 20SEP00 (SKU) CHECK IF ROUTING CODES PRESENT FOR APPLY              *         
* 29AUG00 (SKU) FIX TARGET BUY NUMBER DISPLAY                         *         
* 24AUG00 (SKU) TEMP REDIRECT H7 TO MS SENDER ID                      *         
* 28JUN00 (BU ) REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG         *         
* 15MAR99 (SKU) MULTI-MAKEGOOD SUPPORT                                *         
* 07FEB00 (SKU) TEMP CODE TO FORCE OMNY ORDER TO CORRECT ADV          *         
* 07FEB00 (SKU) DO NOT BUMP VERSION ON ERROR RESENDS                  *         
* 02JAN99 (SKU) ZERO PAD ORDER NUMBER IN DISPLAY                      *         
* 12OCT99 (SKU) MORE Y2K BUG FIXES                                    *         
* 30NOV98 (SKU) Y2K COMPLIANT                                         *         
* 25NOV98 (SKU) ALLOW CANCEL OF REJECTED OFFERS                       *         
* 27AUG98 (SKU) DISPLAY DASH C'-' FOR DATE RANGE                      *         
* 23DEC97 (SKU) CANNOT SEND MULTI-OFFER GROUPS                        *         
* 18AUG97 (SKU) KATZ NATIONAL/AMERICA MERGE SPECIAL CODING            *         
* 14AUG97 (SKU) ERROR SUPPORT                                         *         
* 30JUL97 (SKU) ANOTHER APPLY BUG FIX                                 *         
* 29JUL97 (SKU) BUG FIX FOR TAKEOVER MAKEGOOD                         *         
* 01JUL97 (SKU) SUPPORT DETAIL COMMENT                                *         
* 26FEB97 (SKU) APPLY BUG FIX. DARE TAKEOVER                          *         
* 20JAN97 (SKU) MAKEGOOD FOR MAKEGOOD                                 *         
* 09DEC96 (SKU) SUPPORT OWNERSHIP CONCEPT BETWEEN REP AND STATION     *         
* 23OCT96 (SKU) SUPPORT BONUSES AND PREEMPTS                          *         
* 08MAR95 (SKU) INITIAL RELEASE                                       *         
*                                                                     *         
***********************************************************************         
T80F19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80F19*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
                                                                                
         MVI   MYSCRNUM,X'F3'                                                   
         MVC   MGBLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
                                                                                
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
                                                                                
*        CLI   MODE,VALKEY         VALIDATE KEY?                                
*        BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
                                                                                
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
*----------------------------------------------------------------------         
* PF2: SWAP TO CONTRACT                                                         
*----------------------------------------------------------------------         
         TM    DISPFLG2,SWAP2CON   SWAP TO THE CONTRACT PROGRAM?                
         BZ    VR05                                                             
         GOTO1 =A(MGACTION),DMCB,(RC),('MGSWAPCN',0),RR=RELO                    
*                                                                               
* RESET SWAP-TO-CONTRACT REQUEST                                                
*                                                                               
         NI    DISPFLG2,X'FF'-SWAP2CON                                          
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
* PF4: COMING BACK FROM GLOBBER CALL TO CONTRACT. IF APPLY IN CONTRACT          
*      WAS SUCCESSFUL, UPDATE THE DARE SIDE AND SEND CONFIRMATION TO            
*      TO THE AGENCY                                                            
*----------------------------------------------------------------------         
VR05     DS    0H                  APPLY IN PROCESS?                            
         TM    DISPFLG2,PROCAPPL                                                
         BZ    VR08                                                             
*                                                                               
         NI    DISPFLG2,X'FF'-PROCAPPL                                          
*                                                                               
* APPLY DARE MAKEGOOD AND SEND CONFIRMATION TO AGENCY                           
*                                                                               
*        GOTO1 =A(MGACTION),DMCB,(RC),('MGCAPLYQ',0),RR=RELO                    
*                                                                               
         MVI   BLOCK,3             LENGTH OF MESSAGE WITH TERMATING 0           
         MVC   BLOCK+1(2),MGBGRP                                                
         MVI   BLOCK+3,0                                                        
         B     MGCNFOK                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* PF6: SEND/RESEND MAKEGOOD                                                     
*----------------------------------------------------------------------         
VR08     DS    0H                  SEND OR RESEND?                              
         TM    DISPFLAG,SENDMKG+RESNDMKG                                        
         BZ    VR09                MULTI-OFFER GROUP NOT ALLOWED                
         GOTO1 =A(SUBROUT),DMCB,(RC),('QCHKMUL',0),RR=RELO                      
         BNZ   MULTIERR                                                         
*                                                                               
VR09     DS    0H                                                               
         TM    DISPFLAG,SENDMKG                                                 
         BO    SEND                                                             
         TM    DISPFLAG,RESNDMKG   RESEND IS CAN W/MORE AND THEN SEND           
         BZ    VR10                                                             
         OI    DISPFLG2,CANWMORE                                                
         GOTO1 =A(MGACTION),DMCB,(RC),('MGCANCLQ',0),RR=RELO                    
         B     SEND                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
* PF4: APPLY MAKEGOOD. GLOBBER SWAP TO CONTRACT FIRST                           
*----------------------------------------------------------------------         
VR10     DS    0H                                                               
         TM    DISPFLAG,CFRMMKG    APPLY?                                       
         BZ    VR20                                                             
         NI    DISPFLAG,X'FF'-CFRMMKG                                           
*                                                                               
* CHECK IF OK TO APPLY                                                          
*                                                                               
         GOTO1 =A(MGACTION),DMCB,(RC),('MGOK2APL',0),RR=RELO                    
*                                                                               
* SET UP TO SWAP TO CONTRACT AND APPLY                                          
*                                                                               
         GOTO1 =A(MGACTION),DMCB,(RC),('MGSWAPCN',0),RR=RELO                    
*                                                                               
         OI    DISPFLG2,PROCAPPL   SET APPLY PROCESS FLAG, SO                   
*                                  WE CAN SEND THE DARE CONF LATER              
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
* PF7: DISPLAY HISTORY                                                          
*----------------------------------------------------------------------         
VR20     DS    0H                                                               
         TM    DISPFLAG,HISTMKG    HISTORY?                                     
         BZ    VR30                                                             
         GOTO1 =A(MGACTION),DMCB,(RC),('MGHISTYQ',0),RR=RELO                    
         B     MGHISTRY                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
* PF8/9: CANCEL/RECALL                                                          
*----------------------------------------------------------------------         
VR30     DS    0H                                                               
         TM    DISPFLAG,CANCMKG    CANCEL?                                      
         BO    VR35                                                             
         TM    DISPFLG2,CANWMORE   CANCEL WITH MORE TO FOLLOW?                  
         BZ    VR40                                                             
                                                                                
VR35     DS    0H                                                               
         GOTO1 =A(MGACTION),DMCB,(RC),('MGCANCLQ',0),RR=RELO                    
         EJECT                                                                  
*----------------------------------------------------------------------         
* REDISPLAY SCREEN                                                              
*----------------------------------------------------------------------         
VR40     DS    0H                                                               
         TWAXC MGBLISTH,MGBENDLH,PROT=Y                                         
         LA    R2,MGBLISTH                                                      
         MVI   MGLSTNUM,0                                                       
                                                                                
         TM    DISPFLAG,NEXTPG                                                  
         BZ    VR50                                                             
         MVC   KEY(L'MGKEY),MGKEY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     VR150                                                            
                                                                                
VR50     DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
*                                                                               
* FOR DDS TERMINALS, DISPLAY D/A OF MG GRP CMT                                  
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   VR55                                                             
         MVC   CONHED2+65(4),=C'D/A='                                           
         GOTO1 HEXOUT,DMCB,KEY+28,CONHED2+69,4,=C'TOG'                          
*                                  DISPLAY PFKEY LINE ON BOT OF SCRN            
VR55     DS    0H                                                               
         BAS   RE,GETKINFO         GET CONTRACT INFO                            
                                                                                
         XC    MGBGCMT,MGBGCMT                                                  
         XC    MGBGCM2,MGBGCM2                                                  
         XC    MGBGCM3,MGBGCM3                                                  
*        XC    MGBMCMT,MGBMCMT                                                  
                                                                                
* CONTRACT NUMBER                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CCONKNUM                                               
         EDIT  (P5,MYWORK),(8,MGBHDLN),ALIGN=LEFT,ZERO=NOBLANK                  
                                                                                
* ORDER NUMBER                                                                  
         OC    CDARNUM,CDARNUM                                                  
         BZ    VR60                                                             
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CDARNUM                                                
         EDIT  (P5,MYWORK),(8,MGBAORD),ALIGN=LEFT,FILL=0                        
                                                                                
* GROUP CODE                                                                    
VR60     DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         MVC   MGBGRP,RMKGKGRP                                                  
*                                                                               
         GOTO1 =A(MGACTION),DMCB,(RC),('MGDISPFQ',0),RR=RELO                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         USING RMKGGCEM,R6                                                      
         BRAS  RE,GETEL                                                         
         BNE   VR80                                                             
*                                                                               
* DISPLAY OVERALL GROUP COMMENTS, IF ANY                                        
*                                                                               
         LA    R4,MGBGCMTH                                                      
VR70     CLI   RMKGGCLN,RMKGGCCM-RMKGGCEM                                       
         BNH   VR75                                                             
         ZIC   R1,RMKGGCLN                                                      
         AHI   R1,-3               OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),RMKGGCCM                                                 
VR75     BRAS  RE,NEXTEL                                                        
         BNE   VR80                                                             
         ZIC   R1,0(R4)                                                         
         AR    R4,R1                                                            
         B     VR70                                                             
         DROP  R6                                                               
                                                                                
VR80     DS    0H                  DISPLAY AGENCY REJECTION COMMENT             
         L     R6,AIO              FOR REJECTED ORDERS ONLY                     
         USING RMKGREC,R6                                                       
         TM    RMKGSFG1,RMGF1MRR                                                
         BZ    VR90                                                             
         GOTO1 REJCMT,DMCB,(R6)                                                 
         DROP  R6                                                               
*                                                                               
VR90     DS    0H                  DISPLAY MISSED LINE COMMENT, IF ANY          
         L     R6,AIO                                                           
         MVC   KEY,0(R6)                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    VR100                                                            
         XC    DISPFLAG,DISPFLAG   ALL DONE                                     
         XC    DISPFLG2,DISPFLG2   ALL DONE                                     
         OI    MGBHDLNH+6,X'40'    FORCE CURSOR TO CONTRACT FIELD               
         B     HITBOTTM                                                         
                                                                                
VR100    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*&&DO                                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VR120                                                            
                                                                                
VR110    DS    0H                                                               
         USING RMKGCDEL,R6                                                      
         CLI   RMKGCDLN,RMKGCDDS-RMKGCDEL                                       
         BNH   VR120                                                            
                                                                                
         ZIC   R1,RMKGCDLN                                                      
         SH    R1,=H'11'           OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MGBMCMT(0),RMKGCDDS                                              
         DROP  R6                                                               
*&&                                                                             
                                                                                
VR120    DS    0H                                                               
         XC    DISPFLAG,DISPFLAG                                                
         XC    DISPFLG2,DISPFLG2                                                
         OI    DISPFLAG,FIRSTPG                                                 
                                                                                
VR130    DS    0H                                                               
         BAS   RE,MGFORMAT                                                      
         BAS   RE,MGDETCMT                                                      
         BNZ   VR140                                                            
         BAS   RE,BUMPNEXT                                                      
                                                                                
VR140    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
VR150    DS    0H                  THIS GROUP FINISHED?                         
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   VR160                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     VR130                                                            
                                                                                
VR160    DS    0H                                                               
         XC    DISPFLAG,DISPFLAG   ALL DONE                                     
         XC    DISPFLG2,DISPFLG2   ALL DONE                                     
         OI    MGBHDLNH+6,X'40'    FORCE CURSOR TO CONTRACT FIELD               
         B     HITBOTTM                                                         
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT FIELD                                                            
***********************************************************************         
BUMPNEXT DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,MGLSTNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,MGLSTNUM                                                      
         CLI   MGLSTNUM,MGLSTMAX                                                
         BNL   BMPNXT10                                                         
         NI    1(R2),X'FF'-X'08'   SET TO NORMAL INTENSITY                      
*                                                                               
         SR    R1,R1               OK EXIT                                      
         LTR   R1,R1                                                            
         BR    RE                                                               
                                                                                
BMPNXT10 DS    0H                  ARE WE IN THE PROCESS OF DISPLAYING          
         TM    DISPFLG2,BMPNXTOK   OFFER DATES/MISSED DATES/DAY-TIME?           
         BZ    BMPNXT20            DON'T EXIT WITH MESSAGE IF WE ARE            
         NI    DISPFLG2,X'FF'-BMPNXTOK                                          
*                                                                               
         LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
         BR    RE                                                               
                                                                                
BMPNXT20 DS    0H                  EXCEEDED MAX DISPLAY LENGTH, EXIT!           
         NI    DISPFLAG,X'FF'-FIRSTPG                                           
         OI    DISPFLAG,NEXTPG     DISPLAY NEXT PAGE NEXT TIME AROUND           
         OI    MGBHDLNH+6,X'40'    FORCE CURSOR TO K FIELD                      
         MVC   MGKEY,KEY                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 CHECK IN CASE LAST RECORD HITS LAST          
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE   LINE, WE SHOULD SAY              
         BE    NEXTPAGE            WE HIT BOTTOM                                
         B     HITBOTTM                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY AGENCY REJECTION COMMENT, IF ANY                                      
***********************************************************************         
REJCMT   NTR1                                                                   
         L     R6,0(R1)                                                         
         MVI   ELCODE,X'05'                                                     
         USING RMKGRCEM,R6                                                      
         BRAS  RE,GETEL                                                         
         BNE   REJCMTX                                                          
*                                                                               
         MVC   8(25,R2),=C'Agency Rejection Comment:'                           
         OI    1(R2),X'08'         SET HIGH INTENSITY                           
         BAS   RE,BUMPNEXT                                                      
*                                                                               
REJCMT10 DS    0H                                                               
         ZIC   R4,RMKGRCLN                                                      
         SH    R4,=H'3'                                                         
         CH    R4,=H'77'           MAX DISPLAY LENGTH                           
         BNH   *+8                                                              
         LA    R4,77                                                            
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RMKGRCCM                                                 
         BAS   RE,BUMPNEXT                                                      
         BRAS  RE,NEXTEL           PRINT ALL REJECTION COMMENT LINES            
         BE    REJCMT10                                                         
*                                                                               
REJCMTX  DS    0H                                                               
         XIT1  REGS=(R2)                                                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT MAKEGOOD OFFER                                                         
***********************************************************************         
MGFORMAT NTR1                                                                   
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
*                                                                               
         ST    R2,SCRNPTR          SAVE OFF CURRENT DISPLAY LINE                
         MVC   SVLSTNUM,MGLSTNUM   SAVE OFF HOW MANY LINES LEFT                 
*                                                                               
         LA    R3,8(R2)                                                         
         USING LMGD,R3                                                          
*                                                                               
         ZIC   RF,RMKGKRTY         CHECK RECORD NUMBER                          
         SLL   RF,28               DROP ALL BUT LAST 4 BITS                     
         SRL   RF,28               RESTORE                                      
         CH    RF,=H'1'            FIRST LINE OF POSSIBLE SET? (0/1)            
         BNH   MGFMT05             YES - NO FLAGS TO SET                        
         MVC   LMGOFF#,=C'AND'     NO  - INSERT 'AND' INDICATOR                 
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   MGFMT10             NO  - LEAVE 'AND' SET                        
         MVC   LMGOFF#,=C'OR '     YES - SET 'OR' INDICATOR                     
         B     MGFMT10                                                          
*                                                                               
* OFFER NUMBER                                                                  
MGFMT05  DS    0H                                                               
         EDIT  RMKGKLIN,(3,LMGOFF#),ALIGN=LEFT                                  
         DROP  R6                                                               
*                                                                               
* MISSED LINE NUMBER                                                            
MGFMT10  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MGFMT11                                                          
         USING RMKGMGEL,R6                                                      
         EDIT  RMKGMGLI,(3,LMGLINE#),ALIGN=LEFT                                 
         DROP  R6                                                               
*                                                                               
* MISSED DATE(SPOT)                                                             
MGFMT11  DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'20'                                                    
         BZ    MGFMT12                                                          
         MVC   LMGMISS(5),=C'BONUS'                                             
         B     MGFMT15                                                          
*                                                                               
MGFMT12  DS    0H                                                               
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGFMT13  DS    0H                                                               
         USING RMKGMGEL,R6                                                      
*                                                                               
         CLI   RMKGMGSP,0          DON'T CLUTTER THE SCREEN                     
         BNE   MGFMT13A                                                         
         CLI   LMGOFF#,C'A'                                                     
         BE    MGFMT20                                                          
         CLI   LMGOFF#,C'O'                                                     
         BE    MGFMT20                                                          
*                                                                               
MGFMT13A DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),(5,LMGMISS)                             
         MVI   LMGMISS+8,C'('     INSERT PAREN                                  
         LA    R4,LMGMISS+9                                                     
         EDIT  RMKGMGSP,(3,(R4)),ALIGN=LEFT                                     
         AR    R4,R0               ADD SIGNIFICANT CHARS FROM EDIT              
         MVI   0(R4),C')'          INSERT OTHER PAREN                           
*                                  INSERT NUMBER SPOTS MISSED                   
         AHI   R4,1                                                             
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    MGFMT14                                                          
         MVI   0(R4),C'-'          SIGNIFY THAT THIS IS A DATE RANGE            
         AHI   R4,1                                                             
*        CHI   R0,1                SKIP MULTI CHECK, WON'T FIT ANYWAY           
*        BH    MGFMT14A                                                         
         DROP  R6                                                               
*                                                                               
MGFMT14  DS    0H                                                               
         BRAS  RE,NEXTEL           MORE MISSED DATES?                           
         BNE   MGFMT15                                                          
         MVI   0(R4),C'*'          INDICATE MORE MISSED DATES                   
*                                                                               
MGFMT14A DS    0H                                                               
         OI    DISPFLG2,BMPNXTOK   BUMPNEXT COME BACK IF MAX AT BOTTOM          
         BAS   RE,BUMPNEXT         BUMP TO NEXT DISPLAY LINE                    
         BNZ   MGFMT15                                                          
         LA    R3,8(R2)                                                         
         B     MGFMT13                                                          
*                                                                               
MGFMT15  DS    0H                                                               
         L     R2,SCRNPTR          RESTORE CURRENT DISPLAY LINE                 
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   MGFMT20             NO  - LEAVE 'AND' SET                        
         DROP  R6                                                               
                                                                                
MGFMT18  DS    0H                                                               
         MVI   ELCODE,X'20'        STATUS CONTROL ELEMENT                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      TAG WITH '*' IF SELECTED?                    
         BZ    MGFMT20                                                          
         MVI   LMGOFF#+2,C'*'                                                   
         DROP  R6                                                               
                                                                                
* OFFERED DATE(SPOT)                                                            
MGFMT20  DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'10'                                                    
         BZ    MGFMT23                                                          
         MVC   LMGOFDAT(7),=C'PREEMPT'                                          
         B     MGFMT30                                                          
*                                                                               
MGFMT23  DS    0H                                                               
         TM    RMKGRTS,X'02'       REPLACEMENT OFFER?                           
         BZ    MGFMT25                                                          
         MVC   LMGOFDAT(2),=C'NA'                                               
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    MGFMT25                                                          
         MVC   LMGOFDAT(9),=C'CREDIT NA'                                        
         B     MGFMT30                                                          
*                                                                               
*                                                                               
MGFMT25  DS    0H                                                               
         L     R6,AIO                                                           
         BAS   RE,OFFRDATE                                                      
                                                                                
* DISPLAY DAY/TIME, LENGTH AND COST                                             
         BAS   RE,MKGDETLS                                                      
                                                                                
         MVC   MGLSTNUM,SVLSTNUM   RESTORE NUMBER OF LINES LEFT                 
                                                                                
MGFMT30  DS    0H                  FIND NEXT AVAILABLE DISPLAY LINE             
         BAS   RE,BUMPNEXT                                                      
         LA    R3,8(R2)                                                         
         OC    LMGD(LMGLNQ),LMGD                                                
         BNZ   MGFMT30                                                          
                                                                                
MGFMTX   DS    0H                                                               
         XIT1  REGS=(R2)                                                        
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*  OFFRDATE:  RETRIEVE/DISPLAY OFFER DATE(S) ON SECOND LINE OF                  
*        DISPLAY.  INSERT # SPOTS/WEEK OFFERED.                                 
***********************************************************************         
OFFRDATE NTR1                                                                   
         L     R6,AIO                                                           
         USING RMKGDTEL,R6                                                      
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,8(R2)                                                         
         USING LMGD,R3                                                          
*                                                                               
OFDT0005 DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,RMKGDTST),(4,LMGOFDAT)                            
*                                  START DATE                                   
         CLC   RMKGDTST,RMKGDTED                                                
         BNE   *+12                                                             
         LA    R3,LMGOFDAT+5                                                    
         B     OFDT0010                                                         
*                                                                               
         MVI   LMGOFDAT+5,C'-'                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(3,RMKGDTED),(4,LMGOFDAT+6)                          
         LA    R3,LMGOFDAT+11                                                   
*                                  END DATE                                     
         DROP  R3                                                               
*                                                                               
OFDT0010 DS    0H                                                               
         TM    RMKGDTIN,X'40'      ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R3),C'A'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         OC    RMKGDTNW,RMKGDTNW                                                
         BNZ   OFDT0020                                                         
         MVC   0(3,R3),=C'(0)'                                                  
         LA    R3,3(R3)                                                         
         B     OFDT0030                                                         
                                                                                
OFDT0020 DS    0H                                                               
         MVI   0(R3),C'('                                                       
         EDIT  RMKGDTNW,(3,1(R3)),ALIGN=LEFT                                    
         AR    R3,R0                                                            
         MVI   1(R3),C')'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
OFDT0030 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   OFDTX                                                            
         MVI   0(R3),C'*'                                                       
         OI    DISPFLG2,BMPNXTOK   BUMPNEXT COME BACK IF MAX AT BOTTOM          
         BAS   RE,BUMPNEXT         BUMP TO NEXT DISPLAY LINE                    
         BNZ   OFDTX                                                            
         LA    R3,8(R2)                                                         
         B     OFDT0005                                                         
                                                                                
OFDTX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   MKGDETLS:  DISPLAY DAY/TIME/LENGTH/COST OF SPOTS OF MAKEGOOD                
*        OFFER.                                                                 
***********************************************************************         
MKGDETLS NTR1                                                                   
         LA    R3,8(R2)                                                         
         USING LMGD,R3                                                          
*                                  LENGTH                                       
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         MVC   HALF,RMKGDUR                                                     
                                                                                
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,LMGLEN),ALIGN=LEFT                                   
*                                                                               
         LA    R4,LMGLEN                                                        
         LR    RE,R0               ADD LENGTH OF EDITED FIELD                   
         AR    RE,R4                                                            
         TM    RMKGDUR,X'80'       MINUTES?                                     
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
         EDIT  RMKGCOS,(10,LMGCOST),2,ALIGN=LEFT                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         USING RMKGDYEL,R6                                                      
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  DISPLAY DAY-TIME FIELDS                      
MKGD0010 DS    0H                                                               
         GOTO1 VOUTDAY,DMCB,RMKGDAYS,RMKGDYIN,LMGOFDT                           
*                                  TIME                                         
         LA    R3,LMGOFDT          CHECK FOR LAST POSITION                      
         LA    R4,L'LMGOFDT        MAX FIELD SIZE                               
MKGD0020 EQU   *                                                                
         CLI   0(R3),C' '          SPACE FOUND?                                 
         BE    MKGD0040            YES                                          
         CLI   0(R3),0             OR NULLS                                     
         BE    MKGD0040                                                         
         LA    R3,1(R3)            GO BACK FOR NEXT                             
         BCT   R4,MKGD0020                                                      
         DC    H'0'                NOT FOUND - ERROR                            
MKGD0040 EQU   *                                                                
         MVI   0(R3),C'/'          INSERT SEPARATOR                             
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
         GOTO1 UNTIME,DMCB,RMKGDYT1,0(R3)                                       
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   MKGDX                                                            
*                                                                               
MKGD0050 EQU   *                                                                
         CLI   0(R3),C' '          SPACE FOUND?                                 
         BE    MKGD0060            YES                                          
         LA    R3,1(R3)            GO BACK FOR NEXT                             
         BCT   R4,MKGD0050                                                      
MKGD0060 EQU   *                                                                
         MVI   0(R3),C'*'                                                       
         OI    DISPFLG2,BMPNXTOK   BUMPNEXT COME BACK IF MAX AT BOTTOM          
         BAS   RE,BUMPNEXT         BUMP TO NEXT DISPLAY LINE                    
         BNZ   MKGDX                                                            
         LA    R3,8(R2)                                                         
         B     MKGD0010                                                         
                                                                                
MKGDX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY MAKEGOOD DETAIL COMMENTS                                              
***********************************************************************         
MGDETCMT NTR1                                                                   
         LA    R2,8(R2)                                                         
         USING LMGD,R2                                                          
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   NO                                                               
                                                                                
         USING RMKGDCEL,R6                                                      
         CLI   RMKGDCLN,RMKGDCCM-RMKGDCEL                                       
         BNH   MGCMTX                                                           
         ZIC   R1,RMKGDCLN                                                      
         SH    R1,=H'3'            OVERHEAD                                     
         CH    R1,=H'69'           MAX LENGTH FIT TO SCREEN                     
         BL    MGCMT10                                                          
         LA    R1,69                                                            
                                                                                
MGCMT10  DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LMGMISS(0),RMKGDCCM                                              
                                                                                
MGCMTX   DS    0H                                                               
         B     YES                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SEND MAKEGOOD TO PQ                                                           
* EACH GROUP HAS EXACTLY ONE PQ ENTRY WHICH HAS THE FOLLOWING FORMAT:           
*                                                                               
*  MKGHDR                                                                       
*  MKGDSI                                                                       
*      - OFFER 1 -                                                              
*      MKGMSS, ...                                                              
*          MKGBUY                                                               
*          MKGORB, ...                                                          
*          MKGCOM, ...                                                          
*          MKGDTL, ...                                                          
*          - AND -                                                              
*          MKGBUY                                                               
*          MKGORB, ...                                                          
*          MKGCOM, ...                                                          
*          MKGDTL, ...                                                          
*          - AND -                                                              
*          MKGBUY                                                               
*          MKGORB, ...                                                          
*          MKGCOM, ...                                                          
*          MKGDTL, ...                                                          
*      - OFFER 2 -                                                              
*      ...                                                                      
*                                                                               
***********************************************************************         
SEND     DS    0H                                                               
         NI    DISPFLAG,X'FF'-SENDMKG                                           
*                                  CLEAR SEND FLAG                              
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
*                                                                               
         BAS   RE,CHECKMG                                                       
         BNZ   UNSELOFF            UNSELECTED CHOICE OFFER DETECTED             
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,GETKINFO         GET CONTRACT INFORMATION                     
         OC    CDARNUM,CDARNUM     ORDER NUMBER PRESENT?                        
         BZ    MISSORD             NO? WE NEED IT!                              
*                                                                               
* CHECK IF REVISION IN PROGRESS                                                 
*                                                                               
         GOTO1 =A(CHKREV),RR=RELO                                               
*                                  READ IN DARE X'51' REC TO IO3                
         GOTO1 =A(GETDAR51),DMCB,(RC),RR=RELO                                   
         BNZ   MISSORD                                                          
*                                                                               
*                                                                               
         L     R6,AIO                                                           
         MVC   KEY(L'RMKGKEY),0(R6)                                             
         GOTO1 HIGH                                                             
*                                  MAKEGOOD OFFERS FOR THIS CONTRACT            
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BZ    *+6                 HEADER RECORD MUST BE THERE!                 
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'01'        GET DESCRIPTION ELEMENT                      
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
*                                                                               
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    SEND03                                                           
         TM    RMKGSCST,RMKGSRJQ   REP REJECTED TO STATION                      
         BO    STASEND             STATION MUST SEND IT BACK                    
*                                                                               
SEND03   DS    0H                                                               
         TM    RMKGSFG1,RMGF1MER   LAST STATUS WAS ERROR,                       
         BO    SEND05              DON'T                                        
         ZIC   R1,RMKGSCVR         INCREMENT VERSION NUMBER                     
         LA    R1,1(R1)                                                         
         STC   R1,RMKGSCVR                                                      
*                                                                               
SEND05   DS    0H                                                               
         MVI   RMKGSFG1,RMGF1MSN   MAKEGOOD HAS BEEN SENT!                      
         TM    DISPFLAG,RESNDMKG                                                
         BZ    *+8                                                              
         MVI   RMKGSFG1,RMGF1MCR   MAKEGOOD HAS BEEN RESENT!                    
*                                                                               
         MVC   DMCB+8(1),RMKGSCVR                                               
         MVC   DMCB+9(1),RMKGSFG1                                               
                                                                                
         GOTO1 =A(MGACTION),DMCB,(RC),('MGUPHISQ',0),,RR=RELO                   
                                                                                
         GOTO1 PUTREC              FLAG AND WRITE TO REC                        
         DROP  R6                                                               
                                                                                
*                                  DISPLAY PFKEY LINE ON BOT OF SCRN            
         GOTO1 =A(MGACTION),DMCB,(RC),('MGDISPFQ',0),RR=RELO                    
                                                                                
SEND10   DS    0H                                                               
         BAS   RE,INITPQ           INITIALIZE THE PQ                            
         BAS   RE,EDICT            SEND EDICT INFORMATION                       
                                                                                
         XC    TOTMGSPT,TOTMGSPT   CLEAR TOTAL SPOTS ACCUMULATOR                
         XC    TOTMGDOL,TOTMGDOL   CLEAR TOTAL DOLLARS ACCUMULATOR              
         XC    RECCOUNT,RECCOUNT   TOTAL RECORD COUNT PER PQ ENTRY              
                                                                                
         BAS   RE,MKGHDR           MG OFFER IDENTIFICATION                      
         BAS   RE,MKGDS1           PROCESS MG OFFER COMMENT                     
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                  MAKEGOOD OFFERS FOR THIS CONTRACT            
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVI   MGOFFNUM,1          OFFER NUMBER DEFAULT                         
                                                                                
SEND20   DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'20'       SKIP FOR BONUS                               
         BO    SEND30                                                           
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
SEND25   DS    0H                                                               
         USING RMKGMGEL,R6                                                      
         MVC   MISSDATE,RMKGMGD1                                                
         MVC   MISSDAT2,RMKGMGD2                                                
         MVC   MISS#SPT,RMKGMGSP                                                
         MVC   MISSLINE,RMKGMGLI                                                
         DROP  R6                                                               
                                                                                
         BAS   RE,MKGMSS           MISSED SPOT BUYLINES/DATES                   
                                                                                
         MVI   ELCODE,X'05'        RESTORE ELCODE POINTER                       
         BRAS  RE,NEXTEL           CHECK FOR MULTIPLE MISSED DATES/SPTS         
         BE    SEND25                                                           
                                                                                
SEND30   DS    0H                  IF 'OR' OFFER, SEND ONLY SELECTED            
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGKRTY,X'F0'      ALL LINES?                                   
         BZ    SEND40                                                           
         MVI   ELCODE,X'20'        IF CHOICE, GET STATUS ELEMENT                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    SEND50                                                           
         DROP  R6                                                               
                                                                                
SEND40   DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'10'       PREEMPT: SKIP MKGBUY/MKGORB                  
         BZ    SEND43                                                           
         BAS   RE,MKGCOM           MG BUYLINE COMMENT                           
         B     SEND50                                                           
*                                                                               
SEND43   DS    0H                                                               
         TM    RMKGRTS,X'02'       REPLACEMENT OFFER?                           
         BZ    SEND45              WITH CREDIT NA?                              
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    SEND45              YES, SKIP MKGBUY/MKGORB                      
         BAS   RE,MKGCOM           MG BUYLINE COMMENT                           
         B     SEND50                                                           
         DROP  R6                                                               
*                                                                               
SEND45   DS    0H                                                               
         BAS   RE,MKGBUY           MG BUYLINE DESCRIPTION                       
         BAS   RE,MKGORB           MG ORBIT DESCRIPTION                         
         BAS   RE,MKGCOM           MG BUYLINE COMMENT                           
         BAS   RE,MKGDTL           MG BUYLINE DETAILS                           
                                                                                
SEND50   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                  SAME OFFER?                                  
         CLC   KEY(RMKGKRTY-RMKGKEY),KEYSAVE                                    
         BNE   SEND60                                                           
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGRTS,X'10'+X'08'+X'04'                                        
         BNZ   SEND20              PREEMPT OR LATERUN?                          
         DROP  R6                                                               
         B     SEND30                                                           
                                                                                
SEND60   DS    0H                                                               
         ZIC   RF,MGOFFNUM         BUMP OFFER NUMBER COUNTER                    
         LA    RF,1(RF)                                                         
         STC   RF,MGOFFNUM                                                      
*                                  SAME GROUP, DIFFERENT OFFER?                 
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    SEND20              YES, PROCESS NEW OFFER                       
                                                                                
SEND80   DS    0H                                                               
         BAS   RE,MKGTLR           SEND TRAILER AND CLOSE PQ                    
                                                                                
         OI    MGBHDLNH+6,X'40'    FORCE CURSOR HERE                            
         MVI   BLOCK,3             LENGTH OF MESSAGE WITH TERMATING 0           
         MVC   BLOCK+1(2),MGBGRP                                                
         MVI   BLOCK+3,0                                                        
         TM    DISPFLAG,RESNDMKG   IF RESEND, CLEAR FLAG AND NOTIFY             
         BZ    MGSENTOK                                                         
         NI    DISPFLAG,X'FF'-RESNDMKG                                          
         B     MGRSNTOK                                                         
                                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK IF ANY 'OR' MAKEGOODS OFFERS IN GROUP. IF FOUND, MAKE SURE              
* AT LEAST ONE RECORD WITHIN THE OFFER HAS BEEN SELECTED                        
***********************************************************************         
CHECKMG  NTR1                                                                   
*                                  SET TO GET SELECTED RECORD                   
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),0(R6)                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GET HEADER REC                               
         B     CHKMG20                                                          
                                                                                
CHKMG10  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
CHKMG20  DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   YES                 ALL DONE                                     
         OC    RMKGKPLN(6),RMKGKPLN SKIP COMMENT RECORDS                        
         BZ    CHKMG10                                                          
         TM    RMKGKRTY,X'10'      FIND 'CHOICE' RECORDS                        
         BZ    CHKMG10                                                          
         MVC   MGOFFNUM,RMKGKRTY   SAVE OFFER NUMBER                            
         MVI   MGORSELD,C'N'       DEFAULT NO RECORD SELECTED IN OFFER          
         DROP  R6                                                               
                                                                                
CHKMG30  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        GET STATUS ELEMENT                           
         BRAS  RE,GETEL                                                         
         BE    *+6                 MUST BE THERE                                
         DC    H'0'                                                             
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    CHKMG50                                                          
         MVI   MGORSELD,C'Y'       YES, AT LEAST ONE HAS BEEN SELECTED          
         DROP  R6                                                               
*                                                                               
CHKMG50  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(RMKGKRTY-RMKGKEY),KEYSAVE                                    
         BE    CHKMG60             ALL DONE                                     
         CLI   MGORSELD,C'N'       ANYTHING SELECTED FROM PREV OFFER?           
         BE    NO                                                               
         B     YES                                                              
                                                                                
CHKMG60  DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         MVC   WORK(1),RMKGKRTY                                                 
         NC    WORK(1),MGOFFNUM                                                 
         DROP  R6                                                               
                                                                                
         TM    WORK,X'0F'          CHECK IF SAME OFFER                          
         BO    CHKMG70             YES, SAME OFFER                              
         CLI   MGORSELD,C'Y'       IF AT LEAST ONE REC WAS SELECTED             
         BE    CHKMG50             DO SEQ UNTIL NEXT OFFER                      
         B     CHKMG30             ELSE CHECK IF REC SELECTED                   
                                                                                
CHKMG70  DS    0H                  DIFFERENT OFFER                              
         CLI   MGORSELD,C'N'       ANY RECORDS SELECTED FROM PREV OFF?          
         BE    NO                                                               
         B     CHKMG20             YES, RESET AND START AGAIN                   
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD OFFER IDENTIFICATION                                                 
***********************************************************************         
MKGHDR   NTR1                                                                   
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRHDRD,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOHDTID,=C'MKGHDR'                                               
                                                                                
* ORDER NUMBER                                                                  
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CDARNUM                                                
         EDIT  (P5,MYWORK),(8,MOHDORDR),FILL=0                                  
         MVC   MGORDNUM,MOHDORDR                                                
                                                                                
* DATA VERSION NUMBER                                                           
         MVI   MOHDVERS,C'0'                                                    
                                                                                
* ID OF SENDER                                                                  
         MVC   MOHDFRID,RDARRCVR                                                
                                                                                
* ID OF RECEIVER                                                                
         MVC   MOHDTOID,RDARSNDR                                                
*                                                                               
         CLC   =C'H7',RDARSNDR     TEMP MINDSHARE FIX                           
         BNE   MKGHDR05                                                         
         MVC   MOHDTOID(2),=C'MS'                                               
*                                                                               
         CLC   =C'MSDNS',MOHDTOID                                               
         BNE   MKGHDR05                                                         
         MVC   MOHDTOID(5),=C'MSDV '                                            
*                                                                               
MKGHDR05 DS    0H                                                               
*                                                                               
* ROUTING CODE                                                                  
         MVC   MOHDROUT,RDARKAGY                                                
                                                                                
* CURRENT DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(X'20',MOHDDATE)                               
                                                                                
* CURRENT TIME                                                                  
         GOTO1 GETTIME,DMCB,MOHDTIME                                            
                                                                                
* CONTRACT NUMBER                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CCONKNUM                                               
         EDIT  (P5,MYWORK),(8,MOHDRPCN),FILL=0                                  
                                                                                
* AGENCY 'RETURN TO SENDER' DATA                                                
         MVC   MOHDRTNS,RDARRTS                                                 
         DROP  R6                                                               
**********                                                                      
********** TEMPORARY TO FORCE OMNY ORDERS TO GO TO CORRECT ADV                  
**********                                                                      
*RTSD     USING RTN2SNDR,MOHDRTNS                                               
*        CLC   =C'OM',RTSD.RTNPWRCD                                             
*        BNE   MKGHDR10                                                         
*         MVC   RTSD.RTNSYSID,=C'05'                                            
*         MVC   RTSD.RTNAGYMD,=C'41'                                            
*         DROP  R6,RTSD                                                         
*MKGHDR10 DS    0H                                                              
**********                                                                      
**********                                                                      
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
                                                                                
* OFFER ID                                                                      
         MVC   MOHDOFRI(2),RMKGKGR1                                             
         MVC   MGGRPCDE,RMKGKGR1                                                
         DROP  R6                                                               
                                                                                
* VERSION NUMBER WITHIN ID                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
                                                                                
         EDIT  RMKGSCVR,(2,MOHDSEQN),FILL=0                                     
         MVC   MGVERNUM,MOHDSEQN   SAVE OFF FOR TRAILER                         
                                                                                
         BAS   RE,PRTNBUMP                                                      
                                                                                
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD OFFER COMMENT                                                        
***********************************************************************         
MKGDS1   NTR1                                                                   
         L     R6,AIO                                                           
*                                  SKIP IF NO COMMENTS                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MKGDS1X                                                          
*                                                                               
         USING RMKGGCEM,R6                                                      
         CLI   RMKGGCLN,RMKGGCCM-RMKGGCEM                                       
         BNH   MKGDS1X             SKIP IF NO COMMENTS                          
                                                                                
         LA    R4,P                                                             
         USING MOFRDS1D,R4                                                      
                                                                                
                                                                                
MKGDS110 DS    0H                                                               
* RECORD                                                                        
         MVC   MOD1TID,=C'MKGDS1'                                               
                                                                                
* ORDER NUMBER                                                                  
         MVC   MOD1ORDR,MGORDNUM                                                
                                                                                
* OFFER COMMENT(S)                                                              
         ZIC   R1,RMKGGCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOD1TEXT(0),RMKGGCCM                                             
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   MKGDS120                                                         
         MVI   MOD1CONT,C'*'       IF MORE, INSERT CONTINUATION CHAR            
         BAS   RE,PRTNBUMP                                                      
         B     MKGDS110                                                         
                                                                                
MKGDS120 DS    0H                  PRINT LAST RECORD                            
         BAS   RE,PRTNBUMP                                                      
                                                                                
MKGDS1X  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD MISSED SPOTS BUYLINES/DATES                                          
***********************************************************************         
MKGMSS   NTR1                                                                   
         MVC   SVKEY,KEY                                                        
                                                                                
         L     R6,AIO              GET MISSED BUY RECORD                        
         USING RMKGREC,R6                                                       
         LA    R4,KEY                                                           
         USING RBUYKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,RMKGKREP                                                
         MVC   RBUYKCON,RMKGKCON                                                
         MVC   RBUYKPLN,RMKGKPLN                                                
*        MVC   RBUYKMLN,RMKGKMLN                                                
*        MVC   RBUYKLIN,RMKGKMLN                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
MKGMS03  DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    *+6                 MUST BE THERE!                               
         DC    H'0'                                                             
*                                                                               
         CLC   RBUYKLIN,MISSLINE                                                
         BE    MKGMS05                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     MKGMS03                                                          
         DROP  R4,R6                                                            
*                                                                               
MKGMS05  DS    0H                                                               
         MVC   AIO,AIO2            READ BUY RECORD INTO IO2                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRMISD,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOMSTID,=C'MKGMSS'                                               
                                                                                
* ORDER NUMBER                                                                  
         MVC   MOMSORDR,MGORDNUM                                                
                                                                                
* MISSED AGENCY LINE NUMBER                                                     
         EDIT  RBUYAGBL,(3,MOMSAGYL),FILL=0                                     
         DROP  R6                                                               
                                                                                
* MISSED SPOT DATE                                                              
         GOTO1 DATCON,DMCB,(3,MISSDATE),(X'20',MOMSDATE)                        
                                                                                
* NUMBER OF MISSED SPOTS                                                        
         EDIT  MISS#SPT,(2,MOMSNSPT),FILL=0                                     
                                                                                
* ROTATION MTWTFSS                                                              
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RBUYDYEL,R6                                                      
         MVC   MOMSROTN,=C'MTWTFSS'                                             
         IC    R1,RBUYDAYS         BUY ROTATION                                 
         LA    RE,7                                                             
         LA    RF,MOMSROTN                                                      
                                                                                
MKGMS10  DS    0H                                                               
         SLL   R1,1                                                             
         STC   R1,BYTE                                                          
         TM    BYTE,X'80'                                                       
         BO    MKGMS20                                                          
         MVI   0(RF),C' '          BLANK DAY IF NOT IN ROTATION                 
                                                                                
MKGMS20  DS    0H                                                               
         LA    RF,1(RF)                                                         
         BCT   RE,MKGMS10                                                       
                                                                                
* ROTATION START DAY                                                            
         ZIC   R2,RBUYDYIN                                                      
         SRL   R2,4                                                             
         EDIT  (R2),(1,MOMSRSDT)                                                
                                                                                
* START TIME 0600-3000                                                          
         CLC   RBUYDYT1,=AL2(600)  LESS THAN 0600?                              
         BL    MKGMS30                                                          
         EDIT  RBUYDYT1,(4,MOMSSTIM),FILL=0                                     
         B     MKGMS40                                                          
                                                                                
MKGMS30  DS    0H                                                               
         ZICM  R3,RBUYDYT1,2                                                    
         LA    R3,2400(R3)         ADD 24 HOURS                                 
         EDIT  (R3),(4,MOMSSTIM),FILL=0                                         
                                                                                
* END TIME 0600-3000                                                            
MKGMS40  DS    0H                                                               
         MVC   MOMSETIM,MOMSSTIM   DEFAULT SAME END TIME                        
         OC    RBUYDYT2,RBUYDYT2   ANY END TIME?                                
         BZ    MKGMS60                                                          
         CLC   =C'CC',RBUYDYT2     PROGRAM CONCLUSION??                         
         BNE   MKGMS45                                                          
         MVC   MOMSETIM,=C'CC  '                                                
         B     MKGMS60                                                          
MKGMS45  DS    0H                                                               
         CLC   RBUYDYT2,=AL2(600)  LESS THAN 0600?                              
         BL    MKGMS50                                                          
         EDIT  RBUYDYT2,(4,MOMSETIM),FILL=0                                     
         B     MKGMS60                                                          
                                                                                
MKGMS50  DS    0H                                                               
         ZICM  R3,RBUYDYT2,2                                                    
         LA    R3,2400(R3)         ADD 24 HOURS                                 
         EDIT  (R3),(4,MOMSETIM),FILL=0                                         
         DROP  R6                                                               
                                                                                
MKGMS60  DS    0H                                                               
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
                                                                                
* LENGTH UNITS                                                                  
         MVC   HALF,RBUYDUR                                                     
         MVI   MOMSLUNT,C'S'                                                    
         TM    HALF,X'80'                                                       
         BZ    MKGMS70                                                          
         XI    HALF,X'80'          CLEAR MINUTE BIT                             
         MVI   MOMSLUNT,C'M'                                                    
                                                                                
* SPOT LENGTH (NOTE: NOT TOTAL SPOT LENGTH!)                                    
MKGMS70  DS    0H                                                               
         EDIT  HALF,(3,MOMSTSLN),FILL=0                                         
                                                                                
* COST                                                                          
         EDIT  RBUYCOS,(9,MOMSCOST),FILL=0                                      
         DROP  R6                                                               
                                                                                
* PROGRAM NAME                                                                  
         MVI   ELCODE,X'04'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MKGMS100                                                         
         USING RBUYCMEL,R6                                                      
                                                                                
MKGMS80  DS    0H                                                               
         CLC   =C'P=',RBUYCMNT                                                  
         BNE   MKGMS95                                                          
         CLI   RBUYCMLN,4          GET NEXT IF PROGRAM NAME IS BLANK            
         BNH   MKGMS95                                                          
         ZIC   R1,RBUYCMLN                                                      
         SH    R1,=H'4'            SUBTRACT OVERHEAD                            
         CLI   RBUYCMLN,4+L'MOMSPGNM                                            
         BNH   MKGMS90             MOVE MAX LENGTH OF OUTPUT SIZE               
         LA    R1,L'MOMSPGNM                                                    
                                                                                
MKGMS90  DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOMSPGNM(0),RBUYCMNT+2                                           
         B     MKGMS100                                                         
                                                                                
MKGMS95  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    MKGMS80                                                          
*                                                                               
* INCASE OF CREDIT RANGE, PRINT MULTIPLE MKGMSS'S                               
*                                                                               
MKGMS100 DS    0H                                                               
         OC    MISSDAT2,MISSDAT2                                                
         BZ    MKGMSX                                                           
*                                                                               
         MVC   ELEM(L'P),P                                                      
         GOTO1 ADDAY,DMCB,MOMSDATE,WORK,7                                       
*                                                                               
         BAS   RE,PRTNBUMP                                                      
*                                                                               
MKGMS110 DS    0H                                                               
         MVC   P,ELEM                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         CLC   WORK+6(3),MISSDAT2                                               
         BH    MKGMSXX                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',MOMSDATE)                            
         BAS   RE,PRTNBUMP                                                      
         GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
         B     MKGMS110                                                         
*                                                                               
MKGMSX   DS    0H                                                               
         BAS   RE,PRTNBUMP                                                      
*                                                                               
MKGMSXX  DS    0H                                                               
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
                                                                                
         XC    P,P                 CLEAN UP, JUST IN CASE                       
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD BUY DESCRIPTION                                                      
***********************************************************************         
MKGBUY   NTR1                                                                   
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
                                                                                
         L     RF,TOTMGSPT         TOTAL MAKEGOOD SPOTS ACCUMULATOR             
         ZICM  RE,RMKGTSPT,2                                                    
         AR    RF,RE                                                            
         ST    RF,TOTMGSPT                                                      
                                                                                
         L     RF,TOTMGDOL         TOTAL MAKEGOOD DOLLARS ACCUMULATOR           
         ZICM  RE,RMKGTCOS,4                                                    
         AR    RF,RE                                                            
         ST    RF,TOTMGDOL                                                      
                                                                                
         LA    R4,P                                                             
         USING MOFRBUYD,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOBYTID,=C'MKGBUY'                                               
                                                                                
* ORDER NUMBER                                                                  
         MVC   MOBYORDR,MGORDNUM                                                
                                                                                
* OFFER NUMBER                                                                  
         EDIT  MGOFFNUM,(2,MOBYSEQN),FILL=0                                     
                                                                                
* ROTATION MTWTFSS                                                              
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RMKGDYEL,R6                                                      
         MVC   MOBYROTN,=C'MTWTFSS'                                             
         IC    R1,RMKGDAYS         BUY ROTATION                                 
         LA    RE,7                                                             
         LA    RF,MOBYROTN                                                      
                                                                                
MKGBUY10 DS    0H                                                               
         SLL   R1,1                                                             
         STC   R1,BYTE                                                          
         TM    BYTE,X'80'                                                       
         BO    MKGBUY20                                                         
         MVI   0(RF),C' '          BLANK DAY IF NOT IN ROTATION                 
                                                                                
MKGBUY20 DS    0H                                                               
         LA    RF,1(RF)                                                         
         BCT   RE,MKGBUY10                                                      
                                                                                
* ROTATION START DAY                                                            
         ZIC   R2,RMKGDYIN                                                      
         SRL   R2,4                                                             
         EDIT  (R2),(1,MOBYRSDT)                                                
                                                                                
* START TIME 0600-3000                                                          
         CLC   RMKGDYT1,=AL2(600)  LESS THAN 0600?                              
         BL    MKGBUY30                                                         
         EDIT  RMKGDYT1,(4,MOBYSTIM),FILL=0                                     
         B     MKGBUY40                                                         
                                                                                
MKGBUY30 DS    0H                                                               
         ZICM  R3,RMKGDYT1,2                                                    
         LA    R3,2400(R3)         ADD 24 HOURS                                 
         EDIT  (R3),(4,MOBYSTIM),FILL=0                                         
                                                                                
* END TIME 0600-3000                                                            
MKGBUY40 DS    0H                                                               
         OC    RMKGDYT2,RMKGDYT2   ANY END TIME?                                
         BZ    MKGBUY60                                                         
         CLC   RMKGDYT2,=AL2(600)  LESS THAN 0600?                              
         BL    MKGBUY50                                                         
         EDIT  RMKGDYT2,(4,MOBYETIM),FILL=0                                     
         B     MKGBUY60                                                         
                                                                                
MKGBUY50 DS    0H                                                               
         ZICM  R3,RMKGDYT2,2                                                    
         LA    R3,2400(R3)         ADD 24 HOURS                                 
         EDIT  (R3),(4,MOBYETIM),FILL=0                                         
         DROP  R6                                                               
                                                                                
MKGBUY60 DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
                                                                                
* LENGTH UNITS                                                                  
         MVC   HALF,RMKGDUR                                                     
         MVI   MOBYLUNT,C'S'                                                    
         TM    HALF,X'80'                                                       
         BZ    MKGBUY70                                                         
         XI    HALF,X'80'          CLEAR MINUTE BIT                             
         MVI   MOBYLUNT,C'M'                                                    
                                                                                
* SPOT LENGTH (NOTE: NOT TOTAL SPOT LENGTH!)                                    
MKGBUY70 DS    0H                                                               
         EDIT  HALF,(3,MOBYTSLN),FILL=0                                         
                                                                                
* COST                                                                          
         EDIT  RMKGCOS,(9,MOBYCOST),FILL=0                                      
         DROP  R6                                                               
                                                                                
* COST QUALIFIER                                                                
                                                                                
* PRODUCT1 TIME SHARE                                                           
                                                                                
* SCHEDULE TYPE                                                                 
         MVI   MOBYSTYP,C'W'       ALWAYS WEEKLY                                
                                                                                
* PROGRAM NAME                                                                  
MKGBUY75 DS    0H                                                               
         XC    MGPRGNAM,MGPRGNAM   CLEAR ORBIT PROGRAM NAME                     
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MKGBUYX                                                          
         USING RMKGPGEL,R6                                                      
                                                                                
         CLI   RMKGPGLN,3                                                       
         BL    MKGBUYX                                                          
         ZIC   R1,RMKGPGLN                                                      
         SH    R1,=H'3'            SUBTRACT OVERHEAD                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOBYPGNM(0),RMKGPGM                                              
         MVC   MGPRGNAM,RMKGPGM    SAVE INCASE WE HAVE ORBITS                   
                                                                                
MKGBUYX  DS    0H                                                               
         BAS   RE,PRTNBUMP                                                      
                                                                                
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD ORBIT DESCRIPTION                                                    
***********************************************************************         
MKGORB   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,NEXTEL           ORBITS PRESENT?                              
         BNE   MKGORBX             SKIP IF NO                                   
                                                                                
         L     R6,AIO              GET FIRST ORBIT                              
         USING RMKGDYEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,P                                                             
         USING MORDORBD,R4                                                      
                                                                                
* RECORD                                                                        
MKGORB05 DS    0H                                                               
         MVC   MORBTID,=C'MKGORB'                                               
                                                                                
* ORDER NUMBER                                                                  
         MVC   MORBORDR,MGORDNUM                                                
                                                                                
* OFFER NUMBER                                                                  
         EDIT  MGOFFNUM,(2,MORBSEQN),FILL=0                                     
                                                                                
* ORBIT POSITION DAYS MTWTFSS                                                   
         MVC   MORBPDAY,=C'MTWTFSS'                                             
         IC    R1,RMKGDAYS         BUY ROTATION                                 
         LA    RE,7                                                             
         LA    RF,MORBPDAY                                                      
                                                                                
MKGORB10 DS    0H                                                               
         SLL   R1,1                                                             
         STC   R1,BYTE                                                          
         TM    BYTE,X'80'                                                       
         BO    MKGORB20                                                         
         MVI   0(RF),C' '          BLANK DAY IF NOT IN ROTATION                 
                                                                                
MKGORB20 DS    0H                                                               
         LA    RF,1(RF)                                                         
         BCT   RE,MKGORB10                                                      
                                                                                
* ORBIT POSITION  START DAY                                                     
         ZIC   R2,RMKGDYIN                                                      
         SRL   R2,4                                                             
         EDIT  (R2),(1,MORBPSDY)                                                
                                                                                
* START TIME 0600-3000                                                          
         CLC   RMKGDYT1,=AL2(600)  LESS THAN 0600?                              
         BL    MKGORB30                                                         
         EDIT  RMKGDYT1,(4,MORBPSTI),FILL=0                                     
         B     MKGORB40                                                         
                                                                                
MKGORB30 DS    0H                                                               
         ZICM  R3,RMKGDYT1,2                                                    
         LA    R3,2400(R3)         ADD 24 HOURS                                 
         EDIT  (R3),(4,MORBPSTI),FILL=0                                         
                                                                                
* END TIME 0600-3000                                                            
MKGORB40 DS    0H                                                               
         OC    RMKGDYT2,RMKGDYT2   ANY END TIME?                                
         BZ    MKGORB55                                                         
         CLC   RMKGDYT2,=AL2(600)  LESS THAN 0600?                              
         BL    MKGORB50                                                         
         EDIT  RMKGDYT2,(4,MORBPETI),FILL=0                                     
         B     MKGORB55                                                         
                                                                                
MKGORB50 DS    0H                                                               
         ZICM  R3,RMKGDYT2,2                                                    
         LA    R3,2400(R3)         ADD 24 HOURS                                 
         EDIT  (R3),(4,MORBPETI),FILL=0                                         
                                                                                
* PROGRAM NAME                                                                  
MKGORB55 DS    0H                                                               
         MVC   MORBPGRM,MGPRGNAM                                                
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   MKGORB60                                                         
         MVI   MORBCONT,C'*'       INDICATE MORE ORBITS TO FOLLOW               
         BAS   RE,PRTNBUMP                                                      
         B     MKGORB05                                                         
                                                                                
MKGORB60 DS    0H                                                               
         BAS   RE,PRTNBUMP                                                      
                                                                                
MKGORBX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD COMMENT                                                              
***********************************************************************         
MKGCOM   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'        COMMENTS PRESENT?                            
         BRAS  RE,GETEL                                                         
         BNE   MKGCOMX                                                          
                                                                                
         LA    R4,P                                                             
         USING MOFRBCMD,R4                                                      
                                                                                
* RECORD                                                                        
MKGCOM10 DS    0H                                                               
         MVC   MOBCTID,=C'MKGCOM'                                               
                                                                                
* ORDER NUMBER                                                                  
         MVC   MOBCORDR,MGORDNUM                                                
                                                                                
* OFFER NUMBER                                                                  
         EDIT  MGOFFNUM,(2,MOBCSEQN),FILL=0                                     
                                                                                
* COMMENTS                                                                      
         USING RMKGDCEL,R6                                                      
         ZIC   R1,RMKGDCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MOBCCOMT(0),RMKGDCCM                                             
                                                                                
MKGCOM20 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   MKGCOM30                                                         
         MVI   MOBCCONT,C'*'       MORE TO FOLLOW                               
         BAS   RE,PRTNBUMP                                                      
         B     MKGCOM10                                                         
                                                                                
MKGCOM30 DS    0H                                                               
         BAS   RE,PRTNBUMP                                                      
                                                                                
MKGCOMX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD BUY DETAIL                                                           
***********************************************************************         
MKGDTL   NTR1                                                                   
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         MVC   MGSPTCST,RMKGCOS    SAVE INCASE OF MULTIPLE ORBITS               
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGDTEL,R6                                                      
                                                                                
         LA    R4,P                                                             
         USING MOFRBDTD,R4                                                      
                                                                                
* RECORD                                                                        
MKGDTL10 DS    0H                                                               
         MVC   MOBDTID,=C'MKGDTL'                                               
                                                                                
* ORDER NUMBER                                                                  
         MVC   MOBDORDR,MGORDNUM                                                
                                                                                
* OFFER NUMBER                                                                  
         EDIT  MGOFFNUM,(2,MOBDSEQN),FILL=0                                     
                                                                                
* SPOT COST                                                                     
         EDIT  MGSPTCST,(9,MOBDSCST),FILL=0                                     
                                                                                
* START DATE                                                                    
         GOTO1 DATCON,DMCB,(3,RMKGDTST),(X'20',MOBDSTDT)                        
                                                                                
* NUMBER OF WEEKS                                                               
         EDIT  RMKGDTWK,(2,MOBDNOWK),FILL=0                                     
                                                                                
* SPOTS PER WEEK                                                                
         EDIT  RMKGDTNW,(2,MOBDSPTS),FILL=0                                     
                                                                                
         TM    RMKGDTIN,X'40'      ALTERNATE WEEKS?                             
         BZ    MKGDTL40                                                         
                                                                                
         MVC   MOBDNOWK,=C'01'                                                  
         MVC   MYP,P               SAVE OFF OUTPUT AREA                         
         ZIC   R3,RMKGDTWK         USE NUMBER OF WEEKS AS COUNTER               
         B     MKGDTL30            TO                                           
                                                                                
MKGDTL20 DS    0H                  EXPLODE ALTERNATING WEEKS                    
* ADD 2 WEEKS TO GET NEXT ALTERNATING WEEK                                      
         GOTO1 ADDAY,DMCB,MOBDSTDT,MGALTWK,14                                   
         MVI   MOBDCONT,C'*'       MORE TO FOLLOW                               
         BAS   RE,PRTNBUMP                                                      
         MVC   P,MYP                                                            
         MVC   MOBDSTDT,MGALTWK                                                 
         GOTO1 DATCON,DMCB,(0,MGALTWK),(X'20',MOBDSTDT)                         
                                                                                
MKGDTL30 DS    0H                  MORE TO EXPLODE?                             
         BCT   R3,MKGDTL20                                                      
                                                                                
MKGDTL40 DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   MKGDTL50                                                         
         MVI   MOBDCONT,C'*'       MORE TO FOLLOW                               
         BAS   RE,PRTNBUMP                                                      
         B     MKGDTL10                                                         
                                                                                
MKGDTL50 DS    0H                                                               
         BAS   RE,PRTNBUMP                                                      
                                                                                
MKGDTLX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MAKEGOOD TRAILER                                                              
***********************************************************************         
MKGTLR   NTR1                                                                   
         LA    R4,P                                                             
         USING MOFRTLRD,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOTLTID,=C'MKGTLR'                                               
                                                                                
* ORDER NUMBER                                                                  
         MVC   MOTLORDR,MGORDNUM                                                
                                                                                
* OFFER ID                                                                      
         MVC   MOTLOFID(2),MGGRPCDE                                             
                                                                                
* VERSION NUMBER WITHIN ID                                                      
         MVC   MOTLSEQN,MGVERNUM                                                
                                                                                
* RECORD COUNT                                                                  
         L     RF,RECCOUNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,RECCOUNT                                                      
         EDIT  RECCOUNT,(6,MOTLRCCT),FILL=0                                     
                                                                                
* TOTAL MG SPOTS                                                                
         EDIT  TOTMGSPT,(6,MOTLTSPT),FILL=0                                     
                                                                                
* TOTAL MG DOLLARS                                                              
         EDIT  TOTMGDOL,(10,MOTLTDOL),FILL=0                                    
                                                                                
         BAS   RE,PRTNBUMP                                                      
                                                                                
MKGTLRX  DS    0H                  CLOSE PQ AND EXIT                            
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET CONTRACT INFO BY SETTING UP A FAKE FIELD HEADER POINTED TO BY R2          
***********************************************************************         
GETKINFO NTR1                                                                   
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
                                                                                
* CONTRACT NUMBER                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         PACK  MYWORK(1),RMKGKCON+3(1)                                          
         PACK  MYWORK+1(1),RMKGKCON+2(1)                                        
         PACK  MYWORK+2(1),RMKGKCON+1(1)                                        
         PACK  MYWORK+3(1),RMKGKCON(1)                                          
         MVC   CCONNUM,MYWORK                                                   
         ZAP   MYWORK+5(5),=P'0'                                                
         MVO   MYWORK+5(5),MYWORK(4)                                            
         ZAP   MYWORK(5),=P'99999999'                                           
         SP    MYWORK(5),MYWORK+5(5)                                            
         LA    R2,FAKEHDR          SETUP FAKE FIELD HEADER                      
         EDIT  (P5,MYWORK),(8,8(R2)),ALIGN=LEFT                                 
         DROP  R6                                                               
                                                                                
         STC   R0,5(R2)            SET LENGTH OF FIELD                          
         MVI   0(R2),16            FIELD HEADER LENGTH                          
         MVI   4(R2),X'08'         SET VALID NUMERIC                            
         GOTO1 VALICON,DMCB,(R2)                                                
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE THE PRINT QUEUE                                                    
***********************************************************************         
INITPQ   NTR1                                                                   
*                                                                               
*   SET REPORT ID/CLASS FOR SPOOLING                                            
*                                                                               
         MVC   REMUSER,=C'DMG'                                                  
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=CL11'MAKEGOOD '                                          
         OI    GENSTAT3,NOCLRSPK                                                
         MVI   PLCLASS,C'G'        CLASS G                                      
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
         DROP  RF                                                               
*                                                                               
         LA    RE,SPLKEYAD         SET EXTENDED KEY ADDRESS                     
         ST    RE,SPOOLQLK         SAVE EXTENDED KEY ADDRESS                    
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         DROP  RE                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
***********************************************************************         
EDICT    NTR1                                                                   
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
*                                                                               
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT A ++DDS CARD                                                            
*                                                                               
         LA    R3,P                                                             
         USING EDICTD,R3                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'        UPPER CASE 'D'                               
         MVC   EDIPROG,=C'MKG'     TYPE=MAKEGOOD                                
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
         DROP  R3                                                               
*                                  SEND SPECIAL PRINT LINE                      
EDICT50  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
                                                                                
EDICTX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
* BUMP TOTAL RECORD COUNTER                                                     
***********************************************************************         
PRTNBUMP DS    0H                                                               
         ST    RE,FULL                                                          
         L     RF,RECCOUNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,RECCOUNT                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE ALL TO PAGE ONE                        
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GETTIME: HHMM ONLY                                                            
***********************************************************************         
GETTIME  NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,MYWORK                                                      
         GOTO1 HEXOUT,DMCB,MYWORK,0(R2),2,0                                     
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT ROUTINES                                                                 
***********************************************************************         
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
BADERROR MVC   RERROR,=AL2(440)    UNEXPECTED ERROR OCCURRED                    
         B     ERREND                                                           
*                                                                               
INVLGRP  MVC   RERROR,=AL2(463)    INVALID GROUP CODE                           
         B     ERREND                                                           
*                                                                               
UNSELOFF MVC   RERROR,=AL2(114)    UNSELECTED CHOICE OFFER DETECTED             
         B     ERREND                                                           
*                                                                               
MISSORD  MVC   RERROR,=AL2(468)    CANNOT SEND, MISSING ROUTING INFO            
         B     ERREND                                                           
*                                                                               
STASEND  MVC   RERROR,=AL2(513)    STATION MUST SEND OFFER BACK                 
         B     ERREND                                                           
*                                                                               
MISSHDR  MVC   RERROR,=AL2(431)    CANNOT SEND, MISSING ROUTING INFO            
         B     ERREND                                                           
*                                                                               
REJRECER MVC   RERROR,=AL2(566)    CANNOT APPLY, REJ/REC STAT FOUND             
         B     ERREND                                                           
*                                                                               
STAWIPER MVC   RERROR,=AL2(628)    WIP ON STATION SIDE                          
         B     ERREND                                                           
*                                                                               
TIMEERR  MVC   RERROR,=AL2(646)    5 MINUTES WAIT                               
         B     ERREND                                                           
*                                                                               
MULTIERR MVC   RERROR,=AL2(761)    MULTI-GROUP NOT ALLOWED                      
         B     ERREND                                                           
*                                                                               
APLYERR  MVC   RERROR,=AL2(664)    APPLY ERROR                                  
         B     ERREND                                                           
*                                                                               
MISSAGY  MVC   RERROR,=AL2(817)    MISSING AGENCY EQUIVALENCY CODE              
         B     ERREND                                                           
*                                                                               
REVINPRC MVC   RERROR,=AL2(900)    CAN'T SEND WHILE REVISION IN PROCESS         
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
INVPFERR MVI   GERROR1,ERINVPFK    INVALID PFKEY                                
         B     ERREND                                                           
*                                                                               
NEXTREQ  MVC   RERROR,=AL2(3)      ENTER NEXT REQUEST                           
         B     INFEND                                                           
*                                                                               
NEXTPAGE MVC   RERROR,=AL2(111)    HIT ENTER FOR NEXT SCREEN                    
         B     INFEND                                                           
*                                                                               
HITBOTTM MVC   RERROR,=AL2(125)    ALL MAKEGOOD DISPLAYED                       
         B     INFEND                                                           
*                                                                               
MGSENTOK MVC   RERROR,=AL2(126)    THE MAKEGOOD IS ON ITS WAY!                  
         B     INFRTEXT                                                         
*                                                                               
MGCNFOK  MVC   RERROR,=AL2(127)    MAKEGOOD IS APPLIED!                         
         B     INFRTEXT                                                         
*                                                                               
MGRECALL MVC   RERROR,=AL2(138)    MAKEGOOD IS RECALLED!                        
         B     INFRTEXT                                                         
*                                                                               
MGCANCEL MVC   RERROR,=AL2(128)    MAKEGOOD IS CANCELLED!                       
         B     INFRTEXT                                                         
*                                                                               
MGRSNTOK MVC   RERROR,=AL2(129)    THE NEW MAKEGOOD IS ON ITS WAY!              
         B     INFRTEXT                                                         
*                                                                               
MGHISTRY MVC   RERROR,=AL2(139)    HISTORY DISPLAYED                            
         B     INFEND                                                           
*                                                                               
MGUNKNER MVC   RERROR,=AL2(168)    UNKNOWN MAKEGOOD ERROR                       
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         B     *+8                                                              
INFEND   MVI   RMSGTYPE,C'I'                                                    
         LA    R2,MGBHDLNH         SET CURSOR HERE FOR ALL ERROR/INFO           
         GOTO1 MYERROR                                                          
*                                                                               
INFRTEXT DS    0H                  SUBSTITUTION TEXT                            
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         B     INFEND                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SELECT PFKEY TABLE DEFINITIONS                                                
***********************************************************************         
SPFTABLE DS    0C                                                               
*                                                                               
* SWAP TO CONTRACT                                                              
         DC    AL1(SPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF02X   EQU   *                                                                
*                                                                               
* APPLY MAKEGOOD                                                                
         DC    AL1(SPF04X-*,04,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF04X   EQU   *                                                                
*                                                                               
* SEND MAKEGOOD TO PQ                                                           
         DC    AL1(SPF06X-*,06,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF06X   EQU   *                                                                
*                                                                               
* DISPLAY HISTORY                                                               
         DC    AL1(SPF07X-*,07,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF07X   EQU   *                                                                
*                                                                               
* CANCEL WITH MORE TO FOLLOW                                                    
         DC    AL1(SPF08X-*,08,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF08X   EQU   *                                                                
*                                                                               
* CANCEL MAKEGOOD                                                               
         DC    AL1(SPF09X-*,09,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF09X   EQU   *                                                                
*                                                                               
* RESEND MAKEGOOD TO PQ                                                         
         DC    AL1(SPF10X-*,10,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF10X   EQU   *                                                                
*                                                                               
* RETURN TO MGOFFER/LIST SCREEN                                                 
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION SELECT                                                             
***************                                                                 
STPFKS00 CLI   ACTNUM,MYACTSEL     ACTION SEL?                                  
         BNE   EXIT                                                             
*                                                                               
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPFKS90                                                         
*                                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RMKGSDEM,R6                                                      
*                                                                               
         CLI   PFKEY,2             SWAP TO CONTRACT PROGRAM                     
         BNE   STPFKS05                                                         
         OI    DISPFLG2,SWAP2CON                                                
         B     STPFKS90                                                         
*                                                                               
STPFKS05 DS    0H                                                               
         CLI   PFKEY,4             APPLY MAKEGOOD                               
         BNE   STPFKS10                                                         
*                                                                               
*        BAS   RE,CHKTIME                                                       
*        BNE   TIMEERR                                                          
*                                                                               
         TM    RMKGSFG1,RMGF1MAR   MUST HAVE RECEIVED APPROVAL                  
         BZ    INVPFERR                                                         
         OI    DISPFLAG,CFRMMKG                                                 
         B     STPFKS90                                                         
*                                                                               
STPFKS10 DS    0H                                                               
         CLI   PFKEY,6             SEND MAKEGOOD                                
         BNE   STPFKS20                                                         
         OC    RMKGSFG1,RMKGSFG1   NEW ORDER ONLY                               
         BNZ   INVPFERR                                                         
         OI    DISPFLAG,SENDMKG                                                 
         B     STPFKS90                                                         
*                                                                               
STPFKS20 DS    0H                                                               
         CLI   PFKEY,7             MAKEGOOD HISTORY                             
         BNE   STPFKS25                                                         
         OI    DISPFLAG,HISTMKG                                                 
         B     STPFKS90                                                         
*                                                                               
STPFKS25 DS    0H                                                               
         CLI   PFKEY,8             RECALL/CANCEL WITH MORE TO FOLLOW            
         BNE   STPFKS30                                                         
*                                                                               
*        BAS   RE,CHKTIME                                                       
*        BNE   TIMEERR                                                          
*                                                                               
         TM    RMKGSFG1,RMGF1MSN+RMGF1MAR+RMGF1MCR                              
         BZ    INVPFERR            MUST BE SENT/APPROVED/RESEND/ERROR           
         OI    DISPFLG2,CANWMORE                                                
         B     STPFKS90                                                         
*                                                                               
STPFKS30 DS    0H                                                               
         CLI   PFKEY,9             CANCEL MAKEGOOD                              
         BNE   STPFKS40                                                         
*                                                                               
*        BAS   RE,CHKTIME                                                       
*        BNE   TIMEERR                                                          
*                                                                               
         TM    RMKGSFG1,RMGF1MSN+RMGF1MAR+RMGF1MCR+RMGF1MCM+RMGF1MER+RMX        
               GF1MRR                                                           
         BZ    INVPFERR            MUST BE SENT/APPROVED/RESEND/RECALLD         
         OI    DISPFLAG,CANCMKG    /ERROR/REJECTED                              
         B     STPFKS90                                                         
*                                                                               
STPFKS40 DS    0H                                                               
         CLI   PFKEY,10            RESEND MAKEGOOD                              
         BNE   STPFKS90                                                         
*                                                                               
*        BAS   RE,CHKTIME                                                       
*        BNE   TIMEERR                                                          
*                                                                               
         TM    RMKGSCST,RMKGSRVQ   REVISED?                                     
         BZ    STPFKS45            CONTRACTD: REJECT/RECALL DOES NOT            
         TM    RMKGSCST,RMKGSRCQ+RMKGSRJQ     CLEAR REVISED FLAG                
         BZ    STPFKS50            CHECK TO MAKE SURE NOT REJECT/RECALL         
*                                                                               
STPFKS45 DS    0H                                                               
         TM    RMKGSFG1,RMGF1MSN+RMGF1MAR+RMGF1MRR+RMGF1MCR+RMGF1MCM+RMX        
               GF1MER                                                           
         BZ    INVPFERR            CAN RESEND FOR RESENT/SENT/REJ/APR/          
*                                  CANWMORE/REV/ERROR                           
STPFKS50 DS    0H                                                               
         OI    DISPFLAG,RESNDMKG                                                
*                                                                               
STPFKS90 LA    R2,SPFTABLE         YES, USE SEL PFKEY TABLE                     
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STPFX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* CHECK TO MAKE SURE 5 MINUTES HAVE ELAPSED BEFORE NEXT ACTION                  
* THIS WOULD ENSURE THAT EDICT HAD A CHANCE TO PROCESS OUTSTANDING              
* ENTRIES                                                                       
***********************************************************************         
CHKTIME  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   YES                                                              
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
CHKT10   DS    0H                                                               
         USING RMKGATEM,R6                                                      
         TM    RMKGATAT,X'20'+X'10'                                             
         BNZ   CHKT20              SKIP CHECK FOR APPROVAL/REJECTION            
         MVC   WORK(5),RMKGATDT                                                 
         DROP  R6                                                               
*                                                                               
CHKT20   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    CHKT10                                                           
*                                                                               
         OC    WORK,WORK                                                        
         BZ    YES                                                              
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,WORK+5)                                     
*                                                                               
         CLC   WORK(2),WORK+5      WAS LAST ACTION TODAY?                       
         BNE   YES                                                              
*                                                                               
         THMS  DDSTIME=YES         YES, NEED TO CHECK TIME                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         SP    DUB(4),=P'500'      MUST BE AT LEAST 5 MINUTES APART             
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,WORK+7                                                      
*                                                                               
* NEED TO ADJUST FOR MINUTES. IF ORIGINAL TIME IS 15:03:14, AND WE PACK         
* SUBTRACT 5 MINUTES, THE RESULT IS 15:98:14. THIS CAN BE ADJUSTED BY           
* SUBTRACTING AN ADDITIONAL 40 MINUTES = 15:58:14                               
*                                                                               
         CLI   WORK+8,X'60'                                                     
         BL    CHKT30                                                           
         SP    DUB(4),=P'4000'                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,WORK+7                                                      
*                                                                               
CHKT30   DS    0H                                                               
         CLC   WORK+2(3),WORK+7                                                 
         BL    YES                                                              
         B     NO                                                               
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ DARE X'41' RECORD IN IO3                                                 
*                                                                               
* CHECK IF CURRENT REVISION IN PROGRESS                                         
* ONLY ALLOW MAKEGOOD SEND IF REVISION IS IN STATUS OF APPROVED                 
***********************************************************************         
CHKREV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,KEY              READ AGENCY RECORD TO GET THE                
         USING RAGY2KEY,RF         DARE AGENCY EQUIVALENT CODES                 
         XC    KEY,KEY                                                          
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,CCONKAGY                                                
         MVC   RAGK2AOF,CCONKAOF                                                
         MVC   RAGK2REP,AGENCY                                                  
         DROP  RF                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   CHKREVX             MUST BE THERE!                               
         MVC   AIO,AIO3            READ AGENCY RECORD INTO IO3                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         L     R6,AIO3                                                          
         USING RAGY2REC,R6                                                      
                                                                                
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'41'                                                   
         MVC   RDARKREP,AGENCY                                                  
         MVC   RDARKSTA(5),CCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,SPACES                                                  
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    CHKREVX             YES -- MISSING AGENCY                        
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         LA    R2,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
         B     CHKREV10                                                         
*                                                                               
** PREP KEY FOR SKIP READING : SKIP TO NEXT AGENCY OFFICE IF AGENCY             
** OFFICE DIDN'T CHANGE                                                         
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
CHKREV08 CLC   RDARKAOF,PRVKEY.RDARKAOF  DID AGENCY OFFICE CHANGE?              
         DROP  PRVKEY                                                           
         BNE   CHKREV09              YES -- DON'T INCREMENT                     
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
CHKREV09 XC    RDARKORD(7),RDARKORD   CLEAR FIELDS AFTER AGENCY OFFICE          
*                                                                               
CHKREV10 DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   CHKREV11              NO --  NEXT RECORD                         
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,CDARNUM      MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   CHKREV11                                                         
         CLC   RDARKORD,CDARNUM    SAME ORDER NUMBER?                           
         BNE   CHKREV08            NO -- SKIP READ                              
         CLI   RDARKRT,X'10'       YES -- AGENCY HEADER?                        
         BE    CHKREV20              YES  -- DARE RECORD BUILT...               
         B     CHKREVX                                                          
*                                                                               
CHKREV11 CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
CHKREV12 LA    R2,5(R2)                                                         
         OC    0(3,R2),0(R2)       NULL EQUIVALENCY CODE?                       
         BZ    CHKREVX             YES -- MISSING AGENCY                        
         CLC   RDARKAGY,0(R2)      SAME EQUIVALANCY CODE?                       
         BNE   *+12                NO --  READHI                                
         BCT   R3,CHKREV12         YES -- CHCK NEXT EQUIVALANCY CODE            
         B     CHKREVX                                                          
*                                                                               
         MVC   RDARKAGY(3),0(R2)   EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF CLEAR FIELDS AFTER AGENCY OFFICE            
         BCT   R3,CHKREV10         READHI                                       
         B     CHKREVX                                                          
         DROP  R4                                                               
*                                                                               
CHKREV20 DS    0H                                                               
         MVC   AIO,AIO3            READ X'41' RECORD INTO IO3                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
         CLI   RDARBSTS,C'A'       CANNOT SEND IF REVISION IN PROCESS           
*        BNE   REVINPRC                                                         
         BE    CHKREVX                                                          
         TM    DISPFLAG,CANCMKG    UNLESS WE ARE JUST CANCELLING                
         BZ    REVINPRC                                                         
         DROP  R6                                                               
*                                                                               
CHKREVX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ DARE X'51' RECORD IN IO3                                                 
*                                                                               
* FOR TAKEOVER CONTRACTS, WE NEED TO BUILD A FAKE X'51' HEADER RECORD           
* IN IO3, WHILE INGORING THE AGENCY OFFICE SO WE CAN PROCESS MAKEGOODS.         
* THE NECESSARY INFORMATION ARE KEPT IN X'1C' AND X'1D' ELEMENTS IN THE         
* CONTRACT RECORD                                                               
***********************************************************************         
GETDAR51 DS    0H                                                               
         NMOD1 0,**GETD**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    RF,KEY              READ AGENCY RECORD TO GET THE                
         USING RAGY2KEY,RF         DARE AGENCY EQUIVALENT CODES                 
         XC    KEY,KEY                                                          
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,CCONKAGY                                                
         MVC   RAGK2AOF,CCONKAOF                                                
         MVC   RAGK2REP,AGENCY                                                  
         DROP  RF                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   MISSAGY             MUST BE THERE!                               
         MVC   AIO,AIO3            READ AGENCY RECORD INTO IO3                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         NI    DISPFLG2,X'FF'-CKACTIVE                                          
*                                                                               
GETDAR05 DS    0H                                                               
         L     R6,AIO3                                                          
         USING RAGY2REC,R6                                                      
                                                                                
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RDARKTYP,X'51'                                                   
         TM    DISPFLG2,CKACTIVE                                                
         BZ    *+8                                                              
         MVI   RDARKTYP,X'41'                                                   
*                                                                               
         MVC   RDARKREP,AGENCY                                                  
         MVC   RDARKSTA(5),CCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,SPACES                                                  
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    MISSAGY             YES -- MISSING AGENCY                        
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         LA    R2,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
         B     GETDAR10                                                         
*                                                                               
** PREP KEY FOR SKIP READING : SKIP TO NEXT AGENCY OFFICE IF AGENCY             
** OFFICE DIDN'T CHANGE                                                         
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
GETDAR08 CLC   RDARKAOF,PRVKEY.RDARKAOF  DID AGENCY OFFICE CHANGE?              
         DROP  PRVKEY                                                           
         BNE   GETDAR09              YES -- DON'T INCREMENT                     
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
GETDAR09 XC    RDARKORD(7),RDARKORD   CLEAR FIELDS AFTER AGENCY OFFICE          
*                                                                               
GETDAR10 DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETDAR11              NO --  NEXT RECORD                         
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,CDARNUM      MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE  SAME KEY?                         
         BNE   GETDAR11                                                         
         CLC   RDARKORD,CDARNUM    SAME ORDER NUMBER?                           
         BNE   GETDAR08            NO -- SKIP READ                              
         CLI   RDARKRT,X'10'       YES -- AGENCY HEADER?                        
         BE    GETDAR20              YES  -- DARE RECORD BUILT...               
         B     MISSAGY                                                          
*                                                                               
GETDAR11 CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
GETDAR12 LA    R2,5(R2)                                                         
         OC    0(3,R2),0(R2)       NULL EQUIVALENCY CODE?                       
         BZ    GETDAR15            YES -- MISSING AGENCY                        
         CLC   RDARKAGY,0(R2)      SAME EQUIVALANCY CODE?                       
         BNE   *+12                NO --  READHI                                
         BCT   R3,GETDAR12         YES -- CHCK NEXT EQUIVALANCY CODE            
         B     GETDAR15                                                         
*                                                                               
         MVC   RDARKAGY(3),0(R2)   EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF CLEAR FIELDS AFTER AGENCY OFFICE            
         BCT   R3,GETDAR10         READHI                                       
*                                                                               
* THE CONFIRMED ORDER WAS NOT FOUND, BEFORE CHECKING THE TAKEOVER               
* ELEMENT, CHECK IF AGENCY HAS SENT OVER THE ORDER BY LOOKING FOR               
* X'41' DARE ORDER RECORD                                                       
*                                                                               
GETDAR15 DS    0H                                                               
         TM    DISPFLG2,CKACTIVE   CHECK ACTIVE X'41' RECORD                    
         BO    GETDAR30                                                         
         OI    DISPFLG2,CKACTIVE                                                
         B     GETDAR05                                                         
*                                                                               
GETDAR20 DS    0H                                                               
         MVC   AIO,AIO3            READ X'51' RECORD INTO IO3                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R4,AIO3                                                          
         B     GETDAR35            USE USER SIGNON AS RECEIVER ID               
*                                                                               
* CHECK IF TAKEOVER CONTRACT/DARE                                               
*                                                                               
GETDAR30 DS    0H                                                               
         OC    CCONDKAD,CCONDKAD                                                
         BZ    NO                                                               
         MVC   AIO,AIO2            PUT CONTRACT REC IN IO2                      
         MVC   KEY+28(4),CCONDKAD                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'1C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   NO                                                               
         USING RCONTKEL,R6                                                      
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R4,AIO3                                                          
         USING RDARREC,R4                                                       
         MVC   RDARKSTA(5),CCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'                                                  
         MVC   RDARSNDR,RCONTKRC   SENDER ID                                    
         MVC   RDARRTS,RCONTKRT    AGENCY RETURN TO SENDER INFO                 
         DROP  R6                                                               
*                                                                               
* GET RECEIVER ID                                                               
*                                                                               
GETDAR35 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TWAORIG                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO2                  
*                                                                               
         CLI   8(R1),0                                                          
         BNE   NO                                                               
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BNE   NO                                                               
*                                                                               
         LA    R6,28(R6)                                                        
GETDAR40 CLI   0(R6),X'02'         GET REP SIGN-ON ID                           
         BE    GETDAR50                                                         
         BH    NO                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETDAR40                                                         
         B     NO                                                               
*                                                                               
GETDAR50 DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDARRCVR(0),2(R6)                                                
         OC    RDARRCVR,SPACES                                                  
*                                                                               
GETDARX  DS    0H                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE FOR MAKEGOOD PFKEY ACTIONS                                            
***********************************************************************         
MGACTION DS    0H                                                               
         NMOD1 0,**MGAC**,RR=R4                                                 
         L     RC,0(R1)                                                         
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R5,SYSSPARE                                                      
         ST    R4,RELO2                                                         
                                                                                
MGCAPLYQ EQU   1                                                                
MGCANCLQ EQU   2                                                                
MGHISTYQ EQU   3                                                                
MGUPHISQ EQU   4                                                                
MGDISPFQ EQU   5                                                                
MGSWAPCN EQU   6                                                                
MGOK2APL EQU   7                                                                
                                                                                
         CLI   4(R1),MGCAPLYQ      APPLY MAKEGOOD                               
         BE    APPLY                                                            
         CLI   4(R1),MGCANCLQ      CANCEL MAKEGOOD                              
         BE    CANCEL                                                           
         CLI   4(R1),MGUPHISQ      UPDATE HISTORY                               
         BE    UPHIS00                                                          
         CLI   4(R1),MGHISTYQ      DISPLAY MAKEGOOD HISTORY                     
         BE    HISTORY                                                          
         CLI   4(R1),MGDISPFQ      DISPLAY PFKEYS                               
         BE    DISPFK00                                                         
         CLI   4(R1),MGSWAPCN      SWAP TO THE CONTRACT PROGRAM                 
         BE    SWAPCONT                                                         
         CLI   4(R1),MGOK2APL      CHECK IF OK TO APPLY                         
         BE    CHECKAPL                                                         
         DC    H'0'                                                             
                                                                                
MGACTX   DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* APPLY MAKEGOOD OFFER                                                          
***********************************************************************         
APPLY    DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
*        BZ    BADERROR                                                         
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
                                                                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         BAS   RE,GETKINFO         GET CONTRACT INFORMATION                     
         OC    CDARNUM,CDARNUM     ORDER NUMBER PRESENT?                        
*        BZ    MISSORD             NO? WE NEED IT!                              
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
*                                  READ IN DARE X'51' REC TO IO3                
         GOTO1 =A(GETDAR51),DMCB,(RC),RR=RELO2                                  
*        BNZ   MISSORD                                                          
         BZ    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
                                                                                
         L     R6,AIO                                                           
         MVC   KEY(L'RMKGKEY),0(R6)                                             
         GOTO1 HIGH                                                             
*                                  MAKEGOOD OFFERS FOR THIS CONTRACT            
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BZ    *+6                 HEADER RECORD MUST BE THERE!                 
         DC    H'0'                                                             
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'01'        GET DESCRIPTION ELEMENT                      
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
*                                                                               
         TM    RMKGSCST,RMKGSAPQ   MUST BE APPLIED ON THE CONTRACT SIDE         
*        BZ    APLYERR                                                          
         BNZ   *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
*                                                                               
         MVI   RMKGSFG1,RMGF1MCF   MAKEGOOD HAS BEEN APPLIED!                   
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+8(1),RMKGSCVR                                               
         MVI   DMCB+9,X'08'                                                     
                                                                                
         GOTO1 UPDTHIST,DMCB,,,                                                 
*                                                                               
         GOTO1 PUTREC              FLAG AND WRITE TO REC                        
         DROP  R6                                                               
*                                                                               
*        BAS   RE,INITPQ           INITIALIZE THE PQ                            
*        BAS   RE,EDICT            SEND EDICT INFORMATION                       
*        BAS   RE,MKGROK           SEND MAKEGOOD ACCEPTANCE                     
*                                                                               
         B     MGACTX                                                           
         EJECT                                                                  
***********************************************************************         
* SWAP TO CONTRACT AND APPLY MAKEGOOD                                           
***********************************************************************         
SWAPCONT DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
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
* WE ARE SENDING THE CONTRACT NUMBER AND THE GROUP CODE.                        
* FOR NOW CONTRACT WILL ONLY BRING UP THE CONTRACT WITH ACTION MGL.             
* LATER ON, WE NEED TO GO TO CONTRACT AND AUTOMATICALLY APPLY THIS MG           
* AND COME BACK WITHOUT THE USER EVER SEEING THIS.                              
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(4),CCONKNUM    CONTRACT NUMBER                              
         MVC   ELEM+4(2),SPACES                                                 
*                                                                               
         TM    DISPFLG2,SWAP2CON   IF NOT NORMAL SWAP, MUST BE                  
         BO    SWAPK10             AUTO-APPLY: SEND GROUP CODE TOO              
         NI    DISPFLAG,X'FF'-CFRMMKG                                           
*                                  CLEAR SEND FLAG                              
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         MVC   ELEM+4(2),RMKGKGRP  GROUP CODE                                   
         DROP  R6                                                               
*                                                                               
SWAPK10  DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',ELEM,6,GLRMGAPL                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
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
         B     MGACTX                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK IF OK TO APPLY                                                          
* CODE LIFTED FROM RECNT34                                                      
***********************************************************************         
CHECKAPL DS    0H                                                               
         OC    CCONDKAD,CCONDKAD                                                
         BZ    BADERROR                                                         
         MVC   AIO,AIO2            PUT CONTRACT REC IN IO2                      
         MVC   KEY+28(4),CCONDKAD                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2                                                          
         USING RCONREC,R6                                                       
         TM    RCONMODR+1,X'40'    IF GRAPHNET, REP CAN APPLY ANYTIME           
         BO    CHKAPLX                                                          
         DROP  R6                                                               
*                                                                               
         L     R4,AIO                                                           
         USING RMKGREC,R4                                                       
         TM    RMKGSCST,RMKGSRCQ+RMKGSRJQ                                       
         BNZ   REJRECER            CANNOT BE REJECTED OR RECALLED               
*                                                                               
* STATION WIP?                                                                  
*                                                                               
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    STAWIPER            YES, CANNOT APPLY                            
         DROP  R4                                                               
*                                                                               
CHKAPLX  DS    0H                                                               
         GOTO1 =A(GETDAR51),DMCB,(RC),RR=RELO                                   
         BNZ   MISSHDR                                                          
         B     MGACTX                                                           
         EJECT                                                                  
***********************************************************************         
* APPLY MAKEGOOD                                                                
***********************************************************************         
MKGROK   NTR1                                                                   
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRCFMD,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOCFTID,=C'MKGROK'                                               
                                                                                
* ORDER NUMBER                                                                  
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CDARNUM                                                
         EDIT  (P5,MYWORK),(8,MOCFORDR),FILL=0                                  
                                                                                
* ID OF SENDER                                                                  
         MVC   MOCFFRID,RDARRCVR                                                
                                                                                
* ID OF RECEIVER                                                                
         MVC   MOCFTOID,RDARSNDR                                                
*                                                                               
         CLC   =C'H7',RDARSNDR     TEMP MINDSHARE FIX                           
         BNE   MKGROK05                                                         
         MVC   MOCFTOID(2),=C'MS'                                               
*                                                                               
         CLC   =C'MSDNS',MOCFTOID                                               
         BNE   MKGROK05                                                         
         MVC   MOCFTOID(5),=C'MSDV '                                            
*                                                                               
MKGROK05 DS    0H                                                               
* ROUTING CODE                                                                  
         MVC   MOCFROUT,RDARKAGY                                                
                                                                                
* CURRENT DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(X'20',MOCFDATE)                               
                                                                                
* CURRENT TIME                                                                  
         GOTO1 GETTIME,DMCB,MOCFTIME                                            
*                                                                               
* STATION                                                                       
         MVC   MOCFQSTA,RDARKSTA                                                
         CLI   MOCFQSTA+4,C'L'                                                  
         BE    MKGROK10                                                         
         MVI   MOCFQSTA+5,C'V'     TV OR RADIO?                                 
         CLI   MOCFQSTA+4,C'T'                                                  
         BE    *+8                                                              
         MVI   MOCFQSTA+5,C'M'                                                  
                                                                                
* CONTRACT NUMBER                                                               
MKGROK10 DS    0H                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CCONKNUM                                               
         EDIT  (P5,MYWORK),(8,MOCFRPCN),FILL=0                                  
                                                                                
* AGENCY 'RETURN TO SENDER' DATA                                                
         MVC   MOCFRTNS,RDARRTS                                                 
**********                                                                      
********** TEMPORARY TO FORCE OMNY ORDERS TO GO TO CORRECT ADV                  
**********                                                                      
*RTSD     USING RTN2SNDR,MOCFRTNS                                               
*         CLC   =C'OM',RTSD.RTNPWRCD                                            
*         BNE   MKGROK20                                                        
*         MVC   RTSD.RTNSYSID,=C'05'                                            
*         MVC   RTSD.RTNAGYMD,=C'41'                                            
*         DROP  R6,RTSD                                                         
MKGROK20 DS    0H                                                               
**********                                                                      
**********                                                                      
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
                                                                                
* OFFER ID                                                                      
         MVC   MOCFOFRI(2),RMKGKGR1                                             
         DROP  R6                                                               
                                                                                
* VERSION NUMBER WITHIN ID                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
                                                                                
         EDIT  RMKGSCVR,(2,MOCFSEQN),FILL=0                                     
                                                                                
         BAS   RE,PRTNBUMP                                                      
                                                                                
         MVI   SPMODE,X'FF'        CLOSE PQ AND EXIT                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         B     MGACTX                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* CANCEL MAKEGOOD OFFER                                                         
* NOTE:                                                                         
* RESEND = CAN W/ MORE TO FOLLOW + SEND                                         
* RECALL = CAN W/ MORE TO FOLLOW                                                
***********************************************************************         
CANCEL   DS    0H                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
                                                                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         BAS   RE,GETKINFO         GET CONTRACT INFORMATION                     
         OC    CDARNUM,CDARNUM     ORDER NUMBER PRESENT?                        
         BZ    MISSORD             NO? WE NEED IT!                              
*                                  READ IN DARE X'51' REC TO IO3                
* CHECK IF REVISION IN PROGRESS                                                 
*                                                                               
         GOTO1 =A(CHKREV),RR=RELO                                               
*                                                                               
         GOTO1 =A(GETDAR51),DMCB,(RC),RR=RELO2                                  
         BNZ   MISSORD                                                          
                                                                                
         L     R6,AIO                                                           
         MVC   KEY(L'RMKGKEY),0(R6)                                             
         GOTO1 HIGH                                                             
*                                  MAKEGOOD OFFERS FOR THIS CONTRACT            
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING RMKGREC,R6                                                       
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BZ    *+6                 HEADER RECORD MUST BE THERE!                 
         DC    H'0'                                                             
*                                                                               
* LAST STATUS CANCEL WITH MORE OR REJECTED?                                     
*                                                                               
         CLI   RMKGSFG1,RMGF1MCM                                                
         BE    CANCEL03                                                         
         CLI   RMKGSFG1,RMGF1MRR                                                
         BNE   CANCEL05                                                         
CANCEL03 DS    0H                                                               
         NI    DISPFLG2,X'FF'-CANWMORE                                          
         TM    DISPFLAG,RESNDMKG   IF RESEND, EXIT HERE AND SEND                
         BO    MGACTX                                                           
*                                                                               
CANCEL05 DS    0H                                                               
         MVI   RMKGSFG1,RMGF1MCN   MAKEGOOD HAS BEEN CANCELLED!                 
*                                                                               
         TM    DISPFLAG,RESNDMKG   SKIP FOR RESEND                              
         BO    CANCEL55                                                         
         TM    DISPFLAG,CANCMKG    CANCEL?                                      
         BO    CANCEL10                                                         
         TM    DISPFLG2,CANWMORE   CANCEL W/MORE (RECALL)?                      
         BO    CANCEL20                                                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS FOR CANCEL. REP IS OFFERER ONLY                                       
***********************************************************************         
CANCEL10 DS    0H                                                               
* STATION WIP?                                                                  
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    STAWIPER                                                         
*                                  CLEAR ALL STATUS EXCEPT X'03'                
         NI    RMKGSCST,RMKGSPAQ+RMKGSCRQ                                       
*                                                                               
         TM    RMKGSCST,RMKGSCRQ   IF REP IS THE CREATOR?                       
         BZ    CANCEL15                                                         
*                                                                               
         OI    RMKGSCST,RMKGSCNQ   MARK MAKEGOOD CANCELLED BY REP               
*                                                                               
* DARE SHOULD DELETED THIS MAKEGOOD IF IT'S NEW AND NEVER SENT TO THE           
* STATION. WE'LL MARKED IT AS CANCELLED FOR NOW                                 
*                                                                               
* TODAY                            UPDATE LAST ACTIVITY DATE/TIME               
CANCEL15 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,RMKGSLAD)                                   
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,RMKGSLAT                                                    
*                                                                               
         TM    RMKGSCST,RMKGSCRQ   IF STA IS THE CREATOR?                       
         BO    CANCEL50                                                         
*                                  YES                                          
         OI    RMKGSCST,RMKGSRJQ   MARK MAKEGOOD REJECTED BY REP                
         MVC   RMKGSLRD,RMKGSLAD   DATE/TIME STAMP OF REJECT                    
         MVC   RMKGSLRT,RMKGSLAT                                                
*                                                                               
         B     CANCEL50                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS FOR CANCEL WITH MORE TO FOLLOW (RECALL)                               
***********************************************************************         
CANCEL20 DS    0H                                                               
         MVI   RMKGSFG1,RMGF1MCM   MAKEGOOD HAS BEEN CANCELLED W/MORE!          
         B     CANCEL50                                                         
*&&DO                                                                           
         TM    RMKGSCST,RMKGSCRQ   REP OR STATION CREATOR?                      
         BZ    CANCEL30                                                         
         DROP  R6                                                               
*                                                                               
* REP IS THE CREATOR OF MAKEGOOD GROUP                                          
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'21'        CHECK IF LAST ACTIVITY DATE/TIME             
         BRAS  RE,GETEL            MORE CURRENT THAN LAST MGS DATE/TIME         
         BNE   CANCEL50                                                         
         USING RCONMGEL,R6                                                      
*                                                                               
         L     R4,AIO                                                           
         USING RMKGREC,R4                                                       
         CLC   RCONMGDT,RMKGSLAD                                                
         BH    CANCEL25                                                         
         BL    CANCEL50                                                         
         CLC   RCONMGTM,RMKGSLAT                                                
         BL    CANCEL50            CHECK IF MGS DATE/TIME MORE CURRENT          
         DROP  R4,R6               THAN LAST ACTIVITY DATE/TIME                 
*                                                                               
*                                  MARK REP RECALLED                            
CANCEL25 DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGSCST,RMKGSRJQ+RMKGSRCQ                                       
         BNZ   CANCEL50            ALREADY RECALLED OR REJECTED?                
*                                  NOT REVISED ANYMORE                          
*                                  CLEAR ALL STATUS EXCEPT X'03'                
         NI    RMKGSCST,RMKGSPAQ+RMKGSCRQ                                       
         OI    RMKGSCST,RMKGSRCQ   MARK MAKEGOOD RECALLED FROM STATION          
         B     CANCEL40                                                         
*                                                                               
* STA IS THE CREATOR OF MAKEGOOD GROUP                                          
*                                                                               
CANCEL30 DS    0H                  MARK REP REJECTED                            
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         TM    RMKGSCST,RMKGSRJQ+RMKGSRCQ                                       
         BNZ   CANCEL50            ALREADY RECALLED OR REJECTED?                
*                                  NOT REVISED ANYMORE                          
*                                  CLEAR ALL STATUS EXCEPT X'03'                
         NI    RMKGSCST,RMKGSPAQ+RMKGSCRQ                                       
         OI    RMKGSCST,RMKGSRJQ   MARK MAKEGOOD REJECTED TO STATION            
* TODAY                                                                         
CANCEL40 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,RMKGSLRD)                                   
                                                                                
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,RMKGSLRT                                                    
*                                                                               
         MVC   RMKGSLAD,RMKGSLRD   UPDATE LAST ACTIVITY DATE/TIME               
         MVC   RMKGSLAT,RMKGSLRT                                                
*&&                                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE HISTORY AND WRITE RECORD                                               
***********************************************************************         
CANCEL50 DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         XC    DMCB,DMCB           RESEND STATUS GETS UPDATED LATER             
         MVC   DMCB+8(1),RMKGSCVR                                               
         MVC   DMCB+9(1),RMKGSFG1                                               
                                                                                
         GOTO1 UPDTHIST,DMCB,,,                                                 
                                                                                
         GOTO1 PUTREC              FLAG AND WRITE TO REC                        
         DROP  R6                                                               
*                                                                               
CANCEL55 DS    0H                                                               
         BAS   RE,INITPQ           INITIALIZE THE PQ                            
         BAS   RE,EDICT            SEND EDICT INFORMATION                       
         BAS   RE,MKGCAN           SEND MAKEGOOD CANCELLATION                   
                                                                                
         NI    DISPFLAG,X'FF'-CANCMKG                                           
         NI    DISPFLG2,X'FF'-CANWMORE                                          
*                                                                               
         TM    DISPFLAG,RESNDMKG   IF RESEND, EXIT HERE AND SEND                
         BO    MGACTX                                                           
                                                                                
         MVI   SPMODE,X'FF'        CLOSE PQ                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         BAS   RE,DISPFKEY         REDISPLAY PF LINE                            
*                                                                               
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
         MVI   BLOCK,3             LENGTH OF MESSAGE WITH TERMATING 0           
         MVC   BLOCK+1(2),RMKGKGRP                                              
         MVI   BLOCK+3,0                                                        
         OI    MGBHDLNH+6,X'40'    FORCE CURSOR HERE                            
*                                                                               
         CLI   RMKGSFG1,RMGF1MCM                                                
         BE    MGRECALL            MAKEGOOD RECALLED?                           
         B     MGCANCEL            MAKEGOOD CANCELLED                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CANCEL MAKEGOOD                                                               
***********************************************************************         
MKGCAN   NTR1                                                                   
         L     R6,AIO3                                                          
         USING RDARREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRCAND,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOCNTID,=C'MKGCAN'                                               
                                                                                
* ORDER NUMBER                                                                  
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CDARNUM                                                
         EDIT  (P5,MYWORK),(8,MOCNORDR),FILL=0                                  
                                                                                
* ID OF SENDER                                                                  
         MVC   MOCNFRID,RDARRCVR                                                
                                                                                
* ID OF RECEIVER                                                                
         MVC   MOCNTOID,RDARSNDR                                                
*                                                                               
         CLC   =C'H7',RDARSNDR     TEMP MINDSHARE FIX                           
         BNE   MKGCAN05                                                         
         MVC   MOCNTOID(2),=C'MS'                                               
*                                                                               
         CLC   =C'MSDNS',MOCNTOID                                               
         BNE   MKGCAN05                                                         
         MVC   MOCNTOID(5),=C'MSDV '                                            
*                                                                               
MKGCAN05 DS    0H                                                               
* ROUTING CODE                                                                  
         MVC   MOCNROUT,RDARKAGY                                                
                                                                                
* CURRENT DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(X'20',MOCNDATE)                               
                                                                                
* CURRENT TIME                                                                  
         GOTO1 GETTIME,DMCB,MOCNTIME                                            
                                                                                
* STATION                                                                       
         MVC   MOCNQSTA,RDARKSTA                                                
         CLI   MOCNQSTA+4,C'L'                                                  
         BE    MKGCAN10                                                         
         MVI   MOCNQSTA+5,C'V'     TV OR RADIO?                                 
         CLI   MOCNQSTA+4,C'T'                                                  
         BE    *+8                                                              
         MVI   MOCNQSTA+5,C'M'                                                  
                                                                                
* CONTRACT NUMBER                                                               
MKGCAN10 DS    0H                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),CCONKNUM                                               
         EDIT  (P5,MYWORK),(8,MOCNRPCN),FILL=0                                  
                                                                                
* AGENCY 'RETURN TO SENDER' DATA                                                
         MVC   MOCNRTNS,RDARRTS                                                 
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
                                                                                
* OFFER ID                                                                      
         MVC   MOCNOFRI(2),RMKGKGR1                                             
         DROP  R6                                                               
                                                                                
* VERSION NUMBER WITHIN ID                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
                                                                                
         EDIT  RMKGSCVR,(2,MOCNSEQN),FILL=0                                     
                                                                                
* NO OFFER TO FOLLOW                                                            
         MVI   MOCNNEWO,C'N'                                                    
         TM    DISPFLG2,CANWMORE   YES, UPDATE OFFER TO FOLLOW                  
         BZ    *+8                                                              
         MVI   MOCNNEWO,C'Y'                                                    
                                                                                
         BAS   RE,PRTNBUMP                                                      
                                                                                
         MVI   SPMODE,X'FF'        CLOSE PQ                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         B     MGACTX                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE AUDIT TRAIL/HISTORY                                                    
***********************************************************************         
UPDTHIST NTR1                      ENTERED FROM THE SAME NMOD                   
UPHIS00  DS    0H                  ENTERED FROM DIFFERENT NMOD                  
         MVC   WORK(2),8(R1)       VERSION NUMBER AND ACTION CODE               
                                                                                
         XC    ELEM,ELEM                                                        
                                                                                
         LA    R6,ELEM                                                          
         USING RMKGATEM,R6                                                      
         MVI   RMKGATCD,X'02'      ELEMENT CODE                                 
         MVI   RMKGATLN,RMKGATLQ   LENGTH                                       
         MVC   RMKGATVR(2),WORK    SAVE OFF VERSION#/ACTION                     
* TODAY                                                                         
         GOTO1 DATCON,DMCB,(5,0),(2,RMKGATDT)                                   
                                                                                
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,RMKGATTM                                                    
         DROP  R6                                                               
                                                                                
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM                                                          
                                                                                
UPHISX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY HISTORY                                                               
***********************************************************************         
HISTORY  DS    0H                                                               
         GOTO1 =A(HIST00),RR=RELO2                                              
         B     MGACTX                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY DYNAMIC PFKEY LINE ON BOTTOM OF SCREEN                                
***********************************************************************         
DISPFKEY NTR1                                                                   
DISPFK00 DS    0H                                                               
         L     R6,AIO                                                           
         USING RMKGSDEM,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                 MUST BE THERE!                               
         DC    H'0'                                                             
*                                                                               
* DISPLAY ORIGINATOR                                                            
*                                                                               
         XC    MGBWHO,MGBWHO                                                    
         MVC   MGBWHO(14),=C'Created by Rep'                                    
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    *+10                                                             
         MVC   MGBWHO+11(7),=C'Station'                                         
*                                                                               
* DISPLAY THE CURRENT CONTRACT STATUS                                           
*                                                                               
         XC    MGBSTAT,MGBSTAT                                                  
         MVC   CONTSTAT(9),=C'Con Stat:'                                        
         TM    RMKGSCST,RMKGSAPQ                                                
         BZ    DISPF30                                                          
         MVC   CONTSTAT+10(14),=C'Applied by Rep'                               
         GOTO1 DATETIME,DMCB,CONTSTAT+25                                        
         XC    DARESTAT,DARESTAT                                                
         B     DISPF100                                                         
*                                                                               
DISPF30  DS    0H                                                               
         TM    RMKGSCST,RMKGSBOQ                                                
         BZ    DISPF40                                                          
         MVC   CONTSTAT+10(17),=C'Backed-out by Rep'                            
         GOTO1 DATETIME,DMCB,CONTSTAT+28                                        
         B     DISPF100                                                         
*                                                                               
DISPF40  DS    0H                                                               
         TM    RMKGSCST,RMKGSRCQ                                                
         BZ    DISPF50                                                          
         MVC   CONTSTAT+10(15),=C'Recalled by Rep'                              
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    *+10                                                             
         MVC   CONTSTAT+22(3),=C'Sta'                                           
         GOTO1 DATETIME,DMCB,CONTSTAT+26                                        
         B     DISPF100                                                         
*                                                                               
DISPF50  DS    0H                                                               
         TM    RMKGSCST,RMKGSRJQ                                                
         BZ    DISPF60                                                          
         MVC   CONTSTAT+10(15),=C'Rejected by Rep'                              
         TM    RMKGSCST,RMKGSCRQ                                                
         BZ    *+10                                                             
         MVC   CONTSTAT+22(3),=C'Sta'                                           
         GOTO1 DATETIME,DMCB,CONTSTAT+26                                        
         B     DISPF100                                                         
*                                                                               
DISPF60  DS    0H                                                               
         TM    RMKGSCST,RMKGSCNQ                                                
         BZ    DISPF70                                                          
         MVC   CONTSTAT+10(16),=C'Cancelled by Rep'                             
         GOTO1 DATETIME,DMCB,CONTSTAT+27                                        
         B     DISPF100                                                         
*                                                                               
DISPF70  DS    0H                                                               
         TM    RMKGSCST,RMKGSRVQ                                                
         BZ    DISPF80                                                          
         MVC   CONTSTAT+10(14),=C'Revised by Rep'                               
         TM    RMKGSFG2,RMGF2RPQ                                                
         BO    *+10                                                             
         MVC   CONTSTAT+21(3),=C'Sta'                                           
         GOTO1 DATETIME,DMCB,CONTSTAT+25                                        
         B     DISPF100                                                         
*                                                                               
DISPF80  DS    0H                                                               
         MVC   CONTSTAT+10(9),=C'New Offer'                                     
         GOTO1 DATETIME,DMCB,CONTSTAT+20                                        
*                                                                               
* (RE)DISPLAY CURRENT DARE STATUS                                               
*                                                                               
DISPF100 DS    0H                                                               
         MVC   DARESTAT(9),=C'New Offer'                                        
*                                                                               
         LA    R3,*                GET ADDRESS TO TABLE                         
         A     R3,=A(MGACTTAB-(*-4))                                            
DISPF110 MVC   BYTE,RMKGSFG1                                                    
         NC    BYTE,0(R3)                                                       
         BNZ   DISPF120                                                         
         LA    R3,L'MGACTTAB(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   DISPF110                                                         
         B     DISPF250            NEW OFFER                                    
                                                                                
DISPF120 DS    0H                                                               
         MVC   DARESTAT,SPACES                                                  
         MVC   DARESTAT(16),10(R3)                                              
*                                  DISPLAY LAST DARE ACTION DATE/TIME           
         GOTO1 =A(SUBROUT),DMCB,(RC),('QDAREDT',0),RR=RELO2                     
*                                                                               
         OC    ERRNUM,ERRNUM       ERROR??                                      
         BZ    DISPF150                                                         
         LA    R3,ERRORLST                                                      
DISPF130 CLC   ERRNUM,1(R3)                                                     
         BE    DISPF140                                                         
         ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         BNE   DISPF130                                                         
         B     MGUNKNER            UNKNOWN MAKEGOOD ERROR                       
*                                                                               
DISPF140 DS    0H                                                               
         ZIC   R1,0(R3)                                                         
         SH    R1,=H'5'            SUBTRACT OVERHEAD                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONHED2+11(0),4(R3)                                              
         MVC   CONHED2(11),=C'Error    : '                                      
         MVC   CONHED2+6(3),1(R3)                                               
         OI    CONHED2H+1,X'08'    SET HI-INTENSITY                             
*                                                                               
* SWITCH TABLE FOR PFKEY LINE                                                   
*                                                                               
DISPF150 DS    0H                                                               
         XC    MGBPFKY,MGBPFKY                                                  
*                                                                               
         TM    RMKGSCST,RMKGSAPQ                                                
         BO    DISPF200                                                         
         TM    RMKGSFG1,RMGF1MSN   SENT?                                        
         BO    DISPF210                                                         
         TM    RMKGSFG1,RMGF1MAR   APPROVED?                                    
         BO    DISPF220                                                         
         TM    RMKGSFG1,RMGF1MRR   REJECTED?                                    
         BO    DISPF230                                                         
         TM    RMKGSFG1,RMGF1MCF   ACCEPTED?                                    
         BO    DISPF200                                                         
         TM    RMKGSFG1,RMGF1MCN   CANCELLED?                                   
         BO    DISPF200                                                         
         TM    RMKGSFG1,RMGF1MCR   RESENT?                                      
         BO    DISPF210                                                         
         TM    RMKGSFG1,RMGF1MCM   RECALLED?                                    
         BO    DISPF240                                                         
         TM    RMKGSFG1,RMGF1MER   ERROR?                                       
         BO    DISPF240                                                         
         B     DISPF250                                                         
*                                                                               
* (RE)DISPLAY PF KEY LINE                                                       
* IF APPLIED OR ACCEPTED OR CANCELLED                                           
*                                                                               
DISPF200 DS    0H                                                               
         MVC   MGBPFKY(38),=C'PF2 Contract  PF7 History  PF12 Return'           
         B     DISPFX                                                           
                                                                                
* IF RESENT OR SENT                                                             
DISPF210 DS    0H                                                               
         MVC   MGBPFKY(75),=C'PF2 Contract  PF7 History  PF8 Recall  PFX        
               9 Cancel  PF10 Resend  PF12 Return'                              
         B     DISPFX                                                           
                                                                                
* IF APPROVED                                                                   
DISPF220 DS    0H                                                               
         MVC   MGBPFKY(74),=C'PF2 Contract  4 Apply  7 History  8 RecalX        
               l  9 Cancel  10 Resend  12 Return'                               
         B     DISPFX                                                           
                                                                                
* REJECTED                                                                      
DISPF230 DS    0H                                                               
         MVC   MGBPFKY(63),=C'PF2 Contract  PF7 History  PF9 Cancel  PFX        
               10 Resend  PF12 Return'                                          
         B     DISPFX                                                           
                                                                                
* RECALLED OR ERROR                                                             
DISPF240 DS    0H                                                               
         MVC   MGBPFKY(63),=C'PF2 Contract  PF7 History  PF9 Cancel  PFX        
               10 Resend  PF12 Return'                                          
         B     DISPFX                                                           
                                                                                
* NEW AND REVISED ORDERS                                                        
DISPF250 DS    0H                                                               
         MVC   MGBPFKY(48),=C'PF2 Contract  PF6 Send  PF7 History  PF12X        
                Return'                                                         
         DROP  R6                                                               
                                                                                
DISPFX   DS    0H                                                               
         OI    MGBPFKYH+6,X'80'    XMIT                                         
         B     MGACTX                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY CONTRACT DATE/TIME STAMP                                              
***********************************************************************         
DATETIME NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R6,AIO                                                           
         USING RMKGREC,R6                                                       
*                                  SAVE OFF CURRENT DATE/TIME STAMP             
         XC    CONDTTM,CONDTTM                                                  
         MVC   CONDTTM(2),RMKGSLAD                                              
         MVC   CONDTTM+2(3),RMKGSLAT                                            
*                                                                               
* DATE                                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSLAD),(5,0(R2))                               
*                                                                               
* TIME HH:MM:SS                                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,RMKGSLAT,MYWORK,3,0                                  
         MVC   9(2,R2),MYWORK                                                   
         MVI   11(R2),C':'                                                      
         MVC   12(2,R2),MYWORK+2                                                
*        MVI   14(R2),C':'                                                      
*        MVC   15(2,R2),MYWORK+4                                                
*                                                                               
         B     MGACTX                                                           
*                                                                               
RELO2    DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPDARERROR                                                     
***********************************************************************         
* DISPLAY HISTORY                                                               
***********************************************************************         
         DS    0F                                                               
HIST00   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    DISPFLAG,X'FF'-HISTMKG                                           
                                                                                
         OC    SELECTKY,SELECTKY                                                
         BZ    BADERROR                                                         
                                                                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         TWAXC MGBLISTH,MGBENDLH,PROT=Y,TRNS=Y                                  
         MVC   MGBHIST(23),=C'Action    Date     Time'                          
         OI    MGBHISTH+1,X'08'    SET TO HIGH INTENSITY                        
         LA    R2,MGBHISTH                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         USING LHISTD,R2                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   HISTX                                                            
*                                                                               
         SR    R3,R3               FIND HOW MANY ACTIVITY ELEMENTS WE           
*                                                                               
HIST10   DS    0H                  HAVE AND DISPLAY THE MOST RECENT 11          
         LA    R3,1(R3)                                                         
         BRAS  RE,NEXTEL                                                        
         BE    HIST10                                                           
*                                                                               
         L     R6,AIO              REESTABLISH POINTER TO BEGINNING OF          
         MVI   ELCODE,X'02'        ACTIVITIES ELEMENT                           
         BRAS  RE,GETEL                                                         
         BNE   HISTX                                                            
*                                                                               
         CH    R3,=H'11'           MORE THAN THE MAXIMUM DISPLAY LINES?         
         BNH   HIST30                                                           
         SH    R3,=H'11'           YES, FIND NEW STARTING POINT                 
*                                                                               
HIST20   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   HISTX                                                            
         BCT   R3,HIST20                                                        
*                                                                               
* VERSION                                                                       
*                                                                               
         USING RMKGATEM,R6                                                      
HIST30   DS    0H                                                               
*        EDIT  (1,RMKGATVR),(3,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
                                                                                
* ACTION                                                                        
         LA    R3,MGACTTAB                                                      
HIST40   MVC   BYTE,RMKGATAT                                                    
         NC    BYTE,0(R3)                                                       
         BNZ   HIST50                                                           
         LA    R3,L'MGACTTAB(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   HIST40                                                           
         B     HIST60              ACTION NOT FOUND?                            
                                                                                
HIST50   DS    0H                                                               
         MVC   LHISACT,1(R3)                                                    
         TM    RMKGATAT,X'80'                                                   
         BZ    HIST60                                                           
         CLI   RMKGATLN,RMKGAL2Q                                                
         BL    HIST60                                                           
         OC    RMKGATEN,RMKGATEN                                                
         BZ    HIST60                                                           
         EDIT  RMKGATEN,(3,LHISACT+6)                                           
* DATE                                                                          
HIST60   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RMKGATDT),(5,LHISDATE)                            
                                                                                
* TIME HH:MM:SS                                                                 
         GOTO1 HEXOUT,DMCB,RMKGATTM,MYWORK,3,0                                  
         MVC   LHISTIME(2),MYWORK                                               
         MVI   LHISTIME+2,C':'                                                  
         MVC   LHISTIME+3(2),MYWORK+2                                           
         MVI   LHISTIME+5,C':'                                                  
         MVC   LHISTIME+6(2),MYWORK+4                                           
*                                                                               
HIST70   DS    0H                                                               
         BRAS  RE,NEXTEL           CHECK IF NEXT ELEMENT IS A DELNOT            
         BNE   HISTX                                                            
         CLI   RMKGATAT,0                                                       
         BNE   HIST80                                                           
*                                                                               
*        CLI   TWAOFFC,C'*'        DDS TERMINAL ONLY FOR NOW                    
*        BNE   HIST70                                                           
         MVC   LHISDLNT,=C'Delivered'                                           
                                                                                
* TIME HH:MM:SS                                                                 
         GOTO1 HEXOUT,DMCB,RMKGATTM,MYWORK,3,0                                  
         MVC   LHISDTIM(2),MYWORK                                               
         MVI   LHISDTIM+2,C':'                                                  
         MVC   LHISDTIM+3(2),MYWORK+2                                           
         MVI   LHISDTIM+5,C':'                                                  
         MVC   LHISDTIM+6(2),MYWORK+4                                           
         B     HIST70              INCASE OF MULTIPLE DELNOTS                   
*                                                                               
HIST80   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RF,MGBENDLH                                                      
         CR    R2,RF               EXIT IF WE HIT BOTTOM                        
         BL    HIST30                                                           
         DROP  R6                                                               
                                                                                
HISTX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
MGACTTAB DS    0CL26                                                            
         DC    X'80',C'Error    ',C'Agy Issued Error'                           
         DC    X'40',C'Sent     ',C'Sent by Rep     '                           
         DC    X'20',C'Approved ',C'Approved by Agy '                           
         DC    X'10',C'Rejected ',C'Rejected by Agy '                           
         DC    X'08',C'Applied  ',C'Applied by Rep  '                           
         DC    X'04',C'Cancelled',C'Cancelled by Rep'                           
         DC    X'02',C'Resent   ',C'Resent by Rep   '                           
         DC    X'01',C'Recalled ',C'Recalled by Rep '                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VARIOUS SUBROUTINES                                                           
***********************************************************************         
SUBROUT  DS    0H                                                               
         NMOD1 0,**SUBR**,RR=R4                                                 
         L     RC,0(R1)                                                         
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R5,SYSSPARE                                                      
         ST    R4,RELO3                                                         
                                                                                
QDAREDT  EQU   1                   DISPLAY DARE DATE/TIME STAMP                 
QCHKMUL  EQU   2                   CHECK MULTI-OFFER GROUP                      
                                                                                
         CLI   4(R1),QDAREDT                                                    
         BE    DAREDTTM                                                         
         CLI   4(R1),QCHKMUL                                                    
         BE    CHKMULTI                                                         
         DC    H'0'                                                             
                                                                                
SUBRX    DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY DARE DATE/TIME STAMP                                                  
***********************************************************************         
DAREDTTM DS    0H                                                               
         XC    DARDTTM,DARDTTM     SAVE OFF MOST RECENT DATE/TIME STAMP         
         XC    ERRNUM,ERRNUM                                                    
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DAREDTX                                                          
         XC    WORK,WORK                                                        
*                                                                               
DAREDT10 DS    0H                  GET THE DATE/TIME STAMP OF THE MOST          
         L     R2,AIO              RECENT MATCHING ACTION                       
IOD      USING RMKGREC,R2                                                       
         MVC   BYTE,IOD.RMKGSFG1                                                
         DROP  IOD                                                              
*                                                                               
         USING RMKGATEM,R6                                                      
         NC    BYTE,RMKGATAT       MATCH ON ACTION?                             
         BNZ   DAREDT20                                                         
         BRAS  RE,NEXTEL                                                        
         BE    DAREDT10                                                         
         OC    WORK,WORK                                                        
         BZ    DAREDTX                                                          
         B     DAREDT30                                                         
*                                                                               
DAREDT20 DS    0H                  YES, SAME DATE/TIME AND KEEP LOOKING         
         TM    RMKGATAT,X'80'      ERROR??                                      
         BZ    DAREDT25                                                         
*                                  SAVE OFF ERROR NUMBER                        
         EDIT  RMKGATEN,(3,ERRNUM),FILL=0                                       
         XC    WORK,WORK           EDIT USES WORK                               
*                                                                               
DAREDT25 DS    0H                  YES, SAME DATE/TIME AND KEEP LOOKING         
         MVC   WORK(5),RMKGATDT                                                 
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    DAREDT10                                                         
         DROP  R6                                                               
*                                                                               
DAREDT30 DS    0H                                                               
         LA    R2,DARESTAT+L'DARESTAT-1                                         
DAREDT40 CLI   0(R2),C' '                                                       
         BNE   DAREDT50                                                         
         BCTR  R2,0                                                             
         B     DAREDT40                                                         
*                                                                               
DAREDT50 DS    0H                                                               
         XC    DARDTTM,DARDTTM     SAVE OFF MOST RECENT DATE/TIME STAMP         
         MVC   DARDTTM,WORK                                                     
*                                                                               
* DATE                                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK),(5,2(R2))                                   
*                                                                               
* TIME HH:MM:SS                                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+2,MYWORK,3,0                                    
         MVC   11(2,R2),MYWORK                                                  
         MVI   13(R2),C':'                                                      
         MVC   14(2,R2),MYWORK+2                                                
*                                                                               
DAREDTX  DS    0H                                                               
         B     SUBRX                                                            
         EJECT                                                                  
***********************************************************************         
* CANNOT SEND MULTI-OFFER GROUPS                                                
***********************************************************************         
CHKMULTI DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 SKIP HEADER                                  
*                                                                               
CHKM10   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(RMKGKLIN-RMKGKEY),KEYSAVE                                    
         BNE   YES                                                              
         CLC   KEY(RMKGKRTY-RMKGKEY),KEYSAVE                                    
         BE    CHKM10                                                           
*                                                                               
         B     NO                                                               
*                                                                               
RELO3    DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF3D                                                       
         ORG   MGBSTAT                                                          
DARESTAT DS    CL36                                                             
CONTSTAT DS    CL36                                                             
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
       ++INCLUDE SPDARDARED                                                     
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE SPDARMKGDD        MAKEGOOD PQ RECORD DSECTS                    
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
DISPFLAG DS    X                   DISPLAY STATUS                               
FIRSTPG  EQU   X'01'               DISPLAY FIRST PAGE                           
NEXTPG   EQU   X'02'               DISPLAY NEXT PAGE                            
CFRMMKG  EQU   X'04'               APPLY MAKEGOOD                               
SENDMKG  EQU   X'08'               SEND MAKEGOOD                                
HISTMKG  EQU   X'10'               DISPLAY MAKEGOOD HISTORY                     
CANCMKG  EQU   X'20'               CANCEL MAKEGOOD                              
RESNDMKG EQU   X'40'               RESEND MAKEGOOD                              
UPDATEHY EQU   X'80'               UPDATE HISTORY                               
DISPFLG2 DS    X                                                                
SWAP2CON EQU   X'01'               SWAP TO THE CONTRACT PROGRAM                 
CANWMORE EQU   X'02'               CANCEL WITH MORE TO FOLLOW                   
BMPNXTOK EQU   X'04'               NORMAL EXIT IN ROUTIN BUMPNEXT               
PROCAPPL EQU   X'08'               IN THE PROCESS OF APPLYING                   
CKACTIVE EQU   X'10'               CHECK X'41' ACTIVE KEY                       
*                                                                               
SENDKEY  DS    CL(L'RMKGKEY)       MAKEGOOD HEADER KEY FOR SEND                 
*                                                                               
MGKEY    DS    CL(L'RMKGKEY)       HAS LAST MAKEGOOD KEY DISPLAYED              
SVKEY    DS    CL(L'RMKGKEY)                                                    
MGLSTNUM DS    X                   NUMBER OF AVAIL ROWS FOR MAKEGOOD            
SVLSTNUM DS    X                   SAVED # OF AVAIL ROWS FOR MAKEGOOD           
MGLSTMAX EQU   13-1                MAX IS 13 ROWS FOR MAKEGOOD LIST             
DMCB2    DS    6F                                                               
MYWORK   DS    CL64                                                             
SPLKEYAD DS    133C                EXTENDED SPOOL KEY AREA                      
FAKEHDR  DS    CL16                A FAKE HEADER FOR VCON                       
SCRNPTR  DS    F                   WHERE AM I ON THE SCREEN (R2)                
MGORDNUM DS    CL8                 AGENCY ORDER NUMBER IN DISPLAY FORM          
MISSDATE DS    CL(L'RMKGMGD1)      MISSED DATE (YMD) - START OF WEEK            
MISSDAT2 DS    CL(L'RMKGMGD2)      MISSED DATE (YMD) - END DATE                 
MISS#SPT DS    X                   MISSED NUMBER OF SPOTS                       
MISSLINE DS    X                   MISSED LINE NUMBER                           
MGSPTCST DS    CL4                 SPOT COST                                    
MGGRPCDE DS    CL2                 GROUP CODE                                   
MGOFFNUM DS    X                   OFFER NUMBER                                 
MGORSELD DS    C       Y/N         'OR' MAKEGOOD AT LEAST 1 SELTD               
MGPRGNAM DS    CL(L'MORBPGRM)      SAVE FOR ORBIT PROGRAM NAME                  
RECCOUNT DS    F                   TOTAL RECORD COUNT FOR ENTIRE GROUP          
TOTMGSPT DS    F                   TOTAL MAKEGOOD SPOTS                         
TOTMGDOL DS    F                   TOTAL MAKEGOOD DOLLARS                       
MGVERNUM DS    CL2                 MAKEGOOD VERSION NUMBER                      
MGALTWK  DS    CL6                 NEXT ALTERNATE WEEK DATE                     
DARDTTM  DS    XL5                 DARE MOST RECENT DATE/TIME STAMP             
CONDTTM  DS    XL5                 CONTRACT MOST RECENT DATE/TIME STAMP         
MGSDTTM  DS    XL5                 MGS DATE/TIME STAMP                          
ERRNUM   DS    CL3                 ERROR NUMBER                                 
MYP      DS    CL132                                                            
*                                                                               
* BUY LIST LINE                                                                 
*                                                                               
LMGD     DSECT                                                                  
LMGOFF#  DS    CL3                                                              
         DS    CL1                                                              
LMGLINE# DS    CL3                                                              
         DS    CL1                                                              
LMGMISS  DS    CL13                                                             
         DS    CL1                                                              
LMGOFDAT DS    CL18                                                             
         DS    CL1                                                              
LMGOFDT  DS    CL22                                                             
         DS    CL1                                                              
LMGLEN   DS    CL3                                                              
         DS    CL1                                                              
LMGCOST  DS    CL10                                                             
LMGLNQ   EQU   *-LMGD                                                           
*                                                                               
* HISTORY LIST LINE                                                             
*                                                                               
LHISTD   DSECT                                                                  
         DS    CL8                                                              
LHISACT  DS    CL9                                                              
         DS    CL1                                                              
LHISDATE DS    CL8                                                              
         DS    CL1                                                              
LHISTIME DS    CL8                                                              
         DS    CL1                                                              
LHISDLNT DS    CL9                                                              
         DS    CL1                                                              
LHISDTIM DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'178REDAR19S  08/26/02'                                      
         END                                                                    
