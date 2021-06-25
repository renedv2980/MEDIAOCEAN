*          DATA SET REDAR10S   AT LEVEL 052 AS OF 10/15/02                      
*          DATA SET REDAR10    AT LEVEL 114 AS OF 05/31/94                      
*PHASE T80F10A                                                                  
*                                                                               
         TITLE 'T80F10 - REDAR10 - DARE AGENCY BUY LIST'                        
***********************************************************************         
*                                                                     *         
*  REDAR10 (T80F10) --- DARE AGENCY BUY LIST                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 29JUL02 HQ  PF8 ALLOW USER TO SEE PREVIOUS ORDER/CHANGE CALL TO 30  *         
* 25JUN02 HQ  BUY LIST REPORT, EXCLUDES CONTRACT BUY LINES            *         
* 25FEB02 SKU HANDLE LARGER AND LONGER COMMENTS                       *         
* 03DEC01 SKU ADD TRADE FLAG                                          *         
* 06JUN98 SKU CHANGE CALL TO REGENDAB                                 *         
* 17FEB98 JRD MG INDICATOR                                            *         
* 07MAR97 SKU SUPPORT VARIOUS/BRAND ORDERS                            *         
* 05DEC94 SKU SUPPORT MUTLI-PAGE COMMENT RECORDS                      *         
* 02DEC94 SKU PREVENT BUY LIST FROM EXCEEDING SCREEN SIZE             *         
* 18MAY94 SKU INITIAL RELEASE                                         *         
*                                                                     *         
***********************************************************************         
T80F10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80F10*,R7,RR=R3                                              
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
                                                                                
         MVI   MYSCRNUM,X'FA'                                                   
         GOTO1 MODSCRN                                                          
         MVC   DRBLAST+1(2),=X'0101' RETRANSMIT ENTIRE SCREEN                   
                                                                                
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXITL    CLI   *,X'FF'                                                          
         B     EXIT                                                             
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   CALLSP,0            MUST BE CALLED TO GET HERE                   
         BNE   VKX                                                              
         LA    R2,CONRECH                                                       
         B     INVLRCAC            INVALID REC/ACTION                           
                                                                                
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         NI    BLSTFLG1,X'FF'-BLSTCMT                                           
         CLI   DRBLIST,C'C'        START FROM COMMENT IF COUNTER SCR            
         BE    *+8                 DISPLAYS COMMENT                             
         OI    BLSTFLG1,BLSTCMT    START FROM COMMENT                           
*                                                                               
         TWAXC DRBLISTH,DRBENDLH,PROT=Y                                         
         MVI   BLISTNUM,BLISTMAX                                                
                                                                                
         TM    BITFLAG,BFPRINT     WAS PF5 PRESSED? (REPORT)                    
         BZ    VR01                                                             
         XC    PRTSTAT,PRTSTAT                                                  
         XC    PRTSTAT2,PRTSTAT2                                                
*                                                                               
         OI    PRTSTAT,PRT10CAL    EXCLUDE CON BUYLINES                         
         OI    PRTSTAT2,PRTMGIND   PRINT MG INDICATOR                           
         TM    BITFLAG2,B2ISPREV                                                
         BZ    VR00A                                                            
*                                                                               
         TM    BITFLAG,BFREVNEW                                                 
         BZ    *+12                                                             
         OI    PRTSTAT,PRTCFCON    PRINT THE CONFIRM                            
         B     VR00A                                                            
         OI    PRTSTAT,PRTSHDOW    PRINT THE SHADOW                             
*                                                                               
VR00A    DS    0H                                                               
         GOTO1 VREDAR30,DMCB,(RC),(PRTSTAT,0),(PRTSTAT2,0)                      
         NI    BITFLAG,X'FF'-BFPRINT                                            
         B     EXIT                                                             
*                                                                               
VR01     TM    BITFLAG,BFPREV      WAS PF8 PRESSED? (SHADOW)                    
         BZ    VR02                NO                                           
*                                                                               
         NI    BITFLAG,X'FF'-BFPREV                                             
*                                                                               
         TM    BITFLAG2,B2ISPREV   AM I A SHADOW ALREADY?                       
         BO    VR01A               YES, SKIP CHECK                              
*                                                                               
         TM    BITFLAG2,B2SHADOW   NO, CHECK TO SEE IF THERE IS SHADOW          
         BZ    INVPFERR            IF NO, ERR                                   
*                                                                               
VR01A    DS    0H                                                               
         XI    BITFLAG2,B2ISPREV   TOGGLE STATUS                                
*                                                                               
         MVC   DRBPFLN+15(4),=C'Curr'                                           
         TM    BITFLAG2,B2ISPREV                                                
*                                                                               
         BO    *+10                                                             
         MVC   DRBPFLN+15(4),=C'Prev'                                           
*                                                                               
         TWAXC DRBLISTH,DRBENDLH,PROT=Y                                         
         MVI   BLISTNUM,BLISTMAX   WHEN FLIPPED, DISPLAY THE SAME BUY           
*                                                                               
         MVC   BUYKEY,AGYVKEY                                                   
*                                                                               
         DS    0H                                                               
         LA    R6,BUYKEY                                                        
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'40'       TYPE BUY                                     
         MVC   RDARKSEQ,SVBUYNUM                                                
         DROP  R6                                                               
                                                                                
         MVC   KEY(L'BUYKEY),BUYKEY                                             
         BAS   RE,MODKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VR03                START FROM BEGINING IF NO SUCH BUY           
*                                                                               
         TM    BLSTFLG1,BLSTCMT    START FROM COMMENT IF COUNTER SCR            
         BZ    VR01B               DISPLAYS COMMENT                             
         XC    DRBBNUM,DRBBNUM                                                  
         MVI   DRBBNUMH+5,0                                                     
         B     VR03                                                             
*                                                                               
VR01B    DS    0H                                                               
         MVC   BUYNUM,SVBUYNUM     FOR EASE OF COMPARISONS                      
         EDIT  BUYNUM,(3,DRBBNUM),ALIGN=LEFT                                    
         OI    DRBBNUMH+4,X'20'                                                 
         B     VR25                DON'T START OVER!                            
*                                                                               
*                                                                               
VR02     TM    BUYDPFLG,BFIRSTPG   START FROM THE BEGINNING?                    
         BO    VR03                                                             
*                                                                               
         CLC   BUYKEY(RDARKRT-RDARKEY),SELECTKY                                 
         BE    VR25                FIRST TIME IN??                              
                                                                                
VR03     DS    0H                                                               
         XC    BUYKEY,BUYKEY                                                    
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         BAS   RE,MODKEY                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                  YES, READ IN THE AGENCY ORDER RECORD         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         XC    DRBFLAG,DRBFLAG                                                  
         CLI   RDARCORT,C'T'       TRADE?                                       
         BNE   VR05                                                             
         MVC   DRBFLAG,=C'TRADE'                                                
*                                                                               
VR05     DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RDARESST),(3,FLTSTART)                            
*                                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    VR10                                                             
                                                                                
         ZAP   WORK+17(5),=P'0'       CONTRACT NUMBER                           
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,DRBHDLN),ALIGN=LEFT                              
         STC   R0,DRBHDLNH+5       SET LENGTH OF DESTINATION                    
         MVI   DRBHDLNH+4,X'08'    SET VALID NUMERIC                            
                                                                                
VR10     DS    0H                  AGENCY ORDER NUMBER                          
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARKORD                                              
         EDIT  (P5,WORK+17),(8,DRBAORD),ALIGN=LEFT                              
*                                                                               
VR15     DS    0H                                                               
         OC    DRBCTOT,DRBCTOT     TOTAL ALREADY DISPLAYED??                    
         BNZ   VR20                                                             
*                                                                               
         EDIT  GTOTAL$,(14,DRBCTOT),2,COMMAS=YES,ALIGN=LEFT                     
         EDIT  GSPT#,(5,DRBTSPT),ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
VR20     DS    0H                                                               
         XC    BUYDPFLG,BUYDPFLG                                                
         OI    BUYDPFLG,BFIRSTPG+MORESCMT+MOREOCMT                              
         MVI   SCMTNUM,1                                                        
         MVI   SCMTSEQ,0           SET TO FIRST STANDARD COMMT REC              
         MVI   OCMTNUM,1                                                        
         MVI   OCMTSEQ,0           SET TO FIRST ORDER COMMT RECORD              
         LA    R2,DRBHDLNH                                                      
         CLI   5(R2),0                                                          
         BE    VR25                                                             
         GOTO1 VALICON,DMCB,(R2)                                                
                                                                                
VR25     DS    0H                  USER SPECIFIED BUY NUMBER TO DISPLAY         
         CLI   DRBBNUMH+5,0                                                     
         BNE   VR30                                                             
         MVI   BUYNUM,0                                                         
         OI    DRBBNUMH+4,X'20'                                                 
         B     VR40                                                             
                                                                                
VR30     DS    0H                                                               
         LA    R2,DRBBNUMH                                                      
         TM    4(R2),X'20'         CHECK IF START FROM A DIFFERENT BUY          
         BO    VR40                                                             
*                                                                               
         CLI   8(R2),C'S'          START FROM STANDARD COMMENTS?                
         BNE   VR35                                                             
         MVI   BUYNUM,0                                                         
         XC    BUYDPFLG,BUYDPFLG                                                
         OI    BUYDPFLG,BFIRSTPG+MORESCMT+MOREOCMT                              
         MVI   SCMTSEQ,0                                                        
         MVI   SCMTNUM,1                                                        
         XC    DRBBNUM,DRBBNUM                                                  
         MVI   DRBBNUMH+5,0                                                     
         OI    DRBBNUMH+4,X'20'                                                 
         B     VR40                                                             
*                                                                               
VR35     DS    0H                                                               
         CLI   8(R2),C'O'          START FROM ORDER COMMENTS?                   
         BNE   VR38                                                             
         MVI   BUYNUM,0                                                         
         XC    BUYDPFLG,BUYDPFLG                                                
         OI    BUYDPFLG,BFIRSTPG+MORESCMT+MOREOCMT                              
         MVI   OCMTSEQ,0                                                        
         MVI   OCMTNUM,1                                                        
         LA    R2,DRBLISTH         POINT TO START OF LIST                       
         XC    DRBBNUM,DRBBNUM                                                  
         MVI   DRBBNUMH+5,0                                                     
         OI    DRBBNUMH+4,X'20'                                                 
         B     VR200                                                            
*                                                                               
VR38     DS    0H                                                               
         OI    DRBBNUMH+4,X'20'                                                 
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    INVLFLD                                                          
         CH    R0,=H'254'                                                       
         BH    INVLFLD                                                          
         STC   R0,BUYNUM                                                        
         LA    R2,DRBLISTH         POINT TO START OF LIST                       
         B     DISBUYS                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY STANDARD COMMENTS, ORDER COMMENTS AND AGENCY BUYS                     
***********************************************************************         
VR40     DS    0H                                                               
         LA    R2,DRBLISTH         POINT TO START OF LIST                       
                                                                                
         CLI   BUYNUM,0            DON'T START FROM TOP?                        
         BNE   DISBUYS                                                          
                                                                                
         TM    BUYDPFLG,BFIRSTPG                                                
         BO    VR50                                                             
         TM    BUYDPFLG,MORESCMT                                                
         BZ    VR200                                                            
***********************************************************************         
* CHECK IF ANY STANDARD COMMENTS                                                
* WE CAN HAVE UP TO 256 STANDARD COMMENT RECORDS.                               
* WE ACCESS THEM BY SETTING RDARKSEQ FROM X'00' TO X'FF'                        
***********************************************************************         
VR50     DS    0H                                                               
         XC    KEY,KEY             DISPLAY FIRST PAGE OF BUYS                   
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVC   KEY(L'RDARKEY),AGYVKEY                                           
         MVI   RDARKRT,X'20'       GET STANDARD COMMENTS                        
         MVC   RDARKSEQ,SCMTSEQ                                                 
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    VR60                IF NO STANDARD COMMENTS, CHECK               
         CLI   SCMTSEQ,0           IF FIRST PAGE, SKIP TO ORDER CMTS            
         BE    VR200               SUBSEQUENT PAGES NEED TO CLEAN UP            
         B     VR130               BEFORE PROCEEDING                            
VR60     DS    0H                                                               
         MVC   BUYKEY,KEY                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARSCEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VR110               NO COMMENTS FOUND, CHECK NEXT REC            
                                                                                
         CLI   SCMTNUM,1                                                        
         BE    VR80                                                             
         ZIC   R4,SCMTNUM                                                       
         BCTR  R4,0                                                             
                                                                                
VR70     DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   VR200                                                            
         BCT   R4,VR70                                                          
                                                                                
VR80     DS    0H                  DISPLAY HEADING IF FIRST PAGE                
         CLI   SCMTSEQ,0                                                        
         BNE   VR100                                                            
         MVC   DRBLIST(17),=C'Standard Comment:'                                
         OI    DRBLISTH+1,X'08'    SET TO HIGH INTENSITY                        
         ZIC   RF,BLISTNUM                                                      
         BCTR  RF,0                                                             
         STC   RF,BLISTNUM                                                      
                                                                                
VR90     DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         LA    RF,DRBENDLH         CHECK IF WE HIT THE END                      
         CR    R2,RF                                                            
         BH    VR120                                                            
*                                                                               
VR100    DS    0H                                                               
         ZIC   R1,RDARSCLN                                                      
         CLI   RDARSCLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RDARSCCM                                                 
         NI    1(R2),X'FF'-X'08'   SET TO NORMAL                                
         ZIC   RF,BLISTNUM         NUMBER OF BLANK ROWS LEFT ON SCREEN          
         BCTR  RF,0                                                             
         STC   RF,BLISTNUM                                                      
         BRAS  RE,NEXTEL                                                        
         BNE   VR110                                                            
         ZIC   RF,SCMTNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,SCMTNUM                                                       
         B     VR90                                                             
         DROP  R6                                                               
                                                                                
VR110    DS    0H                                                               
         ZIC   RF,SCMTSEQ          GET NEXT STANDARD COMMENT RECORD             
         LA    RF,1(RF)                                                         
         STC   RF,SCMTSEQ                                                       
         MVI   SCMTNUM,1                                                        
         BAS   RE,BUMPNEXT                                                      
         LA    RF,DRBENDLH         CHECK IF WE HIT THE END                      
         CR    R2,RF                                                            
         BNH   VR50                                                             
*                                                                               
VR120    DS    0H                                                               
         OI    BUYDPFLG,MORESCMT   YES, FLAG MORE TO DISPLAY                    
         NI    BUYDPFLG,X'FF'-BFIRSTPG                                          
         B     DISBX                                                            
                                                                                
VR130    DS    0H                                                               
         MVI   SCMTNUM,1           RESET FOR NEXT TIME                          
         NI    BUYDPFLG,X'FF'-MORESCMT                                          
         NI    BUYDPFLG,X'FF'-BFIRSTPG                                          
         EJECT                                                                  
***********************************************************************         
* ORDER COMMENTS                                                                
***********************************************************************         
VR200    DS    0H                  CHECK IF ANY ORDER COMMENTS                  
         TM    BUYDPFLG,MOREOCMT   IS THIS A CONTINUED DISPLAY?                 
         BZ    DISBUYS                                                          
                                                                                
VR210    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVC   KEY(L'RDARKEY),AGYVKEY                                           
         MVI   RDARKRT,X'30'       GET ORDER COMMENTS                           
         MVC   RDARKSEQ,OCMTSEQ                                                 
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    VR220                                                            
         CLI   OCMTSEQ,0                                                        
         BE    DISBUYS                                                          
         MVI   OCMTNUM,1           RESET FOR NEXT TIME                          
         NI    BUYDPFLG,X'FF'-MOREOCMT                                          
         NI    BUYDPFLG,X'FF'-BFIRSTPG                                          
         B     DISBUYS                                                          
                                                                                
VR220    DS    0H                                                               
         MVC   BUYKEY,KEY                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDAROREL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VR270                                                            
                                                                                
         CLI   OCMTNUM,1           FIND WHERE WE LAST LEFT OFF                  
         BE    VR240                                                            
         ZIC   R4,OCMTNUM                                                       
         BCTR  R4,0                                                             
                                                                                
VR230    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   DISBUYS                                                          
         BCT   R4,VR230                                                         
                                                                                
VR240    DS    0H                  DISPLAY HEADING IF FIRST PAGE                
         CLI   OCMTSEQ,0                                                        
         BNE   VR260                                                            
         MVC   8(14,R2),=C'Order Comment:'                                      
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         ZIC   RF,BLISTNUM                                                      
         BCTR  RF,0                                                             
         STC   RF,BLISTNUM                                                      
                                                                                
VR250    DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         LA    RF,DRBENDLH         CHECK IF WE HIT THE END                      
         CR    R2,RF                                                            
         BH    VR280                                                            
*                                                                               
VR260    DS    0H                                                               
         ZIC   R1,RDARORLN                                                      
         CLI   RDARORLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RDARORCM                                                 
         NI    1(R2),X'FF'-X'08'   SET TO NORMAL                                
         ZIC   RF,BLISTNUM         NUMBER OF BLANK ROWS LEFT ON SCREEN          
         BCTR  RF,0                                                             
         STC   RF,BLISTNUM                                                      
         BRAS  RE,NEXTEL                                                        
         BNE   VR270                                                            
         ZIC   RF,OCMTNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,OCMTNUM                                                       
         B     VR250                                                            
         DROP  R6                                                               
                                                                                
VR270    DS    0H                                                               
         ZIC   RF,OCMTSEQ          GET NEXT STANDARD COMMENT RECORD             
         LA    RF,1(RF)                                                         
         STC   RF,OCMTSEQ                                                       
         MVI   OCMTNUM,1                                                        
         BAS   RE,BUMPNEXT                                                      
         LA    RF,DRBENDLH         CHECK IF WE HIT THE END                      
         CR    R2,RF                                                            
         BNH   VR210                                                            
*                                                                               
VR280    DS    0H                                                               
         OI    BUYDPFLG,MOREOCMT   YES, FLAG MORE TO DISPLAY                    
         NI    BUYDPFLG,X'FF'-BFIRSTPG                                          
         B     DISBX                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY BUYS                                                                  
***********************************************************************         
DISBUYS  DS    0H                                                               
         OC    BUYKEY,BUYKEY                                                    
         BNZ   DISB70                                                           
                                                                                
         MVC   BUYKEY,AGYVKEY                                                   
                                                                                
DISB70   DS    0H                                                               
         LA    R6,BUYKEY                                                        
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'40'       TYPE BUY                                     
         MVC   RDARKSEQ,BUYNUM                                                  
         DROP  R6                                                               
                                                                                
         MVC   KEY(L'BUYKEY),BUYKEY                                             
         MVI   RDUPDATE,C'N'                                                    
         BAS   RE,MODKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    DISB75                                                           
         MVI   BUYNUM,0            NOT FOUND                                    
         B     DISB90                                                           
                                                                                
DISB75   DS    0H                                                               
K        USING RDARKEY,KEY                                                      
         MVC   SVBUYNUM,K.RDARKSEQ                                              
         DROP  K                                                                
         MVC   BUYKEY,KEY                                                       
*************                                                                   
* CHECK THIS !!!!!!!!!!!!!!!!!!!!!!!!                                           
*************                                                                   
         ZIC   RF,BLISTNUM         MINUS ONE FOR TITLE                          
         BCTR  RF,0                                                             
         STC   RF,BLISTNUM                                                      
         CLI   BLISTNUM,BLISTMAX                                                
         BNE   DISB78                                                           
*        ZIC   RF,BLISTNUM         MINUS ONE FOR TITLE                          
*        BCTR  RF,0                                                             
*        STC   RF,BLISTNUM                                                      
                                                                                
DISB78   DS    0H                                                               
         MVC   DMCB2(4),ACOMFACS   SET UP ROUTINE ADDRESS BLOCK                 
         MVC   DMCB2+4(4),VOUTDAY                                               
         MVC   DMCB2+8(4),UNTIME                                                
         MVC   DMCB2+12(4),DEMOCON                                              
*                                                                               
         CLC   RTKODATE,FLTSTART   TAKEOVER DATE CHOPPING REQUIRED??            
         BL    DISB79                                                           
*** TESTING                                                                     
*        GOTO1 DATCON,DMCB,(0,=C'970428'),(3,RTKODATE)                          
*** TESTING                                                                     
*                                                                               
         MVC   WORK(4),VREDARTK                                                 
         MVC   WORK+4(4),PERVERT                                                
         MVC   WORK+8(3),RTKODATE                                               
*                                                                               
         PRINT GEN                                                              
         GOTO1 VREGENDB,DMCB,(X'60',BUYKEY),(BLISTNUM,LISTAREA),(BUYNUMX        
               ,AIO2),DMCB2,WORK                                                
         B     DISB79A                                                          
*                                                                               
DISB79   DS    0H                                                               
         GOTO1 VREGENDB,DMCB,(X'20',BUYKEY),(BLISTNUM,LISTAREA),       X        
               (BUYNUM,AIO2),DMCB2                                              
         PRINT NOGEN                                                            
DISB79A  ZIC   R3,DMCB+8                                                        
         MVC   BUYNUM,DMCB+9                                                    
         LTR   R3,R3                                                            
         BZ    DISB90                                                           
                                                                                
         MVC   8(BUYTITLX,R2),BUYTITLE                                          
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         BAS   RE,BUMPNEXT                                                      
                                                                                
         LA    R4,LISTAREA                                                      
                                                                                
DISB80   DS    0H                                                               
         MVC   8(78,R2),1(R4)                                                   
         NI    1(R2),X'FF'-X'08'   SET TO NORMAL                                
         BAS   RE,BUMPNEXT                                                      
                                                                                
         LA    RF,DRBENDLH         CHECK IF WE'RE EXCEEDING SCREEN SIZE         
         CR    R2,RF                                                            
         BH    DISB90              SKIP IF BUY LIST DOESN'T FIT!                
                                                                                
DISB85   DS    0H                                                               
         LA    R4,L'LISTAREA(R4)                                                
         BCT   R3,DISB80                                                        
                                                                                
DISB90   DS    0H                                                               
         XC    DRBBNUM,DRBBNUM                                                  
         CLI   BUYNUM,0                                                         
         BNE   DISB100                                                          
         XC    BUYDPFLG,BUYDPFLG   NO MORE BUYS, SET TO DISPLAY                 
*                                    FROM BEGINNING ON NEXT PASS                
         OI    BUYDPFLG,BFIRSTPG+MORESCMT+MOREOCMT                              
         B     HITBOTTM                                                         
                                                                                
DISB100  DS    0H                                                               
         EDIT  BUYNUM,(3,DRBBNUM),ALIGN=LEFT                                    
         MVI   DRBBNUMH+5,1        RESET LENGTH                                 
                                                                                
DISBX    DS    0H                                                               
         B     NEXTPAGE                                                         
         EJECT                                                                  
***********************************************************************         
* MODIFY THE KEY ACCORDING TO WHICH MODE WE ARE IN, SHADOW OR CURRENT           
***********************************************************************         
MODKEY   DS    0H                                                               
         MVC   KEY(2),SELECTKY                                                  
         TM    BITFLAG2,B2ISPREV                                                
         BZ    MODKX                                                            
         MVC   KEY(2),=XL2'4101'                                                
         TM    BITFLAG,BFREVNEW                                                 
         BZ    MODKX                                                            
         MVC   KEY(2),=XL2'5100'                                                
MODKX    DS    0H                                                               
         BR    RE                                                               
***********************************************************************         
* MODIFY THE PF KEY LINE, IF THERE IS SHADOW, DISPLAY "PREV" ON PFLINE          
***********************************************************************         
MODSCRN  NTR1                                                                   
         TM    BITFLAG2,B2SCRCHG    SCREEN IS ALREADY CHANGED                   
         BO    MODSCRNX             GET OUT                                     
*                                                                               
         OI    BITFLAG2,B2SCRCHG                                                
         NI    BITFLAG2,X'FF'-B2SHADOW                                          
*                                                                               
         CLI   SELECTKY,X'51'                                                   
         BNE   MSCR05                                                           
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   KEY,X'41'                                                        
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEYSAVE(27),KEY                                                  
         BE    MODSCRNX             THERE IS A 4100, DON'T SHOW COMPAR          
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVC   KEY(2),=XL2'4101'                                                
         B     MSCR50                                                           
*                                                                               
MSCR05   DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   MODSCRN                                                          
*                                                                               
         USING RDARCODE,R6                                                      
         TM    RDARMISC,X'80'                                                   
         BO    MSCR10                                                           
         CLI   RDARRNUM,0                                                       
         BE    MSCR10                                                           
         OI    BITFLAG,BFREVNEW                                                 
         DROP  R6                                                               
*                                                                               
MSCR10   DS    0H                                                               
         MVC   KEY(2),=XL2'4101'                                                
         TM    BITFLAG,BFREVNEW                                                 
         BZ    *+10                                                             
         MVC   KEY(2),=XL2'5100'                                                
*                                                                               
MSCR50   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEYSAVE(27),KEY                                                  
         BNE   MODSCRNX                                                         
*                                                                               
         OI    BITFLAG2,B2SHADOW    THERE IS A SHADOW                           
         XC    DRBPFLN,DRBPFLN                                                  
         MVC   DRBPFLN(32),=C'PF5 Print  PF8 Prev  PF12 Return'                 
MODSCRNX B     EXIT                                                             
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFKL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   EXIT                                                             
*                                                                               
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPFKL10                                                         
*                                                                               
STPFKL10 LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
         TM    CTLRFLG1,CF1BRDQ    BRAND ORDER IN PROCESS??                     
         BZ    STPFINIT                                                         
         LA    R2,BLPFTAB                                                       
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
         CLI   PFKEY,5             PRINT A DRAFT ORDER?                         
         BNE   STPF80                                                           
         OI    BITFLAG,BFPRINT                                                  
         B     STPFX                                                            
STPF80   CLI   PFKEY,8             PRINT A DRAFT ORDER?                         
         BNE   STPFX                                                            
         OI    BITFLAG,BFPREV      READ PREVIOUS VERSION OF THE ORDER           
         B     STPFX                                                            
*                                                                               
STPFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT FIELD                                                            
***********************************************************************         
BUMPNEXT DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
LPFTABLE DS    0C                                                               
*                                                                               
* PRINT                                                                         
         DC    AL1(LPF05X-*,05,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF05X   EQU   *                                                                
*                                                                               
* PREVIOUS VERSION OF THE ORDER (SHADOW)                                        
         DC    AL1(LPF08X-*,08,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF08X   EQU   *                                                                
* RETURN TO SELECT SCREEN                                                       
         DC    AL1(LPF12X-*,12,0,0,(LPF12X-LPF12)/KEYLNQ,0)                     
         DC    CL3' ',CL8'ORDER',CL8'SELECT '                                   
LPF12    DC    AL1(KEYTYTWA,L'DRBHDLN-1),AL2(DRBHDLN-T80FFFD)                   
LPF12X   EQU   *                                                                
*                                                                               
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
BLPFTAB  DS    0C                                                               
*                                                                               
* PRINT                                                                         
         DC    AL1(BLPF05X-*,05,0,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8' ',CL8' '                                             
BLPF05X  EQU   *                                                                
*                                                                               
* TOGGLE BETWEEN PREV/CURR VERSION OF THE ORDER                                 
         DC    AL1(BLPF08X-*,08,0,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8' ',CL8' '                                             
BLPF08X  EQU   *                                                                
*                                                                               
* RETURN TO SELECT SCREEN                                                       
         DC    AL1(BLPF12X-*,12,0,0,(BLPF12X-BLPF12)/KEYLNQ,0)                  
         DC    CL3' ',CL8'BRAND',CL8'SELECT '                                   
BLPF12   DC    AL1(KEYTYTWA,L'DRBHDLN-1),AL2(DRBHDLN-T80FFFD)                   
BLPF12X  EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
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
HITBOTTM MVC   RERROR,=AL2(110)    BUY DISPLAYED                                
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
*                                                                               
INFEND   DS    0H                                                               
         LA    R2,DRBBNUMH                                                      
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BUYTITLE DC    C'C Lin Days         Times       Len Eff. Dates    Nw'           
         DC    C'   Mg    Npw       Rate Spt'                                   
BUYTITLX EQU   *-BUYTITLE                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REDARFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARFAD                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
BLISTNUM DS    X                   NUMBER OF AVAIL ROWS FOR BUY LIST            
BLISTMAX EQU   16                  MAX IS 16 ROWS FOR BUY LIST                  
BUYNUM   DS    X                   NEXT BUY NUMBER                              
SVBUYNUM DS    X                                                                
FLTSTART DS    XL3                                                              
*                                                                               
DMCB2    DS    6F                                                               
LISTAREA DS    17CL81              BUYLIST AREA                                 
PRTSTAT  DS    XL1                 PRINT OPTIONS                                
PRTCFCON EQU   X'04'               PRINT MG INDICATOR                           
PRTSHDOW EQU   X'02'               PRINT SHADOW RECORD                          
PRT10CAL EQU   X'01'               CALLED BY 10 CONTRACT BUYLINE                
PRTSTAT2 DS    XL1                                                              
PRTMGIND EQU   X'80'               PRINT MG INDICATOR                           
BLSTFLG1 DS    XL1                                                              
BLSTCMT  EQU   X'80'                                                            
WORKLQ   EQU   *-MYAREAD                                                        
*                                                                               
* BUY LIST LINE                                                                 
*                                                                               
LBUYD    DSECT                                                                  
LBUYMC   DS    CL2                                                              
         DS    CL1                                                              
LBUYLINE DS    CL3                                                              
         DS    CL1                                                              
LBUYDAYS DS    CL12                                                             
         DS    CL1                                                              
LBUYTIME DS    CL12                                                             
         DS    CL1                                                              
LBUYLEN  DS    CL4                                                              
         DS    CL1                                                              
LBUYDATE DS    CL11                                                             
         DS    CL1                                                              
LBUYNW   DS    CL3                                                              
         DS    CL1                                                              
LBUYNPW  DS    CL3                                                              
         DS    CL1                                                              
LBUYRATE DS    CL10                                                             
         DS    CL1                                                              
LBUYSPT  DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052REDAR10S  10/15/02'                                      
         END                                                                    
